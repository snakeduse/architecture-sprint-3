-module(devices_service_db).
-behaviour(gen_server).

%%% api callbacks
-export([
    start_link/1,
    get/1,
    update/1
]).

%%% gen_event callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("./devices_service.hrl").

-record(state, { 
    connection :: pid()
}).

start_link(DbInfo) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DbInfo, []).

get({id, _DeviceId} = R) ->
    gen_server:call(?MODULE, {get, R}, 5000);
get({telemetry_latest, _DeviceId} = R) ->
    gen_server:call(?MODULE, {get, R}, 5000);
get({telemetry_all, _DeviceId} = R) ->
    gen_server:call(?MODULE, {get, R}, 5000).


update({status, _Id, _Value} = R) ->
    gen_server:call(?MODULE, {update, R}, 5000).

%%% gen_event callbacks
init(ConnInfo) ->
    ?LOG_INFO("Connect to DB. ~tp", ConnInfo),
    {ok, Connection} = epgsql:connect(ConnInfo),
    {ok, #state{ connection = Connection }}.

handle_call({get, {id, DeviceId}}, _From, State = #state{ connection = C }) ->
    ?LOG_DEBUG("Get Device - id: ~tp", [DeviceId]),
    Query = io_lib:format("select * from devices where id = '~s'", [DeviceId]),
    Result = exec_query(
        fun([{Id, Title, Type, House, SerialNumber, Status, LastUpd}]) ->
            {ok, #{
                <<"id">> => Id,
                <<"title">> => Title,
                <<"type_id">> => Type,
                <<"house_id">> => House,
                <<"serial_number">> => SerialNumber,
                <<"status">> => Status,
                <<"last_updated">> => LastUpd
            }}
        end,
        Query,
        C
    ),
    {reply, Result, State};

handle_call({get, {telemetry_latest, DviceId}}, _From, State = #state{ connection = C }) ->
    ?LOG_DEBUG("Get Device - id: ~tp", [DviceId]),
    Query = io_lib:format(
        <<"select * "
          "from public.telemetry "
          "where device_id = '~s' "
          "order by changed_at desc "
          "limit 1;">>, [DviceId]),
    Result = exec_query(
        fun([{_Id, Data, ChangedAt}]) ->
            JSON = jsone:decode(Data),
            {ok, JSON#{
                <<"changed_at">> => ChangedAt
            }}
        end,
        Query,
        C
    ),
    {reply, Result, State};

handle_call({get, {telemetry_all, Id}}, _From, State = #state{ connection = C }) ->
    ?LOG_DEBUG("Get Device - id: ~tp", [Id]),
    Query = io_lib:format(
        <<"select * "
          "from public.telemetry "
          "where device_id = '~s' "
          "order by changed_at desc "
          "limit 1000">>, [Id]),
    Result = exec_query(
        fun(Items) ->
            {ok, lists:map(
                fun({_Id, Data, ChangedAt}) ->
                    JSON = jsone:decode(Data),
                    JSON#{ <<"changed_at">> => ChangedAt }
                end, 
                Items
            )}
        end,
        Query,
        C
    ),
    {reply, Result, State};

handle_call({update, {status, Id, Value}}, _From, State = #state{ connection = C }) ->
    ?LOG_DEBUG("Updsate device status - id: ~tp status: ~tp", [Id, Value]),
    Query = io_lib:format(
        "UPDATE devices SET  status='~s', last_updated=now() WHERE id='~s'", 
        [Value, Id]
    ),
    Result = exec_query(fun(_) -> ok end, Query, C),
    {reply, Result, State};

handle_call(Request, From, State) ->
    ?LOG_INFO("Unexpected call ~tp.", [{Request, From}]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    ?LOG_INFO("Unexpected cast ~tp.", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG_INFO("Unexpected info ~tp.", [Info]),
    {noreply, State}.

terminate(Reason, #state{ connection = C }) ->
    ?LOG_INFO("terminated with reason ~tp.", [Reason]),
    ok = epgsql:close(C).

exec_query(Fn, Q, C) ->
    case epgsql:squery(C, Q) of
        {ok, _Columns, Payload} ->
            Fn(Payload);            
        {ok, _Columns, []} ->
            {ok, not_found};
        {error, #error{ code = Code, message = Message}} ->
            {error, Code, Message}
    end.