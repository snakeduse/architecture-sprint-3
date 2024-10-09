-module(telemetry_service_db).
-behaviour(gen_server).

%%% api callbacks
-export([
    start_link/1,
    add/1
]).

%%% gen_event callbacks
-export([
    init/1,
    handle_continue/2,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-record(state, {
    connection = undefined :: pid(),
    connection_info :: map()
}).

start_link(DbInfo) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DbInfo, []).

add({telemetry, _Id, _Value} = R) ->
    gen_server:call(?MODULE, {add, R}, 5000).

%%% gen_event callbacks
init(ConnInfo) ->
    process_flag(trap_exit, true),
    {ok, #state{ connection_info = ConnInfo }, {continue, connect}}.

handle_continue(connect, State = #state{ connection_info = ConnInfo}) ->
    ?LOG_INFO("Connect to DB. ~tp", [ConnInfo]),

    case epgsql:connect(ConnInfo) of
        {ok, Connection} ->
            {noreply, State#state{ connection = Connection }};
        {error, Reason} ->
            ?LOG_ERROR("Open DB conenction error ~tp", [Reason]),
            timer:sleep(2000),
            exit(Reason)
    end.

handle_call({add, {telemetry, Id, Value}}, _From, State = #state{ connection = C }) ->
    ?LOG_DEBUG("Add Telemetry for device id: ~tp", [Id]),
    
    Json = jsone:encode(Value),
    Query = io_lib:format(
        "insert into public.telemetry (device_id, \"data\") values ('~s', '~s')", 
        [Id, Json]
    ),
    Result = exec_query(
        fun(_) ->
            ok
        end,
        Query,
        C
    ),
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
        {ok, Payload} ->
            Fn(Payload);   
        {ok, _Columns, []} ->
            {ok, not_found};
        {error, #error{ code = Code, message = Message}} ->
            {error, Code, Message}
    end.
