-module(telemetry_service_api_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    malformed_request/2,
    json_to_db/2
]).

-include_lib("kernel/include/logger.hrl").

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    ?LOG_DEBUG("1"),
    Methods = [<<"POST">>],
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    ?LOG_DEBUG("2"),
    {[ {{ <<"application">>, <<"json">>, '*'}, json_to_db} ], Req, State}.

malformed_request(Req = #{method := <<"POST">>}, State = telemetry) ->
    ?LOG_DEBUG("3"),
    {false, Req, State};
malformed_request(Req, State) ->
    ?LOG_DEBUG("4"),
    {true, Req, State}.
    
json_to_db(Req, State = telemetry) ->
    ?LOG_DEBUG("5"),
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    case requests_parser:parse_json(Body) of
        #{ <<"temperature">> := V, <<"device_id">> := Id} when is_number(V) andalso is_binary(Id) ->
            ok = telemetry_service_db:add({telemetry, Id, #{ temperature => V}}),
            {true, Req, State};
        X ->
            ?LOG_DEBUG("Failed json: ~tp", [X]),
            {false, Req, State}
    end;
json_to_db(Req, State) ->
    ?LOG_DEBUG("6"),
    {false, Req, State}.
