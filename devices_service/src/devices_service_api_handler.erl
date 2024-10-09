-module(devices_service_api_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    malformed_request/2,
    db_to_json/2,
    json_to_db/2
]).

-include_lib("kernel/include/logger.hrl").

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"PUT">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[ {{ <<"application">>, <<"json">>, '*'}, db_to_json} ], Req, State}.

content_types_accepted(Req, State) ->
    {[ {{ <<"application">>, <<"json">>, '*'}, json_to_db} ], Req, State}.

malformed_request(Req = #{method := <<"GET">>}, State = device) ->
    {false, Req, State};
malformed_request(Req = #{method := <<"PUT">>}, State = device_status) ->
    {false, Req, State};
malformed_request(Req = #{method := <<"POST">>}, State = device_cmd) ->
    {false, Req, State};
malformed_request(Req = #{method := <<"GET">>}, State = latest_telemetry) ->
    {false, Req, State};
malformed_request(Req = #{method := <<"GET">>}, State = all_telemetry) ->
    {false, Req, State};
malformed_request(Req, State) ->
    {true, Req, State}.

db_to_json(Req, State = device) ->
    DeviceId = cowboy_req:binding(device_id, Req),
    {ok, Device} = devices_service_db:get({id, DeviceId}),
    {response_serializer:serialize_json(Device), Req, State};

db_to_json(Req, State = latest_telemetry) ->
    DeviceId = cowboy_req:binding(device_id, Req),
    {ok, Telemetry} = devices_service_db:get({telemetry_latest, DeviceId}),
    {response_serializer:serialize_json(Telemetry), Req, State};

db_to_json(Req, State = all_telemetry) ->
    DeviceId = cowboy_req:binding(device_id, Req),
    case devices_service_db:get({telemetry_all, DeviceId}) of
        {ok, not_found} ->  
            {<<"[]">>, Req, State};
        {ok, Telemetry} ->
            {response_serializer:serialize_json(Telemetry), Req, State}
    end.

json_to_db(Req, State = device_status) ->
    DeviceId = cowboy_req:binding(device_id, Req),
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    case requests_parser:parse_json(Body) of
        #{ <<"status">> := V} when V =:= <<"on">> orelse V =:= <<"off">> ->
            ok = devices_service_db:update({status, DeviceId, V}),
            {true, Req, State};
        X ->
            ?LOG_DEBUG("Failed json: ~tp", [X]),
            {false, Req, State}
    end;
json_to_db(Req, State = device_cmd) ->
    {true, Req, State};
json_to_db(Req, State) ->
    {false, Req, State}.
