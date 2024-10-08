%%%-------------------------------------------------------------------
%% @doc devices_service public API
%% @end
%%%-------------------------------------------------------------------

-module(devices_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/devices/:device_id", devices_service_api_handler, device},
            {"/devices/:device_id/status", devices_service_api_handler, device_status},
            {"/devices/:device_id/commands", devices_service_api_handler, device_cmd},
            {"/devices/:device_id/telemetry/latest", devices_service_api_handler, latest_telemetry},
            {"/devices/:device_id/telemetry", devices_service_api_handler, all_telemetry}
        ]}
	]),
    Port = erlang:list_to_integer(os:getenv("HTTP_PORT", "8080")),
	{ok, _} = cowboy:start_clear(http, [{port, Port}], #{
		env => #{dispatch => Dispatch}
	}),

    devices_service_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
