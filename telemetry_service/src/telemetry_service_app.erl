%%%-------------------------------------------------------------------
%% @doc telemetry_service public API
%% @end
%%%-------------------------------------------------------------------

-module(telemetry_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/telemetry", telemetry_service_api_handler, telemetry}
        ]}
	]),
    Port = erlang:list_to_integer(os:getenv("HTTP_PORT", "8181")),
	{ok, _} = cowboy:start_clear(http, [{port, Port}], #{
		env => #{dispatch => Dispatch}
	}),
    telemetry_service_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
