%%%-------------------------------------------------------------------
%% @doc telemetry_service public API
%% @end
%%%-------------------------------------------------------------------

-module(telemetry_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    telemetry_service_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
