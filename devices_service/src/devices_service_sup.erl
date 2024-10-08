%%%-------------------------------------------------------------------
%% @doc devices_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(devices_service_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 5000,
        period => 1
    },
    ChildSpecs = [
        devices_service_db_spec()
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

devices_service_db_spec() ->
    Host = os:getenv("DB_HOST", "localhost"),
    Port = erlang:list_to_integer(os:getenv("DB_PORT", "5432")),
    User = os:getenv("DB_USER", "postgres"),
    Pass = os:getenv("DB_PASSWORD", "postgres"),
    DbName = os:getenv("DB_NAME", "smart_home"),
    DbInfo = #{
        host => Host,
        port => Port,
        username => User,
        password => Pass,
        database => DbName
    },
    #{
        id => devices_service_db,
        start => {devices_service_db, start_link, [DbInfo]},
        restart => permanent,
        shutdown => 2000,
        type => worker
    }.