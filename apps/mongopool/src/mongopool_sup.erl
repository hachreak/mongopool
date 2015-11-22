%%%-------------------------------------------------------------------
%% @doc mongopool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('mongopool_sup').

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_link(Pools) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Pools).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, Pools} = application:get_env(mongopool, pools),
  init(Pools);
init(Pools) ->
  RestartStrategy = {
    one_for_all,
    0, % MaxRestart,
    1 % MaxTime
  },
  PoolSpecs = lists:map(
                fun({Name, SizeArgs, WorkerArgs}) ->
                    PoolArgs = [{name, {local, Name}},
                                {worker_module, mongopool_worker}] ++ SizeArgs,
                    poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                end, Pools),
  {ok, {RestartStrategy, PoolSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
