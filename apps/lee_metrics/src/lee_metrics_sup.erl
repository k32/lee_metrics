-module(lee_metrics_sup).

-behavior(supervisor).

%% API:
-export([start_link/1, stop/0]).

%% behavior callbacks:
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% API functions
%%================================================================================

-define(SUP, ?MODULE).
-define(SINKS_SUP, lee_metrics_sinks_sup).

-spec start_link(lee:model()) ->
        supervisor:startlink_ret().
start_link(Model) ->
  supervisor:start_link({local, ?SUP}, ?MODULE, {top, Model}).

-spec stop() -> ok.
stop() ->
  case whereis(?SUP) of
    Pid when is_pid(Pid) ->
      MRef = monitor(process, Pid),
      exit(whereis(?SUP), shutdown),
      receive
        {'DOWN', MRef, _, _, _} ->
          ok
      end;
    undefined ->
      ok
  end.

%%================================================================================
%% behavior callbacks
%%================================================================================

init({top, Model}) ->
  Children = [ registry_spec(Model, [])
             ],
  SupFlags = #{ strategy      => rest_for_one
              , intensity     => 10
              , period        => 10
              },
  {ok, {SupFlags, Children}}.

-spec registry_spec(lee:model(), [lee:data()]) -> supervisor:child_spec().
registry_spec(Model, BaseData) ->
  #{ id          => registry
   , start       => {lee_metrics_registry, start_link, [Model, BaseData]}
   , shutdown    => 5_000
   , restart     => permanent
   , type        => worker
   }.

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
