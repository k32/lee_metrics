-module(lee_metrics_sup).

-behavior(supervisor).

%% API:
-export([start_link/2, start_link_sinks/1, stop/0, attach_sink/1]).

%% behavior callbacks:
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% API functions
%%================================================================================

-define(SUP, ?MODULE).
-define(SINKS_SUP, lee_metrics_sinks_sup).

-spec start_link(lee:model(), [lee_metrics_sink:spec()]) ->
        supervisor:startlink_ret().
start_link(Model, Sinks) ->
  supervisor:start_link({local, ?SUP}, ?MODULE, {top, Model, Sinks}).

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

-spec attach_sink(supervisor:child_spec()) -> ok.
attach_sink(ChildSpec) ->
  case supervisor:start_child(?SINKS_SUP, ChildSpec) of
    {ok, _, _} ->
      ok;
    {ok, _} ->
      ok;
    {error, {already_started, _}} ->
      ok;
    {error, Err} ->
      ?LOG_WARNING(#{ message => failed_to_start_metric_sink
                    , sink => maps:get(id, ChildSpec, undefined)
                    , reason => Err
                    })
  end.

%%================================================================================
%% behavior callbacks
%%================================================================================

init({top, Model, Sinks}) ->
  Children = [ registry_spec(Model, [])
             , sinks_spec(Sinks)
             ],
  SupFlags = #{ strategy      => rest_for_one
              , intensity     => 10
              , period        => 10
              },
  {ok, {SupFlags, Children}};
init({sinks, Children}) ->
  SupFlags = #{ strategy      => one_for_one
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

-spec sinks_spec(_) -> supervisor:child_spec().
sinks_spec(Sinks) ->
  #{ id          => sinks_sup
   , start       => {?MODULE, start_link_sinks, [Sinks]}
   , shutdown    => infinity
   , restart     => permanent
   , type        => supervisor
   }.

-spec collector_spec([lee_metrics_sink:spec()]) -> supervisor:child_spec().
collector_spec(Sinks) ->
  #{ id          => collector
   , start       => {lee_metrics_collector, start_link, [Sinks]}
   , shutdown    => 5_000
   , restart     => permanent
   , type        => worker
   }.

%%================================================================================
%% Internal exports
%%================================================================================

-spec start_link_sinks(_) -> supervisor:startlink_ret().
start_link_sinks(Sinks) ->
  supervisor:start_link({local, ?SINKS_SUP}, ?MODULE, {sinks, Sinks}).

%%================================================================================
%% Internal functions
%%================================================================================
