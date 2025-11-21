-module(lee_metrics_sup).

-behavior(supervisor).

%% API:
-export([start_link/2, stop/0]).

%% behavior callbacks:
-export([init/1]).

%%================================================================================
%% API functions
%%================================================================================

-define(SUP, ?MODULE).

-spec start_link(lee:model(), [lee:data()]) -> supervisor:startlink_ret().
start_link(Model, BaseData) ->
  supervisor:start_link({local, ?SUP}, ?MODULE, {top, Model, BaseData}).

-spec stop() -> ok.
stop() ->
  exit(whereis(?SUP), shutdown),
  ok.

%%================================================================================
%% behavior callbacks
%%================================================================================

init({top, Model, BaseData}) ->
  Children = [ registry_spec(Model, BaseData)
             , collector_spec()
             ],
  SupFlags = #{ strategy      => rest_for_one
              , intensity     => 10
              , period        => 10
              , auto_shutdown => never
              },
  {ok, {SupFlags, Children}}.

-spec registry_spec(lee:model(), [lee:data()]) -> supervisor:child_spec().
registry_spec(Model, BaseData) ->
  #{ id          => registry
   , start       => {lee_metrics_registry, start_link, [Model, BaseData]}
   , shutdown    => 5_000
   , restart     => permanent
   , type        => worker
   , significant => false
   }.

-spec collector_spec() -> supervisor:child_spec().
collector_spec() ->
  #{ id          => collector
   , start       => {lee_metrics_collector, start_link, []}
   , shutdown    => 5_000
   , restart     => permanent
   , type        => worker
   , significant => false
   }.

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
