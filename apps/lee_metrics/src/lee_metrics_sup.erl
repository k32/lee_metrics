-module(lee_metrics_sup).

-behavior(supervisor).

%% API:
-export([start_link/0, stop/0]).

%% behavior callbacks:
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% API functions
%%================================================================================

-define(SUP, ?MODULE).

-spec start_link() ->
        supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?SUP}, ?MODULE, top).

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

init(top) ->
  Children = [ registry_spec([])
             , derivatives_spec()
             ],
  SupFlags = #{ strategy      => rest_for_one
              , intensity     => 10
              , period        => 10
              },
  {ok, {SupFlags, Children}}.


%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================

-spec registry_spec([lee:data()]) -> supervisor:child_spec().
registry_spec(BaseData) ->
  #{ id          => registry
   , start       => {lee_metrics_registry, start_link, [BaseData]}
   , shutdown    => 5_000
   , restart     => permanent
   , type        => worker
   }.

-spec derivatives_spec() -> supervisor:child_spec().
derivatives_spec() ->
  #{ id          => derivatives
   , start       => {lee_metrics_derivatives, start_link, []}
   , shutdown    => 5_000
   , restart     => permanent
   , type        => worker
   }.
