-module(lee_metrics_filter).

%% API:
-export([all/0, of_type/1, union/1]).

-export_type([filter/0]).

-include("lee_metrics.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type filter() :: fun(() -> [lee:model_key()]).

%%================================================================================
%% API functions
%%================================================================================

-spec all() -> filter().
all() ->
  union([of_type(T) || T <- ?lee_metric_types]).

-doc """
Returns all metrics of given type.
""".
-spec of_type(lee_metrics:type()) -> filter().
of_type(Type) ->
  fun() ->
      lee_model:get_metatype_index(Type, lee_metrics_registry:model())
  end.

-doc """
Returns set union of the given filters.
""".
-spec union([filter()]) -> filter().
union(Filters) ->
  fun() ->
      lists:usort(
        lists:flatmap(
          fun(F) ->
              F()
          end,
          Filters))
  end.

%%================================================================================
%% behavior callbacks
%%================================================================================

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
