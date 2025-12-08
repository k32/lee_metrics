-module(lee_metrics_mt).
-moduledoc false.

-behavior(lee_metatype).

%% API:
-export([typeof/1]).

%% behavior callbacks:
-export([names/1, metaparams/1, meta_validate_node/4]).

%% internal exports:
-export([]).

-export_type([]).

-include_lib("typerefl/include/types.hrl").
-include_lib("lee/include/lee.hrl").
-include("lee_metrics.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type gauge_aggregate() :: sum | avg.

-reflect_type([gauge_aggregate/0]).

%%================================================================================
%% API functions
%%================================================================================

-spec typeof(#mnode{}) -> {ok, lee_metrics:type()} | {error, not_a_metric}.
typeof(#mnode{metatypes = MTs}) ->
  Res = lists:search(fun(A) -> lists:member(A, ?lee_metric_types) end, MTs),
  case Res of
    {value, Type} ->
      {ok, Type};
    false ->
      {error, not_a_metric}
  end.

%%================================================================================
%% behavior callbacks
%%================================================================================

names(_) ->
  ?lee_metric_types.

metaparams(counter_metric) ->
  [ {optional, unit, binary()}
  , {optional, wrap_around, pos_integer()}
  | lee_doc:documented()
  ];
metaparams(gauge_metric) ->
  [ {optional, unit, binary()}
  , {optional, signed, boolean()}
  , {optional, aggregate, gauge_aggregate()}
  | lee_doc:documented()
  ];
metaparams(external_counter_metric) ->
  %% TODO: should be fun
  [ {mandatory, collect_callback, term()}
  | metaparams(counter_metric)
  ];
metaparams(external_gauge_metric) ->
  %% TODO: should be fun
  [ {mandatory, collect_callback, term()}
  | metaparams(gauge_metric)
  ];
metaparams(histogram_metric) ->
  [ {optional, xunit, binary()}
  , {mandatory, buckets, nonempty_list(number())}
  | lee_doc:documented()
  ];
metaparams(external_histogram_metric) ->
  [ {mandatory, collect_callback, term()}
  | metaparams(histogram_metric)
  ];
metaparams(derivative_metric) ->
  [ {mandatory, origin, lee:model_key()}
  | lee_doc:documented()
  ];
metaparams(summary_metric) ->
  [ {optional, unit, binary()}
  | lee_doc:documented()
  ].


meta_validate_node(Type, Model, Key, MNode) ->
  Results0 = case Type of
               histogram_metric ->
                 [hist_check_buckets(MNode)];
               derivative_metric ->
                 [check_derivative(Model, Key, MNode)];
               _ ->
                 []
             end,
  Results = [no_other_types(Type, MNode) | Results0],
  lee_lib:compose_checks(Results).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================

hist_check_buckets(#mnode{metaparams = #{buckets := [Fst|Rest]}}) ->
  do_check_buckets(Fst, Rest);
hist_check_buckets(_) ->
  Err = "Invalid bucket specification",
  {[Err], []}.

do_check_buckets(Prev, L) ->
  case L of
    [] ->
      {[], []};
    [Next | _] when Next =< Prev ->
      {["Invalid bucket specification: not strictly increasing"], []};
    [Next | Rest] ->
      do_check_buckets(Next, Rest)
  end.

check_derivative(Model, Key, MNode) ->
  case lee_metrics_derivatives:derivative_meta(Model, Key, MNode) of
    {ok, _} ->
      {[], []};
    Err ->
      {[Err], []}
  end.

no_other_types(Type, #mnode{metatypes = MTs}) ->
  Errors = case [I || I <- MTs, I =/= Type, J <- ?lee_metric_types, I =:= J] of
             [] -> [];
             _  -> ["Only one metric type is allowed per Lee model node"]
           end,
  {Errors, []}.
