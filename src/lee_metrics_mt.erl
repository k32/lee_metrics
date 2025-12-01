-module(lee_metrics_mt).
-moduledoc false.

-behavior(lee_metatype).

%% API:
-export([typeof/1]).

%% behavior callbacks:
-export([names/1, metaparams/1, meta_validate_node/4, post_patch/5]).

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
  Res = lists:search(fun(counter_metric) -> true;
                        (gauge_metric) -> true;
                        (_) -> false
                     end,
                     MTs),
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
  | lee_doc:documented()];
metaparams(gauge_metric) ->
  [ {optional, unit, binary()}
  , {optional, signed, boolean()}
  , {optional, aggregate, gauge_aggregate()}
  | lee_doc:documented()].

meta_validate_node(Type, _Model, _Key, #mnode{metatypes = MTs}) ->
  Errors = case [I || I <- MTs, I =/= Type, J <- ?lee_metric_types, I =:= J] of
             [] -> [];
             _ -> ["Only one metric type is allowed per Lee model node"]
           end,
  {Errors, []}.

post_patch(_Type, _Model, _Data, _Mnode, PatchOp) ->
  case PatchOp of
    {rm, Key} ->
      lee_metrics_collector:notify_metric_removed(Key);
    {set, _, _} ->
      ok
  end.

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
