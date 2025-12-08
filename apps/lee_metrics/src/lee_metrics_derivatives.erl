-module(lee_metrics_derivatives).

-behavior(gen_server).

%% API:
-export([start_link/0]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% internal exports:
-export([derivative_meta/3, get_deltas/1]).

-export_type([]).

-include_lib("lee/include/lee.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-define(uint64_max, 16#FFFFFFFFFFFFFFFF).
-define(TAB, ?MODULE).

%%================================================================================
%% API functions
%%================================================================================

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%================================================================================
%% behavior callbacks
%%================================================================================

-record(datapoint,
        { t_prev :: integer()
        , v_prev :: integer() | [{integer() | infinity, non_neg_integer()}]
        , key :: lee:key()
        }).

-record(s,
        { dps = #{} :: #{lee:key() => #datapoint{}}
        , timer :: reference()
        }).

init(_) ->
  process_flag(trap_exit, true),
  S = #s{ timer = start_timer()
        },
  ets:new(?TAB, [set, protected, named_table, {keypos, 1}]),
  {ok, S}.

handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_Cast, S) ->
  {noreply, S}.

handle_info({timeout, TRef, collect}, S0 = #s{timer = TRef}) ->
  S = collect(S0),
  {noreply, S#s{timer = start_timer()}};
handle_info({'EXIT', _, shutdown}, S) ->
  {stop, shutdown, S};
handle_info(_Info, S) ->
  {noreply, S}.

terminate(_Reason, _S) ->
  ok.

%%================================================================================
%% Internal exports
%%================================================================================

-spec derivative_meta(lee:model(), lee:model_key(), #mnode{}) -> {ok, #mnode{}} | {error, _}.
derivative_meta(Model, MKeyDerived, DerivMeta) ->
  maybe
    {ok, derivative_metric} ?= lee_metrics_mt:typeof(DerivMeta),
    #{origin := MKeyOrigin} ?= DerivMeta#mnode.metaparams,
    {ok, _} ?= derive_key(MKeyDerived, MKeyOrigin),
    {ok, OriginMeta} ?= lee_metrics_registry:get_meta(Model, MKeyOrigin),
    {ok, OriginType} ?= lee_metrics_mt:typeof(OriginMeta),
    derive_mnode(OriginType, OriginMeta)
  else
    {ok, _} ->
      {error, not_a_derivative};
    #{} ->
      {error, badmodel};
    Err ->
      Err
  end.

-spec get_deltas(lee:model_key()) -> lee_metrics:metric_data().
get_deltas(MKey) ->
  case ets:lookup(?TAB, MKey) of
    [{_, Deltas}] -> Deltas;
    []            -> []
  end.

%%================================================================================
%% Internal functions
%%================================================================================

collect(S0 = #s{dps = OldDPs}) ->
  Model = lee_metrics_registry:model(),
  MKeys = lee_model:get_metatype_index(derivative_metric, Model),
  DPs = lists:foldl(
          fun(MKey, DPs) ->
              collect(Model, OldDPs, MKey, DPs)
          end,
          #{},
          MKeys),
  S0#s{dps = DPs}.

collect(Model, OldDPs, MKey, DPs0) ->
  maybe
    #mnode{metaparams = #{origin := OriginKey}} = lee_model:get(MKey, Model),
    T = erlang:monotonic_time(millisecond),
    {ok, OriginMeta, MData} ?= lee_metrics:collect(OriginKey),
    {ok, Type} = lee_metrics_mt:typeof(OriginMeta),
    {DPs, Deltas} = lists:foldl(
      fun({Key, Val}, Acc) ->
          collect_instance(Type, MKey, OriginMeta, OldDPs, T, Key, Val, Acc)
      end,
      {DPs0, []},
      MData),
    ets:insert(?TAB, {MKey, Deltas}),
    DPs
  else
    _ ->
      DPs0
  end.

collect_instance(Type, MKey, Meta, OldDPs, T, Key, Val, {DPs, Deltas}) ->
  case OldDPs of
    #{Key := DP} ->
      #datapoint{t_prev = T0, v_prev = Val0, key = DKey} = DP,
      Dt = (T - T0) / 1_000,
      Delta = calc_delta(Type, Meta, Dt, Val0, Val),
      { DPs#{Key => DP#datapoint{t_prev = T, v_prev = Val}}
      , [{DKey, Delta} | Deltas]
      };
    #{} ->
      {ok, DKey} = derive_key(MKey, Key),
      { DPs#{Key => #datapoint{t_prev = T, v_prev = Val, key = DKey}}
      , Deltas
      }
  end.

calc_delta(counter_metric, #mnode{metaparams = MPs}, Dt, V0, V) ->
  if V0 > V ->
      %% Counter wrapped around:
      WrapAround = maps:get(wrap_around, MPs, ?uint64_max),
      (V + WrapAround - V0) / Dt;
     true ->
      (V - V0) / Dt
  end;
calc_delta(gauge_metric, _, Dt, V0, V) ->
  (V - V0) / Dt;
calc_delta(histogram_metric, _, Dt, V0, V) ->
  lists:zipwith(
    fun({Bucket, B0}, {_, B}) ->
        DB = if B0 > B ->
                 (B + ?uint64_max) - B0;
                true ->
                 B - B0
             end,
        {Bucket, DB / Dt}
    end,
    V0,
    V).

derive_mnode(Scalar, MNode) when Scalar =:= counter_metric;
                                 Scalar =:= gauge_metric;
                                 Scalar =:= external_counter_metric;
                                 Scalar =:= external_gauge_metric ->
  {ok, #mnode{ metatypes = [external_gauge_metric]
             , metaparams = #{ signed => true
                             , unit => derive_unit(MNode#mnode.metaparams)
                             , collect_callback => fun get_deltas/1
                             }
             }};
derive_mnode(Hist, #mnode{metaparams = MPs0}) when
    Hist =:= histogram_metric; Hist =:= external_histogram_metric ->
  MPs = maps:with([xunit, buckets], MPs0),
  {ok, #mnode{ metatypes = [external_histogram_metric]
             , metaparams = MPs#{collect_callback => fun get_deltas/1}
             }};
derive_mnode(Type, _) ->
  {error, {derivative_is_not_supported_for, Type}}.

derive_key(Derived, Origin) ->
  {Base, Suffix} = split_key(Derived),
  {InstanceBase, _} = split_key(Origin),
  case lee_model:get_model_key(InstanceBase) of
    Base ->
      {ok, InstanceBase ++ Suffix};
    _ ->
      {error, "Derived metric must be in the same map as the origin"}
  end.

derive_unit(#{unit := U}) ->
  <<U/binary, "/s">>;
derive_unit(#{}) ->
  <<"1/s">>.

start_timer() ->
  erlang:start_timer(1_000, self(), collect).

split_key(K) ->
  {SuffixR, BaseR} = lists:splitwith(
                       fun(I) -> not is_tuple(I) end,
                       lists:reverse(K)),
  {lists:reverse(BaseR), lists:reverse(SuffixR)}.
