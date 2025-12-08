-module(lee_metrics).

%% API:
-export([collect/1, unregister_metric/2]).
-export([new_counter/2, incr/2]).
-export([new_gauge/2, gauge_set/2]).
-export([new_histogram/2, histogram_observe/2]).
-export([new_summary/2, summary_observe/2]).

%% internal exports:
-export([]).

-export_type([ type/0, metric/0
             , metric_value/0, metric_data/0
             , options/0
             , counter/0, gauge/0, histogram/0, summary/0
             ]).

-include_lib("lee/include/lee.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%%================================================================================
%% Type declarations
%%================================================================================

-type options() :: [{monitor, pid()} | async].

-type type() :: counter_metric
              | gauge_metric
              | external_counter_metric
              | external_gauge_metric
              | histogram_metric
              | external_histogram_metric
              | derivative_metric
              | summary_metric.

-type metric_value() ::
        non_neg_integer()                           % counter_metric
      | integer()                                   % gauge_metric
      | [{number() | infinity, non_neg_integer()}]  % histogram_metric
      | [{number() | infinity, number()}]           % external_histogram_metric
      | number().                                   % external_gauge and counter metrics

-type metric_data() :: [{lee:key(), metric_value()}].

-opaque counter() :: counters:counters_ref().

-opaque gauge() :: atomics:atomics_ref().

-record(histogram,
        { buckets :: tuple()
        , counters :: counters:counters_ref()
        , min :: number()
        , max :: number()
        }).

-opaque histogram() :: #histogram{}.

-record(summary,
        { n_buckets :: pos_integer()
        %% [Nsamples, SumBucket1, SumBucket2, ...]
        , cref :: atomics:atomics_ref()
        }).

-opaque summary() :: #summary{}.

-type metric() :: counter() | gauge().

%%================================================================================
%% API functions
%%================================================================================

-doc """
Remove metric from the global registry.

Note: this function doesn't invalidate metric instances created
by the business applications.
""".
-spec unregister_metric(lee:key(), metric()) -> ok.
unregister_metric(Key, Metric) ->
  lee_metrics_registry:unregister(Key, Metric).

-doc """
Collect all metric instances identified by the model key.
""".
-spec collect(lee:model_key()) -> {ok, #mnode{}, metric_data()} | {error, _}.
collect(MKey) ->
  Model = lee_metrics_registry:model(),
  Data = lee_metrics_registry:metrics(),
  maybe
    {ok, MNode} ?= lee_metrics_registry:get_meta(Model, MKey),
    {ok, Type} ?= lee_metrics_mt:typeof(MNode),
    case is_external(Type) of
      false ->
        Instances = lee:list(Model, Data, MKey),
        Values = [{I, collect_instance(Type, Data, MNode, I)} || I <- Instances],
        {ok, MNode, Values};
      true when Type =:= derivative_metric ->
        collect_derivative(Model, MKey, MNode);
      true ->
        {ok, MNode, collect_external(MKey, MNode)}
    end
  end.

-doc """
Create and register a new counter metric.
""".
-spec new_counter(lee:key(), options()) -> {ok, counter()} | {error, _}.
new_counter(Key, Options) ->
  register_metric(counter_metric, Key, Options).

-doc """
Increment a counter.
""".
-spec incr(counter(), pos_integer()) -> ok.
incr(Counter, Val) ->
  counters:add(Counter, 1, Val).

-doc """
Create and register a new gauge metric.
""".
-spec new_gauge(lee:key(), options()) -> {ok, gauge()} | {error, _}.
new_gauge(Key, Options) ->
  register_metric(gauge_metric, Key, Options).

-doc """
Set value of gauge.
""".
-spec gauge_set(gauge(), integer()) -> ok.
gauge_set(Ref, Value) ->
  atomics:put(Ref, 1, Value).

-doc """
Create a new histogram metric.
""".
-spec new_histogram(lee:key(), options()) -> {ok, histogram()} | {error, _}.
new_histogram(Key, Options) ->
  register_metric(histogram_metric, Key, Options).

-doc """
Add a sample to the histogram.
""".
-spec histogram_observe(histogram(), number()) -> ok.
histogram_observe(Histogram = #histogram{counters = Counters}, Val) ->
  Idx = hist_index(Val, Histogram),
  counters:add(Counters, Idx, 1).

-doc """
Create a new summary metric.
""".
-spec new_summary(lee:key(), options()) -> {ok, summary()} | {error, _}.
new_summary(Key, Options) ->
  register_metric(summary_metric, Key, Options).

-doc """
Add a sample to the summary.
""".
-spec summary_observe(summary(), number()) -> ok.
summary_observe(#summary{n_buckets = N, cref = Cref}, Val) ->
  %% Spread sum over multiple counters to minimize chance of overflow:
  Idx = os:perf_counter() rem N + 2,
  atomics:add(Cref, 1, 1),
  atomics:add(Cref, Idx, Val).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================

-spec register_metric(type(), lee:key(), options()) -> {ok, metric()} | {error, _}.
register_metric(Type, Key, Options) ->
  maybe
    {ok, Meta} ?= get_meta(Type, Key),
    {ok, Metric} ?= create_metric(Type, Meta),
    ok ?= lee_metrics_registry:register(Type, Key, Options, Metric),
    {ok, Metric}
  end.

-spec create_metric(type(), #mnode{}) -> {ok, metric()} | {error, _}.
create_metric(counter_metric, _) ->
  {ok, counters:new(1, [])};
create_metric(gauge_metric, #mnode{metaparams = MPs}) ->
  Signed = maps:get(signed, MPs, false),
  {ok, atomics:new(1, [{signed, Signed}])};
create_metric(histogram_metric, #mnode{metaparams = MPs}) ->
  create_histogram(MPs);
create_metric(summary_metric, MNode) ->
  NBuckets = summary_n_buckets(MNode),
  Summary = #summary{ n_buckets = NBuckets
                    , cref = atomics:new(1 + NBuckets, [])
                    },
  {ok, Summary}.

-spec get_meta(type(), lee:key()) -> {ok, #mnode{}} | {error, _}.
get_meta(ExpectedType, Key) ->
  maybe
    {ok, Type, Meta} ?= lee_metrics_registry:get_meta(Key),
    true ?= (Type =:= ExpectedType) orelse
      {error, #{ message => wrong_metric_type
               , key => Key
               , requested => ExpectedType
               , registered => Type
               }},
    {ok, Meta}
  end.

-spec collect_instance(lee_metrics:type(), lee:data(), #mnode{}, lee:key()) ->
        lee_metrics:metric_value().
collect_instance(counter_metric, Data, _MNode, Key) ->
  Counters = lee_metrics_registry:get_metrics(Data, Key),
  lists:foldl(fun(Ctr, Acc) ->
                  counters:get(Ctr, 1) + Acc
              end,
              0,
              Counters);
collect_instance(gauge_metric, Data, #mnode{metaparams = MPs}, Key) ->
  AggreMethod = maps:get(aggregate, MPs, sum),
  Gauges = lee_metrics_registry:get_metrics(Data, Key),
  {Len, Sum} = lists:foldl(fun(Ctr, {Len, Sum}) ->
                               { Len + 1
                               , atomics:get(Ctr, 1) + Sum
                               }
                           end,
                           {0, 0},
                           Gauges),
  case AggreMethod of
    sum ->
      Sum;
    avg ->
      Sum / Len
  end;
collect_instance(histogram_metric, Data, #mnode{metaparams = #{buckets := Buckets}}, Key) ->
  Hists = lee_metrics_registry:get_metrics(Data, Key),
  {L, _} = lists:foldl(
             fun(LT, {L, Idx}) ->
                 Sum = lists:foldl(
                         fun(#histogram{counters = Ctrs}, Acc) ->
                             Acc + counters:get(Ctrs, Idx)
                         end,
                         0,
                         Hists),
                 {[{LT, Sum} | L], Idx - 1}
             end,
             {[], length(Buckets) + 1},
             [infinity | lists:reverse(Buckets)]),
  L;
collect_instance(summary_metric, Data, _MNode, Key) ->
  Summaries = lee_metrics_registry:get_metrics(Data, Key),
  lists:foldl(
    fun(#summary{n_buckets = NBuckets, cref = CRef}, {N, SumAcc}) ->
        Sum = summary_get_sum(CRef, NBuckets),
        { N + atomics:get(CRef, 1)
        , SumAcc + Sum
        }
    end,
    {0, 0},
    Summaries).

-ifndef(TEST).
collect_external(MKey, MNode) ->
  #{collect_callback := CB} = MNode#mnode.metaparams,
  try CB(MKey)
  catch
    EC:Err:Stacktrace ->
      ?LOG_ERROR(#{ msg => external_metric_crash
                  , EC => Err
                  , stacktrace => Stacktrace
                  , key => MKey
                  }),
      []
  end.
-else.
collect_external(MKey, MNode) ->
  #{collect_callback := CB} = MNode#mnode.metaparams,
  CB(MKey).
-endif.

collect_derivative(Model, MKey, MNodeDeriv) ->
  {ok, MNode} = lee_metrics_derivatives:derivative_meta(Model, MKey, MNodeDeriv),
  {ok, MNode, collect_external(MKey, MNode)}.

is_external(external_counter_metric)   -> true;
is_external(external_gauge_metric)     -> true;
is_external(derivative_metric)         -> true;
is_external(external_histogram_metric) -> true;
is_external(_)                         -> false.

%%--------------------------------------------------------------------------------
%% Histograms
%%--------------------------------------------------------------------------------

create_histogram(#{buckets := []}) ->
  {error, invalid_histogram};
create_histogram(#{buckets := Buckets0}) when is_list(Buckets0) ->
  Buckets = list_to_tuple(lists:sort(Buckets0)),
  Counters = counters:new(size(Buckets) + 1, [write_concurrency]),
  {ok, #histogram{ buckets = Buckets
                 , counters = Counters
                 , min = lists:min(Buckets0)
                 , max = lists:max(Buckets0)
                 }};
create_histogram(_) ->
  {error, wrong_metric_type}.

-spec hist_index(number(), histogram()) -> pos_integer().
hist_index(Val, #histogram{min = Min}) when Val < Min ->
  1;
hist_index(Val, #histogram{max = Max, buckets = Buckets}) when Val >= Max ->
  size(Buckets) + 1;
hist_index(Val, #histogram{buckets = Buckets}) ->
  hist_index(Val, Buckets, 1, size(Buckets)).

hist_index(Val, Buckets, MinIdx, MaxIdx) ->
  Mid = (MinIdx + MaxIdx) div 2,
  UpperBound = element(Mid, Buckets),
  if Val >= UpperBound ->
      hist_index(Val, Buckets, Mid + 1, MaxIdx);
     element(Mid - 1, Buckets) =< Val ->
      Mid;
     true ->
      hist_index(Val, Buckets, MinIdx, Mid)
  end.

summary_n_buckets(#mnode{metaparams = MPs}) ->
  maps:get(n_buckets, MPs, 32).

summary_get_sum(CRef, NBuckets) ->
  summary_get_sum(CRef, 2, NBuckets, 0).

summary_get_sum(_CRef, I, NBuckets, Acc) when I > NBuckets ->
  Acc;
summary_get_sum(CRef, I, NBuckets, Acc) ->
  summary_get_sum(CRef, I + 1, NBuckets, Acc + atomics:get(CRef, I)).

-ifdef(TEST).

hist_index0_test_() ->
  [ ?_assertMatch({error, wrong_metric_type}, create_histogram(#{}))
  , ?_assertMatch({error, invalid_histogram}, create_histogram(#{buckets => []}))
  ].

hist_index1_test_() ->
  {ok, H} = create_histogram(#{buckets => [2]}),
  [ ?_assertMatch(1, hist_index(0, H))
  , ?_assertMatch(1, hist_index(1, H))
  , ?_assertMatch(2, hist_index(2, H))
  , ?_assertMatch(2, hist_index(100, H))
  ].

hist_index2_test_() ->
  {ok, H} = create_histogram(#{buckets => [2, 10]}),
  [ ?_assertMatch(1, hist_index(0, H))
  , ?_assertMatch(1, hist_index(1, H))
  , ?_assertMatch(2, hist_index(2, H))
  , ?_assertMatch(3, hist_index(10, H))
  , ?_assertMatch(3, hist_index(11, H))
  ].

hist_index3_test_() ->
  {ok, H} = create_histogram(#{buckets => [2, 10, 20]}),
  [ ?_assertMatch(1, hist_index(0, H))
  , ?_assertMatch(1, hist_index(1, H))
  , ?_assertMatch(2, hist_index(2, H))
  , ?_assertMatch(2, hist_index(9, H))
  , ?_assertMatch(3, hist_index(10, H))
  , ?_assertMatch(3, hist_index(15, H))
  , ?_assertMatch(4, hist_index(20, H))
  , ?_assertMatch(4, hist_index(200, H))
  ].

buckets_gen() ->
  ?LET(L, non_empty(list(integer(0, 2))),
       begin
         element(1,
           lists:mapfoldl(
             fun(Incr, Acc) ->
                 {Acc, Acc + Incr}
             end,
             0,
             L))
       end).

hist_index_prop() ->
  ?FORALL({Val, Buckets},
          ?LET(B, buckets_gen(),
               begin
                 Min = lists:min(B),
                 Max = lists:max(B),
                 ?LET(I, integer(Min, Max), {I, B})
               end),
          begin
            {ok, Hist} = create_histogram(#{buckets => Buckets}),
            #histogram{buckets = Tup, min = Min, max = Max} = Hist,
            Idx = hist_index(Val, Hist),
            Val < Max andalso
              ?assert(
                 element(Idx, Tup) > Val,
                 {Val, Buckets, Idx}),
            Val > Min andalso
              ?assert(
                 element(Idx - 1, Tup) =< Val,
                 {Val, Buckets, Idx}),
            true
          end).

hist_index_test_() ->
  {timeout, 10,
   ?_assert(
      proper:quickcheck(
        hist_index_prop(),
        [ {numtests, 1000}
        , {max_size, 100}
        , {to_file, user}
        ]))}.

-endif.
