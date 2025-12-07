-module(lee_metrics).

%% API:
-export([collect/1, unregister_metric/2]).
-export([new_counter/2, incr/2]).
-export([new_gauge/2, gauge_set/2]).
-export([new_histogram/2, histogram_observe/2]).

%% internal exports:
-export([]).

-export_type([ type/0, metric/0
             , metric_value/0, metric_data/0
             , options/0
             , counter/0, gauge/0, histogram/0
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
              | histogram_metric.

-type metric_value() ::
        non_neg_integer()                           % counter_metric
      | integer()                                   % gauge
      | float()                                     % gauge
      | [{number() | infinity, non_neg_integer()}]. % histogram

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

-type metric() :: counter() | gauge().

%%================================================================================
%% API functions
%%================================================================================

-spec unregister_metric(lee:key(), metric()) -> ok.
unregister_metric(Key, Metric) ->
  lee_metrics_registry:unregister(Key, Metric).

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
      true ->
        {ok, MNode, collect_external(MKey, MNode)}
    end
  end.

-spec new_counter(lee:key(), options()) -> {ok, counter()} | {error, _}.
new_counter(Key, Options) ->
  register_metric(counter_metric, Key, Options).

-spec incr(counter(), pos_integer()) -> ok.
incr(Counter, Val) ->
  counters:add(Counter, 1, Val).

-spec new_gauge(lee:key(), options()) -> {ok, gauge()} | {error, _}.
new_gauge(Key, Options) ->
  register_metric(gauge_metric, Key, Options).

-spec gauge_set(gauge(), integer()) -> ok.
gauge_set(Ref, Value) ->
  atomics:put(Ref, 1, Value).

-spec new_histogram(lee:key(), options()) -> {ok, histogram()} | {error, _}.
new_histogram(Key, Options) ->
  register_metric(histogram_metric, Key, Options).

-spec histogram_observe(histogram(), number()) -> ok.
histogram_observe(Histogram = #histogram{counters = Counters}, Val) ->
  Idx = hist_index(Val, Histogram),
  counters:add(Counters, Idx, 1).

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
  create_histogram(MPs).

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
  L.

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

is_external(external_counter_metric) ->
  true;
is_external(external_gauge_metric) ->
  true;
is_external(_) ->
  false.

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
