-module(lee_metrics).

%% API:
-export([collect/1, unregister_metric/2]).
-export([new_counter/2, incr/2]).
-export([new_gauge/2, gauge_set/2]).

%% internal exports:
-export([]).

-export_type([ type/0, metric/0
             , metric_value/0
             , options/0
             , counter/0, gauge/0
             ]).

-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").
-include_lib("kernel/include/logger.hrl").

-reflect_type([metric_data/0]).

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
        non_neg_integer() % counter_metric
      | integer()         % gauge
      | float().          % gauge

-type metric_data() :: [{lee:key(), metric_value()}].

-opaque counter() :: counters:counters_ref().

-opaque gauge() :: atomics:atomics_ref().

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
  {ok, atomics:new(1, [{signed, Signed}])}.

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
  end.

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
  Values = CB(MKey),
  ok = typerefl:typecheck(metric_data(), Values),
  Values.
-endif.

is_external(external_counter_metric) ->
  true;
is_external(external_gauge_metric) ->
  true;
is_external(_) ->
  false.
