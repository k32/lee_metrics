-module(lee_metrics).

%% API:
-export([unregister_metric/2]).
-export([new_counter/2, incr/2]).

%% internal exports:
-export([]).

-export_type([ type/0, metric_value/0, metric/0
             , options/0
             , counter/0
             ]).

-include_lib("lee/include/lee.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type options() :: [{monitor, pid()} | async].

-type type() :: counter_metric.

-type metric_value() ::
        non_neg_integer(). % counter_metric

-opaque counter() :: counters:counters_ref().

-type metric() :: counter().

%%================================================================================
%% API functions
%%================================================================================

-spec unregister_metric(lee:key(), metric()) -> ok.
unregister_metric(Key, Metric) ->
  lee_metrics_registry:unregister(Key, Metric).

-spec new_counter(lee:key(), options()) -> {ok, counter()} | {error, _}.
new_counter(Key, Options) ->
  register_metric(counter_metric, Key, Options).

-spec incr(counter(), pos_integer()) -> ok.
incr(Counter, Val) ->
  counters:add(Counter, 1, Val).

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
  {ok, counters:new(1, [])}.

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
