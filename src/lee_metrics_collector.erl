-module(lee_metrics_collector).

-behavior(gen_server).

%% API:
-export([start_link/0]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% internal exports:
-export([]).

-export_type([]).

-include_lib("kernel/include/logger.hrl").
-include_lib("lee/include/lee.hrl").
-include("lee_metrics.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API functions
%%================================================================================

-define(SERVER, ?MODULE).

-define(timeout_collect, timeout_collect).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%================================================================================
%% behavior callbacks
%%================================================================================

-record(s, {}).
-type s() :: #s{}.

init(_) ->
  process_flag(trap_exit, true),
  S = #s{},
  erlang:start_timer(0, self(), ?timeout_collect),
  {ok, S}.

handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_Cast, S) ->
  {noreply, S}.

handle_info({'EXIT', _, shutdown}, S) ->
  {stop, shutdown, S};
handle_info({timeout, _, ?timeout_collect}, S0) ->
  T0 = erlang:system_time(millisecond),
  S = collect(S0),
  T1 = erlang:system_time(millisecond),
  Sleep = max(collect_interval_min(), collect_interval() - (T1 - T0)),
  erlang:start_timer(Sleep, self(), ?timeout_collect),
  {noreply, S};
handle_info(_Info, S) ->
  {noreply, S}.

terminate(_Reason, S) ->
  ok.

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================

-spec collect(s()) -> s().
collect(S0) ->
  Model = lee_metrics_registry:model(),
  Metrics = lee_metrics_registry:metrics(),
  lists:foldl(fun(Type, S) ->
                  collect_type(Type, Model, Metrics, S)
              end,
              S0,
              ?lee_metric_types).

-spec collect_type(lee_metrics:type(), lee:model(), lee:data(), s()) -> s().
collect_type(Type, Model, Data, S0) ->
  Keys = lee_model:get_metatype_index(Type, Model),
  lists:foldl(fun(MKey, S) ->
                  case collect_metric(Model, Data, MKey) of
                    {ok, _MNode, Values} ->
                      io:format(user, "FIXME ~p~n", [Values]),
                      S;
                    {error, Err} ->
                      ?LOG_WARNING(#{ message => failed_to_collect
                                    , type => Type
                                    , key => MKey
                                    , reason => Err
                                    }),
                      S
                  end
              end,
              S0,
              Keys).

-spec collect_metric(lee:model(), lee:data(), lee:model_key()) ->
        {ok, #mnode{}, [{lee:key(), lee_metrics:metric_value()}]}
      | {error, _}.
collect_metric(Model, Data, MKey) ->
  maybe
    {ok, MNode} ?= lee_metrics_registry:get_meta(Model, MKey),
    {ok, Type} ?= lee_metrics_mt:typeof(MNode),
    case is_external(Type) of
      false ->
        Instances = lee:list(Model, Data, MKey),
        Values = [{I, collect_instance(Type, Data, MNode, I)} || I <- Instances],
        {ok, MNode, Values}
    end
  end.

-spec collect_instance(lee_metrics:type(), lee:data(), #mnode{}, lee:key()) ->
        lee_metrics:metric_value().
collect_instance(counter_metric, Data, MNode, Key) ->
  Counters = lee_metrics_registry:get_metrics(Data, Key),
  lists:foldl(fun(Ctr, Acc) ->
                  counters:get(Ctr, 1) + Acc
              end,
              0,
              Counters).

is_external(_) ->
  false.

collect_interval() ->
  application:get_env(lee_metrics, collect_interval, 1000).

collect_interval_min() ->
  application:get_env(lee_metrics, collect_interval_min, 500).
