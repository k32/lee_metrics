-module(lee_metrics_sink_log).

-behavior(gen_server).

%% API:
-export([start_link/0]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("lee/include/lee.hrl").
-include("lee_metrics.hrl").

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

init([]) ->
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

terminate(_Reason, _S) ->
  ok.

%%================================================================================
%% Internal functions
%%================================================================================

-spec collect(s()) -> s().
collect(S0) ->
  lists:foldl(fun(Type, S) ->
                  collect_type(Type, S)
              end,
              S0,
              ?lee_metric_types).

-spec collect_type(lee_metrics:type(), s()) -> s().
collect_type(Type, S0) ->
  Keys = lee_model:get_metatype_index(Type, lee_metrics_registry:model()),
  lists:foldl(fun(MKey, S) ->
                  case lee_metrics:collect(MKey) of
                    {ok, MNode, Values} ->
                      ?LOG_NOTICE(#{ type => Type
                                   , mkey => MKey
                                   , data => Values
                                   , mnode => MNode
                                   }),
                      S;
                    {error, Err} ->
                      ?LOG_WARNING(#{ message => failed_to_collect_metric
                                    , type => Type
                                    , key => MKey
                                    , reason => Err
                                    }),
                      S
                  end
              end,
              S0,
              Keys).

collect_interval() ->
  application:get_env(lee_metrics, log_interval, 1000).

collect_interval_min() ->
  application:get_env(lee_metrics, log_interval_min, 500).
