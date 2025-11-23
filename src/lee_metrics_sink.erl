-module(lee_metrics_sink).

%% API:
-export([init/1, notify_metric_removed/2, metric_data/4]).

-export_type([spec/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("lee/include/lee.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type args() :: term().

-type spec() :: {module(), args()}.

-callback sink_child_spec(args()) -> supervisor:child_spec() | ignore.

-callback notify_metric_removed(args(), lee:key()) -> ok.

-callback metric_data(args(), lee:model_key(), #mnode{}, lee_metrics_collector:metric_data()) ->
  ok.

-optional_callbacks([ notify_metric_removed/2
                    ]).

%%================================================================================
%% API functions
%%================================================================================

-spec init(spec()) -> ok.
init({Module, Args} = Spec) ->
  case safe_call_opt(Module, sink_child_spec, [Args]) of
    #{type := _, start := _} = ChildSpec ->
      lee_metrics_sup:attach_sink(ChildSpec#{id => Spec});
    _ ->
      ok
  end.

-spec notify_metric_removed(spec(), lee:key()) -> ok.
notify_metric_removed({Module, Args}, Key) ->
  safe_call_opt(Module, ?FUNCTION_NAME, [Args, Key]).

-spec metric_data(
        spec(),
        lee:model_key(),
        #mnode{},
        lee_metrics_collector:metric_data()
       ) -> ok.
metric_data({Module, Args}, MKey, MNode, Data) ->
  safe_call(Module, ?FUNCTION_NAME, [Args, MKey, MNode, Data]).

%%================================================================================
%% Internal functions
%%================================================================================

safe_call(Module, Function, Args) ->
  try
    apply(Module, Function, Args)
  catch
    EC:Err:Stack ->
      ?LOG_ERROR(#{ EC => Err
                  , module => Module
                  , function => Function
                  , args => Args
                  , stacktrace => Stack
                  }),
      ok
  end.

safe_call_opt(Module, Function, Args) ->
  case erlang:function_exported(Module, Function, length(Args)) of
    true ->
      safe_call(Module, Function, Args);
    false ->
      ok
  end.
