-module(lee_metrics_sink_log).

-behavior(lee_metrics_sink).

%% behavior callbacks:
-export([sink_child_spec/1, metric_data/4]).

-include_lib("kernel/include/logger.hrl").
-include_lib("snabbkaffe/include/trace.hrl").

%%================================================================================
%% behavior callbacks
%%================================================================================

sink_child_spec(_Args) ->
  ignore.

metric_data(Args, Key, MNode, Metrics) ->
  ?tp(notice, lee_metrics_sink_data,
     #{ args => Args
      , key => Key
      , mnode => MNode
      , metrics => Metrics
      }).

%%================================================================================
%% Internal functions
%%================================================================================
