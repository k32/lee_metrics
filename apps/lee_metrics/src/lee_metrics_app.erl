-module(lee_metrics_app).
-behavior(application).

%% API:
-export([]).

%% behavior callbacks:
-export([start/2, stop/1]).

%% internal exports:
-export([]).

-export_type([]).

%%================================================================================
%% behavior callbacks
%%================================================================================

start(_StartType, _StartArgs) ->
  lee_metrics_sup:start_link().

stop(_) ->
  lee_metrics_sup:stop().
