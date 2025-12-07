-module(lee_metrics_derivatives).

-behavior(gen_server).

%% API:
-export([start_link/0]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% internal exports:
-export([derivative_meta/2]).

-export_type([]).

-include_lib("lee/include/lee.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

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

-record(scalar_data,
        { t_prev :: integer()
        , v_prev :: number()
        , gauge :: lee_metrics:gauge()
        }).

-record(s,
        { scalars = #{} :: #{lee:key() => #scalar_data{}}
        }).

init(_) ->
  process_flag(trap_exit, true),
  S = #s{
        },
  {ok, S}.

handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_Cast, S) ->
  {noreply, S}.

handle_info({'EXIT', _, shutdown}, S) ->
  {stop, shutdown, S};
handle_info(_Info, S) ->
  {noreply, S}.

terminate(_Reason, _S) ->
  ok.

%%================================================================================
%% Internal exports
%%================================================================================

-spec derivative_meta(lee:model(), #mnode{}) -> {ok, #mnode{}} | {error, _}.
derivative_meta(Model, DerivMeta) ->
  maybe
    {ok, derivative_metric} ?= lee_metrics_mt:typeof(DerivMeta),
    #{origin := MKeyOrigin} ?= DerivMeta#mnode.metaparams,
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

%%================================================================================
%% Internal functions
%%================================================================================

derive_mnode(Scalar, MNode) when Scalar =:= counter_metric;
                                 Scalar =:= gauge_metric;
                                 Scalar =:= external_counter_metric;
                                 Scalar =:= external_gauge_metric ->
  {ok, #mnode{ metatypes = [gauge_metric]
             , metaparams = #{ signed => true
                             , unit => derive_unit(MNode#mnode.metaparams)
                             }
             }};
derive_mnode(histogram_metric, #mnode{metaparams = MPs}) ->
  {ok, #mnode{ metatypes = [histogram_metric]
             , metaparams = maps:with([xunit, buckets], MPs)
             }};
derive_mnode(Type, _) ->
  {error, {derivative_is_not_supported_for, Type}}.

derive_unit(#{unit := U}) ->
  <<U/binary, "/s">>;
derive_unit(#{}) ->
  <<"1/s">>.
