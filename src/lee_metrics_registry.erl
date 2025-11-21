-module(lee_metrics_registry).

-behavior(gen_server).

%% API:
-export([ register/4, unregister/2
        , model/0, metrics/0, get_metrics/2, get_meta/1, get_meta/2
        ]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% internal exports:
-export([start_link/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("lee/include/lee.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-define(SERVER, ?MODULE).
-define(MODEL, lee_metrics_registry_model).
-define(METRICS, lee_metrics_registry_metrics).

-record(call_register,
        { key :: lee:key()
        , type :: lee_metrics:type()
        , options :: lee_metrics:options()
        , metric :: lee_metrics:metric()
        }).

-record(cast_unregister,
        { key :: lee:key()
        , metric :: lee_metrics:metric()
        }).

%%================================================================================
%% API functions
%%================================================================================

-spec register(lee_metrics:type(), lee:key(), lee_metrics:options(), lee_metrics:metric()) ->
        ok | {error, _}.
register(Type, Key, Options, Metric) ->
  Req = #call_register{ key = Key
                      , type = Type
                      , options = Options
                      , metric = Metric
                      },
  case lists:member(async, Options) of
    true -> gen_server:cast(?SERVER, Req);
    false -> gen_server:call(?SERVER, Req)
  end.

-spec unregister(lee:key(), lee_metrics:metric()) -> ok.
unregister(Key, Metric) ->
  gen_server:cast(?SERVER, #cast_unregister{key = Key, metric = Metric}).

-spec model() -> lee:model().
model() ->
  persistent_term:get(?MODEL).

-spec metrics() -> lee:data().
metrics() ->
  persistent_term:get(?METRICS).

-spec get_metrics(lee:data(), lee:key()) -> [lee_metrics:metric()].
get_metrics(Data, Key) ->
  case lee_storage:get(Key, Data) of
    {ok, L} ->
      L;
    undefined ->
      []
  end.

-spec get_meta(lee:key()) -> {ok, lee_metrics:type(), #mnode{}} | {error, _}.
get_meta(Key) ->
  maybe
    {ok, Mnode} ?= get_meta(model(), Key),
    {ok, Type} ?= lee_metrics_mt:typeof(Mnode),
    {ok, Type, Mnode}
  end.

-spec get_meta(lee:model(), lee:key()) -> {ok, #mnode{}} | {error, _}.
get_meta(Model, Key) ->
  try
    Mnode = lee_model:get(lee_model:get_model_key(Key), Model),
    {ok, Mnode}
  catch
    error:{bad_model_key, _} ->
      {error, {no_such_metric, Key}}
  end.

%%================================================================================
%% behavior callbacks
%%================================================================================

-type monitors() :: #{reference() => {lee:key(), lee_metrics:metric()}}.

-record(s,
        { model :: lee:model()
          %% Lee storage that holds the metrics:
        , metrics :: lee:data()
          %% Base data layers:
        , base_data :: [lee:data()]
          %% Process monitors for automatic metric unregistration:
        , monitors = #{} :: monitors()
        }).
-type s() :: #s{}.

init([Model, BaseData]) ->
  process_flag(trap_exit, true),
  Metrics = lee_storage:new(lee_metrics_storage),
  S = #s{ model = Model
        , metrics = Metrics
        , base_data = BaseData
        },
  persistent_term:put(?MODEL, Model),
  persistent_term:put(?METRICS, Metrics),
  {ok, S}.

handle_call(#call_register{key = Key, options = _Options, type = Type, metric = Metric}, _From, S0) ->
  maybe
    {ok, S} ?= try_append_metric(S0, Key, Type, Metric),
    {reply, ok, S}
  else
    Err ->
      {reply, Err, S0}
  end;
handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(#call_register{key = Key, options = Options, type = Type, metric = Metric}, S0) ->
  maybe
    {ok, S} ?= try_append_metric(S0, Key, Type, Metric),
    {noreply, maybe_add_monitor(Options, Key, Metric, S)}
  else
    Err ->
      ?LOG_WARNING(#{ message => invalid_metric_key
                    , key => Key
                    , errors => Err
                    }),
      {noreply, S0}
  end;
handle_cast(#cast_unregister{key = Key, metric = Metric}, S0) ->
  Metrics = delete_metric(S0#s.metrics, Key, Metric),
  S = S0#s{metrics = Metrics},
  {noreply, S};
handle_cast(_Cast, S) ->
  {noreply, S}.

handle_info({'EXIT', _, shutdown}, S) ->
  {stop, shutdown, S};
handle_info({'DOWN', MRef, _Type, _Object, _Info}, S) ->
  #s{ monitors = Monitors0
    , metrics = Metrics0
    } = S,
  case maps:take(MRef, Monitors0) of
    {{Key, Metric}, Monitors} ->
      Metrics = delete_metric(Metrics0, Key, Metric),
      S#s{ monitors = Monitors
         , metrics = Metrics
         };
    error ->
      S
  end;
handle_info(_Info, S) ->
  {noreply, S}.

terminate(_Reason, _S) ->
  persistent_term:erase(?MODEL),
  persistent_term:erase(?METRICS),
  ok.

%%================================================================================
%% Internal exports
%%================================================================================

-doc false.
-spec start_link(lee:model(), lee:data()) -> {ok, pid()}.
start_link(Model, Data) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Model, Data], []).

%%================================================================================
%% Internal functions
%%================================================================================

-spec try_append_metric(s(), lee:key(), lee_metrics:type(), lee_metrics:metric()) ->
        {ok, s()} | {error, _}.
try_append_metric(S0, Key, Type, Metric) ->
  #s{ model = Model
    , metrics = Metrics0
    , base_data = BaseData
    } = S0,
  maybe
    {ok, Meta} ?= get_meta(Model, Key),
    {ok, ExpectedType} ?= lee_metrics_mt:typeof(Meta),
    %% This shouldn't normally happen unless the model changes,
    %% because the caller does its own check:
    true ?= ExpectedType =:= Type orelse
      {error, {metric_type_mismatch, Key, Type}},
    ok ?= try_add_values(Model, BaseData, Key),
    S = S0#s{metrics = insert_metric(Metrics0, Key, Metric)},
    {ok, S}
  else
    Err -> Err
  end.

-spec try_add_values(lee:model(), [lee:data()], lee:key()) ->
        ok | {error, _}.
try_add_values(Model, _BaseData, Key) ->
  try
    %% Create a fake storage for values that is only needed for
    %% verification of `Key'. We don't actually need to store any
    %% "real" model data.
    Values0 = lee_storage:new(lee_map_storage),
    [Static | Components] = lee_lib:splitl(fun is_tuple/1, Key),
    {_, Patch} =
      lists:foldl(
        fun([MapKey | Rest], {Parent, Acc}) ->
            #mnode{ metatypes = _MTs
                  , metaparams = MPs
                  } = lee_model:get(Parent, Model),
            KeyElements = maps:get(key_elements, MPs, []),
            length(KeyElements) =:= size(MapKey) orelse
              throw({error, {invalid_map_key, MapKey}}),
            Values = lists:zipwith(
                       fun(K, V) ->
                           {set, Parent ++ [?children | K], V}
                       end,
                       KeyElements,
                       tuple_to_list(MapKey)),
            {
             Parent ++ [?children | Rest],
             Values ++ Acc
            }
        end,
        {Static, []},
        Components),
    Values = lee_storage:patch(Values0, Patch),
    %% TODO: add BaseData
    case lee:validate(Model, Values) of
      {ok, _} ->
        ok;
      {error, Errors, _} ->
        {error, Errors}
    end
  catch
    Err -> Err
  end.

insert_metric(Metrics, Key, Metric) ->
  Old = get_metrics(Metrics, Key),
  Patch = [{set, Key, [Metric | Old]}],
  lee_storage:patch(Metrics, Patch).

delete_metric(Metrics, Key, Metric) ->
  L = get_metrics(Metrics, Key) -- [Metric],
  Patch = case L of
            [] ->
              [{rm, Key}];
            _ ->
              [{set, Key, L}]
          end,
  lee_storage:patch(Metrics, Patch).

-spec maybe_add_monitor(lee_metrics:options(), lee:key(), lee_metrics:metric(), s()) ->
        s().
maybe_add_monitor(Options, Key, Metric, S = #s{monitors = Monitors}) ->
  case lists:keyfind(monitor, 1, Options) of
    {monitor, Pid} when is_pid(Pid) ->
      MRef = monitor(process, Pid),
      S#s{monitors = Monitors#{MRef => {Key, Metric}}};
    _ ->
      S
  end.
