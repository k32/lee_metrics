-module(lee_metrics_storage).

-behavior(lee_storage).

-include_lib("lee/include/lee.hrl").

%% behavior callbacks:
-export([ create/1
        , get/2
        , patch/3
        ]).

-export_type([]).

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% behavior callbacks
%%================================================================================

-doc false.
create(Options) ->
  Name = maps:get(name, Options, ?MODULE),
  ets:new(Name, [public, set, named_table, {read_concurrency, true}]),
  Name.

-doc false.
get(Key, Tab) ->
  case ets:lookup(Tab, Key) of
    [{_, Val}] ->
      {ok, Val};
    [] ->
      undefined
  end.

-doc false.
patch(Tab, Delete, Set) ->
  [ets:delete(Tab, Key) || Key <- Delete],
  ets:insert(Tab, Set),
  Tab.

%%================================================================================
%% Internal functions
%%================================================================================
