-module(lee_metrics_vm).

-export([model/0]).

-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").

model() ->
  #{erlang =>
      #{ reductions =>
           {[external_counter_metric],
            #{ oneliner => "Total number of reductions for all processes"
             , doc => """
                      This value does not include reductions performed
                      in current time slices of currently scheduled
                      processes.
                      """
             , collect_callback => fun reductions/1
             }}
       , context_switches =>
           {[external_counter_metric],
            #{ oneliner => "Total number of context switches since the system started"
             , collect_callback => fun context_switches/1
             }}
       , run_time =>
           {[external_counter_metric],
            #{ oneliner => "Run time for all threads in the Erlang VM"
             , collect_callback => fun run_time/1
             }}
       , port =>
           #{ io =>
               {[map],
                #{ key_elements => [[direction]]
                 },
                #{ direction =>
                     {[value],
                      #{ type => union([input, output])
                       }}
                 , bytes =>
                     {[external_counter_metric],
                      #{ oneliner => "Total number of bytes received or sent to ports"
                       , unit => ~b"B"
                       , collect_callback => fun io/1
                       }}
                 }}}
       , run_queue =>
           {[map],
            #{ key_elements => [[scheduler]]
             },
            #{ scheduler =>
                 {[value],
                  #{ type => union([non_neg_integer(), dcpu, dio])
                   }}
             , length =>
                 {[external_gauge_metric],
                  #{ collect_callback => fun run_queue/1
                   }}
             }}
       , gc =>
           #{ number =>
                {[external_counter_metric],
                 #{ oneliner => "Number of GCs"
                  , collect_callback => fun gc_number/1
                  }}
            , reclaimed =>
                {[external_counter_metric],
                 #{ oneliner => "Total number of bytes reclaimed by GC"
                  , unit => ~b"B"
                  , collect_callback => fun gc_reclaimed/1
                  }}
            }
       }}.

-spec reductions(lee:model_key()) -> lee_metrics:metric_data().
reductions(MKey) ->
  {Reds, _} = erlang:statistics(reductions),
  [{MKey, Reds}].

-spec context_switches(lee:model_key()) -> lee_metrics:metric_data().
context_switches(MKey) ->
  {CS, _} = erlang:statistics(context_switches),
  [{MKey, CS}].

-spec run_time(lee:model_key()) -> lee_metrics:metric_data().
run_time(MKey) ->
  {RT, _} = erlang:statistics(runtime),
  [{MKey, RT}].

-spec io(lee:model_key()) -> lee_metrics:metric_data().
io(MKey) ->
  {{input, I}, {output, O}} = erlang:statistics(io),
  [ {subst_key(MKey, [{input}]), I}
  , {subst_key(MKey, [{output}]), O}
  ].

-spec run_queue(lee:model_key()) -> lee_metrics:metric_data().
run_queue(MKey) ->
  {Normal, Dirty} = lists:split(
                      erlang:system_info(schedulers),
                      erlang:statistics(run_queue_lengths_all)),
  {L0, _} = lists:mapfoldl(
              fun(Val, RQId) ->
                  Key = subst_key(MKey, [{RQId}]),
                  {{Key, Val}, RQId + 1}
              end,
              0,
              Normal),
  case Dirty of
    [DCPU, DIO] ->
      [ {subst_key(MKey, [{dcpu}]), DCPU}
      , {subst_key(MKey, [{dio}]), DIO}
      | L0
      ];
    _ ->
      L0
  end.

subst_key(Key, []) ->
  Key;
subst_key([?children | Rest], [A|L]) ->
  [A | subst_key(Rest, L)];
subst_key([A | Rest], L) ->
  [A | subst_key(Rest, L)].

gc_number(K) ->
  {N, _, _} = erlang:statistics(garbage_collection),
  [{K, N}].

gc_reclaimed(K) ->
  {_, N, _} = erlang:statistics(garbage_collection),
  [{K, N * erlang:system_info(wordsize)}].
