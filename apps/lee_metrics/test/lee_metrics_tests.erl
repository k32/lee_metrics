-module(lee_metrics_tests).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("typerefl/include/types.hrl").

model() ->
  Base =
    #{ ctr =>
         {[counter_metric],
          #{ unit => ~b"messages"
           }}
     , gauge1 =>
         {[gauge_metric],
          #{ unit => ~b"cm"
           , signed => true
           , aggregate => avg
           }}
     , gauge2 =>
         {[gauge_metric],
          #{ unit => ~b"cm"
           , signed => false
           , aggregate => sum
           }}
     , hist =>
         {[histogram_metric],
          #{ xunit => ~b"ms"
           , buckets => [10, 100, 1000]
           }}
     },
  External = lee_metrics_vm:model(),
  Mapped =
    #{mapped =>
        {[map],
         #{ key_elements => [[foo], [bar]]
          },
         Base
         #{ foo =>
              {[value],
               #{ type => integer()
                , default => 1
                }}
          , bar =>
              {[value],
               #{ type => atom()
                , default => bar
                }}
          }}
     },
  {ok, Cooked} =
    lee_model:compile(
      [ lee_metatype:create(lee_value)
      , lee_metatype:create(lee_map)
      , lee_metatype:create(lee_metrics_mt)
      ],
      [ Base
      , Mapped
      , External
      ]),
  Cooked.

sinks() ->
  [ #{ id => log
     , start => {lee_metrics_sink_log, start_link, []}
     , type => worker
     }
  ].

model_test_() ->
  setup(
    [ ?_assertMatch(
         {ok, _},
         lee_metrics:new_counter([ctr], []))
    , ?_assertMatch(
         {error, {no_such_metric, _}},
         lee_metrics:new_counter([wrong], []))
    , ?_assertMatch(
         {ok, _},
         lee_metrics:new_counter([mapped, {1, bar}, ctr], []))
    , ?_assertMatch(
         {error, {invalid_map_key, _}},
         lee_metrics:new_counter([mapped, {}, ctr], []))
    , ?_assertMatch(
         {error, _},
         lee_metrics:new_counter([mapped, {bar, 1}, ctr], []))
    ]).

counter_test_() ->
  setup(
    ?_test(
       begin
         {ok, C1} = lee_metrics:new_counter([ctr], []),
         {ok, C2} = lee_metrics:new_counter([ctr], []),
         ?assertMatch({ok, _, [{_, 0}]}, lee_metrics:collect([ctr])),
         lee_metrics:incr(C1, 1),
         lee_metrics:incr(C2, 2),
         ?assertMatch({ok, _, [{_, 3}]}, lee_metrics:collect([ctr])),
         ?assertMatch(ok, lee_metrics:unregister_metric([ctr], C2)),
         timer:sleep(1),
         ?assertMatch({ok, _, [{_, 1}]}, lee_metrics:collect([ctr]))
       end)).

gauge_test_() ->
  setup(
    [ ?_test(
        begin
          {ok, G1} = lee_metrics:new_gauge([gauge1], []),
          {ok, G2} = lee_metrics:new_gauge([gauge1], []),
          lee_metrics:gauge_set(G1, 1),
          lee_metrics:gauge_set(G2, 2),
          {ok, _, [{_, V1}]} = lee_metrics:collect([gauge1]),
          ?assert(V1 - 1.5 < 0.001)
        end)
    , ?_test(
         begin
           {ok, G1} = lee_metrics:new_gauge([gauge2], []),
           {ok, G2} = lee_metrics:new_gauge([gauge2], []),
           lee_metrics:gauge_set(G1, 1),
           lee_metrics:gauge_set(G2, 2),
           ?assertMatch({ok, _, [{_, 3}]}, lee_metrics:collect([gauge2]))
         end)
    ]).

histogram_test_() ->
  setup(
    [ ?_test(
         begin
           {ok, H1} = lee_metrics:new_histogram([hist], []),
           {ok, H2} = lee_metrics:new_histogram([hist], []),
           ?assertEqual(
              [{[hist], [{10, 0}, {100, 0}, {1000, 0}, {infinity, 0}]}],
              collect([hist])),
           ok = lee_metrics:histogram_observe(H1, 1),
           ok = lee_metrics:histogram_observe(H2, 10000),
           ?assertEqual(
              [{[hist], [{10, 1}, {100, 0}, {1000, 0}, {infinity, 1}]}],
              collect([hist])),
           ok = lee_metrics:histogram_observe(H1, 11),
           ok = lee_metrics:histogram_observe(H2, 101),
           ?assertEqual(
              [{[hist], [{10, 1}, {100, 1}, {1000, 1}, {infinity, 1}]}],
              collect([hist]))
         end)
    ]).

external_test_() ->
  setup(
    [ ?_assertMatch([_], collect([erlang, processes, reductions]))
    , ?_assertMatch([_], collect([erlang, context_switches]))
    , ?_assertMatch([_], collect([erlang, processes, gc, number]))
    , ?_assertMatch([_], collect([erlang, processes, gc, reclaimed]))
    , ?_test(
         begin
           [In, Out] = collect([erlang, ports, io, {}, bytes]),
           ?assertMatch({[erlang, ports, io, {input}, bytes], I} when is_integer(I), In),
           ?assertMatch({[erlang, ports, io, {output}, bytes], I} when is_integer(I), Out)
         end)
    , ?_assertMatch([_], collect([erlang, ports, count]))
    , ?_assertMatch([_], collect([erlang, ports, limit]))
    , ?_test(
         begin
           Values = collect([erlang, schedulers, run_queue, {}, length]),
           [DCPU, DIO, N0 | _] = Values,
           ?assertMatch({[erlang, schedulers, run_queue, {dcpu}, length], I} when is_integer(I), DCPU),
           ?assertMatch({[erlang, schedulers, run_queue, {dio}, length], I} when is_integer(I), DIO),
           ?assertMatch({[erlang, schedulers, run_queue, {0}, length], I} when is_integer(I), N0)
         end)
    , ?_test(
         begin
           Values = collect([erlang, schedulers, microstate_accounting, {}, counters, {}, value]),
           ?assertMatch([_ | _], Values)
         end)
    , ?_assertMatch([_], collect([erlang, atoms, count]))
    , ?_assertMatch([_], collect([erlang, atoms, limit]))
    , ?_assertMatch([_], collect([erlang, schedulers, online]))
    , ?_assertMatch([_], collect([erlang, schedulers, dirty_cpu_online]))
    , ?_assertMatch([_], collect([erlang, schedulers, dirty_io]))
    , ?_assertMatch([_], collect([erlang, schedulers, run_time]))
    ]).

setup(Body) ->
  {setup,
   fun() -> {ok, Pid} = lee_metrics_sup:start_link(model()), Pid end,
   fun(Pid) -> unlink(Pid), lee_metrics_sup:stop() end,
   Body}.

collect(MKey) ->
  {ok, _MNode, Instances} = lee_metrics:collect(MKey),
  lists:foreach(
    fun({Key, _}) ->
        ?assertEqual(MKey, lee_model:get_model_key(Key))
    end,
    Instances),
  Instances.
