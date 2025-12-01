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
     },
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

setup(Body) ->
  {setup,
   fun() -> {ok, Pid} = lee_metrics_sup:start_link(model(), []), Pid end,
   fun(Pid) -> unlink(Pid), lee_metrics_sup:stop() end,
   Body}.
