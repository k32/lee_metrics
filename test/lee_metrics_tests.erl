-module(lee_metrics_tests).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("typerefl/include/types.hrl").

model() ->
  Base =
    #{ ctr =>
         {[counter_metric],
          #{ unit => ~b""
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
  [ {lee_metrics_sink_log, undefined}
  ].

model_test() ->
  {ok, _} = lee_metrics_sup:start_link(model(), sinks()),
  ?assertMatch(
     {ok, _},
     lee_metrics:new_counter([ctr], [])),
  ?assertMatch(
     {error, {no_such_metric, _}},
     lee_metrics:new_counter([wrong], [])),
  ?assertMatch(
     {ok, _},
     lee_metrics:new_counter([mapped, {1, bar}, ctr], [])),
  ?assertMatch(
     {error, {invalid_map_key, _}},
     lee_metrics:new_counter([mapped, {}, ctr], [])),
  ?assertMatch(
     {error, _},
     lee_metrics:new_counter([mapped, {bar, 1}, ctr], [])),
  timer:sleep(2_000).
