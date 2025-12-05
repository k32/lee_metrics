-ifndef(LEE_METRICS_HRL).
-define(LEE_METRICS_HRL, true).

%% [counter_metric, rate_metric, gauge_metric, histogram_metric, rolling_average_metric].
-define(lee_metric_types,
        [ counter_metric
        , gauge_metric
        , external_counter_metric
        , external_gauge_metric
        , histogram_metric
        ]).

-endif.
