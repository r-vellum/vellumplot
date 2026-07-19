# Date and time position scales

Position scales for `Date` (`scale_*_date`), date-time `POSIXct`
(`scale_*_datetime`), and time-of-day / `difftime` / `hms`
(`scale_*_time`) axes. A `Date`/`POSIXct` column already gets a sensible
date axis automatically; declare one of these to control the break
interval or the label format.

## Usage

``` r
scale_x_date(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_y_date(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_x_datetime(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_y_datetime(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_x_time(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)

scale_y_time(
  plot,
  limits = NULL,
  date_breaks = NULL,
  date_labels = NULL,
  breaks = NULL,
  labels = NULL,
  name = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- limits:

  Length-2 vector giving the axis range (same class as the data), or
  `NULL` to train from the data.

- date_breaks:

  A break interval string, e.g. `"1 month"`, `"2 weeks"`, `"10 years"`
  (passed to
  [`scales::breaks_width()`](https://scales.r-lib.org/reference/breaks_width.html)).
  `NULL` uses [`pretty()`](https://rdrr.io/r/base/pretty.html).

- date_labels:

  A [`base::strftime()`](https://rdrr.io/r/base/strptime.html) format
  string for the tick labels, e.g. `"%b %Y"`. `NULL` uses the default
  format.

- breaks, labels:

  Explicit break positions / labels (override
  `date_breaks`/`date_labels`), or `NULL`.

- name:

  Axis title, or `NULL` to derive from the encoding.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
df <- data.frame(day = as.Date("2020-01-01") + 0:364, y = cumsum(rnorm(365)))
vplot(df) |>
  mark_line(x = day, y = y) |>
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
```
