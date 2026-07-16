# Secondary axes

Add a secondary axis to a continuous position scale: a second set of
ticks and labels on the opposite edge (top for `x`, right for `y`),
computed as a 1:1 monotonic transform of the primary axis. Pass the
result to the `sec.axis` argument of
[`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
/
[`scale_y_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md).
This is a labelling convenience (unit conversions, count/proportion dual
readouts, duplicating an axis on a wide plot) — **not** an independent
second axis with its own data.

## Usage

``` r
sec_axis(transform = ~., name = NULL, breaks = NULL, labels = NULL)

dup_axis(transform = ~., name = NULL, breaks = NULL, labels = NULL)
```

## Arguments

- transform:

  The primary-to-secondary mapping: a formula using `.` (e.g.
  `~ . * 1.8 + 32`), a function, or a
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html)-style
  transform object. Must be monotonic over the axis range. Defaults to
  the identity, `~ .`.

- name:

  Secondary axis title, or `NULL` for none.

- breaks:

  Break positions in **secondary** units, or `NULL` to compute them.

- labels:

  A character vector of labels (one per break) or a labelling function,
  or `NULL` for the default number format.

## Value

A `SecAxisSpec` to pass to `sec.axis`.

## Details

`sec_axis()` maps the primary axis through `transform`; `dup_axis()` is
the identity special case (a plain duplicate) with tidy defaults.

## Limitations (current version)

Secondary axes are supported only on continuous position scales under
the default Cartesian coordinate system, with **shared** scales across
facets. Combining `sec.axis` with
[`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
/
[`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
/
[`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md),
with free facet scales, or with
[`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)
raises an error. In a plot composition (`|` / `/`) the secondary axis is
not drawn.

## See also

[`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)

## Examples

``` r
# Celsius with a Fahrenheit axis on top
vplot(data.frame(t = 0:100, y = (0:100)^2)) |>
  mark_line(x = t, y = y) |>
  scale_x_continuous(name = "°C", sec.axis = sec_axis(~ . * 1.8 + 32, name = "°F"))


# Duplicate the y axis on the right
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  scale_y_continuous(sec.axis = dup_axis())
```
