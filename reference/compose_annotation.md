# Annotate a composition

Add figure-level text (a `title`, `subtitle`, `caption`) spanning the
whole composition, and/or auto-tag the sub-plots (`A`, `B`, `C`, …).
Because every `PlotComposition` carries its own annotation, this works
at any nesting level (unlike patchwork, where annotation is top-level
only).

## Usage

``` r
compose_annotation(
  plot,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag_levels = NULL,
  tag_prefix = "",
  tag_suffix = ""
)
```

## Arguments

- plot:

  A `PlotComposition` (from
  [`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
  /
  [`hconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
  /
  [`vconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md)).

- title, subtitle, caption:

  Figure-level text (or `NULL`).

- tag_levels:

  Auto-tag style: `"A"`, `"a"`, `"1"`, `"i"`, or `"I"` (`NULL` = no
  tags).

- tag_prefix, tag_suffix:

  Strings wrapped around each tag.

## Value

The modified `PlotComposition`.

## Examples

``` r
a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
b <- vplot(mtcars) |> mark_point(x = hp, y = mpg)
hconcat(a, b) |> compose_annotation(title = "Fuel economy", tag_levels = "A")
```
