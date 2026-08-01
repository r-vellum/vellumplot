# Render many plots to separate files, in parallel

`render_all()` renders a list of independent plots to separate files
across CPU cores — small multiples, a batch export, one file per group.
The work is embarrassingly parallel (one whole plot per worker), and the
result is byte-identical to rendering them one by one. Parallelism uses
process forks, so it speeds things up on macOS/Linux and falls back to
sequential on Windows; it is only worth it for several substantial
plots.

## Usage

``` r
render_all(plots, paths, workers = NULL, ...)
```

## Arguments

- plots:

  A named or unnamed list of plots.

- paths:

  Output paths, one per plot (the format of each comes from its
  extension, as in
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)).
  As a shortcut, when `plots` is **named** `paths` may be a single
  existing **directory**, and each plot is written to `<name>.png`
  inside it.

- workers:

  Number of parallel workers; `NULL` (default) uses the available cores.
  `1` forces sequential.

- ...:

  Passed to each
  [`vellum::render()`](https://r-vellum.github.io/vellum/reference/vl_scene.html)
  (e.g. `text`, `cvd`).

## Value

`paths` (the resolved file paths), invisibly.

## See also

[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md),
[`pdf_pages()`](https://r-vellum.github.io/vellumplot/reference/pdf_pages.md),
[`repeat_()`](https://r-vellum.github.io/vellumplot/reference/repeat_.md)

## Examples

``` r
plots <- list(
  wt = vplot(mtcars) |> mark_point(x = wt, y = mpg),
  hp = vplot(mtcars) |> mark_point(x = hp, y = mpg)
)
render_all(plots, tempdir())
```
