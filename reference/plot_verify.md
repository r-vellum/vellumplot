# Verify a rendered figure against its data

`plot_verify()` extracts the manifest embedded in an SVG written with
[`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md)`(manifest = TRUE)`
and recomputes the data fingerprint from `data`, reporting whether the
figure still matches the data it was drawn from — a lightweight,
self-contained reproducibility check.

## Usage

``` r
plot_verify(svg, data)
```

## Arguments

- svg:

  An SVG string (from
  [`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md))
  or a path to an `.svg` file.

- data:

  The data frame to check the figure against.

## Value

A list with `ok` (logical), `expected` (the embedded data hash), and
`actual` (the recomputed hash). Errors if the SVG carries no manifest.

## See also

[`plot_manifest()`](https://r-vellum.github.io/vellumplot/reference/plot_manifest.md),
[`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md)

## Examples

``` r
svg <- plot_svg(vplot(mtcars) |> mark_point(x = wt, y = mpg), manifest = TRUE)
plot_verify(svg, mtcars)$ok
#> [1] TRUE
```
