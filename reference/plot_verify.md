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

A list with `ok` (data *and* fonts match), `data_ok`, `expected` /
`actual` (the embedded vs recomputed data hash), `fonts_ok`, and
`fonts_missing` (the recorded font paths absent on this machine). Errors
if the SVG carries no manifest.

## Details

A font mismatch is reported as a **distinct outcome** from a data
mismatch: `data_ok` is whether the data still hashes the same,
`fonts_ok` whether every font the figure was drawn with is still present
on this machine, and `ok` their conjunction. A pixel difference with
`data_ok = TRUE` but `fonts_ok = FALSE` is the font stack, not your data
— a different cause with a different fix (install/register the font)
than a data change.

## See also

[`plot_manifest()`](https://r-vellum.github.io/vellumplot/reference/plot_manifest.md),
[`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md)

## Examples

``` r
svg <- plot_svg(vplot(mtcars) |> mark_point(x = wt, y = mpg), manifest = TRUE)
plot_verify(svg, mtcars)$ok
#> [1] TRUE
```
