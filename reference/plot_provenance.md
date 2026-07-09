# Inspect the compiled-scene provenance of a plot

Compiles `x` and returns its *provenance table*: one record per emitted
mark grob, tying each low-level primitive back to the data rows and
trained scales that produced it. Each record's `id` matches the grob's
`data-vellum-id` in the SVG output (and the `id` column of
[`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html)),
so it is a stable join key between the rendered scene and the grammar —
the substrate for interactivity, linked views, and accessibility.

## Usage

``` r
plot_provenance(x)
```

## Arguments

- x:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  or plot composition.

## Value

A list of provenance records (see Details). Empty for a plot that emits
no mark grobs.

## Details

Each record is a plain, serializable list:

- `id`:

  stable node id, equal to `grob@id` / SVG `data-vellum-id` (join key).

- `layer`:

  1-based layer index within the (sub-)plot spec.

- `mark`:

  mark type (`"point"`, `"line"`, …).

- `kind`:

  `"mark"` (the core layer) or `"effect"` (a decorative underlay).

- `panel`:

  facet panel key (`"panel-r-c"`), or `NA` for a single panel.

- `channels`:

  aesthetic → trained-scale reference (`scale`, `type`, `domain`).

- `rows`:

  the original input-data row indices this grob draws.

The `rows` field is refined per element for marks that map rows
one-to-one to drawn elements (points, bars, tiles, segments, lines,
areas, ribbons, steps, text, boxplots, sf features, edges); for
aggregating marks (histogram, density, smooth, datashade, …) it is the
whole layer, since rows no longer map to single elements.

## See also

[`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html)
and the scene-contract vignette in vellum.

## Examples

``` r
df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, model = rownames(mtcars))
p <- vplot(df) |> mark_point(x = wt, y = mpg, data_id = model)
prov <- plot_provenance(p)
prov[[1]]$id
#> [1] "layer-1-point-g1"
```
