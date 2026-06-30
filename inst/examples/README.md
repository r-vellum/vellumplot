# vellumplot examples

A tour of (nearly) everything vellumplot can draw. Each script is standalone:
it loads the package, builds a handful of plots, and renders them as PNGs into
a `figures/` subfolder.

```sh
# from this directory
cd inst/examples
Rscript 01-basic-marks.R          # run one
for f in [0-9]*.R; do Rscript "$f"; done   # run them all
```

Outputs land in `inst/examples/figures/`. The scripts only use base R datasets
(`mtcars`, `datasets::penguins`, `iris`, `faithful`, …) plus a little simulated
data, so nothing beyond vellumplot needs to be installed.

| Script | Feature group |
|--------|---------------|
| `01-basic-marks.R`        | point, line, rule, segment, bar, step |
| `02-statistical-marks.R`  | histogram, density, smooth, boxplot, summary, error bars |
| `03-heatmaps-2d.R`        | tile, raster, bin2d, hex |
| `04-area-ribbon-step.R`   | area, ribbon, step directions |
| `05-color-scales.R`       | continuous / discrete / manual / gradient colour |
| `06-size-shape-scales.R`  | size and shape aesthetics |
| `07-position-scales.R`    | limits, transforms, breaks/labels, discrete order |
| `08-coordinates.R`        | cartesian zoom, flip, fixed/equal aspect |
| `09-polar-pie-donut.R`    | polar rose, radar, pie, donut |
| `10-facets.R`             | facet_wrap, facet_grid, free scales, resolve_scale |
| `11-positions.R`          | jitter, stack, dodge, fill |
| `12-themes.R`             | built-in themes, legend positions, custom elements |
| `13-labels-richtext.R`    | labs, tags, captions, `md()` rich text |
| `14-annotations.R`        | annotate text / label / point / segment / rect |
| `15-composition.R`        | concat, wrap_plots, design, spacer, inset, tags, repeat_ |
| `16-datashade-blend.R`    | datashading millions of points, auto, blend modes |
| `17-introspection.R`      | layout debug overlay, `why_size()`, per-layer SVG ids |
