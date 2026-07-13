# Changelog

## vellumplot (development version)

### Bug fixes

- **`annotate("rect", ...)` honours infinite bounds.** A bound of
  `-Inf`/`Inf` (e.g. `xmin = -Inf, xmax = Inf` for a full-width band)
  now extends the rectangle to the panel edge, matching ggplot2, instead
  of silently rendering nothing
  ([\#29](https://github.com/r-vellum/vellumplot/issues/29)).

- **Continuous labels no longer group thousands by default.** A default
  continuous axis or legend now renders 4-digit-plus values without a
  grouping separator, so years and IDs read `2010` instead of `2 010`
  ([\#27](https://github.com/r-vellum/vellumplot/issues/27)). Pass
  explicit `labels` to restore grouping.

- **More inputs fail fast.** A `design` layout area must be a solid
  rectangle (and
  [`area()`](https://r-vellum.github.io/vellumplot/reference/area.md)
  requires `t <= b`, `l <= r`); a per-row interactivity value
  (`tooltip`/`data_id`/…) must be length 1 or the data’s row count; and
  [`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)
  requires a cleanly numeric column. Each now errors clearly instead of
  silently mis-rendering.

- **Clearer alt text for grid facets.** A
  [`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  plot’s automatic alt text now distinguishes the row and column
  variables (e.g. “Faceted by rows cyl, columns am”) instead of running
  them together.

- **Compositions can be themed.**
  [`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  now accepts a composition (from \[concat()\] / \[hconcat()\] /
  \[vconcat()\]) and styles its figure-level chrome — the
  title/subtitle/caption bands, the collected legend, panel spacing, and
  tags. Previously a composition’s figure chrome always used the default
  theme. Each sub-plot still carries its own theme.

- **Gradient fills fail clearly on marks that don’t support them.** A
  `fill = linear_gradient(...)` is painted by
  [`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  and
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md);
  on any other mark it now raises a clear error instead of leaking an
  undefined paint into the renderer.

- **Inputs are validated up front.**
  [`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  now checks `panel.spacing`, `plot.margin`, `aspect.ratio`, and
  `axis.ticks.length` (and rejects `NA`/`Inf` in any numeric setting);
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)
  checks `width`/`height` the way it already checked `dpi`; a discrete
  `shape`/`linetype` scale with more categories than its palette errors
  instead of silently reusing a glyph; and a binned colour scale given
  fewer than two breaks errors instead of crashing in
  [`colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).
  Valid input is unaffected.

- **Legends read consistently.** A horizontal (top/bottom) continuous
  colour legend now draws the `NA` key that the vertical one always did.
  `guide_legend(reverse = TRUE)` now actually flips a continuous
  colourbar (previously a silent no-op) and no longer desyncs a binned
  colour scale’s boundaries from its swatches. A merged colour+shape
  legend’s swatches are now tagged for interactive highlight/select.
  Minor gridlines past the outermost breaks use the local break spacing
  at each end, so they sit correctly when breaks are unevenly spaced.

- **Numeric facets order numerically.** Faceting on a numeric variable
  now orders panels `1, 2, 10` rather than the lexicographic `1, 10, 2`;
  multi-variable facets order by each variable’s own type.

- **Log/reversed axes.** Bars and areas on a `scale_*(trans = "log10")`
  (or `"sqrt"`) axis draw from the axis floor instead of a degenerate
  shape (the zero baseline that maps to `-Inf` is clamped into the
  trained range). A descending limit such as `ylim(hi, lo)` on a
  bar/area is no longer silently un-reversed by the zero baseline.

- **Grouped histogram density.** `after_stat(density)` /
  `after_stat(prop)` on a grouped histogram or bar now normalizes per
  group (each group integrates/sums to 1), matching ggplot2, rather than
  dividing by the grand total.

- **Clearer errors instead of silent surprises or crashes.**
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)
  rejects a `"reverse"` transform (a no-op there — use
  `scale_*(trans = "reverse")`) and rejects bar/area marks on a
  nonlinearly transformed axis (a zero baseline has no place on a log
  display). Histogram / 2-D bin / hexbin stats abort cleanly on all-`NA`
  input instead of a cryptic [`seq()`](https://rdrr.io/r/base/seq.html)
  error.
  [`mark_raster()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  and z-surface contours reject a grid with a duplicated cell
  (previously a silent transparent/`NA` hole). Composition auto-tags
  continue past 26 sub-plots (`Z`, `AA`, `AB`, …) instead of `NA`.

- **[`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  now stacks.** `mark_area(position = ...)` was silently ignored; it
  gains a `position` argument (`"stack"` default, plus `"fill"` and
  `"identity"`), so areas sharing an `x` with a mapped `fill`/`color`
  combine into a band instead of overlapping from the zero baseline. An
  area with no fill mapping is unchanged.

- **Mapped aesthetics that were silently dropped now take effect.** A
  mapped `fill` on
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  colours the label background (previously it always fell back to
  white); a mapped or constant `alpha` is honoured per category/tick by
  [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md),
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md),
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md),
  [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  and
  [`mark_rug()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  (previously collapsed to one value or ignored), and
  [`mark_rug()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  also honours a mapped `color`.

- **British spelling works everywhere.**
  `hover_colour`/`selected_colour` are now recognised as interactivity
  arguments (previously only the American spelling was), and
  [`mark_bin2d()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)/[`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  no longer add a default count fill when a British `colour =` is
  supplied.

- **[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  warns on a layout-column clash.** A vertex/edge attribute named
  `x`/`y` (or `xend`/`yend`) is overwritten by the layout coordinates;
  that used to happen silently and now emits a warning naming the
  attribute.

### New features

- **[`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)
  — nonlinear display transforms.** Warps the *display* of one or both
  position axes after the scale has trained, so gridlines bunch up and
  straight lines curve while the axis keeps its original data-value
  labels (unlike `scale_*(trans=)`, which rescales the data and relabels
  in transformed space). Each of `x`/`y` takes a transform name
  (`"log10"`, `"sqrt"`, `"identity"`) or a `scales::transform_*()`
  object, e.g. `coord_trans(y = "log10")`. Supports the common marks
  (points, lines, areas/ribbons, bars, tiles, smooths, text, …);
  segment/interval, boxplot, edges, and raster/datashade marks are not
  warped yet and raise a clear error under
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md).

- **[`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  and
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  no longer clip at the edges.** The position scales are now trained to
  include each mark’s drawn footprint – the full kernel-density support
  for a violin, and the ridge height (`scale`) above the top category
  for a ridgeline – so the tallest ridge and the density tails are no
  longer cropped by the panel. Explicit axis limits still take
  precedence.

- **Marginal distributions with
  [`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md).**
  A new plot modifier draws a density or histogram of the panel’s `x`
  variable along the top edge and of its `y` variable along the right
  edge, each sharing the scatter’s axis so they line up (the vellumplot
  analogue of `ggExtra::ggMarginal()`). Pick the distribution with
  `type = "density"` / `"histogram"`, the edges with `sides` (`"t"`,
  `"r"`, `"tr"`), and the extent with `size`; `group = TRUE` splits each
  marginal by a discrete `color`/`fill` mapping in the matching palette.
  It reads `x`/`y` from the first plain (identity-stat) layer and reuses
  the existing density/bin stats, so no axes or legends are duplicated.
  This version supports a single panel only (an error is raised with
  facets, a non-Cartesian coordinate system, or a locked aspect ratio).

- **Positional constants now render.** A bare literal on a coordinate
  channel (e.g. `mark_segment(x = i, y = 0, xend = i, yend = value)` for
  a lollipop baseline) is now treated as a constant-valued *coordinate*
  rather than a style param, so it populates the layer’s values and
  trains the position scale. Previously `y = 0` was dropped from scale
  training, collapsing each segment’s start onto its end (zero-length,
  invisible), and a segment-only plot errored with “Every layer needs an
  x and y encoding”. Segment/edge endpoints (`xend` / `yend`) now also
  extend the axis domain, so a segment-only plot derives its range from
  both ends. Affects the positional channels `x`, `y`, `xend`, `yend`,
  `xmin`, `xmax`, `ymin`, `ymax`.

- **Faster
  [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  on large maps.** Non-interactive sf layers now batch all features that
  share a resolved fill/colour into a single grob per style group
  (polygons into one `evenodd` `path_grob`, lines into one NA-separated
  `lines_grob`), instead of one grob per feature, and the layer’s
  bounding box is computed in a single linear pass rather than by
  growing coordinate vectors. On a 40k-polygon grid this cut scene
  compilation from ~24s to ~0.8s. Interactive layers (those declaring
  `data_id` / `tooltip` / `hover_group`) are unchanged — they still emit
  one keyed grob per feature so every feature stays addressable. Note:
  because a batched polygon group is filled with one `evenodd` rule, two
  same-fill features that *overlap* would cancel in the overlap region
  (the same property a self-overlapping `MULTIPOLYGON` already has);
  disjoint features — every real choropleth and coverage map — are
  unaffected.

## vellumplot 0.3.0

- Adopted vellum’s renamed `vl_*` graphics primitives (`vl_gpar()`,
  `vl_unit()`, `vl_viewport()`), which no longer mask grid.

- **NA legend keys for `size` / `shape`.** A mapped `size` or `shape`
  aesthetic whose data contains missing values now shows an “NA” key (as
  the colour scales already did). Also fixes a crash: NA in a `shape`
  mapping previously errored (`Unknown point shape: NA`) — it now draws
  as a neutral circle.

- **2-D density contours.**
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  draws iso-density contour lines of a 2-D point cloud and
  [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  fills the bands between them (coloured by level automatically). By
  default the field is a kernel density estimate (via
  [`MASS::kde2d()`](https://rdrr.io/pkg/MASS/man/kde2d.html)); map a `z`
  aesthetic to contour a supplied surface over a regular `x`/`y` grid
  instead. Contour tracing uses the `isoband` package (both `isoband`
  and `MASS` are Suggests).

- **Binned position scales.**
  [`scale_x_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_x_binned.md)
  /
  [`scale_y_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_x_binned.md)
  cut a continuous axis into bins — ticks at the bin boundaries, each
  datum drawn at its bin centre — reusing the binned-colour
  classification (`style`/`n`, or explicit `breaks`).
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  sizes to the bin width.

- **Rich and multi-line text labels.**
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  now carries rich labels through to the renderer instead of flattening
  them: map `label = md(<expr>)` for a per-datum styled label
  (bold/italic/super-/subscript/colour), and plain labels may contain
  newlines (`\n`) to wrap onto stacked lines. Requires vellum’s
  development version.
  ([`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)’s
  background box does not yet support rich labels.)

- **Accessibility (alt text).** Every compiled plot is now an accessible
  image by default.
  [`plot_alt()`](https://r-vellum.github.io/vellumplot/reference/plot_alt.md)
  returns a plot’s text alternative — an author-written string from the
  new `labs(alt = )`, or a prose summary vellumplot generates from the
  spec (chart type, x/y/colour/size mappings, observation count,
  faceting). At the compile seam the plot **title** becomes the scene’s
  accessible name and this alt text becomes its description, so
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  output carries `role="img"` + `<title>`/`<desc>` in SVG and a tagged
  `Figure` with `Alt` in PDF (via vellum). See the new *Accessibility*
  article. Requires vellum’s development version (\>= 0.1.1.9000) for
  the accessible SVG/PDF backend.

- **British spelling.** `mark_*(colour = )` is now honoured as an alias
  for `color` (previously the mapping was silently dropped and no colour
  scale was trained). Added `scale_colour_*()` aliases for the
  `scale_color_*()` family.

- **Faithful alt text.** The auto-generated description now matches what
  the plot actually renders: it honours `scale_*(name = )` axis/legend
  titles, names the implicit “count” axis of a count bar, and describes
  a network graph by its node and edge counts instead of its
  (meaningless) layout x/y axes.

## vellumplot 0.2.1

Consumes `vellum`’s new compound `native + mm` unit (requires vellum \>=
0.1.1).

- **Label nudges**:
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  /
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  gain `nudge_x` / `nudge_y` (in millimetres, `+x` right / `+y` up) that
  shift a label by an exact absolute distance, device-resolved so the
  nudge is constant regardless of scale or panel aspect.
- **Device-exact drop shadows**:
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)’s
  `x` / `y` offset is now an absolute distance in **millimetres** (was a
  panel-relative npc fraction), so a drop shadow stays the same physical
  distance and is isotropic on non-square panels. Defaults changed
  accordingly (a small down-right drop). Replaces the previous
  npc-fraction workaround.

Device-space dodge (`mark_bar`) and jitter (`mark_point`) remain
data-space for now; converting them to the compound unit is deferred (it
changes existing rendered output and needs snapshot review).

## vellumplot 0.2.0

Grammar-breadth release: new scales, mapped aesthetics, legend control,
and distribution marks, closing the most conspicuous gaps versus
ggplot2.

### Scales & axes

- **Date/time scales**:
  [`scale_x_date()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  /
  [`scale_x_datetime()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  /
  [`scale_x_time()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  (and `y` twins) with `date_breaks` (interval strings like
  `"3 months"`) and `date_labels` (strftime formats). `Date`/`POSIXct`
  columns still get a sensible date axis automatically.
- **New mapped aesthetics**:
  [`scale_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md)
  (continuous opacity) and
  [`scale_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_linetype.md)
  (discrete line types), each drawn per element with its own legend.
  `alpha` mapped to data now varies opacity (previously constant-only);
  `linetype` applies to
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  /
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md).
- **Identity scales**:
  [`scale_color_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md),
  [`scale_fill_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md),
  [`scale_size_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md),
  [`scale_shape_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md),
  [`scale_alpha_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md),
  [`scale_linetype_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  — use data values verbatim, no legend.
- **Limit shortcuts**:
  [`xlim()`](https://r-vellum.github.io/vellumplot/reference/lims.md),
  [`ylim()`](https://r-vellum.github.io/vellumplot/reference/lims.md),
  and
  [`lims()`](https://r-vellum.github.io/vellumplot/reference/lims.md).
- **Legend control**:
  [`guides()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  with `guide = "none"` (hide a legend), `guide_legend(reverse = TRUE)`
  (reverse key order), and `guide_legend(title=)`.

### Marks & stats

- **Distribution marks**:
  [`mark_ecdf()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  (empirical CDF step),
  [`mark_rug()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  (marginal ticks),
  [`mark_qq()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md) +
  [`mark_qq_line()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  (quantile-quantile plot).
- **Density-shape marks**:
  [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  (mirrored density per category),
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  (overlapping per-category densities), and
  [`mark_dotplot()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  (binned, stacked dots). Violin and ridgeline carry per-category
  provenance.

### Not yet implemented (still planned)

Secondary axes (`sec_axis`/`dup_axis`); 2-D density / contour
(`mark_contour`/`stat_density_2d`); position-binned scales
(`scale_x_binned`);
[`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)
and free non-position scales across facets; triple-merge legends
(colour+shape+size) and NA keys for size/shape.

## vellumplot 0.1.1

- New exported
  [`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md):
  returns the compiled-scene provenance table — one record per emitted
  mark grob, tying each low-level primitive back to the data rows and
  trained scales that produced it, with an `id` that matches the grob’s
  `data-vellum-id` in SVG and the `id` column of
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html).
  This is the first public consumer of the provenance metadata
  (previously internal), the substrate for interactivity, linked views,
  and accessibility.
- Row-key provenance is now refined per element for the line, area,
  ribbon, step, text, and boxplot marks (previously only
  point/bar/tile/segment/sf/ edges), so a record’s `rows` resolve to the
  actual data rows an element draws rather than the whole layer. See the
  scene-contract vignette in `vellum`.

## vellumplot 0.1.0

First release. vellumplot is a declarative, pipe-first grammar of
graphics that compiles an inspectable spec into a `vellum` scene, with
faceting, coordinate systems, and multi-plot composition. Everything
below ships in this first release.

### Features

- **Interactivity declarations** (host-agnostic; inert on a static
  render). Any `mark_*()` accepts reserved per-row args `data_id`,
  `tooltip`, and `hover_group` (tidy-eval expressions). They flow into
  the vellum scene as per-element keys/metadata — `data_id` becomes the
  SVG `data-key` and both surface in
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html)
  — the foundation a companion widget uses for hover/select/linking. A
  plot without them compiles and renders exactly as before. Applies to
  `stat = "identity"` marks (points, bars, tiles, segments, edges,
  hexbins, polar bars) and to
  [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  — each sf feature (polygon/linestring/point) becomes an addressable,
  keyed element.

- **Per-element interaction styling**: marks also accept `hover_color`
  and `selected_color` (constant or column-mapped), carried into the
  scene so a host (`vellumwidget`) outlines each element in its own
  colour on hover/select — overriding the widget-wide theme.

- **Interactive discrete legends**: when a plot maps a discrete
  `color`/`shape` scale and declares interactivity, each legend swatch
  is tagged with the data series it represents, and every mark carries
  its series membership. A host (`vellumwidget`) uses this to make
  swatches highlight/select their whole series. Inert on a static render
  and when no interactivity is declared.

- **Auto-display**: printing a plot (or composition) draws it into the
  active graphics device — the RStudio / Positron Plots pane, or a
  knitr/Quarto chunk — like ggplot2 (via
  [`vellum::display()`](https://r-vellum.github.io/vellum/reference/display.html)).
  [`summary()`](https://rdrr.io/r/base/summary.html) shows the
  inspectable spec tree instead;
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  still writes a file.

- [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)
  starts an inspectable, serializable `PlotSpec`.

- **Output resolution**: `vplot(dpi =)` sets the authored resolution and
  `render_plot(dpi =)` overrides it per render, so an exported PNG’s
  pixel dimensions are `width * dpi` by `height * dpi`. Compositions
  inherit the first sub-plot’s `dpi` (or take an explicit
  `concat(dpi =)`).

- Marks:
  [`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  (reference lines via `xintercept` / `yintercept`), and
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  (uses explicit `y` heights, or counts rows per category when `y` is
  omitted).

- Encodings captured with tidy evaluation: `x`, `y`, `color`/`fill`,
  `size`, `shape`, `alpha`. Scalar values become constant aesthetics.

- Scales:
  [`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  /
  [`scale_y_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  (linear and `log10`) with auto-trained domains and ggplot-style
  expansion; **discrete (band) position scales** are trained
  automatically for categorical `x`/`y` (bars);
  [`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  (perceptual ramp),
  [`scale_color_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  (qualitative palette), and a trained **size scale** for a mapped
  `size`.

- Guides: trained x/y axes (breaks, labels, titles), a grey panel with
  white gridlines, and a legend area that **stacks multiple guides** — a
  colour legend (continuous gradient bar or discrete swatches) and a
  size legend.

- **Legend layout** is measured in millimetres: each guide is sized to
  its content (a title line above one row per key), the row pitch is
  driven by the key’s drawn size so large bubble keys never overlap, and
  the guide block is centred in the legend track. Titles sit directly
  above their keys, horizontal (top/bottom) legends pack keys tightly
  instead of spreading them across the full width, and a vertical legend
  uses the full figure height.

- **Legend keys match the mark**: a colour legend draws the glyph the
  plot actually uses — a filled circle for point layers, a short line
  for line-like layers (line/step/smooth/segment/…), and a filled square
  for bar/area/tile/ polygon layers — instead of always a square swatch.

- **Legend geometry is themeable**:
  `theme(legend.key.size=, legend.spacing=, legend.margin=)` set the
  key/swatch side, the gap between stacked guides, and the inset around
  the legend block (all in millimetres).

- Continuous colour bars now carry **white break ticks** aligned to
  their labels, on both vertical and horizontal (top/bottom) legends.

- **Merged legends**: mapping one variable to two aesthetics draws a
  single legend whose keys carry both encodings — discrete `colour` +
  `shape` become coloured shape keys; continuous `colour` + `size`
  become colour-graded, size-graduated points. Merging follows ggplot2’s
  rule (same title and breaks/levels); give one scale a different
  `name=` to keep them separate.

- Layering: multiple marks on one panel, with scales trained across all
  layers.

- **Faceting**: `facet_wrap(~var)` and `facet_grid(rows ~ cols)` split
  the data into a panel grid with facet strips and aligned, shared axes.

- **Scale resolution**
  ([`resolve_scale()`](https://r-vellum.github.io/vellumplot/reference/resolve_scale.md)
  / `facet_*(scales=)`): position scales are shared across panels by
  default; opt into `"free_x"` / `"free_y"` / `"free"` (independent per
  panel) for per-panel ranges and axes.

- **Statistical transforms**:
  [`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  (bin a continuous variable into count bars) and
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  (an `"lm"` fit drawn as a line with an optional confidence ribbon).
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  with no `y` uses the count stat. Map computed variables with
  [`after_stat()`](https://r-vellum.github.io/vellumplot/reference/after_stat.md),
  e.g. `y = after_stat(density)`.

- **Position adjustments** (`position =`): grouped bars **stack** by
  default; `"dodge"` places them side by side and `"fill"` normalises
  each group to 1. `mark_point(position = "jitter")` spreads overlapping
  points.

- **Datashading**:
  [`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md)
  aggregates a large point cloud into a density raster (via
  [`vellum::datashade()`](https://r-vellum.github.io/vellum/reference/datashade.html))
  that fills the panel — cost independent of point count.
  `mark_point(auto = TRUE)` switches to this automatically above ~50k
  rows.

- **More marks**: areas/ribbons
  ([`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)),
  steps
  ([`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)),
  intervals
  ([`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md)),
  [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  tiles/heatmaps
  ([`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_raster()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)),
  2-D binning
  ([`mark_bin2d()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)),
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  /
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md),
  and pie/donut shortcuts
  ([`mark_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md),
  [`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)).

- **More statistical transforms**:
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  (kernel density),
  [`mark_summary()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  (aggregate `y` per category), in addition to the histogram / binning /
  smooth stats. Map computed variables with
  [`after_stat()`](https://r-vellum.github.io/vellumplot/reference/after_stat.md).

- **Coordinate systems**:
  [`coord_cartesian()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  (view-window zoom),
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
  [`coord_fixed()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  /
  [`coord_equal()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  (aspect lock), and
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  (pie / coxcomb / radar).

- **Themes**:
  [`theme_gray()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  (default),
  [`theme_minimal()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_bw()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_classic()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_void()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  and
  [`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  for ad-hoc overrides (panel background, gridlines, text, strip
  background, legend position, margins).

- **Effects & paints**: stroked and point marks take a layer
  `effects = list(...)` argument —
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md)
  (neon halo),
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md)
  (contrasting halo for legibility), and
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)
  (drop / ambient).
  [`linear_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
  /
  [`radial_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
  can be used as a `fill` value (area / ribbon / bar), and
  [`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  ties glow + gradients + a neon palette into the
  [mplcyberpunk](https://github.com/dhaitz/mplcyberpunk) look.

- **Hand-drawn (“sketch”) rendering**:
  [`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
  (re-exported from vellum) gives any geometry mark a wobbly,
  hachure-filled [Rough.js](https://roughjs.com) look via a `sketch =`
  argument. Unlike a layer effect it is a *geometry* property, generated
  natively in the engine, so it is exact, cross-backend, and works in
  PDF. It rides three levels, most-specific-wins: a mark’s `sketch =`,
  an
  [`element_line()`](https://r-vellum.github.io/vellumplot/reference/element.md)
  /
  [`element_rect()`](https://r-vellum.github.io/vellumplot/reference/element.md)
  `sketch =` slot, or the plot-wide
  [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)
  one-liner (hand-drawn gridlines, axes, ticks, marks and legend keys on
  a paper background). `sketch = NA` forces an element crisp; text is
  never sketched (pair a handwriting font).

- **Composition**:
  [`hconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`vconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  and
  [`wrap_plots()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
  arrange several plots on a grid; the aligned path lines up panel edges
  and can **collect guides** across sub-plots.
  [`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)
  overlays a plot, and
  [`compose_annotation()`](https://r-vellum.github.io/vellumplot/reference/compose_annotation.md)
  adds figure-level titles and auto-tags (`A`, `B`, …).

- **Repeat**:
  [`repeat_()`](https://r-vellum.github.io/vellumplot/reference/repeat_.md)
  replicates a plot across a set of fields, zipping one or more
  encodings to produce a composition.

- **Blend modes**: marks take a `blend =` argument (the CSS
  `mix-blend-mode` set — `"multiply"`, `"screen"`, `"darken"`, …). The
  layer composites as one isolated group against the panel and earlier
  layers, e.g. two overlapping translucent layers under `"multiply"`.

- **Spatial**:
  [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  draws the geometry column of an `sf` object as a map layer (polygons /
  lines / points), with
  [`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)
  to reproject to a target CRS and lock the map aspect ratio;
  [`scale_fill_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  /
  [`scale_color_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  bin a continuous fill/colour into discrete classes for choropleths.

- **Network**:
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  starts a node-link diagram from an `igraph` graph — it runs a layout
  (stress majorization by default, via `graphlayouts`;
  `"sparse_stress"`, `"backbone"`, `"fr"`, `"circle"`, … or a
  matrix/function), builds a node and an edge table, and locks the
  aspect with a void theme.
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  draws edges (straight, batched; reciprocal/parallel edges offset off
  the centre line, self-loops nested, optional `arrow`),
  [`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  the vertices, and
  [`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  the labels — with a fixed edges-under-nodes draw order. Edges are
  capped exactly at each endpoint’s node boundary (per vertex, at any
  resolution), so arrowheads land on the node edge.
  [`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)
  maps a weight to edge width with its own legend. `igraph` /
  `graphlayouts` are optional (`Suggests`). See
  `_docs/DESIGN-igraph.md`.

- Output: `render_plot(plot, path)`; `vellum::render(plot, path)` and
  `print(plot)` also work. The compiler is registered on vellum’s
  `as_vellum_scene()` seam.

### Under the hood

- **Compiled-scene provenance** (foundation for interactivity /
  accessibility / linked views): every emitted mark grob now carries a
  globally-unique, stable `id` (surfacing as `data-vellum-id` in SVG),
  and the compiler builds a serializable row-key / scale-ref table — one
  record per grob tying it back to the data rows and trained scales that
  produced it — carried on the compiled scene as
  `attr(scene, "vellumplot_provenance")`. Populated on every compile;
  additive metadata only (raster/PDF output is byte-for-byte unchanged).
  See `_docs/DESIGN.md` §4.
- **Continuous integration**: GitHub Actions run `R CMD check` (R
  release/devel/oldrel on Linux + macOS, with the Rust toolchain the
  `vellum` backend needs), including a nightly run against `vellum`’s
  `main` to catch cross-layer breaks early, plus a pkgdown build.

### Not yet implemented (planned)

Reactivity, 2-D contour stats, and the algebraic `*` / `+` layer
combinators. Independent *non-position* (colour/size) scales across
facets are not yet supported (those legends stay shared). On the network
side, community hulls and alternative idioms (arc/matrix/hive) are
deferred (see `_docs/DESIGN-igraph.md`).
