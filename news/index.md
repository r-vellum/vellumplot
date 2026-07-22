# Changelog

## vellumplot (development version)

- **`mark_point(auto = TRUE)` no longer datashades under a warped
  coordinate system.** Under
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  /
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)
  the \>50000-row datashade fallback — which bins in linear data space —
  would rasterise into the wrong place; it is now skipped and the vector
  path draws, matching the documented behaviour and the other `auto`
  marks (line/step/segment/edges).

- **A sankey column with too many nodes errors clearly.** When a column
  has more nodes than its inter-node gaps can fit, the node/ribbon
  heights previously went negative and spilled outside the panel;
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  /
  [`mark_sankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  now abort with a message pointing at `node_gap`.

- **Aggregating stats drop missing summaries consistently, with a
  warning.** For a value-summarising bar (`mark_bar(y = , fun = )` and
  friends), a category whose summary is `NA` (an empty group, or a
  summary of data containing `NA` when the function does not remove it)
  is now dropped with a warning on **both** the grouped and ungrouped
  paths, which previously treated `NA` differently. A layer mapping both
  `color` and `fill` also keeps both aligned after aggregation (`fill`
  could previously be left at the wrong length).

- **Clearer errors for malformed scale bounds.**
  [`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_y_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_size()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  [`scale_size_area()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  [`scale_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md),
  [`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md),
  and
  [`scale_edge_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md)
  now reject a `limits` (or output `range`) that is not a length-2
  vector — and, for the size/alpha/edge scales, a non-numeric one — with
  a clear message, instead of failing later with a cryptic low-level
  error (e.g. `'length = 2' in coercion to 'logical(1)'`).
  [`scale_size_area()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  also validates `max_size`.

- **[`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)
  rejects a non-numeric mapping.** A factor `x`/`y` previously slipped
  through and the marginal was computed over the integer level codes
  rather than the data; it now errors clearly, as a character column
  already did.

- **Stricter validation of interactivity and layout arguments.**
  [`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)/[`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
  reject a non-logical `toggle`/`empty` (it was silently coerced to
  `FALSE`);
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  validates `width`/`height` like
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)/[`vhierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md);
  and
  [`area()`](https://r-vellum.github.io/vellumplot/reference/area.md)
  rejects a non-numeric or non-integer cell index instead of aborting
  with an opaque “missing value where TRUE/FALSE needed”.

- **[`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  honours `hjust`/`vjust` passed as a variable.** They were only read as
  constant params, so a value routed through a variable (as
  [`vdendrogram()`](https://r-vellum.github.io/vellumplot/reference/vdendrogram.md)
  does per direction) silently fell back to centred, letting leaf labels
  run over the edges. `.emit_text()` now reads them value-first, like
  `angle`.
  [`vdendrogram()`](https://r-vellum.github.io/vellumplot/reference/vdendrogram.md)
  leaf labels also get a larger default gap.

- **Dendrograms and unrooted trees.**
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  now accepts a base `hclust`/`dendrogram` and gains a height-aware
  `"dendrogram"` layout plus an `"unrooted"` layout (via
  [`graphlayouts::layout_as_tree_unrooted()`](https://schochastics.github.io/graphlayouts/reference/layout_tree_unrooted.html),
  needs graphlayouts \>= 1.2.5). `mark_edges(routing = "elbow")` gained
  `elbow_at` / `elbow_axis` so the elbow can be the dendrogram *bracket*
  (corner at the parent’s level) rather than only the midpoint S-bend.
  [`vdendrogram()`](https://r-vellum.github.io/vellumplot/reference/vdendrogram.md)
  is a one-line preset: bracket edges, leaf labels, `direction`, and `k`
  to cut the tree and colour clusters. Also fixes
  `mark_node_text(label = )` so an explicit `label` mapping overrides
  the default vertex name (it was previously ignored).

- **[`vhierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md)
  respects `scale_fill_*()`.** By default nodes are still coloured by
  their depth-1 branch and lightened with depth, but the branch is now
  an ordinary discrete fill scale, so
  [`scale_fill_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  /
  [`scale_fill_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  recolour the branches and a `lighten` argument controls the depth fade
  (`0` = flat colour per branch). Mapping `fill` to a node column
  instead colours every node by that variable (discrete or continuous,
  with the matching `scale_fill_*()`), with no depth fade.

## vellumplot 0.7.0

- **New
  [`vhierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md)
  for space-filling hierarchies — breaking.**
  `vhierarchy(id, parent, value, type = )` draws a tree four ways from
  one parent list, switching only `type`: `"sunburst"` (default),
  `"icicle"`, `"treemap"`, or `"circlepack"`. This **replaces
  `vsunburst()`** — `vhierarchy(..., type = "sunburst")` reproduces the
  old radial sunburst, and `vsunburst()` / `mark_sunburst()` are
  removed. Treemaps use a squarified layout; circle-pack is a faithful
  port of d3’s front-chain packing.
  [`mark_hierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md)
  is the exported layer.

- **Sunbursts, polar pies, and self-loops render un-mirrored.** Requires
  vellum (\>= 0.5.1), which fixes `sector_grob()`/`loop_grob()` to draw
  in the same y-up frame as every other primitive. Sunburst segment
  labels now sit on their own wedges (they previously landed on the
  vertical mirror on unbalanced trees), sunbursts wind clockwise from
  twelve o’clock as documented, and graph self-loops point into the
  empty gap between a vertex’s incident edges rather than its mirror
  image.

- **`select_point(group_by=)` now links whole groups.** A point
  selection with `group_by` / `fields` groups elements sharing those
  column values, so hovering or clicking one mark highlights (and
  [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  spotlights) its whole group — the “hover a series, light up the
  series” behaviour. Implemented by emitting the group as the element
  `hover_group`, so it needs no widget change; a user-declared
  `hover_group` still wins. Inert on a static render.

- **Declarative interactivity (new).** Interaction is now part of the
  plot spec rather than something a host is configured to do. Declare a
  named selection with
  [`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
  (click/hover) or
  [`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
  (brush/lasso), then refer to it from
  [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  (style an aesthetic by selection membership),
  [`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
  (show only members — point a second view at the same selection for
  cross-filtering), or
  [`bind_scale()`](https://r-vellum.github.io/vellumplot/reference/bind_scale.md)
  (drive another panel’s view, for overview + detail).
  [`add_selection()`](https://r-vellum.github.io/vellumplot/reference/add_selection.md)
  shares a free-standing selection across views;
  [`interaction_model()`](https://r-vellum.github.io/vellumplot/reference/interaction_model.md)
  returns the compiled declaration block a host reads. Every node is
  inert on a static render (a plot with interactions renders
  byte-for-byte identically to one without), and
  `condition("s", g, "grey80")` trains scales and draws exactly like
  `color = g`. Enacted by `vellumwidget`.

## vellumplot 0.6.0

- **Sankey styling options.**
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  /
  [`mark_sankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  gain `show_values` (append each node’s value to its label),
  `flow_color` (`"source"`, `"target"`, or `"gradient"` — a
  source-to-target colour fade per ribbon), and `node_width` /
  `node_gap` to tune the node rectangles.

- **Sankey crossing minimisation.**
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  now orders the nodes within each column with the Sugiyama barycenter
  heuristic to minimise ribbon crossings (previously first-appearance
  order, which left many avoidable crossings). The reordering is
  deterministic and pure R. A related fix stacks each node’s ribbon
  slices to meet the node in the same vertical order as the nodes they
  connect to, removing the remaining crossings within a fan of ribbons.

- **Sunburst rendering fixes.** `vsunburst()` now colours wedges by
  their top-level branch (each branch a distinct hue, lightened with
  depth) instead of a single colour per ring — sibling branches were
  previously indistinguishable. It also starts the first wedge at twelve
  o’clock and winds clockwise, matching the package’s pies/roses
  (`coord_polar`) rather than starting at three o’clock
  counter-clockwise.

- **Sunburst / radial hierarchies: `vsunburst()`.** A new plot type for
  part-of-whole hierarchies, from a *parent list* — `id`, `parent` (`NA`
  at the root), and `value` (leaf values; internal nodes sum their
  children). Depth maps to a ring and each node’s angular span is its
  share of its parent’s, drawn as one batched `sector_grob` in an
  aspect-locked, axis-free square panel (mirroring
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)/[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)).
  `inner_radius` opens a central hole (a donut/ring sunburst); nodes are
  coloured by depth. `mark_sunburst()` is the exported layer. See
  `vignette("flows-and-hierarchies")`.

- **Sankey / flow diagrams:
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md).**
  A new plot type for layered flows, built from a *flow list* — one row
  per flow with `from`, `to`, and `value` (the ribbon width). Nodes are
  the union of `from`/`to`; a node that is both a source and a target
  makes the diagram multi-stage. `vsankey(data, from, to, value)`
  returns a ready, axis-free plot (mirroring
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md));
  [`mark_sankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  is the exported layer it adds. The layout is computed R-side
  (longest-path layering, value-proportional node heights and ribbon
  widths, filled Bézier ribbons) and is deterministic. Flows must form a
  DAG; nodes are coloured from the qualitative palette. See
  `vignette("flows-and-hierarchies")`.

- **Uncertainty marks:
  [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  and
  [`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md).**
  ggdist-style slab + interval marks for sample/posterior input (many
  `y` rows per categorical `x`).
  [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  draws a one-sided density slab with a point-interval at its base — the
  median (or `point = "mean"`), a thick inner and thin outer
  equal-tailed quantile interval at the `.width` probabilities (default
  `c(0.66, 0.95)`);
  [`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  is the point-interval alone. A natural fit for visualising posterior
  draws (e.g. from ).

- **[`coord_radial()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  and
  [`scale_size_area()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md).**
  [`coord_radial()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  is a fuller polar system (ggplot2 3.5’s name): besides
  `theta`/`start`/`direction` it takes `end` to sweep only a **partial
  arc** (e.g. `start = -pi/2, end = pi/2` for a semicircular gauge) and
  `inner_radius` for a **donut hole**; with the defaults it matches
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md).
  [`scale_size_area()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  maps a value to the marker’s **area** (value `0` → size `0`), the
  perceptually honest default for bubble charts, with `max_size` the
  size of the largest value.

- **Sankey labels stay on-panel.**
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  now reserves horizontal margin for node labels, so the source column’s
  labels (drawn to their left) and the terminal column’s (drawn to their
  right) no longer clip at the panel edge.

- **Robustness of the new marks.** `vsunburst()`/`mark_sunburst()` now
  reject a missing or negative leaf `value` with a clear message
  (instead of a cryptic downstream error), and `mark_sunburst()`
  validates `inner_radius` like `vsunburst()` and
  [`coord_radial()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  do.
  [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)/[`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  skip a category with fewer than two finite observations (with a
  warning) rather than drawing an empty interval, and reject a `width =`
  argument (a likely typo for `.width`) that was previously ignored
  silently.

- **Consistent argument names for the hole radius / ridge height.** The
  central-hole fraction is now spelled `inner_radius` everywhere it
  appears:
  [`coord_radial()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md),
  `vsunburst()`/`mark_sunburst()`, and
  [`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
  (was `hole`).
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)’s
  overlap control is now `height` (was `scale`, which collided with
  `mark_halfeye(scale=)`, a different quantity).

- **Consistent `sketch` / `blend` passthrough.**
  [`mark_ecdf()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md),
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md),
  [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md),
  [`mark_dotplot()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md),
  [`mark_qq()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md),
  and
  [`mark_qq_line()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  now accept a per-layer `sketch =` (their emitters already honoured
  it), and
  [`mark_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
  /
  [`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
  accept `blend =`, matching the rest of the mark surface.

- **Parameterised position adjustments.** New
  [`position_nudge()`](https://r-vellum.github.io/vellumplot/reference/position.md),
  [`position_jitter()`](https://r-vellum.github.io/vellumplot/reference/position.md),
  [`position_dodge()`](https://r-vellum.github.io/vellumplot/reference/position.md),
  [`position_dodge2()`](https://r-vellum.github.io/vellumplot/reference/position.md),
  and
  [`position_jitterdodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  give a mark’s `position` tunable parameters (a bare string like
  `"dodge"` still works with the defaults). Adds three adjustments:
  `nudge` (shift every element by a constant in data units), `dodge2`
  (dodge by the groups actually present at each x, filling the band with
  a `padding` gap — so ragged groupings stay centred), and `jitterdodge`
  (jitter points within their dodged slot).
  `position_jitter(width=, height=, seed=)` and `position_dodge(width=)`
  expose the previously-fixed jitter/dodge extents. A plot using the old
  string positions is unchanged.

- **Label repulsion: `mark_text(repel = TRUE)` /
  `mark_label(repel = TRUE)`.** Overlapping text labels are moved apart
  with a force-directed layout (ggrepel-style), each keeping a thin
  leader line back to its point. Because the plot size is fixed on the
  spec, the repulsion is resolved *exactly* against the true rendered
  panel — a two-pass compile that reads the panel’s device geometry from
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html),
  relaxes the label boxes in pixel space, and maps the result back — so
  it needs no approximation and no `vellum` change, and is deterministic
  under `seed`. Tunable via `box_padding`, `point_padding`,
  `min_segment_length`, and `seed`. Limited to a single cartesian panel
  for now (facets / composition / polar error clearly).

- **`mark_line(window = )`: rolling / cumulative / offset transforms.**
  A line can now transform its `y` per group (over rows ordered by `x`)
  before drawing — moving `mean`/`sum`/`median`/`min`/`max` over a
  window of `k`, running `cumsum`/`cummean`/`cummax`/`cummin`,
  `lag`/`lead` shifts, or `rank`. Pass an op name (`window = "mean"`) or
  a list
  (`window = list(op = "mean", k = 7, align = "right", partial = TRUE)`);
  `align` is trailing/leading/centred and `partial` fills the edges from
  the shorter window so the line stays continuous. A plot without
  `window` is unchanged.

- **Diverging colour scales:
  [`scale_color_gradient2()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  /
  [`scale_fill_gradient2()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md).**
  A three-point ramp (`low`–`mid`–`high`) centred on `midpoint` (default
  `0`), rescaled *about the midpoint*
  ([`scales::rescale_mid`](https://scales.r-lib.org/reference/rescale_mid.html))
  so the neutral colour sits on the chosen value and each side spans as
  far as the data reaches — the correct scale for signed / anomaly data.
  A diverging continuous colorbar also reports `midpoint` + `diverging`
  in its `colorbar` descriptor, so an interactive host can centre a
  value-range filter on the neutral value.

- **`symlog` position transform.**
  `scale_x_continuous(trans = "symlog")` (and `y`) adds a symmetric-log
  axis: linear through zero, logarithmic in the tails
  (`sign(x) · log10(1 + |x|)`), so signed data spanning several orders
  of magnitude — including zero and negatives, which `log10` cannot show
  — reads on one axis. Breaks sit at zero and signed powers of ten. The
  transform name flows into the panel `scales` descriptor like the other
  transforms.

- **New group-region marks:
  [`mark_ellipse()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
  and
  [`mark_hull()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md).**
  Both enclose a set of `(x, y)` points in a single region drawn over a
  scatter — one region per group when a `color`/`fill` is mapped.
  [`mark_ellipse()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
  draws a covariance ellipse (`type = "t"` robust default via , or
  `"norm"`/`"euclid"`), following ggplot2’s `stat_ellipse()`;
  [`mark_hull()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
  draws the convex hull. Both are unfilled boundaries by default
  (map/set a `fill` to shade them) and need at least 3 points per group.
  The region’s boundary trains the position scales, so an ellipse that
  bulges past the data is not clipped.

- **[`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  gains real smoothing methods.** Beyond `"lm"`, the smooth mark now
  fits `"loess"` (local regression, `span =`), `"glm"` (with a `family`
  via `method.args`, e.g. logistic — the fit and its ribbon are
  back-transformed from the link scale), `"gam"` (a penalised smooth,
  default `y ~ s(x)`; needs ), and `"rq"` (quantile regression at a
  single `method.args$tau`; needs , line only, no ribbon). The default
  `method = "auto"` picks `loess` for small groups (\< 1000 points) and
  `gam` for large ones, as in ggplot2. New `formula`, `span`, and
  `method.args` arguments; `glm`/`gam` bands use a normal interval,
  `lm`/`loess` a t-interval. Previously
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  errored on any method other than `"lm"`. and are Suggests — a gated
  method errors clearly if its package is absent.

- **Data panels are emitted as pannable, gridlines tagged.** Cartesian
  data panels (including `coord_flip` and a linear `coord_trans`) now
  push a `pannable` vellum viewport, and gridlines carry
  `role = "grid"`. This is inert for static rendering but lets an
  interactive host (`vellumwidget`) pan/zoom a panel’s marks while its
  clip + axes stay fixed and hide/redraw gridlines — the groundwork for
  axis-aware zoom. Polar / nonlinear-`coord_trans` panels stay
  non-pannable. Requires the current development `vellum`.

- **Continuous colorbar filter metadata.** A continuous `color` scale
  now attaches each mark’s colour value as `filter_value` in its element
  `meta`, and a `colorbar` descriptor (value domain + orientation) to
  the gradient-bar grob. Together (via
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html))
  these let a host such as `vellumwidget` overlay an interactive
  value-range filter on the colorbar. Discrete/binned colour scales are
  unaffected. No change to rendered output.

- **Panels now carry a `scales` descriptor for interactive hosts.** Each
  cartesian data panel’s viewport gains a `meta$scales` record — per
  axis: `type` (continuous / log10 / discrete / binned / date /
  datetime), `transform`, the data and native domains, tick breaks +
  labels, and `time_unit` for date/datetime axes. It surfaces via
  `vellum::scene_model()$panels$meta` and lets a host (e.g.
  `vellumwidget`) map device pixels back to data values — so a brush or
  a reported view can be expressed in data coordinates, not just pixels.
  Requires `vellum` (\>= 0.4.0.9000). Internal: trained position scales
  now also record their `transform` name and, for date/time axes, a
  `time_unit` (previously the date/datetime nature was lost after
  training). No change to rendered output.

## vellumplot 0.5.0

- **Rich [`md()`](https://r-vellum.github.io/vellumplot/reference/md.md)
  legend titles no longer clip.** A legend built from a rich title
  (`scale_color_continuous(name = md("Power (hp m^2^)"))`,
  `labs(color = md(...))`) measured the title as zero width, so the
  legend reserved no room for it and the drawn title spilled off the
  page. Titles are now measured through vellum’s rich text path
  (`vl_strwidth()`), reserving the space they actually occupy.

- **Secondary axes.** Continuous position scales gain a `sec.axis`
  argument fed by the new
  [`sec_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md)
  /
  [`dup_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md):
  a second set of ticks and labels on the opposite edge (top for `x`,
  right for `y`), computed as a 1:1 monotonic transform of the primary
  axis — e.g.
  `scale_x_continuous(sec.axis = sec_axis(~ . * 1.8 + 32, name = "°F"))`
  for a unit conversion, or
  [`dup_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md)
  to duplicate an axis on a wide plot. The transform is a formula,
  function, or `scales::transform_*()` object. Scoped to the default
  Cartesian system with shared facet scales; combining it with
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  /
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  /
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md),
  free facet scales, or
  [`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)
  raises a clear error, and it is not drawn inside a plot composition. A
  plot without a `sec.axis` is byte-for-byte unchanged.

- **Datashading now covers dense lines and large-graph edges.**
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md),
  and
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  gain an `auto = TRUE` switch (parallel to `mark_point(auto = TRUE)`):
  past a row threshold the layer rasterises into a line- /
  segment-density field via
  [`vellum::datashade_lines()`](https://r-vellum.github.io/vellum/reference/datashade_lines.html)
  /
  [`vellum::datashade_segments()`](https://r-vellum.github.io/vellum/reference/datashade_lines.html)
  instead of emitting one vector per element, so overplotted timeseries
  stacks and graph “hairballs” render fast and honestly. The fallback is
  skipped under a warped coordinate system
  ([`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  /
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)),
  which keeps the vector path. (Area / ribbon datashading is not yet
  available, pending area-fill support in vellum.)

- **`mark_datashade(spread = )`** exposes vellum’s post-aggregation
  spreading: `NULL` (default, raw output), a positive integer for a
  fixed pixel radius
  ([`vellum::spread()`](https://r-vellum.github.io/vellum/reference/spread.html)),
  or `"auto"` for density-adaptive dilation
  ([`vellum::dynspread()`](https://r-vellum.github.io/vellum/reference/dynspread.html)).
  Datashaded lines and segments default to `"auto"` so single-pixel
  marks stay visible.

- **Continuous and binned colour scales now interpolate perceptually
  (Oklab) by default.** A colour ramp built from a plain colour vector
  (e.g. `scale_color_gradient(low, high)` or
  `scale_color_continuous(palette = c(...))`) is blended in the
  perceptually-uniform Oklab space instead of sRGB, so it no longer
  passes through muddy, over-dark midtones or drifts in hue — the ramp
  and its legend colourbar read evenly. Designed perceptual palettes
  (the `batlow` default,
  [`hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html) names) are
  already uniform and are unchanged. Set
  `options(vellumplot.color.interpolation = "srgb")` (or `"lab"`) to
  restore the old behaviour. Gradient *fills* opt in per gradient with
  `linear_gradient(..., interpolation = "oklab")` (passed through to
  vellum).

- **Error bars and line ranges are now interactive.**
  [`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  and
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  thread a declared `data_id`/`tooltip`/`hover_group` through to their
  drawn segments, so each bar is keyed to its datum — it appears in
  `scene_model()` and
  [`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md)
  and carries a `data-key` in the SVG, ready for hover/click/select in a
  widget (a bar’s cap segments share the bar’s key).
  [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  already keyed each box by its category; this completes the statistical
  marks. A mark with no interactivity declared is unchanged.

- **Two more shapes in
  [`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md).**
  The `shape` aesthetic’s default palette now extends to
  `"triangle_down"` (a downward triangle) and `"star"` (a five-pointed
  star) after the original six, so a mapped `shape` covers up to eight
  levels without an explicit scale, and both are accepted as
  `scale_shape(values=)`. Requires the accompanying `vellum` dev
  version. (A constant `shape = "star"` on a mark already worked once
  vellum gained the shape.)

## vellumplot 0.4.0

### New features

- **Categorical datashading in one call.**
  [`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md)
  now accepts a mapped discrete `color`/`fill` aesthetic and shades
  **categorically** (datashader’s `count_cat`): each category is
  aggregated separately and every cell is coloured by the count-weighted
  average of the category hues it holds, opacity by density — so you see
  which category dominates where, and where they mix, **with a colour
  legend**. Category hues come from the usual discrete colour scale
  (`scale_color_*()` apply). This replaces the old idiom of stacking one
  datashade layer per category with `blend = "screen"`. New
  `span`/`clip` arguments clamp the density range (absolute limits or
  percentiles) so a few extreme cells don’t flatten the rest. Requires
  the accompanying `vellum` dev version.

- **[`mark_image()`](https://r-vellum.github.io/vellumplot/reference/mark_image.md)
  draws images at data points.** A new mark places a bitmap image
  (e.g. a country flag or company logo) at each `(x, y)`, replacing the
  usual point marker. `src` is a column of local file paths or one
  constant path, and `size` sets the height in millimetres (the width
  follows each image’s native aspect ratio). Requires the suggested
  `magick` package
  ([\#33](https://github.com/r-vellum/vellumplot/issues/33)).

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

### Bug fixes

- **`linetype` now works on stroked marks.**
  [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md),
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_rug()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md),
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
  and
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  (and `annotate("segment", ...)`) previously ignored `linetype` and
  always drew solid lines; they now honour it, consistent with
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  ([\#30](https://github.com/r-vellum/vellumplot/issues/30)).

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

2-D density / contour (`mark_contour`/`stat_density_2d`);
position-binned scales (`scale_x_binned`);
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
  `graphlayouts` are optional (`Suggests`).

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
- **Continuous integration**: GitHub Actions run `R CMD check` (R
  release/devel/oldrel on Linux + macOS, with the Rust toolchain the
  `vellum` backend needs), including a nightly run against `vellum`’s
  `main` to catch cross-layer breaks early, plus a pkgdown build.

### Not yet implemented (planned)

Reactivity, 2-D contour stats, and the algebraic `*` / `+` layer
combinators. Independent *non-position* (colour/size) scales across
facets are not yet supported (those legends stay shared). On the network
side, community hulls and alternative idioms (arc/matrix/hive) are
deferred.
