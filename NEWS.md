# vellumplot 0.9.0.9000 (development version)

## Bug fixes


* **An empty facet cell no longer crashes a mark with an `after_stat()` channel.**
  A `facet_grid()` layout with an unpopulated cell (or any empty stat input) drawing
  an aggregating mark (`mark_bin2d()`, `mark_count()`, `mark_hex()`, `mark_contour()`,
  a `mark_histogram(y = after_stat(density))`, ...) aborted the whole render with
  `object 'count' not found`; the empty layer now renders as a blank panel.
* **`mcp_serve()` now handles more than one request per process.** The stdin
  connection was left unopened, so each `readLines()` reopened it and the loop
  saw end-of-file after the first line — a real MCP client died right after
  `initialize`. The connection is now opened once and held across the loop.
* **`from_spec()` / `vplot_from_spec()` now honour supplied `data` for a spec
  with no data block**, instead of dropping it and failing later in compile with
  an opaque "argument must be coercible to non-negative integer". A spec that
  maps fields but has (and is given) no data now errors clearly, naming the
  missing references.
* **`transition_states()` filters a layer's own `data =` per keyframe.** A layer
  with its own data (e.g. a per-state annotation) was not subset to the current
  state, so every keyframe drew all states' rows at once. A layer whose data
  lacks the state column still persists across all frames.
* **`labs(x = NULL)` now blanks the axis title** (ggplot2 parity), distinct from
  omitting the argument. Previously only `labs(x = "")` blanked it.

## New features

* `guide_legend(nested = TRUE)` draws a **size** legend as a proportional-symbol
  ("bubble") legend: concentric, bottom-aligned circles with a leader from each
  to its label, instead of stacked rows. Best with a wide size range
  (`scale_size(range = c(2, 12)) |> ... guides(size = guide_legend(nested = TRUE))`).
* `guide_coloursteps()` (US alias `guide_colorsteps()`) draws a **binned** colour
  scale as a segmented colour bar — the bin colours in order, labelled at the
  numeric boundaries — instead of the default discrete swatches. Takes the same
  `barwidth` / `barheight` / `ticks` / `label.position` tunables as
  `guide_colourbar()`: `scale_color_binned() |> ... guides(color = guide_coloursteps())`.
* `guide_colourbar()` (and the US alias `guide_colorbar()`) make the continuous
  colour bar tunable: `barwidth` / `barheight` (bar size in mm), `ticks` /
  `ticks.colour` (the break ticks), and `label.position` (`"right"` / `"left"`
  labels on a vertical bar), on top of the `title` / `reverse` that
  `guide_legend()` already offered for a colour bar. For example
  `guides(color = guide_colourbar(barheight = 60, ticks = FALSE))`.
* `mark_sf_label()` labels each `sf` feature at its **interior point**
  (`sf::st_point_on_surface()`, always inside the polygon), reprojected through
  the same `coord_sf()` CRS as `mark_sf()` so the labels land on the geometry.
  Repels crowded labels apart. Layer it over a `mark_sf()` choropleth to name the
  regions: `vplot(nc) |> mark_sf(fill = AREA) |> mark_sf_label(label = NAME)`.
* `guide_legend()` gained an **`override.aes`** argument (ggplot2 parity): a named
  list of aesthetics forced on the legend **keys** only, independent of the
  plotted data — e.g. `guides(color = guide_legend(override.aes = list(size = 5,
  alpha = 1)))` draws large, opaque keys over a scatter of tiny translucent
  points. Recognised: `size`, `alpha`, `colour`/`color`, `fill`, `shape`,
  `linewidth`. The key cell grows to hold an overridden size.
* `scale_x_continuous()` / `scale_y_continuous()` gained an **`expand`** argument
  controlling axis padding: `NULL` (default) keeps the usual 5% margin, and a
  numeric `c(mult, add)` sets a proportion of the data range plus a constant in
  data units — e.g. `expand = c(0, 0)` clamps the axis exactly to the data.
* `theme_sketch()` now **sketch-ifies the plot's current theme** instead of
  resetting to grey, so a self-contained chart that is deliberately axis-free
  (`vwaffle()` / `vvenn()` / `vsankey()` / …) is no longer given back a grid and
  axes when hand-drawn. An ordinary axis chart is unchanged.

* **Tagged-PDF alt text no longer leaks internal identifiers.** A tagged PDF used
  to make every mark its own `Figure` whose `Alt` was the mark's provenance id
  (`layer-1-line-g1`) — or, for a `mark_text(repel = TRUE)` label, its internal
  `vl_place` handle (`repel:panel-1-1:2:0`) — so a screen reader announced those
  strings, one per mark. Marks are now tagged as **artifacts** (`role =
  "presentation"`), leaving a single `Figure` that carries the `plot_alt()`
  description. Provenance (`data-vellum-id`) and interactivity (`data-key`) are
  unchanged. Needs vellum ≥ 0.6.8 (which keeps the figure for an all-artifact
  described scene). Reported in #145.

## New features

* `mark_outlier_label()` — label only the **outliers**, not every point. Keeps the
  rows whose `y` is an outlier (Tukey's IQR rule by default, or `method = "sd"`,
  detected per colour/fill group) and labels just those, repelled apart. Map a
  `label` to name each; with none mapped, the outlier's `y` value is the label.

* `mark_series_label()` — **direct labels**: name each colour series at its line
  end instead of in a legend (the direct-labels idiom). Maps the same
  `x`/`y`/`color` as the lines; the label text and colour come from the series,
  and the labels are pushed apart with the same repel solver as `mark_text()`.
  Give the panel a little x-room (`xlim()`) and drop the legend
  (`guides(color = "none")`).

* `mark_signif()` — significance brackets (the ggsignif / ggpubr idiom): run a
  pairwise test (`wilcox.test` or `t.test`) between `x` groups and draw a bracket
  with its p-value (or stars) over each comparison. Brackets stack above the data
  and the y-axis expands to fit them. Add on top of a `mark_boxplot()` /
  `mark_violin()`.

* `scale_color_steps()` / `scale_fill_steps()` — ggplot2-parity aliases of the
  binned colour scales (a continuous variable cut into steps).
* **Adaptive knit output.** When a plot is knitted (Quarto / R Markdown) to an
  HTML target it is now emitted as a crisp, selectable, resolution-independent
  inline **SVG**; other targets (LaTeX / Word) keep the default device render.
  One plot object, the best format per output — no conditional code.
* `plot_data_uri()` encodes a plot as a self-contained `data:` URI (vector SVG by
  default, or `format = "png"`) — ready for an HTML `<img src>`, a Markdown image,
  an email, or a `gt` table's non-HTML backends.
* Three more complete themes for ggplot2 parity: `theme_light()`, `theme_dark()`,
  and `theme_linedraw()`.
* Named palette scale constructors (thin wrappers over `palette =`, easing
  migration): `scale_*_viridis_c()`/`_d()` (the viridis maps —
  viridis/plasma/inferno/cividis/rocket/mako), `scale_*_brewer()` (discrete) and
  `scale_*_distiller()` (continuous) ColorBrewer-style palettes, and
  `scale_*_gradientn()` for an arbitrary n-stop colour ramp.
* `scale_color_continuous()` / `scale_fill_continuous()` gained a `limits =`
  argument (the direct form of `lims(color = ...)`).
* The named graph-layout registry gained `gem`, `graphopt`, `dh`, and `lgl`
  (igraph force-directed layouts, now reachable by name).
* `vwaffle()` — a self-contained **waffle chart** (square pie): a grid of cells
  coloured by category, each category taking a share of the cells proportional
  to its count (largest-remainder allocation, so the cells sum exactly). Works
  from a `value` column or raw row counts.
* `position_sina()` (and `position = "sina"`) spreads a category's points along
  x by a quasirandom offset scaled to the local y-density, so the cloud traces
  the distribution's shape (ggforce's sina).
* `mark_raincloud()` composes a **raincloud** — a one-sided density cloud
  (`mark_halfeye()`) with the raw observations as sina "rain" below it — in one
  call, forwarding any `color`/`fill` mapping to both layers.
* New reference and summary marks (ggplot2 parity):
  - `mark_abline()` — a sloped reference line `y = slope * x + intercept` across
    the panel (`slope`/`intercept` may be vectors for several lines).
  - `mark_function()` — a curve `y = fun(x)` sampled over the panel's x range,
    e.g. `mark_function(fun = dnorm)` over a density histogram.
  - `mark_pointrange()` / `mark_crossbar()` — identity summary marks (a point or
    a box with `ymin`/`ymax`, the non-aggregating counterparts of
    `mark_interval()`).
  - `mark_count()` — collapse coincident `(x, y)` points to one bubble sized by
    overlap count (`geom_count()` / `stat_sum()`); `size` defaults to
    `after_stat(n)`.

## Internal

* Performance and cleanup with no change to rendered output: the boxplot /
  violin / half-eye emitters group rows by category once instead of rescanning
  per level; the `sf` point path and the circle-pack / sunburst hierarchy layouts
  drop quadratic accumulation and repeated ancestor walks; the horizontal
  colour-bar computes each break position once.

## Bug fixes

* Interop round-trip fidelity: an integer stat parameter (e.g.
  `mark_histogram(bins = 20L)`) now survives `as_spec()`/`from_spec()` and JSON
  as an integer instead of silently widening to a double (which changed the spec
  hash). And a Vega-Lite export where two aesthetics map onto one channel (e.g.
  `color` and `fill`) now reports the dropped encoding rather than silently
  overwriting it.
* Clearer, earlier input validation across several surfaces: `scale_*_binned()`
  rejects a non-positive or non-scalar `n` and a non-scalar `style`; a numeric
  scale `range` rejects a non-finite bound; `coord_fixed(ratio=)`,
  `coord_polar(start=)`, and `coord_radial(start=/end=)` reject non-scalar or
  non-finite values; the pattern constructors reject a non-positive
  `spacing`/`size`/`linewidth`; `mark_rug(sides=)` and a rolling `window`'s `k`
  are validated; and an MCP `tools/call` with no tool name returns a clean error.
  Each previously failed late with an opaque message (or, for `set_mask(region=)`,
  silently ignored the argument — now rejected).
* A discrete position axis no longer draws spurious minor gridlines between and
  outside its categories.
* Legends with mixed titled and untitled guides no longer misalign: a guide with
  no title reserves no title band.
* An `NA` in a facet variable now consistently drops the row (matching ggplot2)
  rather than creating a spurious `"NA"` panel for character/multi-variable facets.
* Stacked bars/areas tolerate an `NA` height (dropped from the stack) instead of
  poisoning the whole group's cumulative total.
* A non-finite scale break no longer anchors a stray gridline, tick, or axis label.
* Serializing a preset theme carrying an `element_*()` override now warns that the
  customisation is dropped (previously suppressed whenever a preset was set).
* `clip_to()` rejects a region vertex whose `group` is `NA` (it was silently
  dropped, deforming the clip).
* Several marks no longer silently ignore aesthetics they document: `alpha` now
  applies to `mark_boxplot()`, `mark_errorbar()`, `mark_linerange()`,
  `mark_rule()`, `mark_halfeye()`/`mark_interval()`, and the `mark_smooth()`
  band; `mark_smooth()` honours `linewidth` for its fitted line; and the text
  marks (`mark_text()`, `mark_label()`, `mark_text_path()`) honour a mapped
  `size` (trained through a size scale, as points already were).
* `facet_grid()` no longer aborts when a row/column combination has no data: a
  structurally-empty panel (e.g. a histogram cell with no observations) now
  renders blank instead of killing the whole plot.
* One-sided continuous limits work as in ggplot2: `ylim(0, NA)` /
  `scale_*_continuous(limits = c(NA, hi))` now pin the supplied end and train the
  open end from the data, rather than silently collapsing the axis.
* Numeric variables drawn on a discrete axis (e.g. `mark_bar(x = a_number)`)
  order their categories numerically (`1, 2, 10`) instead of lexicographically
  (`1, 10, 2`).
* Overlaying two layers on one discrete axis, one mapping a `factor` and one a
  `character`, no longer drops or mislabels categories.
* A pie / donut (`mark_pie()`, `mark_donut()`) with a constant or short `x`
  no longer errors.
* `mark_edge_bundle()` and `mark_flow_map()` now accept their documented
  `effects =` argument (glow / shadow / outline) instead of rejecting every
  effect.
* `layout = "dendrogram"` reports a clear "needs a connected tree" error on a
  disconnected graph instead of an opaque internal failure.
* `vtable()` renders a blank cell for a too-short or empty sparkline series
  instead of aborting the whole table.
* Non-looping `transition_reveal()` / `transition_time()` animations now reach
  their final keyframe instead of stopping roughly one frame short.
* Faceting on a column whose name is not a syntactic R name (e.g.
  `"Miles per Gallon"`, common after a Vega-Lite import) round-trips through
  `as_spec()` / `from_spec()` instead of crashing.

* **One variable colouring both nodes and edges now draws a single legend.**
  When a node/text colour and an edge colour resolve to the same scale — same
  title, levels, and palette, as in `vdendrogram(k =)` — the redundant edge
  guide folds into the node colour guide instead of drawing a second, identical
  legend beside it. Genuinely different node and edge colour scales still keep
  their own guides (#94).

* **Requires vellum >= 0.6.8**, and `plot_lint()` gained everything the linter
  gained there: 13 new rules, two of which vellumplot could not have had on its
  own — colours a colour-blind reader cannot tell apart, and characters no font on
  the running machine can draw. Every `vellum::vl_lint()` argument now comes
  through — `exclude` to accept a finding you have already judged, `cvd` to pick
  which deficiency to check, `rules` to run one rule alone, and `min_text_pt` for
  a print-resolution legibility floor.

* **Breaking, minor: `plot_lint()`'s thresholds must be named.** `...` now sits
  after `x`, so the optional arguments follow it — `plot_lint(p, 5)` used to mean
  `min_text_px = 5` and now fails, because the `5` reaches the engine as a rule
  name. Write `plot_lint(p, min_text_px = 5)`. This is the usual convention for
  arguments after `...`, and it is what makes `cvd =`/`exclude =` reach the engine.

* **Findings carry the box they refer to**, so `vellum::vl_lint_overlay()` can
  draw the report onto the plot rather than describing it, and
  `vellum::vl_lint_assert()` can fail a test suite on it. `plot_lint()` therefore
  returns four more columns (`x0`, `y0`, `x1`, `y1`); a grammar finding is about a
  scale rather than a node, so its box is `NA`.

* **The grammar rules are registered in the engine's rule registry** instead of
  being stitched on afterwards. `vellum::vl_lint_rules()` lists
  `single_level_scale` and `legend_overflow` with a `grammar` tag, `rules =`
  selects them, and a plain `vellum::vl_lint()` of a compiled plot now reports
  encoding problems alongside geometric ones — `plot_lint()` is a thin wrapper
  rather than a second implementation.

  A registry rule is handed the resolved scene, which is not enough for a rule
  about the encoding, so a compiled scene carries a summary of its trained
  scales: the `kind` and `levels` of the legend-bearing ones, a couple of
  kilobytes, computed where the compile had already built them. Deliberately a
  summary and not the plot spec, which for 50,000 rows runs past a megabyte and
  would keep the data alive for as long as anyone held the scene. It leaves the
  scene's hash, serialised form and pixels untouched. A composition or table
  carries none — there is no one set of scales to report on — so those get the
  geometric findings only (#147).

* **Requires vellum >= 0.6.7**, which fixes animated SVG output: duplicate
  `<defs>` ids across frames made every frame's clip resolve to the first
  frame's — hidden for all but one frame of the cycle, and a hidden `<clipPath>`
  child clips everything away — so `anim_save(".svg")` blinked once and then
  showed an empty panel. The same release also fixes frames playing in reverse.

* **`mark_contour_filled()` closes bands that leave the grid.** A density band
  whose contour exits the estimation grid was closed with a straight chord back
  to its start, slicing triangular wedges across the panel (e.g.
  `mark_contour_filled(x = eruptions, y = waiting)` on `faithful`). Open contours
  are now closed along the grid boundary, so each band fills the correct region.

* **`mark_scalebar()` labels read left-to-right in every corner.** With a
  right-anchored `position` (`"bottomright"` / `"topright"`) the `0` and distance
  labels were mirrored, putting the distance on the left. They now always read
  `0` at the left end and the distance at the right.

* **`mark_sf()` no longer paints a constant `color` as the fill.** With both a
  constant `fill` and a constant `color`, the polygon fill took the stroke
  colour and the outline vanished. Fill and stroke are now resolved as the
  distinct channels they are: `fill` fills, `color` strokes.

# vellumplot 0.9.0

First tagged release. Everything below is included.

* **Merged choropleth regions.** `mark_sf(merge = TRUE)` dissolves adjacent
  features that share a fill into one region, so the internal borders between
  same-valued features disappear and each region is a single crisp path — no
  double-stroked seams, and exact in PDF. The union is real boolean geometry
  (`vellum::vl_path_op()`), holes and multipart features preserved. It is ignored
  on an interactive layer, where per-feature keys must survive.

* **Rotated and wrapping axis tick labels.** `axis.text.x` / `axis.text.y` now
  honour an `angle` — `theme(axis.text.x = element_text(angle = 45))` slants the
  labels and the gutter reserves the rotated height (an explicit `hjust`/`vjust`
  sets the anchor; otherwise the end of the run is pinned at the tick). When the
  labels are left horizontal, a long discrete label that would overrun its tick
  spacing wraps onto multiple lines instead of colliding with its neighbour, and
  the label row grows to fit — the per-tick companion of the title/subtitle/
  caption wrapping already shipped. Labels that already fit are unchanged.

* **Multi-page PDFs and parallel batch export.** `pdf_pages()` writes several
  plots into one PDF (a report or slide deck), one plot per page — pages may
  differ in size and each keeps its accessibility tags — or splits a single
  faceted plot into one page per facet cell. `render_all()` renders a list of
  plots to separate files across CPU cores (small multiples, batch export); it is
  byte-identical to rendering them one by one and parallelises via process forks
  on macOS/Linux (sequential on Windows). Both build on the engine's
  `vellum::pdf_pages()` / `vellum::render_all()`.

* **Real blur effects; `glow()`/`shadow()` now work on text.** `glow()` and
  `shadow()` are drawn with the engine's real Gaussian blur
  (`vellum::vl_viewport(blur=)`) — one softened, composited layer instead of a
  stack of N widened low-opacity copies. It is cheaper and smoother, and it lifts
  the old restriction that text could not glow: `mark_text()` / `mark_label()`
  now take an `effects =` argument and accept `glow()` and `shadow()` (a *sharp*
  `outline()` still needs a glyph outline the text primitive can't stroke).
  `outline()`, `motion()`, and `echo()` are unchanged. The `glow()` `layers` /
  `blend` arguments are kept for the neon look (opacity + composite) but no longer
  stack copies.

* **Relative text sizing and tabular figures.** `element_text(cex = )` sets a size
  multiplier that cascades through the theme (a base size scaled, rather than an
  absolute size at every call site). Axis tick labels are now set with tabular
  figures (`tnum`), so digit columns keep a constant width and stop jittering
  from tick to tick.

  Requires vellum >= 0.6.4.

* **Venn / Euler diagrams: `vvenn()`.** Draws 2 or 3 overlapping sets whose
  disjoint regions are rendered as **solid geometry** — computed with the
  engine's boolean set operations (`vellum::vl_path_op()`), so overlapping
  regions do not alpha-composite and stay crisp, including in PDF where a
  rasterised mask degrades. Each region is filled by how many elements fall in
  exactly that combination of sets, and labelled with that count. Input is a
  named list of member vectors or a data frame of logical membership columns.
  Requires vellum >= 0.6.4.

* **SVG icon markers: `shape = <svg>`.** A point `shape` can now be an SVG icon —
  a path `d` string (what icon sets ship) or a path to a `.svg` file — drawn as a
  crisp vector marker via `vellum::svg_grob()` instead of a built-in glyph. Pass
  a literal `d`/file for a constant icon, or map `shape` and give
  `scale_shape(values = )` one icon per level (the legend shows the icons); the
  `size` aesthetic scales them. The vector marker stays sharp at any zoom and in
  PDF, replacing the ggimage/ggsvg raster-per-point hacks. (Per-icon
  interactivity via `data_id` is not yet wired — the icons render on every
  backend.) Requires vellum >= 0.6.4.

* **An accessibility pass.** Four pieces that make a plot legible for
  colour-blind and low-vision readers, and reproducible across machines:
  - `render_plot(cvd = )` renders a `.png` through a colour-vision-deficiency
    simulation (`"protanopia"`, `"deuteranopia"`, `"tritanopia"`,
    `"achromatopsia"`) so you can *see* a palette fail.
  - `plot_lint()` flags the problems a green test suite hides: text below a
    legibility floor, colour contrast under the WCAG threshold, off-canvas or
    overlapping labels (geometric rules from the engine), plus grammar-level
    mistakes — a single-level encoding or a legend too long to read.
  - `pattern_hatch()` adds a crisp **vector** hatch fill (via `vellum::vl_hatch()`)
    that stays sharp in PDF and survives greyscale — a redundant, non-colour
    encoding, usable as a constant `fill` or a `scale_pattern()` value.
  - **Tagged PDF** now marks furniture (gridlines, ticks, panel/plot
    backgrounds) as decorative artifacts, so a screen reader skips them instead
    of reading every gridline; the figure-level alt text (`plot_alt()`) carries
    the meaning.
  - `plot_manifest()` fingerprints the resolved **fonts** and `plot_verify()`
    reports a font mismatch as a **distinct outcome** (`fonts_ok`) from a data
    mismatch (`data_ok`) — a pixel difference from a missing font is a different
    cause, and fix, than a changed dataset.

  Requires vellum >= 0.6.5.
* **`mark_contour()` traces in the engine; `isoband` is no longer needed.**
  2-D contour lines and filled contours are now traced by
  `vellum::vl_contour()` (marching squares) instead of the `isoband` package,
  which is dropped from `Suggests`. `mark_contour()` draws each contour as its
  own chained polyline (closed loops are closed), and `mark_contour_filled()`
  paints the closed rings in level order for the layered filled-density look. A
  density field still uses `MASS::kde2d()`; contouring a supplied `z` surface
  needs nothing extra. Requires vellum >= 0.6.4.

* **Label repulsion works on every panel type.** `mark_text(repel = TRUE)` /
  `mark_label(repel = TRUE)` now go through the engine placement solver
  (`vellum::vl_repel()`), which solves in device pixels and applies the answer as
  an absolute millimetre offset. It is a single post-compile pass — no second
  compile — and it is coordinate-agnostic: **faceted, polar and warped
  (`coord_trans`) plots repel correctly**, where before they errored with
  "single cartesian panel only". Labels in a faceted plot are solved per panel
  and kept inside it. The `seed` argument is now inert (the solver is
  deterministic) and is kept only for back-compatibility. Requires
  vellum >= 0.6.4.

* **`mark_text_path()` — labels that follow a curve.** A new mark that sets one
  label per group along the group's `x`/`y` path, each glyph rotated to the local
  tangent — for labelling a contour, an arc, or a trend line directly instead of
  with a legend. Split runs the usual way (a discrete `color` or a distinct
  `label` starts a new run); `hjust` slides the run along the path, `offset` lifts
  it off the baseline. Emits real, selectable `<text>` in SVG. Glyphs follow the
  tangent, so a path walked right-to-left sets its label upside-down — reverse the
  path to flip it. Requires vellum >= 0.6.4.

* **Long plot titles, subtitles and captions wrap instead of overflowing.** A
  title / subtitle / caption longer than the page now wraps onto as many lines as
  it needs, and the layout reserves the height for them (previously the text ran
  straight off the right edge). Lines are aligned to the band's `hjust`
  (left / centre / right), and short single-line labels are unchanged. Wrapping
  is to the page content width, measured in millimetres at compile time.
  Requires vellum >= 0.6.4 (which fixes `grobheight()` to account for the
  wrapping when it sizes the band's track).

* **Keys on more marks.** Now that the engine can key lines, polygons and text
  (vellum 0.6.x), the marks that could not previously be made interactive are:
  `mark_line()` / `mark_step()` (a series is one hoverable object; a colour
  mapping gives one keyed line per series), `mark_area()` / `mark_ribbon()` (the
  filled band as one polygon), `mark_text()` (each data label individually), and
  the sf polygon / choropleth marks (one keyed feature each). This release also
  fixes `mark_label()`, whose rounded background box is now keyed as the label's
  hit target — it was inert before — and `mark_interval()` / `mark_halfeye()`,
  whose interval bars now carry the datum key like their slab and centre point
  already did. Set `data_id=` (and optionally `tooltip=`) on the mark to make it
  addressable; a mark left without a key stays out of `scene_model()`, so a
  plot's furniture never becomes phantom interactive elements. Requires
  vellum >= 0.6.3.

* **Portable plot specs.** A plot is now a serializable *document*: `as_spec()`
  turns a `PlotSpec` into a plain nested list and `from_spec()` rebuilds it,
  with `spec_to_json()` / `spec_from_json()` for the JSON wire format and
  `spec_schema()` for the bundled JSON Schema. The serializer covers the
  encoding-level grammar exactly and **refuses** (with a classed
  `vellumplot_unserializable` error) anything a portable document cannot carry —
  custom transform functions, paint/pattern fills, sketch geometry, secondary
  axes — rather than dropping it silently.

* **LLM- / agent-native plotting.** `spec_fields()` summarises a data frame's
  columns and inferred encoding types to ground a model; `vplot_from_spec()` /
  `spec_diagnose()` validate a generated spec against the data and return
  *structured* diagnostics (unknown field with a suggestion, compile error)
  instead of a traceback; and `mcp_serve()` runs a pure-R Model Context Protocol
  server (bundled launcher at `system.file("mcp/server.R")`) exposing
  `get_schema` / `list_fields` / `render_spec` tools so an agent generates
  *validated data*, never executed code. `vplot_ask()` is a model-agnostic
  natural-language convenience over the three.

* **Vega-Lite interoperability.** `spec_to_vegalite()` and
  `spec_from_vegalite()` translate a plot to and from a Vega-Lite specification
  over a documented subset (marks, encodings, scales, `bin`/`count`, faceting,
  inline data, title), reporting any features they cannot map rather than
  diverging silently.

* **Click-to-source.** `inspect_source()` declares (on the plot, not as a host
  flag) that a host should surface the source data rows behind a clicked or
  hovered element; it is carried in `interaction_model()` and enacted by a host
  reading the scene's provenance (`provenance_payload()`).

* **Self-documenting, reproducible plots.** `provenance_join()` ties every drawn
  element to both its source data rows and its device-pixel geometry;
  `provenance_payload()` exposes the same as a click-to-source payload for a
  host; and `plot_manifest()` + `plot_svg(manifest = TRUE)` + `plot_verify()`
  embed a data fingerprint in an SVG so a figure can be checked against the data
  it was drawn from.

* **Map decorations.** `coord_sf(graticule = TRUE)` draws meridians and parallels
  behind the map; because they are generated in longitude/latitude and reprojected,
  they curve correctly under a projected CRS (a straight grid would be wrong).
  `mark_scalebar()` adds a segmented distance bar that sizes itself from the map's
  CRS (units `"km"`/`"m"`/`"mi"`/`"ft"`), and `mark_compass()` adds a north arrow;
  both are pinned to a panel corner via `position`. The scale bar and compass need
  no `sf` themselves, but do require a map coordinate system.

* **Keyframe animation.** `transition_states()` turns a plot into an animation
  over the levels of a column; `animate()` compiles one keyframe scene per state —
  training the scales **once over all states and freezing them**, so nothing
  retrains between frames (non-reactive keyframe animation) — and `anim_save()`
  tweens and encodes the in-between frames to a looping GIF, an animated PNG, or an
  **animated SVG** (`.svg` — resolution-independent, honours
  `prefers-reduced-motion`; `anim_save()` advises a raster format when a `.svg`
  scene is dense enough that one would be smaller) in one parallel, streaming pass
  in vellum's Rust backend. `ease_aes()` sets the easing
  (`linear`, or a family like `cubic`/`sine`/`elastic`/`bounce` with an
  `-in`/`-out`/`-in-out` direction). Position, size, alpha and colour interpolate
  (colour perceptually, in Oklab); discrete attributes snap. `transition_time()`
  is the continuous-time variant — it allocates frames in proportion to the time
  gaps, so unevenly spaced times play at a constant rate. Giving an animated mark
  a `data_id` enables per-element **enter/exit**: elements that appear fade in and
  elements that leave fade out (matched elements tween as usual); without it a
  stable element set is assumed. `transition_reveal()` wipes the plot into view
  left to right (the "line draws itself" animation) by growing a clip rectangle
  over a single compile. See the *Animation* article and
  `inst/examples/28-animation.R`. Requires the development version of vellum
  (`vl_render_animation()`).

# vellumplot 0.8.0

* **Flow maps: `mark_flow_map()`.** A one-to-many flow map on a `vgraph()` plot:
  a single `root` fans out to every destination along smooth, merging branches
  whose width tracks the flow volume (the Minard / migration-map idiom).
  `type = "spiral"` (default) builds an angle-restricted spiral tree from the
  \pkg{edgebundle} package; `type = "steiner"` builds an approximate Steiner tree
  (additionally needs \pkg{interp}). Edge `weight` drives the flow, which is
  mapped onto `width_range`. Needs the suggested \pkg{edgebundle} package.

* **Edge bundling: `mark_edge_bundle()`.** A drop-in alternative to `mark_edges()`
  that routes a graph's edges as bundled curves instead of straight lines, so a
  dense node-link hairball collapses into a few legible trunks. `type` selects the
  algorithm (`"force"`, `"divided"`, `"stub"`, `"path"`, `"hammer"`, `"mingle"`)
  and `params` passes tuning through to the \pkg{edgebundle} package, which
  computes the geometry; vellumplot draws the returned paths with the usual edge
  aesthetics (faint by default so overlapping trunks read as density). Needs the
  suggested \pkg{edgebundle} package (`>= 1.0.0`).

* **Chord diagrams: `vchord()`.** Wraps a weighted flow (a `from`/`to`/`value`
  list or a square flow matrix) onto a circle -- one arc per node, one ribbon per
  flow through the centre. Directed: each node's arc splits into an outgoing then
  an incoming block, so `a -> b` and `b -> a` are distinct ribbons and self-flows
  loop within a node's sector. `direction` shows a ribbon's direction by fading
  it from source to target (`"gradient"`, default), stopping it short of the
  target (`"gap"`), or `"both"`. `sort`, `gap`, and `link_color` echo
  `vsankey()`; `mark_chord()` is the exported layer.

* **Removed the unused `max_overlaps` argument** from `mark_text()`,
  `mark_label()`, and `mark_node_text()`. It was reserved for a never-implemented
  overlap cap and did nothing; drop it from any call.

* **More robust handling of degenerate and extreme inputs.** A single distinct
  value on an axis under `coord_fixed()` / `coord_sf()` no longer collapses the
  panel to nothing; a map centred on a pole no longer gets a runaway aspect
  ratio; a circle-pack of collinear equal-value leaves no longer produces `NaN`
  coordinates; and a pathologically deep hierarchy or dendrogram aborts with a
  clear "too deep" message instead of overflowing the call stack.

* **`mark_line(window = )` requires a positive integer `k`.** A zero, negative,
  or fractional window size now errors clearly instead of silently producing a
  wrong (or empty) result.

* **A computed-constant `if_false` in `condition()` is carried into the
  interaction model.** A negative literal (e.g. `-0.5`) or a computed constant
  (e.g. `grey(0.5)`) was previously dropped; only genuine per-row column
  references are now excluded (and carried per element instead).

* **A faceted layer whose own data lacks the facet variable still draws on every
  panel**, but a genuine evaluation error there is no longer silently swallowed
  into that fallback.

* **`mark_point(auto = TRUE)` no longer datashades under a warped coordinate
  system.** Under `coord_polar()` / `coord_trans()` the >50000-row datashade
  fallback — which bins in linear data space — would rasterise into the wrong
  place; it is now skipped and the vector path draws, matching the documented
  behaviour and the other `auto` marks (line/step/segment/edges).

* **A sankey column with too many nodes errors clearly.** When a column has more
  nodes than its inter-node gaps can fit, the node/ribbon heights previously went
  negative and spilled outside the panel; `vsankey()` / `mark_sankey()` now abort
  with a message pointing at `node_gap`.

* **Aggregating stats drop missing summaries consistently, with a warning.** For
  a value-summarising bar (`mark_bar(y = , fun = )` and friends), a category
  whose summary is `NA` (an empty group, or a summary of data containing `NA`
  when the function does not remove it) is now dropped with a warning on **both**
  the grouped and ungrouped paths, which previously treated `NA` differently. A
  layer mapping both `color` and `fill` also keeps both aligned after
  aggregation (`fill` could previously be left at the wrong length).

* **Clearer errors for malformed scale bounds.** `scale_x_continuous()`,
  `scale_y_continuous()`, `scale_size()`, `scale_size_area()`, `scale_alpha()`,
  `scale_edge_width()`, and `scale_edge_alpha()` now reject a `limits` (or output
  `range`) that is not a length-2 vector — and, for the size/alpha/edge scales, a
  non-numeric one — with a clear message, instead of failing later with a cryptic
  low-level error (e.g. `'length = 2' in coercion to 'logical(1)'`). `scale_size_area()`
  also validates `max_size`.

* **`add_marginal()` rejects a non-numeric mapping.** A factor `x`/`y` previously
  slipped through and the marginal was computed over the integer level codes
  rather than the data; it now errors clearly, as a character column already did.

* **Stricter validation of interactivity and layout arguments.**
  `select_point()`/`select_interval()` reject a non-logical `toggle`/`empty` (it
  was silently coerced to `FALSE`); `vgraph()` validates `width`/`height` like
  `vsankey()`/`vhierarchy()`; and `area()` rejects a non-numeric or non-integer
  cell index instead of aborting with an opaque "missing value where TRUE/FALSE
  needed".

* **`mark_text()` honours `hjust`/`vjust` passed as a variable.** They were only
  read as constant params, so a value routed through a variable (as
  `vdendrogram()` does per direction) silently fell back to centred, letting leaf
  labels run over the edges. `.emit_text()` now reads them value-first, like
  `angle`. `vdendrogram()` leaf labels also get a larger default gap.

* **Dendrograms and unrooted trees.** `vgraph()` now accepts a base
  `hclust`/`dendrogram` and gains a height-aware `"dendrogram"` layout plus an
  `"unrooted"` layout (via `graphlayouts::layout_as_tree_unrooted()`, needs
  graphlayouts >= 1.2.5). `mark_edges(routing = "elbow")` gained `elbow_at` /
  `elbow_axis` so the elbow can be the dendrogram *bracket* (corner at the
  parent's level) rather than only the midpoint S-bend. `vdendrogram()` is a
  one-line preset: bracket edges, leaf labels, `direction`, and `k` to cut the
  tree and colour clusters. Also fixes `mark_node_text(label = )` so an explicit
  `label` mapping overrides the default vertex name (it was previously ignored).

* **`vhierarchy()` respects `scale_fill_*()`.** By default nodes are still
  coloured by their depth-1 branch and lightened with depth, but the branch is
  now an ordinary discrete fill scale, so `scale_fill_manual()` /
  `scale_fill_discrete()` recolour the branches and a `lighten` argument controls
  the depth fade (`0` = flat colour per branch). Mapping `fill` to a node column
  instead colours every node by that variable (discrete or continuous, with the
  matching `scale_fill_*()`), with no depth fade.

# vellumplot 0.7.0

* **New `vhierarchy()` for space-filling hierarchies — breaking.**
  `vhierarchy(id, parent, value, type = )` draws a tree four ways from one
  parent list, switching only `type`: `"sunburst"` (default), `"icicle"`,
  `"treemap"`, or `"circlepack"`. This **replaces `vsunburst()`** —
  `vhierarchy(..., type = "sunburst")` reproduces the old radial sunburst, and
  `vsunburst()` / `mark_sunburst()` are removed. Treemaps use a squarified
  layout; circle-pack is a faithful port of d3's front-chain packing. `mark_hierarchy()`
  is the exported layer.

* **Sunbursts, polar pies, and self-loops render un-mirrored.** Requires
  vellum (>= 0.5.1), which fixes `sector_grob()`/`loop_grob()` to draw in the
  same y-up frame as every other primitive. Sunburst segment labels now sit on
  their own wedges (they previously landed on the vertical mirror on unbalanced
  trees), sunbursts wind clockwise from twelve o'clock as documented, and graph
  self-loops point into the empty gap between a vertex's incident edges rather
  than its mirror image.

* **`select_point(group_by=)` now links whole groups.** A point selection with
  `group_by` / `fields` groups elements sharing those column values, so hovering
  or clicking one mark highlights (and `condition()` spotlights) its whole group
  — the "hover a series, light up the series" behaviour. Implemented by emitting
  the group as the element `hover_group`, so it needs no widget change; a
  user-declared `hover_group` still wins. Inert on a static render.

* **Declarative interactivity (new).** Interaction is now part of the plot spec
  rather than something a host is configured to do. Declare a named selection
  with `select_point()` (click/hover) or `select_interval()` (brush/lasso), then
  refer to it from `condition()` (style an aesthetic by selection membership),
  `filter_by()` (show only members — point a second view at the same selection
  for cross-filtering), or `bind_scale()` (drive another panel's view, for
  overview + detail). `add_selection()` shares a free-standing selection across
  views; `interaction_model()` returns the compiled declaration block a host
  reads. Every node is inert on a static render (a plot with interactions renders
  byte-for-byte identically to one without), and `condition("s", g, "grey80")`
  trains scales and draws exactly like `color = g`. Enacted by `vellumwidget`.

# vellumplot 0.6.0

* **Sankey styling options.** `vsankey()` / `mark_sankey()` gain `show_values`
  (append each node's value to its label), `flow_color` (`"source"`, `"target"`,
  or `"gradient"` — a source-to-target colour fade per ribbon), and
  `node_width` / `node_gap` to tune the node rectangles.

* **Sankey crossing minimisation.** `vsankey()` now orders the nodes within each
  column with the Sugiyama barycenter heuristic to minimise ribbon crossings
  (previously first-appearance order, which left many avoidable crossings). The
  reordering is deterministic and pure R. A related fix stacks each node's ribbon
  slices to meet the node in the same vertical order as the nodes they connect to,
  removing the remaining crossings within a fan of ribbons.

* **Sunburst rendering fixes.** `vsunburst()` now colours wedges by their
  top-level branch (each branch a distinct hue, lightened with depth) instead of
  a single colour per ring — sibling branches were previously indistinguishable.
  It also starts the first wedge at twelve o'clock and winds clockwise, matching
  the package's pies/roses (`coord_polar`) rather than starting at three o'clock
  counter-clockwise.

* **Sunburst / radial hierarchies: `vsunburst()`.** A new plot type for
  part-of-whole hierarchies, from a *parent list* — `id`, `parent` (`NA` at the
  root), and `value` (leaf values; internal nodes sum their children). Depth maps
  to a ring and each node's angular span is its share of its parent's, drawn as
  one batched `sector_grob` in an aspect-locked, axis-free square panel (mirroring
  `vsankey()`/`vgraph()`). `inner_radius` opens a central hole (a donut/ring
  sunburst); nodes are coloured by depth. `mark_sunburst()` is the exported layer.
  See `vignette("flows-and-hierarchies")`.

* **Sankey / flow diagrams: `vsankey()`.** A new plot type for layered flows,
  built from a *flow list* — one row per flow with `from`, `to`, and `value` (the
  ribbon width). Nodes are the union of `from`/`to`; a node that is both a source
  and a target makes the diagram multi-stage. `vsankey(data, from, to, value)`
  returns a ready, axis-free plot (mirroring `vgraph()`); `mark_sankey()` is the
  exported layer it adds. The layout is computed R-side (longest-path layering,
  value-proportional node heights and ribbon widths, filled Bézier ribbons) and is
  deterministic. Flows must form a DAG; nodes are coloured from the qualitative
  palette. See `vignette("flows-and-hierarchies")`.

* **Uncertainty marks: `mark_halfeye()` and `mark_interval()`.** ggdist-style
  slab + interval marks for sample/posterior input (many `y` rows per categorical
  `x`). `mark_halfeye()` draws a one-sided density slab with a point-interval at
  its base — the median (or `point = "mean"`), a thick inner and thin outer
  equal-tailed quantile interval at the `.width` probabilities (default
  `c(0.66, 0.95)`); `mark_interval()` is the point-interval alone. A natural fit
  for visualising posterior draws (e.g. from \pkg{brms}).

* **`coord_radial()` and `scale_size_area()`.** `coord_radial()` is a fuller
  polar system (ggplot2 3.5's name): besides `theta`/`start`/`direction` it takes
  `end` to sweep only a **partial arc** (e.g. `start = -pi/2, end = pi/2` for a
  semicircular gauge) and `inner_radius` for a **donut hole**; with the defaults
  it matches `coord_polar()`. `scale_size_area()` maps a value to the marker's
  **area** (value `0` → size `0`), the perceptually honest default for bubble
  charts, with `max_size` the size of the largest value.

* **Sankey labels stay on-panel.** `vsankey()` now reserves horizontal margin for
  node labels, so the source column's labels (drawn to their left) and the
  terminal column's (drawn to their right) no longer clip at the panel edge.

* **Robustness of the new marks.** `vsunburst()`/`mark_sunburst()` now reject a
  missing or negative leaf `value` with a clear message (instead of a cryptic
  downstream error), and `mark_sunburst()` validates `inner_radius` like
  `vsunburst()` and `coord_radial()` do. `mark_halfeye()`/`mark_interval()` skip a
  category with fewer than two finite observations (with a warning) rather than
  drawing an empty interval, and reject a `width =` argument (a likely typo for
  `.width`) that was previously ignored silently.

* **Consistent argument names for the hole radius / ridge height.** The
  central-hole fraction is now spelled `inner_radius` everywhere it appears:
  `coord_radial()`, `vsunburst()`/`mark_sunburst()`, and `mark_donut()` (was
  `hole`). `mark_ridgeline()`'s overlap control is now `height` (was `scale`,
  which collided with `mark_halfeye(scale=)`, a different quantity).

* **Consistent `sketch` / `blend` passthrough.** `mark_ecdf()`, `mark_contour()`,
  `mark_contour_filled()`, `mark_dotplot()`, `mark_qq()`, and `mark_qq_line()` now
  accept a per-layer `sketch =` (their emitters already honoured it), and
  `mark_pie()` / `mark_donut()` accept `blend =`, matching the rest of the mark
  surface.

* **Parameterised position adjustments.** New `position_nudge()`,
  `position_jitter()`, `position_dodge()`, `position_dodge2()`, and
  `position_jitterdodge()` give a mark's `position` tunable parameters (a bare
  string like `"dodge"` still works with the defaults). Adds three adjustments:
  `nudge` (shift every element by a constant in data units), `dodge2` (dodge by
  the groups actually present at each x, filling the band with a `padding` gap —
  so ragged groupings stay centred), and `jitterdodge` (jitter points within
  their dodged slot). `position_jitter(width=, height=, seed=)` and
  `position_dodge(width=)` expose the previously-fixed jitter/dodge extents. A
  plot using the old string positions is unchanged.

* **Label repulsion: `mark_text(repel = TRUE)` / `mark_label(repel = TRUE)`.**
  Overlapping text labels are moved apart with a force-directed layout
  (ggrepel-style), each keeping a thin leader line back to its point. Because the
  plot size is fixed on the spec, the repulsion is resolved *exactly* against the
  true rendered panel — a two-pass compile that reads the panel's device geometry
  from `vellum::scene_model()`, relaxes the label boxes in pixel space, and maps
  the result back — so it needs no approximation and no `vellum` change, and is
  deterministic under `seed`. Tunable via `box_padding`, `point_padding`,
  `min_segment_length`, and `seed`. Limited to a single cartesian panel for now
  (facets / composition / polar error clearly).

* **`mark_line(window = )`: rolling / cumulative / offset transforms.** A line can
  now transform its `y` per group (over rows ordered by `x`) before drawing —
  moving `mean`/`sum`/`median`/`min`/`max` over a window of `k`, running
  `cumsum`/`cummean`/`cummax`/`cummin`, `lag`/`lead` shifts, or `rank`. Pass an op
  name (`window = "mean"`) or a list (`window = list(op = "mean", k = 7, align =
  "right", partial = TRUE)`); `align` is trailing/leading/centred and `partial`
  fills the edges from the shorter window so the line stays continuous. A plot
  without `window` is unchanged.

* **Diverging colour scales: `scale_color_gradient2()` / `scale_fill_gradient2()`.**
  A three-point ramp (`low`--`mid`--`high`) centred on `midpoint` (default `0`),
  rescaled *about the midpoint* (`scales::rescale_mid`) so the neutral colour sits
  on the chosen value and each side spans as far as the data reaches — the correct
  scale for signed / anomaly data. A diverging continuous colorbar also reports
  `midpoint` + `diverging` in its `colorbar` descriptor, so an interactive host can
  centre a value-range filter on the neutral value.

* **`symlog` position transform.** `scale_x_continuous(trans = "symlog")` (and
  `y`) adds a symmetric-log axis: linear through zero, logarithmic in the tails
  (`sign(x) · log10(1 + |x|)`), so signed data spanning several orders of magnitude
  — including zero and negatives, which `log10` cannot show — reads on one axis.
  Breaks sit at zero and signed powers of ten. The transform name flows into the
  panel `scales` descriptor like the other transforms.

* **New group-region marks: `mark_ellipse()` and `mark_hull()`.** Both enclose a
  set of `(x, y)` points in a single region drawn over a scatter — one region per
  group when a `color`/`fill` is mapped. `mark_ellipse()` draws a covariance
  ellipse (`type = "t"` robust default via \pkg{MASS}, or `"norm"`/`"euclid"`),
  following ggplot2's `stat_ellipse()`; `mark_hull()` draws the convex hull. Both
  are unfilled boundaries by default (map/set a `fill` to shade them) and need at
  least 3 points per group. The region's boundary trains the position scales, so
  an ellipse that bulges past the data is not clipped.

* **`mark_smooth()` gains real smoothing methods.** Beyond `"lm"`, the smooth
  mark now fits `"loess"` (local regression, `span =`), `"glm"` (with a `family`
  via `method.args`, e.g. logistic — the fit and its ribbon are back-transformed
  from the link scale), `"gam"` (a penalised smooth, default `y ~ s(x)`; needs
  \pkg{mgcv}), and `"rq"` (quantile regression at a single `method.args$tau`;
  needs \pkg{quantreg}, line only, no ribbon). The default `method = "auto"`
  picks `loess` for small groups (< 1000 points) and `gam` for large ones, as in
  ggplot2. New `formula`, `span`, and `method.args` arguments; `glm`/`gam` bands
  use a normal interval, `lm`/`loess` a t-interval. Previously `mark_smooth()`
  errored on any method other than `"lm"`. \pkg{mgcv} and \pkg{quantreg} are
  Suggests — a gated method errors clearly if its package is absent.

* **Data panels are emitted as pannable, gridlines tagged.** Cartesian data panels
  (including `coord_flip` and a linear `coord_trans`) now push a `pannable` vellum
  viewport, and gridlines carry `role = "grid"`. This is inert for static rendering
  but lets an interactive host (`vellumwidget`) pan/zoom a panel's marks while its
  clip + axes stay fixed and hide/redraw gridlines — the groundwork for axis-aware
  zoom. Polar / nonlinear-`coord_trans` panels stay non-pannable. Requires the
  current development `vellum`.

* **Continuous colorbar filter metadata.** A continuous `color` scale now attaches
  each mark's colour value as `filter_value` in its element `meta`, and a `colorbar`
  descriptor (value domain + orientation) to the gradient-bar grob. Together (via
  `vellum::scene_model()`) these let a host such as `vellumwidget` overlay an
  interactive value-range filter on the colorbar. Discrete/binned colour scales are
  unaffected. No change to rendered output.

* **Panels now carry a `scales` descriptor for interactive hosts.** Each cartesian
  data panel's viewport gains a `meta$scales` record — per axis: `type`
  (continuous / log10 / discrete / binned / date / datetime), `transform`, the
  data and native domains, tick breaks + labels, and `time_unit` for date/datetime
  axes. It surfaces via `vellum::scene_model()$panels$meta` and lets a host (e.g.
  `vellumwidget`) map device pixels back to data values — so a brush or a reported
  view can be expressed in data coordinates, not just pixels. Requires
  `vellum` (>= 0.4.0.9000). Internal: trained position scales now also record their
  `transform` name and, for date/time axes, a `time_unit` (previously the
  date/datetime nature was lost after training). No change to rendered output.

# vellumplot 0.5.0

* **Rich `md()` legend titles no longer clip.** A legend built from a rich title
  (`scale_color_continuous(name = md("Power (hp m^2^)"))`, `labs(color = md(...))`)
  measured the title as zero width, so the legend reserved no room for it and the
  drawn title spilled off the page. Titles are now measured through vellum's rich
  text path (`vl_strwidth()`), reserving the space they actually occupy.

* **Secondary axes.** Continuous position scales gain a `sec.axis` argument fed by
  the new `sec_axis()` / `dup_axis()`: a second set of ticks and labels on the
  opposite edge (top for `x`, right for `y`), computed as a 1:1 monotonic
  transform of the primary axis — e.g.
  `scale_x_continuous(sec.axis = sec_axis(~ . * 1.8 + 32, name = "°F"))` for a
  unit conversion, or `dup_axis()` to duplicate an axis on a wide plot. The
  transform is a formula, function, or `scales::transform_*()` object. Scoped to
  the default Cartesian system with shared facet scales; combining it with
  `coord_flip()` / `coord_polar()` / `coord_trans()`, free facet scales, or
  `add_marginal()` raises a clear error, and it is not drawn inside a plot
  composition. A plot without a `sec.axis` is byte-for-byte unchanged.
* **Datashading now covers dense lines and large-graph edges.** `mark_line()`,
  `mark_step()`, `mark_segment()`, and `mark_edges()` gain an `auto = TRUE`
  switch (parallel to `mark_point(auto = TRUE)`): past a row threshold the layer
  rasterises into a line- / segment-density field via `vellum::datashade_lines()`
  / `vellum::datashade_segments()` instead of emitting one vector per element, so
  overplotted timeseries stacks and graph "hairballs" render fast and honestly.
  The fallback is skipped under a warped coordinate system (`coord_polar()` /
  `coord_trans()`), which keeps the vector path. (Area / ribbon datashading is not
  yet available, pending area-fill support in vellum.)
* **`mark_datashade(spread = )`** exposes vellum's post-aggregation spreading:
  `NULL` (default, raw output), a positive integer for a fixed pixel radius
  (`vellum::spread()`), or `"auto"` for density-adaptive dilation
  (`vellum::dynspread()`). Datashaded lines and segments default to `"auto"` so
  single-pixel marks stay visible.
* **Continuous and binned colour scales now interpolate perceptually (Oklab) by
  default.** A colour ramp built from a plain colour vector (e.g.
  `scale_color_gradient(low, high)` or `scale_color_continuous(palette = c(...))`)
  is blended in the perceptually-uniform Oklab space instead of sRGB, so it no
  longer passes through muddy, over-dark midtones or drifts in hue — the ramp and
  its legend colourbar read evenly. Designed perceptual palettes (the `batlow`
  default, `hcl.colors()` names) are already uniform and are unchanged. Set
  `options(vellumplot.color.interpolation = "srgb")` (or `"lab"`) to restore the
  old behaviour. Gradient *fills* opt in per gradient with
  `linear_gradient(..., interpolation = "oklab")` (passed through to vellum).

* **Error bars and line ranges are now interactive.** `mark_errorbar()` and
  `mark_linerange()` thread a declared `data_id`/`tooltip`/`hover_group` through to
  their drawn segments, so each bar is keyed to its datum — it appears in
  `scene_model()` and `plot_provenance()` and carries a `data-key` in the SVG,
  ready for hover/click/select in a widget (a bar's cap segments share the bar's
  key). `mark_boxplot()` already keyed each box by its category; this completes the
  statistical marks. A mark with no interactivity declared is unchanged.

* **Two more shapes in `scale_shape()`.** The `shape` aesthetic's default palette
  now extends to `"triangle_down"` (a downward triangle) and `"star"` (a
  five-pointed star) after the original six, so a mapped `shape` covers up to eight
  levels without an explicit scale, and both are accepted as `scale_shape(values=)`.
  Requires the accompanying `vellum` dev version. (A constant `shape = "star"` on a
  mark already worked once vellum gained the shape.)

# vellumplot 0.4.0

## New features

* **Categorical datashading in one call.** `mark_datashade()` now accepts a mapped
  discrete `color`/`fill` aesthetic and shades **categorically** (datashader's
  `count_cat`): each category is aggregated separately and every cell is coloured by
  the count-weighted average of the category hues it holds, opacity by density — so
  you see which category dominates where, and where they mix, **with a colour
  legend**. Category hues come from the usual discrete colour scale
  (`scale_color_*()` apply). This replaces the old idiom of stacking one datashade
  layer per category with `blend = "screen"`. New `span`/`clip` arguments clamp the
  density range (absolute limits or percentiles) so a few extreme cells don't
  flatten the rest. Requires the accompanying `vellum` dev version.

* **`mark_image()` draws images at data points.** A new mark places a bitmap
  image (e.g. a country flag or company logo) at each `(x, y)`, replacing the
  usual point marker. `src` is a column of local file paths or one constant
  path, and `size` sets the height in millimetres (the width follows each
  image's native aspect ratio). Requires the suggested `magick` package (#33).

* **`coord_trans()` — nonlinear display transforms.** Warps the *display* of one
  or both position axes after the scale has trained, so gridlines bunch up and
  straight lines curve while the axis keeps its original data-value labels (unlike
  `scale_*(trans=)`, which rescales the data and relabels in transformed space).
  Each of `x`/`y` takes a transform name (`"log10"`, `"sqrt"`, `"identity"`) or a
  `scales::transform_*()` object, e.g. `coord_trans(y = "log10")`. Supports the
  common marks (points, lines, areas/ribbons, bars, tiles, smooths, text, …);
  segment/interval, boxplot, edges, and raster/datashade marks are not warped yet
  and raise a clear error under `coord_trans()`.

* **`mark_violin()` and `mark_ridgeline()` no longer clip at the edges.** The
  position scales are now trained to include each mark's drawn footprint -- the
  full kernel-density support for a violin, and the ridge height (`scale`) above
  the top category for a ridgeline -- so the tallest ridge and the density tails
  are no longer cropped by the panel. Explicit axis limits still take precedence.

* **Marginal distributions with `add_marginal()`.** A new plot modifier draws a
  density or histogram of the panel's `x` variable along the top edge and of its
  `y` variable along the right edge, each sharing the scatter's axis so they line
  up (the vellumplot analogue of `ggExtra::ggMarginal()`). Pick the distribution
  with `type = "density"` / `"histogram"`, the edges with `sides` (`"t"`, `"r"`,
  `"tr"`), and the extent with `size`; `group = TRUE` splits each marginal by a
  discrete `color`/`fill` mapping in the matching palette. It reads `x`/`y` from
  the first plain (identity-stat) layer and reuses the existing density/bin
  stats, so no axes or legends are duplicated. This version supports a single
  panel only (an error is raised with facets, a non-Cartesian coordinate system,
  or a locked aspect ratio).

* **Positional constants now render.** A bare literal on a coordinate channel
  (e.g. `mark_segment(x = i, y = 0, xend = i, yend = value)` for a lollipop
  baseline) is now treated as a constant-valued *coordinate* rather than a style
  param, so it populates the layer's values and trains the position scale.
  Previously `y = 0` was dropped from scale training, collapsing each segment's
  start onto its end (zero-length, invisible), and a segment-only plot errored
  with "Every layer needs an x and y encoding". Segment/edge endpoints (`xend` /
  `yend`) now also extend the axis domain, so a segment-only plot derives its
  range from both ends. Affects the positional channels `x`, `y`, `xend`,
  `yend`, `xmin`, `xmax`, `ymin`, `ymax`.

* **Faster `mark_sf()` on large maps.** Non-interactive sf layers now batch all
  features that share a resolved fill/colour into a single grob per style group
  (polygons into one `evenodd` `path_grob`, lines into one NA-separated
  `lines_grob`), instead of one grob per feature, and the layer's bounding box is
  computed in a single linear pass rather than by growing coordinate vectors. On
  a 40k-polygon grid this cut scene compilation from ~24s to ~0.8s. Interactive
  layers (those declaring `data_id` / `tooltip` / `hover_group`) are unchanged —
  they still emit one keyed grob per feature so every feature stays addressable.
  Note: because a batched polygon group is filled with one `evenodd` rule, two
  same-fill features that *overlap* would cancel in the overlap region (the same
  property a self-overlapping `MULTIPOLYGON` already has); disjoint features —
  every real choropleth and coverage map — are unaffected.

## Bug fixes

* **`linetype` now works on stroked marks.** `mark_segment()`, `mark_linerange()`,
  `mark_rule()`, `mark_errorbar()`, `mark_rug()`, `mark_edges()`, and
  `mark_contour()` (and `annotate("segment", ...)`) previously ignored
  `linetype` and always drew solid lines; they now honour it, consistent with
  `mark_line()` (#30).

* **`annotate("rect", ...)` honours infinite bounds.** A bound of `-Inf`/`Inf`
  (e.g. `xmin = -Inf, xmax = Inf` for a full-width band) now extends the
  rectangle to the panel edge, matching ggplot2, instead of silently rendering
  nothing (#29).

* **Continuous labels no longer group thousands by default.** A default
  continuous axis or legend now renders 4-digit-plus values without a grouping
  separator, so years and IDs read `2010` instead of `2 010` (#27). Pass
  explicit `labels` to restore grouping.

* **More inputs fail fast.** A `design` layout area must be a solid rectangle (and
  `area()` requires `t <= b`, `l <= r`); a per-row interactivity value
  (`tooltip`/`data_id`/…) must be length 1 or the data's row count; and
  `add_marginal()` requires a cleanly numeric column. Each now errors clearly
  instead of silently mis-rendering.
* **Clearer alt text for grid facets.** A `facet_grid()` plot's automatic alt text
  now distinguishes the row and column variables (e.g. "Faceted by rows cyl,
  columns am") instead of running them together.

* **Compositions can be themed.** `theme()` now accepts a composition (from
  [concat()] / [hconcat()] / [vconcat()]) and styles its figure-level chrome —
  the title/subtitle/caption bands, the collected legend, panel spacing, and
  tags. Previously a composition's figure chrome always used the default theme.
  Each sub-plot still carries its own theme.

* **Gradient fills fail clearly on marks that don't support them.** A
  `fill = linear_gradient(...)` is painted by `mark_area()`, `mark_ribbon()`, and
  `mark_bar()`; on any other mark it now raises a clear error instead of leaking
  an undefined paint into the renderer.

* **Inputs are validated up front.** `theme()` now checks `panel.spacing`,
  `plot.margin`, `aspect.ratio`, and `axis.ticks.length` (and rejects `NA`/`Inf`
  in any numeric setting); `vplot()` checks `width`/`height` the way it already
  checked `dpi`; a discrete `shape`/`linetype` scale with more categories than
  its palette errors instead of silently reusing a glyph; and a binned colour
  scale given fewer than two breaks errors instead of crashing in
  `colorRampPalette()`. Valid input is unaffected.

* **Legends read consistently.** A horizontal (top/bottom) continuous colour
  legend now draws the `NA` key that the vertical one always did.
  `guide_legend(reverse = TRUE)` now actually flips a continuous colourbar
  (previously a silent no-op) and no longer desyncs a binned colour scale's
  boundaries from its swatches. A merged colour+shape legend's swatches are now
  tagged for interactive highlight/select. Minor gridlines past the outermost
  breaks use the local break spacing at each end, so they sit correctly when
  breaks are unevenly spaced.

* **Numeric facets order numerically.** Faceting on a numeric variable now orders
  panels `1, 2, 10` rather than the lexicographic `1, 10, 2`; multi-variable
  facets order by each variable's own type.

* **Log/reversed axes.** Bars and areas on a `scale_*(trans = "log10")` (or
  `"sqrt"`) axis draw from the axis floor instead of a degenerate shape (the zero
  baseline that maps to `-Inf` is clamped into the trained range). A descending
  limit such as `ylim(hi, lo)` on a bar/area is no longer silently un-reversed by
  the zero baseline.

* **Grouped histogram density.** `after_stat(density)` / `after_stat(prop)` on a
  grouped histogram or bar now normalizes per group (each group integrates/sums
  to 1), matching ggplot2, rather than dividing by the grand total.

* **Clearer errors instead of silent surprises or crashes.** `coord_trans()`
  rejects a `"reverse"` transform (a no-op there — use
  `scale_*(trans = "reverse")`) and rejects bar/area marks on a nonlinearly
  transformed axis (a zero baseline has no place on a log display). Histogram /
  2-D bin / hexbin stats abort cleanly on all-`NA` input instead of a cryptic
  `seq()` error. `mark_raster()` and z-surface contours reject a grid with a
  duplicated cell (previously a silent transparent/`NA` hole). Composition
  auto-tags continue past 26 sub-plots (`Z`, `AA`, `AB`, …) instead of `NA`.

* **`mark_area()` now stacks.** `mark_area(position = ...)` was silently ignored;
  it gains a `position` argument (`"stack"` default, plus `"fill"` and
  `"identity"`), so areas sharing an `x` with a mapped `fill`/`color` combine into
  a band instead of overlapping from the zero baseline. An area with no fill
  mapping is unchanged.

* **Mapped aesthetics that were silently dropped now take effect.** A mapped
  `fill` on `mark_label()` colours the label background (previously it always fell
  back to white); a mapped or constant `alpha` is honoured per category/tick by
  `mark_violin()`, `mark_ridgeline()`, `mark_contour()`, `mark_contour_filled()`
  and `mark_rug()` (previously collapsed to one value or ignored), and `mark_rug()`
  also honours a mapped `color`.

* **British spelling works everywhere.** `hover_colour`/`selected_colour` are now
  recognised as interactivity arguments (previously only the American spelling
  was), and `mark_bin2d()`/`mark_hex()` no longer add a default count fill when a
  British `colour =` is supplied.

* **`vgraph()` warns on a layout-column clash.** A vertex/edge attribute named
  `x`/`y` (or `xend`/`yend`) is overwritten by the layout coordinates; that used
  to happen silently and now emits a warning naming the attribute.

# vellumplot 0.3.0

* Adopted vellum's renamed `vl_*` graphics primitives (`vl_gpar()`, `vl_unit()`,
  `vl_viewport()`), which no longer mask grid.

* **NA legend keys for `size` / `shape`.** A mapped `size` or `shape` aesthetic
  whose data contains missing values now shows an "NA" key (as the colour scales
  already did). Also fixes a crash: NA in a `shape` mapping previously errored
  (`Unknown point shape: NA`) — it now draws as a neutral circle.

* **2-D density contours.** `mark_contour()` draws iso-density contour lines of a
  2-D point cloud and `mark_contour_filled()` fills the bands between them
  (coloured by level automatically). By default the field is a kernel density
  estimate (via `MASS::kde2d()`); map a `z` aesthetic to contour a supplied
  surface over a regular `x`/`y` grid instead. Contour tracing uses the `isoband`
  package (both `isoband` and `MASS` are Suggests).

* **Binned position scales.** `scale_x_binned()` / `scale_y_binned()` cut a
  continuous axis into bins — ticks at the bin boundaries, each datum drawn at its
  bin centre — reusing the binned-colour classification (`style`/`n`, or explicit
  `breaks`). `mark_bar()` sizes to the bin width.

* **Rich and multi-line text labels.** `mark_text()` now carries rich labels
  through to the renderer instead of flattening them: map `label = md(<expr>)` for
  a per-datum styled label (bold/italic/super-/subscript/colour), and plain labels
  may contain newlines (`\n`) to wrap onto stacked lines. Requires vellum's
  development version. (`mark_label()`'s background box does not yet support rich
  labels.)

* **Accessibility (alt text).** Every compiled plot is now an accessible image
  by default. `plot_alt()` returns a plot's text alternative — an author-written
  string from the new `labs(alt = )`, or a prose summary vellumplot generates from the
  spec (chart type, x/y/colour/size mappings, observation count, faceting). At the
  compile seam the plot **title** becomes the scene's accessible name and this alt
  text becomes its description, so `render_plot()` output carries `role="img"` +
  `<title>`/`<desc>` in SVG and a tagged `Figure` with `Alt` in PDF (via vellum).
  See the new *Accessibility* article. Requires vellum's development version
  (>= 0.1.1.9000) for the accessible SVG/PDF backend.

* **British spelling.** `mark_*(colour = )` is now honoured as an alias for
  `color` (previously the mapping was silently dropped and no colour scale was
  trained). Added `scale_colour_*()` aliases for the `scale_color_*()` family.

* **Faithful alt text.** The auto-generated description now matches what the plot
  actually renders: it honours `scale_*(name = )` axis/legend titles, names the
  implicit "count" axis of a count bar, and describes a network graph by its node
  and edge counts instead of its (meaningless) layout x/y axes.

# vellumplot 0.2.1

Consumes `vellum`'s new compound `native + mm` unit (requires vellum >= 0.1.1).

* **Label nudges**: `mark_text()` / `mark_label()` gain `nudge_x` / `nudge_y`
  (in millimetres, `+x` right / `+y` up) that shift a label by an exact absolute
  distance, device-resolved so the nudge is constant regardless of scale or panel
  aspect.
* **Device-exact drop shadows**: `shadow()`'s `x` / `y` offset is now an absolute
  distance in **millimetres** (was a panel-relative npc fraction), so a drop
  shadow stays the same physical distance and is isotropic on non-square panels.
  Defaults changed accordingly (a small down-right drop). Replaces the previous
  npc-fraction workaround.

Device-space dodge (`mark_bar`) and jitter (`mark_point`) remain data-space for
now; converting them to the compound unit is deferred (it changes existing
rendered output and needs snapshot review).

# vellumplot 0.2.0

Grammar-breadth release: new scales, mapped aesthetics, legend control, and
distribution marks, closing the most conspicuous gaps versus ggplot2.

## Scales & axes

* **Date/time scales**: `scale_x_date()` / `scale_x_datetime()` /
  `scale_x_time()` (and `y` twins) with `date_breaks` (interval strings like
  `"3 months"`) and `date_labels` (strftime formats). `Date`/`POSIXct` columns
  still get a sensible date axis automatically.
* **New mapped aesthetics**: `scale_alpha()` (continuous opacity) and
  `scale_linetype()` (discrete line types), each drawn per element with its own
  legend. `alpha` mapped to data now varies opacity (previously constant-only);
  `linetype` applies to `mark_line()` / `mark_step()`.
* **Identity scales**: `scale_color_identity()`, `scale_fill_identity()`,
  `scale_size_identity()`, `scale_shape_identity()`, `scale_alpha_identity()`,
  `scale_linetype_identity()` — use data values verbatim, no legend.
* **Limit shortcuts**: `xlim()`, `ylim()`, and `lims()`.
* **Legend control**: `guides()` with `guide = "none"` (hide a legend),
  `guide_legend(reverse = TRUE)` (reverse key order), and `guide_legend(title=)`.

## Marks & stats

* **Distribution marks**: `mark_ecdf()` (empirical CDF step), `mark_rug()`
  (marginal ticks), `mark_qq()` + `mark_qq_line()` (quantile-quantile plot).
* **Density-shape marks**: `mark_violin()` (mirrored density per category),
  `mark_ridgeline()` (overlapping per-category densities), and `mark_dotplot()`
  (binned, stacked dots). Violin and ridgeline carry per-category provenance.

## Not yet implemented (still planned)

2-D density / contour
(`mark_contour`/`stat_density_2d`); position-binned scales (`scale_x_binned`);
`coord_trans()` and free non-position scales across facets; triple-merge legends
(colour+shape+size) and NA keys for size/shape.

# vellumplot 0.1.1

* New exported `plot_provenance()`: returns the compiled-scene provenance table
  — one record per emitted mark grob, tying each low-level primitive back to the
  data rows and trained scales that produced it, with an `id` that matches the
  grob's `data-vellum-id` in SVG and the `id` column of `vellum::scene_model()`.
  This is the first public consumer of the provenance metadata (previously
  internal), the substrate for interactivity, linked views, and accessibility.
* Row-key provenance is now refined per element for the line, area, ribbon,
  step, text, and boxplot marks (previously only point/bar/tile/segment/sf/
  edges), so a record's `rows` resolve to the actual data rows an element draws
  rather than the whole layer. See the scene-contract vignette in `vellum`.

# vellumplot 0.1.0

First release. vellumplot is a declarative, pipe-first grammar of graphics that
compiles an inspectable spec into a `vellum` scene, with faceting, coordinate
systems, and multi-plot composition. Everything below ships in this first
release.

## Features

* **Interactivity declarations** (host-agnostic; inert on a static render). Any
  `mark_*()` accepts reserved per-row args `data_id`, `tooltip`, and
  `hover_group` (tidy-eval expressions). They flow into the vellum scene as
  per-element keys/metadata — `data_id` becomes the SVG `data-key` and both
  surface in `vellum::scene_model()` — the foundation a companion widget uses for
  hover/select/linking. A plot without them compiles and renders exactly as
  before. Applies to `stat = "identity"` marks (points, bars, tiles, segments,
  edges, hexbins, polar bars) and to `mark_sf()` — each sf feature
  (polygon/linestring/point) becomes an addressable, keyed element.
* **Per-element interaction styling**: marks also accept `hover_color` and
  `selected_color` (constant or column-mapped), carried into the scene so a host
  (`vellumwidget`) outlines each element in its own colour on hover/select — overriding
  the widget-wide theme.

* **Interactive discrete legends**: when a plot maps a discrete `color`/`shape`
  scale and declares interactivity, each legend swatch is tagged with the data
  series it represents, and every mark carries its series membership. A host
  (`vellumwidget`) uses this to make swatches highlight/select their whole series. Inert
  on a static render and when no interactivity is declared.

* **Auto-display**: printing a plot (or composition) draws it into the active
  graphics device — the RStudio / Positron Plots pane, or a knitr/Quarto chunk —
  like ggplot2 (via `vellum::display()`). `summary()` shows the inspectable spec
  tree instead; `render_plot()` still writes a file.
* `vplot()` starts an inspectable, serializable `PlotSpec`.
* **Output resolution**: `vplot(dpi =)` sets the authored resolution and
  `render_plot(dpi =)` overrides it per render, so an exported PNG's pixel
  dimensions are `width * dpi` by `height * dpi`. Compositions inherit the first
  sub-plot's `dpi` (or take an explicit `concat(dpi =)`).
* Marks: `mark_point()`, `mark_line()`, `mark_rule()` (reference lines via
  `xintercept` / `yintercept`), and `mark_bar()` (uses explicit `y` heights, or
  counts rows per category when `y` is omitted).
* Encodings captured with tidy evaluation: `x`, `y`, `color`/`fill`, `size`,
  `shape`, `alpha`. Scalar values become constant aesthetics.
* Scales: `scale_x_continuous()` / `scale_y_continuous()` (linear and `log10`)
  with auto-trained domains and ggplot-style expansion; **discrete (band)
  position scales** are trained automatically for categorical `x`/`y` (bars);
  `scale_color_continuous()` (perceptual ramp), `scale_color_discrete()`
  (qualitative palette), and a trained **size scale** for a mapped `size`.
* Guides: trained x/y axes (breaks, labels, titles), a grey panel with white
  gridlines, and a legend area that **stacks multiple guides** — a colour legend
  (continuous gradient bar or discrete swatches) and a size legend.
* **Legend layout** is measured in millimetres: each guide is sized to its
  content (a title line above one row per key), the row pitch is driven by the
  key's drawn size so large bubble keys never overlap, and the guide block is
  centred in the legend track. Titles sit directly above their keys, horizontal
  (top/bottom) legends pack keys tightly instead of spreading them across the
  full width, and a vertical legend uses the full figure height.
* **Legend keys match the mark**: a colour legend draws the glyph the plot
  actually uses — a filled circle for point layers, a short line for line-like
  layers (line/step/smooth/segment/…), and a filled square for bar/area/tile/
  polygon layers — instead of always a square swatch.
* **Legend geometry is themeable**: `theme(legend.key.size=, legend.spacing=,
  legend.margin=)` set the key/swatch side, the gap between stacked guides, and
  the inset around the legend block (all in millimetres).
* Continuous colour bars now carry **white break ticks** aligned to their
  labels, on both vertical and horizontal (top/bottom) legends.
* **Merged legends**: mapping one variable to two aesthetics draws a single
  legend whose keys carry both encodings — discrete `colour` + `shape` become
  coloured shape keys; continuous `colour` + `size` become colour-graded,
  size-graduated points. Merging follows ggplot2's rule (same title and
  breaks/levels); give one scale a different `name=` to keep them separate.
* Layering: multiple marks on one panel, with scales trained across all layers.
* **Faceting**: `facet_wrap(~var)` and `facet_grid(rows ~ cols)` split the data
  into a panel grid with facet strips and aligned, shared axes.
* **Scale resolution** (`resolve_scale()` / `facet_*(scales=)`): position scales
  are shared across panels by default; opt into `"free_x"` / `"free_y"` /
  `"free"` (independent per panel) for per-panel ranges and axes.
* **Statistical transforms**: `mark_histogram()` (bin a continuous variable into
  count bars) and `mark_smooth()` (an `"lm"` fit drawn as a line with an optional
  confidence ribbon). `mark_bar()` with no `y` uses the count stat. Map computed
  variables with `after_stat()`, e.g. `y = after_stat(density)`.
* **Position adjustments** (`position =`): grouped bars **stack** by default;
  `"dodge"` places them side by side and `"fill"` normalises each group to 1.
  `mark_point(position = "jitter")` spreads overlapping points.
* **Datashading**: `mark_datashade()` aggregates a large point cloud into a
  density raster (via `vellum::datashade()`) that fills the panel — cost
  independent of point count. `mark_point(auto = TRUE)` switches to this
  automatically above ~50k rows.
* **More marks**: areas/ribbons (`mark_area()`, `mark_ribbon()`), steps
  (`mark_step()`), intervals (`mark_errorbar()`, `mark_linerange()`,
  `mark_segment()`), `mark_boxplot()`, tiles/heatmaps (`mark_tile()`,
  `mark_raster()`), 2-D binning (`mark_bin2d()`, `mark_hex()`), `mark_text()` /
  `mark_label()`, and pie/donut shortcuts (`mark_pie()`, `mark_donut()`).
* **More statistical transforms**: `mark_density()` (kernel density),
  `mark_summary()` (aggregate `y` per category), in addition to the histogram /
  binning / smooth stats. Map computed variables with `after_stat()`.
* **Coordinate systems**: `coord_cartesian()` (view-window zoom),
  `coord_flip()`, `coord_fixed()` / `coord_equal()` (aspect lock), and
  `coord_polar()` (pie / coxcomb / radar).
* **Themes**: `theme_gray()` (default), `theme_minimal()`, `theme_bw()`,
  `theme_classic()`, `theme_void()`, and `theme()` for ad-hoc overrides (panel
  background, gridlines, text, strip background, legend position, margins).
* **Effects & paints**: stroked and point marks take a layer
  `effects = list(...)` argument — `glow()` (neon halo), `outline()` (contrasting
  halo for legibility), and `shadow()` (drop / ambient). `linear_gradient()` /
  `radial_gradient()` can be used as a `fill` value (area / ribbon / bar), and
  `theme_cyberpunk()` ties glow + gradients + a neon palette into the
  [mplcyberpunk](https://github.com/dhaitz/mplcyberpunk) look.
* **Hand-drawn ("sketch") rendering**: `sketch()` (re-exported from vellum) gives
  any geometry mark a wobbly, hachure-filled [Rough.js](https://roughjs.com) look
  via a `sketch =` argument. Unlike a layer effect it is a *geometry* property,
  generated natively in the engine, so it is exact, cross-backend, and works in
  PDF. It rides three levels, most-specific-wins: a mark's `sketch =`, an
  `element_line()` / `element_rect()` `sketch =` slot, or the plot-wide
  `theme_sketch()` one-liner (hand-drawn gridlines, axes, ticks, marks and legend
  keys on a paper background). `sketch = NA` forces an element crisp; text is
  never sketched (pair a handwriting font).
* **Composition**: `hconcat()`, `vconcat()`, `concat()`, and `wrap_plots()`
  arrange several plots on a grid; the aligned path lines up panel edges and can
  **collect guides** across sub-plots. `inset()` overlays a plot, and
  `compose_annotation()` adds figure-level titles and auto-tags (`A`, `B`, ...).
* **Repeat**: `repeat_()` replicates a plot across a set of fields, zipping one
  or more encodings to produce a composition.
* **Blend modes**: marks take a `blend =` argument (the CSS `mix-blend-mode` set
  — `"multiply"`, `"screen"`, `"darken"`, ...). The layer composites as one
  isolated group against the panel and earlier layers, e.g. two overlapping
  translucent layers under `"multiply"`.
* **Spatial**: `mark_sf()` draws the geometry column of an `sf` object as a map
  layer (polygons / lines / points), with `coord_sf()` to reproject to a target
  CRS and lock the map aspect ratio; `scale_fill_binned()` / `scale_color_binned()`
  bin a continuous fill/colour into discrete classes for choropleths.
* **Network**: `vgraph()` starts a node-link diagram from an `igraph` graph —
  it runs a layout (stress majorization by default, via `graphlayouts`;
  `"sparse_stress"`, `"backbone"`, `"fr"`, `"circle"`, ... or a matrix/function),
  builds a node and an edge table, and locks the aspect with a void theme.
  `mark_edges()` draws edges (straight, batched; reciprocal/parallel edges offset
  off the centre line, self-loops nested, optional `arrow`), `mark_nodes()` the
  vertices, and `mark_node_text()` the labels — with a fixed edges-under-nodes
  draw order. Edges are capped exactly at each endpoint's node boundary (per
  vertex, at any resolution), so arrowheads land on the node edge. `scale_edge_width()`
  maps a weight to edge width with its own legend.
  `igraph` / `graphlayouts` are optional (`Suggests`).
* Output: `render_plot(plot, path)`; `vellum::render(plot, path)` and
  `print(plot)` also work. The compiler is registered on vellum's
  `as_vellum_scene()` seam.

## Under the hood

* **Compiled-scene provenance** (foundation for interactivity / accessibility /
  linked views): every emitted mark grob now carries a globally-unique, stable
  `id` (surfacing as `data-vellum-id` in SVG), and the compiler builds a
  serializable row-key / scale-ref table — one record per grob tying it back to
  the data rows and trained scales that produced it — carried on the compiled
  scene as `attr(scene, "vellumplot_provenance")`. Populated on every compile;
  additive metadata only (raster/PDF output is byte-for-byte unchanged).
* **Continuous integration**: GitHub Actions run `R CMD check` (R
  release/devel/oldrel on Linux + macOS, with the Rust toolchain the `vellum`
  backend needs), including a nightly run against `vellum`'s `main` to catch
  cross-layer breaks early, plus a pkgdown build.

## Not yet implemented (planned)

Reactivity, 2-D contour stats, and the algebraic `*` / `+` layer combinators.
Independent *non-position* (colour/size) scales across facets are not yet
supported (those legends stay shared). On the network side, community hulls and
alternative idioms (arc/matrix/hive) are deferred.
