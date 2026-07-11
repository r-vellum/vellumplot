#' @include classes.R
NULL

# ---------------------------------------------------------------------------
# Emitted-scene provenance: the row-key / scale-ref metadata schema (DESIGN §4).
#
# This is the concrete realisation of the "keep the emitted scene carrying
# stable node IDs and data bindings" requirement -- the correctness-of-
# architecture lesson from Gadfly (#680): once the grammar has been compiled to
# the low-level layer, *which row / which scale produced which primitive* must
# still be recoverable, or any future interaction / accessibility / linked-view
# feature would have to recompute the entire grammar per frame.
#
# The schema is deliberately implemented *before* it has a consumer. Nothing in
# vellumplot reads it yet; it exists so the metadata never has to be retrofitted onto
# the emitter after the fact. The eventual consumers are SVG interactivity, a11y
# annotations, and linked/brushed views -- see the plug points in DESIGN §4.
#
# WHERE IT LIVES
#   * Every emitted grob is stamped with a globally-unique, stable id (`grob@id`,
#     which vellum surfaces as `data-vellum-id` in SVG). This is the JOIN KEY.
#   * A parallel provenance table -- one plain, serializable record per emitted
#     grob, keyed by that id -- is accumulated during compile and attached to the
#     returned scene as attr(scene, "vellumplot_provenance"). vellum grobs expose only
#     `id` + `role` (no arbitrary metadata slot, and invariant §8 forbids touching
#     vellum internals), so the rich record is carried vellumplot-side.
#
# SCHEMA (one entry per emitted grob; all fields plain / serializable):
#   id       chr   stable node id == grob@id == SVG data-vellum-id (join key)
#   layer    int   1-based layer index within the (sub-)plot spec
#   mark     chr   mark type ("point", "line", ...)
#   kind     chr   "mark" (the core layer) or "effect" (a decorative underlay copy)
#   panel    chr   facet panel key ("panel-r-c"), or NA for a single panel
#   channels list  aesthetic -> list(scale=, type=, domain=): the SCALE-REF half
#   rows     int   original input-data row indices this grob draws: the ROW-KEY half
#
# THE ROW-KEY CONTRACT (this is the "plug it wherever needed" seam)
#   `.mark_ctx$rows` defaults to *all* rows of the current layer, so every entry
#   is correct-but-coarse out of the box and never wrong. An emitter that groups
#   rows by resolved style (the `.style_groups()` pattern) should refine it to the
#   actual rows of each emitted group by passing `rows=` to `.draw()`. Grep for
#   `PROVENANCE:` to see where this is done.
#
#   REFINED (rows resolve to the actual data rows an element draws): point, bar,
#   bar-polar, tile, segment, sf, edges (main segments), line, area, ribbon,
#   step, text, boxplot.
#   STILL WHOLE-LAYER (backlog; correct-but-coarse): rule, smooth, errorbar/
#   linerange, raster, hex, datashade, and the self-loop draw in `.emit_edges`.
#   These are aggregates or references where a per-row key is ill-defined or of
#   low value; refine them when a consumer needs finer granularity.
# ---------------------------------------------------------------------------

# Which trained scale resolves each aesthetic channel (the scale-ref lookup).
# color/fill share the colour scale (see `.aes_aliases`).
.PROV_CHANNEL_SCALE <- c(
  x = "x",
  y = "y",
  color = "color",
  fill = "color",
  size = "size",
  shape = "shape"
)

# Normalise a trained scale (whose field names differ by kind: position scales
# carry `type`/`domain`, colour/size carry `kind`/`levels`/`range`) to a compact,
# serializable reference. Best-effort and defensive -- a missing field is NA/NULL,
# never an error.
.scale_ref <- function(key, sc) {
  list(
    scale = key,
    type = sc$type %||% sc$kind %||% NA_character_,
    domain = sc$domain %||% sc$levels %||% NULL
  )
}

# The scale-ref half for one resolved layer: each aesthetic the layer actually
# maps (present in `L$values`) that routes through a trained scale.
.layer_channels <- function(L, scales) {
  out <- list()
  for (ch in names(.PROV_CHANNEL_SCALE)) {
    if (is.null(L$values[[ch]])) {
      next
    }
    key <- .PROV_CHANNEL_SCALE[[ch]]
    sc <- scales[[key]]
    if (is.null(sc)) {
      next
    }
    out[[ch]] <- .scale_ref(key, sc)
  }
  out
}

# Reset the provenance accumulator + the global grob counter at the top of a
# full compile (a single plot, or a whole composition). Called from the compile
# entry points so entries accumulate across every panel / sub-plot.
.provenance_reset <- function() {
  .mark_ctx$provenance <- list()
  .mark_ctx$seq <- 0L
  .mark_ctx$panel <- NA_character_
  .mark_ctx$kind <- "mark"
  invisible(NULL)
}

# Snapshot the accumulated table (the value attached to the compiled scene).
.provenance_snapshot <- function() {
  .mark_ctx$provenance %||% list()
}

# Record one emitted grob. Returns the stable id to stamp on the grob. Called
# only from `.draw()`, the single choke point every mark grob passes through, so
# every emitter gets scale-ref + stable-id provenance for free; only the row-key
# refinement is per-emitter (see the row-key contract above).
.provenance_record <- function(rows = NULL) {
  base <- .mark_ctx$id
  if (is.null(base)) {
    return(NULL) # not emitting a mark layer (e.g. a guide/strip grob)
  }
  seq <- (.mark_ctx$seq %||% 0L) + 1L
  .mark_ctx$seq <- seq
  id <- sprintf("%s-g%d", base, seq)
  entry <- list(
    id = id,
    layer = .mark_ctx$layer,
    mark = .mark_ctx$mark,
    kind = .mark_ctx$kind %||% "mark",
    panel = .mark_ctx$panel %||% NA_character_,
    channels = .mark_ctx$channels %||% list(),
    rows = rows %||% .mark_ctx$rows
  )
  .mark_ctx$provenance <- c(.mark_ctx$provenance, list(entry))
  id
}

#' Inspect the compiled-scene provenance of a plot
#'
#' Compiles `x` and returns its *provenance table*: one record per emitted mark
#' grob, tying each low-level primitive back to the data rows and trained scales
#' that produced it. Each record's `id` matches the grob's `data-vellum-id` in
#' the SVG output (and the `id` column of [vellum::scene_model()]), so it is a
#' stable join key between the rendered scene and the grammar — the substrate for
#' interactivity, linked views, and accessibility.
#'
#' @details
#' Each record is a plain, serializable list:
#' \describe{
#'   \item{`id`}{stable node id, equal to `grob@id` / SVG `data-vellum-id` (join key).}
#'   \item{`layer`}{1-based layer index within the (sub-)plot spec.}
#'   \item{`mark`}{mark type (`"point"`, `"line"`, …).}
#'   \item{`kind`}{`"mark"` (the core layer) or `"effect"` (a decorative underlay).}
#'   \item{`panel`}{facet panel key (`"panel-r-c"`), or `NA` for a single panel.}
#'   \item{`channels`}{aesthetic → trained-scale reference (`scale`, `type`, `domain`).}
#'   \item{`rows`}{the original input-data row indices this grob draws.}
#' }
#'
#' The `rows` field is refined per element for marks that map rows one-to-one to
#' drawn elements (points, bars, tiles, segments, lines, areas, ribbons, steps,
#' text, boxplots, sf features, edges); for aggregating marks (histogram,
#' density, smooth, datashade, …) it is the whole layer, since rows no longer map
#' to single elements.
#'
#' @param x A [PlotSpec] or plot composition.
#' @return A list of provenance records (see Details). Empty for a plot that
#'   emits no mark grobs.
#' @seealso [vellum::scene_model()] and the scene-contract vignette in vellum.
#' @examples
#' df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, model = rownames(mtcars))
#' p <- vplot(df) |> mark_point(x = wt, y = mpg, data_id = model)
#' prov <- plot_provenance(p)
#' prov[[1]]$id
#' @export
plot_provenance <- function(x) {
  attr(vellum::as_vellum_scene(x), "vellumplot_provenance") %||% list()
}
