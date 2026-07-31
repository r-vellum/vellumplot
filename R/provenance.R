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
#   step, text, boxplot, errorbar/linerange.
#   STILL WHOLE-LAYER (backlog; correct-but-coarse): rule, smooth, raster, hex,
#   datashade, and the self-loop draw in `.emit_edges`.
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
    domain = sc$domain %||% sc$levels
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

#' Join a plot's provenance to its rendered geometry
#'
#' `provenance_join()` compiles `x` once and returns a tidy table with one row
#' per emitted mark element, tying each drawn primitive to **both** the source
#' data rows that produced it (`rows`, a list column) **and** its device-pixel
#' geometry (`x0`, `y0`, `x1`, `y1`, `x`, `y`, `w`, `h`) from
#' [vellum::scene_model()]. It is the consumer of the provenance table that
#' [plot_provenance()] emits — the substrate for click-to-source interactivity,
#' auditing "which rows made this element?", and linked views.
#'
#' @param x A [PlotSpec] or composition.
#' @return A data frame: `id`, `layer`, `mark`, `kind`, `panel`, the pixel bbox
#'   (`x0`, `y0`, `x1`, `y1`, `x`, `y`, `w`, `h`), `n_rows`, and `rows` (a list
#'   column of integer source-row indices). Empty for a plot with no mark grobs.
#' @seealso [plot_provenance()], [vellum::scene_model()]
#' @examples
#' df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, cyl = factor(mtcars$cyl))
#' p <- vplot(df) |> mark_point(x = wt, y = mpg, color = cyl)
#' pj <- provenance_join(p)
#' pj[, c("id", "mark", "n_rows")]
#' @export
provenance_join <- function(x) {
  if (S7::S7_inherits(x, PlotSpec) && !length(x@layers)) {
    return(.empty_provenance_join())
  }
  scene <- vellum::as_vellum_scene(x)
  prov <- attr(scene, "vellumplot_provenance") %||% list()
  if (!length(prov)) {
    return(.empty_provenance_join())
  }
  model <- vellum::scene_model(scene)
  el <- model$elements
  el <- el[!is.na(el$id), , drop = FALSE]
  # A grob (one provenance record) can expand to many elements in scene_model
  # (e.g. a points grob -> one box per point) that share its id. Reduce to the
  # group's aggregate bounding box so the join is one row per provenance record.
  geom_for <- function(id) {
    g <- el[el$id == id, , drop = FALSE]
    if (!nrow(g)) {
      return(data.frame(
        x0 = NA,
        y0 = NA,
        x1 = NA,
        y1 = NA,
        x = NA,
        y = NA,
        w = NA,
        h = NA
      ))
    }
    x0 <- min(g$x0)
    y0 <- min(g$y0)
    x1 <- max(g$x1)
    y1 <- max(g$y1)
    data.frame(
      x0 = x0,
      y0 = y0,
      x1 = x1,
      y1 = y1,
      x = mean(g$x),
      y = mean(g$y),
      w = x1 - x0,
      h = y1 - y0
    )
  }
  rows <- lapply(prov, function(p) {
    cbind(
      data.frame(
        id = p$id,
        layer = p$layer,
        mark = p$mark,
        kind = p$kind %||% "mark",
        panel = p$panel %||% NA_character_,
        n_rows = length(p$rows %||% integer(0)),
        stringsAsFactors = FALSE
      ),
      geom_for(p$id)
    )
  })
  out <- do.call(rbind, rows)
  # rows as a list column, aligned to the provenance order.
  out$rows <- lapply(prov, function(p) p$rows %||% integer(0))
  out[order(out$layer, out$id), , drop = FALSE]
}

.empty_provenance_join <- function() {
  df <- data.frame(
    id = character(0),
    layer = integer(0),
    mark = character(0),
    kind = character(0),
    panel = character(0),
    n_rows = integer(0),
    x0 = numeric(0),
    y0 = numeric(0),
    x1 = numeric(0),
    y1 = numeric(0),
    x = numeric(0),
    y = numeric(0),
    w = numeric(0),
    h = numeric(0),
    stringsAsFactors = FALSE
  )
  df$rows <- list()
  df
}

#' A reproducibility manifest for a plot
#'
#' `plot_manifest()` returns a small, serializable fingerprint of a plot: a hash
#' of its input data (order- and column-sensitive), the data's shape and column
#' names, a structural hash of the spec, and the number of emitted elements. It
#' is what [plot_svg()] can embed (with `manifest = TRUE`) so a figure carries
#' its own provenance, and what [plot_verify()] recomputes to confirm a figure
#' still matches its data.
#'
#' @param x A [PlotSpec].
#' @return A named list: `version`, `data` (a `hash`/`nrow`/`columns` record),
#'   `spec_hash`, `n_elements`, and `fonts` (the faces the plot resolved to, via
#'   [vellum::font_pin()], each a `path`/`index`/`glyphs` record). The fonts are
#'   what lets [plot_verify()] tell a font-stack difference apart from a data
#'   change — the same pixels are only guaranteed when the same fonts are
#'   present.
#' @seealso [plot_verify()], [plot_svg()], [provenance_join()]
#' @examples
#' plot_manifest(vplot(mtcars) |> mark_point(x = wt, y = mpg))$data$hash
#' @export
plot_manifest <- function(x) {
  data_ir <- .data_to_ir(x@data)
  data_rec <- if (is.null(data_ir)) {
    NULL
  } else {
    list(hash = data_ir$hash, nrow = data_ir$nrow, columns = data_ir$columns)
  }
  # Fonts the scene resolved to (path/index/glyphs). vellum's determinism holds
  # only when the same faces are present, so pinning them turns "same pixels"
  # from an assumption into something plot_verify() can check.
  fonts <- tryCatch(
    {
      pin <- vellum::font_pin(vellum::as_vellum_scene(x))
      f <- pin$fonts
      if (is.null(f) || !nrow(f)) NULL else f
    },
    error = function(e) NULL
  )
  # A structural spec hash that does not depend on full serialisability: try the
  # real spec, else fall back to the layer/encoding skeleton.
  spec_hash <- tryCatch(
    {
      s <- as_spec(x)
      rlang::hash(s[setdiff(names(s), "data")])
    },
    error = function(e) {
      skel <- lapply(x@layers, function(L) {
        list(mark = L@mark, aes = names(L@encoding), stat = L@stat)
      })
      rlang::hash(skel)
    }
  )
  n_elements <- length(
    attr(vellum::as_vellum_scene(x), "vellumplot_provenance") %||% list()
  )
  list(
    version = .SPEC_VERSION,
    data = data_rec,
    spec_hash = spec_hash,
    n_elements = n_elements,
    fonts = fonts
  )
}

# Font paths recorded in a (round-tripped) manifest, whichever JSON shape they
# came back in: a list of `{path,...}` records (fromJSON simplifyDataFrame=FALSE)
# or a columnar `list(path = c(...))`. Empty character vector when none.
.manifest_font_paths <- function(fonts) {
  if (is.null(fonts) || !length(fonts)) {
    return(character(0))
  }
  if (!is.null(fonts$path)) {
    return(as.character(fonts$path))
  }
  as.character(unlist(lapply(fonts, function(r) r$path %||% NA_character_)))
}

#' Verify a rendered figure against its data
#'
#' `plot_verify()` extracts the manifest embedded in an SVG written with
#' [plot_svg()]`(manifest = TRUE)` and recomputes the data fingerprint from
#' `data`, reporting whether the figure still matches the data it was drawn from
#' — a lightweight, self-contained reproducibility check.
#'
#' A font mismatch is reported as a **distinct outcome** from a data mismatch:
#' `data_ok` is whether the data still hashes the same, `fonts_ok` whether every
#' font the figure was drawn with is still present on this machine, and `ok`
#' their conjunction. A pixel difference with `data_ok = TRUE` but
#' `fonts_ok = FALSE` is the font stack, not your data — a different cause with a
#' different fix (install/register the font) than a data change.
#'
#' @param svg An SVG string (from [plot_svg()]) or a path to an `.svg` file.
#' @param data The data frame to check the figure against.
#' @return A list with `ok` (data *and* fonts match), `data_ok`, `expected` /
#'   `actual` (the embedded vs recomputed data hash), `fonts_ok`, and
#'   `fonts_missing` (the recorded font paths absent on this machine). Errors if
#'   the SVG carries no manifest.
#' @seealso [plot_manifest()], [plot_svg()]
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' svg <- plot_svg(vplot(mtcars) |> mark_point(x = wt, y = mpg), manifest = TRUE)
#' plot_verify(svg, mtcars)$ok
#' @export
plot_verify <- function(svg, data) {
  .need_pkg("jsonlite", "plot_verify()")
  if (length(svg) == 1 && !grepl("<svg", svg) && file.exists(svg)) {
    svg <- paste(readLines(svg, warn = FALSE), collapse = "\n")
  }
  # Non-greedy match up to the first comment terminator, so a payload can't be
  # over-matched across other comments.
  m <- regmatches(
    svg,
    regexpr("vellumplot-manifest:\\s*(.+?)\\s*-->", svg, perl = TRUE)
  )
  if (!length(m)) {
    cli::cli_abort(c(
      "No vellumplot manifest found in the SVG.",
      "i" = "Write it with {.code plot_svg(x, manifest = TRUE)}."
    ))
  }
  payload <- trimws(sub(
    ".*vellumplot-manifest:\\s*(.+?)\\s*-->.*",
    "\\1",
    m,
    perl = TRUE
  ))
  # Current form is base64; the legacy form embedded raw JSON (starts with `{`).
  json <- if (startsWith(payload, "{")) {
    payload
  } else {
    rawToChar(jsonlite::base64_dec(payload))
  }
  manifest <- jsonlite::fromJSON(
    json,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE
  )
  expected <- manifest$data$hash
  actual <- .data_hash(data)
  data_ok <- identical(expected, actual)
  # Fonts: a distinct cause. A recorded face absent here means the figure cannot
  # reproduce pixel-for-pixel on this machine even if the data is identical.
  font_paths <- .manifest_font_paths(manifest$fonts)
  fonts_missing <- font_paths[!file.exists(font_paths)]
  fonts_ok <- length(fonts_missing) == 0L
  list(
    ok = data_ok && fonts_ok,
    data_ok = data_ok,
    expected = expected,
    actual = actual,
    fonts_ok = fonts_ok,
    fonts_missing = fonts_missing
  )
}

#' A widget-ready provenance payload (click-to-source)
#'
#' `provenance_payload()` returns a JSON-serializable structure mapping each
#' rendered element's stable id (`data-vellum-id`, the join key an SVG / widget
#' carries) to the source data rows that produced it — the enabler for a host
#' (e.g. vellumwidget) to answer "which rows made this element?" on click,
#' without re-running the grammar. With `values = TRUE` the referenced data rows
#' are inlined so the host can display them directly.
#'
#' @param x A [PlotSpec] or composition.
#' @param values If `TRUE`, inline the source data rows (row-wise records) so the
#'   host needs no separate data access. Default `FALSE` (ids + row indices only).
#' @return A list with `fields` (column names), `elements` (a list of
#'   `list(id, rows)` records), and — when `values = TRUE` — `data` (row-wise
#'   records of the plot's data).
#' @seealso [provenance_join()], [plot_provenance()]
#' @examples
#' p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))
#' pl <- provenance_payload(p)
#' pl$elements[[1]]$id
#' @export
provenance_payload <- function(x, values = FALSE) {
  scene <- vellum::as_vellum_scene(x)
  prov <- attr(scene, "vellumplot_provenance") %||% list()
  data <- if (S7::S7_inherits(x, PlotSpec)) x@data else NULL
  elements <- lapply(prov, function(p) {
    list(id = p$id, rows = as.integer(p$rows %||% integer(0)))
  })
  out <- list(
    fields = as.list(names(data) %||% character(0)),
    elements = elements
  )
  if (isTRUE(values) && !is.null(data)) {
    out$data <- lapply(seq_len(nrow(data)), function(i) {
      as.list(data[i, , drop = FALSE])
    })
  }
  out
}

# The manifest as a single-line HTML/XML comment for embedding in an SVG. The
# JSON is base64-encoded so a column name containing `--` (or `>`) can never
# corrupt the enclosing comment; `plot_verify()` decodes it (and still reads the
# legacy raw-JSON form).
.manifest_comment <- function(x) {
  .need_pkg("jsonlite", "plot_svg(manifest = TRUE)")
  json <- jsonlite::toJSON(plot_manifest(x), auto_unbox = TRUE, null = "null")
  enc <- gsub("\\s", "", jsonlite::base64_enc(json))
  paste0("<!-- vellumplot-manifest: ", enc, " -->")
}
