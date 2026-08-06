#' @include classes.R
NULL

# Mark emitters: text and label marks.

# Text colour for text/label marks: `.aes_colour` without the fill fallback,
# since `fill` is the label background here, not the ink colour.
.text_colour <- function(L, scales, default) {
  .aes_colour(L, scales, default, fill_fallback = FALSE)
}

# Per-element text angle: a mapped channel or a constant param (degrees).
.text_angle <- function(L, n) {
  if (!is.null(L$values$angle)) {
    rep_len(L$values$angle, n)
  } else {
    rep_len(.aes_param(L, "angle", 0), n)
  }
}

.emit_text <- function(scene, L, scales) {
  n <- L$n
  # The `label` is a mapped column (`L$values$label`) or a constant (a literal
  # passes through to `L$params$label`, bypassing the encoding split) -- read
  # either. A text mark with neither has nothing to write (it would otherwise draw
  # invisible `NA` glyphs at every point), so fail with a clear message.
  raw <- L$values$label %||% .aes_param(L, "label", NULL)
  if (is.null(raw)) {
    fn <- switch(
      L$mark,
      sf_label = "mark_sf_label",
      node_text = "mark_node_text",
      edge_text = "mark_edge_text",
      "mark_text"
    )
    cli::cli_abort("{.fn {fn}} needs a {.field label} encoding.")
  }
  # Labels are drawn at their data anchors; a repel layer is nudged afterwards by
  # the engine solver, which addresses each label grob by the `name` set below.
  repel_on <- isTRUE(L$stat_params$repel$on)
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  # `raw` (resolved above) may be plain character (multi-line "\n" supported by
  # vellum), a single rich md() label (drawn at every position), or a per-datum
  # list of md() labels. Only plain labels are flattened with as.character(); rich
  # labels pass through.
  rich_single <- inherits(raw, "vellum::vellum_label")
  rich_list <- !rich_single &&
    is.list(raw) &&
    length(raw) > 0L &&
    all(vapply(
      raw,
      function(x) inherits(x, "vellum::vellum_label"),
      logical(1)
    ))
  if (rich_list) {
    labs <- raw[rep_len(seq_along(raw), n)]
  } else if (!rich_single) {
    label <- rep_len(as.character(raw), n)
  }
  .label_of <- function(idx) {
    if (rich_single) {
      raw
    } else if (rich_list) {
      labs[idx]
    } else {
      label[idx]
    }
  }
  col <- rep_len(.text_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  ang <- .text_angle(L, n)
  # A mapped `size` trains a size scale (as for points); honour it, falling back
  # to a constant `size` param and then the 8pt default. One gpar carries one
  # fontsize, so a per-datum size splits into style groups below.
  fs <- rep_len(.aes_size(L, scales, .aes_param(L, "size", 8)), n)
  # `hjust`/`vjust` may arrive as a constant param or (when passed a variable) a
  # resolved value; check values first, like `.text_angle`. A single just applies
  # to the whole batch, so take the first if a vector came through.
  hj <- L$values$hjust %||% .aes_param(L, "hjust", "centre")
  vj <- L$values$vjust %||% .aes_param(L, "vjust", "centre")
  just <- c(hj[[1]], vj[[1]])

  gi <- 0L
  for (idx in .style_groups(n, list(col = col, alpha = alpha, size = fs))) {
    a <- alpha[idx[1]]
    xy <- .nudge_xy(.xy_units(scales, xn[idx], yn[idx]), L, idx)
    scene <- .draw(
      scene,
      vellum::text_grob(
        .label_of(idx),
        xy$x,
        xy$y,
        just = just,
        rot = ang[idx],
        name = if (repel_on) .repel_name(gi),
        gp = vellum::vl_gpar(
          fontsize = fs[idx[1]],
          col = col[idx[1]],
          fontfamily = .aes_param(L, "family", NULL),
          fontface = .aes_param(L, "fontface", NULL),
          alpha = gp_alpha(a)
        )
      ),
      # PROVENANCE: `idx` are the layer rows in this style group.
      rows = idx
    )
    gi <- gi + 1L
  }
  scene
}

# A text mark with a filled rounded background sized to each label.
.emit_label <- function(scene, L, scales) {
  n <- L$n
  # Anchors; a repel layer is nudged afterwards by the engine solver, which
  # addresses the text ("repel:...") and its background ("repelbg:...") by name.
  repel_on <- isTRUE(L$stat_params$repel$on)
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  label <- rep_len(as.character(L$values$label), n)
  col <- rep_len(.text_colour(L, scales, "black"), n)
  # Label background: a mapped `fill` channel (through the colour scale), else a
  # constant `fill` param, else white. `.text_colour` deliberately keeps `fill`
  # out of the ink colour, so the background is resolved here on its own.
  bg <- if (!is.null(scales$color) && !is.null(L$values$fill)) {
    scales$color$map(L$values$fill)
  } else {
    L$params$fill %||% "white"
  }
  bg <- rep_len(bg, n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  sk <- .mark_sketch(L, scales)
  # A mapped `size` trains a size scale (as for points); honour it per datum so
  # each box is measured at its own font size. Falls back to a constant `size`
  # param, then the 8pt default.
  fs <- rep_len(.aes_size(L, scales, .aes_param(L, "size", 8)), n)
  pad <- vellum::vl_unit(1.2, "mm")
  # Build each text grob once, then take its width and height (two passes built
  # the grob twice).
  txts <- lapply(seq_len(n), function(i) .txt(label[i], fs[i]))
  ws <- do.call(c, lapply(txts, function(t) vellum::grobwidth(t) + pad))
  hs <- do.call(c, lapply(txts, function(t) vellum::grobheight(t) + pad))

  gi <- 0L
  for (idx in .style_groups(
    n,
    list(col = col, fill = bg, alpha = alpha, size = fs)
  )) {
    a <- alpha[idx[1]]
    xy <- .nudge_xy(.xy_units(scales, xn[idx], yn[idx]), L, idx)
    # Key the background box, not the text on top: the rounded rect is the whole
    # label's hit target, and (being a keyed batch mark) it contributes exactly
    # one `scene_model()` row per label when keyed and none when not -- so a label
    # is one addressable element, never two. The text stays unkeyed backdrop-side.
    # For a repel layer the background is named "repelbg:..." so it moves in
    # lockstep with its text ("repel:...", which the solver actually places).
    scene <- .draw(
      scene,
      vellum::roundrect_grob(
        x = xy$x,
        y = xy$y,
        width = ws[idx],
        height = hs[idx],
        r = vellum::vl_unit(0.8, "mm"),
        sketch = sk,
        name = if (repel_on) .repel_name(gi, bg = TRUE),
        gp = vellum::vl_gpar(
          fill = bg[idx[1]],
          col = NA,
          alpha = gp_alpha(a)
        )
      ),
      # PROVENANCE: `idx` are the layer rows in this style group.
      rows = idx
    )
    scene <- .draw(
      scene,
      vellum::text_grob(
        label[idx],
        xy$x,
        xy$y,
        name = if (repel_on) .repel_name(gi),
        gp = vellum::vl_gpar(
          fontsize = fs[idx[1]],
          col = col[idx[1]],
          alpha = gp_alpha(a)
        )
      )
    )
    gi <- gi + 1L
  }
  scene
}

# A label set ALONG a path: one string per group, its glyphs following the
# group's points (in data order) rotated to the local tangent -- for a contour, a
# curved axis, or a directly-labelled trend line. Distinct from `.emit_text` (one
# label at each point): here the whole group is one run riding one polyline.
# Groups split on style AND label, so two series with different colours or labels
# each get their own run; the label is constant within a run (a per-run string,
# taken from the group's first row). Glyphs follow the tangent, so a label on the
# underside of a closed curve reads upside-down -- reverse the path to flip it.
.emit_text_path <- function(scene, L, scales) {
  n <- L$n
  xn <- rep_len(scales$x$map(L$values$x), n)
  yn <- rep_len(scales$y$map(L$values$y), n)
  col <- rep_len(.text_colour(L, scales, "black"), n)
  alpha <- rep_len(.aes_alpha(L, scales, NA_real_), n)
  label <- rep_len(as.character(L$values$label), n)
  # Honour a mapped `size` (trained size scale) like the other text marks; a run
  # carries one font size, so size joins the grouping key below.
  fs <- rep_len(.aes_size(L, scales, .aes_param(L, "size", 8)), n)
  # `hjust` slides the run along the baseline (left = start of the path, centre,
  # right = end); `vjust` sets the glyphs' standoff from the baseline.
  hj <- L$values$hjust %||% .aes_param(L, "hjust", "centre")
  vj <- L$values$vjust %||% .aes_param(L, "vjust", "centre")
  just <- c(hj[[1]], vj[[1]])
  # Perpendicular standoff in points (+ = left of travel), for a label riding
  # just above or below its curve rather than sitting on it.
  offset <- .aes_param(L, "offset", 0)
  fam <- .aes_param(L, "family", NULL)
  face <- .aes_param(L, "fontface", NULL)
  for (idx in .style_groups(
    n,
    list(col = col, alpha = alpha, lab = label, size = fs)
  )) {
    lab <- label[idx[1]]
    if (is.na(lab) || !nzchar(lab) || length(idx) < 2L) {
      next # a path needs a label and at least two points
    }
    xy <- .xy_path(scales, xn[idx], yn[idx])
    scene <- .draw(
      scene,
      vellum::text_path_grob(
        lab,
        xy$x,
        xy$y,
        just = just,
        offset = offset,
        gp = vellum::vl_gpar(
          fontsize = fs[idx[1]],
          col = col[idx[1]],
          fontfamily = fam,
          fontface = face,
          alpha = gp_alpha(alpha[idx[1]])
        )
      )
    )
  }
  scene
}
