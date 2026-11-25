#' @include classes.R compile-guides.R
NULL

#' Binned legend
#'
#' `guide_bins()` draws the legend of a **binned** scale the way ggplot2's
#' `guide_bins()` does: one key per bin, stacked edge to edge, with a small
#' axis beside them whose ticks and labels mark the bin **boundaries** rather
#' than naming each bin as an interval. Use it for [scale_size_binned()],
#' [scale_alpha_binned()] and the binned colour scales ([scale_color_binned()],
#' `scale_*_steps()`, `scale_*_fermenter()`, ...). It follows the legend's
#' direction: keys run top to bottom (smallest first) in a left / right legend
#' and left to right in a top / bottom one.
#'
#' A bins legend is drawn on its own: it does not merge with another legend of
#' the same variable, and it shows no key for missing values. On a scale that
#' is not binned it is ignored, with a warning, and the scale keeps its usual
#' legend.
#'
#' @param title The legend title, or `NULL` to keep the scale's.
#' @param show.limits Also label the outer limits of the first and last bins
#'   (default `FALSE`: only the boundaries between bins are labelled).
#' @param reverse Reverse the order of the keys.
#' @param axis Draw the axis line and ticks beside the keys (default `TRUE`).
#' @inheritParams guides
#' @return A guide specification for [guides()] or a scale's `guide`.
#' @seealso [guides()], [guide_coloursteps()] for a binned colour scale as a
#'   segmented bar.
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, size = hp) |>
#'   scale_size_binned() |>
#'   guides(size = guide_bins())
#'
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg, color = qsec) |>
#'   scale_color_binned(n = 4) |>
#'   guides(color = guide_bins(show.limits = TRUE)) |>
#'   theme(legend.position = "bottom")
#' @export
guide_bins <- function(
  title = NULL,
  show.limits = FALSE,
  reverse = FALSE,
  axis = TRUE,
  position = NULL
) {
  if (.ggplot2_asks()) return(.ggplot2_own("guide_bins"))
  for (nm in c("show.limits", "reverse", "axis")) {
    v <- get(nm)
    if (!.is_flag(v)) {
      cli::cli_abort("{.arg {nm}} must be {.code TRUE} or {.code FALSE}.")
    }
  }
  list(
    kind = "bins",
    title = title,
    show.limits = show.limits,
    reverse = reverse,
    axis = axis,
    position = .check_guide_position(position)
  )
}

# Apply `guide_bins()` to a trained legend scale: flag it `bins` and record the
# bin boundaries the drawer labels (`bin_breaks`, one more than the keys, in key
# order -- `.reverse_guide()` has already flipped the keys when `reverse`). A
# scale with no bins (not binned) keeps its usual legend, with a warning.
.apply_bins_guide <- function(trained, guide) {
  brks <- trained$bin_breaks %||%
    (if (identical(trained$type, "binned")) trained$breaks)
  k <- .bins_n_keys(trained)
  if (is.null(brks) || length(brks) != k + 1L) {
    cli::cli_warn(c(
      "{.fn guide_bins} needs a binned scale; the {.field {trained$aesthetic}} legend is drawn as usual.",
      i = "Use e.g. {.fn scale_size_binned} or {.fn scale_color_binned}."
    ))
    return(trained)
  }
  brks <- sort(as.numeric(brks))
  if (isTRUE(guide$reverse)) {
    brks <- rev(brks)
  }
  .scale_set(
    trained,
    bins = TRUE,
    bin_breaks = brks,
    bins_limits = isTRUE(guide$show.limits),
    bins_axis = !isFALSE(guide$axis)
  )
}

# The number of keys a bins legend draws: one per bin (no NA key).
.bins_n_keys <- function(sc) {
  length(sc$colors %||% sc$legend_breaks %||% sc$levels)
}

# The labels of a bins legend, one per boundary (`k + 1`), formatted like a
# stepped colour bar's; the outer two are only drawn under `show.limits`.
.bins_labels <- function(sc) {
  .steps_break_labels(list(breaks = sc$bin_breaks))
}

# Which boundaries (0-based, 0..k) carry a tick and a label.
.bins_shown <- function(sc) {
  k <- .bins_n_keys(sc)
  if (isTRUE(sc$bins_limits)) 0:k else seq_len(max(k - 1L, 0L))
}

# The geometry (mm) of a bins legend, shared by its measurement and its
# drawer. Vertical: the keys stack `pitch` apart in a column `key_d` wide; the
# axis runs down their right edge, ticks `tick` long, labels `gap` beyond.
# Horizontal: the keys sit `cell` apart (wide enough that neighbouring boundary
# labels never touch), the axis under them, labels below the ticks.
.bins_geom <- function(g, m, horizontal = FALSE) {
  sc <- g$sc
  k <- .bins_n_keys(sc)
  key_d <- .guide_key_d(g, m)
  labs <- .bins_labels(sc)
  shown <- .bins_shown(sc)
  lw <- vapply(labs, .mm_tw, 0, fs = m$text_fs, el = m$text_el)
  lw[-(shown + 1L)] <- 0
  tick <- .LEGEND_TICK_MM
  gap <- m$lab_gap / 2
  th <- if (isTRUE(m$show_title) && !is.null(sc$name)) {
    m$title_h + m$title_gap
  } else {
    0
  }
  tw <- if (th > 0) .mm_tw_any(sc$name, m$title_fs, m$title_el) else 0
  # an end label (show.limits) overhangs the key run by half its extent
  if (horizontal) {
    # neighbouring boundary labels sit one cell apart, centred on their ticks
    pair <- if (k > 1L) (lw[-1L] + lw[-(k + 1L)]) / 2 else 0
    cell <- max(key_d, max(pair) + m$lab_gap)
    over_l <- unname(lw[1]) / 2
    over_r <- unname(lw[k + 1L]) / 2
    width <- max(tw, over_l + k * cell + over_r)
    height <- th + key_d + tick + gap + m$text_h
    list(
      k = k,
      key_d = key_d,
      labs = labs,
      shown = shown,
      tick = tick,
      gap = gap,
      th = th,
      cell = cell,
      x0 = over_l,
      width = width,
      height = height
    )
  } else {
    pitch <- max(key_d, m$text_h)
    end <- if (isTRUE(sc$bins_limits)) m$text_h / 2 else 0
    width <- max(tw, m$pad + key_d + tick + gap + max(lw))
    height <- th + 2 * end + k * pitch
    list(
      k = k,
      key_d = key_d,
      labs = labs,
      shown = shown,
      tick = tick,
      gap = gap,
      th = th,
      pitch = pitch,
      end = end,
      width = width,
      height = height
    )
  }
}

# Draw a vertical bins legend into the current viewport (`.draw_guide_v()`).
.draw_guide_bins_v <- function(scene, g, m, rt) {
  m$show_title <- isTRUE(m$show_title) && !is.null(g$sc$name)
  geo <- .bins_geom(g, m)
  txt <- .el_gpar_text(rt[["legend.text"]])
  if (geo$th > 0) {
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        y = vellum::vl_unit(1, "npc") - vellum::vl_unit(geo$th / 2, "mm"),
        height = vellum::vl_unit(geo$th, "mm")
      )
    )
    scene <- .draw_guide_title(scene, g$sc$name, rt)
    scene <- vellum::pop(scene)
  }
  run <- geo$k * geo$pitch
  top <- geo$th + geo$end # mm from the top to the first key's top edge
  y_at <- function(d) {
    vellum::vl_unit(1, "npc") - vellum::vl_unit(top + d, "mm")
  }
  key_x <- m$pad + geo$key_d / 2
  for (i in seq_len(geo$k)) {
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        x = vellum::vl_unit(key_x, "mm"),
        y = y_at((i - 0.5) * geo$pitch),
        width = vellum::vl_unit(geo$key_d, "mm"),
        height = vellum::vl_unit(geo$pitch, "mm")
      )
    )
    scene <- .draw_key(scene, g, i, m, rt, geo$labs[i])
    scene <- vellum::pop(scene)
  }
  ax <- m$pad + geo$key_d
  line_gp <- vellum::vl_gpar(col = "grey20", lwd = 0.5)
  if (isTRUE(g$sc$bins_axis)) {
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::vl_unit(ax, "mm"),
        y_at(0),
        vellum::vl_unit(ax, "mm"),
        y_at(run),
        gp = line_gp
      )
    )
  }
  for (j in geo$shown) {
    if (isTRUE(g$sc$bins_axis)) {
      scene <- vellum::draw(
        scene,
        vellum::segments_grob(
          vellum::vl_unit(ax, "mm"),
          y_at(j * geo$pitch),
          vellum::vl_unit(ax + geo$tick, "mm"),
          y_at(j * geo$pitch),
          gp = line_gp
        )
      )
    }
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        geo$labs[j + 1L],
        x = vellum::vl_unit(ax + geo$tick + geo$gap, "mm"),
        y = y_at(j * geo$pitch),
        just = c("left", "centre"),
        gp = txt
      )
    )
  }
  scene
}

# Draw a horizontal bins legend into the current viewport (`.draw_guide_h()`).
.draw_guide_bins_h <- function(scene, g, m, rt) {
  m$show_title <- isTRUE(m$show_title) && !is.null(g$sc$name)
  geo <- .bins_geom(g, m, horizontal = TRUE)
  txt <- .el_gpar_text(rt[["legend.text"]])
  if (geo$th > 0) {
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        y = vellum::vl_unit(1, "npc") - vellum::vl_unit(geo$th / 2, "mm"),
        height = vellum::vl_unit(geo$th, "mm")
      )
    )
    scene <- .draw_guide_title(scene, g$sc$name, rt)
    scene <- vellum::pop(scene)
  }
  y_at <- function(d) {
    vellum::vl_unit(1, "npc") - vellum::vl_unit(geo$th + d, "mm")
  }
  x_at <- function(d) vellum::vl_unit(geo$x0 + d, "mm")
  for (i in seq_len(geo$k)) {
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        x = x_at((i - 0.5) * geo$cell),
        y = y_at(geo$key_d / 2),
        width = vellum::vl_unit(geo$cell, "mm"),
        height = vellum::vl_unit(geo$key_d, "mm")
      )
    )
    scene <- .draw_key(scene, g, i, m, rt, geo$labs[i])
    scene <- vellum::pop(scene)
  }
  line_gp <- vellum::vl_gpar(col = "grey20", lwd = 0.5)
  if (isTRUE(g$sc$bins_axis)) {
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        x_at(0),
        y_at(geo$key_d),
        x_at(geo$k * geo$cell),
        y_at(geo$key_d),
        gp = line_gp
      )
    )
  }
  for (j in geo$shown) {
    if (isTRUE(g$sc$bins_axis)) {
      scene <- vellum::draw(
        scene,
        vellum::segments_grob(
          x_at(j * geo$cell),
          y_at(geo$key_d),
          x_at(j * geo$cell),
          y_at(geo$key_d + geo$tick),
          gp = line_gp
        )
      )
    }
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        geo$labs[j + 1L],
        x = x_at(j * geo$cell),
        y = y_at(geo$key_d + geo$tick + geo$gap),
        just = c("centre", "top"),
        gp = txt
      )
    )
  }
  scene
}
