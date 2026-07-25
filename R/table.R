#' @include classes.R sparkline.R concat.R
NULL

# A laid-out table: per-column descriptors (text or sparkline cells) plus the
# resolved grid geometry (mm), compiled to a vellum scene by `.compile_vtable`.
VTable <- S7::new_class(
  "VTable",
  package = "vellumplot",
  properties = list(
    descs = S7::class_list, # per column: list(name, kind, header, cells, align, width)
    col_widths = S7::class_double, # mm, one per column
    row_heights = S7::class_double, # mm, header + body rows
    n = S7::class_double, # body rows
    header = S7::new_property(S7::class_logical, default = TRUE),
    font_size = S7::new_property(S7::class_double, default = 9),
    width = S7::new_property(S7::class_double, default = 6),
    height = S7::new_property(S7::class_double, default = 4),
    dpi = S7::new_property(S7::class_double, default = 96)
  )
)

# `units` length -> mm (the table works in mm internally; strings measure in mm).
.mm <- function(x, units) {
  x * switch(units, mm = 1, cm = 10, `in` = 25.4, pt = 25.4 / 72, 1)
}

# A `spark` spec entry -> a function(values) -> a sparkline PlotSpec. Accepts a
# ready function, or a `type` string as shorthand for vsparkline(v, type=).
.as_spark_fn <- function(spec) {
  if (is.function(spec)) {
    return(spec)
  }
  if (is.character(spec) && length(spec) == 1L) {
    return(function(v) vsparkline(v, type = spec))
  }
  cli::cli_abort(
    "Each {.arg spark} entry must be a function {.code function(values)} or a type string."
  )
}

# Format a data column for display in a text cell.
.fmt_col <- function(x) {
  if (is.numeric(x)) {
    trimws(formatC(x, format = "g", digits = 4)) # alignment is set per cell
  } else if (inherits(x, c("Date", "POSIXt"))) {
    format(x)
  } else {
    as.character(x)
  }
}

.strw_mm <- function(labels, fontsize) {
  vapply(
    labels,
    function(s) vellum::vl_strwidth(s, fontsize = fontsize, unit = "mm"),
    numeric(1)
  )
}

#' Tables with sparkline columns
#'
#' `vtable()` lays a data frame out as a grid of cells where an ordinary column
#' renders as text and a **list-column of numeric vectors** renders as a
#' per-row [vsparkline()] — chart-in-table, drawn as one vector scene (so it
#' renders on PNG / SVG / PDF like any plot). It returns a compiled object;
#' [render_plot()] it, `print()` it, or drop it into a composition.
#'
#' @param data A data frame. Sparkline columns must be **list-columns** whose
#'   cells are numeric vectors.
#' @param spark A named list mapping a (list-)column name to how it draws: a
#'   `type` string (`"line"`/`"bar"`/`"winloss"`) or a function
#'   `function(values)` returning a [vsparkline()].
#' @param cols Character vector of columns to show, in order (default all).
#' @param align Optional named list of per-column alignment
#'   (`"left"`/`"right"`/`"centre"`); defaults to right for numeric, left otherwise.
#' @param header Draw a bold header row with an underline (default `TRUE`).
#' @param font_size Text size in points (default `9`).
#' @param spark_width Width of a sparkline column, in `units` (default `24`).
#' @param row_height Row height, in `units` (default `7`).
#' @param cell_pad Horizontal padding inside a text cell, in `units` (default `1.5`).
#' @param units Length unit for the sizes above: `"mm"` (default), `"cm"`, `"in"`,
#'   `"pt"`.
#' @param dpi Resolution for raster output.
#' @return A `VTable` (renders via [render_plot()]).
#' @seealso [vsparkline()]
#' @examples
#' df <- data.frame(name = c("A", "B", "C"), mean = c(3.1, 5.4, 2.2))
#' df$trend <- list(cumsum(rnorm(20)), cumsum(rnorm(20)), cumsum(rnorm(20)))
#' vtable(df, spark = list(trend = "line"))
#' @export
vtable <- function(
  data,
  spark = list(),
  cols = NULL,
  align = NULL,
  header = TRUE,
  font_size = 9,
  spark_width = 24,
  row_height = 7,
  cell_pad = 1.5,
  units = "mm",
  dpi = 96
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  cols <- cols %||% names(data)
  bad <- setdiff(cols, names(data))
  if (length(bad)) {
    cli::cli_abort("Unknown column{?s} in {.arg cols}: {.val {bad}}.")
  }
  n <- nrow(data)
  spark_fns <- lapply(spark, .as_spark_fn)
  sw_mm <- .mm(spark_width, units)
  pad_mm <- .mm(cell_pad, units)

  descs <- lapply(cols, function(nm) {
    if (nm %in% names(spark_fns)) {
      colvals <- data[[nm]]
      if (!is.list(colvals)) {
        cli::cli_abort(
          "Spark column {.field {nm}} must be a list-column of numeric vectors."
        )
      }
      list(
        name = nm,
        kind = "spark",
        header = nm,
        cells = lapply(colvals, function(v) spark_fns[[nm]](as.numeric(v))),
        align = "centre",
        width = sw_mm
      )
    } else {
      fmt <- .fmt_col(data[[nm]])
      al <- (align %||% list())[[nm]] %||%
        (if (is.numeric(data[[nm]])) "right" else "left")
      w <- max(.strw_mm(c(nm, fmt), font_size), na.rm = TRUE) + 2 * pad_mm
      list(
        name = nm,
        kind = "text",
        header = nm,
        cells = fmt,
        align = al,
        width = w
      )
    }
  })
  names(descs) <- cols

  col_widths <- vapply(descs, function(d) d$width, numeric(1))
  rh <- .mm(row_height, units)
  row_heights <- rep(rh, n + as.integer(isTRUE(header)))
  VTable(
    descs = descs,
    col_widths = unname(col_widths),
    row_heights = row_heights,
    n = as.double(n),
    header = isTRUE(header),
    font_size = font_size,
    width = sum(col_widths) / 25.4,
    height = sum(row_heights) / 25.4,
    dpi = dpi
  )
}

# Draw a text cell at grid (row, col), aligned within it.
.vtable_text <- function(scene, row, col, text, align, fontsize, bold) {
  x <- switch(align, left = 0.06, right = 0.94, 0.5)
  just <- switch(align, left = "left", right = "right", "centre")
  scene <- vellum::push(scene, vellum::vl_viewport(row = row, col = col))
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      as.character(text),
      x = vellum::vl_unit(x, "npc"),
      y = vellum::vl_unit(0.5, "npc"),
      just = just,
      gp = vellum::vl_gpar(
        fontsize = fontsize,
        fontface = if (bold) "bold" else "plain",
        col = "grey15"
      )
    )
  )
  vellum::pop(scene)
}

.compile_vtable <- function(vt) {
  .provenance_reset()
  scene <- vellum::vl_scene(
    width = vt@width,
    height = vt@height,
    dpi = vt@dpi,
    bg = "white"
  )
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(
        widths = vellum::vl_unit(vt@col_widths, "mm"),
        heights = vellum::vl_unit(vt@row_heights, "mm")
      )
    )
  )
  ncol <- length(vt@descs)
  row0 <- if (vt@header) 1L else 0L
  if (vt@header) {
    for (c in seq_len(ncol)) {
      d <- vt@descs[[c]]
      scene <- .vtable_text(scene, 1L, c, d$header, d$align, vt@font_size, TRUE)
    }
    # header underline: a rule spanning all columns at the header row's baseline.
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(row = 1L, col = 1L, colspan = ncol)
    )
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::vl_unit(0, "npc"),
        vellum::vl_unit(0.02, "npc"),
        vellum::vl_unit(1, "npc"),
        vellum::vl_unit(0.02, "npc"),
        gp = vellum::vl_gpar(col = "grey60", lwd = 1)
      )
    )
    scene <- vellum::pop(scene)
  }
  for (r in seq_len(vt@n)) {
    grow <- r + row0
    for (c in seq_len(ncol)) {
      d <- vt@descs[[c]]
      if (identical(d$kind, "spark")) {
        scene <- vellum::push(
          scene,
          vellum::vl_viewport(row = grow, col = c)
        )
        # inset a little so the sparkline has breathing room from cell edges /
        # the neighbouring rows.
        scene <- vellum::push(
          scene,
          vellum::vl_viewport(
            width = vellum::vl_unit(0.94, "npc"),
            height = vellum::vl_unit(0.78, "npc")
          )
        )
        scene <- .draw_plot(scene, d$cells[[r]])
        scene <- vellum::pop(scene)
        scene <- vellum::pop(scene)
      } else {
        scene <- .vtable_text(
          scene,
          grow,
          c,
          d$cells[[r]],
          d$align,
          vt@font_size,
          FALSE
        )
      }
    }
  }
  scene <- vellum::pop(scene)
  attr(scene, "vellumplot_provenance") <- .provenance_snapshot()
  scene
}

S7::method(.as_vellum_scene, VTable) <- function(x, ...) {
  .compile_vtable(x)
}

S7::method(print, VTable) <- function(x, ...) {
  vellum::display(vellum::as_vellum_scene(x))
  invisible(x)
}
