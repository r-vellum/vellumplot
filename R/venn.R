#' @include classes.R compile-marks.R
NULL

# --- set membership ----------------------------------------------------------

# Normalise `sets` to a named list of member vectors. A named list passes
# through; a data frame is read as logical membership columns (each column a
# set, TRUE = the row/element is a member) -> the members are the row ids.
.venn_sets <- function(sets) {
  if (is.data.frame(sets)) {
    logi <- vapply(
      sets,
      function(c) is.logical(c) || all(c %in% c(0, 1, NA)),
      logical(1)
    )
    if (!any(logi)) {
      cli::cli_abort(c(
        "A data frame passed to {.arg sets} needs logical membership columns.",
        i = "Each column is a set; {.code TRUE} means that row is a member."
      ))
    }
    ids <- seq_len(nrow(sets))
    return(lapply(sets[logi], function(c) ids[!is.na(c) & as.logical(c)]))
  }
  if (!is.list(sets) || is.null(names(sets)) || any(!nzchar(names(sets)))) {
    cli::cli_abort(
      "{.arg sets} must be a named list of vectors, or a data frame of logical columns."
    )
  }
  sets
}

# The 2^k - 1 Venn regions for k sets: for each non-empty subset of the sets, the
# count of elements belonging to *exactly* that subset (in every set of it, no
# other). Returns a data frame with a logical membership matrix + `count`.
.venn_regions <- function(sets) {
  k <- length(sets)
  nm <- names(sets)
  universe <- unique(unlist(sets, use.names = FALSE))
  # membership matrix: element x set
  member <- vapply(sets, function(s) universe %in% s, logical(length(universe)))
  if (is.null(dim(member))) {
    member <- matrix(member, ncol = k)
  }
  patterns <- expand.grid(rep(list(c(FALSE, TRUE)), k))
  patterns <- patterns[rowSums(patterns) > 0L, , drop = FALSE]
  names(patterns) <- nm
  patterns$count <- vapply(
    seq_len(nrow(patterns)),
    function(i) {
      want <- unlist(patterns[i, seq_len(k)])
      sum(apply(member, 1L, function(row) all(row == want)))
    },
    integer(1)
  )
  patterns
}

# --- geometry ----------------------------------------------------------------

# Circle centres (npc) + radius for a k-set Venn: the canonical 2- and 3-circle
# layouts. Returns a list of `list(cx, cy)` and a shared `r`.
.venn_layout <- function(k) {
  if (k == 2L) {
    # two circles side by side, low enough that the labels above them fit
    list(
      pos = list(c(0.36, 0.44), c(0.64, 0.44)),
      r = 0.26,
      cx = 0.5,
      cy = 0.44
    )
  } else {
    # three circles on a small ring, apex up
    ang <- pi / 2 + c(0, 2 * pi / 3, 4 * pi / 3)
    d <- 0.15
    r <- 0.25
    list(
      pos = lapply(ang, function(a) c(0.5 + d * cos(a), 0.47 + d * sin(a))),
      r = r,
      cx = 0.5,
      cy = 0.47
    )
  }
}

# Clamp an npc coordinate away from the panel edge so a label never clips.
.venn_clamp <- function(v) pmin(0.98, pmax(0.02, v))

# A circle as a closed ring of npc-numeric coordinates (a plain list, so
# vl_path_op reads it as one ring in a single coordinate space).
.venn_circle <- function(cx, cy, r, n = 120L) {
  t <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
  list(x = cx + r * cos(t), y = cy + r * sin(t))
}

# Re-wrap a vl_path_op() result (a path_grob in native-tagged numbers) as an
# npc path_grob so it draws in the panel box; NULL for an empty result.
.venn_as_npc <- function(g, gp) {
  rx <- as.numeric(vctrs::field(g@x, "value"))
  ry <- as.numeric(vctrs::field(g@y, "value"))
  if (!length(rx)) {
    return(NULL)
  }
  nper <- g@nper %||% length(rx)
  vellum::path_grob(
    vellum::vl_unit(rx, "npc"),
    vellum::vl_unit(ry, "npc"),
    id = rep(seq_along(nper), nper),
    rule = "winding",
    gp = gp
  )
}

# The solid geometry of one region: intersect the "in" circles, subtract the
# "out" ones, via vl_path_op. `circ` is the list of circle rings; `inset` the
# logical membership. Returns an npc path_grob (or NULL when the region is empty
# geometry, e.g. two disjoint circles have no intersection region).
.venn_region_grob <- function(circ, inset, gp) {
  ins <- which(inset)
  outs <- which(!inset)
  acc <- circ[[ins[1]]]
  for (j in ins[-1]) {
    acc <- vellum::vl_path_op(acc, circ[[j]], op = "intersect")
  }
  for (j in outs) {
    acc <- vellum::vl_path_op(acc, circ[[j]], op = "difference")
  }
  # With >= 2 sets every region runs at least one op, so `acc` is a path_grob
  # (never the bare input ring); `.venn_as_npc()` returns NULL for empty geometry.
  .venn_as_npc(acc, gp)
}

# Rough centroid (npc) of a region's rings, for placing its count label.
.venn_centroid <- function(g) {
  c(
    mean(as.numeric(vctrs::field(g@x, "value"))),
    mean(as.numeric(vctrs::field(g@y, "value")))
  )
}

# --- emitter -----------------------------------------------------------------

# Draw the Venn/Euler diagram (abstract, in npc): each disjoint region as a
# solid path (boolean geometry, so overlaps do not alpha-composite and it stays
# crisp in PDF), filled by its count; circle outlines over them; and the set
# names and per-region counts as labels.
.emit_venn <- function(scene, L, scales) {
  v <- L$params$venn
  circ <- v$circ
  regions <- v$regions
  nm <- v$names
  k <- length(nm)
  cnt <- regions$count
  fills <- .venn_fills(cnt)
  npc <- function(u) vellum::vl_unit(u, "npc")

  # regions (solid, boolean geometry), each with its count centred on it
  for (i in seq_len(nrow(regions))) {
    inset <- unlist(regions[i, seq_len(k)])
    g <- .venn_region_grob(
      circ,
      inset,
      vellum::vl_gpar(fill = fills[i], col = NA)
    )
    if (is.null(g)) {
      next
    }
    scene <- .draw(scene, g)
    ct <- .venn_centroid(g)
    scene <- .draw(
      scene,
      vellum::text_grob(
        as.character(cnt[i]),
        npc(ct[1]),
        npc(ct[2]),
        gp = vellum::vl_gpar(fontsize = 9, col = .venn_ink(fills[i]))
      )
    )
  }

  # circle outlines on top of the fills
  for (j in seq_along(circ)) {
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        npc(circ[[j]]$x),
        npc(circ[[j]]$y),
        gp = vellum::vl_gpar(fill = NA, col = "grey30", lwd = 1.1)
      )
    )
  }

  # set-name labels. Two circles: above each (they sit low for the room). Three:
  # pushed radially outward from the diagram centre. Both clamped inside the box.
  for (j in seq_along(circ)) {
    cx <- mean(circ[[j]]$x)
    cy <- mean(circ[[j]]$y)
    if (k == 2L) {
      lab <- c(cx, cy + v$r + 0.06)
    } else {
      dx <- cx - v$cx
      dy <- cy - v$cy
      len <- sqrt(dx^2 + dy^2)
      if (len < 1e-6) {
        dx <- 0
        dy <- 1
        len <- 1
      }
      lab <- c(cx + dx / len * (v$r + 0.05), cy + dy / len * (v$r + 0.05))
    }
    scene <- .draw(
      scene,
      vellum::text_grob(
        nm[j],
        npc(.venn_clamp(lab[1])),
        npc(.venn_clamp(lab[2])),
        gp = vellum::vl_gpar(fontsize = 11, fontface = "bold")
      )
    )
  }
  scene
}

# Per-region fill: a light-to-colour ramp by count, so an empty region is nearly
# white and the fullest is saturated.
.venn_fills <- function(count) {
  mx <- max(count, 1L)
  ramp <- grDevices::colorRamp(c("#eef3fb", "#2c5c9e"))
  apply(ramp(count / mx), 1L, function(rgb) {
    grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
  })
}

# Legible ink (dark or white) for a count sitting on a fill of a given darkness.
.venn_ink <- function(fill) {
  rgb <- grDevices::col2rgb(fill)[, 1]
  lum <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
  if (lum < 0.5) "white" else "grey15"
}

# --- constructor + mark ------------------------------------------------------

#' Venn / Euler diagrams
#'
#' `vvenn()` draws a Venn diagram of 2 or 3 sets: overlapping circles whose
#' disjoint regions are filled by how many elements fall in exactly that
#' combination of sets. Each region is drawn as solid **geometry** (via
#' `vellum::vl_path_op()`'s boolean set operations), so overlapping regions do
#' not alpha-composite and stay crisp — including in PDF, where a rasterised
#' mask would degrade.
#'
#' @param sets Either a **named list** of member vectors (`list(A = ..., B =
#'   ...)`), or a **data frame** of logical membership columns (each column is a
#'   set; `TRUE` means that row is a member). 2 or 3 sets.
#' @param width,height,dpi Figure size (inches) and resolution.
#' @return A [PlotSpec]; print or [render_plot()] it.
#' @examples
#' vvenn(list(
#'   Coffee = c("Ann", "Bo", "Cy", "Di"),
#'   Tea = c("Bo", "Di", "Ed", "Fi")
#' ))
#' @export
vvenn <- function(sets, width = 5, height = 5, dpi = 96) {
  .check_dim(width, "width")
  .check_dim(height, "height")
  .check_dpi(dpi)
  sets <- .venn_sets(sets)
  k <- length(sets)
  if (!k %in% c(2L, 3L)) {
    cli::cli_abort(c(
      "{.fn vvenn} draws 2 or 3 sets; got {k}.",
      i = "For more sets an UpSet plot is usually clearer."
    ))
  }
  lay <- .venn_layout(k)
  circ <- lapply(lay$pos, function(p) .venn_circle(p[1], p[2], lay$r))
  venn <- list(
    names = names(sets),
    regions = .venn_regions(sets),
    circ = circ,
    r = lay$r,
    cx = lay$cx,
    cy = lay$cy
  )
  p <- PlotSpec(
    data = data.frame(.venn = 1L),
    coord = CoordSpec(kind = "fixed", ratio = 1),
    theme = .theme_vgraph(),
    width = as.double(width),
    height = as.double(height),
    dpi = as.double(dpi)
  )
  .add_layer(p, "venn", rlang::enquos(), const_params = list(venn = venn))
}
