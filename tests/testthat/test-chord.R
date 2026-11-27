# vchord() / mark_chord(): circular flow diagram (directed out/in split + self).

flows <- data.frame(
  from = c("A", "A", "B", "C", "A"),
  to = c("B", "C", "A", "A", "A"),
  value = c(3, 2, 4, 1, 2) # A->A self is the last row
)

# ---- constructor / plumbing ------------------------------------------------

test_that("vchord() returns an aspect-locked PlotSpec with one chord layer", {
  p <- vchord(flows, from, to, value)
  expect_s3_class(p, "vellumplot::PlotSpec")
  expect_length(p@layers, 1L)
  expect_identical(p@layers[[1]]@mark, "chord")
  expect_identical(p@coord@kind, "fixed")
})

test_that("a chord plot cannot be faceted or share the panel", {
  fg <- cbind(flows, g = c("x", "x", "y", "y", "x"))
  expect_error(
    vellum::as_vellum_scene(
      vplot(fg) |> mark_chord(from, to, value) |> facet_wrap(~g)
    ),
    "faceted"
  )
  expect_error(
    vellum::as_vellum_scene(
      vplot(flows) |> mark_chord(from, to, value) |> mark_point(x = 1, y = 1)
    ),
    "no other layers"
  )
})

test_that("bad flows are a clear error", {
  bad <- data.frame(from = "a", to = "b", value = -1)
  expect_error(
    vellumplot:::.chord_layout(bad$from, bad$to, bad$value),
    "finite and non-negative"
  )
  expect_error(
    vellumplot:::.chord_layout(c("a", "b"), "c", 1),
    "same length"
  )
})

# ---- layout: sectors, out/in split, self-flows -----------------------------

test_that("sector spans are proportional to total incident weight", {
  lay <- vellumplot:::.chord_layout(flows$from, flows$to, flows$value)
  sec <- lay$sectors
  expect_setequal(sec$node, c("A", "B", "C"))
  span <- setNames(sec$theta1 - sec$theta0, sec$node)
  # incident weight: A out=3+2+2=7, in=4+1+2=7 -> 14; B out=4,in=3 -> 7; C out=1,in=2 -> 3
  w <- c(A = 14, B = 7, C = 3)
  expect_equal(
    unname(span[names(w)] / span[["B"]]),
    unname(w / w[["B"]]),
    tolerance = 1e-6
  )
  # sectors + gaps fill the circle: sum(span) = (1 - n*gap) * 2*pi
  expect_equal(sum(span), (1 - 3 * 0.02) * 2 * pi, tolerance = 1e-6)
})

test_that("each node's outgoing ribbons precede its incoming ones (out/in split)", {
  lay <- vellumplot:::.chord_layout(flows$from, flows$to, flows$value)
  rib <- lay$ribbons
  # angles decrease clockwise, so a node's out block (drawn first) sits at larger
  # angles than its in block. For node A: its source arcs (outgoing) should all be
  # at higher angles than its target arcs (incoming).
  out_A <- rib[rib$src == "A", c("sa0", "sa1")]
  in_A <- rib[rib$tgt == "A", c("ta0", "ta1")]
  expect_gte(min(unlist(out_A)), max(unlist(in_A)) - 1e-9)
})

test_that("a self-flow is flagged and stays within one sector", {
  lay <- vellumplot:::.chord_layout(flows$from, flows$to, flows$value)
  rib <- lay$ribbons
  self <- rib[rib$self, ]
  expect_equal(nrow(self), 1L) # A->A
  expect_identical(self$src, "A")
  expect_identical(self$tgt, "A")
  # its source and target arcs lie inside A's sector
  a <- lay$sectors[lay$sectors$node == "A", ]
  expect_true(all(c(self$sa0, self$sa1, self$ta0, self$ta1) <= a$theta1 + 1e-9))
  expect_true(all(c(self$sa0, self$sa1, self$ta0, self$ta1) >= a$theta0 - 1e-9))
})

test_that("link_color chooses the ribbon's source or target hue", {
  src <- vellumplot:::.chord_layout(
    flows$from,
    flows$to,
    flows$value,
    link_color = "source"
  )
  tgt <- vellumplot:::.chord_layout(
    flows$from,
    flows$to,
    flows$value,
    link_color = "target"
  )
  # the A->B ribbon: source colour = A's, target colour = B's (differ)
  pal <- setNames(src$sectors$colour, src$sectors$node)
  ab_s <- src$ribbons[src$ribbons$src == "A" & src$ribbons$tgt == "B", ]
  ab_t <- tgt$ribbons[tgt$ribbons$src == "A" & tgt$ribbons$tgt == "B", ]
  expect_identical(ab_s$colour, unname(pal[["A"]]))
  expect_identical(ab_t$colour, unname(pal[["B"]]))
})

# ---- matrix input ----------------------------------------------------------

test_that("a square matrix unrolls to from/to/value flows (diagonal = self)", {
  m <- matrix(
    c(0, 3, 1, 2, 0, 4, 0, 2, 5),
    3,
    3,
    byrow = TRUE,
    dimnames = list(c("X", "Y", "Z"), c("X", "Y", "Z"))
  )
  fl <- vellumplot:::.chord_as_flows(m)
  # zero cells dropped; Z->Z (diagonal, 5) kept
  expect_false(any(fl$value == 0))
  zz <- fl[fl$from == "Z" & fl$to == "Z", ]
  expect_equal(zz$value, 5)
  expect_s3_class(vchord(m), "vellumplot::PlotSpec")
})

# ---- labels ----------------------------------------------------------------

test_that("sector labels are anchored outward (justified, not centred on the ring)", {
  # regression: centred labels overlapped the sector band; they must be
  # start-/end-anchored so they sit outside the ring.
  svg <- vellum::scene_svg(vellum::as_vellum_scene(vchord(
    flows,
    from,
    to,
    value
  )))
  anch <- regmatches(svg, gregexpr('text-anchor="[a-z]+"', svg))[[1]]
  anch <- sub('text-anchor="([a-z]+)"', "\\1", anch)
  expect_true(length(anch) > 0)
  expect_false(any(anch == "middle")) # every label justified away from the ring
})

# ---- direction cue ---------------------------------------------------------

test_that("direction = gradient/both adds a fade; gap/none do not", {
  faded <- function(dir) {
    svg <- vellum::scene_svg(vellum::as_vellum_scene(
      vchord(flows, from, to, value, direction = dir)
    ))
    grepl("stop-opacity", svg) # a linear-gradient fill emits per-stop opacity
  }
  expect_true(faded("gradient"))
  expect_true(faded("both"))
  expect_false(faded("gap"))
  expect_false(faded("none"))
})

# ---- label room (no clipping) ----------------------------------------------

test_that("panel extent widens for long labels so they do not clip", {
  short <- vellumplot:::.chord_extent(c("A", "B"), TRUE)
  long <- vellumplot:::.chord_extent(c("Borussia Moenchengladbach", "X"), TRUE)
  expect_gt(long, short) # longer labels reserve more room outside the ring
  # labels off: just the ring plus a touch (no label margin)
  off <- vellumplot:::.chord_extent(c("Borussia Moenchengladbach"), FALSE)
  expect_lt(off, short)
})

# ---- rendering -------------------------------------------------------------

test_that("chord renders (data frame, matrix, sorted, target-coloured)", {
  m <- matrix(
    c(0, 2, 1, 3, 0, 1, 2, 1, 0),
    3,
    dimnames = list(letters[1:3], letters[1:3])
  )
  for (p in list(
    vchord(flows, from, to, value),
    vchord(flows, from, to, value, sort = "value", link_color = "target"),
    vchord(m),
    vchord(flows, from, to, value, label = FALSE)
  )) {
    f <- local_tempfile(fileext = ".png")
    render_plot(p, f)
    expect_gt(file.info(f)$size, 0)
  }
})

# The chord's sector ring radius (native) and its panel's x domain, from the
# compiled scene: the ring is the one sector grob.
chord_ring <- function(p) {
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  val <- function(x) if (is.list(x)) x$v %||% x else x
  out <- NULL
  walk <- function(node) {
    if (identical(node$class, "grob_sector") && is.null(out)) {
      out <<- max(unlist(val(node$r1$value)))
    }
    for (ch in node) {
      if (is.list(ch)) walk(ch)
    }
  }
  walk(s$root)
  out
}

test_that("chord labels fit the real panel: margin, titles, composition (VP-155)", {
  flows <- data.frame(
    from = c("Northern Territories", "Basin and Range", "East"),
    to = c("Basin and Range", "Northern Territories", "Northern Territories"),
    value = c(3, 4, 2)
  )
  p <- vchord(flows, from, to, value)
  alone <- chord_ring(p)
  # A wide margin and a title leave a smaller panel, so the ring shrinks to
  # keep the labels inside it; so does a composition cell.
  boxed <- chord_ring(
    p |> labs(title = "Title", subtitle = "Sub") |> theme(plot.margin = 20)
  )
  cell <- chord_ring(hconcat(p, p, width = 6, height = 3))
  expect_lt(boxed, alone)
  expect_lt(cell, alone)
  # Without labels nothing needs room: the ring keeps its radius.
  bare <- vchord(flows, from, to, value, label = FALSE)
  expect_equal(chord_ring(bare), vellumplot:::.CHORD_R_OUT)
  expect_equal(
    chord_ring(bare |> theme(plot.margin = 20)),
    vellumplot:::.CHORD_R_OUT
  )
})

test_that(".chord_fit() puts the longest label's end inside the panel (VP-155)", {
  labs <- c("Borussia Moenchengladbach", "X")
  ext <- vellumplot:::.chord_extent(labs, TRUE)
  ch <- list(ext = ext, sectors = data.frame(node = labs))
  ctx <- vellumplot:::.mark_ctx
  old <- ctx$panel_mm
  on.exit(ctx$panel_mm <- old)
  ctx$panel_mm <- NULL
  expect_identical(vellumplot:::.chord_fit(ch, TRUE), 1)
  lw <- vellumplot:::.mm_tw(
    labs[1],
    fs = vellumplot:::.CHORD_LABEL_FS,
    el = vellumplot:::.mark_text_el(vellumplot:::.CHORD_LABEL_FS)
  )
  # Sides that leave the label room (a narrower panel clamps the extent),
  # taken from the measured width: text widths differ between platforms.
  for (side in lw * c(3, 4, 6)) {
    ctx$panel_mm <- c(w = side, h = side)
    k <- vellumplot:::.chord_fit(ch, TRUE)
    # mm per native unit on the ~5%-expanded domain [-1.05 ext, 1.05 ext]
    mm <- side / (2.1 * ext)
    reach <- k * (vellumplot:::.CHORD_R_OUT + 0.04) * mm + lw
    expect_lte(reach, side / 2)
  }
})
