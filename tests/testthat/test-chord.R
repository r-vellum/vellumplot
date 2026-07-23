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
