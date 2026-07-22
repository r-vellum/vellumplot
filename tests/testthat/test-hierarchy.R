# vhierarchy() / mark_hierarchy(): space-filling hierarchy layouts + emit.

h <- data.frame(
  id = c("root", "A", "B", "A1", "A2", "B1"),
  parent = c(NA, "root", "root", "A", "A", "B"),
  value = c(NA, NA, NA, 3, 2, 4)
)

# ---- constructor / plumbing ------------------------------------------------

test_that("vhierarchy() returns an aspect-locked PlotSpec with one layer", {
  p <- vhierarchy(h, id, parent, value)
  expect_s3_class(p, "vellumplot::PlotSpec")
  expect_length(p@layers, 1L)
  expect_identical(p@layers[[1]]@mark, "hierarchy")
  expect_identical(p@layers[[1]]@params$type, "sunburst") # default
  expect_identical(p@coord@kind, "fixed") # square
})

test_that("type is validated in both constructors", {
  expect_error(vhierarchy(h, id, parent, value, type = "spiral"), "arg")
  expect_error(
    vplot(h) |> mark_hierarchy(id, parent, value, type = "spiral"),
    "arg"
  )
})

test_that("a hierarchy plot cannot be faceted or share the panel", {
  hh <- data.frame(
    id = c("r", "a", "b"),
    parent = c(NA, "r", "r"),
    value = c(NA, 1, 2),
    g = c("x", "x", "y")
  )
  expect_error(
    vellum::as_vellum_scene(
      vplot(hh) |> mark_hierarchy(id, parent, value) |> facet_wrap(~g)
    ),
    "faceted"
  )
  expect_error(
    vellum::as_vellum_scene(
      vplot(hh) |> mark_hierarchy(id, parent, value) |> mark_point(x = 1, y = 1)
    ),
    "no other layers"
  )
})

# ---- shared tree core ------------------------------------------------------

test_that("the tree core sums subtree values and carries the root", {
  lay <- vellumplot:::.hierarchy_layout(h$id, h$parent, h$value)
  expect_false("root" %in% lay$id) # root is structural, never drawn
  val <- setNames(lay$value, lay$id)
  expect_equal(unname(val[["A1"]]), 3) # leaf keeps its own value
  expect_equal(unname(val[["A"]]), 5) # internal node sums its children
  expect_true(all(c("depth", "branch", "leaf") %in% names(lay)))
  expect_identical(lay$leaf[lay$id == "A1"], TRUE)
  expect_identical(lay$leaf[lay$id == "A"], FALSE)
  root <- attr(lay, "root")
  expect_identical(root$id, "root")
  expect_equal(root$value, 9) # 3 + 2 + 4
})

test_that("the layout carries the branch id and its input-order levels", {
  lay <- vellumplot:::.hierarchy_layout(h$id, h$parent, h$value)
  br <- setNames(lay$branch, lay$id)
  expect_identical(unname(br[c("A", "A1", "A2")]), c("A", "A", "A"))
  expect_identical(unname(br[c("B", "B1")]), c("B", "B"))
  # depth-1 branches in input order, so the default palette matches historically
  expect_identical(attr(lay, "branch_levels"), c("A", "B"))
})

test_that("non-tree inputs are rejected", {
  expect_error(
    vellumplot:::.hierarchy_layout(c("a", "b"), c(NA, NA), c(1, 1)),
    "one root"
  )
  expect_error(
    vellumplot:::.hierarchy_layout(c("a", "b"), c("b", "a"), c(1, 1)),
    "one root|cycle|descend"
  )
  expect_error(
    vellumplot:::.hierarchy_layout(c("r", "a"), c(NA, "ghost"), c(NA, 1)),
    "parent"
  )
})

test_that("a non-finite or negative leaf value is a clear error", {
  bad <- data.frame(
    id = c("root", "A", "B"),
    parent = c(NA, "root", "root"),
    value = c(NA, 1, -2)
  )
  expect_error(
    vellumplot:::.hierarchy_layout(bad$id, bad$parent, bad$value),
    "finite and non-negative"
  )
})

# ---- sunburst geometry -----------------------------------------------------

test_that("sunburst: depth -> ring, angular span ~ value, 12 o'clock clockwise", {
  lay <- vellumplot:::.hierarchy_layout(
    h$id,
    h$parent,
    h$value,
    type = "sunburst"
  )
  d <- setNames(lay$depth, lay$id)
  expect_equal(unname(d[c("A", "A1", "B1")]), c(1, 2, 2))
  expect_equal(lay$r1[lay$id == "A"], lay$r0[lay$id == "A1"], tolerance = 1e-9)
  span <- setNames(lay$theta1 - lay$theta0, lay$id)
  expect_equal(unname(span[["A"]] / span[["B"]]), 5 / 4, tolerance = 1e-9)
  expect_equal(unname(span[["A"]] + span[["B"]]), 2 * pi, tolerance = 1e-9)
  # first input child owns fraction 0, so its upper edge is at 12 o'clock
  expect_equal(lay$theta1[lay$id == "A"], pi / 2, tolerance = 1e-9)
})

test_that("sunburst inner_radius opens a centre hole", {
  lay0 <- vellumplot:::.hierarchy_layout(
    h$id,
    h$parent,
    h$value,
    inner_radius = 0
  )
  lay3 <- vellumplot:::.hierarchy_layout(
    h$id,
    h$parent,
    h$value,
    inner_radius = 0.3
  )
  expect_equal(min(lay0$r0), 0)
  expect_equal(min(lay3$r0), 0.3)
  expect_error(vhierarchy(h, id, parent, value, inner_radius = 1), "\\[0, 1\\)")
})

# ---- icicle geometry -------------------------------------------------------

test_that("icicle: depth -> band, width ~ value, flow orients the depth axis", {
  down <- vellumplot:::.hierarchy_layout(
    h$id,
    h$parent,
    h$value,
    type = "icicle"
  )
  # two depth levels -> two bands filling the [-1, 1] panel
  expect_equal(sort(unique(round(down$y1, 6))), c(0, 1))
  # depth-1 nodes sit in the top band (flow = down)
  expect_true(all(down$y0[down$depth == 1] >= 0 - 1e-9))
  # A's width : B's width = 5 : 4
  wa <- with(down, x1[id == "A"] - x0[id == "A"])
  wb <- with(down, x1[id == "B"] - x0[id == "B"])
  expect_equal(wa / wb, 5 / 4, tolerance = 1e-9)
  # "right" flow puts depth on the x axis instead
  right <- vellumplot:::.hierarchy_layout(
    h$id,
    h$parent,
    h$value,
    type = "icicle",
    flow = "right"
  )
  expect_equal(sort(unique(round(right$x1, 6))), c(0, 1))
})

# ---- treemap geometry ------------------------------------------------------

test_that("treemap: rects nest inside their parent and fill the panel", {
  lay <- vellumplot:::.hierarchy_layout(
    h$id,
    h$parent,
    h$value,
    type = "treemap"
  )
  # everything within the [-1, 1] panel
  expect_true(all(lay$x0 >= -1 - 1e-9 & lay$x1 <= 1 + 1e-9))
  expect_true(all(lay$y0 >= -1 - 1e-9 & lay$y1 <= 1 + 1e-9))
  within <- function(child, par) {
    c <- lay[lay$id == child, ]
    p <- lay[lay$id == par, ]
    c$x0 >= p$x0 - 1e-9 &&
      c$x1 <= p$x1 + 1e-9 &&
      c$y0 >= p$y0 - 1e-9 &&
      c$y1 <= p$y1 + 1e-9
  }
  expect_true(within("A1", "A"))
  expect_true(within("A2", "A"))
  expect_true(within("B1", "B"))
  # leaf areas are proportional to value (A1:A2 = 3:2)
  area <- function(id) {
    r <- lay[lay$id == id, ]
    (r$x1 - r$x0) * (r$y1 - r$y0)
  }
  expect_equal(area("A1") / area("A2"), 3 / 2, tolerance = 0.02)
})

# ---- circle-pack geometry --------------------------------------------------

test_that("circlepack: sibling circles do not overlap and fit the panel", {
  lay <- vellumplot:::.hierarchy_layout(
    h$id,
    h$parent,
    h$value,
    type = "circlepack"
  )
  expect_true(all(sqrt(lay$cx^2) - lay$cr >= -1 - 1e-6)) # inside x
  expect_true(all(abs(lay$cx) + lay$cr <= 1 + 1e-6))
  expect_true(all(abs(lay$cy) + lay$cr <= 1 + 1e-6))
  # A1 and A2 (siblings) must not overlap
  a1 <- lay[lay$id == "A1", ]
  a2 <- lay[lay$id == "A2", ]
  d <- sqrt((a1$cx - a2$cx)^2 + (a1$cy - a2$cy)^2)
  expect_gte(d, a1$cr + a2$cr - 1e-6)
  # leaf radius ~ sqrt(value): A1(3) larger than A2(2)
  expect_gt(a1$cr, a2$cr)
})

test_that(".pack_siblings/.pack_enclose: non-overlap + containment", {
  r <- c(3, 1, 4, 1, 5, 9, 2, 6)
  p <- vellumplot:::.pack_siblings(r)
  n <- length(r)
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      d <- sqrt((p$x[i] - p$x[j])^2 + (p$y[i] - p$y[j])^2)
      expect_gte(d, r[i] + r[j] - 1e-6)
    }
  }
  e <- vellumplot:::.pack_enclose(p$x, p$y, r)
  for (i in seq_len(n)) {
    expect_lte(sqrt((p$x[i] - e$x)^2 + (p$y[i] - e$y)^2) + r[i], e$r + 1e-6)
  }
  # degenerate sizes must not error
  expect_silent(vellumplot:::.pack_siblings(numeric(0)))
  expect_silent(vellumplot:::.pack_siblings(2))
})

# ---- label helpers ---------------------------------------------------------

test_that(".upright_rot() keeps text upright, in (-90, 90]", {
  th <- seq(-pi, pi, length.out = 33)
  for (along in c("radius", "tangent")) {
    r <- vellumplot:::.upright_rot(th, along)
    expect_true(all(r > -90 & r <= 90))
  }
  expect_equal(vellumplot:::.upright_rot(0, "radius"), 0)
  expect_equal(vellumplot:::.upright_rot(0, "tangent"), 90)
})

test_that(".contrast_ink() picks white on dark fills, black on light", {
  ink <- vellumplot:::.contrast_ink(c("#0b1f2a", "#f0f0f0", "black", "white"))
  expect_identical(ink, c("white", "black", "white", "black"))
})

test_that("sunburst labels sit on their own wedge (y-up, since vellum 0.5.1)", {
  text_grobs <- function(p) {
    root <- vellum:::.materialize(vellum::as_vellum_scene(p))
    acc <- list()
    walk <- function(node) {
      if (S7::S7_inherits(node, vellum:::gtree)) {
        for (ch in node@children) {
          walk(ch)
        }
      } else if (S7::S7_inherits(node, vellum:::grob_text)) {
        acc[[length(acc) + 1L]] <<- node
      }
    }
    walk(root)
    acc
  }
  lay <- vellumplot:::.hierarchy_layout(
    h$id,
    h$parent,
    h$value,
    type = "sunburst"
  )
  want_ang <- setNames((lay$theta0 + lay$theta1) / 2, lay$id)
  want_rad <- setNames((lay$r0 + lay$r1) / 2, lay$id)
  # drop the branch legend, whose keys share the node ids ("A"/"B")
  p <- vhierarchy(h, id, parent, value) |> theme(legend.position = "none")
  for (g in text_grobs(p)) {
    id <- g@label
    if (!id %in% lay$id) {
      next
    }
    x <- vctrs::field(g@x, "value")
    y <- vctrs::field(g@y, "value")
    d <- ((atan2(y, x) - want_ang[[id]] + pi) %% (2 * pi)) - pi
    expect_equal(d, 0, tolerance = 1e-6, info = id)
    expect_equal(sqrt(x^2 + y^2), want_rad[[id]], tolerance = 1e-6, info = id)
  }
})

# ---- fill: branch default vs mapped ----------------------------------------

hf <- data.frame(
  id = c("root", "A", "B", "A1", "A2", "B1"),
  parent = c(NA, "root", "root", "A", "A", "B"),
  value = c(NA, NA, NA, 3, 2, 4),
  region = c(NA, NA, NA, "x", "y", "x"),
  score = c(NA, NA, NA, 1, 5, 9)
)

resolve1 <- function(p) vellumplot:::.resolve_layer(p@layers[[1]], p@data)

test_that("unmapped fill colours by branch: a factor in input order, branch mode", {
  r <- resolve1(vhierarchy(hf, id, parent, value))
  expect_identical(r$hier_fill_mode, "branch")
  expect_s3_class(r$values$fill, "factor")
  expect_identical(levels(r$values$fill), c("A", "B")) # input order
  # one fill value per drawn node, aligned to the (depth-sorted) layout rows
  expect_length(r$values$fill, nrow(r$hierarchy))
  expect_identical(as.character(r$values$fill), r$hierarchy$branch)
})

test_that("mapped fill trains on the column, realigned to the layout rows", {
  r <- resolve1(vhierarchy(hf, id, parent, value, fill = region))
  expect_identical(r$hier_fill_mode, "mapped")
  # value realigned to the layout's node order (via the frame's .node index)
  expect_identical(r$values$fill, hf$region[r$hierarchy$.node])
})

test_that("branch mode sets a 'branch' legend title; mapped and labs() override", {
  # labs(fill=) and the branch default both live under the canonical `color` key
  expect_identical(vhierarchy(hf, id, parent, value)@labels$color, "branch")
  expect_null(vhierarchy(hf, id, parent, value, fill = region)@labels$color)
  p <- vhierarchy(hf, id, parent, value) |> labs(fill = "Division")
  expect_identical(p@labels$color, "Division")
})

test_that("lighten is validated", {
  expect_error(vhierarchy(hf, id, parent, value, lighten = 2), "\\[0, 1\\]")
  expect_error(
    vplot(hf) |> mark_hierarchy(id, parent, value, lighten = -1),
    "\\[0, 1\\]"
  )
})

test_that("lighten and mapped fill change the rendered fills", {
  fills <- function(p) {
    svg <- vellum::scene_svg(vellum::as_vellum_scene(p))
    unique(regmatches(svg, gregexpr("#[0-9a-fA-F]{6}", svg))[[1]])
  }
  base <- fills(vhierarchy(hf, id, parent, value, type = "treemap"))
  flat <- fills(vhierarchy(
    hf,
    id,
    parent,
    value,
    type = "treemap",
    lighten = 0
  ))
  # flat colouring collapses each branch to one hue -> fewer distinct fills
  expect_lt(length(flat), length(base))
  # a manual branch palette puts its colours on the plot
  manual <- fills(
    vhierarchy(hf, id, parent, value, type = "treemap") |>
      scale_fill_manual(values = c(A = "#112233", B = "#445566"))
  )
  expect_true("#112233" %in% tolower(manual))
})

# ---- rendering -------------------------------------------------------------

test_that("all four hierarchy types render", {
  for (ty in c("sunburst", "icicle", "treemap", "circlepack")) {
    f <- local_tempfile(fileext = ".png")
    render_plot(
      vhierarchy(h, id, parent, value, type = ty, show_values = TRUE),
      f
    )
    expect_gt(file.info(f)$size, 0)
  }
})
