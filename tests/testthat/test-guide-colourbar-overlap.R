# A colour bar's end labels are centred on their ticks, so they overhang the
# bar by half a text line: the legend title must not sit in that overhang, and
# on a horizontal bar no two labels may collide.

# Device-px boxes of every text element drawn in the legend panel. vellum's
# `tiny_text` lint reports each text element with its box; a huge legibility
# floor makes it flag (and so list) all of them.
legend_text_boxes <- function(p) {
  sc <- vellum::as_vellum_scene(p)
  pan <- vellum::scene_model(sc)$panels
  lg <- pan[pan$name == "legend", ]
  expect_equal(nrow(lg), 1L)
  r <- vellum::vl_lint(
    sc,
    rules = "tiny_text",
    min_text_pt = 1e4,
    min_text_px = 1e4
  )
  inside <- r$x0 >= lg$px0 - 0.5 &
    r$x1 <= lg$px1 + 0.5 &
    r$y0 >= lg$py0 - 0.5 &
    r$y1 <= lg$py1 + 0.5
  as.data.frame(r)[inside, c("x0", "y0", "x1", "y1")]
}

# Number of pairs of legend text boxes that intersect.
n_intersecting <- function(b) {
  n <- 0L
  for (i in seq_len(nrow(b) - 1L)) {
    for (j in (i + 1L):nrow(b)) {
      if (
        b$x0[i] < b$x1[j] &&
          b$x0[j] < b$x1[i] &&
          b$y0[i] < b$y1[j] &&
          b$y0[j] < b$y1[i]
      ) {
        n <- n + 1L
      }
    }
  }
  n
}

base <- vplot(pressure) |>
  mark_line(x = temperature, y = pressure, color = pressure)
steps <- base |>
  scale_color_binned() |>
  guides(color = guide_coloursteps())

test_that("the colourbar title clears the top tick label (vertical)", {
  b <- legend_text_boxes(base)
  expect_equal(nrow(b), 5L) # the title + 4 break labels
  b <- b[order(b$y0), ]
  # Title box ends above the top ("800") label's box.
  expect_lte(b$y1[1], b$y0[2])
  expect_equal(n_intersecting(b), 0L)
  left <- base |> guides(color = guide_colourbar(label.position = "left"))
  expect_equal(n_intersecting(legend_text_boxes(left)), 0L)
  tall <- base |> guides(color = guide_colourbar(barheight = 15))
  expect_equal(n_intersecting(legend_text_boxes(tall)), 0L)
})

test_that("the bottom label clears the NA key's label", {
  d <- transform(pressure, z = replace(pressure, 3, NA))
  p <- vplot(d) |> mark_point(x = temperature, y = pressure, color = z)
  expect_equal(n_intersecting(legend_text_boxes(p)), 0L)
})

test_that("guide_coloursteps() labels clear the title (vertical)", {
  b <- legend_text_boxes(steps)
  b <- b[order(b$y0), ]
  expect_lte(b$y1[1], b$y0[2])
  expect_equal(n_intersecting(b), 0L)
})

test_that("horizontal colour bars keep title and labels apart", {
  h <- legend_text_boxes(base |> theme(legend.position = "bottom"))
  expect_gte(nrow(h), 5L)
  expect_equal(n_intersecting(h), 0L)
  hs <- legend_text_boxes(steps |> theme(legend.position = "bottom"))
  expect_equal(n_intersecting(hs), 0L)
})
