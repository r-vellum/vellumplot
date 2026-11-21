# VR-16 / VR-19: `.elem_meta()` builds the element meta column-wise, as a data
# frame vellum reads one record per row. Read back through
# `vellum::scene_model()`, it must give exactly the records of the old per-row
# build, kept here as the reference.

ref_elem_meta_rows <- function(rows, ctx) {
  lapply(rows, function(i) {
    rec <- list()
    if (!is.null(ctx$tt)) {
      rec$tooltip <- as.character(ctx$tt[[i]])
    }
    if (!is.null(ctx$hg)) {
      rec$hover_group <- as.character(ctx$hg[[i]])
    }
    if (!is.null(ctx$hc)) {
      rec$hover_color <- as.character(ctx$hc[[i]])
    }
    if (!is.null(ctx$sc)) {
      rec$selected_color <- as.character(ctx$sc[[i]])
    }
    if (!is.null(ctx$esrc)) {
      rec$source <- as.character(ctx$esrc[[i]])
    }
    if (!is.null(ctx$etgt)) {
      rec$target <- as.character(ctx$etgt[[i]])
    }
    if (!is.null(ctx$lg)) {
      rec$legend <- ctx$lg[[i]]
    }
    if (!is.null(ctx$fv)) {
      rec$filter_value <- ctx$fv[[i]]
    }
    if (!is.null(ctx$cond_tags)) {
      rec$cond <- ctx$cond_tags
      if (!is.null(ctx$cond_rowvals)) {
        rec$cond_value <- lapply(ctx$cond_rowvals, function(v) v[[i]])
      }
    }
    if (!is.null(ctx$filt_tags)) {
      rec$filt <- ctx$filt_tags
    }
    if (!is.null(ctx$join)) {
      rec$join <- as.character(ctx$join[[i]])
    }
    for (a in names(ctx$iv)) {
      v <- ctx$iv[[a]][[i]]
      if (is.character(v)) {
        rec[[paste0("i", a, "d")]] <- v
      } else {
        rec[[paste0("i", a)]] <- v
      }
    }
    rec
  })
}

# The records vellum makes of a columnar meta, via the public scene model.
via_scene_model <- function(df) {
  n <- nrow(df)
  g <- vellum::points_grob(
    seq(0.1, 0.9, length.out = n),
    0.5,
    key = paste0("k", seq_len(n)),
    meta = df
  )
  vellum::scene_model(vellum::vl_scene(2, 2) |> vellum::draw(g))$elements$meta
}

test_that("column-wise element meta equals the per-row build", {
  rows <- c(3L, 1L, 2L, 5L)
  ctxs <- list(
    list(
      lg = list("color:a", "color:b", NULL, c("color:a", "shape:x"), "color:c")
    ),
    list(lg = c("color:a", "color:b", "color:a", "color:c", "color:b")),
    list(
      tt = factor(c("p", "q", "r", "s", "t")),
      hg = 1:5,
      lg = list("color:a", NULL, "color:b", "color:b", NULL),
      fv = c(a = 0.1, b = 0.2, c = NA, d = 0.4, e = 0.5),
      cond_tags = c("s1:color", "s2:alpha"),
      filt_tags = "f",
      join = c("j1", "j2", "j3", "j4", "j5")
    ),
    list(
      tt = list("a", NULL, c("x", "y"), 4, "e"),
      hc = rep("red", 5),
      sc = rep("#00ff00", 5),
      esrc = letters[1:5],
      etgt = LETTERS[1:5],
      fv = list(1, NULL, 3, 4, NULL)
    ),
    # the fields whose shape used to need the per-row build
    list(
      tt = letters[1:5],
      cond_tags = c("s1:color", "s2:size"),
      cond_rowvals = list(
        "s1:color" = c("red", "blue", "red", "green", "grey"),
        "s2:size" = list(1, 2, 3, 4, 5)
      ),
      iv = list(
        x = c(0.5, 1.5, NA, 3.5, 4.5),
        xh = 1:5 + 0.5,
        y = c("a", "b", "c", "d", "e")
      )
    )
  )
  for (ctx in ctxs) {
    df <- vellumplot:::.elem_meta_df(rows, ctx)
    expect_s3_class(df, "data.frame")
    expect_identical(nrow(df), length(rows))
    expect_identical(via_scene_model(df), ref_elem_meta_rows(rows, ctx))
  }
})

test_that("plain fields are plain columns, so no record is built per point", {
  ctx <- list(
    tt = paste("pt", 1:5),
    lg = c("color:a", "color:b", "color:a", "color:c", "color:b"),
    fv = c(0.1, 0.2, 0.3, 0.4, 0.5),
    iv = list(x = c(1, 2, 3, 4, 5))
  )
  df <- vellumplot:::.elem_meta_df(1:5, ctx)
  expect_named(df, c("tooltip", "legend", "filter_value", "ix"))
  expect_false(any(vapply(df, is.list, logical(1))))
})

test_that("legend membership lists every series key per element", {
  sc <- list(type = "discrete")
  L <- list(
    n = 3L,
    values = list(color = c("a", "b", "a"), shape = c("x", "y", "z"))
  )
  scales <- list(color = sc, shape = list(type = "discrete"))
  expect_identical(
    vellumplot:::.legend_membership(L, scales),
    list(
      c("color:a", "shape:x"),
      c("color:b", "shape:y"),
      c("color:a", "shape:z")
    )
  )
  L$values$shape <- NULL
  expect_identical(
    vellumplot:::.legend_membership(L, scales),
    c("color:a", "color:b", "color:a")
  )
})
