# VP-69: `.facet_key_vals()`, `.facet_assign()` and `.style_groups()` work per
# distinct value instead of per row. The per-row versions they replaced are
# kept here as references; the results must be identical.

ref_facet_key_vals <- function(vals, na_level = TRUE) {
  if (length(vals) == 1 && is.factor(vals[[1]])) {
    f <- droplevels(vals[[1]])
    if (!anyNA(f) || !na_level) {
      return(f)
    }
    chr <- as.character(f)
    chr[is.na(chr)] <- "NA"
    return(factor(chr, levels = unique(c(levels(f), "NA"))))
  }
  parts <- lapply(vals, .facet_chr)
  combined <- do.call(paste, c(parts, sep = ", "))
  if (!na_level) {
    combined[Reduce(`|`, lapply(vals, is.na))] <- NA_character_
  }
  ord <- do.call(
    order,
    lapply(vals, function(v) if (is.factor(v)) as.integer(v) else v)
  )
  lv <- unique(combined[ord])
  factor(combined, levels = lv[!is.na(lv)])
}

ref_style_groups <- function(n, fields) {
  fields <- fields[!vapply(fields, is.null, logical(1))]
  if (
    !length(fields) ||
      all(vapply(fields, function(v) length(unique(v)) <= 1L, logical(1)))
  ) {
    return(list(seq_len(n)))
  }
  codes <- lapply(fields, function(v) {
    v <- rep_len(v, n)
    match(v, unique(v))
  })
  unname(split(seq_len(n), do.call(paste, c(codes, sep = "\036"))))
}

facet_values <- function(n) {
  list(
    chr = sample(c("b", "a", "NA", NA, "é", "B"), n, TRUE),
    dbl = sample(c(10, 2, 1, NA, NaN, -0, 0, 0.1 + 0.2, 0.3), n, TRUE),
    fct = factor(sample(c("z", "y", NA), n, TRUE), levels = c("z", "y", "x")),
    lgl = sample(c(TRUE, FALSE, NA), n, TRUE),
    date = as.Date("2020-01-01") + sample(c(0, 5, NA), n, TRUE),
    int = sample(c(3L, 1L, 20L), n, TRUE)
  )
}

test_that(".facet_key_vals() matches the per-row reference", {
  set.seed(11)
  for (n in c(0, 1, 7, 300)) {
    v <- facet_values(n)
    combos <- c(
      lapply(names(v), function(a) v[a]),
      list(v[c("chr", "dbl")], v[c("fct", "int")], v[c("dbl", "date", "lgl")])
    )
    for (vals in combos) {
      for (nl in c(TRUE, FALSE)) {
        expect_identical(
          .facet_key_vals(unname(vals), nl),
          ref_facet_key_vals(unname(vals), nl)
        )
      }
    }
  }
})

test_that(".style_groups() matches the per-row reference", {
  set.seed(12)
  for (n in c(1, 9, 500)) {
    v <- facet_values(n)
    cases <- list(
      list(col = v$chr),
      list(col = v$chr, alpha = v$dbl),
      list(col = v$fct, alpha = 0.5, lwd = v$int),
      list(col = "red", alpha = NULL),
      list(fill = v$lgl, col = v$date, lty = v$chr),
      # 12 codes: "10" sorts before "2" in the code strings
      list(col = sample(sprintf("c%02d", 1:12), n, TRUE), alpha = v$int)
    )
    for (fields in cases) {
      expect_identical(.style_groups(n, fields), ref_style_groups(n, fields))
    }
  }
})

test_that(".facet_assign() rows per panel match a per-panel which()", {
  set.seed(13)
  d <- data.frame(
    x = rnorm(400),
    y = rnorm(400),
    a = sample(c("p", "q", NA), 400, TRUE),
    b = sample(c(2, 10, 1), 400, TRUE)
  )
  check <- function(p) {
    fa <- .facet_assign(p)
    keys <- .facet_keys(p@facet, p@data)
    for (pn in fa$panels) {
      want <- if (fa$type == "wrap") {
        which(as.character(keys$wrap) == pn$lvl$wrap)
      } else {
        which(as.character(keys$r) == pn$lvl$r & as.character(keys$c) == pn$lvl$c)
      }
      expect_identical(pn$idx, want)
    }
    # every row lands in exactly one panel
    expect_setequal(unlist(lapply(fa$panels, `[[`, "idx")), seq_len(nrow(p@data)))
  }
  check(vplot(d) |> mark_point(x = x, y = y) |> facet_wrap(~a))
  check(vplot(d) |> mark_point(x = x, y = y) |> facet_wrap(~ a + b))
  check(vplot(d) |> mark_point(x = x, y = y) |> facet_grid(a ~ b))
  check(vplot(d) |> mark_point(x = x, y = y) |> facet_grid(. ~ b))
  check(vplot(d) |> mark_point(x = x, y = y) |> facet_grid(a ~ .))
  # A level only a layer's own data has gets an empty panel.
  extra <- data.frame(x = 0, y = 0, a = "zz", b = 2)
  fa <- .facet_assign(
    vplot(d) |>
      mark_point(x = x, y = y) |>
      mark_point(data = extra, x = x, y = y) |>
      facet_wrap(~a)
  )
  lv <- vapply(fa$panels, function(pn) pn$lvl$wrap, "")
  expect_true("zz" %in% lv)
  expect_identical(fa$panels[[match("zz", lv)]]$idx, integer(0))
})
