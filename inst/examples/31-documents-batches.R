# Multi-page PDFs and parallel batch export: pdf_pages() and render_all().

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

# --- a multi-page PDF report ------------------------------------------------
# Each plot becomes one page; pages may differ in size and keep their a11y tags.
report <- list(
  vplot(mtcars) |> mark_point(x = wt, y = mpg) |> labs(title = "Weight vs mpg"),
  vplot(mtcars) |> mark_histogram(x = mpg, bins = 10) |> labs(title = "mpg"),
  vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg) |> labs(title = "mpg by cyl")
)
pdf_pages(report, file.path(outdir, "31-report.pdf"))

# --- one page per facet cell ------------------------------------------------
# A single faceted plot split into one page per facet (facet dropped, data
# filtered per page).
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl)) |>
  facet_wrap(~cyl) |>
  pdf_pages(file.path(outdir, "31-by-facet.pdf"))

# --- parallel batch export to separate files --------------------------------
# render_all() renders a list of plots across cores (forks on macOS/Linux),
# byte-identical to rendering them one at a time. A named list + a directory
# writes <name>.png.
render_all(
  list(
    weight = vplot(mtcars) |> mark_point(x = wt, y = mpg),
    power = vplot(mtcars) |> mark_point(x = hp, y = mpg),
    disp = vplot(mtcars) |> mark_point(x = disp, y = mpg)
  ),
  outdir
)

message("31-documents-batches: wrote a 3-page report, a facet-paged PDF, ",
        "and 3 batch PNGs to ", outdir)
