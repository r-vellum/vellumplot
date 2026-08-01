# Write a multi-page PDF

`pdf_pages()` writes several plots into one PDF, one plot per page — a
report, a slide deck, or one page per facet. It is the multi-page
companion of
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
(which writes a single page). Pages may differ in size (each plot keeps
its own `width`/`height`), and the per-page accessibility tags
(structure tree + `Alt`, see the *Accessibility* article) are written
for every page.

## Usage

``` r
pdf_pages(x, path)
```

## Arguments

- x:

  Either a **list** of plots (each a
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
  composition, or table) — one page each — or a **single faceted**
  `PlotSpec`, in which case it is split into one page per facet cell
  (the facet is dropped and the data filtered per page; each page trains
  its own scales).

- path:

  Output `.pdf` path.

## Value

`path`, invisibly.

## See also

[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md),
[`render_all()`](https://r-vellum.github.io/vellumplot/reference/render_all.md)

## Examples

``` r
p1 <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
p2 <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
f <- tempfile(fileext = ".pdf")
pdf_pages(list(p1, p2), f)

# one page per facet cell:
faceted <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
pdf_pages(faceted, tempfile(fileext = ".pdf"))
```
