# Rich-text labels

`md(text)` builds a rich-text label from a markdown subset: `**bold**`,
`*italic*` / `_italic_`, `^superscript^`, `~subscript~`, a colour span
`[text]{#c00}`, and inline math `$...$` (see
[`tex()`](https://r-vellum.github.io/vellumplot/reference/tex.md)). It
is
[`vellum::md()`](https://r-vellum.github.io/vellum/reference/md.html),
re-exported. The result can be used anywhere vellumplot draws a *title*:
[`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md)
(`title` / `subtitle` / `caption` / `tag` / `x` / `y` / `color`) and
`scale_*(name = )`. Per-element rich labels go in
[`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
as a list, one per row.

## Details

`text` is a length-one markdown string; the result is a
`vellum_md_label` object accepted by
[`vellum::text_grob()`](https://r-vellum.github.io/vellum/reference/grob.html).
See
[`vellum::md()`](https://r-vellum.github.io/vellum/reference/md.html)
for the full reference.

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  labs(title = md("**Fuel** economy"), y = md("Efficiency (mi gal^-1^)"))
```
