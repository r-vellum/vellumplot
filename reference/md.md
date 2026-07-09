# Rich-text labels

A thin wrapper around
[`vellum::md()`](https://rdrr.io/pkg/vellum/man/md.html) that builds a
rich-text label from a markdown subset: `**bold**`, `*italic*` /
`_italic_`, `^superscript^`, `~subscript~`, and a colour span
`[text]{#c00}`. The result can be used anywhere vellumplot draws a
*title*:
[`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md)
(`title` / `subtitle` / `caption` / `tag` / `x` / `y` / `color`) and
`scale_*(name = )`. Per-element rich labels (in
[`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md))
are not yet supported.

## Usage

``` r
md(text)
```

## Arguments

- text:

  A length-one markdown string.

## Value

A `vellum_md_label` object accepted by
[`vellum::text_grob()`](https://rdrr.io/pkg/vellum/man/grob.html).

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  labs(title = md("**Fuel** economy"), y = md("Efficiency (mi gal^-1^)"))
```
