# A plot theme from a brand.yml file

`theme_brand()` themes a plot from a
[brand.yml](https://posit-dev.github.io/brand-yml/) file, the format
Quarto uses for `_brand.yml`. The brand's colours, palette and fonts go
onto a base preset
([`theme_minimal()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
by default):

## Usage

``` r
theme_brand(
  plot,
  brand = NULL,
  mode = c("light", "dark"),
  base = theme_minimal,
  base_size = 11,
  fonts = TRUE,
  palette = TRUE,
  fetch_fonts = FALSE
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- brand:

  The brand: `NULL` (default) to find it (see below), a path to a
  brand.yml file, a `brand_yml` object from
  [`brand.yml::read_brand_yml()`](https://rdrr.io/pkg/brand.yml/man/read_brand_yml.html),
  or a list with `light` and `dark` entries (each a path or a
  `brand_yml` object), like Quarto's `brand: {light:, dark:}`.

- mode:

  `"light"` (default) or `"dark"`: which brand to use when there are
  both (in a Quarto document with light and dark brands, or a
  `list(light =, dark =)`). With a single brand it is used as given.

- base:

  A theme preset to start from, such as
  [`theme_minimal()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  (default),
  [`theme_bw()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  or
  [`theme_classic()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md).

- base_size:

  Base text size in points, as for the presets.

- fonts:

  Use the brand's font families? `FALSE` keeps the base preset's.

- palette:

  Use the brand's colours as the default discrete palette?

- fetch_fonts:

  Download or register brand fonts that are not installed (see the Fonts
  section)? Default `FALSE`.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

- `background` becomes the page (plot and panel background, and the
  `paper` of
  [`element_geom()`](https://r-vellum.github.io/vellumplot/reference/element_geom.md));
  `foreground` becomes the ink of titles and unmapped marks; `primary`
  is the `accent` of smooths and contours. The greys in between (text,
  axis text, gridlines, caption) are blends of foreground and
  background, as in brand.yml's own `theme_brand_ggplot2()` (axis text
  and caption a little darker, so they keep WCAG AA contrast), and a
  dark brand gives a dark plot.

- The discrete colour palette is `color.palette` in file order, or else
  the brand's `primary`, `secondary`, `tertiary`, `success`, `info`,
  `warning` and `danger` colours (those that are set and distinct).
  Entries in the background colour are left out. When a scale has more
  levels than the brand has colours, vellumplot's own qualitative
  colours follow. A `scale_*()` palette still wins.

- `typography.base` sets the family of all text and
  `typography.headings` the family of the title, subtitle and facet
  strips.

Elements the base preset leaves out (the axis ticks of
[`theme_minimal()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
say) stay out.

## Where the brand comes from

`brand = NULL` finds the brand the way Quarto does. Inside a Quarto
render it is the brand Quarto resolved for the document (a project
`_brand.yml`, or `brand:` in `_quarto.yml` or the front matter,
including light and dark brands; `brand: false` means no brand). Outside
Quarto it is the file named by the `BRAND_YML_PATH` environment
variable, or the first `_brand.yml` found in the working directory or a
parent of it (also in a `brand/` or `_brand/` subdirectory). Reading a
file needs the brand.yml package.

## Using the brand for every plot

Set `options(vellumplot.brand = TRUE)` (in a document's setup chunk,
say) to theme every plot that has no theme of its own with
`theme_brand()`, found as for `brand = NULL`.
`options(vellumplot.brand = "path/to/_brand.yml")` names the file. A
plot with an explicit theme
([`theme_bw()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
a `theme_brand()` of its own) keeps it;
[`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
tweaks on an unthemed plot apply on top of the brand. The first branded
plot of a session says which file it used. The option is off by default,
so existing documents do not change their look. Add
`options(vellumplot.brand.fetch_fonts = TRUE)` to fetch missing brand
fonts there, as `fetch_fonts = TRUE` does.

## Fonts

A brand font that is not installed falls back to the default family,
with a warning. `fetch_fonts = TRUE` installs it for the session first:
a `google` or `bunny` font is downloaded with
[`systemfonts::require_font()`](https://systemfonts.r-lib.org/reference/require_font.html)
(this needs the network), and a `file` font is registered from the
brand's directory with
[`systemfonts::add_fonts()`](https://systemfonts.r-lib.org/reference/add_fonts.html).
Fonts registered this way are used for drawing and measuring text in
every output format.

## See also

[`theme_minimal()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
[`element_geom()`](https://r-vellum.github.io/vellumplot/reference/element_geom.md)

## Examples

``` r
brand <- tempfile(fileext = ".yml")
writeLines(c(
  "color:",
  "  palette:",
  "    ink: '#1d2b3a'",
  "    coral: '#e4572e'",
  "    teal: '#17bebb'",
  "    gold: '#ffc914'",
  "  foreground: ink",
  "  background: '#fbf8f1'",
  "  primary: coral"
), brand)

if (requireNamespace("brand.yml", quietly = TRUE)) {
  vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    labs(title = "Fuel use by weight") |>
    theme_brand(brand)
}
```
