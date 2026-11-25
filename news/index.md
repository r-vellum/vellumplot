# Changelog

## vellumplot (development version)

### Bug fixes

- Layout measures text in the font the theme draws it in (VP-132).
  Legend labels and titles, axis titles and tick labels, facet strips,
  the tag and the composition title bands used to reserve space for the
  default family in the plain face, so a wider or heavier theme font
  (the `family`, `face`, `weight` or `cex` of an
  [`element_text()`](https://r-vellum.github.io/vellumplot/reference/element.md),
  a
  [`theme_brand()`](https://r-vellum.github.io/vellumplot/reference/theme_brand.md)
  font) could overflow the legend column or run into the axis title.
  Plots in the default font are unchanged.

## vellumplot 0.24.0

### New features

- [`theme_brand()`](https://r-vellum.github.io/vellumplot/reference/theme_brand.md)
  themes a plot from a brand.yml file, the format of Quarto’s
  `_brand.yml` (F-26). The brand’s background becomes the page, its
  foreground the ink of titles and unmapped marks
  ([`element_geom()`](https://r-vellum.github.io/vellumplot/reference/element_geom.md)),
  its primary colour the accent, and the greys between are
  foreground/background blends, as in brand.yml’s
  `theme_brand_ggplot2()`. `color.palette` (or the primary, secondary, …
  colours) is the default discrete palette, padded with vellumplot’s own
  colours past its length, and the base and headings font families go to
  the text and the titles and strips. It builds on
  [`theme_minimal()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  (`base =` picks another preset) and keeps the preset’s blank elements
  blank. With no `brand =` it reads the brand Quarto resolved for the
  document being rendered (light or dark, `mode =`), or else the nearest
  `_brand.yml`. `fetch_fonts = TRUE` downloads Google/Bunny brand fonts
  or registers the brand’s font files for the session. Reading a file
  needs the brand.yml package (Suggests).
- `element_text(weight = )` sets a numeric font weight (100 to 900, or a
  name such as `"light"` or `"semi-bold"`), drawn in that face of a
  multi-weight family through vellum’s new `vl_gpar(fontweight = )`
  (F-26). A bold `face` draws at least 700, so `face = "bold"` stays
  bold under a light inherited weight.
  [`theme_brand()`](https://r-vellum.github.io/vellumplot/reference/theme_brand.md)
  maps the brand’s `typography.base.weight` to the text (axis and legend
  titles and labels and the caption stay at least 400) and
  `typography.headings.weight` to the title and strips. Needs vellum
  0.16.0.9001.
- `options(vellumplot.brand = TRUE)` (or a path) themes every plot that
  has no theme of its own with the brand, and says once per session
  which file it used (F-26). Explicit themes are never overridden. Off
  by default. `options(vellumplot.brand.fetch_fonts = TRUE)` fetches
  missing brand fonts for it.
- A composition with no theme of its own draws its page background and
  title bands from its first themed sub-plot’s theme (the page was
  always white), or from the brand when auto-apply is on and no sub-plot
  sets a theme, so composed dark or branded plots have no white margin
  (F-26).
- [`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  now also warns when a theme’s default palette is dropped (a brand or
  custom palette; a preset’s own palette still round-trips).

## vellumplot 0.23.1

### Bug fixes

- [`scale_colour_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md),
  [`scale_fill_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  and the other manual scales accept a list of rich labels in
  `labels =`, as the discrete scales already did (F-31):
  `labels = list(tex("\\sigma = 1"), "plain")`.

## vellumplot 0.23.0

### New features

- Math labels everywhere a plot draws text (F-31).
  [`tex()`](https://r-vellum.github.io/vellumplot/reference/tex.md) and
  [`plotmath()`](https://r-vellum.github.io/vellumplot/reference/tex.md)
  are re-exported from vellum, and
  [`md()`](https://r-vellum.github.io/vellumplot/reference/md.md) labels
  take inline math between `$` signs. A math label works in every
  [`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md)
  title (the rotated y title included), in a discrete legend’s
  `labels =` (a list, one label per key; plain strings in the list are
  read as
  [`md()`](https://r-vellum.github.io/vellumplot/reference/md.md)), in
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  and
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  (one label, or a list with one per row), and in
  [`annotate()`](https://r-vellum.github.io/vellumplot/reference/annotate.md).
  A list may mix rich labels with plain strings; the strings are drawn
  literally (no markdown or `$` is read).

- [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  and
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  gain `parse = TRUE`, which reads each label as plotmath, as in ggplot2
  (F-31).

- [`label_parsed()`](https://r-vellum.github.io/vellumplot/reference/labellers.md)
  typesets facet strips from plotmath values, like ggplot2’s labeller of
  the same name; it survives
  [`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  (F-31).

### Bug fixes

- A list of rich labels given to a discrete colour scale’s `labels =`
  drew `<object>` in the legend, and so did a mixed list in
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md);
  `annotate("text", label = md(...))` failed with “cannot coerce class
  to a data.frame”; and
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  flattened rich labels to text (F-31). An NA label with `parse = TRUE`
  draws nothing (no empty
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  box).

- Alt text, the accessible name and legend tooltips now use a rich
  label’s text (an
  [`md()`](https://r-vellum.github.io/vellumplot/reference/md.md)
  label’s words, a
  [`tex()`](https://r-vellum.github.io/vellumplot/reference/tex.md)
  label’s TeX source) where they used to skip it or show `<rich text>`
  (F-31).

## vellumplot 0.22.1

### Bug fixes

- Edges stop at the rim of a
  [`mark_node_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  pie, as they do at a
  [`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  marker (VP-130). They used to run to the pie’s centre, so an arrowhead
  into a pie was hidden under it, and parallel or reciprocal edges
  between pies were drawn on top of each other. A vertex with both a
  marker and a pie caps at the larger of the two.

### Documentation

- The
  [`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)
  examples and the composition article give their insets a larger box
  (VP-131). Since `align_to = "panel"` became the default (VP-127) the
  fractions are of the panel, and the old boxes left the histogram inset
  cramped.

## vellumplot 0.22.0

### Breaking changes

- A bare name in a mark argument that is not a column of the layer’s
  data and holds a single value is now a setting, not a mapping
  (VP-126). `ink <- "#f4eee0"; mark_ridgeline(color = ink)` draws in
  that colour, like `color = "#f4eee0"`; it used to map the value,
  adding a one-entry legend and a default colour. A non-column name
  holding one value per row still maps, and a column still wins over a
  variable of the same name. `I(x)` forces a setting and `.env$x` forces
  a mapping (with the legend titled `x`). The check runs at draw time
  against the layer’s own data (else the plot’s), and covers the scaled
  aesthetics (`color`, `fill`, `size`, `shape`, `alpha`, `linewidth`,
  `linetype`, `pattern`, `edge_*`); a spec exported with
  [`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  /
  [`spec_to_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)
  /
  [`spec_to_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  carries the value. See “Mappings and settings” in
  [`?mark_point`](https://r-vellum.github.io/vellumplot/reference/mark_point.md).

- [`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)
  honours `align_to` (VP-127). The default `"panel"` now takes `left` /
  `bottom` / `right` / `top` as fractions of the base plot’s panel area
  (all panels of a faceted plot), `"plot"` of the plot without its
  title, subtitle and caption, and `"full"` of the whole base cell, as
  patchwork’s `inset_element()` does. Insets used to be placed in the
  whole base cell whatever `align_to` said, so an existing inset moves
  into the panel and gets smaller; pass `align_to = "full"` to keep the
  old placement. A composition of several plots still places its insets
  in the whole figure.

- [`theme_void()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  has no plot margin, matching ggplot2 (VP-128): a void plot now fills
  its box edge to edge, where it kept the 5.5 mm `plot.margin` of
  [`theme_gray()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md).
  Maps, locator insets and other void plots grow by that margin; add
  `theme(plot.margin = c(5.5, 5.5, 5.5, 5.5))` for the old look.

### New features

- [`mark_node_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  wedge colours get a legend (VP-123): the `cols` are the categories of
  the fill scale, so the legend names them (untitled unless
  `labs(fill =)` gives a title) and
  [`scale_fill_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  /
  [`scale_fill_brewer()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  recolour the wedges; colours given with `fill =` become the scale’s
  values. `size` can be mapped (`size = total`), drawn through the size
  scale with a size legend of neutral grey discs.

### Bug fixes

- `mark_node_text(repel = TRUE)` leader lines stop at the rim of the
  node glyph under the label (a
  [`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  marker or a
  [`mark_node_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  pie, plus the small gap edges leave) instead of running over it to the
  vertex centre, which made a single-colour pie look as if it had a
  radial line (VP-129). Static renders and widget payloads both carry
  the trimmed leader.

- [`mark_node_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  draws a node made of a single category as a whole disc (or ring, for a
  donut) instead of a 360-degree wedge, whose radial edges showed a seam
  from the centre to the rim (VP-122).

- [`spec_to_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  exports `scale: {zero: false}` on a continuous x or y scale whose data
  leaves zero out, so the Vega-Lite chart frames the data as vellumplot
  does instead of stretching the axis to zero (VP-124). A layer’s
  `tooltip =` now exports as Vega-Lite’s `tooltip` channel (a plot with
  any interactivity used to fail to export), and
  [`spec_from_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  imports the `tooltip` channel (one field, an array of fields, or
  `true`) into the layer’s `tooltip` instead of dropping it.

- [`guide_bivariate()`](https://r-vellum.github.io/vellumplot/reference/guide_bivariate.md)
  sizes its swatches so the break labels on the x edge no longer run
  into each other (VP-125). The automatic size fitted the widest label,
  but the first label starts at its edge rather than centring on it, so
  it could reach the second. The swatch now fits each pair of
  neighbouring labels plus a 1 mm gap.

- A cartesian panel’s scale metadata now says which gridlines the panel
  draws along each axis (`grid = list(major, minor)`, from the theme’s
  `panel.grid.major.*` / `panel.grid.minor.*`), so a widget re-ticking
  the axes draws the same set (VW-66). In a composition, a cell themed
  with `panel.grid = element_blank()` used to get gridlines back in the
  widget.

- [`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  draws under
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  and
  [`coord_radial()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  (VP-121), where the layer used to vanish without a warning. An
  intercept on the radius aesthetic is a circle (an arc over a partial
  `coord_radial(end =)` sweep) and one on the angle aesthetic a spoke
  from the inner to the outer radius.

- An aligned composition
  ([`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`hconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`vconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md))
  draws each sub-plot’s own `labs(title, subtitle, caption, tag)`
  (VP-119). The shared grid reserved their rows but drew nothing into
  them, leaving a blank band above the panel. Each band spans its own
  cell and lines up with the other cells of its row.

- An element set with
  [`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  is drawn even when its parent is blank, as in ggplot2:
  `theme_void() |> theme(axis.text.x = element_text())` now draws the x
  axis labels, which a blank `axis.text` used to swallow. The element
  takes its unset properties from its nearest non-blank ancestor. A
  preset’s own elements still follow a blank parent, so
  [`theme_classic()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  and
  [`theme_void()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  draw no minor gridlines.

- [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  no longer suggests a `data_id` for
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  and
  [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  layers. Each ridge or violin is one outline per group, so its rows are
  never paired by order; the hint fired whenever the groups had
  different numbers of rows in different states.

- A secondary axis on a log scale gets log breaks:
  `scale_x_continuous(trans = "log10", sec.axis = sec_axis(~ . * 2.2))`
  ticks the secondary axis at powers of ten, as in ggplot2, where it
  used linear breaks that left one or two crowded labels at the top of
  the range. The secondary’s default breaks now come from the primary
  scale’s transform, and the labels are placed through a lookup grid
  spaced evenly along the axis.

- A fixed-aspect plot
  ([`coord_fixed()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
  [`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md),
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md),
  `theme(aspect.ratio =)`) placed with
  [`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)
  or
  [`vellum::scene_inset()`](https://r-vellum.github.io/vellum/reference/scene_inset.html)
  fits its box (VP-120). It kept the whole box and letterboxed its panel
  inside, so a small locator map sat shrunk in the middle of a mostly
  empty frame.
  [`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)
  now shrinks the box to the plot’s shape, keeping the side nearest the
  edge the box sits on, and an aspect-locked plot’s scene tells
  [`vellum::scene_inset()`](https://r-vellum.github.io/vellum/reference/scene_inset.html)
  its shape (needs vellum with `fit =`).
  [`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)
  also gains `shadow =` for a soft drop shadow, and names each inset’s
  viewport `"inset-<k>"`.

## vellumplot 0.21.1

### Bug fixes

- Point legend keys of a layer with a tiny constant `size` are no
  smaller than a readable minimum (VP-118):
  `theme(legend.key.point.min =)`, a radius in mm, defaulting to 1 (an
  unsized point’s radius). Since VP-113 (0.21.0) a plot of small points
  keyed its colour, shape, merged and alpha legends at the points’ size,
  too small to read. The cap at half the key side stays; a size legend
  keeps its mapped sizes, an `override.aes` size is kept as given, and
  SVG icon keys keep the key size.

## vellumplot 0.21.0

### Bug fixes

- Point legend keys take a point layer’s constant `size` (VP-113):
  `mark_point(shape = g, size = 1)` keys its shapes at the points’ size
  (with R’s per-shape size factors), not at the larger default key size.
  Colour and alpha keys of a point layer do the same. A size larger than
  the key keeps to the key; a size legend keeps its mapped sizes. SVG
  icon keys keep the key size, as an icon’s `size` scales it far beyond
  a point’s.

- Line-type, line-width and line-only merged legend keys draw in their
  layer’s colour, its default black (the theme’s ink) unless the layer
  sets a constant `color`, and alpha keys of a point or line layer
  likewise (VP-114). They were grey35 and grey20.

- On a narrow date axis the sparser break candidates keep the short
  labels of the denser ones where those stay distinct (VP-116):
  six-monthly breaks read “Jan Jul”, not pretty()’s “2023-01 2023-07”,
  which were wider than the “Jan Apr Jul” that did not fit.

- ggplot2’s point shapes 8 and 11 draw as R draws them (VP-115): 8 is an
  asterisk (a plus over a cross, at the sizes of 3 and 4) and 11 two
  overlaid triangles with R’s proportions, both stroke-only, in the plot
  and in the legend keys. Both drew as an open star.

- ggplot2’s point shapes 7, 9, 10, 12, 13 and 14 draw both of their
  glyphs (a square with a cross, a diamond with a plus, a circle with a
  plus, a square with a plus, a circle with a cross, a square with a
  triangle), as R does; they drew as the plain open square, diamond or
  circle.

- `mark_crossbar(width =)` is in data units, as `mark_errorbar(width =)`
  and `mark_bar(width =)` are and as in ggplot2’s `geom_crossbar()`
  (VP-105). It was a fraction of the band, so on a continuous x a box
  could be many times too wide. The default is now 90% of the band,
  ggplot2’s default (it was 50%), so cross-bars line up with bars and
  error bars. Dodged cross-bars share the width among the groups, and
  the boxes train a continuous axis, so the outer boxes are not clipped.

- [`mark_crossbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  draws its middle line 2.5 times as wide as the box outline, as
  ggplot2’s `geom_crossbar()` does; it drew both at one width, so the
  middle value did not stand out.

- ggplot2’s solid point shapes 15-18 and 20 draw as in ggplot2 (VP-106).
  R draws 15-18 without an outline, so a 16 is slightly smaller than a
  19, and 20 (the bullet) at 2/3 the size of 19; vellumplot drew them
  all outlined at full size. Legend keys draw them the same way. By name
  these are `"circle bare"` (and so on, no outline) and
  `"circle small"`.

- Colour and size legend keys draw a point layer’s constant shape
  whatever it is (VP-107): `mark_point(shape = "square", color = g)`
  keys its colours with squares, not circles. VP-103 did this only for
  open and bordered shapes.

- Size and shape legend keys take a point layer’s constant colour:
  `mark_point(size = hp, color = "red")` keyed its sizes in grey.

- Point legend keys default to the points’ own colour (VP-109): size,
  shape and merged size + shape keys of `mark_point(size = hp)` are
  black, as the points are and as ggplot2 keys them, where they were
  grey. A merged size + shape legend takes a constant `color` too, and a
  constant `fill` that colours solid points colours their keys.

- Mapping `shape` to a variable or column of SVG paths warns (VP-112):
  it is a mapping like any other, so the levels cycle through the shape
  palette. The warning points to the literal string (or
  `shape = !!path`) for one icon and to
  [`scale_shape_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  for each row’s own icon.

- ggplot2’s point shape codes draw at R’s size for each symbol (VP-111).
  R’s graphics engine scales its symbols from the circle’s radius:
  triangles (2, 6, 17, 24, 25) are drawn about 1.56 times vellum’s
  triangle, so their area matches the circle’s; the open diamond (5, 9)
  is sqrt(2) and the bordered one (23) 1.25 times vellum’s diamond; the
  bordered square (22) is 0.89 of the open and solid squares (0, 15);
  and the plus (3) and cross (4) are sqrt(2) times vellum’s. vellumplot
  drew them all at one size, so pch 17 looked much smaller than in
  ggplot2. The plain marker names keep their sizes.

- The date-break fit judges each pair of neighbouring labels by the mean
  of their two widths (VP-110). It required the widest label’s width
  between every pair, so a month axis was thinned by its widest month
  name even where narrower names sat side by side; a six-month axis now
  keeps monthly labels on a slightly narrower plot.

## vellumplot 0.20.0

### Bug fixes

- `mark_errorbar(width =)` is in data units, as `mark_bar(width =)` is
  since VP-98 and as in ggplot2’s `geom_errorbar()` (VP-104). It was a
  fraction of the band, so on a continuous x the caps did not match the
  bars (`width = 4` on an axis with a resolution of 10 drew caps 40
  units wide). The default is now 90% of the band, ggplot2’s default and
  a default bar’s width (it was 50%), so error bars over bars span them.
  Dodged error bars share the width among the groups as dodged bars do,
  and the caps train a continuous axis, as bars’ edges do, so the outer
  caps are not clipped.

- ggplot2’s numeric point shapes draw as in ggplot2 (VP-103). The open
  codes 0-14 draw only an outline in the point colour, the solid 15-20
  fill with it, and the bordered 21-25 fill with `fill` inside a `color`
  outline (black by default; unfilled when no `fill` is given). Before,
  all three families drew the same solid marker, so
  `scale_shape_manual(values = c(1, 0))` gave filled circles and
  squares. The variants are also available by name (`"circle open"`,
  `"square filled"`, …). Legend keys follow: shape keys, colour and size
  keys of a point layer with a constant open or bordered shape, merged
  keys and `override.aes = list(shape = )`. In vellumwidget the hollow
  fallback dots (VW-63) now pick these marks up.

- A constant numeric `shape` (`mark_point(shape = 1)`) and numeric codes
  under
  [`scale_shape_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  drew no points at all: the code was read as an SVG icon path. They now
  map to their marker, as
  [`scale_shape_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md)
  values do.

- A date axis on which no
  [`pretty()`](https://rdrr.io/r/base/pretty.html) breaks fit keeps the
  first and last break of the sparsest candidate with two in range, so
  the reader can still tell the scale (VP-102). VP-92’s fallback could
  leave a single label (a 20-year axis beside a wide legend showed only
  “2000”; it now shows “2000” and “2010”).

- `mark_bar(width =)` is honoured (VP-98): it sets the bar width in data
  units, as in ggplot2’s `geom_bar()`, where it was ignored and bars
  stayed at 90% of the resolution. Dodged bars share it, and the
  continuous-x extent training (VP-90) now follows the drawn bars
  exactly, so a narrow or `position_dodge(width =)` layer pads the axis
  as ggplot2 does.

- [`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  and
  [`mark_raster()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  on a continuous axis are no longer clipped at the ends (VP-99): the
  tiles’ edges (centre +/- half the width / height) train the x and y
  scales, as ggplot2 does, the tile version of VP-90. A constant
  `mark_tile(width =, height =)` is honoured too (it was ignored).

- The discrete size and alpha scales
  ([`scale_size_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  [`scale_size_ordinal()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  [`scale_size_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  and their `scale_alpha_*()` twins) gain `drop =` (VP-100), as VP-91
  gave the colour, fill, shape and linetype scales: `drop = FALSE` keeps
  a factor level no row uses in the legend and the output range.

- Date-break fitting (VP-92) reads `guide_axis(angle =, n.dodge =)` and
  measures the panel the way the layout does (VP-101). Rotated labels
  only need their line height between them, and labels staggered over
  `n.dodge` rows sit `n.dodge` breaks apart, so such an axis gets denser
  breaks. The axis length used to be estimated as 75% of the plot’s
  width. It now subtracts the plot margins, the legends, the other
  axis’s title and label gutter, the title/subtitle/caption bands (for a
  vertical axis) and the panel spacing, so a wide legend thins the date
  labels instead of letting them collide. The wanted gap between labels
  is now 1 mm (was 2 mm, which was calibrated against the rough
  estimate).

- [`mark_edge_bundle()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)’s
  legend key is a line, drawn at the bundle’s default 0.3 opacity
  (VP-73). It was a square swatch at full opacity.

- A flow-map trunk’s default tooltip names the destinations it carries
  (VP-74), e.g. “v1 → v3, v5, v8: 35”, or “v1 → v2, v3, v4 + 2 more: 90”
  past three, where it said “v1 → …: 35”.

## vellumplot 0.19.0

### New features

- [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  gains `start_pause`, `end_pause` and `rewind` (VP-95), with
  gganimate’s semantics: hold the first / last frame for that many
  frames, and play back to the start; they come out of `nframes`. GIF,
  APNG and SVG files,
  [`anim_frame()`](https://r-vellum.github.io/vellumplot/reference/anim_frame.md)
  and the animated widget’s scrubber all follow them.

- `anim_frame(filename, animation, frame)` saves one frame of an
  animation as a PNG, SVG or PDF (VP-96), drawn exactly as in the
  animation (same frozen scales, tween and frame variables), so a poster
  matches its animation. The default frame is the one shown under
  reduced motion: the final frame of an animation that ends on its last
  state, else the first. Needs vellum’s new `vl_render_frame()`.

- [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  labels may use frame variables in an animation (VP-93):
  `label = "{round(frame_time)}"` (a constant, or a column of templates)
  is filled in on every frame, through vellum’s per-frame text, while
  the label moves as its mark. Repelled and rich-text labels are not
  templated.

- [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  tells you, once, when marks without a `data_id` are matched by row
  order within groups whose size changes between states, and suggests a
  `data_id` (VP-87).

- Frame variables in animation titles (VP-82):
  `labs(title = "Year: {round(frame_time)}")`, glue-style, as in
  gganimate. Available: `frame`, `nframes`, `progress` always;
  `closest_state`, `previous_state`, `next_state`, `transitioning` for
  [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md);
  `frame_time` for
  [`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md);
  `frame_along` for
  [`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md).
  Each `{...}` is an R expression evaluated per frame. The title,
  subtitle and caption update on **every** frame, not only at keyframes:
  each distinct string is shaped once and swapped into the tweened frame
  by vellum’s new per-frame text (needs vellum 0.13.0.9000). Data labels
  ([`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md))
  are not templated. See *Frame variables* in
  [`?animate`](https://r-vellum.github.io/vellumplot/reference/animate.md).

- [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  supports faceted plots (VP-84). Every panel of the whole data is drawn
  in every frame (a state with no rows for a panel leaves it empty
  instead of changing the layout, which would cut), the scales are
  frozen across frames as before, and a mark’s identity includes its
  panel, so a row whose facet value changes fades out of one panel and
  into the other rather than sliding across. Per-panel scales
  (`scales = "free"`, `"free_x"`, `"free_y"`, or an independent
  [`resolve_scale()`](https://r-vellum.github.io/vellumplot/reference/resolve_scale.md))
  are refused with a clear message, since frozen scales cannot honour
  them.
  [`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md)
  works with facets too.

- [`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
  writes `.apng` as well as `.png` (both an animated PNG), and explains
  a video (`.mp4`, `.webm`, …) or `.webp` filename instead of a bare
  “must end in” error: there is no encoder for them, so it suggests
  saving an APNG/GIF and converting it (an `ffmpeg` or `magick`
  one-liner).
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  on an animation now says to use
  [`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
  (or to render the plot before
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  for a still) instead of a class error (VP-85).

- [`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md)
  accepts `Date` and `POSIXct` times (VP-83). They run on their numeric
  value (segments weighted by the day/second gaps) and keep their class
  for `{frame_time}` (with the time zone) and in the cut warning’s state
  names.

### Bug fixes

- [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md),
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)
  and `glow(color =)` on a multi-series line draw one underlay per
  series again (VP-97). The recoloured underlay dropped the colour
  mapping that grouped the line, so its copy joined every series into
  one zig-zag path. The copy now keeps the layer’s grouping (or its
  explicit `group`).

- `nudge_x` / `nudge_y` follow the data axes, as in ggplot2 (VP-88):
  under
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  `nudge_y` moves a label across and `nudge_x` up/down (they used to
  move it along the screen axes, so a flipped bar chart’s labels shifted
  by about one bar), and on a reversed axis a positive nudge points
  toward larger values. Applies to
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md),
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  and
  [`mark_image()`](https://r-vellum.github.io/vellumplot/reference/mark_image.md).

- Bars, histograms, tiles and boxplot boxes draw on a reversed axis
  again (VP-89):
  [`scale_x_reverse()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  /
  [`scale_y_reverse()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  with or without
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
  drew no bars because their native width resolved to a negative size.
  Fixed in vellum (needs vellum with VP-89).

- Bars on a continuous x axis are no longer clipped at the ends (VP-90).
  The x scale is now trained on each bar’s edges (centre +/- half its
  width, dodged groups included), not only its centre, as in ggplot2, so
  `mark_bar(x = year, ...)` gets a slightly wider x domain. Explicit
  limits, discrete and binned axes are unchanged. Histograms already
  trained on their bin edges.

- A constant `if_true` in
  [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  is a setting, not a mapping (VP-86): `alpha = condition("s", 1, 0.2)`
  no longer adds an alpha legend titled “1” with keys 0.5-1.5. The same
  holds for colour, fill and size
  (`color = condition("s", "red", "grey80")` draws red points and no
  legend), and for a computed constant such as `rgb(1, 0, 0)`. A mapped
  `if_true` still trains its scale and legend as before.

- Date and date-time axes no longer overlap their labels (VP-92). The
  default breaks are chosen by the available width:
  [`pretty()`](https://rdrr.io/r/base/pretty.html) breaks are thinned
  until the widest label, measured with vellum’s text metrics at the
  axis text size, fits between neighbouring ticks along an estimated
  axis length (75% of the plot’s width, or height for a vertical axis,
  shared among facet columns or rows). The default labels are now
  [`pretty()`](https://rdrr.io/r/base/pretty.html)’s, as ggplot2 shows
  them: “2000”, “2002” for years, “Jan”, “Feb” for months, rather than
  full ISO dates. `date_breaks`, `breaks` and `date_labels` are used as
  given (`date_labels` is measured for the fit).

- Effect underlays
  ([`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md),
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md),
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md),
  [`motion()`](https://r-vellum.github.io/vellumplot/reference/motion.md))
  are keyed with their mark in animation keyframes (VR-34), so a halo
  fades in and out with its series or point instead of switching halfway
  and triggering the “cut rather than tween” warning.

- With vellum’s morphing (VR-37), a line or area whose number of points
  changes between states morphs;
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)’s
  warning and docs no longer list it as something that switches (only
  lines that cannot be morphed do).

- [`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md)
  with several series in one line, path, area, ribbon or step layer no
  longer draws a spike at the line end (VP-94): each series’ leading
  point is interpolated toward that series’ own next row (and within its
  facet panel), whatever the row order. The grouping lookup evaluated
  the wrong object and found no groups, so every series’ lead aimed at
  whichever row came next in `along`.

- [`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md)
  reveals the data along `along`, as gganimate does (VP-81). It used to
  ignore `along` and wipe a clip across the panel left to right,
  background and gridlines included: a spiral wiped by x, a
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  series wiped along the value axis and a reversed scale showed its
  newest data first. Each frame now shows the rows whose `along` has
  been reached – points appear as they are reached, and a line, path,
  area, ribbon or step grows through its rows with an interpolated
  leading point – on scales frozen over the whole data, with the
  background, grid, axes and legends drawn whole throughout. `along` may
  be numeric, Date or POSIXct. Each frame is its own keyframe, so
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  compiles `nframes` scenes (about 6 s for 100 frames of a 240-row time
  series).

- [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  tweens marks by data identity, as gganimate matches by group (VR-32).
  Each mark is keyed by its `data_id`, else its `group` (or the
  combination of the layer’s discrete mapped aesthetics), then its order
  within that group; lines and areas by their group. Reordered or
  missing rows no longer blend unrelated marks into muddy colours, a bar
  race sorted by rank slides its bars between ranks in their own
  colours, and a colour group present in only one state fades in or out
  instead of cutting the whole transition. The cut warning now fires
  only for what truly cannot be matched (a line whose number of points
  changes). Needs vellum 0.13.0.9000.

- [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  now warns, once, when adjacent states cannot tween mark for mark – a
  state that adds or drops a colour group, or a line whose number of
  points changes – naming the states. Such a transition now cuts to the
  next state halfway through instead of never showing it (VR-26, VR-27;
  the engine fix is in vellum). Needs vellum 0.13.0.9000.

- [`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md)
  no longer drops a keyframe’s rows for integer times whose text form
  differs from the double’s, such as `100000L` versus `"1e+05"` (VP-77).

- `animate(nframes =)` must be a positive whole number; `2.5` used to be
  floored silently (VP-78).

- Fractional `fps` (12.5, 0.5) is now written exactly instead of being
  rounded to a whole number (VL-15, in vellum).

- Legends no longer list unused factor levels (VP-91). As in ggplot2, a
  discrete colour, fill, shape, linetype or pattern scale drops the
  levels no row uses, and the palette goes to the levels that remain
  (two used levels of four get the palette’s first two colours). A level
  used in only some facet panels stays. The discrete colour/fill scales
  (`_discrete()`, `_manual()`, `_hue()`, `_brewer()`, `_viridis_d()`,
  `_grey()`, `_ordinal()`),
  [`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md),
  [`scale_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_linetype.md)
  and their `_manual()` forms gain `drop = FALSE` to keep every level;
  `limits` still decide the levels.

### Documentation

- `inst/examples/28-animation.R`: part 2 now animates telephones by
  region (the same seven bars every year) so the bars visibly grow, and
  part 3 is a 12-phase wave with 10 frames per step, so every frame
  moves (VP-79). The
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md),
  [`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md)
  and
  [`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
  examples use data with the same marks in every state, and
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  documents when states cut.

### Tests

- New mid-transition tests render the in-between frames for unequal
  counts, colour groups appearing and disappearing, changing vertex
  counts, changing text, two animated SVGs’ CSS isolation and fps timing
  (VP-80). The reordered-rows identity case (VR-32) is recorded as a
  skipped test.

## vellumplot 0.18.1

### Bug fixes

- A line, path or step mapped to a continuous colour that holds constant
  within each series (`mark_path(group = id, color = level)` on contour
  lines) draws each series as one plain path in its colour. It used to
  draw one segment record per vertex pair: 91 volcano contours went from
  9,097 records and 18.2k points to 91 records and 9.2k points. A series
  whose colour does vary is still coloured per segment (VP-76).

- Two composition cells that condition the same aesthetic on the same
  selection (`hconcat(p1, p2)`, each
  `color = condition("s", ..., if_false)`) no longer share a condition
  tag, so in vellumwidget each cell’s non-members take their own cell’s
  `if_false`, not the last cell’s. A tag that two cells share takes the
  cell id, `"cell-<k>:<sel>:<aes>"` (with any `"@<layer>"` suffix kept),
  in the elements and in
  [`interaction_model()`](https://r-vellum.github.io/vellumplot/reference/interaction_model.md);
  tags no other cell shares are unchanged (VP-75).

- A per-row `if_false` of a `color` or `fill` condition
  (`color = condition(sel, g, col)`) is carried to the host as colours:
  a column of the scale’s levels (or numbers, on a continuous scale) is
  taken through that scale, as `if_true` is, and any other value is read
  as a colour (`NA`, the theme dim, when it is not one). It used to be
  the raw column, which a host could not paint.
  [`?condition`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  documents per-row `if_false` columns (VW-46).

## vellumplot 0.18.0

- Requires vellum (\>= 0.13.0).

### New features

- An interactive
  [`mark_flow_map()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  keys its branches, so flows get hover, tooltips and selection in
  vellumwidget. A branch reaching a vertex the root has an edge to
  stands for that edge: it carries the edge’s two endpoints (the node
  keys, so
  [`select_neighbours()`](https://r-vellum.github.io/vellumplot/reference/select_neighbours.md)
  highlights a node’s flows), the edge’s key as `join`, and a key of its
  own (the edge key with a `flow` suffix) that never collides with the
  edge drawn by
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md).
  A trunk merging several flows gets a key but no endpoints. The default
  tooltip names the root, the vertex the flow reaches and the flow; a
  layer `tooltip` replaces it. Faceted and composed plots prefix the
  keys and endpoints as they do for nodes and edges. A static render is
  unchanged (VP-71).

### Bug fixes

- Two layers that condition the same aesthetic on the same selection (a
  point layer and a text layer, each
  `color = condition(sel, ..., if_false)`) no longer share a condition
  tag. Each such layer’s elements and its
  [`interaction_model()`](https://r-vellum.github.io/vellumplot/reference/interaction_model.md)
  condition carry `"<sel>:<aes>@<layer>"`, so in vellumwidget a
  non-selected point takes its own layer’s `if_false`, not the text
  layer’s. Conditions also carry their `tag` in
  [`interaction_model()`](https://r-vellum.github.io/vellumplot/reference/interaction_model.md).
  Tags that no other layer shares stay `"<sel>:<aes>"` (VP-72).

- Legend keys of filled marks now take the layer’s alpha, as ggplot2’s
  keys do. A
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  key draws its fill at the default `alpha = 0.8` (VP-66) and keeps an
  outline opaque, as the curve does, and a set `alpha` on
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md),
  [`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  and the other filled marks reaches their keys. Before, the keys were
  opaque. A mapped `alpha` still draws its own legend and
  `guide_legend(override.aes = list(alpha = ))` still wins (VP-67).

- Point and line legend keys now take a constant layer alpha too, as
  ggplot2’s `draw_key_point()` and `draw_key_path()` do:
  `mark_point(color = g, alpha = 0.4)` draws see-through colour keys,
  and so do the size, shape, line-type and line-width keys of a layer
  with a set `alpha`, a merged legend’s keys, and the keys of
  `mark_edges(edge_alpha = )` and `mark_nodes(alpha = )`. Each key reads
  the layer that draws its glyph (a point layer for a point key). A
  mapped `alpha` still draws its own legend, `override.aes` still wins,
  and a
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  key stays opaque, since its `alpha` is the band’s and the fitted line
  is opaque (VP-70).

- A
  [`mark_flow_map()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  in a graph faceted by a vertex attribute now draws, in each panel,
  only the edges whose two endpoints are both among the panel’s nodes,
  with those edges’ own weights, as
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  has since VP-64. A panel without the `root` draws no flow. Before,
  every panel drew the whole flow map (VP-68).

## vellumplot 0.17.1

- Requires vellum (\>= 0.12.0).

### Performance

- Faceting assigns rows to panels, and marks split rows into style
  groups, per distinct value instead of per row (VP-69): the level
  order, labels and panel rows are worked out once per facet value (or
  style tuple), and each panel’s rows come from one
  [`split()`](https://rdrr.io/r/base/split.html) instead of a comparison
  over every row per panel. Building a 200,000-point scatter with
  [`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  (4 panels) as a raster widget went from 1.46 s to 1.18 s. Scenes are
  unchanged.

- Interactive marks carry their element meta (tooltips, legend
  membership, interval values, …) as columns, a data frame with one row
  per element, instead of a list with one record per element.
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html)
  returns the same records as before, and vellumwidget reads the columns
  directly, so a large keyed plot no longer builds a record per point
  (VR-19). Needs vellum 0.11.1.9000 or later.

## vellumplot 0.17.0

### Behaviour changes

- **Behaviour change:** an unmapped
  [`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  or
  [`mark_raster()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  now fills with ggplot2’s default for areas, ribbons and tiles,
  `col_mix(ink, paper, 0.2)` from the theme’s
  [`element_geom()`](https://r-vellum.github.io/vellumplot/reference/element_geom.md):
  `"grey20"` on a light theme, instead of `"grey50"`. The old grey was
  the colour of
  [`theme_dark()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)’s
  panel, so a default area or density disappeared into it. Light-theme
  plots with these marks now draw them darker, matching
  [`ggplot2::layer_data()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
  on `theme_grey()` and
  [`theme_dark()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md).
  ggplot2’s `geom_density()` is unfilled by default;
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  keeps its filled curve and fills it like an area (VP-60).

- **Behaviour change:**
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  now draws its fill at `alpha = 0.8` by default, so a single curve
  reads lighter than VP-60’s solid grey20 and overlapping groups (`fill`
  mapped) no longer hide the ones behind them. An `alpha` you set
  (including `alpha = 1`) or map still wins, and an outline (a `color`
  beside a `fill`) stays opaque. ggplot2’s `geom_density()` is unfilled,
  so there is no ggplot2 default to match here (VP-66).

- **Behaviour change:** `position = "jitterdodge"` jitters as ggplot2’s
  [`position_jitterdodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  does: by default 40% of the resolution divided by (the most groups at
  any x + 2) each way, and not at all vertically. Before, it jittered
  40% of the dodged slot sideways (0.15 instead of 0.1 of a category
  with two groups) and 40% of the y resolution up and down. A
  [`position_jitterdodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  built with the constructor already had no vertical jitter (VP-63).

### Bug fixes

- More of the colours vellumplot draws on its own now follow a dark
  theme’s
  [`element_geom()`](https://r-vellum.github.io/vellumplot/reference/element_geom.md)
  ink, paper and accent, as the mark defaults have since VP-58: legend
  keys for missing values (`NA`) and the pattern-key border, the nested
  size legend’s leader lines,
  [`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md)
  (its line and bars `col_mix(ink, paper, 0.3)`, its highlight dots and
  win bars the accent), including a sparkline added with
  `annotate("sparkline")`,
  [`vhierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md)
  (cell borders and the root label; branch shades fade toward the
  paper), the
  [`vchord()`](https://r-vellum.github.io/vellumplot/reference/vchord.md)
  ring borders and labels,
  [`mark_node_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  wedge borders, leader lines to repelled labels, and the
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md)
  effect, which now defaults to the paper colour. So
  [`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  no longer draws black chord labels on its black canvas.
  `vsparkline(color, point_color, win_color)`, `mark_node_pie(color)`
  and `outline(color)` now default to `NULL` (the theme’s colour). Light
  themes draw exactly as before (VP-61).

- Per-panel facet legends placed above or below the panels (a theme’s
  `legend.position = "top"` / `"bottom"`) are now collected into one
  plot legend, with a message, when they would leave the panels shorter
  than 25 mm on the page. VP-59 did this only for legends beside the
  panels and only checked the panel width (VP-62).

- A composition’s collected legend (`guides = "collect"`) now goes where
  the composition theme’s `legend.position` puts it, as in patchwork:
  `wrap_plots(a, b) |> theme(legend.position = "bottom")` draws it in a
  row below the plots, spanning their panels. `"left"` and `"top"` work
  the same way, and `"none"` drops it. Before, it always went in a
  column on the right (VP-62).

- `position_dodge(preserve = "single")` in a faceted plot now sizes
  every panel’s bars from the x with the most groups across the whole
  layer, as ggplot2 does. Before, each panel counted only its own
  groups, so a panel with at most two groups drew wider bars than one
  with three (VP-63).

- A faceted interactive graph plot
  ([`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  with
  [`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md))
  keys each panel’s nodes and edges uniquely (`"p1-2:v1"`), with the
  vertex name as the cross-panel `join`. Before, edges repeated in every
  panel had the same keys. Edge endpoints now carry the same prefix as
  the node keys, also in compositions, where neighbour highlighting
  matched no node before (VP-63).

- A graph plot faceted by a vertex attribute
  ([`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  with
  [`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  or
  [`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md))
  now draws in each panel only the edges whose two endpoints are both
  among that panel’s nodes, as ggraph’s `facet_nodes()` does.
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
  [`mark_edge_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  and
  [`mark_edge_bundle()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  used to draw the whole edge table in every panel, with edges ending at
  nodes drawn in another panel. Each kept edge is now also capped at its
  own endpoints’ node sizes; before, a faceted panel looked them up by
  position and could cap an edge at another node’s radius. An edge keeps
  its key’s row number from the whole edge table, so its `join` is the
  same as in the unfaceted plot (VP-64).

- On
  [`theme_dark()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  with its count fill turned off (`fill = NULL`) and
  [`vwaffle()`](https://r-vellum.github.io/vellumplot/reference/vwaffle.md)’s
  fallback cell fill no longer draw grey50 on the theme’s grey50 panel.
  When the default grey is that close to the panel, they fill with
  `col_mix(ink, paper, 0.2)` (`"grey20"`), the area fill. This differs
  from ggplot2, whose `geom_hex()` draws grey50 there. The colourbar and
  coloursteps frames now follow the theme’s ink and paper
  ([`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  draws them in its own mid grey). Light themes draw exactly as before
  (VP-65).

## vellumplot 0.16.0

### Behaviour changes

- **Behaviour change:** `position = "dodge"` now splits the width at
  each x between the groups present there, as ggplot2’s
  [`position_dodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  does. Before, every x kept a slot for every group, so
  `mark_bar(x = factor(cyl), fill = factor(gear), position = "dodge")`
  left a gap for the missing gear-4 bar at `cyl = 8`; now that x shows
  two wider bars, centred. The default dodge width is now the bar width
  (90% of the band) instead of the whole band, so dodged bars touch, and
  a dodged box plot no longer leaves a gap between its boxes, both as in
  ggplot2. `position_dodge(preserve = "single")` keeps every element one
  group wide. Points now dodge with `position = "dodge"` too; before
  they were drawn undodged (VP-56).

- **Behaviour change:** a discrete `alpha`, `size` or `linewidth`
  mapping now groups a layer too, as ggplot2 groups by every discrete
  aesthetic. Before, `mark_line(x = wt, y = mpg, alpha = factor(am))`
  drew one line through all rows; now it draws one per `am` level, and
  stats (boxplots, densities, fits), stacks and dodges split the same
  way.
  [`mark_outlier_label()`](https://r-vellum.github.io/vellumplot/reference/mark_outlier_label.md)
  still judges outliers within colour / fill groups, so a discrete
  `alpha` or `size` there styles the labels without splitting the data
  (VP-57).

- **Behaviour change:** default element keys (for an interactive layer
  that declares no `data_id`) now always name the panel, the layer and
  the row, as `"p<row>-<col>:l<layer>:<row>"`: `"p1-1:l2:7"` is row 7 of
  layer 2 in the first (or only) panel. Composition cells keep their
  prefix in front (`"subplot-2:p1-1:l1:7"`, `"cell-1:p1-2:l1:7"`).
  Before, keys were plain row numbers, so two default-keyed layers
  shared them: in
  `mark_line(color = g) |> mark_point() |> select_point()` selecting a
  point also selected the line whose first row had the same number. Keys
  made for a tooltip-only layer also repeated across facet panels. Both
  are fixed, and the `"layer-<i>:"` prefix from 0.15.0 is gone (VP-55).
  Layers no longer link through matching row numbers: to link elements
  on purpose, give the layers the same `data_id =` (for example
  `data_id = g` on both the line and the points). The same datum drawn
  in several facet panels or composition cells still links across them
  through its `join` id.

### New features

- Themes now set the default colours of unmapped marks, as ggplot2’s
  [`element_geom()`](https://r-vellum.github.io/vellumplot/reference/element_geom.md)
  does: `theme(geom = element_geom(ink, paper, accent))`. Points, lines,
  text and error bars use `ink`, bars and areas a mix of `ink` and
  `paper`, box plot, violin and label backgrounds `paper`, smooths and
  contours `accent`; `element_geom(colour =, fill =)` override them all.
  [`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  sets a light ink, so
  `vplot(mtcars) |> mark_point(x = wt, y = mpg) |> theme_cyberpunk()` no
  longer draws black points on its near-black panel, and legend keys of
  an unmapped colour follow suit. A
  [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)
  given its own `ink` / `paper` colours its marks with them. Light
  themes draw exactly as before; mapped scales and constant `color =` /
  `fill =` still win.
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)’s
  `fill` and the `color` / `fill` of
  [`mark_scalebar()`](https://r-vellum.github.io/vellumplot/reference/map_decorations.md)
  /
  [`mark_compass()`](https://r-vellum.github.io/vellumplot/reference/map_decorations.md)
  now default to the theme’s colours (white / black on light themes)
  (VP-58).

### Bug fixes

- In an aligned composition
  ([`wrap_plots()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  `|`, `/`) each sub-plot’s legends now sit on the side their guide asks
  for (`guide_legend(position = "bottom")` and co.) or, failing that,
  where the sub-plot’s `theme(legend.position = )` puts them: left,
  right, above or below its panel, or inside it. Before, every sub-plot
  legend went to a band on its right. Collected legends
  (`guides = "collect"`) gather them from every side (VP-59).

- Legends drawn beside each facet panel
  (`guide_legend(position = "panel")`, the default of an independent
  scale) are collected into the plot’s legend, titled `"title (panel)"`,
  when they would leave the panels narrower than 25 mm on the page, with
  a message saying so. Before, a small page could squeeze the panels to
  slivers (VP-59).

## vellumplot 0.15.1

- [`annotate()`](https://r-vellum.github.io/vellumplot/reference/annotate.md)
  layers no longer get default keys just because the plot declares a
  selection. A keyed annotation made the widget’s display list inexact,
  which switched off its zoomed canvas and detail tier for the whole
  plot (VW-24). An annotation with its own `data_id` or a
  [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  is still keyed.

- Compiling a big plot is faster (VR-16): per-element legend membership,
  element meta records and SVG-marker lookup are built column-wise
  instead of with a closure per point. A 200k-point scatter compiles in
  about 0.65 s instead of 1.1 s. Output is unchanged.

## vellumplot 0.15.0

### Behaviour changes

- **Behaviour change:** the legends of a scale resolved `"independent"`
  across facet panels (`resolve_scale(color = "independent")`, and size,
  alpha, shape, linetype, linewidth, pattern, fill) are now drawn beside
  each panel, titled with the scale title alone, instead of collected in
  the legend area as `"title (panel)"`.
  `guides(color = guide_legend(position = "right"))` (or any other side)
  collects them as before. A panel with no values of an independent
  discrete scale no longer shows an empty legend (VP-53).

- **Behaviour change:** a layer is now grouped by the interaction of all
  its discrete `color`, `fill`, `pattern`, `linetype` and `shape`
  mappings, as ggplot2 groups by every discrete aesthetic. Before, only
  the first of them formed the groups, so
  `mark_bar(fill = gear, color = am)` counted by `am` alone and dropped
  `fill` with a warning, and dodges, stacks and stats (densities, fits,
  boxplots) missed the other split. Now each (gear, am) pair is its own
  bar, box, curve or fit, ordered by the first aesthetic, then the next.
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  also draws a mapped `linetype`. A layer with a single discrete
  aesthetic (or one variable on several) groups as before (VP-52).

- **Behaviour change:** `scale_*_binned()` now bins like ggplot2 by
  default (`style = "extended"`): about `n` “nice” class boundaries
  inside the data range, the outer classes widened to whole bins,
  classes closed on the right (`(a, b]`), and a colour-vector `palette`
  sampled at each class’s centre. Before, it cut the data at quantiles
  and spread the colours evenly; that is still `style = "quantile"`.
  `scale_*_steps()` is now ggplot2’s two-colour binned scale (`low`,
  `high`, `n.breaks`, `limits`, …) rather than an alias of
  `scale_*_binned()`, and matches ggplot2’s breaks and colours (VP-49).

- **Behaviour change:**
  [`scale_size_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  and
  [`scale_alpha_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md)
  on a numeric variable now error, as in ggplot2 (“Continuous values
  supplied to a discrete scale”), instead of keying every distinct
  number (VP-49).

- **Behaviour change:** `color` and `fill` are now separate scales, as
  in ggplot2. Each trains on its own channel’s values and draws its own
  legend; `scale_fill_*()` sets only the fill scale and
  `scale_color_*()` only the colour scale, so the two can be styled
  independently when both are mapped (before, both channels fed one
  pooled colour scale and the last scale declared won). The two legends
  merge into one when they show the same title and the same keys (one
  variable under one title), each key then filled by the fill scale and
  outlined by the colour scale; a colour bar and a fill bar of one
  variable fold into one bar when their palettes also match. In
  consequence: `labs(fill =)`, `guides(fill =)`, `lims(fill =)` and
  `resolve_scale(fill =)` address the fill scale only; a
  `scale_fill_*()` no longer recolours a `color` mapping (or the
  reverse); two layers mapping `color` and `fill` to variables of
  different type no longer clash; and a fill legend beside a separate
  colour legend tags its interactive keys `"fill:<level>"` (a lone fill
  legend keeps `"color:<level>"`). Plots that map only `color` or only
  `fill` render exactly as before (VP-48).

- **Behaviour change:**
  [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  and
  [`mark_density_2d_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_density_2d.md)
  now draw ggplot2’s lowest band, from the highest break at or below the
  field’s minimum (`0` for a density) to the first contour level, so the
  whole data grid is painted in the lowest colour as in
  `geom_contour_filled()`. Before, the region below the first level was
  left as panel background, and the fill scale started at the first
  level (VP-45).

### New features

- [`guide_legend()`](https://r-vellum.github.io/vellumplot/reference/guides.md),
  [`guide_colourbar()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  /
  [`guide_colorbar()`](https://r-vellum.github.io/vellumplot/reference/guides.md),
  [`guide_coloursteps()`](https://r-vellum.github.io/vellumplot/reference/guides.md),
  [`guide_bins()`](https://r-vellum.github.io/vellumplot/reference/guide_bins.md)
  and
  [`guide_bivariate()`](https://r-vellum.github.io/vellumplot/reference/guide_bivariate.md)
  gain `position =` (`"right"`, `"left"`, `"top"`, `"bottom"` or
  `"inside"`), as in ggplot2 3.5: it overrides
  `theme(legend.position = )` for that legend only, so one plot can
  carry legends on several sides (VP-53).

- The guide position `"panel"` draws a legend beside each facet panel:
  right of each panel by default, or on the side
  `theme(legend.position = )` names (left, above, below, or inside each
  panel), justified along the panel edge by `legend.justification`. Each
  column (or row) of panels gets a legend track sized to its largest
  legend, for
  [`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  and
  [`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md).
  Shared and per-panel legends mix in one plot (VP-53).

- `scale_*_stepsn()` gains `values =`, the positions of its `colours`
  along the ramp, as in ggplot2 (VP-49).

- [`bind_scale()`](https://r-vellum.github.io/vellumplot/reference/bind_scale.md)
  is now enacted by vellumwidget: brushing an overview view zooms every
  view bound to the brush to the brushed range, and clearing the brush
  resets the zoom (overview + detail, Vega-Lite’s
  `scale = list(domain = list(param = ...))`). It now checks that it
  names an interval selection that constrains the bound axis (F-15).

- [`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_interval.md)
  gains `bind = "scales"` (Vega-Lite’s `bind: "scales"`): the selection
  is the view’s own pan/zoom domain instead of a drawn brush, so zooming
  one view can drive
  [`bind_scale()`](https://r-vellum.github.io/vellumplot/reference/bind_scale.md)
  on another (linked zoom) or a
  [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  /
  [`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
  (F-15).

- Compositions
  ([`hconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`vconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`wrap_plots()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  `|`, `/`) now give each cell’s panels a unique name
  (`"cell-2-panel-1-1"`) and record the cell id in the panel `meta`. The
  panels of an aligned composition also carry the scale descriptors and
  pan group a single plot’s panel has, so a host can map their pixels to
  data and zoom them (axis-aware zoom, re-ticked axes). The interaction
  model tags each bind with its `cell`, and each selection with the
  `cells` that register it (F-15).

- [`scale_x_date()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md),
  [`scale_x_datetime()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md),
  [`scale_x_time()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md),
  [`scale_x_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_x_binned.md)
  and their `y` counterparts take `guide =`, like the continuous and
  discrete position scales:
  `scale_x_date(guide = guide_axis(n.dodge = 2))` staggers the month
  labels (VP-45).

- [`spec_to_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  exports free facet scales and
  [`resolve_scale()`](https://r-vellum.github.io/vellumplot/reference/resolve_scale.md)
  as the facet’s Vega-Lite `resolve` (`resolve.scale`, with
  `resolve.axis` for x / y and `resolve.legend` for the other
  aesthetics), and
  [`spec_from_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  reads a facet-level `resolve.scale` back. Before, the exporter ignored
  resolution, so free scales came out shared (VP-47).

- [`guide_bins()`](https://r-vellum.github.io/vellumplot/reference/guide_bins.md)
  draws the legend of a binned scale as ggplot2’s does: one key per bin,
  edge to edge, with the bin boundaries (not interval labels) on a small
  axis beside them; `show.limits = TRUE` also labels the outer limits.
  It works on
  [`scale_size_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  `scale_alpha_binned()` and the binned colour scales, vertically and in
  a top / bottom legend (VP-45).

### Bug fixes

- Default element keys no longer coincide with user `data_id`s. A mark
  keyed by default (row position, or a line’s first row) got keys such
  as `"1"`, so in
  `mark_line(x = t, y = v) |> mark_point(x = t, y = v, data_id = t)` the
  line and the point with `t = 1` shared a key and were selected
  together. In a plot where any layer declares a `data_id`, default keys
  now carry their layer (`"layer-1:1"`); user `data_id`s, and the keys
  of plots without one, are unchanged (VP-54).

- An aligned composition
  ([`hconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`vconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`wrap_plots()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
  of single-panel plots) now keeps the theme’s `plot.margin` around the
  figure, as a single plot does. Before, the first row’s panels touched
  the top edge and the last column’s the right edge (VP-50).

- Element keys no longer collide between the cells of a composition when
  a cell is faceted (or otherwise drawn as a whole plot rather than
  aligned). Such a cell names its panels `panel-r-c` like a lone plot,
  so two faceted cells both keyed marks `"panel-1-1:3"` and hovering or
  brushing one cell lit up the other. Their keys now start with the cell
  id (`"cell-2:panel-1-1:3"`), with the unprefixed row kept as the
  `join` id, as aligned cells’ keys start with `subplot-N`. Lone plots
  are unchanged (VP-51).

- ggplot2 plots no longer break when vellumplot is attached after
  ggplot2. ggplot2 looks default scales
  ([`scale_colour_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md),
  …) and guides given by name (`guide = "legend"`) up along the search
  path, where it found vellumplot’s constructors of the same name. Those
  constructors now hand a call coming from ggplot2 back to ggplot2’s own
  (VP-49).

- A merged legend (colour + shape, size, alpha, …) of a filled mark now
  draws the mark’s constant outline or fill in its keys, as a lone
  colour legend has since 0.14.0 (VP-48).

- [`scale_fill_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  declared a colour scale instead of a fill scale (VP-48).

- `guide_axis(check.overlap = TRUE)` now also thins the labels of free
  facet scales (panel by panel), fixed-aspect plots
  ([`coord_fixed()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
  [`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md),
  `aspect.ratio`) and marginal plots; before, it silently kept every
  label there. Polar axes still keep every label (VP-45).

- [`position_dodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  /
  [`position_dodge2()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  on a horizontal
  [`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  or
  [`mark_pointrange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  (`orientation = "y"`) now dodge the groups along y, the category axis,
  as a horizontal box plot does and as in ggplot2. Before, horizontal
  error bars were not dodged at all and a horizontal point-range’s
  points were shifted along the value axis (VP-45).

## vellumplot 0.14.1

### Bug fixes

- `add_marginal(group = TRUE)` fills each group’s histogram or density
  in its colour again. Since 0.14.0 made a `color` given with a `fill`
  an outline (VP-46), the marginals’ constant grey fill turned the group
  colours into thin outlines on grey.

## vellumplot 0.14.0

### Behaviour changes

- **Behaviour change:** a discrete colour scale now honours character
  `limits` (e.g. `lims(color = c("a", "b"))`): it covers just those
  levels, in that order, and other levels draw in the NA colour. Before,
  the limits of a discrete colour scale were ignored. Legend `labels` on
  a discrete colour scale may now be a function of the levels or a
  vector named by level (VP-43).

- **Behaviour change:** rows a position scale’s `oob = "censor"` puts
  outside its limits are now counted in the “Removed n rows containing
  missing values or values outside the scale range (layer k,
  `mark_x()`)” warning instead of vanishing silently, and – for the
  marks that draw one element per row, including boxplot, violin and
  smooth – they are removed before the layer’s statistic, as in ggplot2
  (VP-41).

- **Behaviour change:** on the filled marks a `color` given alongside a
  `fill` now outlines the shapes instead of overriding the fill, as in
  ggplot2: `mark_histogram(fill = "#3182bd", color = "white")` draws
  blue bars with white edges, where it used to draw white bars. This
  covers
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md),
  [`mark_count()`](https://r-vellum.github.io/vellumplot/reference/mark_count.md),
  [`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  (top edge only),
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  (both edges),
  [`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_bin2d()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_crossbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  (box and middle line),
  [`mark_dotplot()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md),
  [`mark_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
  and
  [`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md),
  matching
  [`mark_rect()`](https://r-vellum.github.io/vellumplot/reference/mark_rect.md)
  and
  [`mark_polygon()`](https://r-vellum.github.io/vellumplot/reference/mark_polygon.md);
  the slab of
  [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  takes the `fill` and leaves `color` to the point-interval. A mapped
  `color` beside a constant `fill` outlines in the mapped colours. A
  lone `color` still fills, as before. Legend keys draw the constant
  outline (or the constant fill inside an outlined box) too, also for
  box plots, violins and ridgelines (VP-46).

### New features

- More ggplot2 scale constructors, with ggplot2’s argument names
  (VP-43):
  [`scale_shape_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md),
  [`scale_linetype_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_linetype.md),
  [`scale_size_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  and
  [`scale_alpha_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md)
  (with `breaks`, `labels`, `limits` and `na.value`; shape and line-type
  `values` also take ggplot2’s numeric codes); `scale_*_hue()`,
  `scale_*_grey()` and `scale_*_ordinal()`; the binned
  `scale_*_steps2()`, `scale_*_stepsn()`, `scale_*_fermenter()` and
  `scale_*_viridis_b()`, which bin with ggplot2’s breaks (nice
  boundaries, right-closed classes) and take `oob`;
  [`scale_size_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  [`scale_radius()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  [`scale_size_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  [`scale_size_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  /
  [`scale_size_ordinal()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  [`scale_alpha_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md)
  /
  [`scale_alpha_ordinal()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md),
  [`scale_shape_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md);
  and the position shortcuts
  [`scale_x_log10()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_x_sqrt()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_x_reverse()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  and their `y` forms. Each `colour` scale also has its `color` and
  `fill` spellings. `scale_*_manual()` for colour gains `breaks`,
  `labels`, `limits` and `na.value`, and
  [`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md)
  /
  [`scale_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_linetype.md)
  gain `breaks`, `labels`, `limits` and `na.value`.

- New
  [`guide_axis()`](https://r-vellum.github.io/vellumplot/reference/guide_axis.md)
  for the position axes, used as `guides(x = )` / `guides(y = )` or the
  new `guide` argument of
  [`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_y_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_x_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  and
  [`scale_y_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md):
  `n.dodge` staggers crowded labels over rows (or columns), `angle`
  rotates them, `check.overlap` drops labels that would collide,
  `minor.ticks` adds minor ticks and `cap` ends the axis line at the
  outermost breaks. The label gutter is measured with the dodging and
  rotation, so nothing clips. `guides(x = "none")` draws an axis without
  labels or ticks.
  [`guide_axis_logticks()`](https://r-vellum.github.io/vellumplot/reference/guide_axis.md)
  draws long, middle and short log10 ticks (F-17).

- [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  and
  [`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  gain `orientation`, inferred as for box plots and violins: a discrete
  `y` with a continuous `x` draws horizontal intervals, with the slab
  above each one (VP-42).

- `scale_*_gradient()`, `scale_*_gradient2()`, `scale_*_gradientn()`,
  `scale_*_viridis_c()` and `scale_*_distiller()` gain `limits`, `oob`
  and `na.value` (and the gradients `breaks` / `labels`), as
  `scale_*_continuous()` has; the default `oob = NULL` keeps today’s
  behaviour (VP-41).

- [`mark_density_2d()`](https://r-vellum.github.io/vellumplot/reference/mark_density_2d.md)
  and
  [`mark_density_2d_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_density_2d.md):
  2-D kernel density contours with ggplot2’s `geom_density_2d()`
  semantics (`h`, `adjust`, `contour_var`, one estimate per group);
  [`mark_quantile()`](https://r-vellum.github.io/vellumplot/reference/mark_quantile.md)
  draws quantile regression lines like `geom_quantile()` (needs
  quantreg);
  [`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  and
  [`mark_pointrange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  gain `orientation` and draw horizontally when given `xmin`/`xmax` but
  no `ymin`/`ymax`, like `geom_errorbarh()` (F-16).

- New geometry marks matching ggplot2’s (F-16):
  [`mark_rect()`](https://r-vellum.github.io/vellumplot/reference/mark_rect.md)
  (corner bounds, now a documented layer mark rather than only
  `annotate("rect")`; `color` outlines alongside a `fill`),
  [`mark_polygon()`](https://r-vellum.github.io/vellumplot/reference/mark_polygon.md)
  (one polygon per group, holes via `subgroup`),
  [`mark_path()`](https://r-vellum.github.io/vellumplot/reference/mark_polygon.md)
  (connects rows in data order),
  [`mark_curve()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md)
  (`curvature`, `angle`, `ncp`) and
  [`mark_spoke()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md)
  (`angle` + `radius`).

- Independent non-position scales across facet panels:
  [`resolve_scale()`](https://r-vellum.github.io/vellumplot/reference/resolve_scale.md)
  now accepts `color` (`colour`, `fill`), `size`, `alpha`, `shape`,
  `linetype`, `linewidth`, `pattern` and the edge scales.
  `"independent"` trains that scale in each panel from the panel’s own
  data, and each panel gets its own legend, collected in the legend area
  and titled with the scale title and the panel’s strip text
  (`"hp (cyl: 4)"`). Shared scales keep their single legend, and the
  default (everything shared) is unchanged (F-8).

### Bug fixes

- Wrapped x tick labels reserve the height of the label that wraps to
  the most lines, not of the longest one, so a short-worded label no
  longer runs into the axis title (F-17).

- In a composition (`|`, `/`,
  [`wrap_plots()`](https://r-vellum.github.io/vellumplot/reference/concat.md)),
  each sub-plot now honours its own `theme(legend.position = "inside")`
  (or `c(x, y)`), drawing its legend over its panel instead of in a
  right-hand band, and `legend.position = "none"` hides that sub-plot’s
  legend. With `guides = "collect"` only the legends placed outside
  their panels are collected (VP-41).

- Filled contour bands
  ([`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md),
  [`mark_density_2d_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_density_2d.md))
  no longer flood the panel when a band spans two peaks: every band now
  closes along the edge of the density grid. Groups with too few points
  for a 2-D density (e.g. in a facet panel holding other groups) are
  skipped instead of failing the plot (F-16).

- A literal `xend`, `yend`, `xmin`, `xmax`, `ymin` or `ymax` (e.g.
  `mark_segment(xend = 5)`) is a coordinate again: it trains its axis
  and is drawn, rather than being taken for a constant style setting
  (F-16).

- [`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  (and
  [`spec_to_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md))
  serialize a facet’s declarative labeller: `label_value`, `label_both`,
  `label_wrap_gen(width)`, a named character vector, and
  [`as_labeller()`](https://r-vellum.github.io/vellumplot/reference/labellers.md)
  over a lookup table with one of those as its default. They round-trip
  through
  [`from_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  /
  [`spec_from_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)
  and render the same strips. Only an arbitrary labeller function still
  raises the classed `vellumplot_unserializable` error (VP-44).

## vellumplot 0.13.0

### Behaviour changes

- **Behaviour change:** a discrete colour scale (`scale_*_manual()`,
  `scale_*_discrete()`, `scale_*_brewer()`, …) on a numeric variable now
  errors (“Continuous values supplied to a discrete colour scale”), as
  in ggplot2, instead of giving every distinct number its own colour
  (VP-11).

- **Behaviour change:** discrete position axes drop factor levels no row
  uses, as ggplot2 does by default; `scale_x_discrete(drop = FALSE)`
  keeps them (F-10).

- **Behaviour change:** `x = Inf` / `-Inf` (and `y`) on a continuous
  axis now sits on the panel edge, as in ggplot2, rather than being lost
  (VP-11).

- **Behaviour change:** a factor, character or logical mapped to `size`
  or `alpha` now trains a discrete scale, as in ggplot2. Before, a
  factor was trained as its integer codes (the legend read 1.0, 1.5, …
  3.0, never merged with a `colour` legend of the same variable, and the
  marks failed to draw) and a character failed outright. The levels now
  spread evenly over the output range (by area for size), the legend
  keys are the levels, and a discrete size or alpha legend merges with a
  colour or shape legend of the same title and levels
  ([\#220](https://github.com/r-vellum/vellumplot/issues/220)). As in
  ggplot2, an unordered variable warns that size or alpha “for a
  discrete variable is not advised”; an ordered factor does not.
  [`scale_size()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  and
  [`scale_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md)
  `breaks` pick the levels the legend shows, and `range` sets the spread
  (VP-39).

- **Behaviour change:**
  [`guide_coloursteps()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  labels each bin boundary on its own, in the colourbar’s plain number
  style, so quantile breaks that span orders of magnitude read
  `0.0002, 0.066, 2.32, 193, 806` instead of all padded to the smallest
  one’s precision (`0.0002 ... 806.0000`) (VP-33).

- **Behaviour change:** the grammar lint rules (`single_level_scale`,
  `legend_overflow`) now check discrete shape, linetype and pattern
  scales as well as colour ones. They were meant to all along, but only
  a colour scale reported itself as discrete, so a one-level shape went
  unflagged. `guide_legend(reverse = TRUE)` now also reverses a pattern
  legend (R-14).

- **Stacks follow the group key, whatever aesthetic defines it
  (VP-38).** Without `group`, `color` or `fill`, a discrete `pattern`,
  `linetype` or `shape` now groups the layer. **Behaviour change:**
  `mark_bar(pattern = category, position = "stack")` now stacks in
  legend order (first level on top), as stacks grouped by `fill` have
  since the stack order change above. Before, a pattern-only layer was
  one group, so its stack ran in data order. Stats and dodges group by
  such an aesthetic too, as in ggplot2 (for example
  `mark_density(linetype = g)` draws one curve per `g`).

- **A violin or ridgeline fill that varies within a shape is dropped
  with a warning (VP-37).** Each violin (ridge, slab) is one polygon
  with one fill, so `mark_violin(fill = <continuous>)` painted every
  shape with its first row’s fill while the legend showed a gradient. As
  in ggplot2, whose violins are computed by a stat that drops such
  aesthetics, the varying `fill` (or `color`, `alpha`, …) is now dropped
  with a warning, and no legend is drawn for it. A continuous fill that
  is constant within each category is kept. (Areas and ribbons error
  instead, as ggplot2’s do.)

- **Stat edge cases follow ggplot2 (VP-22).**

  - **Behaviour change:** a stat layer with too few points (a density of
    one value, a smooth through one x, a hull of two points) or no
    finite values now warns and draws nothing, whether or not it is
    grouped. Before, an ungrouped layer aborted the whole plot while a
    grouped one only warned.
  - A plot whose data has zero rows draws an empty panel instead of
    reporting that it “needs an x and y encoding”. A plot that really
    maps no `x` (or no `y`) now says which axis is missing.
  - `mark_rug(x = )` (or `y =`) works on its own, with a blank axis for
    the unmapped direction.
  - The numeric stats (histogram, density, smooth, …) warn “Removed n
    rows containing non-finite values” for rows they leave out because
    an input is `Inf` or missing, as ggplot2’s stats do, instead of
    dropping them silently.

- **Small stat differences from ggplot2 are fixed (VP-23).**

  - **Behaviour change:**
    [`mark_count()`](https://r-vellum.github.io/vellumplot/reference/mark_count.md)’s
    `after_stat(prop)` is the share of the point’s group, so each
    colour’s bubbles sum to 1 (ggplot2’s `stat_sum()`). Before, it was
    the share of the whole layer.
  - The count stat behind
    [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
    now names its proportion `prop`, as ggplot2 does.
    `after_stat(density)` still works as an alias.
  - **Behaviour change:**
    [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
    evaluates the curve over the range of the layer’s data, as ggplot2
    does, instead of running on ~3 bandwidths past it. The x axis is no
    longer padded by the kernel’s tails, and every group’s curve spans
    the same x. The new `trim = TRUE` ends each group’s curve at its own
    data.
  - **Behaviour change:**
    [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
    trims each violin to the range of its data by default, as ggplot2’s
    `geom_violin()` does. Use `trim = FALSE` for the old untrimmed
    outline.
  - **Behaviour change:** `mark_hull(expand =)` and
    `mark_node_hull(expand =)` grow each hull by a uniform margin with
    rounded corners, a fraction of the layer’s range on each axis,
    instead of scaling it about its centroid. A long thin hull now gets
    the same margin as a round one, and every hull in a layer gets the
    same margin whatever its size.
    [`mark_node_hull()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)’s
    default `expand` is now `0.04`.

### New features

- **[`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  bins like ggplot2.** It gains `binwidth`, `breaks`, `boundary`,
  `center` and `closed`, with ggplot2’s meaning: `breaks` gives the bin
  edges (bins may differ in width), `binwidth` the width, and `boundary`
  / `center` where the bins are aligned. The bin stat also computes
  `ncount` and `ndensity`, and the x axis now covers the outer bars’
  edges (F-10).

  **Behaviour change:** the default `bins = 30` now places bins as
  ggplot2 does, with the outer bins centred on the smallest and largest
  values (width `range / (bins - 1)`), instead of 30 bins from the
  minimum to the maximum. Counts now match `geom_histogram()` for the
  same `bins`.

- **New
  [`mark_freqpoly()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)**
  draws the bin counts as a frequency polygon, one line per group,
  padded with an empty bin at each end like ggplot2’s `geom_freqpoly()`.
  It takes the same binning arguments as
  [`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  (F-10).

- **[`mark_summary()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  draws ranges.** With `fun.data` (`"mean_se"`, `"mean_cl_normal"`,
  `"mean_sdl"`, `"median_hilow"`, or a function such as
  [`ggplot2::mean_se()`](https://ggplot2.tidyverse.org/reference/mean_se.html)),
  or with `fun.min` / `fun.max`, it summarises each category into a
  point and a range and draws a point range, like ggplot2’s
  `stat_summary()`. `fun.args` passes extra arguments. Without them it
  draws points as before (F-10).

- **Horizontal box plots and violins.**
  [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  and
  [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  gain `orientation`. By default it is inferred as in ggplot2: a
  discrete `y` with a continuous `x`
  (`mark_boxplot(x = mpg, y = factor(cyl))`) draws horizontal boxes /
  violins, and the axis is widened for the violin outline as for
  vertical ones. `orientation = "x"` or `"y"` forces either (F-10).

- [`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  and
  [`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  take a `labeller` argument for the strip text, with ggplot2’s
  labellers:
  [`label_value()`](https://r-vellum.github.io/vellumplot/reference/labellers.md),
  [`label_both()`](https://r-vellum.github.io/vellumplot/reference/labellers.md)
  (`"cyl: 4"`), `label_wrap_gen(width)` for long values, and
  [`as_labeller()`](https://r-vellum.github.io/vellumplot/reference/labellers.md)
  for a lookup table (`c("0" = "automatic")`) or a function of the
  values. A named character vector or a plain function can be passed to
  `labeller =` directly. With several facet variables a strip shows one
  line per variable, centred, and the strip grows to fit (F-10).

- [`guide_bivariate()`](https://r-vellum.github.io/vellumplot/reference/guide_bivariate.md)
  labels the swatch edges with the class breaks
  [`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md)
  chose, along both axes, so the legend says which range each colour
  stands for (`labels = FALSE` turns them off; the swatches widen to fit
  them unless `key.size` is set). `guide_bivariate(rotate = TRUE)` turns
  the grid 45 degrees into a diamond, low-low at the bottom and
  high-high at the top. The “Scales and guides” article has a new
  bivariate section (VP-35).

- [`scale_x_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  /
  [`scale_y_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  gain `breaks` (the levels that get a tick), `labels` (a vector, a
  vector named by level, or a function) and `drop` (F-10).

- [`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_y_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md),
  [`scale_fill_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md),
  [`scale_size()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  and
  [`scale_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md)
  gain `oob`: `"censor"`, `"squish"`, `"keep"` or a `function(x, range)`
  such as
  [`scales::oob_squish_any()`](https://scales.r-lib.org/reference/oob.html),
  with `scales::oob_*()` semantics. The defaults are unchanged:
  positions and sizes keep out-of-limits values, colours squish them
  (F-10).

- Legends can sit inside the panels: `theme(legend.position = "inside")`
  with `legend.position.inside = c(x, y)` and `legend.justification`, or
  the older `legend.position = c(x, y)`. Positions are fractions of the
  panel area; the justification (a `c(x, y)` or words such as
  `c("right", "top")`) defaults to the position, so `c(1, 1)` tucks the
  legend into the top-right corner (F-10).

- `coord_cartesian(xlim =, ylim =)` (and the other coords’ limits) now
  zoom a discrete axis too, in level positions as in ggplot2:
  `xlim = c(2, 4)` shows the second to fourth levels (VP-11).

- [`spec_diagnose()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)’s
  `$report` now also counts the rows a stat leaves out itself, in a new
  `stat` column: an infinite value in a histogram, density, smooth or
  2-D bin, or every row of a group too small for a density, smooth,
  ellipse or hull. The stats record which input rows they skip, and the
  report maps those back to the layer’s data across facet panels, so the
  count matches what the compile draws. Limits now follow each scale’s
  `oob`: a clipped or censored position, or a censored size, counts
  under `limits`. A second new column, `oob`, counts rows that are
  outside a scale’s `limits` but still drawn (a squished position, or a
  colour, fill, size, alpha or linewidth value that is clamped, given
  the NA colour or extrapolated). Those rows are not part of `dropped`,
  and each layer with any adds its own warning. The pre-compile check
  for a manual or discrete colour palette on a continuous variable is
  now an error, matching the compile, which refuses it (VP-36,
  [\#208](https://github.com/r-vellum/vellumplot/issues/208)).

- **Stat parameters are checked where you write them.** `bins`,
  `binwidth`, `adjust`, `span`, `level`, `height`, `segments` and `n` in
  [`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md),
  [`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_bin2d()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_dotplot()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md),
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md),
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md),
  [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md),
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md),
  [`mark_ellipse()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
  and
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  are validated by the constructor, with an error naming the argument.
  Before, `bins = 0` crashed with “arguments imply differing number of
  rows”, `adjust = 0` with “‘bw’ is not positive”, and `level = 2` or
  `binwidth = -1` drew a wrong plot without complaint (VP-21).

### Bug fixes

- **Violins on a binned axis keep their shape (VP-30).** With
  [`scale_y_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_x_binned.md),
  a violin’s density (and the axis room made for it) was estimated from
  the values snapped to their bin centres, giving a lumpy outline cut
  off at the outer centres. The density now uses the continuous values.

- An aesthetic whose values are all missing no longer draws a 0-1
  colourbar or size legend; a continuous scale with no data and no
  limits draws no legend (VP-11).

- A two-vertex graph on the default stress layout no longer warns “data
  length \[3\] is not a sub-multiple or multiple of the number of rows
  \[2\]”. The warning came from graphlayouts’ fixed two-vertex branch;
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  now returns that same layout itself (VP-40).

- Faceted interactive plots without a `data_id` no longer link marks
  across panels by accident. Their default element keys were row
  positions within each panel’s layer, so they repeated from panel to
  panel, and a hover, click or brush in one panel also lit up the marks
  at the same positions in the others. Default keys in a faceted plot
  are now `"<panel>:<row>"`, using the source row when the layer draws
  one element per input row, and that source row is carried as the
  element’s `join` id so a declarative selection still matches the same
  datum wherever it is drawn. Single-panel plots and declared `data_id`s
  are unchanged (VW-11).

- [`md()`](https://r-vellum.github.io/vellumplot/reference/md.md),
  [`vl_pattern()`](https://r-vellum.github.io/vellumplot/reference/vl_pattern.md),
  [`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md),
  [`linear_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
  and
  [`radial_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
  are now vellum’s own functions re-exported, not wrappers around them.
  They behave the same, but attaching vellum and vellumplot together (as
  [`library(vellumverse)`](https://rdrr.io/r/base/library.html) does) no
  longer reports them as conflicts.
  [`pdf_pages()`](https://r-vellum.github.io/vellumplot/reference/pdf_pages.md)
  and
  [`render_all()`](https://r-vellum.github.io/vellumplot/reference/render_all.md)
  differ from vellum’s (they take plots, and
  [`pdf_pages()`](https://r-vellum.github.io/vellumplot/reference/pdf_pages.md)
  splits a faceted plot into pages), so they stay, and their docs now
  say how they differ (VV-2).

### Internal

- Internal: a trained scale now records its scale type (`continuous`,
  `discrete`, `binned`, `identity`, …) in `type` for every aesthetic,
  where `kind` held the type for colour and position scales and the
  aesthetic name for the rest. The provenance scale-refs report the same
  `type` values as before (R-14).

## vellumplot 0.12.0

### New features

- **[`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_interval.md)
  brushes a data range.** `select_interval(name, encodings = "x")`
  declares an interval selection that a brush fills;
  [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  and
  [`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
  react to it the same way they react to a point selection. Membership
  is by data value: a host such as vellumwidget converts the brushed
  pixels to a data range through the panel’s scale and tests each
  element’s own values, so continuous (including log, sqrt and
  reversed), date, datetime and discrete axes all work, and a brush in
  one facet selects matching rows in every facet. `encodings` takes
  `"x"`, `"y"` or `c("x", "y")`. Plots with an interval selection now
  carry each keyed element’s data values on the brushed channels, and
  the panel axis descriptors gain `aesthetic` and, for discrete axes,
  `levels`. The interaction model records the selection as
  `{name, type: "interval", encodings}`. Static renders are unchanged.

  `encodings` defaults to `c("x", "y")`, the same rectangle brush as
  before. The old `on = "x" / "y" / "xy"` argument still works as an
  alias.

- **Bivariate choropleths with a 2-D legend.**
  [`bi_class()`](https://r-vellum.github.io/vellumplot/reference/bi_class.md)
  bins two numeric columns into `dim` classes each (quantile by default,
  or any
  [`scale_fill_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  style) and adds one `"x-y"` class column. Mapping a fill or colour to
  that column colours it from a bivariate palette and draws the legend
  as a square grid of swatches, with the `x` variable along the bottom,
  the `y` variable up the side, and an arrow on each axis.
  [`scale_fill_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)
  /
  [`scale_color_bivariate()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_bivariate.md)
  choose the palette
  ([`bi_pal()`](https://r-vellum.github.io/vellumplot/reference/bi_pal.md):
  Stevens’ `"DkBlue"`, `"GrPink"`, `"DkViolet"` and `"Brown"`, at 2x2,
  3x3 or 4x4) and the axis titles, and
  [`guide_bivariate()`](https://r-vellum.github.io/vellumplot/reference/guide_bivariate.md)
  retitles the axes or resizes the swatches through
  [`guides()`](https://r-vellum.github.io/vellumplot/reference/guides.md).
  It works on
  [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  polygons and on ordinary marks. The classes are computed over the
  whole table, so facet panels share them. On an interactive plot each
  swatch is keyed to the features in its class, so hovering a swatch
  highlights them.

- **A mapped `linewidth` tapers smoothly along the line
  ([\#165](https://github.com/r-vellum/vellumplot/issues/165)).**
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  and
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  drew a mapped `linewidth` as one segment per span, each at the mean of
  its two endpoint widths, so the width jumped at every vertex and every
  series became a batch of separate path elements. Each series is now
  one `lines_grob(lwd_profile =)` stroke from vellum whose width varies
  linearly between the values at its vertices, so the taper is
  continuous and the series is a single element in
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html),
  [`vellum::element_geometry()`](https://r-vellum.github.io/vellum/reference/element_geometry.html)
  and the SVG, keyed like a constant-width line. A series falls back to
  the old per-segment drawing where a tapered stroke cannot express it:
  a continuous colour (each segment keeps its own colour), a dashed or
  dotted `linetype`, a sketch theme, or a missing width.

- **[`spec_diagnose()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
  reports rows that will not be shown and catches spec mistakes before
  compiling.** vellumplot never deletes rows, so a row with a missing
  position, a value of zero or less on a log axis, or a value outside
  explicit scale limits used to disappear without a word.
  [`spec_diagnose()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
  now also takes a `PlotSpec`, and its result carries a `report` data
  frame with one row per layer counting those rows by reason (`missing`,
  `facet`, `transform`, `limits`). Each layer that loses rows adds a
  warning. Three checks now run before the dry-run compile: a facet
  variable missing from the plot data (an error) or from a layer’s own
  data (a warning, since that layer repeats in every panel); `color` and
  `fill` mapped to a continuous and a discrete variable although they
  share one legend (a named error in place of the generic compile
  failure); and a manual or discrete palette on a continuous colour
  channel, which turns every distinct number into its own category. The
  result now prints as a short summary.

### Bug fixes

- **Date axes take their breaks from the limits.** Default date and
  datetime breaks were picked from the data, so a month of data under
  `limits` of 2019 to 2022 crammed weekly ticks into that month and left
  the rest of the axis bare. Breaks now span the trained range, limits
  included. An all-`NA` Date column no longer aborts with “‘from’ must
  be a finite number”; the axis draws with no ticks, as in ggplot2
  (VP-8).

- **Theme text sizes cascade from `text`.**
  `theme(text = element_text(size = 20))` resized only the axis titles;
  axis text, legend text, strips and the plot title kept their own fixed
  sizes. The complete themes now size every text element relative to the
  `text` root, so the change reaches all of them. New
  [`rel()`](https://r-vellum.github.io/vellumplot/reference/rel.md)
  gives a size as a multiple of the inherited one
  (`theme(axis.text = element_text(size = rel(0.8)))`), and every
  complete theme takes `base_size` and `base_family`, as in ggplot2.
  Default sizes are unchanged. Colours set by a preset (grey axis text,
  for example) still win over a `text` colour, as in ggplot2 (VP-10).

- **A layer’s own data is faceted on the variables it has.** In
  `facet_grid(am ~ cyl)`, an annotation layer whose data had `am` but
  not `cyl` was drawn in all six panels. It now appears only in its `am`
  row, repeated across the `cyl` columns, as in ggplot2. A layer without
  any facet variable is still drawn in every panel. Facet levels are now
  the union over the plot data and every layer’s data, so a level that
  only a layer has gets its own panel instead of being dropped. The
  [`spec_diagnose()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
  warning for a missing facet variable now says which panels the layer
  is repeated in (VP-9).

- **[`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  draws each rule in its own colour.** A mapped `color`, `alpha` or
  `linetype` used to collapse to the first row, so every rule took one
  colour while the legend showed several. The rules are now split into
  style groups, as
  [`mark_abline()`](https://r-vellum.github.io/vellumplot/reference/mark_abline.md)
  styles each line.

- **Repelled labels survive missing values.** `mark_text(repel = TRUE)`
  and `mark_label(repel = TRUE)` crashed when any row had an `NA`
  position or label: the solver skips such labels, so its solution had
  fewer rows than the label grob and the offsets were recycled to the
  wrong length. Incomplete rows are now dropped before the labels are
  drawn, and the solved offsets are applied by element index.

- **Missing values are shown or reported, never dropped silently.** They
  now follow ggplot2 (VP-6):

  - A missing category on a discrete position is a trailing `"NA"`
    level, so its rows are drawn and
    [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
    counts them as an `NA` bar. Before,
    [`sort()`](https://rdrr.io/r/base/sort.html) dropped the level and
    the rows were emitted at `NaN`. Explicit discrete `limits` still
    decide what is shown. Colour, shape and the other discrete scales
    already drew `NA` in their `na_value` with an “NA” legend key, and
    still do.
  - A missing facet value gets its own trailing `"NA"` panel (last on
    each dimension of a grid). Before, its rows vanished:
    `facet_wrap(~cyl)` on data with three missing `cyl` values drew 29
    of 32 rows.
  - A row missing a continuous position (`x`, `y`, `xend`, `ymin`, …) is
    removed before the stat, with one warning per layer, “Removed n rows
    containing missing values”. Provenance no longer lists it as drawn.
    Lines, steps, areas and ribbons keep such rows as a gap, as before.
  - [`spec_diagnose()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
    counts exactly those removed rows as `missing`. Its report loses the
    `facet` column, since a missing facet value is now drawn, and it no
    longer repeats the compile warning.

- **An inset plot no longer resets the outer plot’s interaction
  context.** A sub-plot drawn inside a plot (`annotate("sparkline")`, or
  `annotate("grob")` with a plot) set the shared mark context for itself
  and left it that way, so the outer plot’s later panels lost their
  selection keys,
  [`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
  tags and page size. Each plot now saves the context it was called with
  and restores it when it returns (VP-12).

- **[`position_stack()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  and
  [`position_fill()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  stack like ggplot2.** Positive and negative heights now stack
  separately, away from zero, so `y = c(2, -1, 3)` at one x no longer
  draws overlapping spans. Every row at an x stacks, so bars or areas
  with no `fill`/`group` mapped pile up instead of all starting at zero.
  Grouped areas sampled at different x values are interpolated onto the
  union of their x values (as ggplot2’s `stat_align()` does) and stack;
  before, they were never stacked and overlapped. Under
  [`position_fill()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  the negative stack fills `[-1, 0]`.

  **Behaviour change:** the stack order is reversed to match ggplot2.
  The first group level now sits on top and the last at the bottom, so
  the stack reads in the same order as the legend. Plots that relied on
  the old bottom-up order (including pie slices built from a stacked
  bar) draw their segments in the opposite order. A stacked label with
  `vjust = 1` on a negative bar now sits at its far (lower) end, as in
  ggplot2.

- **Legends merge on title and keys, as in ggplot2.** A colour and a
  size legend merged whenever their titles matched, so two variables
  under one
  [`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md)
  title drew one legend with the size keys painted in the wrong colours.
  Meanwhile colour and linetype mapped to the same variable drew two
  legends. Legends now merge only when they share the title and the same
  keys and labels, and any pair of keyed legends can merge: colour,
  shape, linetype, size, alpha and linewidth. A merged key draws a line
  when a linetype or line width is involved, a point for shape or size,
  or both (VP-7).

- **Provenance carries every trained scale.**
  [`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md)
  records listed scale-refs only for `x`, `y`, colour/fill, `size` and
  `shape`. A mapped `alpha`, `linetype`, `linewidth` or `pattern` now
  carries its scale-ref too, as do the edge colour, alpha and linetype
  channels of a graph. `linewidth` names the line-width scale on a line,
  step or segment and the edge-width scale elsewhere. The aesthetics
  that the scale trainer, the legend builder and provenance each listed
  separately now come from one internal registry.

- **Alt text names the drawn axes under
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md).**
  The automatic description said “It plots y (vertical axis) against x
  (horizontal axis)” even when
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  drew x up the side. It now names the scale on each drawn axis,
  including the implicit “count” axis of a flipped bar chart. The
  description also follows a
  [`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md)
  alias the legend already honoured, so `labs(fill = "Cylinders")` on a
  fill mapping is “colour shows Cylinders” rather than the mapped
  expression. Axes, legends, alt text and the lint summary now read one
  titles record computed once per panel build, so they can no longer
  disagree about a title (VP-11).

## vellumplot 0.11.1

### Bug fixes

- **Violins, ridgelines and half-eyes draw cleanly on log and sqrt
  axes.** The density was estimated on the raw data and its support,
  which [`stats::density()`](https://rdrr.io/r/stats/density.html) pads
  by about three bandwidths, was then mapped through the scale. For data
  near zero that padding dips to or below zero, so a log10 or sqrt axis
  produced NaN outline vertices. The density is now estimated on the
  transformed values, as ggplot2 does, so every vertex is finite and the
  trained axis still covers the drawn shape. On a transformed axis the
  shape is now the density of the transformed values; identity axes draw
  exactly as before.

- **A widened density axis gets ticks over its tails.** The axis is
  widened to fit a violin’s or ridgeline’s tails, but the breaks had
  already been picked for the raw data range, so the tails ran past the
  last labelled tick. The breaks are now re-picked over the widened
  extent. Breaks you set yourself are left alone.

- **Tooltips and data keys follow stats that reorder or summarise
  rows.** `tooltip`, `data_id`, `hover_group`, `hover_color` and
  `selected_color` were still in pre-stat row order after a stat. A dot
  plot sorts its dots, so each dot showed another row’s tooltip and key.
  A bin2d or hex layer that happened to have as many cells as input rows
  keyed every cell with an unrelated row. These values now go through
  the same realignment as aesthetics. A row-preserving stat (dot plot,
  outlier label, window, series label, count) gives each element its own
  row’s value. A summarising stat (bin, bin2d, hex, density, smooth, …)
  keeps a value only if it is constant within each group
  (`tooltip = species` on a per-species density) or across the whole
  layer. Otherwise the value is dropped, because a summary element has
  no single row to take it from. If the `data_id` itself is dropped the
  elements stay unkeyed, but a key that was only the default row index
  is renumbered over the summary elements, so a constant tooltip still
  shows.

- **Provenance names each element’s own rows after a row-preserving
  stat.** A dot plot, outlier label, window or count layer used to key
  every element to the whole layer. Elements now map through the stat’s
  row order back to their own source rows, under facets and for a
  layer’s own `data =` too. A count bubble names every coincident row it
  merges.

- **The colourbar title no longer overlaps the top tick label.** Break
  labels are centred on their ticks, so a label at the end of a vertical
  colour bar hangs half a text line past it. The title band ended
  exactly at the bar top, so in
  `vplot(pressure) |> mark_line(x = temperature, y = pressure, color = pressure)`
  the title “pressure” ran into the “800” label. Vertical
  [`guide_colourbar()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  and
  [`guide_coloursteps()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  bars now leave half a label height above and below the bar, which also
  keeps the bottom label clear of the NA key. A horizontal bar is now
  long enough that adjacent labels never collide: an end label justified
  inward used to sit on top of its neighbour (“600800”).

- **A continuous fill on an area or ribbon errors clearly.**
  [`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  and
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  draw one polygon per series, and a polygon has a single fill. A
  continuous `fill` (or `color`, or `alpha`) that varied along a series
  was grouped like a discrete style, so the band shattered into one
  sliver polygon per vertex and mostly vanished. As in ggplot2 this now
  aborts, saying to map a discrete variable, set a `group`, or use a
  constant fill. A continuous value that is constant within each group
  (one value per `group`) is still drawn, one band per group.

- **`scale_*_brewer()` accepts ColorBrewer palette names.** Only
  [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html)
  names were recognised, so `scale_color_brewer(palette = "Set1")` (or
  `"Accent"`, `"Paired"`, `"Pastel2"`) aborted with “`palette`/`values`
  are not colours or a palette name”, and names shared with hcl
  (`"Blues"`, `"RdBu"`) drew hcl approximations. The brewer and
  distiller scales now draw the real ColorBrewer colours (via
  [`scales::pal_brewer()`](https://scales.r-lib.org/reference/pal_brewer.html))
  for all 35 sequential, diverging and qualitative palettes, sized to
  the number of levels. As in ggplot2 they also take `type` (to pick a
  palette by number) and `direction` (`scale_*_distiller()` defaults to
  `-1`). Other hcl names still pass through, and
  [`scale_color_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  /
  [`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  also fall back to ColorBrewer for names hcl does not know.

## vellumplot 0.11.0

### New features

- **A `group` aesthetic.** Every mark now honours `group`, which says
  which rows form one series without styling them. It was accepted
  before but silently ignored, so a spaghetti plot drew one zig-zag
  through every row:

  ``` r

  d <- data.frame(t = rep(1:5, 3), y = c(1:5, 5:1, rep(3, 5)), id = rep(c("a", "b", "c"), each = 5))
  vplot(d) |> mark_line(x = t, y = y, group = id)   # three lines
  ```

  Grouping now has one rule, used by every stat, position and connected
  mark: an explicit `group` wins (`group = 1` makes the whole layer one
  group); otherwise the first *discrete* of `color`, `fill` groups.
  Connected marks (line, step, area, ribbon, smooth, text path) split by
  group first, then by style within a group. Stats run per group, and a
  grouped stat’s output keeps each group’s own colour, so
  `mark_smooth(group = id, color = treatment)` paints each fit by its
  treatment.
  [`spec_to_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  exports `group` as Vega-Lite’s `detail`.

### Bug fixes

- **Violins, ridgelines and half-eyes fit their axis on transformed
  scales and free facets.** The panel is widened to fit a density mark’s
  drawn footprint, but that footprint was measured in data units and
  merged into a domain that lives in transformed units: a violin on
  `scale_y_continuous(trans = "log10")` stretched the axis up to about
  10^41, and on `trans = "reverse"` the axis came out un-reversed. Free
  facet scales (`facet_wrap(scales = "free_y")`) skipped the widening
  entirely and clipped the density tails. The footprint now goes through
  the scale transform, keeps a reversed axis reversed, and is applied by
  the same code path for shared and free scales.

- **The colour scale now trains on `fill` values even when another layer
  maps `color`.** `color` and `fill` share one colour scale, but `fill`
  values only reached it when no layer mapped `color`. A tile layer
  filled by `z` under a point layer coloured by a subset of `z` trained
  on the points’ range alone and silently squished every tile below it
  onto one end of the ramp; a quantitative `fill` next to a discrete
  `color` got grey for every fill. The scale now pools values from every
  layer mapping either channel, and layers that disagree on whether the
  variable is quantitative abort with an error explaining that they
  share one scale (an explicit scale kind,
  e.g. [`scale_color_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md),
  still settles it).

- **Dodge and stack work on every mark that can use them.** Positions
  used to apply only to bars and areas (and jitter to points), so
  `mark_errorbar(position = "dodge")` drew every error bar on the
  category tick, away from the dodged bars it belonged to, and
  `mark_text(position = "stack")` left labels at their raw heights. A
  shared draw-time step now dodges error bars, line/point ranges,
  cross-bars, text and labels onto exactly the slots dodged bars use,
  and stacking places points, text and labels inside their stacked bar.
  The new `position_stack(vjust =)` / `position_fill(vjust =)` pick
  where in the span a label sits:

  ``` r

  vplot(df) |>
    mark_bar(x = cat, y = n, fill = g) |>
    mark_text(x = cat, y = n, label = n, group = g, position = position_stack(vjust = 0.5))
  ```

- **Box plots, violins and slab-intervals split each category by
  group.**
  `vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg, fill = factor(am))`
  now draws two boxes per cylinder count, side by side, one per `am`. It
  used to draw one box per category, coloured by its first row, while
  the legend listed both levels. Sub-groups present at a category share
  its width (ggplot2’s dodge2), or keep a fixed slot each with
  `position = "dodge"`. Ridgelines overlay their sub-groups on the
  shared baseline.

- **Breaking: on box plots, violins and ridgelines, `color` strokes the
  outline and `fill` fills (ggplot2’s semantics).** A plot that maps
  only `color` used to come out filled in that colour. It now draws
  coloured outlines over the default fill (white boxes, grey violins and
  ridges), and its legend shows outlined keys. Map `fill` to get the old
  look. The outline is no longer hard-coded: it follows `color` and
  defaults to `"grey20"`. Violins and ridges used to be outlined in
  `"grey30"`.

- **Stats that reorder or summarise rows keep their colours aligned.** A
  stat that changes the row count or order used to hand its output the
  pre-stat colour vector, recycled with
  [`rep_len()`](https://rdrr.io/r/base/rep.html). Outputs were then
  painted from unrelated rows:

  - [`mark_dotplot()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
    sorts x, so its dots took the colours of the unsorted rows. With
    `x = c(5, 1, 3, 2, 4)` and `g = c("a", "b", "a", "b", "a")`, the dot
    at `x = 1` came out `"a"`.
  - [`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
    [`mark_bin2d()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
    and
    [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
    /
    [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
    pooled every group into one binning or one kernel density, so
    `mark_contour(color = Species)` drew one KDE of all the points in
    arbitrarily coloured rings.

  Hex and 2-D bins are now counted per group on one shared grid, and 2-D
  densities are estimated per group over a shared window with shared
  levels. A dot keeps its own row’s aesthetics (dots stack by group
  within a bin). The outlier, series-label, count and window stats also
  carry aesthetics by row.

  After a stat, an aesthetic that varies *within* a group has no single
  value for a summarised row. It is now dropped with a warning instead
  of being drawn wrong, which is what ggplot2 does.
  `mark_smooth(color = hp)` therefore fits one uncoloured curve, and
  `mark_bar(x = cyl, fill = hp)` (counted bars with a continuous fill)
  draws plain bars. A stat that leaves an aesthetic it cannot realign is
  caught by an internal consistency check rather than silently recycled.

- **A mark given a position it does not support errors.** `position` is
  now a formal argument of every `mark_*()`. On most marks it used to
  fall into `...` and turn into an inert constant aesthetic, so
  `mark_line(position = "dodge")` silently drew an undodged line. An
  unsupported position now errors, naming the mark and the positions it
  does support.

- **A line or step mapped to a continuous colour draws.**
  `mark_line(color = pressure)` split the line wherever the colour
  changed, which for a continuous colour is every vertex, so each piece
  was a single point and the panel came out empty. A continuous colour
  no longer splits a series: the line stays connected and each segment
  takes the colour of the mean of its two endpoint values (the rule a
  mapped `linewidth` uses). For the same reason a continuous colour no
  longer groups a stat: `mark_smooth(color = hp)` fits one curve, not
  one per distinct `hp` value.

## vellumplot 0.10.0

### New features

- **A flow map’s branches taper smoothly instead of stepping.**
  [`mark_flow_map()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  drew each branch as a series of separate strokes, one per run of equal
  flow, so every width change landed on a join – round caps softened it
  but the staircase was there, and a `steiner` branch gathering
  tributaries could be a dozen strokes. Each branch is now a single
  tapering ribbon, built by the engine at render inside the panel
  viewport (`vellum`’s new `lines_grob(lwd_profile =)`), so the width
  varies continuously and is still a physical width that holds at any
  figure size.

  This only changes the picture for `type = "steiner"`, whose flow is
  per-vertex; a spiral tree’s flow is constant along each branch, so the
  old run-splitting always produced exactly one stroke there anyway.

  A `sketch =` flow map keeps one uniform stroke per branch: a sketched
  ribbon would jitter its outline rather than the pen that drew it,
  which is a different picture, so the two are not combined.

- **`linewidth` is a mappable aesthetic on
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  and
  [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md),
  with a new
  [`scale_linewidth()`](https://r-vellum.github.io/vellumplot/reference/scale_linewidth.md).**
  A line whose width follows the data – a river thickening downstream, a
  weighted flow, a tapering trend – now draws, and gets its own legend.

  ``` r

  vplot(pressure) |>
    mark_line(x = temperature, y = pressure, linewidth = pressure) |>
    scale_linewidth(range = c(0.5, 5))
  ```

  Each **segment** gets one constant width, the mean of its two endpoint
  values, so the width changes in visible steps at the vertices rather
  than tapering smoothly, and round joins do not perfectly fill a sharp
  corner. No backend can stroke the segments of one path at different
  widths in a single call, so a mapped `linewidth` also emits one path
  element per segment in SVG and PDF – worth knowing before mapping it
  over tens of thousands of rows. A **constant** `linewidth =` is
  untouched: still one stroke, byte for byte the same output.

  A missing (`NA`) width takes its segment’s width from whichever
  endpoint is known, so one missing value thins the line rather than
  erasing the two segments that meet at it; a segment with no known
  width at either end is dropped.

  Edges keep their own scale: a mapped `linewidth` on
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  still trains
  [`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md),
  independently of the line-width scale, so one plot can map edge width
  and line width to different variables. A mapped `linewidth` on a line
  therefore no longer draws a stray, purely decorative edge-width
  legend.

- **Breaking, minor: `guides(linewidth = )` and `lims(linewidth = )` now
  address the line-width scale, not the edge-width one.** `linewidth`
  previously canonicalised to `edge_width`, so on a
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  these two shortcuts reached a graph’s edges. Now that lines have their
  own width scale they address that instead, and on a graph they no
  longer affect the edges – silently, since a declared scale for an
  untrained aesthetic is simply unused. Use `guides(edge_width = )` /
  `lims(edge_width = )` for edges;
  [`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)
  was always unambiguous and is unaffected.

- **`guide_colourbar(n.breaks = )` sets roughly how many ticks a
  continuous colour bar carries.** Previously the tick count was
  whatever the break algorithm chose, and the only way to change it was
  to name every break by hand.

  ``` r

  vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    guides(color = guide_colourbar(n.breaks = 4))
  ```

  It is a target, not a promise: the ticks are re-derived over the bar’s
  range and the algorithm prefers round numbers near the count asked
  for, so `n.breaks = 4` may draw 3 or 5 rather than put a tick on
  23.33. An explicit `breaks =` on the scale names the values that get a
  tick and still wins, as does `labels =`, which is paired with those
  breaks.

  There is deliberately no `nbin` argument: the bar is one real gradient
  fill rather than a stack of rectangles approximating one, so it has no
  bands to count. A segmented bar is
  [`guide_coloursteps()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  on a binned scale, where the segments are the scale’s own bins.

### Animation

- **[`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md)
  can now ease each aesthetic on its own curve.** Alongside the
  positional `ease`, it takes `position`, `color` (or `colour`), `size`
  and `alpha`, each naming an easing of its own; anything left unset
  falls back to `ease`, so an existing `ease_aes("cubic-in-out")`
  renders exactly as before.

  ``` r

  # Bars settle into place gently while the fill crossfades at a steady rate.
  ease_aes("cubic-in-out", color = "linear")
  ```

  The classes cover: `position` — x/y and every coordinate-space
  quantity (bar widths, angles, path vertices, rotation); `color` —
  colour and fill; `size` — size, linewidth and radii; `alpha` —
  opacity, **including the fade of marks entering or leaving a state**,
  so easing `alpha` retimes entrances too. `position` also decides
  *when* discrete attributes flip, since those snap at the halfway
  point. x and y share one curve and cannot be split.

### Bug fixes

- **[`spec_from_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)
  reads back a spec that has a coord.** JSON has no integer type, so a
  whole number such as a coord’s default `start = 0` came back from
  `jsonlite` as an integer and `CoordSpec` rejected it – any `coord_*()`
  plot failed to round-trip through
  [`spec_to_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md).
  Coords, the
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md)
  /
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)
  /
  [`motion()`](https://r-vellum.github.io/vellumplot/reference/motion.md)
  effects, scale limits and breaks, and untagged mark params now decode
  whole numbers as doubles, and a round-trip renders the same plot.

- **Provenance row keys point at the right rows under facets and for
  layer data.**
  [`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md),
  [`provenance_join()`](https://r-vellum.github.io/vellumplot/reference/provenance_join.md)
  and
  [`provenance_payload()`](https://r-vellum.github.io/vellumplot/reference/provenance_payload.md)
  numbered rows within each facet panel, so every panel claimed rows
  `1, 2, 3, ...`; and a layer with its own `data =` was keyed into that
  data while the payload shipped the plot’s data. Rows are now indices
  into the whole table the layer reads. Each record carries a `source`
  (`"plot"` or `"layer:<i>"`), and
  [`provenance_payload()`](https://r-vellum.github.io/vellumplot/reference/provenance_payload.md)
  gains a `sources` block with the fields (and, with `values = TRUE`,
  the rows) of every table an element refers to. Its top-level `fields`
  / `data` still describe the plot data. Aggregating stats still key the
  whole layer, which under facets now means the panel’s rows.

- Clearer errors on a few edge cases:
  [`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)
  validates its 0-1 fractional bounds (rejecting inverted / out-of-range
  coordinates that rendered a broken inset);
  [`repeat_()`](https://r-vellum.github.io/vellumplot/reference/repeat_.md)
  with an empty field vector,
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  on an empty graph, and a chord diagram with a negative flow now each
  report the actual problem instead of leaking an internal message.

- **[`mark_raster()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  fails clearly on input it cannot draw.** A categorical x/y aborted
  with an opaque `'min' not meaningful for factors`, and an
  irregularly-spaced grid (e.g. x = 1, 2, 4) rendered with every cell
  silently misplaced. It now errors with a message pointing to
  [`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  in both cases; numeric and evenly-spaced date/time grids are
  unaffected.

- **[`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  no longer silently drops declarative interactivity or animation.** A
  plot carrying `select_*()` /
  [`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
  /
  [`bind_scale()`](https://r-vellum.github.io/vellumplot/reference/bind_scale.md),
  `transition_*()`, or
  [`inspect_source()`](https://r-vellum.github.io/vellumplot/reference/inspect_source.md)
  was serialized as if that state did not exist (leaving, e.g., a
  [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  encoding pointing at a vanished selection). These slots — which hold
  non-serializable tidy-eval quosures — are now **refused** with the
  same classed `vellumplot_unserializable` error as the other
  unsupported state, matching the documented contract.

- **Ordered factors and POSIXct time zones now survive a spec
  round-trip.** An ordered factor came back unordered (so
  [`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md)
  mislabelled it nominal instead of ordinal); a timestamp’s display zone
  was forced to UTC. Both are now preserved (the instant was always
  correct).

- **[`spec_to_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  /
  [`spec_from_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  no longer crash off the happy path.** Any unmapped mark, aesthetic,
  layer param, or scale transform (e.g.
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md),
  a `group` encoding, `trans = "reverse"`) hit a lookup that threw
  `subscript out of bounds` instead of returning `NULL`, defeating the
  bridge’s own fallbacks. The translation now degrades as documented —
  the unsupported feature is dropped and reported in the warning —
  rather than aborting.

- **Constant text labels now render.** `mark_text(label = "...")` (a
  literal, not a mapped column) drew invisible `NA` glyphs; the label is
  now read from the layer params too. A text mark
  ([`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  /
  [`mark_sf_label()`](https://r-vellum.github.io/vellumplot/reference/mark_sf_label.md)
  /
  [`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md))
  with no `label` at all now errors clearly instead of drawing blanks.

- **[`mark_series_label()`](https://r-vellum.github.io/vellumplot/reference/mark_series_label.md)
  on a discrete x** now errors with a clear message (it labels each
  series at its largest x, which a categorical axis has no notion of)
  instead of a cryptic internal failure.

- **[`mark_sf_label()`](https://r-vellum.github.io/vellumplot/reference/mark_sf_label.md)
  skips empty-geometry features** (which have no interior point) rather
  than placing a label at `NA` coordinates.

- `guide_legend(override.aes =)` is now kept when a colour scale’s
  legend merges with its shape/size partner.

- **Colour-guide fixes** (all in the recently-added
  [`guide_colourbar()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  /
  [`guide_coloursteps()`](https://r-vellum.github.io/vellumplot/reference/guides.md)):
  `guide_coloursteps(reverse = TRUE)` no longer mislabels its segments
  (colours and boundary labels now flip together); a **horizontal**
  (`legend.position = "top"`/`"bottom"`) stepped bar or
  `guide_colourbar(barheight =)` no longer overflows its reserved row;
  `barwidth` now sets the bar length on a horizontal legend; a binned
  colour with `NA` shows its NA swatch under
  [`guide_coloursteps()`](https://r-vellum.github.io/vellumplot/reference/guides.md);
  and `label.position` is validated as `"right"`/`"left"` (the only
  positions a vertical bar supports).

- **An empty facet cell no longer crashes a mark with an
  [`after_stat()`](https://r-vellum.github.io/vellumplot/reference/after_stat.md)
  channel.** A
  [`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  layout with an unpopulated cell (or any empty stat input) drawing an
  aggregating mark
  ([`mark_bin2d()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_count()`](https://r-vellum.github.io/vellumplot/reference/mark_count.md),
  [`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md),
  a `mark_histogram(y = after_stat(density))`, …) aborted the whole
  render with `object 'count' not found`; the empty layer now renders as
  a blank panel.

- **[`mcp_serve()`](https://r-vellum.github.io/vellumplot/reference/mcp_serve.md)
  now handles more than one request per process.** The stdin connection
  was left unopened, so each
  [`readLines()`](https://rdrr.io/r/base/readLines.html) reopened it and
  the loop saw end-of-file after the first line — a real MCP client died
  right after `initialize`. The connection is now opened once and held
  across the loop.

- **[`from_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  /
  [`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
  now honour supplied `data` for a spec with no data block**, instead of
  dropping it and failing later in compile with an opaque “argument must
  be coercible to non-negative integer”. A spec that maps fields but has
  (and is given) no data now errors clearly, naming the missing
  references.

- **[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md)
  filters a layer’s own `data =` per keyframe.** A layer with its own
  data (e.g. a per-state annotation) was not subset to the current
  state, so every keyframe drew all states’ rows at once. A layer whose
  data lacks the state column still persists across all frames.

- **`labs(x = NULL)` now blanks the axis title** (ggplot2 parity),
  distinct from omitting the argument. Previously only `labs(x = "")`
  blanked it.

### New features

- `guide_legend(nested = TRUE)` draws a **size** legend as a
  proportional-symbol (“bubble”) legend: concentric, bottom-aligned
  circles with a leader from each to its label, instead of stacked rows.
  Best with a wide size range
  (`scale_size(range = c(2, 12)) |> ... guides(size = guide_legend(nested = TRUE))`).

- [`guide_coloursteps()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  (US alias
  [`guide_colorsteps()`](https://r-vellum.github.io/vellumplot/reference/guides.md))
  draws a **binned** colour scale as a segmented colour bar — the bin
  colours in order, labelled at the numeric boundaries — instead of the
  default discrete swatches. Takes the same `barwidth` / `barheight` /
  `ticks` / `label.position` tunables as
  [`guide_colourbar()`](https://r-vellum.github.io/vellumplot/reference/guides.md):
  `scale_color_binned() |> ... guides(color = guide_coloursteps())`.

- [`guide_colourbar()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  (and the US alias
  [`guide_colorbar()`](https://r-vellum.github.io/vellumplot/reference/guides.md))
  make the continuous colour bar tunable: `barwidth` / `barheight` (bar
  size in mm), `ticks` / `ticks.colour` (the break ticks), and
  `label.position` (`"right"` / `"left"` labels on a vertical bar), on
  top of the `title` / `reverse` that
  [`guide_legend()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  already offered for a colour bar. For example
  `guides(color = guide_colourbar(barheight = 60, ticks = FALSE))`.

- [`mark_sf_label()`](https://r-vellum.github.io/vellumplot/reference/mark_sf_label.md)
  labels each `sf` feature at its **interior point**
  ([`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html),
  always inside the polygon), reprojected through the same
  [`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)
  CRS as
  [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  so the labels land on the geometry. Repels crowded labels apart. Layer
  it over a
  [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  choropleth to name the regions:
  `vplot(nc) |> mark_sf(fill = AREA) |> mark_sf_label(label = NAME)`.

- [`guide_legend()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  gained an **`override.aes`** argument (ggplot2 parity): a named list
  of aesthetics forced on the legend **keys** only, independent of the
  plotted data —
  e.g. `guides(color = guide_legend(override.aes = list(size = 5, alpha = 1)))`
  draws large, opaque keys over a scatter of tiny translucent points.
  Recognised: `size`, `alpha`, `colour`/`color`, `fill`, `shape`,
  `linewidth`. The key cell grows to hold an overridden size.

- [`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  /
  [`scale_y_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  gained an **`expand`** argument controlling axis padding: `NULL`
  (default) keeps the usual 5% margin, and a numeric `c(mult, add)` sets
  a proportion of the data range plus a constant in data units —
  e.g. `expand = c(0, 0)` clamps the axis exactly to the data.

- [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)
  now **sketch-ifies the plot’s current theme** instead of resetting to
  grey, so a self-contained chart that is deliberately axis-free
  ([`vwaffle()`](https://r-vellum.github.io/vellumplot/reference/vwaffle.md)
  /
  [`vvenn()`](https://r-vellum.github.io/vellumplot/reference/vvenn.md)
  /
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  / …) is no longer given back a grid and axes when hand-drawn. An
  ordinary axis chart is unchanged.

- **Tagged-PDF alt text no longer leaks internal identifiers.** A tagged
  PDF used to make every mark its own `Figure` whose `Alt` was the
  mark’s provenance id (`layer-1-line-g1`) — or, for a
  `mark_text(repel = TRUE)` label, its internal `vl_place` handle
  (`repel:panel-1-1:2:0`) — so a screen reader announced those strings,
  one per mark. Marks are now tagged as **artifacts**
  (`role = "presentation"`), leaving a single `Figure` that carries the
  [`plot_alt()`](https://r-vellum.github.io/vellumplot/reference/plot_alt.md)
  description. Provenance (`data-vellum-id`) and interactivity
  (`data-key`) are unchanged. Needs vellum ≥ 0.6.8 (which keeps the
  figure for an all-artifact described scene). Reported in
  [\#145](https://github.com/r-vellum/vellumplot/issues/145).

### New features

- [`mark_outlier_label()`](https://r-vellum.github.io/vellumplot/reference/mark_outlier_label.md)
  — label only the **outliers**, not every point. Keeps the rows whose
  `y` is an outlier (Tukey’s IQR rule by default, or `method = "sd"`,
  detected per colour/fill group) and labels just those, repelled apart.
  Map a `label` to name each; with none mapped, the outlier’s `y` value
  is the label.

- [`mark_series_label()`](https://r-vellum.github.io/vellumplot/reference/mark_series_label.md)
  — **direct labels**: name each colour series at its line end instead
  of in a legend (the direct-labels idiom). Maps the same
  `x`/`y`/`color` as the lines; the label text and colour come from the
  series, and the labels are pushed apart with the same repel solver as
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md).
  Give the panel a little x-room
  ([`xlim()`](https://r-vellum.github.io/vellumplot/reference/lims.md))
  and drop the legend (`guides(color = "none")`).

- [`mark_signif()`](https://r-vellum.github.io/vellumplot/reference/mark_signif.md)
  — significance brackets (the ggsignif / ggpubr idiom): run a pairwise
  test (`wilcox.test` or `t.test`) between `x` groups and draw a bracket
  with its p-value (or stars) over each comparison. Brackets stack above
  the data and the y-axis expands to fit them. Add on top of a
  [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  /
  [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md).

- [`scale_color_steps()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  /
  [`scale_fill_steps()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  — ggplot2-parity aliases of the binned colour scales (a continuous
  variable cut into steps).

- **Adaptive knit output.** When a plot is knitted (Quarto / R Markdown)
  to an HTML target it is now emitted as a crisp, selectable,
  resolution-independent inline **SVG**; other targets (LaTeX / Word)
  keep the default device render. One plot object, the best format per
  output — no conditional code.

- [`plot_data_uri()`](https://r-vellum.github.io/vellumplot/reference/plot_data_uri.md)
  encodes a plot as a self-contained `data:` URI (vector SVG by default,
  or `format = "png"`) — ready for an HTML `<img src>`, a Markdown
  image, an email, or a `gt` table’s non-HTML backends.

- Three more complete themes for ggplot2 parity:
  [`theme_light()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_dark()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  and
  [`theme_linedraw()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md).

- Named palette scale constructors (thin wrappers over `palette =`,
  easing migration): `scale_*_viridis_c()`/`_d()` (the viridis maps —
  viridis/plasma/inferno/cividis/rocket/mako), `scale_*_brewer()`
  (discrete) and `scale_*_distiller()` (continuous) ColorBrewer-style
  palettes, and `scale_*_gradientn()` for an arbitrary n-stop colour
  ramp.

- [`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  /
  [`scale_fill_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  gained a `limits =` argument (the direct form of `lims(color = ...)`).

- The named graph-layout registry gained `gem`, `graphopt`, `dh`, and
  `lgl` (igraph force-directed layouts, now reachable by name).

- [`vwaffle()`](https://r-vellum.github.io/vellumplot/reference/vwaffle.md)
  — a self-contained **waffle chart** (square pie): a grid of cells
  coloured by category, each category taking a share of the cells
  proportional to its count (largest-remainder allocation, so the cells
  sum exactly). Works from a `value` column or raw row counts.

- [`position_sina()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  (and `position = "sina"`) spreads a category’s points along x by a
  quasirandom offset scaled to the local y-density, so the cloud traces
  the distribution’s shape (ggforce’s sina).

- [`mark_raincloud()`](https://r-vellum.github.io/vellumplot/reference/mark_raincloud.md)
  composes a **raincloud** — a one-sided density cloud
  ([`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md))
  with the raw observations as sina “rain” below it — in one call,
  forwarding any `color`/`fill` mapping to both layers.

- New reference and summary marks (ggplot2 parity):

  - [`mark_abline()`](https://r-vellum.github.io/vellumplot/reference/mark_abline.md)
    — a sloped reference line `y = slope * x + intercept` across the
    panel (`slope`/`intercept` may be vectors for several lines).
  - [`mark_function()`](https://r-vellum.github.io/vellumplot/reference/mark_abline.md)
    — a curve `y = fun(x)` sampled over the panel’s x range,
    e.g. `mark_function(fun = dnorm)` over a density histogram.
  - [`mark_pointrange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
    /
    [`mark_crossbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
    — identity summary marks (a point or a box with `ymin`/`ymax`, the
    non-aggregating counterparts of
    [`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)).
  - [`mark_count()`](https://r-vellum.github.io/vellumplot/reference/mark_count.md)
    — collapse coincident `(x, y)` points to one bubble sized by overlap
    count (`geom_count()` / `stat_sum()`); `size` defaults to
    `after_stat(n)`.

### Internal

- Performance and cleanup with no change to rendered output: the boxplot
  / violin / half-eye emitters group rows by category once instead of
  rescanning per level; the `sf` point path and the circle-pack /
  sunburst hierarchy layouts drop quadratic accumulation and repeated
  ancestor walks; the horizontal colour-bar computes each break position
  once.

### Bug fixes

- Interop round-trip fidelity: an integer stat parameter (e.g.
  `mark_histogram(bins = 20L)`) now survives
  [`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)/[`from_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  and JSON as an integer instead of silently widening to a double (which
  changed the spec hash). And a Vega-Lite export where two aesthetics
  map onto one channel (e.g. `color` and `fill`) now reports the dropped
  encoding rather than silently overwriting it.

- Clearer, earlier input validation across several surfaces:
  `scale_*_binned()` rejects a non-positive or non-scalar `n` and a
  non-scalar `style`; a numeric scale `range` rejects a non-finite
  bound; `coord_fixed(ratio=)`, `coord_polar(start=)`, and
  `coord_radial(start=/end=)` reject non-scalar or non-finite values;
  the pattern constructors reject a non-positive
  `spacing`/`size`/`linewidth`; `mark_rug(sides=)` and a rolling
  `window`’s `k` are validated; and an MCP `tools/call` with no tool
  name returns a clean error. Each previously failed late with an opaque
  message (or, for `set_mask(region=)`, silently ignored the argument —
  now rejected).

- A discrete position axis no longer draws spurious minor gridlines
  between and outside its categories.

- Legends with mixed titled and untitled guides no longer misalign: a
  guide with no title reserves no title band.

- An `NA` in a facet variable now consistently drops the row (matching
  ggplot2) rather than creating a spurious `"NA"` panel for
  character/multi-variable facets.

- Stacked bars/areas tolerate an `NA` height (dropped from the stack)
  instead of poisoning the whole group’s cumulative total.

- A non-finite scale break no longer anchors a stray gridline, tick, or
  axis label.

- Serializing a preset theme carrying an `element_*()` override now
  warns that the customisation is dropped (previously suppressed
  whenever a preset was set).

- [`clip_to()`](https://r-vellum.github.io/vellumplot/reference/clip.md)
  rejects a region vertex whose `group` is `NA` (it was silently
  dropped, deforming the clip).

- Several marks no longer silently ignore aesthetics they document:
  `alpha` now applies to
  [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)/[`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md),
  and the
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  band;
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  honours `linewidth` for its fitted line; and the text marks
  ([`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md),
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md),
  [`mark_text_path()`](https://r-vellum.github.io/vellumplot/reference/mark_text_path.md))
  honour a mapped `size` (trained through a size scale, as points
  already were).

- [`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  no longer aborts when a row/column combination has no data: a
  structurally-empty panel (e.g. a histogram cell with no observations)
  now renders blank instead of killing the whole plot.

- One-sided continuous limits work as in ggplot2: `ylim(0, NA)` /
  `scale_*_continuous(limits = c(NA, hi))` now pin the supplied end and
  train the open end from the data, rather than silently collapsing the
  axis.

- Numeric variables drawn on a discrete axis
  (e.g. `mark_bar(x = a_number)`) order their categories numerically
  (`1, 2, 10`) instead of lexicographically (`1, 10, 2`).

- Overlaying two layers on one discrete axis, one mapping a `factor` and
  one a `character`, no longer drops or mislabels categories.

- A pie / donut
  ([`mark_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md),
  [`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md))
  with a constant or short `x` no longer errors.

- [`mark_edge_bundle()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  and
  [`mark_flow_map()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  now accept their documented `effects =` argument (glow / shadow /
  outline) instead of rejecting every effect.

- `layout = "dendrogram"` reports a clear “needs a connected tree” error
  on a disconnected graph instead of an opaque internal failure.

- [`vtable()`](https://r-vellum.github.io/vellumplot/reference/vtable.md)
  renders a blank cell for a too-short or empty sparkline series instead
  of aborting the whole table.

- Non-looping
  [`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md)
  /
  [`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md)
  animations now reach their final keyframe instead of stopping roughly
  one frame short.

- Faceting on a column whose name is not a syntactic R name (e.g.
  `"Miles per Gallon"`, common after a Vega-Lite import) round-trips
  through
  [`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  /
  [`from_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  instead of crashing.

- **One variable colouring both nodes and edges now draws a single
  legend.** When a node/text colour and an edge colour resolve to the
  same scale — same title, levels, and palette, as in `vdendrogram(k =)`
  — the redundant edge guide folds into the node colour guide instead of
  drawing a second, identical legend beside it. Genuinely different node
  and edge colour scales still keep their own guides
  ([\#94](https://github.com/r-vellum/vellumplot/issues/94)).

- **Requires vellum \>= 0.6.8**, and
  [`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)
  gained everything the linter gained there: 13 new rules, two of which
  vellumplot could not have had on its own — colours a colour-blind
  reader cannot tell apart, and characters no font on the running
  machine can draw. Every
  [`vellum::vl_lint()`](https://r-vellum.github.io/vellum/reference/vl_lint.html)
  argument now comes through — `exclude` to accept a finding you have
  already judged, `cvd` to pick which deficiency to check, `rules` to
  run one rule alone, and `min_text_pt` for a print-resolution
  legibility floor.

- **Breaking, minor:
  [`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)’s
  thresholds must be named.** `...` now sits after `x`, so the optional
  arguments follow it — `plot_lint(p, 5)` used to mean `min_text_px = 5`
  and now fails, because the `5` reaches the engine as a rule name.
  Write `plot_lint(p, min_text_px = 5)`. This is the usual convention
  for arguments after `...`, and it is what makes `cvd =`/`exclude =`
  reach the engine.

- **Findings carry the box they refer to**, so
  [`vellum::vl_lint_overlay()`](https://r-vellum.github.io/vellum/reference/vl_lint_overlay.html)
  can draw the report onto the plot rather than describing it, and
  [`vellum::vl_lint_assert()`](https://r-vellum.github.io/vellum/reference/vl_lint_assert.html)
  can fail a test suite on it.
  [`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)
  therefore returns four more columns (`x0`, `y0`, `x1`, `y1`); a
  grammar finding is about a scale rather than a node, so its box is
  `NA`.

- **The grammar rules are registered in the engine’s rule registry**
  instead of being stitched on afterwards.
  [`vellum::vl_lint_rules()`](https://r-vellum.github.io/vellum/reference/vl_lint_rule.html)
  lists `single_level_scale` and `legend_overflow` with a `grammar` tag,
  `rules =` selects them, and a plain
  [`vellum::vl_lint()`](https://r-vellum.github.io/vellum/reference/vl_lint.html)
  of a compiled plot now reports encoding problems alongside geometric
  ones —
  [`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)
  is a thin wrapper rather than a second implementation.

  A registry rule is handed the resolved scene, which is not enough for
  a rule about the encoding, so a compiled scene carries a summary of
  its trained scales: the `kind` and `levels` of the legend-bearing
  ones, a couple of kilobytes, computed where the compile had already
  built them. Deliberately a summary and not the plot spec, which for
  50,000 rows runs past a megabyte and would keep the data alive for as
  long as anyone held the scene. It leaves the scene’s hash, serialised
  form and pixels untouched. A composition or table carries none — there
  is no one set of scales to report on — so those get the geometric
  findings only
  ([\#147](https://github.com/r-vellum/vellumplot/issues/147)).

- **Requires vellum \>= 0.6.7**, which fixes animated SVG output:
  duplicate `<defs>` ids across frames made every frame’s clip resolve
  to the first frame’s — hidden for all but one frame of the cycle, and
  a hidden `<clipPath>` child clips everything away — so
  `anim_save(".svg")` blinked once and then showed an empty panel. The
  same release also fixes frames playing in reverse.

- **[`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  closes bands that leave the grid.** A density band whose contour exits
  the estimation grid was closed with a straight chord back to its
  start, slicing triangular wedges across the panel (e.g.
  `mark_contour_filled(x = eruptions, y = waiting)` on `faithful`). Open
  contours are now closed along the grid boundary, so each band fills
  the correct region.

- **[`mark_scalebar()`](https://r-vellum.github.io/vellumplot/reference/map_decorations.md)
  labels read left-to-right in every corner.** With a right-anchored
  `position` (`"bottomright"` / `"topright"`) the `0` and distance
  labels were mirrored, putting the distance on the left. They now
  always read `0` at the left end and the distance at the right.

- **[`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  no longer paints a constant `color` as the fill.** With both a
  constant `fill` and a constant `color`, the polygon fill took the
  stroke colour and the outline vanished. Fill and stroke are now
  resolved as the distinct channels they are: `fill` fills, `color`
  strokes.

## vellumplot 0.9.0

First tagged release. Everything below is included.

- **Merged choropleth regions.** `mark_sf(merge = TRUE)` dissolves
  adjacent features that share a fill into one region, so the internal
  borders between same-valued features disappear and each region is a
  single crisp path — no double-stroked seams, and exact in PDF. The
  union is real boolean geometry
  ([`vellum::vl_path_op()`](https://r-vellum.github.io/vellum/reference/vl_path_op.html)),
  holes and multipart features preserved. It is ignored on an
  interactive layer, where per-feature keys must survive.

- **Rotated and wrapping axis tick labels.** `axis.text.x` /
  `axis.text.y` now honour an `angle` —
  `theme(axis.text.x = element_text(angle = 45))` slants the labels and
  the gutter reserves the rotated height (an explicit `hjust`/`vjust`
  sets the anchor; otherwise the end of the run is pinned at the tick).
  When the labels are left horizontal, a long discrete label that would
  overrun its tick spacing wraps onto multiple lines instead of
  colliding with its neighbour, and the label row grows to fit — the
  per-tick companion of the title/subtitle/ caption wrapping already
  shipped. Labels that already fit are unchanged.

- **Multi-page PDFs and parallel batch export.**
  [`pdf_pages()`](https://r-vellum.github.io/vellumplot/reference/pdf_pages.md)
  writes several plots into one PDF (a report or slide deck), one plot
  per page — pages may differ in size and each keeps its accessibility
  tags — or splits a single faceted plot into one page per facet cell.
  [`render_all()`](https://r-vellum.github.io/vellumplot/reference/render_all.md)
  renders a list of plots to separate files across CPU cores (small
  multiples, batch export); it is byte-identical to rendering them one
  by one and parallelises via process forks on macOS/Linux (sequential
  on Windows). Both build on the engine’s
  [`vellum::pdf_pages()`](https://r-vellum.github.io/vellum/reference/pdf_pages.html)
  /
  [`vellum::render_all()`](https://r-vellum.github.io/vellum/reference/render_all.html).

- **Real blur effects;
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md)/[`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)
  now work on text.**
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md)
  and
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)
  are drawn with the engine’s real Gaussian blur
  (`vellum::vl_viewport(blur=)`) — one softened, composited layer
  instead of a stack of N widened low-opacity copies. It is cheaper and
  smoother, and it lifts the old restriction that text could not glow:
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  /
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  now take an `effects =` argument and accept
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md)
  and
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)
  (a *sharp*
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md)
  still needs a glyph outline the text primitive can’t stroke).
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md),
  [`motion()`](https://r-vellum.github.io/vellumplot/reference/motion.md),
  and
  [`echo()`](https://r-vellum.github.io/vellumplot/reference/motion.md)
  are unchanged. The
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md)
  `layers` / `blend` arguments are kept for the neon look (opacity +
  composite) but no longer stack copies.

- **Relative text sizing and tabular figures.** `element_text(cex = )`
  sets a size multiplier that cascades through the theme (a base size
  scaled, rather than an absolute size at every call site). Axis tick
  labels are now set with tabular figures (`tnum`), so digit columns
  keep a constant width and stop jittering from tick to tick.

  Requires vellum \>= 0.6.4.

- **Venn / Euler diagrams:
  [`vvenn()`](https://r-vellum.github.io/vellumplot/reference/vvenn.md).**
  Draws 2 or 3 overlapping sets whose disjoint regions are rendered as
  **solid geometry** — computed with the engine’s boolean set operations
  ([`vellum::vl_path_op()`](https://r-vellum.github.io/vellum/reference/vl_path_op.html)),
  so overlapping regions do not alpha-composite and stay crisp,
  including in PDF where a rasterised mask degrades. Each region is
  filled by how many elements fall in exactly that combination of sets,
  and labelled with that count. Input is a named list of member vectors
  or a data frame of logical membership columns. Requires vellum \>=
  0.6.4.

- **SVG icon markers: `shape = <svg>`.** A point `shape` can now be an
  SVG icon — a path `d` string (what icon sets ship) or a path to a
  `.svg` file — drawn as a crisp vector marker via
  [`vellum::svg_grob()`](https://r-vellum.github.io/vellum/reference/vl_svg_path.html)
  instead of a built-in glyph. Pass a literal `d`/file for a constant
  icon, or map `shape` and give `scale_shape(values = )` one icon per
  level (the legend shows the icons); the `size` aesthetic scales them.
  The vector marker stays sharp at any zoom and in PDF, replacing the
  ggimage/ggsvg raster-per-point hacks. (Per-icon interactivity via
  `data_id` is not yet wired — the icons render on every backend.)
  Requires vellum \>= 0.6.4.

- **An accessibility pass.** Four pieces that make a plot legible for
  colour-blind and low-vision readers, and reproducible across machines:

  - `render_plot(cvd = )` renders a `.png` through a
    colour-vision-deficiency simulation (`"protanopia"`,
    `"deuteranopia"`, `"tritanopia"`, `"achromatopsia"`) so you can
    *see* a palette fail.
  - [`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)
    flags the problems a green test suite hides: text below a legibility
    floor, colour contrast under the WCAG threshold, off-canvas or
    overlapping labels (geometric rules from the engine), plus
    grammar-level mistakes — a single-level encoding or a legend too
    long to read.
  - [`pattern_hatch()`](https://r-vellum.github.io/vellumplot/reference/pattern_hatch.md)
    adds a crisp **vector** hatch fill (via
    [`vellum::vl_hatch()`](https://r-vellum.github.io/vellum/reference/vl_hatch.html))
    that stays sharp in PDF and survives greyscale — a redundant,
    non-colour encoding, usable as a constant `fill` or a
    [`scale_pattern()`](https://r-vellum.github.io/vellumplot/reference/scale_pattern.md)
    value.
  - **Tagged PDF** now marks furniture (gridlines, ticks, panel/plot
    backgrounds) as decorative artifacts, so a screen reader skips them
    instead of reading every gridline; the figure-level alt text
    ([`plot_alt()`](https://r-vellum.github.io/vellumplot/reference/plot_alt.md))
    carries the meaning.
  - [`plot_manifest()`](https://r-vellum.github.io/vellumplot/reference/plot_manifest.md)
    fingerprints the resolved **fonts** and
    [`plot_verify()`](https://r-vellum.github.io/vellumplot/reference/plot_verify.md)
    reports a font mismatch as a **distinct outcome** (`fonts_ok`) from
    a data mismatch (`data_ok`) — a pixel difference from a missing font
    is a different cause, and fix, than a changed dataset.

  Requires vellum \>= 0.6.5.

- **[`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  traces in the engine; `isoband` is no longer needed.** 2-D contour
  lines and filled contours are now traced by
  [`vellum::vl_contour()`](https://r-vellum.github.io/vellum/reference/vl_contour.html)
  (marching squares) instead of the `isoband` package, which is dropped
  from `Suggests`.
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  draws each contour as its own chained polyline (closed loops are
  closed), and
  [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  paints the closed rings in level order for the layered filled-density
  look. A density field still uses
  [`MASS::kde2d()`](https://rdrr.io/pkg/MASS/man/kde2d.html); contouring
  a supplied `z` surface needs nothing extra. Requires vellum \>= 0.6.4.

- **Label repulsion works on every panel type.**
  `mark_text(repel = TRUE)` / `mark_label(repel = TRUE)` now go through
  the engine placement solver
  ([`vellum::vl_repel()`](https://r-vellum.github.io/vellum/reference/vl_place.html)),
  which solves in device pixels and applies the answer as an absolute
  millimetre offset. It is a single post-compile pass — no second
  compile — and it is coordinate-agnostic: **faceted, polar and warped
  (`coord_trans`) plots repel correctly**, where before they errored
  with “single cartesian panel only”. Labels in a faceted plot are
  solved per panel and kept inside it. The `seed` argument is now inert
  (the solver is deterministic) and is kept only for back-compatibility.
  Requires vellum \>= 0.6.4.

- **[`mark_text_path()`](https://r-vellum.github.io/vellumplot/reference/mark_text_path.md)
  — labels that follow a curve.** A new mark that sets one label per
  group along the group’s `x`/`y` path, each glyph rotated to the local
  tangent — for labelling a contour, an arc, or a trend line directly
  instead of with a legend. Split runs the usual way (a discrete `color`
  or a distinct `label` starts a new run); `hjust` slides the run along
  the path, `offset` lifts it off the baseline. Emits real, selectable
  `<text>` in SVG. Glyphs follow the tangent, so a path walked
  right-to-left sets its label upside-down — reverse the path to flip
  it. Requires vellum \>= 0.6.4.

- **Long plot titles, subtitles and captions wrap instead of
  overflowing.** A title / subtitle / caption longer than the page now
  wraps onto as many lines as it needs, and the layout reserves the
  height for them (previously the text ran straight off the right edge).
  Lines are aligned to the band’s `hjust` (left / centre / right), and
  short single-line labels are unchanged. Wrapping is to the page
  content width, measured in millimetres at compile time. Requires
  vellum \>= 0.6.4 (which fixes `grobheight()` to account for the
  wrapping when it sizes the band’s track).

- **Keys on more marks.** Now that the engine can key lines, polygons
  and text (vellum 0.6.x), the marks that could not previously be made
  interactive are:
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  /
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  (a series is one hoverable object; a colour mapping gives one keyed
  line per series),
  [`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  /
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  (the filled band as one polygon),
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  (each data label individually), and the sf polygon / choropleth marks
  (one keyed feature each). This release also fixes
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md),
  whose rounded background box is now keyed as the label’s hit target —
  it was inert before — and
  [`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  /
  [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md),
  whose interval bars now carry the datum key like their slab and centre
  point already did. Set `data_id=` (and optionally `tooltip=`) on the
  mark to make it addressable; a mark left without a key stays out of
  `scene_model()`, so a plot’s furniture never becomes phantom
  interactive elements. Requires vellum \>= 0.6.3.

- **Portable plot specs.** A plot is now a serializable *document*:
  [`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  turns a `PlotSpec` into a plain nested list and
  [`from_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  rebuilds it, with
  [`spec_to_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)
  /
  [`spec_from_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)
  for the JSON wire format and
  [`spec_schema()`](https://r-vellum.github.io/vellumplot/reference/spec_schema.md)
  for the bundled JSON Schema. The serializer covers the encoding-level
  grammar exactly and **refuses** (with a classed
  `vellumplot_unserializable` error) anything a portable document cannot
  carry — custom transform functions, paint/pattern fills, sketch
  geometry, secondary axes — rather than dropping it silently.

- **LLM- / agent-native plotting.**
  [`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md)
  summarises a data frame’s columns and inferred encoding types to
  ground a model;
  [`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
  /
  [`spec_diagnose()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
  validate a generated spec against the data and return *structured*
  diagnostics (unknown field with a suggestion, compile error) instead
  of a traceback; and
  [`mcp_serve()`](https://r-vellum.github.io/vellumplot/reference/mcp_serve.md)
  runs a pure-R Model Context Protocol server (bundled launcher at
  `system.file("mcp/server.R")`) exposing `get_schema` / `list_fields` /
  `render_spec` tools so an agent generates *validated data*, never
  executed code.
  [`vplot_ask()`](https://r-vellum.github.io/vellumplot/reference/vplot_ask.md)
  is a model-agnostic natural-language convenience over the three.

- **Vega-Lite interoperability.**
  [`spec_to_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  and
  [`spec_from_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  translate a plot to and from a Vega-Lite specification over a
  documented subset (marks, encodings, scales, `bin`/`count`, faceting,
  inline data, title), reporting any features they cannot map rather
  than diverging silently.

- **Click-to-source.**
  [`inspect_source()`](https://r-vellum.github.io/vellumplot/reference/inspect_source.md)
  declares (on the plot, not as a host flag) that a host should surface
  the source data rows behind a clicked or hovered element; it is
  carried in
  [`interaction_model()`](https://r-vellum.github.io/vellumplot/reference/interaction_model.md)
  and enacted by a host reading the scene’s provenance
  ([`provenance_payload()`](https://r-vellum.github.io/vellumplot/reference/provenance_payload.md)).

- **Self-documenting, reproducible plots.**
  [`provenance_join()`](https://r-vellum.github.io/vellumplot/reference/provenance_join.md)
  ties every drawn element to both its source data rows and its
  device-pixel geometry;
  [`provenance_payload()`](https://r-vellum.github.io/vellumplot/reference/provenance_payload.md)
  exposes the same as a click-to-source payload for a host; and
  [`plot_manifest()`](https://r-vellum.github.io/vellumplot/reference/plot_manifest.md) +
  `plot_svg(manifest = TRUE)` +
  [`plot_verify()`](https://r-vellum.github.io/vellumplot/reference/plot_verify.md)
  embed a data fingerprint in an SVG so a figure can be checked against
  the data it was drawn from.

- **Map decorations.** `coord_sf(graticule = TRUE)` draws meridians and
  parallels behind the map; because they are generated in
  longitude/latitude and reprojected, they curve correctly under a
  projected CRS (a straight grid would be wrong).
  [`mark_scalebar()`](https://r-vellum.github.io/vellumplot/reference/map_decorations.md)
  adds a segmented distance bar that sizes itself from the map’s CRS
  (units `"km"`/`"m"`/`"mi"`/`"ft"`), and
  [`mark_compass()`](https://r-vellum.github.io/vellumplot/reference/map_decorations.md)
  adds a north arrow; both are pinned to a panel corner via `position`.
  The scale bar and compass need no `sf` themselves, but do require a
  map coordinate system.

- **Keyframe animation.**
  [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md)
  turns a plot into an animation over the levels of a column;
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  compiles one keyframe scene per state — training the scales **once
  over all states and freezing them**, so nothing retrains between
  frames (non-reactive keyframe animation) — and
  [`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
  tweens and encodes the in-between frames to a looping GIF, an animated
  PNG, or an **animated SVG** (`.svg` — resolution-independent, honours
  `prefers-reduced-motion`;
  [`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
  advises a raster format when a `.svg` scene is dense enough that one
  would be smaller) in one parallel, streaming pass in vellum’s Rust
  backend.
  [`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md)
  sets the easing (`linear`, or a family like
  `cubic`/`sine`/`elastic`/`bounce` with an `-in`/`-out`/`-in-out`
  direction). Position, size, alpha and colour interpolate (colour
  perceptually, in Oklab); discrete attributes snap.
  [`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md)
  is the continuous-time variant — it allocates frames in proportion to
  the time gaps, so unevenly spaced times play at a constant rate.
  Giving an animated mark a `data_id` enables per-element
  **enter/exit**: elements that appear fade in and elements that leave
  fade out (matched elements tween as usual); without it a stable
  element set is assumed.
  [`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md)
  wipes the plot into view left to right (the “line draws itself”
  animation) by growing a clip rectangle over a single compile. See the
  *Animation* article and `inst/examples/28-animation.R`. Requires the
  development version of vellum (`vl_render_animation()`).

## vellumplot 0.8.0

- **Flow maps:
  [`mark_flow_map()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md).**
  A one-to-many flow map on a
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  plot: a single `root` fans out to every destination along smooth,
  merging branches whose width tracks the flow volume (the Minard /
  migration-map idiom). `type = "spiral"` (default) builds an
  angle-restricted spiral tree from the package; `type = "steiner"`
  builds an approximate Steiner tree (additionally needs ). Edge
  `weight` drives the flow, which is mapped onto `width_range`. Needs
  the suggested package.

- **Edge bundling:
  [`mark_edge_bundle()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md).**
  A drop-in alternative to
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  that routes a graph’s edges as bundled curves instead of straight
  lines, so a dense node-link hairball collapses into a few legible
  trunks. `type` selects the algorithm (`"force"`, `"divided"`,
  `"stub"`, `"path"`, `"hammer"`, `"mingle"`) and `params` passes tuning
  through to the package, which computes the geometry; vellumplot draws
  the returned paths with the usual edge aesthetics (faint by default so
  overlapping trunks read as density). Needs the suggested package
  (`>= 1.0.0`).

- **Chord diagrams:
  [`vchord()`](https://r-vellum.github.io/vellumplot/reference/vchord.md).**
  Wraps a weighted flow (a `from`/`to`/`value` list or a square flow
  matrix) onto a circle – one arc per node, one ribbon per flow through
  the centre. Directed: each node’s arc splits into an outgoing then an
  incoming block, so `a -> b` and `b -> a` are distinct ribbons and
  self-flows loop within a node’s sector. `direction` shows a ribbon’s
  direction by fading it from source to target (`"gradient"`, default),
  stopping it short of the target (`"gap"`), or `"both"`. `sort`, `gap`,
  and `link_color` echo
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md);
  [`mark_chord()`](https://r-vellum.github.io/vellumplot/reference/vchord.md)
  is the exported layer.

- **Removed the unused `max_overlaps` argument** from
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md),
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md),
  and
  [`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md).
  It was reserved for a never-implemented overlap cap and did nothing;
  drop it from any call.

- **More robust handling of degenerate and extreme inputs.** A single
  distinct value on an axis under
  [`coord_fixed()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  /
  [`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)
  no longer collapses the panel to nothing; a map centred on a pole no
  longer gets a runaway aspect ratio; a circle-pack of collinear
  equal-value leaves no longer produces `NaN` coordinates; and a
  pathologically deep hierarchy or dendrogram aborts with a clear “too
  deep” message instead of overflowing the call stack.

- **`mark_line(window = )` requires a positive integer `k`.** A zero,
  negative, or fractional window size now errors clearly instead of
  silently producing a wrong (or empty) result.

- **A computed-constant `if_false` in
  [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  is carried into the interaction model.** A negative literal
  (e.g. `-0.5`) or a computed constant (e.g. `grey(0.5)`) was previously
  dropped; only genuine per-row column references are now excluded (and
  carried per element instead).

- **A faceted layer whose own data lacks the facet variable still draws
  on every panel**, but a genuine evaluation error there is no longer
  silently swallowed into that fallback.

- **`mark_point(auto = TRUE)` no longer datashades under a warped
  coordinate system.** Under
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  /
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)
  the \>50000-row datashade fallback — which bins in linear data space —
  would rasterise into the wrong place; it is now skipped and the vector
  path draws, matching the documented behaviour and the other `auto`
  marks (line/step/segment/edges).

- **A sankey column with too many nodes errors clearly.** When a column
  has more nodes than its inter-node gaps can fit, the node/ribbon
  heights previously went negative and spilled outside the panel;
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  /
  [`mark_sankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  now abort with a message pointing at `node_gap`.

- **Aggregating stats drop missing summaries consistently, with a
  warning.** For a value-summarising bar (`mark_bar(y = , fun = )` and
  friends), a category whose summary is `NA` (an empty group, or a
  summary of data containing `NA` when the function does not remove it)
  is now dropped with a warning on **both** the grouped and ungrouped
  paths, which previously treated `NA` differently. A layer mapping both
  `color` and `fill` also keeps both aligned after aggregation (`fill`
  could previously be left at the wrong length).

- **Clearer errors for malformed scale bounds.**
  [`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_y_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md),
  [`scale_size()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  [`scale_size_area()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md),
  [`scale_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md),
  [`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md),
  and
  [`scale_edge_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md)
  now reject a `limits` (or output `range`) that is not a length-2
  vector — and, for the size/alpha/edge scales, a non-numeric one — with
  a clear message, instead of failing later with a cryptic low-level
  error (e.g. `'length = 2' in coercion to 'logical(1)'`).
  [`scale_size_area()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  also validates `max_size`.

- **[`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)
  rejects a non-numeric mapping.** A factor `x`/`y` previously slipped
  through and the marginal was computed over the integer level codes
  rather than the data; it now errors clearly, as a character column
  already did.

- **Stricter validation of interactivity and layout arguments.**
  [`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)/[`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_interval.md)
  reject a non-logical `toggle`/`empty` (it was silently coerced to
  `FALSE`);
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  validates `width`/`height` like
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)/[`vhierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md);
  and
  [`area()`](https://r-vellum.github.io/vellumplot/reference/area.md)
  rejects a non-numeric or non-integer cell index instead of aborting
  with an opaque “missing value where TRUE/FALSE needed”.

- **[`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  honours `hjust`/`vjust` passed as a variable.** They were only read as
  constant params, so a value routed through a variable (as
  [`vdendrogram()`](https://r-vellum.github.io/vellumplot/reference/vdendrogram.md)
  does per direction) silently fell back to centred, letting leaf labels
  run over the edges. `.emit_text()` now reads them value-first, like
  `angle`.
  [`vdendrogram()`](https://r-vellum.github.io/vellumplot/reference/vdendrogram.md)
  leaf labels also get a larger default gap.

- **Dendrograms and unrooted trees.**
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  now accepts a base `hclust`/`dendrogram` and gains a height-aware
  `"dendrogram"` layout plus an `"unrooted"` layout (via
  [`graphlayouts::layout_as_tree_unrooted()`](https://schochastics.github.io/graphlayouts/reference/layout_tree_unrooted.html),
  needs graphlayouts \>= 1.2.5). `mark_edges(routing = "elbow")` gained
  `elbow_at` / `elbow_axis` so the elbow can be the dendrogram *bracket*
  (corner at the parent’s level) rather than only the midpoint S-bend.
  [`vdendrogram()`](https://r-vellum.github.io/vellumplot/reference/vdendrogram.md)
  is a one-line preset: bracket edges, leaf labels, `direction`, and `k`
  to cut the tree and colour clusters. Also fixes
  `mark_node_text(label = )` so an explicit `label` mapping overrides
  the default vertex name (it was previously ignored).

- **[`vhierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md)
  respects `scale_fill_*()`.** By default nodes are still coloured by
  their depth-1 branch and lightened with depth, but the branch is now
  an ordinary discrete fill scale, so
  [`scale_fill_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  /
  [`scale_fill_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  recolour the branches and a `lighten` argument controls the depth fade
  (`0` = flat colour per branch). Mapping `fill` to a node column
  instead colours every node by that variable (discrete or continuous,
  with the matching `scale_fill_*()`), with no depth fade.

## vellumplot 0.7.0

- **New
  [`vhierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md)
  for space-filling hierarchies — breaking.**
  `vhierarchy(id, parent, value, type = )` draws a tree four ways from
  one parent list, switching only `type`: `"sunburst"` (default),
  `"icicle"`, `"treemap"`, or `"circlepack"`. This **replaces
  `vsunburst()`** — `vhierarchy(..., type = "sunburst")` reproduces the
  old radial sunburst, and `vsunburst()` / `mark_sunburst()` are
  removed. Treemaps use a squarified layout; circle-pack is a faithful
  port of d3’s front-chain packing.
  [`mark_hierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md)
  is the exported layer.

- **Sunbursts, polar pies, and self-loops render un-mirrored.** Requires
  vellum (\>= 0.5.1), which fixes `sector_grob()`/`loop_grob()` to draw
  in the same y-up frame as every other primitive. Sunburst segment
  labels now sit on their own wedges (they previously landed on the
  vertical mirror on unbalanced trees), sunbursts wind clockwise from
  twelve o’clock as documented, and graph self-loops point into the
  empty gap between a vertex’s incident edges rather than its mirror
  image.

- **`select_point(group_by=)` now links whole groups.** A point
  selection with `group_by` / `fields` groups elements sharing those
  column values, so hovering or clicking one mark highlights (and
  [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  spotlights) its whole group — the “hover a series, light up the
  series” behaviour. Implemented by emitting the group as the element
  `hover_group`, so it needs no widget change; a user-declared
  `hover_group` still wins. Inert on a static render.

- **Declarative interactivity (new).** Interaction is now part of the
  plot spec rather than something a host is configured to do. Declare a
  named selection with
  [`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
  (click/hover) or
  [`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_interval.md)
  (brush/lasso), then refer to it from
  [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  (style an aesthetic by selection membership),
  [`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
  (show only members — point a second view at the same selection for
  cross-filtering), or
  [`bind_scale()`](https://r-vellum.github.io/vellumplot/reference/bind_scale.md)
  (drive another panel’s view, for overview + detail).
  [`add_selection()`](https://r-vellum.github.io/vellumplot/reference/add_selection.md)
  shares a free-standing selection across views;
  [`interaction_model()`](https://r-vellum.github.io/vellumplot/reference/interaction_model.md)
  returns the compiled declaration block a host reads. Every node is
  inert on a static render (a plot with interactions renders
  byte-for-byte identically to one without), and
  `condition("s", g, "grey80")` trains scales and draws exactly like
  `color = g`. Enacted by `vellumwidget`.

## vellumplot 0.6.0

- **Sankey styling options.**
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  /
  [`mark_sankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  gain `show_values` (append each node’s value to its label),
  `flow_color` (`"source"`, `"target"`, or `"gradient"` — a
  source-to-target colour fade per ribbon), and `node_width` /
  `node_gap` to tune the node rectangles.

- **Sankey crossing minimisation.**
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  now orders the nodes within each column with the Sugiyama barycenter
  heuristic to minimise ribbon crossings (previously first-appearance
  order, which left many avoidable crossings). The reordering is
  deterministic and pure R. A related fix stacks each node’s ribbon
  slices to meet the node in the same vertical order as the nodes they
  connect to, removing the remaining crossings within a fan of ribbons.

- **Sunburst rendering fixes.** `vsunburst()` now colours wedges by
  their top-level branch (each branch a distinct hue, lightened with
  depth) instead of a single colour per ring — sibling branches were
  previously indistinguishable. It also starts the first wedge at twelve
  o’clock and winds clockwise, matching the package’s pies/roses
  (`coord_polar`) rather than starting at three o’clock
  counter-clockwise.

- **Sunburst / radial hierarchies: `vsunburst()`.** A new plot type for
  part-of-whole hierarchies, from a *parent list* — `id`, `parent` (`NA`
  at the root), and `value` (leaf values; internal nodes sum their
  children). Depth maps to a ring and each node’s angular span is its
  share of its parent’s, drawn as one batched `sector_grob` in an
  aspect-locked, axis-free square panel (mirroring
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)/[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)).
  `inner_radius` opens a central hole (a donut/ring sunburst); nodes are
  coloured by depth. `mark_sunburst()` is the exported layer. See
  `vignette("flows-and-hierarchies")`.

- **Sankey / flow diagrams:
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md).**
  A new plot type for layered flows, built from a *flow list* — one row
  per flow with `from`, `to`, and `value` (the ribbon width). Nodes are
  the union of `from`/`to`; a node that is both a source and a target
  makes the diagram multi-stage. `vsankey(data, from, to, value)`
  returns a ready, axis-free plot (mirroring
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md));
  [`mark_sankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  is the exported layer it adds. The layout is computed R-side
  (longest-path layering, value-proportional node heights and ribbon
  widths, filled Bézier ribbons) and is deterministic. Flows must form a
  DAG; nodes are coloured from the qualitative palette. See
  `vignette("flows-and-hierarchies")`.

- **Uncertainty marks:
  [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  and
  [`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md).**
  ggdist-style slab + interval marks for sample/posterior input (many
  `y` rows per categorical `x`).
  [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  draws a one-sided density slab with a point-interval at its base — the
  median (or `point = "mean"`), a thick inner and thin outer
  equal-tailed quantile interval at the `.width` probabilities (default
  `c(0.66, 0.95)`);
  [`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  is the point-interval alone. A natural fit for visualising posterior
  draws (e.g. from ).

- **[`coord_radial()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  and
  [`scale_size_area()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md).**
  [`coord_radial()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  is a fuller polar system (ggplot2 3.5’s name): besides
  `theta`/`start`/`direction` it takes `end` to sweep only a **partial
  arc** (e.g. `start = -pi/2, end = pi/2` for a semicircular gauge) and
  `inner_radius` for a **donut hole**; with the defaults it matches
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md).
  [`scale_size_area()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  maps a value to the marker’s **area** (value `0` → size `0`), the
  perceptually honest default for bubble charts, with `max_size` the
  size of the largest value.

- **Sankey labels stay on-panel.**
  [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  now reserves horizontal margin for node labels, so the source column’s
  labels (drawn to their left) and the terminal column’s (drawn to their
  right) no longer clip at the panel edge.

- **Robustness of the new marks.** `vsunburst()`/`mark_sunburst()` now
  reject a missing or negative leaf `value` with a clear message
  (instead of a cryptic downstream error), and `mark_sunburst()`
  validates `inner_radius` like `vsunburst()` and
  [`coord_radial()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  do.
  [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)/[`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  skip a category with fewer than two finite observations (with a
  warning) rather than drawing an empty interval, and reject a `width =`
  argument (a likely typo for `.width`) that was previously ignored
  silently.

- **Consistent argument names for the hole radius / ridge height.** The
  central-hole fraction is now spelled `inner_radius` everywhere it
  appears:
  [`coord_radial()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md),
  `vsunburst()`/`mark_sunburst()`, and
  [`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
  (was `hole`).
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)’s
  overlap control is now `height` (was `scale`, which collided with
  `mark_halfeye(scale=)`, a different quantity).

- **Consistent `sketch` / `blend` passthrough.**
  [`mark_ecdf()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md),
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md),
  [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md),
  [`mark_dotplot()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md),
  [`mark_qq()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md),
  and
  [`mark_qq_line()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  now accept a per-layer `sketch =` (their emitters already honoured
  it), and
  [`mark_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
  /
  [`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
  accept `blend =`, matching the rest of the mark surface.

- **Parameterised position adjustments.** New
  [`position_nudge()`](https://r-vellum.github.io/vellumplot/reference/position.md),
  [`position_jitter()`](https://r-vellum.github.io/vellumplot/reference/position.md),
  [`position_dodge()`](https://r-vellum.github.io/vellumplot/reference/position.md),
  [`position_dodge2()`](https://r-vellum.github.io/vellumplot/reference/position.md),
  and
  [`position_jitterdodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  give a mark’s `position` tunable parameters (a bare string like
  `"dodge"` still works with the defaults). Adds three adjustments:
  `nudge` (shift every element by a constant in data units), `dodge2`
  (dodge by the groups actually present at each x, filling the band with
  a `padding` gap — so ragged groupings stay centred), and `jitterdodge`
  (jitter points within their dodged slot).
  `position_jitter(width=, height=, seed=)` and `position_dodge(width=)`
  expose the previously-fixed jitter/dodge extents. A plot using the old
  string positions is unchanged.

- **Label repulsion: `mark_text(repel = TRUE)` /
  `mark_label(repel = TRUE)`.** Overlapping text labels are moved apart
  with a force-directed layout (ggrepel-style), each keeping a thin
  leader line back to its point. Because the plot size is fixed on the
  spec, the repulsion is resolved *exactly* against the true rendered
  panel — a two-pass compile that reads the panel’s device geometry from
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html),
  relaxes the label boxes in pixel space, and maps the result back — so
  it needs no approximation and no `vellum` change, and is deterministic
  under `seed`. Tunable via `box_padding`, `point_padding`,
  `min_segment_length`, and `seed`. Limited to a single cartesian panel
  for now (facets / composition / polar error clearly).

- **`mark_line(window = )`: rolling / cumulative / offset transforms.**
  A line can now transform its `y` per group (over rows ordered by `x`)
  before drawing — moving `mean`/`sum`/`median`/`min`/`max` over a
  window of `k`, running `cumsum`/`cummean`/`cummax`/`cummin`,
  `lag`/`lead` shifts, or `rank`. Pass an op name (`window = "mean"`) or
  a list
  (`window = list(op = "mean", k = 7, align = "right", partial = TRUE)`);
  `align` is trailing/leading/centred and `partial` fills the edges from
  the shorter window so the line stays continuous. A plot without
  `window` is unchanged.

- **Diverging colour scales:
  [`scale_color_gradient2()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  /
  [`scale_fill_gradient2()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md).**
  A three-point ramp (`low`–`mid`–`high`) centred on `midpoint` (default
  `0`), rescaled *about the midpoint*
  ([`scales::rescale_mid`](https://scales.r-lib.org/reference/rescale_mid.html))
  so the neutral colour sits on the chosen value and each side spans as
  far as the data reaches — the correct scale for signed / anomaly data.
  A diverging continuous colorbar also reports `midpoint` + `diverging`
  in its `colorbar` descriptor, so an interactive host can centre a
  value-range filter on the neutral value.

- **`symlog` position transform.**
  `scale_x_continuous(trans = "symlog")` (and `y`) adds a symmetric-log
  axis: linear through zero, logarithmic in the tails
  (`sign(x) · log10(1 + |x|)`), so signed data spanning several orders
  of magnitude — including zero and negatives, which `log10` cannot show
  — reads on one axis. Breaks sit at zero and signed powers of ten. The
  transform name flows into the panel `scales` descriptor like the other
  transforms.

- **New group-region marks:
  [`mark_ellipse()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
  and
  [`mark_hull()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md).**
  Both enclose a set of `(x, y)` points in a single region drawn over a
  scatter — one region per group when a `color`/`fill` is mapped.
  [`mark_ellipse()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
  draws a covariance ellipse (`type = "t"` robust default via , or
  `"norm"`/`"euclid"`), following ggplot2’s `stat_ellipse()`;
  [`mark_hull()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
  draws the convex hull. Both are unfilled boundaries by default
  (map/set a `fill` to shade them) and need at least 3 points per group.
  The region’s boundary trains the position scales, so an ellipse that
  bulges past the data is not clipped.

- **[`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  gains real smoothing methods.** Beyond `"lm"`, the smooth mark now
  fits `"loess"` (local regression, `span =`), `"glm"` (with a `family`
  via `method.args`, e.g. logistic — the fit and its ribbon are
  back-transformed from the link scale), `"gam"` (a penalised smooth,
  default `y ~ s(x)`; needs ), and `"rq"` (quantile regression at a
  single `method.args$tau`; needs , line only, no ribbon). The default
  `method = "auto"` picks `loess` for small groups (\< 1000 points) and
  `gam` for large ones, as in ggplot2. New `formula`, `span`, and
  `method.args` arguments; `glm`/`gam` bands use a normal interval,
  `lm`/`loess` a t-interval. Previously
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  errored on any method other than `"lm"`. and are Suggests — a gated
  method errors clearly if its package is absent.

- **Data panels are emitted as pannable, gridlines tagged.** Cartesian
  data panels (including `coord_flip` and a linear `coord_trans`) now
  push a `pannable` vellum viewport, and gridlines carry
  `role = "grid"`. This is inert for static rendering but lets an
  interactive host (`vellumwidget`) pan/zoom a panel’s marks while its
  clip + axes stay fixed and hide/redraw gridlines — the groundwork for
  axis-aware zoom. Polar / nonlinear-`coord_trans` panels stay
  non-pannable. Requires the current development `vellum`.

- **Continuous colorbar filter metadata.** A continuous `color` scale
  now attaches each mark’s colour value as `filter_value` in its element
  `meta`, and a `colorbar` descriptor (value domain + orientation) to
  the gradient-bar grob. Together (via
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html))
  these let a host such as `vellumwidget` overlay an interactive
  value-range filter on the colorbar. Discrete/binned colour scales are
  unaffected. No change to rendered output.

- **Panels now carry a `scales` descriptor for interactive hosts.** Each
  cartesian data panel’s viewport gains a `meta$scales` record — per
  axis: `type` (continuous / log10 / discrete / binned / date /
  datetime), `transform`, the data and native domains, tick breaks +
  labels, and `time_unit` for date/datetime axes. It surfaces via
  `vellum::scene_model()$panels$meta` and lets a host (e.g.
  `vellumwidget`) map device pixels back to data values — so a brush or
  a reported view can be expressed in data coordinates, not just pixels.
  Requires `vellum` (\>= 0.4.0.9000). Internal: trained position scales
  now also record their `transform` name and, for date/time axes, a
  `time_unit` (previously the date/datetime nature was lost after
  training). No change to rendered output.

## vellumplot 0.5.0

- **Rich [`md()`](https://r-vellum.github.io/vellumplot/reference/md.md)
  legend titles no longer clip.** A legend built from a rich title
  (`scale_color_continuous(name = md("Power (hp m^2^)"))`,
  `labs(color = md(...))`) measured the title as zero width, so the
  legend reserved no room for it and the drawn title spilled off the
  page. Titles are now measured through vellum’s rich text path
  (`vl_strwidth()`), reserving the space they actually occupy.

- **Secondary axes.** Continuous position scales gain a `sec.axis`
  argument fed by the new
  [`sec_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md)
  /
  [`dup_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md):
  a second set of ticks and labels on the opposite edge (top for `x`,
  right for `y`), computed as a 1:1 monotonic transform of the primary
  axis — e.g.
  `scale_x_continuous(sec.axis = sec_axis(~ . * 1.8 + 32, name = "°F"))`
  for a unit conversion, or
  [`dup_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md)
  to duplicate an axis on a wide plot. The transform is a formula,
  function, or `scales::transform_*()` object. Scoped to the default
  Cartesian system with shared facet scales; combining it with
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  /
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  /
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md),
  free facet scales, or
  [`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)
  raises a clear error, and it is not drawn inside a plot composition. A
  plot without a `sec.axis` is byte-for-byte unchanged.

- **Datashading now covers dense lines and large-graph edges.**
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md),
  and
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  gain an `auto = TRUE` switch (parallel to `mark_point(auto = TRUE)`):
  past a row threshold the layer rasterises into a line- /
  segment-density field via
  [`vellum::datashade_lines()`](https://r-vellum.github.io/vellum/reference/datashade_lines.html)
  /
  [`vellum::datashade_segments()`](https://r-vellum.github.io/vellum/reference/datashade_lines.html)
  instead of emitting one vector per element, so overplotted timeseries
  stacks and graph “hairballs” render fast and honestly. The fallback is
  skipped under a warped coordinate system
  ([`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  /
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)),
  which keeps the vector path. (Area / ribbon datashading is not yet
  available, pending area-fill support in vellum.)

- **`mark_datashade(spread = )`** exposes vellum’s post-aggregation
  spreading: `NULL` (default, raw output), a positive integer for a
  fixed pixel radius
  ([`vellum::spread()`](https://r-vellum.github.io/vellum/reference/spread.html)),
  or `"auto"` for density-adaptive dilation
  ([`vellum::dynspread()`](https://r-vellum.github.io/vellum/reference/dynspread.html)).
  Datashaded lines and segments default to `"auto"` so single-pixel
  marks stay visible.

- **Continuous and binned colour scales now interpolate perceptually
  (Oklab) by default.** A colour ramp built from a plain colour vector
  (e.g. `scale_color_gradient(low, high)` or
  `scale_color_continuous(palette = c(...))`) is blended in the
  perceptually-uniform Oklab space instead of sRGB, so it no longer
  passes through muddy, over-dark midtones or drifts in hue — the ramp
  and its legend colourbar read evenly. Designed perceptual palettes
  (the `batlow` default,
  [`hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html) names) are
  already uniform and are unchanged. Set
  `options(vellumplot.color.interpolation = "srgb")` (or `"lab"`) to
  restore the old behaviour. Gradient *fills* opt in per gradient with
  `linear_gradient(..., interpolation = "oklab")` (passed through to
  vellum).

- **Error bars and line ranges are now interactive.**
  [`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  and
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  thread a declared `data_id`/`tooltip`/`hover_group` through to their
  drawn segments, so each bar is keyed to its datum — it appears in
  `scene_model()` and
  [`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md)
  and carries a `data-key` in the SVG, ready for hover/click/select in a
  widget (a bar’s cap segments share the bar’s key).
  [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  already keyed each box by its category; this completes the statistical
  marks. A mark with no interactivity declared is unchanged.

- **Two more shapes in
  [`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md).**
  The `shape` aesthetic’s default palette now extends to
  `"triangle_down"` (a downward triangle) and `"star"` (a five-pointed
  star) after the original six, so a mapped `shape` covers up to eight
  levels without an explicit scale, and both are accepted as
  `scale_shape(values=)`. Requires the accompanying `vellum` dev
  version. (A constant `shape = "star"` on a mark already worked once
  vellum gained the shape.)

## vellumplot 0.4.0

### New features

- **Categorical datashading in one call.**
  [`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md)
  now accepts a mapped discrete `color`/`fill` aesthetic and shades
  **categorically** (datashader’s `count_cat`): each category is
  aggregated separately and every cell is coloured by the count-weighted
  average of the category hues it holds, opacity by density — so you see
  which category dominates where, and where they mix, **with a colour
  legend**. Category hues come from the usual discrete colour scale
  (`scale_color_*()` apply). This replaces the old idiom of stacking one
  datashade layer per category with `blend = "screen"`. New
  `span`/`clip` arguments clamp the density range (absolute limits or
  percentiles) so a few extreme cells don’t flatten the rest. Requires
  the accompanying `vellum` dev version.

- **[`mark_image()`](https://r-vellum.github.io/vellumplot/reference/mark_image.md)
  draws images at data points.** A new mark places a bitmap image
  (e.g. a country flag or company logo) at each `(x, y)`, replacing the
  usual point marker. `src` is a column of local file paths or one
  constant path, and `size` sets the height in millimetres (the width
  follows each image’s native aspect ratio). Requires the suggested
  `magick` package
  ([\#33](https://github.com/r-vellum/vellumplot/issues/33)).

- **[`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)
  — nonlinear display transforms.** Warps the *display* of one or both
  position axes after the scale has trained, so gridlines bunch up and
  straight lines curve while the axis keeps its original data-value
  labels (unlike `scale_*(trans=)`, which rescales the data and relabels
  in transformed space). Each of `x`/`y` takes a transform name
  (`"log10"`, `"sqrt"`, `"identity"`) or a `scales::transform_*()`
  object, e.g. `coord_trans(y = "log10")`. Supports the common marks
  (points, lines, areas/ribbons, bars, tiles, smooths, text, …);
  segment/interval, boxplot, edges, and raster/datashade marks are not
  warped yet and raise a clear error under
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md).

- **[`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  and
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  no longer clip at the edges.** The position scales are now trained to
  include each mark’s drawn footprint – the full kernel-density support
  for a violin, and the ridge height (`scale`) above the top category
  for a ridgeline – so the tallest ridge and the density tails are no
  longer cropped by the panel. Explicit axis limits still take
  precedence.

- **Marginal distributions with
  [`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md).**
  A new plot modifier draws a density or histogram of the panel’s `x`
  variable along the top edge and of its `y` variable along the right
  edge, each sharing the scatter’s axis so they line up (the vellumplot
  analogue of `ggExtra::ggMarginal()`). Pick the distribution with
  `type = "density"` / `"histogram"`, the edges with `sides` (`"t"`,
  `"r"`, `"tr"`), and the extent with `size`; `group = TRUE` splits each
  marginal by a discrete `color`/`fill` mapping in the matching palette.
  It reads `x`/`y` from the first plain (identity-stat) layer and reuses
  the existing density/bin stats, so no axes or legends are duplicated.
  This version supports a single panel only (an error is raised with
  facets, a non-Cartesian coordinate system, or a locked aspect ratio).

- **Positional constants now render.** A bare literal on a coordinate
  channel (e.g. `mark_segment(x = i, y = 0, xend = i, yend = value)` for
  a lollipop baseline) is now treated as a constant-valued *coordinate*
  rather than a style param, so it populates the layer’s values and
  trains the position scale. Previously `y = 0` was dropped from scale
  training, collapsing each segment’s start onto its end (zero-length,
  invisible), and a segment-only plot errored with “Every layer needs an
  x and y encoding”. Segment/edge endpoints (`xend` / `yend`) now also
  extend the axis domain, so a segment-only plot derives its range from
  both ends. Affects the positional channels `x`, `y`, `xend`, `yend`,
  `xmin`, `xmax`, `ymin`, `ymax`.

- **Faster
  [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  on large maps.** Non-interactive sf layers now batch all features that
  share a resolved fill/colour into a single grob per style group
  (polygons into one `evenodd` `path_grob`, lines into one NA-separated
  `lines_grob`), instead of one grob per feature, and the layer’s
  bounding box is computed in a single linear pass rather than by
  growing coordinate vectors. On a 40k-polygon grid this cut scene
  compilation from ~24s to ~0.8s. Interactive layers (those declaring
  `data_id` / `tooltip` / `hover_group`) are unchanged — they still emit
  one keyed grob per feature so every feature stays addressable. Note:
  because a batched polygon group is filled with one `evenodd` rule, two
  same-fill features that *overlap* would cancel in the overlap region
  (the same property a self-overlapping `MULTIPOLYGON` already has);
  disjoint features — every real choropleth and coverage map — are
  unaffected.

### Bug fixes

- **`linetype` now works on stroked marks.**
  [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md),
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_rug()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md),
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md),
  and
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  (and `annotate("segment", ...)`) previously ignored `linetype` and
  always drew solid lines; they now honour it, consistent with
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  ([\#30](https://github.com/r-vellum/vellumplot/issues/30)).

- **`annotate("rect", ...)` honours infinite bounds.** A bound of
  `-Inf`/`Inf` (e.g. `xmin = -Inf, xmax = Inf` for a full-width band)
  now extends the rectangle to the panel edge, matching ggplot2, instead
  of silently rendering nothing
  ([\#29](https://github.com/r-vellum/vellumplot/issues/29)).

- **Continuous labels no longer group thousands by default.** A default
  continuous axis or legend now renders 4-digit-plus values without a
  grouping separator, so years and IDs read `2010` instead of `2 010`
  ([\#27](https://github.com/r-vellum/vellumplot/issues/27)). Pass
  explicit `labels` to restore grouping.

- **More inputs fail fast.** A `design` layout area must be a solid
  rectangle (and
  [`area()`](https://r-vellum.github.io/vellumplot/reference/area.md)
  requires `t <= b`, `l <= r`); a per-row interactivity value
  (`tooltip`/`data_id`/…) must be length 1 or the data’s row count; and
  [`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)
  requires a cleanly numeric column. Each now errors clearly instead of
  silently mis-rendering.

- **Clearer alt text for grid facets.** A
  [`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  plot’s automatic alt text now distinguishes the row and column
  variables (e.g. “Faceted by rows cyl, columns am”) instead of running
  them together.

- **Compositions can be themed.**
  [`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  now accepts a composition (from \[concat()\] / \[hconcat()\] /
  \[vconcat()\]) and styles its figure-level chrome — the
  title/subtitle/caption bands, the collected legend, panel spacing, and
  tags. Previously a composition’s figure chrome always used the default
  theme. Each sub-plot still carries its own theme.

- **Gradient fills fail clearly on marks that don’t support them.** A
  `fill = linear_gradient(...)` is painted by
  [`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  and
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md);
  on any other mark it now raises a clear error instead of leaking an
  undefined paint into the renderer.

- **Inputs are validated up front.**
  [`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  now checks `panel.spacing`, `plot.margin`, `aspect.ratio`, and
  `axis.ticks.length` (and rejects `NA`/`Inf` in any numeric setting);
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)
  checks `width`/`height` the way it already checked `dpi`; a discrete
  `shape`/`linetype` scale with more categories than its palette errors
  instead of silently reusing a glyph; and a binned colour scale given
  fewer than two breaks errors instead of crashing in
  [`colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).
  Valid input is unaffected.

- **Legends read consistently.** A horizontal (top/bottom) continuous
  colour legend now draws the `NA` key that the vertical one always did.
  `guide_legend(reverse = TRUE)` now actually flips a continuous
  colourbar (previously a silent no-op) and no longer desyncs a binned
  colour scale’s boundaries from its swatches. A merged colour+shape
  legend’s swatches are now tagged for interactive highlight/select.
  Minor gridlines past the outermost breaks use the local break spacing
  at each end, so they sit correctly when breaks are unevenly spaced.

- **Numeric facets order numerically.** Faceting on a numeric variable
  now orders panels `1, 2, 10` rather than the lexicographic `1, 10, 2`;
  multi-variable facets order by each variable’s own type.

- **Log/reversed axes.** Bars and areas on a `scale_*(trans = "log10")`
  (or `"sqrt"`) axis draw from the axis floor instead of a degenerate
  shape (the zero baseline that maps to `-Inf` is clamped into the
  trained range). A descending limit such as `ylim(hi, lo)` on a
  bar/area is no longer silently un-reversed by the zero baseline.

- **Grouped histogram density.** `after_stat(density)` /
  `after_stat(prop)` on a grouped histogram or bar now normalizes per
  group (each group integrates/sums to 1), matching ggplot2, rather than
  dividing by the grand total.

- **Clearer errors instead of silent surprises or crashes.**
  [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)
  rejects a `"reverse"` transform (a no-op there — use
  `scale_*(trans = "reverse")`) and rejects bar/area marks on a
  nonlinearly transformed axis (a zero baseline has no place on a log
  display). Histogram / 2-D bin / hexbin stats abort cleanly on all-`NA`
  input instead of a cryptic [`seq()`](https://rdrr.io/r/base/seq.html)
  error.
  [`mark_raster()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  and z-surface contours reject a grid with a duplicated cell
  (previously a silent transparent/`NA` hole). Composition auto-tags
  continue past 26 sub-plots (`Z`, `AA`, `AB`, …) instead of `NA`.

- **[`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  now stacks.** `mark_area(position = ...)` was silently ignored; it
  gains a `position` argument (`"stack"` default, plus `"fill"` and
  `"identity"`), so areas sharing an `x` with a mapped `fill`/`color`
  combine into a band instead of overlapping from the zero baseline. An
  area with no fill mapping is unchanged.

- **Mapped aesthetics that were silently dropped now take effect.** A
  mapped `fill` on
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  colours the label background (previously it always fell back to
  white); a mapped or constant `alpha` is honoured per category/tick by
  [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md),
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md),
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md),
  [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  and
  [`mark_rug()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  (previously collapsed to one value or ignored), and
  [`mark_rug()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  also honours a mapped `color`.

- **British spelling works everywhere.**
  `hover_colour`/`selected_colour` are now recognised as interactivity
  arguments (previously only the American spelling was), and
  [`mark_bin2d()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)/[`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  no longer add a default count fill when a British `colour =` is
  supplied.

- **[`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  warns on a layout-column clash.** A vertex/edge attribute named
  `x`/`y` (or `xend`/`yend`) is overwritten by the layout coordinates;
  that used to happen silently and now emits a warning naming the
  attribute.

## vellumplot 0.3.0

- Adopted vellum’s renamed `vl_*` graphics primitives (`vl_gpar()`,
  `vl_unit()`, `vl_viewport()`), which no longer mask grid.

- **NA legend keys for `size` / `shape`.** A mapped `size` or `shape`
  aesthetic whose data contains missing values now shows an “NA” key (as
  the colour scales already did). Also fixes a crash: NA in a `shape`
  mapping previously errored (`Unknown point shape: NA`) — it now draws
  as a neutral circle.

- **2-D density contours.**
  [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  draws iso-density contour lines of a 2-D point cloud and
  [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  fills the bands between them (coloured by level automatically). By
  default the field is a kernel density estimate (via
  [`MASS::kde2d()`](https://rdrr.io/pkg/MASS/man/kde2d.html)); map a `z`
  aesthetic to contour a supplied surface over a regular `x`/`y` grid
  instead. Contour tracing uses the `isoband` package (both `isoband`
  and `MASS` are Suggests).

- **Binned position scales.**
  [`scale_x_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_x_binned.md)
  /
  [`scale_y_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_x_binned.md)
  cut a continuous axis into bins — ticks at the bin boundaries, each
  datum drawn at its bin centre — reusing the binned-colour
  classification (`style`/`n`, or explicit `breaks`).
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  sizes to the bin width.

- **Rich and multi-line text labels.**
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  now carries rich labels through to the renderer instead of flattening
  them: map `label = md(<expr>)` for a per-datum styled label
  (bold/italic/super-/subscript/colour), and plain labels may contain
  newlines (`\n`) to wrap onto stacked lines. Requires vellum’s
  development version.
  ([`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)’s
  background box does not yet support rich labels.)

- **Accessibility (alt text).** Every compiled plot is now an accessible
  image by default.
  [`plot_alt()`](https://r-vellum.github.io/vellumplot/reference/plot_alt.md)
  returns a plot’s text alternative — an author-written string from the
  new `labs(alt = )`, or a prose summary vellumplot generates from the
  spec (chart type, x/y/colour/size mappings, observation count,
  faceting). At the compile seam the plot **title** becomes the scene’s
  accessible name and this alt text becomes its description, so
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  output carries `role="img"` + `<title>`/`<desc>` in SVG and a tagged
  `Figure` with `Alt` in PDF (via vellum). See the new *Accessibility*
  article. Requires vellum’s development version (\>= 0.1.1.9000) for
  the accessible SVG/PDF backend.

- **British spelling.** `mark_*(colour = )` is now honoured as an alias
  for `color` (previously the mapping was silently dropped and no colour
  scale was trained). Added `scale_colour_*()` aliases for the
  `scale_color_*()` family.

- **Faithful alt text.** The auto-generated description now matches what
  the plot actually renders: it honours `scale_*(name = )` axis/legend
  titles, names the implicit “count” axis of a count bar, and describes
  a network graph by its node and edge counts instead of its
  (meaningless) layout x/y axes.

## vellumplot 0.2.1

Consumes `vellum`’s new compound `native + mm` unit (requires vellum \>=
0.1.1).

- **Label nudges**:
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  /
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  gain `nudge_x` / `nudge_y` (in millimetres, `+x` right / `+y` up) that
  shift a label by an exact absolute distance, device-resolved so the
  nudge is constant regardless of scale or panel aspect.
- **Device-exact drop shadows**:
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)’s
  `x` / `y` offset is now an absolute distance in **millimetres** (was a
  panel-relative npc fraction), so a drop shadow stays the same physical
  distance and is isotropic on non-square panels. Defaults changed
  accordingly (a small down-right drop). Replaces the previous
  npc-fraction workaround.

Device-space dodge (`mark_bar`) and jitter (`mark_point`) remain
data-space for now; converting them to the compound unit is deferred (it
changes existing rendered output and needs snapshot review).

## vellumplot 0.2.0

Grammar-breadth release: new scales, mapped aesthetics, legend control,
and distribution marks, closing the most conspicuous gaps versus
ggplot2.

### Scales & axes

- **Date/time scales**:
  [`scale_x_date()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  /
  [`scale_x_datetime()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  /
  [`scale_x_time()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  (and `y` twins) with `date_breaks` (interval strings like
  `"3 months"`) and `date_labels` (strftime formats). `Date`/`POSIXct`
  columns still get a sensible date axis automatically.
- **New mapped aesthetics**:
  [`scale_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md)
  (continuous opacity) and
  [`scale_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_linetype.md)
  (discrete line types), each drawn per element with its own legend.
  `alpha` mapped to data now varies opacity (previously constant-only);
  `linetype` applies to
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  /
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md).
- **Identity scales**:
  [`scale_color_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md),
  [`scale_fill_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md),
  [`scale_size_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md),
  [`scale_shape_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md),
  [`scale_alpha_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md),
  [`scale_linetype_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  — use data values verbatim, no legend.
- **Limit shortcuts**:
  [`xlim()`](https://r-vellum.github.io/vellumplot/reference/lims.md),
  [`ylim()`](https://r-vellum.github.io/vellumplot/reference/lims.md),
  and
  [`lims()`](https://r-vellum.github.io/vellumplot/reference/lims.md).
- **Legend control**:
  [`guides()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  with `guide = "none"` (hide a legend), `guide_legend(reverse = TRUE)`
  (reverse key order), and `guide_legend(title=)`.

### Marks & stats

- **Distribution marks**:
  [`mark_ecdf()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  (empirical CDF step),
  [`mark_rug()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  (marginal ticks),
  [`mark_qq()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md) +
  [`mark_qq_line()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  (quantile-quantile plot).
- **Density-shape marks**:
  [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  (mirrored density per category),
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  (overlapping per-category densities), and
  [`mark_dotplot()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  (binned, stacked dots). Violin and ridgeline carry per-category
  provenance.

### Not yet implemented (still planned)

2-D density / contour (`mark_contour`/`stat_density_2d`);
position-binned scales (`scale_x_binned`);
[`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)
and free non-position scales across facets; triple-merge legends
(colour+shape+size) and NA keys for size/shape.

## vellumplot 0.1.1

- New exported
  [`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md):
  returns the compiled-scene provenance table — one record per emitted
  mark grob, tying each low-level primitive back to the data rows and
  trained scales that produced it, with an `id` that matches the grob’s
  `data-vellum-id` in SVG and the `id` column of
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html).
  This is the first public consumer of the provenance metadata
  (previously internal), the substrate for interactivity, linked views,
  and accessibility.
- Row-key provenance is now refined per element for the line, area,
  ribbon, step, text, and boxplot marks (previously only
  point/bar/tile/segment/sf/ edges), so a record’s `rows` resolve to the
  actual data rows an element draws rather than the whole layer. See the
  scene-contract vignette in `vellum`.

## vellumplot 0.1.0

First release. vellumplot is a declarative, pipe-first grammar of
graphics that compiles an inspectable spec into a `vellum` scene, with
faceting, coordinate systems, and multi-plot composition. Everything
below ships in this first release.

### Features

- **Interactivity declarations** (host-agnostic; inert on a static
  render). Any `mark_*()` accepts reserved per-row args `data_id`,
  `tooltip`, and `hover_group` (tidy-eval expressions). They flow into
  the vellum scene as per-element keys/metadata — `data_id` becomes the
  SVG `data-key` and both surface in
  [`vellum::scene_model()`](https://r-vellum.github.io/vellum/reference/scene_model.html)
  — the foundation a companion widget uses for hover/select/linking. A
  plot without them compiles and renders exactly as before. Applies to
  `stat = "identity"` marks (points, bars, tiles, segments, edges,
  hexbins, polar bars) and to
  [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  — each sf feature (polygon/linestring/point) becomes an addressable,
  keyed element.

- **Per-element interaction styling**: marks also accept `hover_color`
  and `selected_color` (constant or column-mapped), carried into the
  scene so a host (`vellumwidget`) outlines each element in its own
  colour on hover/select — overriding the widget-wide theme.

- **Interactive discrete legends**: when a plot maps a discrete
  `color`/`shape` scale and declares interactivity, each legend swatch
  is tagged with the data series it represents, and every mark carries
  its series membership. A host (`vellumwidget`) uses this to make
  swatches highlight/select their whole series. Inert on a static render
  and when no interactivity is declared.

- **Auto-display**: printing a plot (or composition) draws it into the
  active graphics device — the RStudio / Positron Plots pane, or a
  knitr/Quarto chunk — like ggplot2 (via
  [`vellum::display()`](https://r-vellum.github.io/vellum/reference/display.html)).
  [`summary()`](https://rdrr.io/r/base/summary.html) shows the
  inspectable spec tree instead;
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  still writes a file.

- [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)
  starts an inspectable, serializable `PlotSpec`.

- **Output resolution**: `vplot(dpi =)` sets the authored resolution and
  `render_plot(dpi =)` overrides it per render, so an exported PNG’s
  pixel dimensions are `width * dpi` by `height * dpi`. Compositions
  inherit the first sub-plot’s `dpi` (or take an explicit
  `concat(dpi =)`).

- Marks:
  [`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
  [`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  (reference lines via `xintercept` / `yintercept`), and
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  (uses explicit `y` heights, or counts rows per category when `y` is
  omitted).

- Encodings captured with tidy evaluation: `x`, `y`, `color`/`fill`,
  `size`, `shape`, `alpha`. Scalar values become constant aesthetics.

- Scales:
  [`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  /
  [`scale_y_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  (linear and `log10`) with auto-trained domains and ggplot-style
  expansion; **discrete (band) position scales** are trained
  automatically for categorical `x`/`y` (bars);
  [`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  (perceptual ramp),
  [`scale_color_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  (qualitative palette), and a trained **size scale** for a mapped
  `size`.

- Guides: trained x/y axes (breaks, labels, titles), a grey panel with
  white gridlines, and a legend area that **stacks multiple guides** — a
  colour legend (continuous gradient bar or discrete swatches) and a
  size legend.

- **Legend layout** is measured in millimetres: each guide is sized to
  its content (a title line above one row per key), the row pitch is
  driven by the key’s drawn size so large bubble keys never overlap, and
  the guide block is centred in the legend track. Titles sit directly
  above their keys, horizontal (top/bottom) legends pack keys tightly
  instead of spreading them across the full width, and a vertical legend
  uses the full figure height.

- **Legend keys match the mark**: a colour legend draws the glyph the
  plot actually uses — a filled circle for point layers, a short line
  for line-like layers (line/step/smooth/segment/…), and a filled square
  for bar/area/tile/ polygon layers — instead of always a square swatch.

- **Legend geometry is themeable**:
  `theme(legend.key.size=, legend.spacing=, legend.margin=)` set the
  key/swatch side, the gap between stacked guides, and the inset around
  the legend block (all in millimetres).

- Continuous colour bars now carry **white break ticks** aligned to
  their labels, on both vertical and horizontal (top/bottom) legends.

- **Merged legends**: mapping one variable to two aesthetics draws a
  single legend whose keys carry both encodings — discrete `colour` +
  `shape` become coloured shape keys; continuous `colour` + `size`
  become colour-graded, size-graduated points. Merging follows ggplot2’s
  rule (same title and breaks/levels); give one scale a different
  `name=` to keep them separate.

- Layering: multiple marks on one panel, with scales trained across all
  layers.

- **Faceting**: `facet_wrap(~var)` and `facet_grid(rows ~ cols)` split
  the data into a panel grid with facet strips and aligned, shared axes.

- **Scale resolution**
  ([`resolve_scale()`](https://r-vellum.github.io/vellumplot/reference/resolve_scale.md)
  / `facet_*(scales=)`): position scales are shared across panels by
  default; opt into `"free_x"` / `"free_y"` / `"free"` (independent per
  panel) for per-panel ranges and axes.

- **Statistical transforms**:
  [`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  (bin a continuous variable into count bars) and
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  (an `"lm"` fit drawn as a line with an optional confidence ribbon).
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  with no `y` uses the count stat. Map computed variables with
  [`after_stat()`](https://r-vellum.github.io/vellumplot/reference/after_stat.md),
  e.g. `y = after_stat(density)`.

- **Position adjustments** (`position =`): grouped bars **stack** by
  default; `"dodge"` places them side by side and `"fill"` normalises
  each group to 1. `mark_point(position = "jitter")` spreads overlapping
  points.

- **Datashading**:
  [`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md)
  aggregates a large point cloud into a density raster (via
  [`vellum::datashade()`](https://r-vellum.github.io/vellum/reference/datashade.html))
  that fills the panel — cost independent of point count.
  `mark_point(auto = TRUE)` switches to this automatically above ~50k
  rows.

- **More marks**: areas/ribbons
  ([`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md),
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)),
  steps
  ([`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)),
  intervals
  ([`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md)),
  [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md),
  tiles/heatmaps
  ([`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_raster()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)),
  2-D binning
  ([`mark_bin2d()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md),
  [`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)),
  [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  /
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md),
  and pie/donut shortcuts
  ([`mark_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md),
  [`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)).

- **More statistical transforms**:
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  (kernel density),
  [`mark_summary()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  (aggregate `y` per category), in addition to the histogram / binning /
  smooth stats. Map computed variables with
  [`after_stat()`](https://r-vellum.github.io/vellumplot/reference/after_stat.md).

- **Coordinate systems**:
  [`coord_cartesian()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  (view-window zoom),
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md),
  [`coord_fixed()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  /
  [`coord_equal()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  (aspect lock), and
  [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  (pie / coxcomb / radar).

- **Themes**:
  [`theme_gray()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  (default),
  [`theme_minimal()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_bw()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_classic()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  [`theme_void()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
  and
  [`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  for ad-hoc overrides (panel background, gridlines, text, strip
  background, legend position, margins).

- **Effects & paints**: stroked and point marks take a layer
  `effects = list(...)` argument —
  [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md)
  (neon halo),
  [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md)
  (contrasting halo for legibility), and
  [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)
  (drop / ambient).
  [`linear_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
  /
  [`radial_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
  can be used as a `fill` value (area / ribbon / bar), and
  [`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  ties glow + gradients + a neon palette into the
  [mplcyberpunk](https://github.com/dhaitz/mplcyberpunk) look.

- **Hand-drawn (“sketch”) rendering**:
  [`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
  (re-exported from vellum) gives any geometry mark a wobbly,
  hachure-filled [Rough.js](https://roughjs.com) look via a `sketch =`
  argument. Unlike a layer effect it is a *geometry* property, generated
  natively in the engine, so it is exact, cross-backend, and works in
  PDF. It rides three levels, most-specific-wins: a mark’s `sketch =`,
  an
  [`element_line()`](https://r-vellum.github.io/vellumplot/reference/element.md)
  /
  [`element_rect()`](https://r-vellum.github.io/vellumplot/reference/element.md)
  `sketch =` slot, or the plot-wide
  [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)
  one-liner (hand-drawn gridlines, axes, ticks, marks and legend keys on
  a paper background). `sketch = NA` forces an element crisp; text is
  never sketched (pair a handwriting font).

- **Composition**:
  [`hconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`vconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  [`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md),
  and
  [`wrap_plots()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
  arrange several plots on a grid; the aligned path lines up panel edges
  and can **collect guides** across sub-plots.
  [`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)
  overlays a plot, and
  [`compose_annotation()`](https://r-vellum.github.io/vellumplot/reference/compose_annotation.md)
  adds figure-level titles and auto-tags (`A`, `B`, …).

- **Repeat**:
  [`repeat_()`](https://r-vellum.github.io/vellumplot/reference/repeat_.md)
  replicates a plot across a set of fields, zipping one or more
  encodings to produce a composition.

- **Blend modes**: marks take a `blend =` argument (the CSS
  `mix-blend-mode` set — `"multiply"`, `"screen"`, `"darken"`, …). The
  layer composites as one isolated group against the panel and earlier
  layers, e.g. two overlapping translucent layers under `"multiply"`.

- **Spatial**:
  [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  draws the geometry column of an `sf` object as a map layer (polygons /
  lines / points), with
  [`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)
  to reproject to a target CRS and lock the map aspect ratio;
  [`scale_fill_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  /
  [`scale_color_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  bin a continuous fill/colour into discrete classes for choropleths.

- **Network**:
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  starts a node-link diagram from an `igraph` graph — it runs a layout
  (stress majorization by default, via `graphlayouts`;
  `"sparse_stress"`, `"backbone"`, `"fr"`, `"circle"`, … or a
  matrix/function), builds a node and an edge table, and locks the
  aspect with a void theme.
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  draws edges (straight, batched; reciprocal/parallel edges offset off
  the centre line, self-loops nested, optional `arrow`),
  [`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  the vertices, and
  [`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  the labels — with a fixed edges-under-nodes draw order. Edges are
  capped exactly at each endpoint’s node boundary (per vertex, at any
  resolution), so arrowheads land on the node edge.
  [`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)
  maps a weight to edge width with its own legend. `igraph` /
  `graphlayouts` are optional (`Suggests`).

- Output: `render_plot(plot, path)`; `vellum::render(plot, path)` and
  `print(plot)` also work. The compiler is registered on vellum’s
  `as_vellum_scene()` seam.

### Under the hood

- **Compiled-scene provenance** (foundation for interactivity /
  accessibility / linked views): every emitted mark grob now carries a
  globally-unique, stable `id` (surfacing as `data-vellum-id` in SVG),
  and the compiler builds a serializable row-key / scale-ref table — one
  record per grob tying it back to the data rows and trained scales that
  produced it — carried on the compiled scene as
  `attr(scene, "vellumplot_provenance")`. Populated on every compile;
  additive metadata only (raster/PDF output is byte-for-byte unchanged).
- **Continuous integration**: GitHub Actions run `R CMD check` (R
  release/devel/oldrel on Linux + macOS, with the Rust toolchain the
  `vellum` backend needs), including a nightly run against `vellum`’s
  `main` to catch cross-layer breaks early, plus a pkgdown build.

### Not yet implemented (planned)

Reactivity, 2-D contour stats, and the algebraic `*` / `+` layer
combinators. Independent *non-position* (colour/size) scales across
facets are not yet supported (those legends stay shared). On the network
side, community hulls and alternative idioms (arc/matrix/hive) are
deferred.
