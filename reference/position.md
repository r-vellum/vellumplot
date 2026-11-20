# Position adjustments

Fine-grained control over how a mark's overlapping elements are placed.
Pass the result to a mark's `position` argument (e.g.
`mark_point(position = position_jitter(width = 0.2))`). A bare string
(`position = "dodge"`) still works and uses the defaults.

## Usage

``` r
position_nudge(x = 0, y = 0)

position_jitter(width = NULL, height = NULL, seed = NULL)

position_dodge(width = NULL, preserve = c("total", "single"))

position_dodge2(padding = 0.1)

position_stack(vjust = 1)

position_fill(vjust = 1)

position_sina(width = 0.8, seed = NULL)

position_jitterdodge(
  jitter.width = NULL,
  jitter.height = 0,
  dodge.width = 0.75,
  seed = NULL
)
```

## Arguments

- x, y:

  `position_nudge()` shift (data units).

- width, height:

  Maximum jitter (data units); `NULL` uses the default. For
  `position_sina()`, `width` is the spread as a fraction of the band.

- seed:

  Optional integer seed for the random jitter.

- preserve:

  `position_dodge()`: `"total"` splits the dodge width between the
  groups present at each x; `"single"` gives each element the width of
  one group at the x with the most groups, counted over the whole layer
  (every facet panel), as in ggplot2.

- padding:

  `position_dodge2()` gap between dodged elements, as a fraction of the
  element width.

- vjust:

  `position_stack()` / `position_fill()`: where a point, text or label
  sits inside its stacked span, from `0` (bottom) to `1` (top).

- dodge.width, jitter.width, jitter.height:

  `position_jitterdodge()` dodge / jitter extents.

## Value

A `vellumplot_position` object for a mark's `position` argument.

## Details

- `position_nudge()` shifts every element by a constant amount in
  **data** units (for continuous axes) — handy for offsetting labels
  from points.

- `position_jitter()` adds uniform random noise; `width`/`height` are
  the maximum shift in data units (default: 40% of the resolution),
  `seed` makes it reproducible.

- `position_dodge()` places grouped elements side by side; `width` is
  the total data-space width shared by the groups at one x (default: the
  bar width, 90% of the category band). As in ggplot2, only the groups
  present at an x share it: with `preserve = "total"` (the default) an x
  with fewer groups gets wider elements, while `preserve = "single"`
  keeps every element as wide as one group at the fullest x.

- `position_dodge2()` dodges by the groups actually present at each x
  and splits the band between them with a `padding` gap — so ragged
  groupings stay centred and evenly spaced.

- `position_jitterdodge()` dodges grouped elements, then jitters within
  each dodged slot (points over dodged boxes). As in ggplot2, the
  default `jitter.width` is 40% of the resolution divided by (the most
  groups at any x, over every facet panel, + 2), and there is no
  vertical jitter.

- `position_stack()` / `position_fill()` stack grouped elements sharing
  an x (`fill` normalises each stack to 1). Bars and areas take the
  stacked span; a point, text or label moves to a height inside its
  span: `vjust = 1` (the default) is the top, `0.5` the middle – so
  `mark_text(position = position_stack(vjust = 0.5))` centres labels on
  the segments of a stacked bar.

- `position_sina()` spreads each category's points along x by a
  quasirandom offset scaled to the local y-density, so the cloud traces
  the distribution's shape (ggforce's sina). `width` is the maximum
  spread as a fraction of the category band (default `0.8`).

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = factor(cyl), y = mpg, position = position_jitter(width = 0.15))
```
