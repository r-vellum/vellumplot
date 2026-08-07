# Set the easing of an animation's frames

`ease_aes()` chooses the easing function that shapes how the
interpolation fraction moves through each transition — e.g.
`"cubic-in-out"` starts and ends gently. By default it applies to every
interpolated aesthetic; the `position`/`color`/`size`/`alpha` arguments
override it for one class each, so marks can glide into place on a cubic
curve while their colour crossfades evenly.

## Usage

``` r
ease_aes(
  plot,
  ease = "linear",
  position = NULL,
  color = NULL,
  size = NULL,
  alpha = NULL,
  colour = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  carrying a
  [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md).

- ease:

  An easing name: `"linear"`, or a family
  (`quad`/`cubic`/`quart`/`quint`/`sine`/`expo`/`circ`/`back`/`elastic`/`bounce`)
  with a direction suffix (`-in`, `-out`, `-in-out`), e.g.
  `"cubic-in-out"`. Used for any class not given its own easing below.

- position, color, size, alpha:

  Optional per-class easings, named the same way as `ease`. `NULL` (the
  default) means "use `ease`". Each covers one class of drawn property:

  - `position` — x and y, and every other coordinate-space quantity (bar
    widths, angles, path vertices, text rotation). It also decides
    *when* discrete attributes flip, since those snap at the halfway
    point.

  - `color` — `color` and `fill`, including per-element fills.

  - `size` — `size`, `linewidth`, and radii.

  - `alpha` — opacity, **and the fade of marks entering or leaving** a
    state.

  `position` covers x and y together: the engine has a single positional
  curve, so the two axes cannot be eased apart.

- colour:

  An alias for `color`; supplying both is an error.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## See also

[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md),
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  transition_states(cyl) |>
  ease_aes("cubic-in-out")


# Marks settle into place gently, but the colour crossfade stays even.
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(gear)) |>
  transition_states(cyl) |>
  ease_aes("cubic-in-out", color = "linear")
```
