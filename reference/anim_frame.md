# Save one frame of an animation as a still image

`anim_frame()` writes a single frame of a `vellum_animation` to a PNG,
SVG or PDF file: the poster for an animation, its thumbnail, or the
moment to put in print. The frame is drawn exactly as it appears in the
animation – the same frozen scales and colours, the same tween, the same
frame variables in the title – so a poster matches the animation it
stands for, which a separately built static plot (its scales trained on
one state) need not.

## Usage

``` r
anim_frame(filename, animation, frame = NULL, scale = 1)
```

## Arguments

- filename:

  Output path; `.png`, `.svg` or `.pdf`.

- animation:

  A `vellum_animation` (from
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)).

- frame:

  The frame to save: a whole number from 1 to the number of frames. The
  default is the frame a reader who asks for reduced motion sees: the
  last frame when the animation ends on its final state (a reveal,
  [`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md),
  a non-looping
  [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md)),
  else the first.

- scale:

  Resolution multiplier (default 1), as in
  [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md):
  `2` doubles the pixels of a PNG at the same layout.

## Value

`filename`, invisibly.

## Details

Frames are numbered as in the files
[`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
writes, pauses and rewind included (see
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)).

## See also

[`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md),
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)

## Examples

``` r
if (FALSE) { # \dontrun{
d <- data.frame(t = rep(1:3, each = 4), g = rep(letters[1:4], 3),
                v = c(1, 3, 2, 4, 4, 1, 3, 2, 2, 4, 1, 3))
a <- vplot(d) |>
  mark_bar(x = g, y = v, fill = g) |>
  transition_states(t, wrap = FALSE) |>
  animate()
anim_frame("poster.png", a) # the final state
anim_frame("middle.svg", a, frame = 25)
} # }
```
