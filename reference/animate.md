# Build a keyframe animation from a plot

`animate()` compiles a plot carrying a
[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md)
into one keyframe scene per state, training the scales once over all
states and freezing them, and returns a `vellum_animation`. Write it
with
[`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md);
printing it shows the first keyframe as a preview.

## Usage

``` r
animate(plot, nframes = 50, fps = 25)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  with a
  [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md)
  (and optionally an
  [`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md)).

- nframes:

  Total number of frames to render.

- fps:

  Frames per second (the playback rate written into the file).

## Value

A `vellum_animation`.

## Details

Per-state statistics (e.g. a smooth) are recomputed per keyframe, but no
scale domain retrains — the frozen scales are the observable proof the
animation stayed non-reactive (axis breaks do not move between frames).

## See also

[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md),
[`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md),
[`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)

## Examples

``` r
if (FALSE) { # \dontrun{
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  transition_states(cyl) |>
  animate(nframes = 50, fps = 25)
} # }
```
