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

  Total number of frames to render, a positive whole number.

- fps:

  Frames per second (the playback rate written into the file).

## Value

A `vellum_animation`.

## Details

Per-state statistics (e.g. a smooth) are recomputed per keyframe, but no
scale domain retrains — the frozen scales are the observable proof the
animation stayed non-reactive (axis breaks do not move between frames).

Marks tween from one state to the next only when the two keyframes draw
the same layers and groups, and each line or path keeps its number of
points. A state that adds or drops a colour group, say, cannot be
matched mark for mark: its frames cut to the next state halfway through
the transition, and a line whose point count changes switches whole at
the same moment. `animate()` warns (once per call) when that will
happen, naming the states. Within a group, points carrying a `data_id`
match by it, so single points may enter and leave without breaking the
tween.

Give each transition enough frames to be seen: `nframes` is spread over
all the states, so 40 states in 80 frames leave two frames per state and
nothing visibly moves.

## See also

[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md),
[`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md),
[`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Four bars in every state, so each bar tweens into its next height.
d <- data.frame(t = rep(1:3, each = 4), g = rep(letters[1:4], 3),
                v = c(1, 3, 2, 4, 4, 1, 3, 2, 2, 4, 1, 3))
vplot(d) |>
  mark_bar(x = g, y = v, fill = g) |>
  transition_states(t) |>
  animate(nframes = 60, fps = 25)
} # }
```
