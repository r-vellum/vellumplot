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

## Which mark becomes which

Marks tween by **identity**, as gganimate matches by group: each mark of
one state moves into the mark that *is the same thing* in the next
state, however the rows are ordered and whatever groups each state
contains. A mark's identity is

1.  its `data_id`, when the layer declares one
    (`mark_point(data_id = country)`): the way to follow individual
    observations;

2.  otherwise its group – the `group` aesthetic if mapped, else the
    combination of the layer's discrete mapped aesthetics (`color`,
    `fill`, `shape`, ..., and a discrete `x`, `y` or `label`) – so a bar
    race's bars, filled by country, slide between ranks keeping their
    colours;

3.  and within a group, the order of its rows – the last resort, enough
    when each group has one mark per state, but rows of one group pair
    up by position. Give such a layer a `data_id` (or a `group`) when
    its rows are individual observations whose order may change.

A line, path or area is one mark per series and is matched by its group.

A mark found in only one of two adjacent states fades in or out (points,
bars and tiles, labels, lines and areas alike) on the alpha easing, and
a point whose colour group changes slides to its new place while its
colour blends. What cannot move switches whole halfway through the
transition: a matched line or path whose number of points changes, and
decoration without an identity (an effect underlay) that comes and goes.
`animate()` warns (once per call) when that will happen, naming the
states.

Give each transition enough frames to be seen: `nframes` is spread over
all the states, so 40 states in 80 frames leave two frames per state and
nothing visibly moves.

## Frame variables

A title, subtitle or caption (set with
[`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md)) may
embed frame variables in braces, glue-style, as in gganimate:
`labs(title = "Year: {frame_time}")`. Each `{...}` is an R expression
evaluated per frame with the variables below in scope (and the
environment `animate()` was called from behind them), so
`"{round(frame_time)}"` or `"{format(frame_time, '%Y')}"` work; write
`{{` and `}}` for literal braces. The label is updated on **every
frame**, not only at keyframes: a `{frame_time}` title counts through
the in-between times while the marks move.

- every transition: `frame` (the frame number), `nframes`, `progress`
  (`frame / nframes`);

- [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md):
  `closest_state`, `previous_state`, `next_state` (the state the frame
  is nearest to, comes from and goes to; all three are the held state
  during a pause) and `transitioning` (`TRUE` while moving).
  `closest_state` switches halfway through a transition, on the position
  easing, when the marks' discrete attributes do;

- [`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md):
  `frame_time`, the time the frame has reached (on the position easing,
  so it agrees with where the marks are), of the time column's class
  (`Date`, `POSIXct`, or double);

- [`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md):
  `frame_along`, the `along` value revealed so far.

Data labels
([`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md))
take their text from the data and are not templated: a label whose
string differs between two states takes the nearer state's string,
switching halfway through.

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
