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
animate(
  plot,
  nframes = 50,
  fps = 25,
  start_pause = 0,
  end_pause = 0,
  rewind = FALSE
)
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

  Total number of frames to render, a positive whole number. The pauses
  and the rewind are part of it, as in gganimate.

- fps:

  Frames per second (the playback rate written into the file).

- start_pause, end_pause:

  Number of frames to hold the first and the last frame for, before the
  animation starts and after it ends (default 0). A looping file (every
  format loops) then rests on the final picture before starting over:
  `end_pause = fps` holds it for a second.

- rewind:

  Play the animation backwards after it has played forwards, so the loop
  returns to its start smoothly instead of cutting (default `FALSE`).
  The frames are split between the two directions; the end pause sits
  between them and the start pause at both ends.

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

A faceted plot animates panel by panel: every panel of the whole data is
drawn in every frame (a state without rows for a panel leaves it empty
rather than dropping it), and a mark's identity includes its panel, so a
mark never slides from one panel into another – a row whose facet value
changes between states fades out of one panel and into the other. The
scales are frozen across frames, so per-panel scales
(`facet_wrap(scales = "free")`, an independent
[`resolve_scale()`](https://r-vellum.github.io/vellumplot/reference/resolve_scale.md))
are refused.

A mark found in only one of two adjacent states fades in or out (points,
bars and tiles, labels, lines and areas alike, with their effect
underlays such as
[`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md)) on
the alpha easing, and a point whose colour group changes slides to its
new place while its colour blends. A line, path or area whose number of
points changes morphs from one outline into the other. Where marks
overlap, the stacking order switches to the next state's halfway through
the transition. What cannot move switches whole halfway through:
decoration without an identity that comes and goes, or a line that
cannot be morphed (a broken line with missing values, mixed units).
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

A
[`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
label can use the same variables (VP-93):
`mark_text(data = data.frame(x = 3000, y = 40), x = x, y = y, label = "{round(frame_time)}", size = 60)`
draws the year large behind a gapminder bubble chart, counting on every
frame. A constant label or a column of labels may contain `{...}`; the
label is placed (and moves) as the mark it belongs to, and only its
string changes per frame. Repelled and rich-text labels are not
templated. An ordinary data label whose string differs between two
states takes the nearer state's string, switching halfway through.

## Pauses and rewind

`start_pause` and `end_pause` hold the first and last frame for that
many frames, and `rewind = TRUE` plays the animation back to the start,
as in gganimate. They come out of `nframes`:
`animate(nframes = 100, end_pause = 20)` moves for 80 frames and rests
on the final picture for 20. Every output honours them –
[`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
to GIF, APNG or SVG,
[`anim_frame()`](https://r-vellum.github.io/vellumplot/reference/anim_frame.md)'s
frame numbers, and the frame scrubber of an animated `vellumwidget`.

## See also

[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md),
[`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md),
[`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md),
[`anim_frame()`](https://r-vellum.github.io/vellumplot/reference/anim_frame.md)

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
