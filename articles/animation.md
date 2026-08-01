# Animation

An animated plot is a sequence of still plots played in quick
succession. In vellumplot the stills are **keyframes** — one compiled
plot per *state* — and the frames between them are **tweened**: their
marks are interpolated so a point glides from where it sits in one state
to where it sits in the next.

This is *non-reactive* keyframe animation. The states are decided up
front (the levels of a column), and the scales are trained **once over
every state and then frozen**. Nothing retrains between frames: the axis
breaks, the colour ramp and the size legend are the same in every frame,
so the eye can read motion against a fixed backdrop. Animation is not a
live/reactive runtime — it is a fixed set of states compiled ahead of
time.

## The pipeline

Four verbs, added to any plot:

- `transition_states(states)` — marks the column whose levels are the
  states.
- `ease_aes(ease)` — chooses the easing (how the interpolation
  accelerates).
- `animate(nframes, fps)` — compiles the keyframes and returns a
  `vellum_animation`.
- `anim_save(file, animation)` — tweens and encodes the frames to a
  file.

``` r

library(gapminder)

vplot(gapminder) |>
  mark_point(x = gdpPercap, y = lifeExp, size = pop, color = continent) |>
  scale_x_continuous(trans = "log10") |>
  transition_states(year, transition_length = 2, state_length = 1) |>
  ease_aes("cubic-in-out") |>
  animate(nframes = 100, fps = 25) |>
  (\(a) anim_save("gapminder.gif", a))()
```

The output format follows the file extension: `.gif` for a looping
animated GIF (universally viewable), `.png` for an animated PNG (APNG —
lossless, with transparency), or `.svg` for an animated SVG. The tween,
the parallel render across CPU cores, and the streaming encode all
happen in one pass in vellum’s Rust backend.

### Animated SVG, and choosing a format

`.svg` writes a single **animated SVG**: every frame is emitted as
vector markup and shown in turn by a CSS step animation. Two things make
it worth reaching for. It is **resolution-independent** — the same file
is crisp in a slide, on a retina screen, and in print, which no raster
format is. And it honours **`prefers-reduced-motion`**: a reader who has
asked their system not to animate is shown the first frame, held, with
no extra work from you.

The trade-off is size. Because every frame is emitted in full, an
animated SVG grows with the number of marks times the number of frames,
where a GIF does not. So it wins clearly on **line art** — a handful of
moving marks, the common explanatory case — and loses on a **dense
scatter**, where a raster format is the right answer. Choose by mark
count, not by preference;
[`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
says so when a `.svg` scene is dense enough that a raster format would
likely be smaller. Serve it gzipped (`.svgz`, or a web server with
compression) to shrink it several-fold.

``` r

p |> animate() |> (\(a) anim_save("draw.svg", a))()
```

## What a keyframe looks like

[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
compiles one keyframe per state. Each is an ordinary scene, so you can
inspect one directly — here the first state of an `mtcars`-by-cylinder
animation:

``` r

a <- vplot(mtcars) |>
  mark_point(x = wt, y = mpg, size = hp, color = factor(cyl)) |>
  transition_states(cyl) |>
  ease_aes("cubic-in-out") |>
  animate(nframes = 60, fps = 20)

# a@scenes holds one vellum scene per state; render the first as a still.
vellum::render(a@scenes[[1]], tempfile(fileext = ".png"))
```

## Frozen scales — the non-reactive guardrail

Because the scales are trained once over the pooled states, the domain
is the *union* across every state. A point that is extreme in one state
and typical in another keeps a stable position reference throughout: the
axes never jump. This is the observable proof that the animation stayed
non-reactive — compare the axis breaks of any two keyframes and they are
identical, even though the two states’ data cover very different ranges.

Per-state *statistics* still recompute (a smooth is refit per state, a
bin count re-tallied), but no scale *domain* retrains. Retraining a
scale in response to the data of the moment would be a reactive runtime,
which vellumplot is deliberately not.

## Easing

[`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md)
shapes how the interpolation fraction moves through each transition.
`"linear"` is a constant rate; a family name with a direction suffix
eases in, out, or both:

- families: `quad`, `cubic`, `quart`, `quint`, `sine`, `expo`, `circ`,
  `back`, `elastic`, `bounce`
- directions: `-in`, `-out`, `-in-out`

`"cubic-in-out"` — slow start, quick middle, slow finish — reads as the
most natural motion for most plots.

## What tweens, and what does not

Between two keyframes, an element’s **position**, **size**, **alpha**,
and **colour** interpolate — colour perceptually, in Oklab, so a hue
transition stays vivid instead of passing through grey. Discrete
attributes (a marker shape, a line type, a text label) do not
interpolate continuously; they snap at the midpoint.

When the set of elements changes between states — a country that only
has data in some years — give the animated mark a `data_id` identifying
each element across states (`mark_point(..., data_id = country)`).
Elements that appear **enter** (fade in) and elements that leave
**exit** (fade out), while the ones present in both tween normally.
Without a `data_id` a stable element set is assumed (elements are
matched by position). Faceted animations are outside the current scope.

## Continuous time

[`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md)
is the continuous-time sibling of
[`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md):
it treats the column as a numeric time and allocates frames **in
proportion to each time gap**, so the animation runs at a constant rate
even when the times are unevenly spaced. Time flows once — no pause on a
state, no wrap.

``` r

vplot(gapminder) |>
  mark_point(x = gdpPercap, y = lifeExp, size = pop, color = continent) |>
  scale_x_continuous(trans = "log10") |>
  transition_time(year) |>
  animate(nframes = 100, fps = 25) |>
  (\(a) anim_save("gapminder.gif", a))()
```

## Reveal

[`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md)
is a different kind of animation: instead of tweening between states, it
**wipes the plot into view** left to right — the classic “line draws
itself”. The full plot is compiled once and a clip rectangle grows
across the panel, revealing the marks in order. The axes stay put; only
the data wipes in.

``` r

vplot(data.frame(t = 1:100, y = cumsum(rnorm(100)))) |>
  mark_line(x = t, y = y) |>
  transition_reveal(t) |>
  animate(nframes = 80, fps = 25) |>
  (\(a) anim_save("draw.gif", a))()
```

## Timing

`transition_length` and `state_length` are *relative* durations: how
long the moving segments last versus the held pauses on each state.
`nframes` is the total frame count and `fps` the playback rate, so the
animation runs for `nframes / fps` seconds. `wrap = TRUE` (the default)
loops the last state back to the first.
