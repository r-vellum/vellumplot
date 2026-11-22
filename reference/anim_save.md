# Write a keyframe animation to a file

`anim_save()` renders a `vellum_animation` (from
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md))
to an animated image, tweening the frames between keyframes and encoding
them in one parallel, streaming pass in vellum's Rust backend. The
format follows the file extension:

## Usage

``` r
anim_save(filename, animation)
```

## Arguments

- filename:

  Output path; `.gif`, `.png`, or `.svg`.

- animation:

  A `vellum_animation` (from
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)).

## Value

`filename`, invisibly.

## Details

- `.gif` — a looping GIF (universal compatibility).

- `.png` — an animated PNG (APNG; lossless, alpha).

- `.svg` — a single **animated SVG**: every frame is emitted as vector
  markup and shown in turn by a CSS step animation. It is
  resolution-independent (crisp at any size, in print and on retina) and
  honours `prefers-reduced-motion` (a reader who has asked their system
  not to animate gets the first frame, held).

## Choosing a format

Pick by scene complexity, not preference. An animated SVG emits *every
frame in full*, so its size grows with the number of marks times the
number of frames; a raster format (GIF/APNG) does not. So SVG wins
clearly on line art — a few moving marks, the common explanatory case —
and loses on a dense scatter, where a raster format is the right answer.
`anim_save()` says so when a `.svg` scene is dense enough that a raster
format would likely be smaller.

## See also

[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Four bars in every state, so each bar tweens into its next height.
d <- data.frame(t = rep(1:3, each = 4), g = rep(letters[1:4], 3),
                v = c(1, 3, 2, 4, 4, 1, 3, 2, 2, 4, 1, 3))
a <- vplot(d) |>
  mark_bar(x = g, y = v, fill = g) |>
  transition_states(t) |>
  animate()
anim_save("bars.gif", a)
anim_save("bars.svg", a) # resolution-independent
} # }
```
