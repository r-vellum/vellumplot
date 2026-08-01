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
a <- vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  transition_states(cyl) |>
  animate()
anim_save("cyl.gif", a)
anim_save("cyl.svg", a) # resolution-independent
} # }
```
