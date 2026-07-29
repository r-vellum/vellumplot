# Write a keyframe animation to a file

`anim_save()` renders a `vellum_animation` (from
[`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md))
to an animated image, tweening the frames between keyframes and encoding
them in one parallel, streaming pass in vellum's Rust backend. The
format follows the file extension: `.gif` (looping GIF) or `.png`
(animated PNG / APNG).

## Usage

``` r
anim_save(filename, animation)
```

## Arguments

- filename:

  Output path; `.gif` or `.png`.

- animation:

  A `vellum_animation` (from
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)).

## Value

`filename`, invisibly.

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
} # }
```
