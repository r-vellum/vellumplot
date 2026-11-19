# Relative sizing

`rel()` gives a theme element size as a multiple of the size it
inherits, as in ggplot2.
`theme(axis.text = element_text(size = rel(0.8)))` draws the axis text
at 80% of the base text size, so it follows a later
`theme(text = element_text(size = ...))` or a preset's `base_size`.

## Usage

``` r
rel(x)
```

## Arguments

- x:

  A single positive number, the multiplier.

## Value

A number of class `rel`, for the `size` of
[`element_text()`](https://r-vellum.github.io/vellumplot/reference/element.md).

## Details

A `rel()` size is resolved against the parent element in the theme tree
(`text` -\> `axis.text` -\> `axis.text.x` and so on). Set on a slot that
already holds an absolute size, such as the `text` root, it scales that
size instead: `theme(text = element_text(size = rel(2)))` doubles every
text size.

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  theme(axis.text = element_text(size = rel(1.2)))
```
