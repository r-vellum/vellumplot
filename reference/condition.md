# Conditional encoding: style by selection membership

Use `condition()` as the value of a mark aesthetic to make that
aesthetic depend on whether an element is in a
[selection](https://r-vellum.github.io/vellumplot/reference/select_point.md):
members get `if_true`, non-members get `if_false`. This is the general
form of highlight-on-interaction.

## Usage

``` r
condition(selection, if_true, if_false, empty = TRUE)
```

## Arguments

- selection:

  The selection name (a string) that drives the condition.

- if_true:

  The aesthetic value for selection members — a column expression
  (data-masked) or a constant.

- if_false:

  The value for non-members — a constant, a column expression
  (data-masked, one value per row), or omitted to use the theme's dim
  appearance. For `color` / `fill`, a column whose values are levels
  (or, on a continuous scale, numbers) of the aesthetic's scale is taken
  through that scale, as `if_true` is; any other value is read as a
  colour, and a row whose value is neither gets the theme dim.

- empty:

  Whether an empty selection matches all elements (`TRUE`, default).

## Value

Used only inside a `mark_*()` encoding; not called directly.

## Details

`condition()` is recognised where it appears in a `mark_*()` encoding;
calling it directly is an error. The `if_true` branch is drawn on a
static render and trains scales exactly as the equivalent plain
encoding, so `color = condition("s", g, "grey80")` produces the same
colour scale and legend as `color = g`. With `empty = TRUE` (the
default) an empty selection matches everything, so a static / initial
render shows `if_true` for all elements and interaction dims non-members
to `if_false`.

## See also

[`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md),
[`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
