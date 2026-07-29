# Summarise a data frame's fields for a model

`spec_fields()` returns one row per column of `data` with its name,
inferred encoding `type` (`quantitative` / `nominal` / `ordinal` /
`temporal`), R class, distinct-value count, and a few example values. It
is the *grounding* step for LLM plot generation
([`vplot_ask()`](https://r-vellum.github.io/vellumplot/reference/vplot_ask.md),
the MCP `list_fields` tool): handing a model the real vocabulary up
front is what prevents hallucinated column names, and
[`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
then validates against exactly these.

## Usage

``` r
spec_fields(data, n_examples = 3L)
```

## Arguments

- data:

  A data frame.

- n_examples:

  How many example values to show per field (default 3).

## Value

A data frame with columns `name`, `type`, `class`, `n_unique`,
`examples`.

## See also

[`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md),
[`spec_schema()`](https://r-vellum.github.io/vellumplot/reference/spec_schema.md)

## Examples

``` r
spec_fields(mtcars)
#>    name         type   class n_unique            examples
#> 1   mpg quantitative numeric       25    21.0, 22.8, 21.4
#> 2   cyl quantitative numeric        3             6, 4, 8
#> 3  disp quantitative numeric       27       160, 108, 258
#> 4    hp quantitative numeric       22        110, 93, 175
#> 5  drat quantitative numeric       22    3.90, 3.85, 3.08
#> 6    wt quantitative numeric       29 2.620, 2.875, 2.320
#> 7  qsec quantitative numeric       30 16.46, 17.02, 18.61
#> 8    vs quantitative numeric        2                0, 1
#> 9    am quantitative numeric        2                1, 0
#> 10 gear quantitative numeric        3             4, 3, 5
#> 11 carb quantitative numeric        6             4, 1, 2
```
