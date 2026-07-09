# Map an aesthetic to a statistic computed by a stat

Used inside a `mark_*()` encoding to reference a variable produced by
the layer's statistical transform rather than a raw data column, e.g.
`y = after_stat(count)` or `y = after_stat(density)`.

## Usage

``` r
after_stat(x)
```

## Arguments

- x:

  An expression in terms of the stat's computed variables.

## Value

Its argument (the marker is interpreted at compile time).
