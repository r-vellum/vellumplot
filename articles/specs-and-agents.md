# Specs, agents, and interoperability

Because a `PlotSpec` is plain, inspectable data behind a compiler (see
[the compiler
article](https://r-vellum.github.io/vellumplot/articles/the-compiler.md)),
a plot can be turned into a portable *document* — serialized, generated
by a model, translated to another engine, and traced back to its data.
This article covers the four capabilities that build on that.

## The spec round-trip

[`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
turns a plot into a nested list;
[`from_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
rebuilds it. The JSON form is the wire format.

``` r

p <- vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl)) |>
  labs(title = "Fuel economy")

spec <- as_spec(p)
str(spec$layers[[1]], max.level = 2)
#> List of 3
#>  $ mark       : chr "point"
#>  $ encoding   :List of 3
#>   ..$ x    :List of 1
#>   ..$ y    :List of 1
#>   ..$ color:List of 1
#>  $ stat_params:List of 1
#>   ..$ auto: logi FALSE

json <- spec_to_json(p)
identical(as_spec(spec_from_json(json))$layers, as_spec(p)$layers)
#> [1] TRUE
```

The serializer covers the **encoding-level grammar** exactly. Anything a
portable document cannot carry — custom transform *functions*,
paint/pattern fills, hand-drawn
[`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
geometry, secondary axes, `sf` CRS objects, per-layer data — is
**refused** with a classed `vellumplot_unserializable` error naming the
slot, never silently dropped. (Theme is the one exception: the preset
name and scalar settings round-trip; custom element overrides drop with
a warning, since styling is orthogonal to the data spec.) Data is
inlined when small and stored by reference (a content hash + column
schema) otherwise.

## Generating plots from an agent

The serializable spec makes vellumplot a safe target for LLM / agent
generation: the agent emits *data* (a spec) that you validate and render
with your own trusted code — never
[`eval()`](https://rdrr.io/r/base/eval.html)-d R. Three pieces support
the loop.

[`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md)
grounds the model in the real columns and inferred types:

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

[`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
validates a generated spec against the data and returns structured
diagnostics — the machine-readable repair loop — instead of a traceback:

``` r

bad <- spec
bad$layers[[1]]$encoding$x$field <- "weight" # a hallucinated column
d <- spec_diagnose(bad, data = mtcars)
d$ok
#> [1] FALSE
d$diagnostics[[1]]
#> $severity
#> [1] "error"
#> 
#> $field
#> [1] "weight"
#> 
#> $message
#> [1] "Unknown field 'weight'."
#> 
#> $hint
#> [1] "Available fields: mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb."
```

[`mcp_serve()`](https://r-vellum.github.io/vellumplot/reference/mcp_serve.md)
runs a pure-R [Model Context Protocol](https://modelcontextprotocol.io)
server exposing `get_schema`, `list_fields`, and `render_spec` tools.
Point an agent at the bundled launcher:

``` sh
Rscript -e 'vellumplot::mcp_serve()'
```

[`vplot_ask()`](https://r-vellum.github.io/vellumplot/reference/vplot_ask.md)
is a model-agnostic convenience: give it a `responder` function
(wrapping any model or MCP client) and it assembles the grounding
payload and validates the result.

## Vega-Lite interoperability

[`spec_to_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
and
[`spec_from_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
translate a plot to and from a
[Vega-Lite](https://vega.github.io/vega-lite/) specification.

``` r

vl <- spec_to_vegalite(
  vplot(mtcars) |> mark_point(x = wt, y = mpg, color = cyl)
)
str(vl, max.level = 2)
#> List of 4
#>  $ $schema : chr "https://vega.github.io/schema/vega-lite/v5.json"
#>  $ data    :List of 1
#>   ..$ values:List of 32
#>  $ mark    :List of 1
#>   ..$ type: chr "point"
#>  $ encoding:List of 3
#>   ..$ x    :List of 1
#>   ..$ y    :List of 1
#>   ..$ color:List of 1
```

### Coverage

The bridge covers a documented subset; anything it cannot map is
reported (a warning), never silently diverged.

| vellumplot | Vega-Lite | mapped |
|----|----|----|
| `mark_point/line/area/bar/rule/text/boxplot/rug/errorbar` | `point/line/area/bar/rule/text/boxplot/tick/errorbar` | ✅ both ways |
| `mark_tile`, `mark_pie`/`donut`, `mark_step` | `rect`, `arc`, `line` (`step-after`) | ✅ export |
| encodings `x/y/color/fill/size/shape/alpha/label`, `xmin/xmax/ymin/ymax` | `x/y/color/size/shape/opacity/text`, `x2/y2` | ✅ |
| `scale_*` domain, `trans` (log/sqrt), categorical range, `name` | `encoding.scale.{domain,type,range}`, `.title` | ✅ |
| `mark_histogram` (`stat="bin"`) | `bin` + `aggregate:"count"` | ✅ both ways |
| `facet_wrap` / `facet_grid` | `facet` / `row` + `column` | ✅ |
| inline data, plot title | `data.values`, `title` | ✅ |
| layer effects, patterns, sketch | — | ⚠ dropped (reported) |
| polar / flipped coordinates | — | ⚠ dropped (reported) |
| statistical stats (`smooth`, `density`, …) | — | ⚠ dropped (reported) |

## Provenance: tracing a plot back to its data

Every drawn element keeps a stable id tying it to the data rows that
produced it.
[`provenance_join()`](https://r-vellum.github.io/vellumplot/reference/provenance_join.md)
surfaces that alongside the element’s device-pixel geometry:

``` r

pj <- provenance_join(p)
pj[, c("id", "mark", "n_rows")]
#>                 id  mark n_rows
#> 1 layer-1-point-g1 point      7
#> 2 layer-1-point-g2 point     11
#> 3 layer-1-point-g3 point     14
pj$rows[[1]] # the source-data rows behind the first element
#> [1]  1  2  4  6 10 11 30
```

A figure can also carry a fingerprint of its data, so it verifies
against the data it was drawn from:

``` r

svg <- plot_svg(p, manifest = TRUE)
plot_verify(svg, mtcars)$ok # TRUE
#> [1] TRUE
plot_verify(svg, mtcars[1:5, ])$ok # FALSE — different data
#> [1] FALSE
```

[`provenance_payload()`](https://r-vellum.github.io/vellumplot/reference/provenance_payload.md)
exposes the same element → rows mapping as a payload a host (such as
vellumwidget) can use for click-to-source interaction.
