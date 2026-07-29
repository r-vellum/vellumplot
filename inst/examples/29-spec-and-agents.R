# 29 — Portable specs, agent tooling, interop, and provenance
#
# A plot is a serializable *document*. This script shows the round-trip, the
# JSON wire format, the grounding + validation an LLM/MCP agent uses, the
# Vega-Lite bridge, and data-traceable provenance.

library(vellumplot)

df <- data.frame(
  wt = mtcars$wt,
  mpg = mtcars$mpg,
  cyl = factor(mtcars$cyl),
  model = rownames(mtcars)
)

p <- vplot(df) |>
  mark_point(x = wt, y = mpg, color = cyl) |>
  mark_smooth(x = wt, y = mpg) |>
  labs(title = "Fuel economy")

# --- 1. Serialize to a spec and back -------------------------------------
spec <- as_spec(p)
str(spec$layers[[1]], max.level = 2)
p2 <- from_spec(spec) # recompiles to the same plot

# JSON wire format (what an agent emits / consumes):
json <- spec_to_json(p)
cat(substr(json, 1, 200), "...\n")
p3 <- spec_from_json(json)

# --- 2. Ground and validate an agent-generated spec ----------------------
spec_fields(df) # columns + inferred encoding types for the model

# a stub responder standing in for a model call (see mcp_serve() for the real
# agent loop):
responder <- function(payload) {
  spec_to_json(vplot(df) |> mark_point(x = wt, y = mpg))
}
p_ai <- vplot_ask("scatter of weight against mpg", df, responder)

# a hallucinated column comes back as a structured diagnostic, not a traceback:
bad <- spec
bad$layers[[1]]$encoding$x$field <- "weight"
d <- spec_diagnose(bad, data = df)
d$ok
d$diagnostics[[1]]

# --- 3. Vega-Lite interop -------------------------------------------------
vl <- spec_to_vegalite(vplot(df) |> mark_point(x = wt, y = mpg, color = cyl))
str(vl, max.level = 2)

# --- 4. Provenance: trace every element back to its data -----------------
pj <- provenance_join(p)
print(pj[, c("id", "mark", "n_rows")])
pj$rows[[1]] # the source-data rows behind the first element

# a self-verifying SVG: the figure carries a fingerprint of its data
svg <- plot_svg(vplot(df) |> mark_point(x = wt, y = mpg), manifest = TRUE)
plot_verify(svg, df)$ok # TRUE — matches the data it was drawn from
plot_verify(svg, df[1:5, ])$ok # FALSE — different data
