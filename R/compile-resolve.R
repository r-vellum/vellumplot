#' @include classes.R
NULL

# Infer a channel's variable type from its resolved values.
.infer_type <- function(v) {
  if (is.numeric(v)) "quantitative" else "nominal"
}

# Evaluate one layer's channel expressions against the data, returning a list
# with the resolved values per aesthetic, their (possibly inferred) types, and
# the row count.
.resolve_layer <- function(layer, data) {
  values <- list()
  types <- list()
  for (nm in names(layer@encoding)) {
    ch <- layer@encoding[[nm]]
    v <- rlang::eval_tidy(ch@expr, data = data)
    values[[nm]] <- v
    types[[nm]] <- if (nzchar(ch@type)) ch@type else .infer_type(v)
  }
  n <- if (length(values)) max(lengths(values)) else nrow(data)
  list(mark = layer@mark, values = values, types = types,
       params = layer@params, n = n)
}

# Resolve every layer of a spec.
.resolve_layers <- function(spec) {
  lapply(spec@layers, .resolve_layer, data = spec@data)
}

# The user-declared scale for an aesthetic, or NULL. `color` and `fill` share a
# colour scale; either declaration applies.
.scale_for <- function(spec, aesthetic) {
  match_aes <- if (aesthetic %in% c("color", "fill")) c("color", "fill") else aesthetic
  for (s in rev(spec@scales)) if (s@aesthetic %in% match_aes) return(s)
  NULL
}

# Pool the resolved values of an aesthetic across all layers that map it. For
# position channels this is x or y; returns NULL if no layer maps it.
.pool_values <- function(resolved, aesthetic) {
  vs <- lapply(resolved, function(L) L$values[[aesthetic]])
  vs <- vs[!vapply(vs, is.null, logical(1))]
  if (!length(vs)) return(NULL)
  vs
}

# Derive a default axis/legend title for an aesthetic from the first layer that
# maps it (its channel expression as text).
.default_title <- function(spec, aesthetic) {
  match_aes <- if (aesthetic %in% c("color", "fill")) c("color", "fill") else aesthetic
  for (layer in spec@layers) {
    for (a in match_aes) {
      ch <- layer@encoding[[a]]
      if (!is.null(ch)) return(rlang::as_label(rlang::quo_get_expr(ch@expr)))
    }
  }
  aesthetic
}
