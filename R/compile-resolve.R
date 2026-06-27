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

# A bar with no `y` encoding counts rows per x (per x*colour, if colour is
# mapped) — the only built-in stat in v1 (general transforms are a later stage).
.bar_count_fixup <- function(L) {
  if (!identical(L$mark, "bar") || !is.null(L$values$y)) return(L)
  x <- L$values$x
  if (is.null(x)) return(L)
  if (is.numeric(x)) {
    cli::cli_abort(c(
      "{.fn mark_bar} on a continuous x needs an explicit {.field y}.",
      i = "Binning a continuous variable into bars is not yet available."
    ))
  }
  grp <- L$values$color %||% L$values$fill
  if (is.null(grp)) {
    tab <- table(as.character(x))
    L$values$x <- names(tab)
    L$values$y <- as.numeric(tab)
    L$n <- length(tab)
  } else {
    agg <- as.data.frame(table(x = as.character(x), g = as.character(grp)),
                         stringsAsFactors = FALSE)
    agg <- agg[agg$Freq > 0, , drop = FALSE]
    L$values$x <- agg$x
    if (!is.null(L$values$color)) L$values$color <- agg$g else L$values$fill <- agg$g
    L$values$y <- agg$Freq
    L$n <- nrow(agg)
  }
  L$types$y <- "quantitative"
  L
}

# Resolve every layer of a spec against a given data frame (a facet panel's
# subset, or the whole data).
.resolve_on <- function(spec, data) {
  lapply(spec@layers, function(layer) .bar_count_fixup(.resolve_layer(layer, data)))
}

# Resolve every layer of a spec against its full data.
.resolve_layers <- function(spec) .resolve_on(spec, spec@data)

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
