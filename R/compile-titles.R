#' @include classes.R coord.R compile-resolve.R print.R aes-registry.R
NULL

# ---------------------------------------------------------------------------
# The titles record: every axis / legend title of a plot, resolved once.
#
# A title is the user's `scale_*(name = )` if set, else its `labs()` entry, else
# the mapped expression of the first layer that maps it, else the aesthetic's name.
# An unmapped `y` on a bar chart reads "count". `.build_panels()` computes the
# record once and the scale trainers (so the axes and legends), the alt text and
# the lint summary all read it, so they cannot disagree about what a scale is
# called -- or, under `coord_flip()`, about which title sits on which axis.
# ---------------------------------------------------------------------------

# A single non-empty plain string, else NULL. A rich label stands in by its
# plain text (`.rich_plain()`): an md() label's words without the markup, a
# tex() label's formula read as Unicode text, not its TeX source (VP-172).
.plain_title <- function(x) {
  if (.is_rich(x)) {
    x <- .rich_plain(x)
  }
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) x else NULL
}

# The mapped expression of the first layer (in layer order) that maps
# `channel`, as text; NULL when no layer maps it. A `condition()` with a
# constant `if_true` maps nothing (VP-86).
.mapped_title <- function(spec, channel) {
  for (layer in spec@layers) {
    ch <- layer@encoding[[channel]]
    if (!is.null(ch) && !.cond_constant(ch, channel)) {
      return(.channel_label(ch))
    }
  }
  NULL
}

# Does a mark_node_pie() layer train the fill scale on its wedge categories
# (no layer mapping fill)? Its legend then needs no default title.
.pie_fill <- function(spec) {
  any(vapply(
    spec@layers,
    function(L) identical(L@mark, "node_pie") && is.null(L@encoding$fill),
    logical(1)
  ))
}

# The labs() entry for `channel`, or NULL.
.labs_title <- function(spec, channel) spec@labels[[channel]]

# Does an unmapped y read "count"? True when a bar layer counts rows (no layer
# maps y). From the resolved layers when the caller has them, else from the spec
# (a histogram or a y-less bar is a `"bar"` layer in both).
.y_counts <- function(spec, resolved = NULL) {
  y_mapped <- any(vapply(
    spec@layers,
    function(L) "y" %in% names(L@encoding),
    logical(1)
  ))
  if (y_mapped) {
    return(FALSE)
  }
  # a frequency polygon (`mark_freqpoly()`) counts too
  counts_bins <- any(vapply(
    spec@layers,
    function(L) identical(L@stat, "bin"),
    logical(1)
  ))
  if (!is.null(resolved)) {
    .has_bar(resolved) || counts_bins
  } else {
    counts_bins ||
      any(vapply(spec@layers, function(L) identical(L@mark, "bar"), logical(1)))
  }
}

# Build the titles record for `spec`. `resolved` (the pooled resolved layers) is
# optional: `.build_panels()` passes it; `plot_alt()` on a bare spec does not.
# Returns a list:
#   title    -- per scale key, the title the axis/legend draws (may be rich)
#   default  -- per scale key, the title ignoring `scale_*(name = )` (what an
#               identity scale, which draws no guide, carries)
#   text     -- per scale key, a plain-text title for a description, or NULL
#               when the aesthetic is neither mapped nor named
#   named    -- per scale key, the title the user gave (`scale_*(name = )`,
#               else `labs()`), or NULL: no fallback to the mapped expression.
#               A bivariate legend titles its axes itself, so it only carries a
#               title the user asked for
#   flip     -- TRUE under coord_flip()
#   h, v     -- the scale key drawn on the horizontal / vertical axis
.plot_titles <- function(spec, resolved = NULL) {
  keys <- names(.AES_REGISTRY)
  counts <- .y_counts(spec, resolved)
  default <- list()
  title <- list()
  text <- list()
  named <- list()
  for (key in keys) {
    # The channel whose labs() entry / mapped expression supplies the default
    # (edge width reads the `linewidth` channel). See .AES_REGISTRY.
    channel <- .AES_REGISTRY[[key]]$title
    lab <- .labs_title(spec, channel)
    mapped <- .mapped_title(spec, channel)
    fallback <- if (key == "y" && counts) {
      "count"
    } else if (is.null(mapped) && key == "fill" && .pie_fill(spec)) {
      NULL # the wedge categories name themselves (VP-123)
    } else {
      mapped %||% channel
    }
    default[[key]] <- lab %||% fallback
    sc <- .scale_for(spec, key)
    name <- if (!is.null(sc)) sc@name else NULL
    title[key] <- list(name %||% default[[key]])
    named[key] <- list(name %||% lab)
    text[key] <- list(
      .plain_title(name) %||%
        .plain_title(lab) %||%
        mapped %||%
        (if (key == "y" && counts) "count")
    )
  }
  flip <- identical(.coord_of(spec)@kind, "flip")
  structure(
    list(
      title = title,
      default = default,
      text = text,
      named = named,
      flip = flip,
      h = if (flip) "y" else "x",
      v = if (flip) "x" else "y"
    ),
    class = "vp_titles"
  )
}
