#' @include classes.R coord.R compile-resolve.R print.R
NULL

# ---------------------------------------------------------------------------
# The titles record: every axis / legend title of a plot, resolved once.
#
# A title is the user's `scale_*(name = )` if set, else a `labs()` entry (under
# any alias of the aesthetic: `labs(fill = )` titles the colour scale), else the
# mapped expression of the first layer that maps it, else the aesthetic's name.
# An unmapped `y` on a bar chart reads "count". `.build_panels()` computes the
# record once and the scale trainers (so the axes and legends), the alt text and
# the lint summary all read it, so they cannot disagree about what a scale is
# called -- or, under `coord_flip()`, about which title sits on which axis.
# ---------------------------------------------------------------------------

# Scale key -> the channel whose labs() entry / mapped expression supplies the
# default title. Edge width shares the `linewidth` channel with the line-width
# scale (they differ by the marks that read them, not by the channel).
.TITLE_KEYS <- c(
  x = "x",
  y = "y",
  color = "color",
  size = "size",
  shape = "shape",
  pattern = "pattern",
  edge_width = "linewidth",
  linewidth = "linewidth",
  alpha = "alpha",
  linetype = "linetype",
  edge_color = "edge_color",
  edge_alpha = "edge_alpha",
  edge_linetype = "edge_linetype"
)

# A single non-empty plain string (a rich md() title has no reliable plain-text
# form, so it never stands in for a description), else NULL.
.plain_title <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) x else NULL
}

# The mapped expression of the first layer (in layer order) that maps `channel`
# or one of its aliases, as text; NULL when no layer maps it.
.mapped_title <- function(spec, channel) {
  match_aes <- .aes_aliases(channel)
  for (layer in spec@layers) {
    for (a in match_aes) {
      ch <- layer@encoding[[a]]
      if (!is.null(ch)) {
        return(.channel_label(ch))
      }
    }
  }
  NULL
}

# The labs() entry for `channel` under its first alias that has one, or NULL.
.labs_title <- function(spec, channel) {
  for (a in .aes_aliases(channel)) {
    lab <- spec@labels[[a]]
    if (!is.null(lab)) {
      return(lab)
    }
  }
  NULL
}

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
  if (!is.null(resolved)) {
    .has_bar(resolved)
  } else {
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
#   flip     -- TRUE under coord_flip()
#   h, v     -- the scale key drawn on the horizontal / vertical axis
.plot_titles <- function(spec, resolved = NULL) {
  keys <- names(.TITLE_KEYS)
  counts <- .y_counts(spec, resolved)
  default <- list()
  title <- list()
  text <- list()
  for (key in keys) {
    channel <- .TITLE_KEYS[[key]]
    lab <- .labs_title(spec, channel)
    mapped <- .mapped_title(spec, channel)
    fallback <- if (key == "y" && counts) "count" else mapped %||% channel
    default[[key]] <- lab %||% fallback
    sc <- .scale_for(spec, key)
    name <- if (!is.null(sc)) sc@name else NULL
    title[key] <- list(name %||% default[[key]])
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
      flip = flip,
      h = if (flip) "y" else "x",
      v = if (flip) "x" else "y"
    ),
    class = "vp_titles"
  )
}
