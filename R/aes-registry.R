#' @include classes.R
NULL

# ---------------------------------------------------------------------------
# The aesthetic registry: one row per trained scale, read by every consumer that
# used to keep its own list of aesthetics -- the trainer (`.train_scales()`), the
# legend builder (`.legend_guides()`), the provenance scale-ref lookup
# (`.layer_channels()`) and the titles record (`.plot_titles()`). Adding a scale
# means adding a row here; the consumers follow.
#
# Fields (one record per scale key, in legend order):
#   name        the scale key (`scales[[name]]`, `scale_*()` aesthetic)
#   aliases     the resolved-data channels that feed the scale (`color` and
#               `fill` share one colour scale; edge width and line width both
#               read the `linewidth` channel)
#   marks       for a channel two scales share: the marks whose layers this
#               scale serves (`include`) or does not (`exclude`); NULL = all
#   title       the channel whose labs() entry / mapped expression supplies the
#               default title (see .plot_titles())
#   trainer     the per-aesthetic trainer's name, `fn(spec, resolved, titles = )`;
#               NULL for the position scales, which `.train_axis()` trains
#   guide       the legend it draws, NULL for an axis. `"color"` is
#               resolved per scale type (bar, steps, swatches, bivariate) by
#               `.legend_guides()`
#   merge       the partners its legend may fold into *and disappear*, each
#               with the trained fields that must be identical for the fold. A
#               field set may be keyed by the scale's `type`, with `.default`
#               for the rest (the edge colour guide folding into the node one)
#   legend_merge TRUE when its legend joins ggplot2's legend merge: any two
#               such legends with the same title and the same keys (values and
#               labels) draw as one legend whose keys carry every encoding
#               (see `.merge_legend_guides()`, R/compile-guides.R)
#   provenance  TRUE when an element's provenance carries a scale-ref for it
#   types       the `type`s a trained scale under this key may have (checked by
#               `trained_scale()`, R/trained-scale.R)
# ---------------------------------------------------------------------------

.aes_entry <- function(
  name,
  aliases = name,
  marks = NULL,
  title = aliases[[1]],
  trainer = NULL,
  guide = NULL,
  merge = list(),
  legend_merge = FALSE,
  provenance = TRUE,
  types = .CONTINUOUS_TYPES
) {
  list(
    name = name,
    aliases = aliases,
    marks = marks,
    title = title,
    trainer = trainer,
    guide = guide,
    merge = merge,
    legend_merge = legend_merge,
    provenance = provenance,
    types = types
  )
}

# The marks whose emitter varies stroke width *per element*, and so read the
# `linewidth` scale. Everywhere else a mapped `linewidth` still belongs to the
# edge-width scale (`mark_edges()` and friends), which is trained separately --
# the two share the channel name but not the scale, so a graph can map edge
# width and a line's width to different variables in one plot.
.LINEWIDTH_MARKS <- c("line", "step", "segment")

# The types a trained scale comes in, by aesthetic family. A keyed aesthetic
# (shape, linetype, pattern) is always discrete; a sized one (size, alpha, the
# widths) is continuous; colour and position take more.
.POSITION_TYPES <- c("continuous", "log10", "discrete", "binned")
.COLOUR_TYPES <- c("continuous", "discrete", "binned", "identity")
.CONTINUOUS_TYPES <- c("continuous", "identity")
.DISCRETE_TYPES <- c("discrete", "identity")

.AES_REGISTRY <- list(
  x = .aes_entry("x", types = .POSITION_TYPES),
  y = .aes_entry("y", types = .POSITION_TYPES),
  color = .aes_entry(
    "color",
    aliases = c("color", "fill"),
    trainer = ".train_colour",
    guide = "color",
    types = .COLOUR_TYPES,
    # A discrete colour legend merges with any other discrete legend (shape,
    # linetype, ...) of the same title and keys; a continuous colour bar turns
    # into keys when it merges with a size/alpha/linewidth legend.
    legend_merge = TRUE
  ),
  size = .aes_entry(
    "size",
    trainer = ".train_size",
    guide = "size",
    legend_merge = TRUE
  ),
  shape = .aes_entry(
    "shape",
    trainer = ".train_shape",
    guide = "shape",
    legend_merge = TRUE,
    types = .DISCRETE_TYPES
  ),
  pattern = .aes_entry(
    "pattern",
    trainer = ".train_pattern",
    guide = "pattern",
    types = "discrete"
  ),
  edge_width = .aes_entry(
    "edge_width",
    aliases = "linewidth",
    marks = list(exclude = .LINEWIDTH_MARKS),
    trainer = ".train_edge_width",
    guide = "edge_width"
  ),
  linewidth = .aes_entry(
    "linewidth",
    marks = list(include = .LINEWIDTH_MARKS),
    trainer = ".train_linewidth",
    guide = "linewidth",
    legend_merge = TRUE
  ),
  alpha = .aes_entry(
    "alpha",
    trainer = ".train_alpha",
    guide = "alpha",
    legend_merge = TRUE
  ),
  linetype = .aes_entry(
    "linetype",
    trainer = ".train_linetype",
    guide = "linetype",
    legend_merge = TRUE,
    types = .DISCRETE_TYPES
  ),
  edge_color = .aes_entry(
    "edge_color",
    trainer = ".train_edge_colour",
    guide = "color",
    types = .COLOUR_TYPES,
    # One variable colouring nodes and edges: the node guide already carries the
    # encoding when the palette-defining fields agree (`key_glyph` and the `map`
    # closure are expected to differ and are not compared).
    merge = list(
      color = list(
        continuous = c("type", "name", "range", "pal256", "midpoint"),
        .default = c("type", "name", "levels", "colors")
      )
    )
  ),
  edge_alpha = .aes_entry(
    "edge_alpha",
    trainer = ".train_edge_alpha",
    guide = "alpha"
  ),
  edge_linetype = .aes_entry(
    "edge_linetype",
    trainer = ".train_edge_linetype",
    guide = "linetype",
    types = .DISCRETE_TYPES
  )
)

# The registry rows that draw a legend, in legend order.
.aes_legend_keys <- function() {
  names(Filter(function(e) !is.null(e$guide), .AES_REGISTRY))
}

# The trainer function a registry row names.
.aes_trainer <- function(e) {
  get(e$trainer, mode = "function", envir = topenv(environment(.aes_entry)))
}

# Does layer mark `mark` route its channel to registry row `e`? A row with no
# `marks` restriction serves every layer.
.aes_serves_mark <- function(e, mark) {
  m <- e$marks
  if (is.null(m)) {
    return(TRUE)
  }
  if (!is.null(m$include)) mark %in% m$include else !mark %in% m$exclude
}

# The trained scale key a layer's `channel` routes through (the provenance
# scale-ref lookup), or NULL when no registered scale carries it.
.aes_scale_for_channel <- function(channel, mark) {
  for (e in .AES_REGISTRY) {
    if (
      isTRUE(e$provenance) &&
        channel %in% e$aliases &&
        .aes_serves_mark(e, mark)
    ) {
      return(e$name)
    }
  }
  NULL
}

# Every resolved-data channel some registered scale reads, in registry order.
.aes_channels <- function() {
  unique(unlist(
    lapply(.AES_REGISTRY, function(e) e$aliases),
    use.names = FALSE
  ))
}

# May legend `from` (a trained scale under registry key `from_key`) fold into
# legend `to` (key `to_key`)? True when the registry lists the partner and every
# field it names is identical on both scales. A field set keyed by type is
# picked by `from`'s type (`.default` for any other).
.aes_can_merge <- function(from, to, from_key, to_key) {
  if (is.null(from) || is.null(to)) {
    return(FALSE)
  }
  fields <- .AES_REGISTRY[[from_key]]$merge[[to_key]]
  if (is.null(fields)) {
    return(FALSE)
  }
  if (is.list(fields)) {
    fields <- fields[[from$type %||% ".default"]] %||% fields[[".default"]]
  }
  all(vapply(
    fields,
    function(f) identical(from[[f]], to[[f]]),
    logical(1)
  ))
}
