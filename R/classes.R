#' @include vellumplot-package.R
NULL

# --- spec leaves ------------------------------------------------------------

# A single encoding channel: a defused column expression plus an (optionally
# inferred) variable type. `type = ""` means "infer from the resolved values".
channel <- S7::new_class(
  "channel",
  package = "vellumplot",
  properties = list(
    expr = S7::class_any, # a quosure (rlang)
    type = S7::new_property(S7::class_character, default = ""),
    after = S7::new_property(S7::class_logical, default = FALSE) # after_stat() channel?
  )
)

# One drawing layer: a mark, its encodings (named list<channel>), constant
# aesthetics (`params`), an optional statistical transform (`stat`, with its
# own `stat_params`), and a position adjustment (`position`).
LayerSpec <- S7::new_class(
  "LayerSpec",
  package = "vellumplot",
  properties = list(
    mark = S7::class_character, # "point" | "line" | "rule" | "bar" | "smooth" | "hex"
    encoding = S7::new_property(S7::class_list, default = list()),
    params = S7::new_property(S7::class_list, default = list()),
    stat = S7::new_property(S7::class_character, default = "identity"),
    stat_params = S7::new_property(S7::class_list, default = list()),
    position = S7::new_property(S7::class_character, default = "identity"),
    blend = S7::new_property(S7::class_character, default = "normal")
  )
)

# A user-declared scale override. `domain`/`palette`/`name` = NULL mean "derive
# while training".
ScaleSpec <- S7::new_class(
  "ScaleSpec",
  package = "vellumplot",
  properties = list(
    aesthetic = S7::class_character, # "x" | "y" | "color" | "fill" | "size" | "shape"
    type = S7::class_character, # "continuous" | "discrete"
    domain = S7::new_property(S7::class_any, default = NULL), # limits
    palette = S7::new_property(S7::class_any, default = NULL), # colours / shapes
    name = S7::new_property(S7::class_any, default = NULL),
    trans = S7::new_property(S7::class_any, default = NULL), # transform name / object
    range = S7::new_property(S7::class_any, default = NULL), # output range (size)
    breaks = S7::new_property(S7::class_any, default = NULL), # explicit breaks
    labels = S7::new_property(S7::class_any, default = NULL) # explicit labels
  )
)

# --- the spec ---------------------------------------------------------------

#' The plot specification
#'
#' `PlotSpec` is the S7 class that [vplot()] creates and the `mark_*()` /
#' `scale_*()` functions extend. It is a plain, inspectable, serializable data
#' object: data, a list of layers, a list of scale overrides, and the page size.
#' Nothing is drawn until it is compiled with [vellum::as_vellum_scene()] (e.g.
#' via [render_plot()]). Printing it draws the plot; inspect its structure with
#' [summary()].
#'
#' @param data The data frame.
#' @param layers A list of layer specifications (one per `mark_*()`).
#' @param scales A list of declared scale overrides.
#' @param width,height Page size in inches.
#' @param facet A faceting specification (from [facet_wrap()] / [facet_grid()]),
#'   or `NULL` for a single panel.
#' @param resolve A named list mapping an aesthetic to `"shared"` or
#'   `"independent"` (the scale-resolution lattice; see [resolve_scale()]).
#' @param theme Reserved for future use.
#'
#' @return A `PlotSpec`.
#' @seealso [vplot()], [mark_point()], [scale_x_continuous()]
#' @export
PlotSpec <- S7::new_class(
  "PlotSpec",
  package = "vellumplot",
  properties = list(
    data = S7::class_any, # a data.frame
    layers = S7::new_property(S7::class_list, default = list()), # list<LayerSpec>
    scales = S7::new_property(S7::class_list, default = list()), # list<ScaleSpec>
    facet = S7::new_property(S7::class_any, default = NULL), # FacetSpec | NULL
    resolve = S7::new_property(S7::class_list, default = list()), # aes -> shared|independent
    width = S7::new_property(S7::class_double, default = 6),
    height = S7::new_property(S7::class_double, default = 4),
    theme = S7::new_property(S7::class_any, default = NULL),
    labels = S7::new_property(S7::class_list, default = list()) # plot/axis labels
  )
)
