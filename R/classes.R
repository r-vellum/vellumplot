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
    type = S7::new_property(S7::class_character, default = "")
  )
)

# One drawing layer: a mark name, its encodings (named list<channel>), and any
# constant aesthetics supplied as scalars (e.g. `size = 3`).
LayerSpec <- S7::new_class(
  "LayerSpec",
  package = "vellumplot",
  properties = list(
    mark = S7::class_character, # "point" | "line" | "rule" | "bar"
    encoding = S7::new_property(S7::class_list, default = list()),
    params = S7::new_property(S7::class_list, default = list())
  )
)

# A user-declared scale override. `domain`/`palette`/`name` = NULL mean "derive
# while training".
ScaleSpec <- S7::new_class(
  "ScaleSpec",
  package = "vellumplot",
  properties = list(
    aesthetic = S7::class_character, # "x" | "y" | "color" | "fill" | ...
    type = S7::class_character, # "continuous" | "discrete" | "log10"
    domain = S7::new_property(S7::class_any, default = NULL),
    palette = S7::new_property(S7::class_any, default = NULL),
    name = S7::new_property(S7::class_any, default = NULL)
  )
)

# --- the spec ---------------------------------------------------------------

#' The plot specification
#'
#' `PlotSpec` is the S7 class that [vplot()] creates and the `mark_*()` /
#' `scale_*()` functions extend. It is a plain, inspectable, serializable data
#' object: data, a list of layers, a list of scale overrides, and the page size.
#' Nothing is drawn until it is compiled with [vellum::as_vellum_scene()] (e.g.
#' via [render_plot()]). Inspect it with [print()].
#'
#' @param data The data frame.
#' @param layers A list of layer specifications (one per `mark_*()`).
#' @param scales A list of declared scale overrides.
#' @param width,height Page size in inches.
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
    width = S7::new_property(S7::class_double, default = 6),
    height = S7::new_property(S7::class_double, default = 4),
    theme = S7::new_property(S7::class_any, default = NULL)
  )
)
