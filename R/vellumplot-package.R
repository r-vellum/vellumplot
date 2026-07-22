#' @keywords internal
"_PACKAGE"

#' @importFrom rlang enquos eval_tidy quo_is_null quo_get_expr `%||%`
#' @importFrom vellum vl_scene push pop draw vl_viewport grid_layout vl_unit vl_gpar
#' @importFrom vellum points_grob lines_grob segments_grob rect_grob text_grob
#' @importFrom vellum grobwidth grobheight render as_vellum_scene
NULL

# Names that appear only inside data-masked / after_stat() expressions
# (mark_bin2d/mark_hex default `fill = after_stat(count)`, annotate()'s inline
# `width`/`height`), so R CMD check cannot see them bound.
utils::globalVariables(c(
  "count",
  "width",
  "height",
  # network marks read igraph node/edge columns via NSE
  "x",
  "y",
  "xend",
  "yend",
  "name",
  "level",
  # vdendrogram() maps these dendrogram node/edge columns via NSE
  "label",
  "cluster",
  # per-row radial label offsets attached by mark_node_text(dist=)
  ".node_nudge_x",
  ".node_nudge_y"
))
