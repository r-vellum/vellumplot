#' Rich-text labels
#'
#' A thin wrapper around [vellum::md()] that builds a rich-text label from a
#' markdown subset: `**bold**`, `*italic*` / `_italic_`, `^superscript^`,
#' `~subscript~`, and a colour span `[text]{#c00}`. The result can be used
#' anywhere vellumplot draws a *title*: [labs()] (`title` / `subtitle` /
#' `caption` / `tag` / `x` / `y` / `color`) and `scale_*(name = )`. Per-element
#' rich labels (in `mark_text()`) are not yet supported.
#'
#' @param text A length-one markdown string.
#' @return A `vellum_md_label` object accepted by [vellum::text_grob()].
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   labs(title = md("**Fuel** economy"), y = md("Efficiency (mi gal^-1^)"))
#' @export
md <- function(text) {
  vellum::md(text)
}
