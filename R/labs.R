#' @include classes.R
NULL

#' Set plot titles and axis/legend labels
#'
#' Add plot-level text — a `title`, `subtitle`, `caption`, and `tag` — and
#' override the titles of individual axes and legends (`x`, `y`, `color`,
#' `fill`, `size`).
#' The title, subtitle, and tag are drawn in a band above the panels; the caption
#' in a band below. Repeated `labs()` calls merge, with later values winning.
#'
#' Axis/legend overrides set here are used unless a matching `scale_*(name = )`
#' is also given, which takes precedence. With neither, the title is derived from
#' the mapping.
#'
#' @param plot A [PlotSpec].
#' @param title,subtitle,caption,tag Plot-level text (or `NULL` to leave unset).
#'   In an animation, the title, subtitle and caption may embed frame variables
#'   such as `"{closest_state}"` or `"{round(frame_time)}"` (see *Frame
#'   variables* in [animate()]).
#' @param x,y,size Axis / legend title overrides for those aesthetics.
#' @param color,colour Colour-scale title override; `colour` is an alias for
#'   `color`.
#' @param fill Fill-scale title override. `color` and `fill` are separate
#'   scales, so each takes its own title; give both the same title (and map the
#'   same variable) to draw them as one legend.
#' @param alt A text alternative (alt text) describing the plot for screen
#'   readers and other assistive technology. Overrides the description vellumplot
#'   generates automatically; see [plot_alt()]. Emitted into the accessible SVG
#'   (`<desc>`) and tagged PDF (Figure `Alt`) by [render_plot()].
#' @param ... Reserved; passing anything here is an error.
#' @details
#' Any label may be rich text: [md()] for Markdown, [tex()] for LaTeX math, or
#' [plotmath()] for base R plotmath. A plotmath `expression()` (or a quoted
#' call) is typeset through [plotmath()] for you, so
#' `labs(x = expression(alpha^2))` draws \eqn{\alpha^2}, as in ggplot2.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   labs(title = "Fuel economy", x = "Weight", y = "MPG")
#' @export
labs <- function(
  plot,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  x = NULL,
  y = NULL,
  color = NULL,
  colour = NULL,
  fill = NULL,
  size = NULL,
  alt = NULL,
  ...
) {
  .check_plot(plot)
  rlang::check_dots_empty()
  # Which arguments were actually supplied (even as NULL). An *explicit* NULL means
  # "blank this label" (ggplot2 parity: `labs(x = NULL)` drops the axis title),
  # which is distinct from not passing the argument at all -- so a bare default of
  # NULL cannot carry that intent and we consult the call.
  passed <- names(match.call())[-1L]
  color <- color %||% colour
  color_passed <- any(c("color", "colour") %in% passed)
  candidates <- list(
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    x = x,
    y = y,
    color = color,
    fill = fill,
    size = size,
    alt = alt
  )
  supplied <- c(intersect(names(candidates), passed), if (color_passed) "color")
  over <- candidates[unique(supplied)]
  # An explicitly-passed NULL blanks the label ("" also blanks it downstream).
  env <- environment()
  over <- Map(
    function(v, nm) if (is.null(v)) "" else .labs_value(v, nm, call = env),
    over,
    names(over)
  )
  plot@labels <- utils::modifyList(plot@labels, over)
  plot
}

# A base-R plotmath expression (`expression(alpha^2)`, or a quoted call such as
# `quote(alpha^2)`) is typeset through [plotmath()], as ggplot2 users expect.
# Before, it was deparsed and drawn as its source text (VP-163). The label's
# plain text -- what the alt text, the SVG <title>/<desc> and other text-only
# outputs read -- is a readable rendering of the expression
# (`.plotmath_plain()`), not the TeX it is typeset from. `alt` is plain text
# itself, so it takes no expression. Only the first element of an expression
# is drawn, as in ggplot2; more is a warning.
.labs_value <- function(v, name, call = rlang::caller_env()) {
  if (is.language(v) && !is.expression(v)) {
    v <- as.expression(list(v))
  }
  if (!is.expression(v)) {
    return(v)
  }
  if (identical(name, "alt")) {
    cli::cli_abort(
      c(
        "{.arg alt} must be plain text, not an expression.",
        i = "Alt text is read by screen readers; write it out in words."
      ),
      call = call
    )
  }
  if (length(v) > 1L) {
    cli::cli_warn(
      "{.fn labs} uses only the first element of the expression for {.arg {name}}.",
      call = call
    )
  }
  v <- v[1]
  out <- vellum::plotmath(v)
  out@text <- .plotmath_plain(v[[1]])
  out
}

# A plain-text reading of a plotmath expression, for text-only outputs: Greek
# letter names become the letters, `paste()` / `*` join their parts, `list()`
# separates them with commas, a superscript keeps `^`, a subscript becomes
# `_`, styling functions (`bold()`, `italic()`, `hat()`, ...) drop out, and an
# operator keeps its symbol. `alpha^2` reads "\u03b1^2", `beta[i]` "\u03b2_i".
.plotmath_plain <- function(e) {
  greek <- c(
    alpha = "\u03b1", beta = "\u03b2", gamma = "\u03b3", delta = "\u03b4",
    epsilon = "\u03b5", zeta = "\u03b6", eta = "\u03b7", theta = "\u03b8",
    iota = "\u03b9", kappa = "\u03ba", lambda = "\u03bb", mu = "\u03bc",
    nu = "\u03bd", xi = "\u03be", omicron = "\u03bf", pi = "\u03c0",
    rho = "\u03c1", sigma = "\u03c3", tau = "\u03c4", upsilon = "\u03c5",
    phi = "\u03c6", chi = "\u03c7", psi = "\u03c8", omega = "\u03c9",
    Gamma = "\u0393", Delta = "\u0394", Theta = "\u0398", Lambda = "\u039b",
    Xi = "\u039e", Pi = "\u03a0", Sigma = "\u03a3", Phi = "\u03a6",
    Psi = "\u03a8", Omega = "\u03a9"
  )
  walk <- function(e) {
    if (is.symbol(e)) {
      nm <- as.character(e)
      return(if (nm %in% names(greek)) greek[[nm]] else nm)
    }
    if (!is.call(e)) {
      return(paste(as.character(e), collapse = ""))
    }
    fn <- as.character(e[[1]])[1]
    args <- lapply(as.list(e)[-1], walk)
    if (fn %in% c("paste", "*")) {
      return(paste0(unlist(args), collapse = ""))
    }
    if (identical(fn, "list")) {
      return(paste(unlist(args), collapse = ", "))
    }
    if (identical(fn, "^") && length(args) == 2L) {
      return(paste0(args[[1]], "^", args[[2]]))
    }
    if (identical(fn, "[") && length(args) == 2L) {
      return(paste0(args[[1]], "_", args[[2]]))
    }
    if (identical(fn, "(") && length(args) == 1L) {
      return(paste0("(", args[[1]], ")"))
    }
    if (identical(fn, "frac") && length(args) == 2L) {
      return(paste0(args[[1]], "/", args[[2]]))
    }
    if (identical(fn, "~") && length(args) == 2L) {
      return(paste(args[[1]], args[[2]]))
    }
    if (length(args) == 2L && grepl("^[^A-Za-z.]", fn)) {
      return(paste(args[[1]], if (fn == "==") "=" else fn, args[[2]]))
    }
    if (length(args) == 1L && grepl("^[^A-Za-z.]", fn)) {
      return(paste0(fn, args[[1]]))
    }
    if (length(args) == 1L) {
      return(args[[1]])
    }
    paste(deparse(e), collapse = "")
  }
  walk(e)
}
