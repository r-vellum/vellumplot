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
  if (identical(name, "alt") && (is.language(v) || is.expression(v))) {
    cli::cli_abort(
      c(
        "{.arg alt} must be plain text, not an expression.",
        i = "Alt text is read by screen readers; write it out in words."
      ),
      call = call
    )
  }
  .math_label(v, name, fn = "labs", call = call)
}

# A title given as a plotmath `expression()` or quoted call, typeset through
# [plotmath()] with a plain-text reading (`.plotmath_plain()`) for text-only
# outputs; any other value is returned as it is. Shared by `labs()`, the scale
# names (`.add_scale()`), the guide titles (`guide_*(title = )`, through
# `.set_guide()` and a scale's `guide`) and `sec_axis(name = )`, so a title
# reads the same wherever it is set (VP-163, VP-171). Only the first element of
# an expression is drawn, as in ggplot2; more is a warning naming `fn` and
# the argument `what`.
.math_label <- function(v, what, fn = NULL, call = rlang::caller_env()) {
  if (is.language(v) && !is.expression(v)) {
    v <- as.expression(list(v))
  }
  if (!is.expression(v)) {
    return(v)
  }
  if (length(v) > 1L) {
    if (is.null(fn)) {
      cli::cli_warn(
        "Only the first element of the expression for {.arg {what}} is used.",
        call = call
      )
    } else {
      cli::cli_warn(
        "{.fn {fn}} uses only the first element of the expression for {.arg {what}}.",
        call = call
      )
    }
  }
  v <- v[1]
  out <- vellum::plotmath(v)
  out@text <- .plotmath_plain(v[[1]])
  out
}

# A guide spec (`guide_legend()`, `guide_axis()`, ...) with its `title` read
# through `.math_label()`; any other guide (`"none"`, NULL) as it is.
.guide_math_title <- function(guide, call = rlang::caller_env()) {
  if (is.list(guide) && !is.null(guide$title)) {
    guide$title <- .math_label(guide$title, "title", call = call)
  }
  guide
}

# Greek letter names and their letters, for the plain-text readings of math
# labels (`.plotmath_plain()`, `.tex_plain()`).
.GREEK_PLAIN <- c(
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

# TeX symbol commands beyond the Greek letters, and their Unicode characters.
.TEX_SYMBOLS <- c(
  varepsilon = "\u03b5", vartheta = "\u03b8", varphi = "\u03c6",
  varpi = "\u03c0", varrho = "\u03c1", varsigma = "\u03c2",
  times = "\u00d7", cdot = "\u00b7", pm = "\u00b1", mp = "\u2213",
  div = "\u00f7", le = "\u2264", leq = "\u2264", ge = "\u2265",
  geq = "\u2265", ne = "\u2260", neq = "\u2260", approx = "\u2248",
  equiv = "\u2261", sim = "~", propto = "\u221d", infty = "\u221e",
  partial = "\u2202", nabla = "\u2207", sum = "\u03a3", prod = "\u03a0",
  int = "\u222b", to = "\u2192", rightarrow = "\u2192",
  leftarrow = "\u2190", Rightarrow = "\u21d2", leftrightarrow = "\u2194",
  "in" = "\u2208", notin = "\u2209", subset = "\u2282", cup = "\u222a",
  cap = "\u2229", forall = "\u2200", exists = "\u2203", ell = "\u2113",
  circ = "\u2218", degree = "\u00b0", ldots = "\u2026", dots = "\u2026",
  cdots = "\u22ef", prime = "\u2032", hbar = "\u0127", star = "*",
  ast = "*", lt = "<", gt = ">"
)

# A plain-text reading of a plotmath expression, for text-only outputs: Greek
# letter names become the letters, `paste()` / `*` join their parts, `list()`
# separates them with commas, a superscript keeps `^`, a subscript becomes
# `_`, styling functions (`bold()`, `italic()`, `hat()`, ...) drop out, and an
# operator keeps its symbol. `alpha^2` reads "\u03b1^2", `beta[i]` "\u03b2_i".
.plotmath_plain <- function(e) {
  greek <- .GREEK_PLAIN
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

# A plain-text reading of a TeX formula, for text-only outputs (the alt text,
# the SVG <title>/<desc>), in the style of `.plotmath_plain()`: Greek letters
# and common symbols become their Unicode characters, a superscript keeps `^`
# and a subscript becomes `_` (with parentheses around more than one
# character), `\frac{a}{b}` reads "a/b" and `\sqrt{x}` "\u221a(x)", styling
# and accent commands (`\mathrm{}`, `\hat{}`, ...) keep their argument, and
# spacing commands become a space (VP-172). `\alpha^{2}` reads "\u03b1^2",
# `\beta_{i}` "\u03b2_i".
.tex_plain <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    return(x)
  }
  sym <- c(.GREEK_PLAIN, .TEX_SYMBOLS)
  # Escaped braces stand aside (restored at the end) so they are not read as
  # a group; other escaped characters are just the character.
  x <- gsub("\\{", "\u0002", x, fixed = TRUE)
  x <- gsub("\\}", "\u0003", x, fixed = TRUE)
  x <- gsub("\\\\([$%&#_])", "\\1", x, perl = TRUE)
  # Spacing: `\,` `\:` `\;` `\ ` `~` `\quad` read as a space, `\!` as none.
  x <- gsub("\\\\[,:;> ]|~|\\\\q?quad(?![A-Za-z])", " ", x, perl = TRUE)
  x <- gsub("\\!", "", x, fixed = TRUE)
  x <- gsub(
    paste0(
      "\\\\(left|right|[bB]igg?[lr]?|displaystyle|textstyle|scriptstyle|",
      "scriptscriptstyle|limits|nolimits)(?![A-Za-z])"
    ),
    "",
    x,
    perl = TRUE
  )
  # Symbol commands (a whole command name, so `\in` leaves `\int` alone).
  m <- gregexpr("\\\\[A-Za-z]+", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(cmd) {
    nm <- substring(cmd, 2L)
    ifelse(nm %in% names(sym), unname(sym[nm]), cmd)
  })
  wrap <- function(a) if (nchar(a) > 1L) paste0("(", a, ")") else a
  # Innermost brace groups first, until none is left.
  repeat {
    before <- x
    x <- .gsub_fn("\\\\frac\\{([^{}]*)\\}\\{([^{}]*)\\}", x, function(g) {
      paste0(wrap(trimws(g[1])), "/", wrap(trimws(g[2])))
    })
    x <- .gsub_fn("\\\\sqrt\\{([^{}]*)\\}", x, function(g) {
      paste0("\u221a(", trimws(g[1]), ")")
    })
    # A script reads as `^`/`_` and its argument, in parentheses when it is
    # longer than one character.
    x <- .gsub_fn("([\\^_])\\{([^{}]*)\\}", x, function(g) {
      paste0(g[1], wrap(trimws(g[2])))
    })
    # A command with a group argument (style, accent, `\text{}`) keeps it.
    x <- gsub("\\\\[A-Za-z]+\\*?\\{([^{}]*)\\}", "\\1", x, perl = TRUE)
    x <- gsub("\\{([^{}]*)\\}", "\\1", x, perl = TRUE)
    if (identical(x, before)) {
      break
    }
  }
  # Any command left over reads as its name.
  x <- gsub("\\\\([A-Za-z]+)", "\\1", x, perl = TRUE)
  x <- chartr("\u0002\u0003", "{}", x)
  trimws(gsub(" {2,}", " ", x))
}

# `gsub()` with a function of the captured groups as the replacement.
.gsub_fn <- function(pattern, x, fn) {
  m <- regexpr(pattern, x, perl = TRUE)
  while (m > 0L) {
    st <- attr(m, "capture.start")
    ln <- attr(m, "capture.length")
    groups <- substring(x, st, st + ln - 1L)
    rep <- fn(groups)
    x <- paste0(
      substr(x, 1L, m - 1L),
      rep,
      substr(x, m + attr(m, "match.length"), nchar(x))
    )
    m <- regexpr(pattern, x, perl = TRUE)
  }
  x
}

# The plain text of a rich label, for text-only outputs: a tex() or plotmath()
# label's formula read by `.tex_plain()` (unless it carries a reading of its
# own, as an expression typeset by `.math_label()` does), an md() label's
# words without the markup and with each inline `$...$` formula read by
# `.tex_plain()` (VP-172).
.rich_plain <- function(x) {
  if (inherits(x, "vellum::vellum_tex_label")) {
    return(if (identical(x@text, x@tex)) .tex_plain(x@tex) else x@text)
  }
  txt <- x@text
  if (inherits(x, "vellum::vellum_md_label") && is.character(txt) &&
    length(txt) == 1L && grepl("$", txt, fixed = TRUE)) {
    m <- gregexpr("(?<!\\\\)\\$([^$]+)\\$", txt, perl = TRUE)
    regmatches(txt, m) <- lapply(regmatches(txt, m), function(f) {
      vapply(
        f,
        function(one) .tex_plain(substr(one, 2L, nchar(one) - 1L)),
        "",
        USE.NAMES = FALSE
      )
    })
  }
  txt
}
