#' @include classes.R
NULL

# Position adjustments with parameters. Marks still accept a bare string
# (`position = "dodge"`); the `position_*()` constructors below add tunable
# parameters (ggplot2's model). A constructor returns a small classed record; the
# layer builder (`.normalize_position`) splits it into the `position` type string
# (stored on the LayerSpec) plus a parameter list merged into `stat_params`, where
# the emitters and `.apply_position` read them.

#' Position adjustments
#'
#' Fine-grained control over how a mark's overlapping elements are placed. Pass
#' the result to a mark's `position` argument (e.g.
#' `mark_point(position = position_jitter(width = 0.2))`). A bare string
#' (`position = "dodge"`) still works and uses the defaults.
#'
#' * `position_nudge()` shifts every element by a constant amount in **data**
#'   units (for continuous axes) — handy for offsetting labels from points.
#' * `position_jitter()` adds uniform random noise; `width`/`height` are the
#'   maximum shift in data units (default: 40% of the resolution), `seed` makes
#'   it reproducible.
#' * `position_dodge()` places grouped elements side by side; `width` is the
#'   total data-space width shared by the group (default: the category band).
#' * `position_dodge2()` dodges by the groups actually present at each x and
#'   splits the band between them with a `padding` gap — so ragged groupings
#'   stay centred and evenly spaced.
#' * `position_jitterdodge()` dodges grouped elements, then jitters within each
#'   dodged slot (points over dodged boxes).
#' * `position_sina()` spreads each category's points along x by a quasirandom
#'   offset scaled to the local y-density, so the cloud traces the distribution's
#'   shape (ggforce's sina). `width` is the maximum spread as a fraction of the
#'   category band (default `0.8`).
#'
#' @param x,y `position_nudge()` shift (data units).
#' @param width,height Maximum jitter (data units); `NULL` uses the default. For
#'   `position_sina()`, `width` is the spread as a fraction of the band.
#' @param dodge.width,jitter.width,jitter.height `position_jitterdodge()`
#'   dodge / jitter extents.
#' @param padding `position_dodge2()` gap between dodged elements, as a fraction
#'   of the element width.
#' @param seed Optional integer seed for the random jitter.
#' @return A `vellumplot_position` object for a mark's `position` argument.
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = factor(cyl), y = mpg, position = position_jitter(width = 0.15))
#' @name position
NULL

.new_position <- function(type, ...) {
  structure(c(list(type = type), list(...)), class = "vellumplot_position")
}

#' @rdname position
#' @export
position_nudge <- function(x = 0, y = 0) {
  .new_position("nudge", nudge_x = as.numeric(x), nudge_y = as.numeric(y))
}

#' @rdname position
#' @export
position_jitter <- function(width = NULL, height = NULL, seed = NULL) {
  .new_position(
    "jitter",
    jitter_width = width,
    jitter_height = height,
    seed = seed
  )
}

#' @rdname position
#' @export
position_dodge <- function(width = NULL) {
  .new_position("dodge", dodge_width = width)
}

#' @rdname position
#' @export
position_dodge2 <- function(padding = 0.1) {
  .new_position("dodge2", dodge2_padding = padding)
}

#' @rdname position
#' @export
position_sina <- function(width = 0.8, seed = NULL) {
  .new_position("sina", sina_width = as.numeric(width), seed = seed)
}

#' @rdname position
#' @export
position_jitterdodge <- function(
  jitter.width = NULL,
  jitter.height = 0,
  dodge.width = 0.75,
  seed = NULL
) {
  .new_position(
    "jitterdodge",
    jitter_width = jitter.width,
    jitter_height = jitter.height,
    dodge_width = dodge.width,
    seed = seed
  )
}

# Split a `position` argument (string or `vellumplot_position`) into the type
# string + a list of parameters to merge into a layer's `stat_params`. Drops
# NULL parameters so a layer without them is unchanged.
.normalize_position <- function(position, call = rlang::caller_env()) {
  if (inherits(position, "vellumplot_position")) {
    args <- position[setdiff(names(position), "type")]
    args <- args[!vapply(args, is.null, logical(1))]
    return(list(type = position$type, args = args))
  }
  if (is.character(position) && length(position) == 1L) {
    return(list(type = position, args = list()))
  }
  cli::cli_abort(
    "{.arg position} must be a string or a {.fn position_*} object.",
    call = call
  )
}
