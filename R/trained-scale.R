#' @include aes-registry.R
NULL

# ---------------------------------------------------------------------------
# The trained scale: what a trainer (R/compile-train.R) hands the mark emitters,
# the axis/legend drawers, provenance and the seam. It stays a plain named list
# (every consumer reads it with `$`), tagged with the S3 class
# `vellum_trained_scale` and built only through `trained_scale()`, which checks
# the invariants below. Fields changed after training go through
# `.scale_set()`, which re-checks them.
#
# Common fields (every scale):
#   aesthetic  the scale key, a row of `.AES_REGISTRY` ("x", "color", "size",
#              "edge_alpha", ...)
#   type       the scale type, one of the registry row's `types`:
#                x / y              continuous, log10, discrete, binned
#                color / edge_color continuous, discrete, binned, identity
#                size / alpha       continuous, discrete, identity
#                widths, edge_alpha continuous, identity
#                shape / linetype   discrete, identity
#                pattern            discrete
#              What the scale draws as a legend (a size key, a shape key, ...)
#              is the aesthetic's: the registry row's `guide`.
#   map        function(values) -> native position / colour / size / ...
#   name       the scale title (from the titles record), may be NULL
#
# Position fields (x / y): `discrete` (TRUE exactly for type "discrete"),
#   `band_width` (a positive number for a discrete or binned axis, else NULL),
#   `data_range` and `domain` (length 2), `breaks` and `labels` (one label per
#   break); optionally `levels` (discrete only), `sec`, `transform`, `time_unit`.
#
# Legend fields, by guide and type (absent on an identity scale, which draws no
# guide):
#   discrete / binned colour  levels, labels, colors (one each per key; a
#                             binned scale also has length(levels) + 1 breaks)
#   continuous colour         range, pal256, legend_breaks, legend_labels
#   continuous size / alpha / widths
#                             range, legend_breaks, legend_labels
#   shape / linetype /pattern levels plus shapes / linetypes / patterns
#
# Flags: `identity` (data values pass through verbatim; TRUE exactly for type
#   "identity", and implies `no_guide`), and the guide flags `.apply_guide()`
#   sets on a legend scale -- `no_guide`, `nested` (size only), `stepped`
#   (binned colour only), `reverse_bar` -- plus `na`, `breaks_asked`,
#   `labels_asked`. Every flag is a single TRUE/FALSE. Position scales carry no
#   guide flags (guides() never reaches an axis).
# ---------------------------------------------------------------------------

.POSITION_AES <- c("x", "y")

.TRAINED_SCALE_FLAGS <- c(
  "identity",
  "no_guide",
  "nested",
  "stepped",
  "reverse_bar",
  "na",
  "breaks_asked",
  "labels_asked",
  "discrete"
)

.GUIDE_FLAGS <- c("no_guide", "nested", "stepped", "reverse_bar")

# Build a trained scale from its common fields plus the per-type fields in
# `...`, and check it. Field order is the order given (common fields first).
trained_scale <- function(aesthetic, type, map, name = NULL, ...) {
  sc <- c(
    list(aesthetic = aesthetic, type = type, map = map, name = name),
    list(...)
  )
  class(sc) <- "vellum_trained_scale"
  .validate_trained_scale(sc)
}

.is_trained_scale <- function(x) inherits(x, "vellum_trained_scale")

# The legend a trained scale draws, as a dispatch key: a colour scale by its
# type ("discrete", "binned", "continuous", "identity"), any other legend scale
# by its registry row's guide ("size", "shape", "alpha", ...). NULL for an axis.
.scale_guide_key <- function(sc) {
  guide <- if (.is_string(sc$aesthetic)) .AES_REGISTRY[[sc$aesthetic]]$guide
  if (identical(guide, "color")) sc$type else guide
}

# The scale type as provenance (and vellumwidget) report it. Before the trained
# scale split `type` from the aesthetic, one field carried the type for axis and
# colour scales and the aesthetic's guide for the others ("size", "shape",
# "alpha" for edge_alpha, ...); the public value stays that, so a host reading
# the provenance sees no change.
.scale_public_type <- function(key, sc) {
  guide <- .AES_REGISTRY[[key]]$guide
  if (is.null(guide) || identical(guide, "color")) sc$type else guide
}

# Change fields of a trained scale after training (a guide flag, a retitle, a
# warped domain) and re-check it. A NULL value drops the field, as `$<-` does.
.scale_set <- function(sc, ...) {
  vals <- list(...)
  for (nm in names(vals)) {
    sc[[nm]] <- vals[[nm]]
  }
  .validate_trained_scale(sc)
}

# Check the invariants above; returns `sc` invisibly, or aborts with an internal
# error naming every broken one.
.validate_trained_scale <- function(sc) {
  problems <- .trained_scale_problems(sc)
  if (length(problems)) {
    aes <- if (.is_string(sc$aesthetic)) sc$aesthetic else "?"
    cli::cli_abort(
      c(
        "Invalid trained scale for {.field {aes}}.",
        rlang::set_names(problems, rep("x", length(problems)))
      ),
      .internal = TRUE
    )
  }
  invisible(sc)
}

.is_flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)

.is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

# Every broken invariant of `sc`, as messages (empty when it is valid).
.trained_scale_problems <- function(sc) {
  if (!.is_trained_scale(sc)) {
    return("not a `vellum_trained_scale`")
  }
  out <- character()
  bad <- function(msg) out <<- c(out, msg)
  if (!.is_string(sc$aesthetic) || is.null(.AES_REGISTRY[[sc$aesthetic]])) {
    return("`aesthetic` is not a registered scale key")
  }
  types <- .AES_REGISTRY[[sc$aesthetic]]$types
  if (!.is_string(sc$type) || !sc$type %in% types) {
    return(paste0(
      "`type` must be one of ",
      paste(types, collapse = ", "),
      " for ",
      sc$aesthetic
    ))
  }
  if (!is.function(sc$map)) {
    bad("`map` is not a function")
  }
  for (f in intersect(.TRAINED_SCALE_FLAGS, names(sc))) {
    if (!is.null(sc[[f]]) && !.is_flag(sc[[f]])) {
      bad(paste0("`", f, "` is not a single TRUE/FALSE"))
    }
  }
  if (sc$aesthetic %in% .POSITION_AES) {
    c(out, .position_scale_problems(sc))
  } else {
    c(out, .legend_scale_problems(sc))
  }
}

# Position (axis) scale invariants.
.position_scale_problems <- function(sc) {
  out <- character()
  bad <- function(msg) out <<- c(out, msg)
  for (f in c("discrete", "domain", "data_range")) {
    if (is.null(sc[[f]])) bad(paste0("an axis needs `", f, "`"))
  }
  if (length(out)) {
    return(out)
  }
  if (!identical(sc$discrete, identical(sc$type, "discrete"))) {
    bad("`discrete` must be TRUE exactly for type \"discrete\"")
  }
  banded <- sc$type %in% c("discrete", "binned")
  bw <- sc$band_width
  if (banded && !(is.numeric(bw) && length(bw) == 1L && bw > 0)) {
    bad("a discrete or binned axis needs a positive `band_width`")
  }
  if (!banded && !is.null(bw)) {
    bad("a continuous axis has no `band_width`")
  }
  if (length(sc$domain) != 2L || length(sc$data_range) != 2L) {
    bad("`domain` and `data_range` must have length 2")
  }
  if (length(sc$labels) != length(sc$breaks)) {
    bad("an axis needs one label per break")
  }
  if (!is.null(sc$levels) && !identical(sc$type, "discrete")) {
    bad("only a discrete axis has `levels`")
  }
  guide_flags <- intersect(.GUIDE_FLAGS, names(sc))
  if (length(guide_flags)) {
    bad("an axis carries no guide flags")
  }
  out
}

# Legend (non-position) scale invariants. The fields a scale needs follow from
# its guide (the registry row's: a colour, size, shape, ... legend) and its type.
.legend_scale_problems <- function(sc) {
  out <- character()
  bad <- function(msg) out <<- c(out, msg)
  guide <- .AES_REGISTRY[[sc$aesthetic]]$guide
  same_len <- function(fields) {
    present <- fields[fields %in% names(sc)]
    if (length(present) != length(fields)) {
      bad(paste0(
        "a ",
        sc$type,
        " ",
        guide,
        " scale needs ",
        paste0("`", fields, "`", collapse = ", ")
      ))
    } else if (length(unique(lengths(sc[fields]))) != 1L) {
      bad(paste0(
        paste0("`", fields, "`", collapse = ", "),
        " must have equal lengths"
      ))
    }
  }
  if (!identical(isTRUE(sc$identity), identical(sc$type, "identity"))) {
    bad("`identity = TRUE` exactly for type \"identity\"")
    return(out)
  }
  if (isTRUE(sc$identity)) {
    if (!isTRUE(sc$no_guide)) {
      bad("an identity scale draws no guide")
    }
    return(out)
  }
  # The guide flags that only make sense on one kind of legend.
  if (isTRUE(sc$nested) && !identical(sc$aesthetic, "size")) {
    bad("only a size scale can be `nested`")
  }
  if (isTRUE(sc$stepped) && !identical(sc$type, "binned")) {
    bad("only a binned scale can be `stepped`")
  }
  if (identical(guide, "color")) {
    switch(
      sc$type,
      discrete = same_len(c("levels", "labels", "colors")),
      binned = {
        same_len(c("levels", "labels", "colors"))
        if (length(sc$breaks) != length(sc$colors) + 1L) {
          bad("a binned scale needs one more break than colours")
        }
      },
      continuous = {
        same_len(c("legend_breaks", "legend_labels"))
        if (length(sc$range) != 2L || is.null(sc$pal256)) {
          bad(
            "a continuous colour scale needs a length-2 `range` and `pal256`"
          )
        }
      }
    )
    return(out)
  }
  switch(
    guide,
    shape = same_len(c("levels", "shapes")),
    linetype = same_len(c("levels", "linetypes")),
    pattern = same_len(c("levels", "patterns")),
    {
      # size / alpha / edge_width / linewidth
      same_len(c("legend_breaks", "legend_labels"))
      if (length(sc$range) != 2L) {
        bad(paste0("a ", guide, " scale needs a length-2 `range`"))
      }
    }
  )
  out
}
