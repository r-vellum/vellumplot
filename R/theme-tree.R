#' @include elements.R
NULL

# The theme element tree -------------------------------------------------------
#
# A theme is a named list keyed by dotted slot names (element values) plus a few
# scalar settings. Slots inherit unset properties from their parent; `text`,
# `line`, `rect` are abstract roots (inheritance origins, never drawn directly).
# `.resolve_theme()` flattens a complete theme into one resolved element per
# *drawn* leaf.

# child slot -> parent slot (roots map to NA).
.ELEMENT_PARENTS <- c(
  text = NA_character_,
  title = "text",
  plot.title = "title",
  plot.subtitle = "title",
  plot.caption = "title",
  plot.tag = "title",
  axis.title = "title",
  axis.title.x = "axis.title",
  axis.title.y = "axis.title",
  legend.title = "title",
  axis.text = "text",
  axis.text.x = "axis.text",
  axis.text.y = "axis.text",
  legend.text = "text",
  strip.text = "text",
  line = NA_character_,
  axis.ticks = "line",
  axis.ticks.x = "axis.ticks",
  axis.ticks.y = "axis.ticks",
  axis.line = "line",
  axis.line.x = "axis.line",
  axis.line.y = "axis.line",
  panel.grid = "line",
  panel.grid.major = "panel.grid",
  panel.grid.major.x = "panel.grid.major",
  panel.grid.major.y = "panel.grid.major",
  panel.grid.minor = "panel.grid",
  panel.grid.minor.x = "panel.grid.minor",
  panel.grid.minor.y = "panel.grid.minor",
  rect = NA_character_,
  plot.background = "rect",
  panel.background = "rect",
  legend.background = "rect",
  legend.key = "rect",
  strip.background = "rect"
)

# The leaves the drawers actually request (the resolver fills these).
.DRAWN_LEAVES <- c(
  "plot.title",
  "plot.subtitle",
  "plot.caption",
  "plot.tag",
  "axis.title.x",
  "axis.title.y",
  "legend.title",
  "axis.text.x",
  "axis.text.y",
  "legend.text",
  "strip.text",
  "axis.ticks.x",
  "axis.ticks.y",
  "axis.line.x",
  "axis.line.y",
  "panel.grid.major.x",
  "panel.grid.major.y",
  "panel.grid.minor.x",
  "panel.grid.minor.y",
  "plot.background",
  "panel.background",
  "legend.background",
  "legend.key",
  "strip.background"
)

# Non-element scalar settings + defaults.
.SETTINGS_DEFAULTS <- list(
  legend.position = "right",
  panel.spacing = 1.6, # mm
  plot.margin = c(5.5, 5.5, 5.5, 5.5), # mm (t, r, b, l); applied in 2.1b commit 2
  aspect.ratio = NULL, # carried for coord_fixed (2.3)
  axis.ticks.length = 1.5 # mm
)

.theme_setting_names <- names(.SETTINGS_DEFAULTS)

# The root (text/line/rect) a slot descends from.
.slot_root <- function(slot) {
  while (!is.na(.ELEMENT_PARENTS[[slot]])) {
    slot <- .ELEMENT_PARENTS[[slot]]
  }
  slot
}

# An empty element of a slot's class (all properties NULL).
.empty_element <- function(slot) {
  switch(
    .slot_root(slot),
    text = .element_text(),
    line = .element_line(),
    rect = .element_rect()
  )
}

# Does a value have the element class a slot expects (or is it blank)?
.slot_accepts <- function(slot, value) {
  if (.is_blank(value)) {
    return(TRUE)
  }
  cls <- switch(
    .slot_root(slot),
    text = .element_text,
    line = .element_line,
    rect = .element_rect
  )
  S7::S7_inherits(value, cls)
}

# Resolve one slot against a complete theme, memoised in `cache` (an env).
.resolve_slot <- function(slot, theme, cache) {
  if (!is.null(cache[[slot]])) {
    return(cache[[slot]])
  }
  el <- theme[[slot]]
  parent <- .ELEMENT_PARENTS[[slot]]
  resolved <- if (is.na(parent)) {
    # root: a complete theme defines it; fall back to an empty element
    if (.is_blank(el)) el else (el %||% .empty_element(slot))
  } else {
    presolved <- .resolve_slot(parent, theme, cache)
    if (.is_blank(presolved) || .is_blank(el)) {
      element_blank()
    } else if (is.null(el)) {
      presolved
    } else {
      .merge_element(presolved, el)
    }
  }
  cache[[slot]] <- resolved
  resolved
}

# Flatten a complete theme into a resolved element per drawn leaf, plus the
# scalar settings (filled from defaults).
.resolve_theme <- function(theme) {
  cache <- new.env(parent = emptyenv())
  out <- list()
  for (slot in .DRAWN_LEAVES) {
    out[[slot]] <- .resolve_slot(slot, theme, cache)
  }
  for (nm in .theme_setting_names) {
    out[[nm]] <- theme[[nm]] %||% .SETTINGS_DEFAULTS[[nm]]
  }
  out
}

# Merge a partial theme onto a complete base (ggplot2 element-wise semantics):
# a blank replaces; an element of the same class merges onto the base element;
# a scalar or a slot absent from the base replaces.
.merge_theme <- function(base, partial) {
  for (nm in names(partial)) {
    val <- partial[[nm]]
    if (nm %in% .theme_setting_names) {
      base[[nm]] <- val
    } else if (.is_blank(val) || is.null(base[[nm]]) || .is_blank(base[[nm]])) {
      base[[nm]] <- val
    } else {
      base[[nm]] <- .merge_element(base[[nm]], val)
    }
  }
  base
}

# Validate the named arguments to theme(): each must be a known element slot
# (with a matching element/blank value) or a known scalar setting.
.validate_theme_args <- function(args, call = rlang::caller_env()) {
  nms <- names(args)
  if (is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort("All arguments to {.fn theme} must be named.", call = call)
  }
  known_slots <- names(.ELEMENT_PARENTS)
  for (nm in nms) {
    if (nm %in% .theme_setting_names) {
      next
    }
    if (!nm %in% known_slots) {
      cli::cli_abort(
        "Unknown theme element {.field {nm}}.",
        call = call
      )
    }
    if (!.slot_accepts(nm, args[[nm]])) {
      want <- paste0("element_", .slot_root(nm))
      cli::cli_abort(
        "{.field {nm}} must be an {.fn {want}} or {.fn element_blank}.",
        call = call
      )
    }
  }
  invisible(args)
}
