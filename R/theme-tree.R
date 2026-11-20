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

# The default base text size (points) of the complete themes.
.BASE_SIZE <- 11

# Non-element scalar settings + defaults.
.SETTINGS_DEFAULTS <- list(
  legend.position = "right", # or "inside" / c(x, y): over the panels
  legend.position.inside = NULL, # c(x, y) panel npc for "inside"
  legend.justification = "center", # anchor point of an inside legend
  legend.key.size = 4, # mm; default legend key / swatch side
  legend.spacing = 3.5, # mm; gap between stacked guides
  legend.margin = c(2, 2, 2, 2), # mm (t, r, b, l) inset around the legend block
  panel.spacing = 1.6, # mm
  plot.margin = c(5.5, 5.5, 5.5, 5.5), # mm (t, r, b, l) padding around the plot
  aspect.ratio = NULL, # carried for coord_fixed
  axis.ticks.length = 1.5 # mm
)

.theme_setting_names <- names(.SETTINGS_DEFAULTS)

# The root (text/line/rect) a slot descends from.
.slot_root <- function(slot) {
  if (!slot %in% names(.ELEMENT_PARENTS)) {
    cli::cli_abort("Unknown theme element {.val {slot}}.", .internal = TRUE)
  }
  while (!is.na(.ELEMENT_PARENTS[[slot]])) {
    slot <- .ELEMENT_PARENTS[[slot]]
  }
  slot
}

# An empty element of a slot's class (all properties NULL).
.empty_element <- function(slot) {
  root <- .slot_root(slot)
  switch(
    root,
    text = .element_text(),
    line = .element_line(),
    rect = .element_rect(),
    cli::cli_abort(
      "Theme element {.val {slot}} has no element class.",
      .internal = TRUE
    )
  )
}

# Does a value have the element class a slot expects (or is it blank)?
.slot_accepts <- function(slot, value) {
  if (.is_blank(value)) {
    return(TRUE)
  }
  root <- .slot_root(slot)
  cls <- switch(
    root,
    text = .element_text,
    line = .element_line,
    rect = .element_rect,
    cli::cli_abort(
      "Theme element {.val {slot}} has no element class.",
      .internal = TRUE
    )
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
    # root: a complete theme defines it; fall back to an empty element. A
    # root text size left relative has no parent to scale; read it against
    # the default base size so drawers always get points.
    root <- if (.is_blank(el)) el else (el %||% .empty_element(slot))
    if (S7::S7_inherits(root, .element_text) && .is_rel(root@size)) {
      root@size <- .combine_size(.BASE_SIZE, root@size)
    }
    root
  } else {
    presolved <- .resolve_slot(parent, theme, cache)
    if (.is_blank(presolved) || .is_blank(el)) {
      element_blank()
    } else if (is.null(el)) {
      presolved
    } else {
      .inherit_element(presolved, el)
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
      merged <- .merge_element(base[[nm]], val)
      # A `rel()` size laid over an absolute one scales it (so
      # `theme(text = element_text(size = rel(2)))` doubles the base size);
      # over a relative or unset size it replaces, and is resolved against
      # the parent element later, as in ggplot2.
      if (
        S7::S7_inherits(val, .element_text) &&
          .is_rel(val@size) &&
          is.numeric(base[[nm]]@size) &&
          !.is_rel(base[[nm]]@size)
      ) {
        merged@size <- .combine_size(base[[nm]]@size, val@size)
      }
      base[[nm]] <- merged
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
      if (nm == "legend.position") {
        ok <- c("right", "left", "top", "bottom", "none", "inside")
        val <- args[[nm]]
        if (
          !.is_npc_pair(val) &&
            (!is.character(val) || length(val) != 1L || !val %in% ok)
        ) {
          cli::cli_abort(
            "{.field legend.position} must be one of {.val {ok}}, or a numeric
            {.code c(x, y)} inside the panel (0-1).",
            call = call
          )
        }
      } else if (nm == "legend.position.inside") {
        if (!is.null(args[[nm]]) && !.is_npc_pair(args[[nm]])) {
          cli::cli_abort(
            "{.field legend.position.inside} must be a numeric {.code c(x, y)} (0-1).",
            call = call
          )
        }
      } else if (nm == "legend.justification") {
        if (is.null(.legend_just(args[[nm]]))) {
          cli::cli_abort(
            "{.field legend.justification} must be a numeric {.code c(x, y)}
            (0-1), or {.val center}, {.val left}, {.val right}, {.val top},
            {.val bottom} (or a pair such as {.code c(\"right\", \"top\")}).",
            call = call
          )
        }
      } else if (
        nm %in%
          c(
            "legend.key.size",
            "legend.spacing",
            "panel.spacing",
            "axis.ticks.length"
          )
      ) {
        val <- args[[nm]]
        if (
          !is.numeric(val) || length(val) != 1L || !is.finite(val) || val < 0
        ) {
          cli::cli_abort(
            "{.field {nm}} must be a single non-negative number (mm).",
            call = call
          )
        }
      } else if (nm %in% c("legend.margin", "plot.margin")) {
        val <- args[[nm]]
        if (
          !is.numeric(val) ||
            !length(val) %in% c(1L, 4L) ||
            any(!is.finite(val)) ||
            any(val < 0)
        ) {
          cli::cli_abort(
            "{.field {nm}} must be 1 or 4 non-negative numbers (mm).",
            call = call
          )
        }
      } else if (nm == "aspect.ratio") {
        val <- args[[nm]]
        if (
          !is.null(val) &&
            (!is.numeric(val) ||
              length(val) != 1L ||
              !is.finite(val) ||
              val <= 0)
        ) {
          cli::cli_abort(
            "{.field aspect.ratio} must be {.code NULL} or a single positive number.",
            call = call
          )
        }
      }
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

# A numeric c(x, y) pair of finite panel fractions.
.is_npc_pair <- function(x) {
  is.numeric(x) && length(x) == 2L && all(is.finite(x))
}

# A legend.justification value as numeric c(x, y), or NULL when it is not
# one. Words name a side ("left" = c(0, 0.5)); two words name a corner
# (c("right", "top") = c(1, 1)).
.legend_just <- function(x) {
  if (.is_npc_pair(x)) {
    return(as.numeric(x))
  }
  if (!is.character(x) || !length(x) %in% 1:2 || anyNA(x)) {
    return(NULL)
  }
  h <- c(left = 0, center = 0.5, centre = 0.5, right = 1)
  v <- c(bottom = 0, center = 0.5, centre = 0.5, top = 1)
  if (length(x) == 1L) {
    out <- switch(
      x,
      center = ,
      centre = c(0.5, 0.5),
      left = c(0, 0.5),
      right = c(1, 0.5),
      top = c(0.5, 1),
      bottom = c(0.5, 0)
    )
    return(out)
  }
  if (!x[1] %in% names(h) || !x[2] %in% names(v)) {
    return(NULL)
  }
  unname(c(h[[x[1]]], v[[x[2]]]))
}

# Where an inside legend sits, from the resolved theme: list(pos, just), both
# numeric c(x, y) in panel fractions, or NULL when the legend is outside the
# panels (or off). `legend.position = c(x, y)` (the old ggplot2 form) places
# it there; "inside" uses `legend.position.inside`, else the justification.
# The justification defaults to the position, so c(1, 1) is the top-right
# corner with the legend inside it. `force = TRUE` answers for a legend that
# asked to be inside itself (`guide_legend(position = "inside")`) whatever the
# theme's position: `legend.position.inside`, else the justification.
.legend_inside <- function(rt, force = FALSE) {
  pos <- rt[["legend.position"]]
  just <- rt[["legend.justification"]]
  explicit_just <- !identical(just, "center")
  just <- .legend_just(just) %||% c(0.5, 0.5)
  at <- if (.is_npc_pair(pos)) {
    as.numeric(pos)
  } else if (identical(pos, "inside") || force) {
    rt[["legend.position.inside"]] %||% just
  } else {
    return(NULL)
  }
  list(pos = at, just = if (explicit_just) just else at)
}
