#' @include classes.R
NULL

# ---------------------------------------------------------------------------
# Declarative interactivity: the authoring verbs. All are pipe-first and return
# a PlotSpec (or, for the free-standing selection form, a SelectionSpec). Every
# node is inert on a static render (DECLARATIVE-INTERACTIVITY-DESIGN); a host
# (vellumwidget) enacts them on the frozen scene.
# ---------------------------------------------------------------------------

# A selection flag (`toggle`/`empty`) must be a single `TRUE`/`FALSE`. Without
# this, a truthy-but-non-logical value (`toggle = "yes"`, `empty = 1`) was
# silently coerced to `FALSE` by `isTRUE()` -- the opposite of what the caller
# meant -- rather than rejected.
.check_flag <- function(x, arg, call = rlang::caller_env()) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single {.code TRUE} or {.code FALSE}.",
      call = call
    )
  }
  invisible(x)
}

# Build a SelectionSpec, or attach it to a plot when piped. `x` is either a
# PlotSpec (attach inline, return the plot) or the selection name string (return
# a free-standing SelectionSpec for cross-view use via add_selection()).
.selection <- function(
  x,
  name,
  kind,
  on,
  region,
  fields,
  toggle,
  empty,
  expand = NULL,
  call = rlang::caller_env()
) {
  inline <- S7::S7_inherits(x, PlotSpec)
  if (inline) {
    if (missing(name) || !is.character(name) || length(name) != 1L) {
      cli::cli_abort("{.arg name} must be a single string.", call = call)
    }
  } else {
    # free-standing: the first positional arg *is* the name
    name <- x
    if (!is.character(name) || length(name) != 1L) {
      cli::cli_abort(
        "First argument must be a {.cls PlotSpec} (piped) or a selection name string.",
        call = call
      )
    }
  }
  .check_flag(toggle, "toggle", call = call)
  .check_flag(empty, "empty", call = call)
  sel <- SelectionSpec(
    name = name,
    kind = kind,
    on = on,
    region = region,
    fields = fields,
    toggle = toggle,
    empty = empty,
    expand = expand
  )
  if (inline) add_selection(x, sel) else sel
}

#' Declare an interactive selection
#'
#' A **selection** is a named set of data elements defined by a user gesture. On
#' its own it does nothing visible; refer to it from [condition()] (style by
#' membership), [filter_by()] (show only members), or [bind_scale()] (drive
#' another panel's view). Selections are part of the plot spec, so they travel
#' with it, serialise with it, and are enacted by any capable host
#' (`vellumwidget`). They are inert on a static render.
#'
#' `select_point()` is driven by a click or hover (or a legend value);
#' `select_interval()` by a drag (a brush rectangle or a lasso), optionally locked
#' to an axis.
#'
#' Both are **pipe-first** (attach to the plot and return it) *or* **free-standing**
#' (call with the name as the first argument to get a `SelectionSpec` you can share
#' across views, then attach with [add_selection()]).
#'
#' @param plot A [PlotSpec] (piped form), or a selection **name** string
#'   (free-standing form).
#' @param name The selection name (piped form) — a string other nodes reference.
#' @param on The gesture. Point: `"click"` (default) or `"hover"`. Interval:
#'   `"xy"` (default, both axes), `"x"`, or `"y"` (a single-axis brush).
#' @param fields For a point selection, the column name(s) whose match extends
#'   membership: clicking one element selects every row equal to it on these
#'   fields (e.g. `fields = "grp"` selects the whole group). `NULL` (default)
#'   selects only the clicked element. `group_by` is an alias.
#' @param group_by Alias for `fields`.
#' @param toggle For a point selection, whether clicking toggles membership
#'   (`TRUE`, default) or replaces it (`FALSE`, single-select).
#' @param region For an interval selection, `"rect"` (default, a brush rectangle)
#'   or `"lasso"` (a freehand polygon).
#' @param empty Whether an **empty** selection contains *all* elements (`TRUE`,
#'   default — so an unselected plot shows its full self and selecting *narrows*)
#'   or *none* (`FALSE`).
#'
#' @return A [PlotSpec] (piped form) or a `SelectionSpec` (free-standing form).
#' @seealso [condition()], [filter_by()], [add_selection()], [bind_scale()]
#' @examples
#' df <- data.frame(x = 1:10, y = runif(10), g = rep(c("a", "b"), 5))
#'
#' # highlight on hover (piped)
#' vplot(df) |>
#'   mark_point(x = x, y = y, color = condition("hi", g, "grey80")) |>
#'   select_point("hi", on = "hover")
#'
#' # free-standing, for cross-view use
#' sel <- select_interval("brush", on = "x")
#' @export
select_point <- function(
  plot,
  name,
  on = c("click", "hover"),
  fields = NULL,
  group_by = NULL,
  toggle = TRUE,
  empty = TRUE
) {
  on <- match.arg(on)
  fields <- fields %||% group_by
  if (!is.null(fields) && !is.character(fields)) {
    cli::cli_abort("{.arg fields} must be a character vector of column names.")
  }
  .selection(
    plot,
    name,
    kind = "point",
    on = on,
    region = "rect",
    fields = fields,
    toggle = toggle,
    empty = empty
  )
}

#' @rdname select_point
#' @export
select_interval <- function(
  plot,
  name,
  on = c("xy", "x", "y"),
  region = c("rect", "lasso"),
  empty = TRUE
) {
  on <- match.arg(on)
  region <- match.arg(region)
  .selection(
    plot,
    name,
    kind = "interval",
    on = on,
    region = region,
    fields = NULL,
    toggle = TRUE,
    empty = empty
  )
}

#' Highlight a node's graph neighbourhood
#'
#' A network-aware [select_point()] preset for [vgraph()] plots: pointing at (or
#' clicking) a node selects **its neighbourhood** — the node, its incident edges,
#' and its adjacent nodes — so a host spotlights them and dims the rest. Pointing
#' at an edge highlights the edge and its two endpoint nodes. It builds on the
#' node/edge identity `vgraph()` emits for an interactive plot (each node keyed by
#' its vertex `name`, each edge carrying its endpoint names); the host
#' reconstructs the adjacency and projects the gesture across it.
#'
#' Like the other selections it is **inert on a static render** and enacted by a
#' capable host (`vellumwidget`). On its own it spotlights the neighbourhood; pair
#' it with [condition()] to restyle members explicitly, or [filter_by()] to show
#' only the neighbourhood.
#'
#' @param plot A [PlotSpec], normally from [vgraph()] (piped form), or a selection
#'   **name** string (free-standing form).
#' @param name The selection name. Defaults to `"neighbours"`.
#' @param on The gesture: `"hover"` (default) or `"click"`.
#' @param degree How many hops out from the pointed node to include (`1`, the
#'   default, is the immediate neighbourhood; `2` adds neighbours-of-neighbours).
#' @param edges Whether to include the incident edges in the highlight
#'   (`TRUE`, default) or only the nodes.
#' @param empty Whether an empty selection matches *all* elements (`TRUE`,
#'   default — an un-hovered graph shows its full self) or none.
#' @return A [PlotSpec] (piped form) or a `SelectionSpec` (free-standing form).
#' @seealso [select_point()], [condition()], [vgraph()], [mark_edges()]
#' @examples
#' \dontrun{
#' g <- igraph::make_graph("Zachary")
#' vgraph(g) |>
#'   mark_edges() |>
#'   mark_nodes(size = 3) |>
#'   select_neighbours(on = "hover")
#' }
#' @export
select_neighbours <- function(
  plot,
  name = "neighbours",
  on = c("hover", "click"),
  degree = 1L,
  edges = TRUE,
  empty = TRUE
) {
  on <- match.arg(on)
  degree <- as.integer(degree)
  if (length(degree) != 1L || is.na(degree) || degree < 1L) {
    cli::cli_abort("{.arg degree} must be a positive integer.")
  }
  .selection(
    plot,
    name,
    kind = "point",
    on = on,
    region = "rect",
    fields = NULL,
    toggle = FALSE,
    empty = empty,
    expand = list(mode = "neighbours", degree = degree, edges = isTRUE(edges))
  )
}

#' Register a free-standing selection on a plot
#'
#' Attach a `SelectionSpec` (from the free-standing form of [select_point()] /
#' [select_interval()]) to a plot, marking that the selection's gesture is active
#' on this plot's marks. Use it when a selection is defined once and shared across
#' views in a composition (define + `add_selection()` on one plot, [filter_by()]
#' on another).
#'
#' @param plot A [PlotSpec].
#' @param selection A `SelectionSpec`.
#' @return The [PlotSpec], with the selection registered.
#' @seealso [select_point()], [filter_by()]
#' @export
add_selection <- function(plot, selection) {
  .check_plot(plot)
  if (!S7::S7_inherits(selection, SelectionSpec)) {
    cli::cli_abort("{.arg selection} must be a {.cls SelectionSpec}.")
  }
  plot@selections <- c(plot@selections, list(selection))
  plot
}

#' Conditional encoding: style by selection membership
#'
#' Use `condition()` as the value of a mark aesthetic to make that aesthetic
#' depend on whether an element is in a [selection][select_point]: members get
#' `if_true`, non-members get `if_false`. This is the general form of
#' highlight-on-interaction.
#'
#' `condition()` is recognised where it appears in a `mark_*()` encoding; calling
#' it directly is an error. The `if_true` branch is drawn on a static render and
#' trains scales exactly as the equivalent plain encoding, so
#' `color = condition("s", g, "grey80")` produces the same colour scale and legend
#' as `color = g`. With `empty = TRUE` (the default) an empty selection matches
#' everything, so a static / initial render shows `if_true` for all elements and
#' interaction dims non-members to `if_false`.
#'
#' @param selection The selection name (a string) that drives the condition.
#' @param if_true The aesthetic value for selection members — a column expression
#'   (data-masked) or a constant.
#' @param if_false The value for non-members — a constant, or omitted to use the
#'   theme's dim appearance.
#' @param empty Whether an empty selection matches all elements (`TRUE`, default).
#' @return Used only inside a `mark_*()` encoding; not called directly.
#' @seealso [select_point()], [filter_by()]
#' @export
condition <- function(selection, if_true, if_false, empty = TRUE) {
  cli::cli_abort(c(
    "{.fn condition} must be used inside a mark encoding.",
    i = "e.g. {.code mark_point(color = condition(\"sel\", grp, \"grey80\"))}."
  ))
}

#' Filter a view by a selection
#'
#' Show only the rows in `selection`, hiding the rest (a display-tier hide, not a
#' re-aggregation). Point a second view's `filter_by()` at a selection whose
#' gesture lives in a first view and you get **cross-filtering**: brush one panel,
#' a linked panel redraws to just those rows.
#'
#' @param plot A [PlotSpec].
#' @param selection A `SelectionSpec`, or a selection name string.
#' @return The [PlotSpec], with the filter registered.
#' @seealso [select_interval()], [add_selection()]
#' @export
filter_by <- function(plot, selection) {
  .check_plot(plot)
  name <- .selection_ref(selection)
  plot@filters <- c(plot@filters, list(FilterSpec(selection = name)))
  plot
}

#' Bind a panel's view to a selection (overview + detail)
#'
#' Make the `aes` view of this plot's panel follow an interval
#' [selection][select_interval] defined on another (overview) plot: dragging the
#' overview brush pans and zooms this (detail) panel to the selected range. This
#' is a display-tier view change (pan/zoom), not a scale retrain.
#'
#' @param plot A [PlotSpec] (the detail view).
#' @param selection A `SelectionSpec` (an interval selection), or a name string.
#' @param aes The axis to bind, `"x"` (default) or `"y"`.
#' @return The [PlotSpec], with the bind registered.
#' @seealso [select_interval()]
#' @export
bind_scale <- function(plot, selection, aes = c("x", "y")) {
  .check_plot(plot)
  aes <- match.arg(aes)
  name <- .selection_ref(selection)
  plot@binds <- c(plot@binds, list(DomainBindSpec(selection = name, aes = aes)))
  plot
}

#' Inspect the data behind a clicked element (click-to-source)
#'
#' Declare that a host should surface the **source data rows** behind an element
#' when the user clicks (or hovers) it — the "click-to-source" affordance. It is
#' inert on a static render; a host (`vellumwidget::as_widget()`) reads the
#' compiled scene's provenance ([provenance_join()] / [provenance_payload()]) to
#' answer the gesture, and under Shiny reports the clicked element's rows to
#' `input$<id>_source`.
#'
#' This is opt-in because the row mapping adds to the widget payload; declaring it
#' on the plot (rather than as a host flag) keeps interactivity a property of the
#' spec.
#'
#' @param plot A [PlotSpec].
#' @param on The gesture that reveals the source, `"click"` (default) or
#'   `"hover"`.
#' @param values If `TRUE`, also ship the referenced data rows so the host can
#'   display their values (a heavier payload); `FALSE` (default) ships only row
#'   indices.
#' @return The [PlotSpec], with source inspection registered.
#' @seealso [provenance_payload()], [select_point()]
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   inspect_source()
#' @export
inspect_source <- function(plot, on = c("click", "hover"), values = FALSE) {
  .check_plot(plot)
  on <- match.arg(on)
  plot@source <- SourceSpec(on = on, values = isTRUE(values))
  plot
}

# Resolve a selection reference (a SelectionSpec or a name string) to its name.
.selection_ref <- function(selection, call = rlang::caller_env()) {
  if (S7::S7_inherits(selection, SelectionSpec)) {
    return(selection@name)
  }
  if (is.character(selection) && length(selection) == 1L) {
    return(selection)
  }
  cli::cli_abort(
    "{.arg selection} must be a {.cls SelectionSpec} or a selection name string.",
    call = call
  )
}

#' The interaction model of a compiled plot
#'
#' Returns the serialisable declaration block a host needs to enact a plot's
#' declarative interactivity (selections, conditional encodings, filters, and
#' scale-domain binds), or `NULL` when the plot declares none. This is the
#' plot-level companion to the per-element metadata carried on the compiled scene;
#' `vellumwidget::as_widget()` reads it to wire gestures to reactions.
#'
#' @param x A [PlotSpec] or a plot composition.
#' @return A named list (`selections`, `conditions`, `filters`, `binds`, and
#'   `source` when [inspect_source()] is declared), or `NULL` if there is no
#'   declared interactivity.
#' @export
interaction_model <- function(x) {
  UseMethod("interaction_model")
}

#' @export
interaction_model.default <- function(x) {
  if (S7::S7_inherits(x, PlotSpec)) {
    return(.interaction_model_plot(x))
  }
  if (S7::S7_inherits(x, PlotComposition)) {
    return(.interaction_model_composition(x))
  }
  NULL
}

# The plot-level facts a host needs for each conditional encoding: the driving
# selection name, the target aesthetic, a *constant* `if_false` (a per-row
# `if_false` is carried per element instead; NULL here), and `empty`. Read off the
# channel's `condition` slot (populated at split by `.condition_channel`).
.plot_conditions <- function(plot) {
  out <- list()
  for (layer in plot@layers) {
    for (nm in names(layer@encoding)) {
      cnd <- layer@encoding[[nm]]@condition
      if (is.null(cnd)) {
        next
      }
      fe <- if (!is.null(cnd$if_false)) {
        rlang::quo_get_expr(cnd$if_false)
      } else {
        NULL
      }
      # A constant `if_false` is one that references no data column, i.e. its
      # expression has no free variables: a bare literal, but also a negative
      # literal (`-0.5`, a call to unary `-`) or a computed constant like
      # `rgb(1, 0, 0)`. Anything with a free variable is a per-row column,
      # carried per element instead (NULL here).
      if_false <- if (!is.null(fe) && length(all.vars(fe)) == 0L) {
        rlang::eval_tidy(cnd$if_false) # a constant expression
      } else {
        NULL # absent (theme dim) or a per-row column (carried per element)
      }
      out[[length(out) + 1L]] <- list(
        selection = cnd$selection,
        aes = nm,
        if_false = if_false,
        empty = cnd$empty
      )
    }
  }
  out
}

# Error on any condition/filter/bind that references an undeclared selection.
.validate_selection_refs <- function(
  defined,
  refs,
  call = rlang::caller_env()
) {
  missing <- setdiff(unique(refs), defined)
  if (length(missing)) {
    cli::cli_abort(
      c(
        "Interaction references an undeclared selection: {.val {missing}}.",
        i = "Declare it with {.fn select_point} / {.fn select_interval}, or {.fn add_selection}."
      ),
      call = call
    )
  }
}

.selection_record <- function(sel) {
  rec <- list(
    name = sel@name,
    kind = sel@kind,
    on = sel@on,
    region = sel@region,
    fields = sel@fields,
    toggle = sel@toggle,
    empty = sel@empty
  )
  if (!is.null(sel@expand)) {
    rec$expand <- sel@expand
  }
  rec
}

# Collect a single plot's interaction declarations into the plain block form,
# WITHOUT cross-reference validation (a composition validates once across all
# cells, since a selection may be defined in one cell and referenced in another).
.plot_interaction_parts <- function(plot) {
  parts <- list(
    selections = lapply(plot@selections, .selection_record),
    conditions = .plot_conditions(plot),
    filters = lapply(plot@filters, function(f) list(selection = f@selection)),
    binds = lapply(plot@binds, function(b) {
      list(selection = b@selection, aes = b@aes)
    })
  )
  if (!is.null(plot@source)) {
    parts$source <- list(on = plot@source@on, values = plot@source@values)
  }
  parts
}

.parts_empty <- function(p) {
  !length(p$selections) &&
    !length(p$conditions) &&
    !length(p$filters) &&
    !length(p$binds) &&
    is.null(p$source)
}

# Validate that every condition/filter/bind reference resolves to a declared
# selection, given the merged block.
.validate_parts <- function(parts) {
  defined <- vapply(parts$selections, function(s) s$name, character(1))
  refs <- c(
    vapply(parts$conditions, function(cnd) cnd$selection, character(1)),
    vapply(parts$filters, function(f) f$selection, character(1)),
    vapply(parts$binds, function(b) b$selection, character(1))
  )
  .validate_selection_refs(defined, refs)
}

.interaction_model_plot <- function(plot) {
  parts <- .plot_interaction_parts(plot)
  if (.parts_empty(parts)) {
    return(NULL)
  }
  .validate_parts(parts)
  parts
}

# Composition-level interaction model: collect declarations across all cells so a
# selection defined in one cell can be referenced (filter/condition/bind) in
# another. Cross-view scoping (which cell a gesture reads from / a reaction
# targets) is refined in a later stage; the merged block already lets the single
# runtime resolve references by name across the whole composition.
.interaction_model_composition <- function(comp) {
  cells <- .composition_leaf_plots(comp)
  parts <- lapply(cells, .plot_interaction_parts) # collect, DON'T validate per cell
  merged <- list(
    selections = list(),
    conditions = list(),
    filters = list(),
    binds = list()
  )
  for (p in parts) {
    for (k in names(merged)) {
      merged[[k]] <- c(merged[[k]], p[[k]])
    }
    # source inspection is plot-wide; the first cell that declares it wins.
    if (is.null(merged$source) && !is.null(p$source)) {
      merged$source <- p$source
    }
  }
  if (.parts_empty(merged)) {
    return(NULL)
  }
  # A selection may be registered on more than one cell (same object, same name);
  # keep one record per name.
  seen <- character(0)
  merged$selections <- Filter(
    function(s) {
      if (s$name %in% seen) {
        FALSE
      } else {
        seen <<- c(seen, s$name)
        TRUE
      }
    },
    merged$selections
  )
  .validate_parts(merged)
  merged
}

# Flatten a (possibly nested) composition to its leaf PlotSpecs.
.composition_leaf_plots <- function(comp) {
  out <- list()
  for (p in comp@plots) {
    if (S7::S7_inherits(p, PlotComposition)) {
      out <- c(out, .composition_leaf_plots(p))
    } else if (S7::S7_inherits(p, PlotSpec)) {
      out <- c(out, list(p))
    }
  }
  out
}
