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
    type = S7::new_property(S7::class_character, default = ""),
    after = S7::new_property(S7::class_logical, default = FALSE), # after_stat() channel?
    # A conditional encoding (from `condition()`): a list(selection=, if_false=,
    # empty=) where `expr` above holds the `if_true` branch, so labelling, type
    # inference, scale training, and drawing all see the plain (if_true) encoding.
    # NULL for an ordinary channel.
    condition = S7::new_property(S7::class_any, default = NULL)
  )
)

# A layer render effect: a serializable directive that transforms a layer's
# emitted grob(s) at compile time (e.g. glow -> a stack of widened, low-alpha
# copies). Abstract root so `effects` can grow (shadow/outline) without touching
# the mark signatures; the compiler dispatches on the concrete subclass.
Effect <- S7::new_class("Effect", package = "vellumplot", abstract = TRUE)

# A neon-glow effect: `layers` widened, low-`alpha` copies of a stroked/point
# mark composited under a `blend` mode (the crisp original drawn on top). `size`
# is the extra visual spread in mm; `color` NULL inherits the mark's own colour.
GlowSpec <- S7::new_class(
  "GlowSpec",
  package = "vellumplot",
  parent = Effect,
  properties = list(
    size = S7::new_property(S7::class_double, default = 6),
    layers = S7::new_property(S7::class_integer, default = 6L),
    alpha = S7::new_property(S7::class_double, default = 0.12),
    blend = S7::new_property(S7::class_character, default = "screen"),
    color = S7::new_property(S7::class_any, default = NULL)
  )
)

# An outline / halo effect: one opaque, wider copy of a stroked/point mark drawn
# beneath the crisp original in a contrasting `color`, so the mark reads clearly
# over a busy or dark backdrop. `size` is the halo width per side, in mm.
OutlineSpec <- S7::new_class(
  "OutlineSpec",
  package = "vellumplot",
  parent = Effect,
  properties = list(
    size = S7::new_property(S7::class_double, default = 1),
    color = S7::new_property(S7::class_character, default = "white"),
    alpha = S7::new_property(S7::class_double, default = 1)
  )
)

# A drop / ambient shadow: dark, low-`alpha` copies of a mark drawn beneath the
# original, offset by (`x`, `y`) in millimetres (+x right, +y up), and softened by
# stacking `layers` copies widened up to `spread` mm. Defaults match `shadow()`
# (the only constructor); the offset is resolved device-side in mm via vellum's
# compound npc+mm unit (see `.emit_copies`).
ShadowSpec <- S7::new_class(
  "ShadowSpec",
  package = "vellumplot",
  parent = Effect,
  properties = list(
    x = S7::new_property(S7::class_double, default = 0.5),
    y = S7::new_property(S7::class_double, default = -0.5),
    color = S7::new_property(S7::class_character, default = "black"),
    alpha = S7::new_property(S7::class_double, default = 0.3),
    spread = S7::new_property(S7::class_double, default = 1.5),
    layers = S7::new_property(S7::class_integer, default = 3L)
  )
)

# A motion trail: `n` copies of a mark drawn beneath the original, marching off
# along direction (`x`, `y`) in mm (+x right, +y up) with each successive copy
# further out and fainter (opacity ramps from `alpha` at the nearest copy toward
# the tail, shaped by `decay`), optionally widening by `spread` mm. `motion()`
# uses many close low-alpha copies (a blur streak); `echo()` uses a few wider,
# more-opaque ghosts. `color` NULL inherits the mark's own colour.
MotionSpec <- S7::new_class(
  "MotionSpec",
  package = "vellumplot",
  parent = Effect,
  properties = list(
    x = S7::new_property(S7::class_double, default = 3),
    y = S7::new_property(S7::class_double, default = 0),
    n = S7::new_property(S7::class_integer, default = 8L),
    alpha = S7::new_property(S7::class_double, default = 0.15),
    decay = S7::new_property(S7::class_double, default = 1),
    spread = S7::new_property(S7::class_double, default = 0),
    blend = S7::new_property(S7::class_character, default = "normal"),
    color = S7::new_property(S7::class_any, default = NULL)
  )
)

# One drawing layer: a mark, its encodings (named list<channel>), constant
# aesthetics (`params`), an optional statistical transform (`stat`, with its
# own `stat_params`), a position adjustment (`position`), and render `effects`.
LayerSpec <- S7::new_class(
  "LayerSpec",
  package = "vellumplot",
  properties = list(
    mark = S7::class_character, # "point" | "line" | "rule" | "bar" | "smooth" | "hex"
    encoding = S7::new_property(S7::class_list, default = list()),
    params = S7::new_property(S7::class_list, default = list()),
    stat = S7::new_property(S7::class_character, default = "identity"),
    stat_params = S7::new_property(S7::class_list, default = list()),
    position = S7::new_property(S7::class_character, default = "identity"),
    blend = S7::new_property(S7::class_character, default = "normal"),
    effects = S7::new_property(S7::class_list, default = list()), # list<Effect>
    # A hand-drawn sketch spec (vellum_sketch), `NA`/`FALSE` for forced-crisp, or
    # NULL to inherit the plot-wide theme_sketch() default. A geometry property,
    # not an effect (see reexports.R `sketch()`).
    sketch = S7::new_property(S7::class_any, default = NULL),
    data = S7::new_property(S7::class_any, default = NULL), # per-layer data | NULL
    z = S7::new_property(S7::class_integer, default = 0L), # draw-order band
    # A per-layer clip (from clip_layer()): a ClipSpec masking just this layer, or
    # NULL. Distinct from PlotSpec@clip, which masks the whole panel.
    clip = S7::new_property(S7::class_any, default = NULL),
    # Interactivity declarations (host-agnostic; ignored by a static render). A
    # named list of quosures for the reserved args `tooltip`, `data_id`,
    # `hover_group`, `hover_color`, `selected_color` — evaluated per data row at
    # compile and threaded into the vellum scene as per-element keys/metadata.
    # Empty by default: a plot without them compiles and renders exactly as before.
    interactivity = S7::new_property(S7::class_list, default = list())
  )
)

# A secondary-axis declaration (from `sec_axis()` / `dup_axis()`), attached to a
# continuous position `ScaleSpec` via its `sec_axis` slot. `transform` is a
# normalised forward function mapping primary data values to secondary data
# values (monotonic); `dup = TRUE` marks a plain duplicate (`dup_axis()`).
SecAxisSpec <- S7::new_class(
  "SecAxisSpec",
  package = "vellumplot",
  properties = list(
    transform = S7::class_function, # primary data -> secondary data (monotonic)
    name = S7::new_property(S7::class_any, default = NULL), # secondary axis title
    breaks = S7::new_property(S7::class_any, default = NULL), # secondary-unit breaks
    labels = S7::new_property(S7::class_any, default = NULL), # explicit labels / fn
    dup = S7::new_property(S7::class_logical, default = FALSE) # a plain duplicate
  )
)

# A user-declared scale override. `domain`/`palette`/`name` = NULL mean "derive
# while training".
ScaleSpec <- S7::new_class(
  "ScaleSpec",
  package = "vellumplot",
  properties = list(
    aesthetic = S7::class_character, # "x" | "y" | "color" | "fill" | "size" | "shape"
    # Default "" (not character(0)) means "infer while training"; a zero-length
    # value would make `type %in% ...` return logical(0) and crash `||`/`&&` on
    # the position path (guide-only scales never set `type`).
    type = S7::new_property(S7::class_character, default = ""),
    domain = S7::new_property(S7::class_any, default = NULL), # limits
    palette = S7::new_property(S7::class_any, default = NULL), # colours / shapes
    name = S7::new_property(S7::class_any, default = NULL),
    trans = S7::new_property(S7::class_any, default = NULL), # transform name / object
    range = S7::new_property(S7::class_any, default = NULL), # output range (size)
    breaks = S7::new_property(S7::class_any, default = NULL), # explicit breaks
    labels = S7::new_property(S7::class_any, default = NULL), # explicit labels
    style = S7::new_property(S7::class_any, default = NULL), # binned: classInt style
    n = S7::new_property(S7::class_any, default = NULL), # binned: class count
    na_value = S7::new_property(S7::class_any, default = NULL), # colour for NA values
    midpoint = S7::new_property(S7::class_any, default = NULL), # diverging colour: value at the ramp's midpoint
    date_breaks = S7::new_property(S7::class_any, default = NULL), # date/time: interval string
    date_labels = S7::new_property(S7::class_any, default = NULL), # date/time: format string
    guide = S7::new_property(S7::class_any, default = NULL), # "none"/guide spec (legend control)
    sec_axis = S7::new_property(S7::class_any, default = NULL), # SecAxisSpec | NULL (secondary axis)
    expand = S7::new_property(S7::class_any, default = NULL) # continuous position: c(mult, add) axis padding; NULL = default 5%
  )
)

# --- interactivity spec nodes -----------------------------------------------

# A named selection: a set of data elements defined by a user gesture. Inert on a
# static render; a host (vellumwidget) binds it to a gesture it already performs.
# `kind`: "point" (click/hover/legend) or "interval" (brush/lasso/axis range).
# `on`: the gesture ("click"|"hover" for point; "xy"|"x"|"y" for interval).
# `region`: interval region shape ("rect"|"lasso"). `fields`: for a point
# selection, the columns whose match extends membership (a click selects every row
# equal on these fields -- e.g. "select the whole group"); NULL = the clicked
# element only. `toggle`: click toggles membership. `empty`: does an empty
# selection contain all elements (TRUE, the default -- spotlight semantics) or
# none (FALSE)?
SelectionSpec <- S7::new_class(
  "SelectionSpec",
  package = "vellumplot",
  properties = list(
    name = S7::class_character,
    kind = S7::class_character, # "point" | "interval"
    on = S7::new_property(S7::class_character, default = "click"),
    region = S7::new_property(S7::class_character, default = "rect"),
    fields = S7::new_property(S7::class_any, default = NULL), # chr | NULL
    toggle = S7::new_property(S7::class_logical, default = TRUE),
    empty = S7::new_property(S7::class_logical, default = TRUE),
    # Optional projection that expands a raw gesture to a related set before it is
    # applied, e.g. `list(mode = "neighbours", degree, edges)` from
    # `select_neighbours()` (a hovered graph node -> its incident edges +
    # neighbour nodes). NULL for an ordinary selection. Enacted host-side.
    expand = S7::new_property(S7::class_any, default = NULL)
  )
)

# A conditional encoding: an aesthetic whose value depends on selection
# membership. Held inside a `LayerSpec@encoding` channel (resolved from a
# `condition()` call). `if_true` is drawn (and trains scales) exactly as the
# equivalent plain encoding, so a static render shows the full plot; `if_false`
# (a constant, or NULL = the theme dim) is applied to non-members once the
# selection is active. `empty = TRUE`: an empty selection matches all, so the
# static / initial state shows `if_true` for every element.
ConditionSpec <- S7::new_class(
  "ConditionSpec",
  package = "vellumplot",
  properties = list(
    selection = S7::class_character,
    if_true = S7::new_property(S7::class_any, default = NULL), # resolved value(s)
    if_false = S7::new_property(S7::class_any, default = NULL), # constant | NULL (theme dim)
    empty = S7::new_property(S7::class_logical, default = TRUE)
  )
)

# A cross-view (or single-view) filter: show only the rows in `selection`,
# hiding the rest. Attached to a plot via `filter_by()`. Display-tier (hide, not
# re-aggregate).
FilterSpec <- S7::new_class(
  "FilterSpec",
  package = "vellumplot",
  properties = list(
    selection = S7::class_character
  )
)

# A scale-domain bind (overview + detail): the `aes` view of this plot's panel
# follows an interval `selection` defined elsewhere -- the host pans/zooms the
# panel to the selected range (display-tier, no scale retrain).
DomainBindSpec <- S7::new_class(
  "DomainBindSpec",
  package = "vellumplot",
  properties = list(
    selection = S7::class_character,
    aes = S7::new_property(S7::class_character, default = "x") # "x" | "y"
  )
)

# A source-inspection declaration (from `inspect_source()`): the plot asks a host
# to surface, on a gesture, the source data rows behind a clicked/hovered element
# (the "click-to-source" affordance). Inert on a static render; enacted by a host
# (vellumwidget), which reads the compiled scene's provenance table to answer it.
# `on`: the gesture ("click" or "hover"). `values`: also ship the referenced data
# rows so the host can display them (heavier payload; off by default).
SourceSpec <- S7::new_class(
  "SourceSpec",
  package = "vellumplot",
  properties = list(
    on = S7::new_property(S7::class_character, default = "click"),
    values = S7::new_property(S7::class_logical, default = FALSE)
  )
)

# A non-reactive keyframe-animation transition: the plot is compiled into K
# keyframe scenes (one per state) whose scales are trained once over all states
# and frozen, then the frames between them are tweened. `var` is the quosure whose
# levels enumerate the states; `kind` is "states" (more kinds later). Lengths are
# relative durations: `transition_length` weights the moving segments between
# states, `state_length` the held pause on each state. `wrap` loops the last state
# back to the first. Inert on a static render (only `animate()` reads it).
TransitionSpec <- S7::new_class(
  "TransitionSpec",
  package = "vellumplot",
  properties = list(
    var = S7::class_any, # a quosure (rlang) naming the state column
    kind = S7::new_property(S7::class_character, default = "states"),
    transition_length = S7::new_property(S7::class_double, default = 1),
    state_length = S7::new_property(S7::class_double, default = 1),
    wrap = S7::new_property(S7::class_logical, default = TRUE)
  )
)

# Easing for a transition's frame schedule. `default` names the easing applied to
# every interpolated aesthetic (e.g. "cubic-in-out"); the four class slots
# override it for one property class each, and `NA` means "use `default`". The
# classes are the ones the engine tweens separately -- see `.ease_classes()`.
# Inert on a static render.
EaseSpec <- S7::new_class(
  "EaseSpec",
  package = "vellumplot",
  properties = list(
    default = S7::new_property(S7::class_character, default = "linear"),
    position = S7::new_property(S7::class_character, default = NA_character_),
    color = S7::new_property(S7::class_character, default = NA_character_),
    size = S7::new_property(S7::class_character, default = NA_character_),
    alpha = S7::new_property(S7::class_character, default = NA_character_)
  )
)

# --- the spec ---------------------------------------------------------------

#' The plot specification
#'
#' `PlotSpec` is the S7 class that [vplot()] creates and the `mark_*()` /
#' `scale_*()` functions extend. It is a plain, inspectable, serializable data
#' object: data, a list of layers, a list of scale overrides, and the page size.
#' Nothing is drawn until it is compiled with [vellum::as_vellum_scene()] (e.g.
#' via [render_plot()]). Printing it draws the plot; inspect its structure with
#' [base::summary()].
#'
#' @param data The data frame.
#' @param edge_data For a graph plot (from [vgraph()]), the edge table; the
#'   default data for [mark_edges()]. `NULL` for ordinary plots.
#' @param layers A list of layer specifications (one per `mark_*()`).
#' @param scales A list of declared scale overrides.
#' @param width,height Page size in inches.
#' @param dpi Output resolution in dots per inch (pixels per inch). The exported
#'   PNG's pixel dimensions are `width * dpi` by `height * dpi`.
#' @param facet A faceting specification (from [facet_wrap()] / [facet_grid()]),
#'   or `NULL` for a single panel.
#' @param resolve A named list mapping an aesthetic to `"shared"` or
#'   `"independent"` (the scale-resolution lattice; see [resolve_scale()]).
#' @param coord A coordinate specification (from [coord_cartesian()] /
#'   [coord_flip()] / [coord_fixed()]), or `NULL` for the default Cartesian system.
#' @param theme A theme (a named list of resolved element/setting overrides, from
#'   [theme()] / a `theme_*()` preset), or `NULL` for the default theme.
#' @param labels A named list of plot/axis/legend label overrides (see [labs()]).
#' @param marginal A marginal-distribution specification (from [add_marginal()]),
#'   or `NULL` for no marginals.
#' @param selections A list of interactive selection declarations (from
#'   [select_point()] / [select_interval()] / [add_selection()]).
#' @param filters A list of filter declarations (from [filter_by()]).
#' @param binds A list of scale-domain bind declarations (from [bind_scale()]).
#' @param clip A [clip_to()] / [set_mask()] geometry clip (a `ClipSpec`), or
#'   `NULL` for none.
#' @param transition A keyframe-animation transition (from [transition_states()] /
#'   [transition_time()] / [transition_reveal()]), or `NULL`. Read only by
#'   [animate()]; inert on a static render.
#' @param ease An animation easing (from [ease_aes()]), or `NULL`.
#' @param source A source-inspection declaration (from [inspect_source()]), or
#'   `NULL`. Inert on a static render; a host surfaces the data rows behind a
#'   clicked element.
#'
#' @return A `PlotSpec`.
#' @seealso [vplot()], [mark_point()], [scale_x_continuous()]
#' @export
PlotSpec <- S7::new_class(
  "PlotSpec",
  package = "vellumplot",
  properties = list(
    data = S7::class_any, # a data.frame
    edge_data = S7::new_property(S7::class_any, default = NULL), # graph edge table | NULL
    layers = S7::new_property(S7::class_list, default = list()), # list<LayerSpec>
    scales = S7::new_property(S7::class_list, default = list()), # list<ScaleSpec>
    facet = S7::new_property(S7::class_any, default = NULL), # FacetSpec | NULL
    coord = S7::new_property(S7::class_any, default = NULL), # CoordSpec | NULL
    resolve = S7::new_property(S7::class_list, default = list()), # aes -> shared|independent
    width = S7::new_property(S7::class_double, default = 6),
    height = S7::new_property(S7::class_double, default = 4),
    dpi = S7::new_property(S7::class_double, default = 96),
    theme = S7::new_property(S7::class_any, default = NULL),
    labels = S7::new_property(S7::class_list, default = list()), # plot/axis labels
    marginal = S7::new_property(S7::class_any, default = NULL), # MarginalSpec | NULL
    # Declarative interactivity (all inert on a static render). `selections`:
    # named gestures (from select_point()/select_interval()). `filters`: show-only
    # references (from filter_by()). `binds`: scale-domain binds (from
    # bind_scale()). Empty by default -> a plot without them compiles and renders
    # exactly as before.
    selections = S7::new_property(S7::class_list, default = list()), # list<SelectionSpec>
    filters = S7::new_property(S7::class_list, default = list()), # list<FilterSpec>
    binds = S7::new_property(S7::class_list, default = list()), # list<DomainBindSpec>
    clip = S7::new_property(S7::class_any, default = NULL), # ClipSpec | NULL
    # Keyframe animation (inert on a static render; read only by animate()).
    # `transition` (a TransitionSpec) enumerates the states; `ease` (an EaseSpec)
    # sets the frame easing. NULL -> a plain, non-animated plot.
    transition = S7::new_property(S7::class_any, default = NULL), # TransitionSpec | NULL
    ease = S7::new_property(S7::class_any, default = NULL), # EaseSpec | NULL
    # Source inspection (from inspect_source()): a SourceSpec asking a host to
    # surface the data rows behind a clicked element. NULL -> no click-to-source.
    source = S7::new_property(S7::class_any, default = NULL) # SourceSpec | NULL
  )
)

# A clip / mask applied to the whole plot panel (from clip_to() / set_mask()),
# resolved in seam at panel-push into a `vl_viewport(mask = )`. `region` is a
# normalised list of coordinate rings (data coords) or NULL (whole-panel).
# `kind`: "clip" (hard alpha) or "mask" (soft luminance vignette). `NULL` on
# PlotSpec@clip means no clip (the common path is untouched).
ClipSpec <- S7::new_class(
  "ClipSpec",
  package = "vellumplot",
  properties = list(
    region = S7::new_property(S7::class_any, default = NULL),
    kind = S7::new_property(S7::class_character, default = "clip"),
    type = S7::new_property(S7::class_character, default = "alpha"),
    feather = S7::new_property(S7::class_double, default = 0),
    invert = S7::new_property(S7::class_logical, default = FALSE),
    # For `kind = "reveal"` (transition_reveal): the fraction of the panel, left
    # to right, that is unmasked. The animation tween grows this from 0 to 1.
    reveal_frac = S7::new_property(S7::class_double, default = NA_real_)
  )
)
