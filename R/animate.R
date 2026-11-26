#' @include classes.R seam.R compile-facet.R compile-train.R
NULL

# ============================================================================
# Non-reactive keyframe animation.
#
# `transition_states()` marks a column whose levels are the animation's states;
# `animate()` compiles one keyframe scene per state -- training the scales ONCE
# over every state and freezing them, so nothing retrains between frames -- and
# returns a `vellum_animation`. `anim_save()` builds the eased frame schedule and
# hands the keyframes to vellum's parallel tween/render/encode engine.
# ============================================================================

# ---- pipe-step verbs -------------------------------------------------------

#' Animate a plot across the states of a variable
#'
#' `transition_states()` turns a plot into a keyframe animation: the plot is
#' compiled once per level of `states` (a "keyframe"), and [animate()] tweens the
#' frames between successive keyframes. The scales are trained **once over all
#' states and frozen**, so axis breaks, colour ramps and sizes stay put across the
#' whole animation (the interpolation is non-reactive: the states are fixed at
#' author time, nothing retrains in response to anything).
#'
#' The step only records the intent; call [animate()] to build the animation and
#' [anim_save()] to write it. A plain (non-animated) render ignores it.
#'
#' @param plot A [PlotSpec] (from [vplot()]).
#' @param states A column (bare or a string) whose distinct values, in level
#'   order, are the animation's states.
#' @param transition_length Relative duration of the moving segments between
#'   states.
#' @param state_length Relative duration of the held pause on each state.
#' @param wrap Loop the last state back to the first?
#' @return The modified [PlotSpec].
#' @seealso [ease_aes()], [animate()], [anim_save()]
#' @examples
#' if (requireNamespace("gapminder", quietly = TRUE)) {
#'   library(gapminder)
#'   p <- vplot(gapminder) |>
#'     mark_point(x = gdpPercap, y = lifeExp, size = pop, color = continent) |>
#'     scale_x_continuous(trans = "log10") |>
#'     transition_states(year)
#' }
#' @export
transition_states <- function(
  plot,
  states,
  transition_length = 1,
  state_length = 1,
  wrap = TRUE
) {
  .check_plot(plot)
  var <- rlang::enquo(states)
  if (rlang::quo_is_missing(var)) {
    cli::cli_abort(
      "{.fn transition_states} needs a state column, e.g. {.code transition_states(year)}."
    )
  }
  plot@transition <- TransitionSpec(
    var = var,
    kind = "states",
    transition_length = .pos_num(transition_length, "transition_length"),
    state_length = .nonneg_num(state_length, "state_length"),
    wrap = isTRUE(wrap)
  )
  plot
}

#' Animate a plot along a continuous time
#'
#' `transition_time()` is like [transition_states()] but treats `time` as a
#' continuous quantity: the states are its distinct values and each transition is
#' allocated frames **in proportion to its time gap**, so the animation plays at a
#' constant rate through unevenly spaced times. There is no pause on a state and no
#' wrap — time flows once, start to finish.
#'
#' `{frame_time}` in a title, subtitle or caption shows the time each frame
#' has reached, updated on every frame (see *Frame variables* in [animate()]);
#' for a `Date` or `POSIXct` time it is a `Date` or `POSIXct`, so
#' `"{format(frame_time, '%b %Y')}"` works.
#'
#' @param plot A [PlotSpec] (from [vplot()]).
#' @param time A numeric, `Date` or `POSIXct` column giving each row's time.
#' @return The modified [PlotSpec].
#' @seealso [transition_states()], [ease_aes()], [animate()]
#' @examples
#' if (requireNamespace("gapminder", quietly = TRUE)) {
#'   vplot(gapminder::gapminder) |>
#'     mark_point(x = gdpPercap, y = lifeExp, size = pop, color = continent) |>
#'     scale_x_continuous(trans = "log10") |>
#'     transition_time(year)
#' }
#' @export
transition_time <- function(plot, time) {
  .check_plot(plot)
  var <- rlang::enquo(time)
  if (rlang::quo_is_missing(var)) {
    cli::cli_abort(
      "{.fn transition_time} needs a time column, e.g. {.code transition_time(year)}."
    )
  }
  plot@transition <- TransitionSpec(var = var, kind = "time", wrap = FALSE)
  plot
}

#' Reveal a plot progressively along a variable
#'
#' `transition_reveal()` draws the data in, in the order of `along` -- the
#' classic "line draws itself" animation, as in gganimate. Each frame shows the
#' rows whose `along` value has been reached: a point appears when the reveal
#' reaches it, and a line, path, area, ribbon or step grows through its rows
#' with an interpolated leading point, so it advances smoothly between data
#' points rather than jumping from one to the next.
#'
#' The reveal follows `along`, not the screen: a spiral revealed along its
#' parameter winds outwards, a `coord_flip()` series grows up the vertical axis,
#' and a reversed scale grows from the right. Only the data layers change; the
#' panel background, gridlines, axes and legends are drawn whole on every frame,
#' and the scales are trained once over all the data and frozen, so nothing
#' moves as the data arrive. A layer whose own `data` lacks the `along`
#' variable (a reference line, say) is drawn whole throughout.
#'
#' `animate(nframes = )` frames are spread evenly over the range of `along` (the
#' [ease_aes()] position easing shapes the pace). Each frame is compiled from its
#' own rows, so a statistic (a smooth) is recomputed on the data revealed so far.
#' The leading point is interpolated on the data values, before any scale
#' transformation, and among the rows of each group (the `group` aesthetic, else
#' the layer's discrete colour/fill/linetype/... mappings).
#'
#' @param plot A [PlotSpec] (from [vplot()]).
#' @param along The variable the reveal follows: numeric, `Date` or `POSIXct`
#'   (often time, but any ordering column, such as a path's parameter).
#' @return The modified [PlotSpec].
#' @seealso [transition_states()], [transition_time()], [animate()]
#' @examples
#' vplot(data.frame(t = 1:20, y = cumsum(rnorm(20)))) |>
#'   mark_line(x = t, y = y) |>
#'   transition_reveal(t)
#' @export
transition_reveal <- function(plot, along) {
  .check_plot(plot)
  var <- rlang::enquo(along)
  if (rlang::quo_is_missing(var)) {
    cli::cli_abort(
      "{.fn transition_reveal} needs an {.arg along} variable, e.g. {.code transition_reveal(time)}."
    )
  }
  plot@transition <- TransitionSpec(var = var, kind = "reveal", wrap = FALSE)
  plot
}

#' Set the easing of an animation's frames
#'
#' `ease_aes()` chooses the easing function that shapes how the interpolation
#' fraction moves through each transition — e.g. `"cubic-in-out"` starts and ends
#' gently. By default it applies to every interpolated aesthetic; the
#' `position`/`color`/`size`/`alpha` arguments override it for one class each, so
#' marks can glide into place on a cubic curve while their colour crossfades
#' evenly.
#'
#' @param plot A [PlotSpec] carrying a [transition_states()].
#' @param ease An easing name: `"linear"`, or a family
#'   (`quad`/`cubic`/`quart`/`quint`/`sine`/`expo`/`circ`/`back`/`elastic`/`bounce`)
#'   with a direction suffix (`-in`, `-out`, `-in-out`), e.g. `"cubic-in-out"`.
#'   Used for any class not given its own easing below.
#' @param position,color,size,alpha Optional per-class easings, named the same way
#'   as `ease`. `NULL` (the default) means "use `ease`". Each covers one class of
#'   drawn property:
#'
#'   * `position` — x and y, and every other coordinate-space quantity (bar
#'     widths, angles, path vertices, text rotation). It also decides *when*
#'     discrete attributes flip, since those snap at the halfway point.
#'   * `color` — `color` and `fill`, including per-element fills.
#'   * `size` — `size`, `linewidth`, and radii.
#'   * `alpha` — opacity, **and the fade of marks entering or leaving** a state.
#'
#'   `position` covers x and y together: the engine has a single positional
#'   curve, so the two axes cannot be eased apart.
#' @param colour An alias for `color`; supplying both is an error.
#' @return The modified [PlotSpec].
#' @seealso [transition_states()], [animate()]
#' @examples
#' # Four bars in every state, so each bar tweens into its next height.
#' d <- data.frame(t = rep(1:3, each = 4), g = rep(letters[1:4], 3),
#'                 v = c(1, 3, 2, 4, 4, 1, 3, 2, 2, 4, 1, 3))
#' vplot(d) |>
#'   mark_bar(x = g, y = v, fill = g) |>
#'   transition_states(t) |>
#'   ease_aes("cubic-in-out")
#'
#' # Bars settle into place gently, but the colour crossfade stays even.
#' vplot(d) |>
#'   mark_bar(x = g, y = v, fill = v) |>
#'   transition_states(t) |>
#'   ease_aes("cubic-in-out", color = "linear")
#' @export
ease_aes <- function(
  plot,
  ease = "linear",
  position = NULL,
  color = NULL,
  size = NULL,
  alpha = NULL,
  colour = NULL
) {
  .check_plot(plot)
  ease <- ease[[1L]]
  .validate_ease(ease)
  if (!is.null(colour)) {
    if (!is.null(color)) {
      cli::cli_abort("Supply either {.arg color} or {.arg colour}, not both.")
    }
    color <- colour
  }
  # `NA` in a class slot means "fall back to `ease`" (see `.ease_classes()`).
  one <- function(x) {
    if (is.null(x)) {
      return(NA_character_)
    }
    x <- x[[1L]]
    .validate_ease(x)
    x
  }
  plot@ease <- EaseSpec(
    default = ease,
    position = one(position),
    color = one(color),
    size = one(size),
    alpha = one(alpha)
  )
  plot
}

# The four easings an EaseSpec resolves to, one per property class the engine
# tweens separately. Every unset class falls back to `default`, so a plain
# `ease_aes("cubic-in-out")` still shapes the whole scene and produces the same
# schedule the single-curve code produced.
.ease_classes <- function(ease) {
  if (is.null(ease)) {
    ease <- EaseSpec()
  }
  pick <- function(x) if (is.na(x)) ease@default else x
  list(
    position = pick(ease@position),
    color = pick(ease@color),
    size = pick(ease@size),
    alpha = pick(ease@alpha)
  )
}

.pos_num <- function(x, arg) {
  x <- x[[1L]]
  if (!is.numeric(x) || !is.finite(x) || x <= 0) {
    cli::cli_abort("{.arg {arg}} must be a positive number.")
  }
  as.double(x)
}
.pos_whole <- function(x, arg) {
  x <- x[[1L]]
  if (!is.numeric(x) || !is.finite(x) || x < 1 || x != round(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a positive whole number, not {.val {x}}."
    )
  }
  as.double(x)
}
.nonneg_num <- function(x, arg) {
  x <- x[[1L]]
  if (!is.numeric(x) || !is.finite(x) || x < 0) {
    cli::cli_abort("{.arg {arg}} must be a non-negative number.")
  }
  as.double(x)
}

# ---- the animation object --------------------------------------------------

# K compiled keyframe scenes plus the timing/easing needed to schedule the
# in-between frames. Produced by animate(); consumed by anim_save().
vellum_animation <- S7::new_class(
  "vellum_animation",
  package = "vellumplot",
  properties = list(
    scenes = S7::class_list, # K vellum_scene objects (one per state)
    states = S7::class_character, # state labels, in order
    nframes = S7::new_property(S7::class_double, default = 50),
    fps = S7::new_property(S7::class_double, default = 25),
    seg_weights = S7::new_property(S7::class_double, default = numeric(0)), # per segment
    state_length = S7::new_property(S7::class_double, default = 1),
    wrap = S7::new_property(S7::class_logical, default = TRUE),
    easing = S7::new_property(S7::class_character, default = "linear"),
    # Per-class easing overrides (a named list from `.ease_classes()`); an absent
    # or `NA` entry falls back to `easing`, so an empty list = one curve for
    # everything, exactly as before per-aesthetic easing existed.
    easing_aes = S7::new_property(S7::class_list, default = list()),
    # Frame variables in the title/subtitle/caption (VP-82): the templates, how
    # each band was drawn, and what the per-frame values are computed from (see
    # `.frame_text()`). Empty when the labels carry no `{...}`.
    frame_text = S7::new_property(S7::class_list, default = list()),
    # Frames held on the first / last frame, and whether the animation plays
    # back to the start (VP-95, gganimate's `start_pause`, `end_pause`,
    # `rewind`). They are part of `nframes`.
    start_pause = S7::new_property(S7::class_double, default = 0),
    end_pause = S7::new_property(S7::class_double, default = 0),
    rewind = S7::new_property(S7::class_logical, default = FALSE),
    width = S7::new_property(S7::class_double, default = 6),
    height = S7::new_property(S7::class_double, default = 4),
    dpi = S7::new_property(S7::class_double, default = 96)
  )
)

#' Build a keyframe animation from a plot
#'
#' `animate()` compiles a plot carrying a [transition_states()] into one keyframe
#' scene per state, training the scales once over all states and freezing them,
#' and returns a `vellum_animation`. Write it with [anim_save()]; printing it
#' shows the first keyframe as a preview.
#'
#' Per-state statistics (e.g. a smooth) are recomputed per keyframe, but no scale
#' domain retrains — the frozen scales are the observable proof the animation
#' stayed non-reactive (axis breaks do not move between frames).
#'
#' # Which mark becomes which
#'
#' Marks tween by **identity**, as gganimate matches by group: each mark of one
#' state moves into the mark that *is the same thing* in the next state, however
#' the rows are ordered and whatever groups each state contains. A mark's
#' identity is
#'
#' 1. its `data_id`, when the layer declares one (`mark_point(data_id =
#'    country)`): the way to follow individual observations;
#' 2. otherwise its group -- the `group` aesthetic if mapped, else the
#'    combination of the layer's discrete mapped aesthetics (`color`, `fill`,
#'    `shape`, ..., and a discrete `x`, `y` or `label`) -- so a bar race's bars,
#'    filled by country, slide between ranks keeping their colours;
#' 3. and within a group, the order of its rows -- the last resort, enough when
#'    each group has one mark per state, but rows of one group pair up by
#'    position. Give such a layer a `data_id` (or a `group`) when its rows are
#'    individual observations whose order may change.
#'
#' A line, path or area is one mark per series and is matched by its group.
#'
#' A faceted plot animates panel by panel: every panel of the whole data is
#' drawn in every frame (a state without rows for a panel leaves it empty
#' rather than dropping it), and a mark's identity includes its panel, so a
#' mark never slides from one panel into another -- a row whose facet value
#' changes between states fades out of one panel and into the other. The
#' scales are frozen across frames, so per-panel scales (`facet_wrap(scales =
#' "free")`, an independent `resolve_scale()`) are refused.
#'
#' A mark found in only one of two adjacent states fades in or out (points,
#' bars and tiles, labels, lines and areas alike, with their effect underlays
#' such as [glow()]) on the alpha easing, and a point whose colour group changes
#' slides to its new place while its colour blends. A line, path or area whose
#' number of points changes morphs from one outline into the other. Where marks
#' overlap, the stacking order switches to the next state's halfway through the
#' transition. What cannot move switches whole halfway through: decoration
#' without an identity that comes and goes, or a line that cannot be morphed (a
#' broken line with missing values, mixed units). `animate()` warns (once per
#' call) when that will happen, naming the states.
#'
#' Give each transition enough frames to be seen: `nframes` is spread over all
#' the states, so 40 states in 80 frames leave two frames per state and nothing
#' visibly moves.
#'
#' # Frame variables
#'
#' A title, subtitle or caption (set with [labs()]) may embed frame variables
#' in braces, glue-style, as in gganimate: `labs(title = "Year: {frame_time}")`.
#' Each `{...}` is an R expression evaluated per frame with the variables below
#' in scope (and the environment `animate()` was called from behind them), so
#' `"{round(frame_time)}"` or `"{format(frame_time, '%Y')}"` work; write `{{`
#' and `}}` for literal braces. The label is updated on **every frame**, not
#' only at keyframes: a `{frame_time}` title counts through the in-between
#' times while the marks move.
#'
#' * every transition: `frame` (the frame number), `nframes`, `progress`
#'   (`frame / nframes`);
#' * [transition_states()]: `closest_state`, `previous_state`, `next_state`
#'   (the state the frame is nearest to, comes from and goes to; all three are
#'   the held state during a pause) and `transitioning` (`TRUE` while moving).
#'   `closest_state` switches halfway through a transition, on the position
#'   easing, when the marks' discrete attributes do;
#' * [transition_time()]: `frame_time`, the time the frame has reached (on the
#'   position easing, so it agrees with where the marks are), of the time
#'   column's class (`Date`, `POSIXct`, or double);
#' * [transition_reveal()]: `frame_along`, the `along` value revealed so far.
#'
#' A [mark_text()] label can use the same variables (VP-93): `mark_text(data =
#' data.frame(x = 3000, y = 40), x = x, y = y, label = "{round(frame_time)}",
#' size = 60)` draws the year large behind a gapminder bubble chart, counting
#' on every frame. A constant label or a column of labels may contain `{...}`;
#' the label is placed (and moves) as the mark it belongs to, and only its
#' string changes per frame. Repelled and rich-text labels are not templated.
#' An ordinary data label whose string differs between two states takes the
#' nearer state's string, switching halfway through.
#'
#' # Pauses and rewind
#'
#' `start_pause` and `end_pause` hold the first and last frame for that many
#' frames, and `rewind = TRUE` plays the animation back to the start, as in
#' gganimate. They come out of `nframes`: `animate(nframes = 100, end_pause =
#' 20)` moves for 80 frames and rests on the final picture for 20. Every output
#' honours them -- [anim_save()] to GIF, APNG or SVG, [anim_frame()]'s frame
#' numbers, and the frame scrubber of an animated `vellumwidget`.
#'
#' @param plot A [PlotSpec] with a [transition_states()] (and optionally an
#'   [ease_aes()]).
#' @param nframes Total number of frames to render, a positive whole number.
#'   The pauses and the rewind are part of it, as in gganimate.
#' @param fps Frames per second (the playback rate written into the file).
#' @param start_pause,end_pause Number of frames to hold the first and the last
#'   frame for, before the animation starts and after it ends (default 0).
#'   A looping file (every format loops) then rests on the final picture
#'   before starting over: `end_pause = fps` holds it for a second.
#' @param rewind Play the animation backwards after it has played forwards, so
#'   the loop returns to its start smoothly instead of cutting (default
#'   `FALSE`). The frames are split between the two directions; the end pause
#'   sits between them and the start pause at both ends.
#' @return A `vellum_animation`.
#' @seealso [transition_states()], [ease_aes()], [anim_save()], [anim_frame()]
#' @examples
#' \dontrun{
#' # Four bars in every state, so each bar tweens into its next height.
#' d <- data.frame(t = rep(1:3, each = 4), g = rep(letters[1:4], 3),
#'                 v = c(1, 3, 2, 4, 4, 1, 3, 2, 2, 4, 1, 3))
#' vplot(d) |>
#'   mark_bar(x = g, y = v, fill = g) |>
#'   transition_states(t) |>
#'   animate(nframes = 60, fps = 25)
#' }
#' @export
animate <- function(
  plot,
  nframes = 50,
  fps = 25,
  start_pause = 0,
  end_pause = 0,
  rewind = FALSE
) {
  .check_plot(plot)
  env <- rlang::caller_env()
  pauses <- .anim_pauses(nframes, start_pause, end_pause, rewind)
  tr <- plot@transition
  if (is.null(tr)) {
    cli::cli_abort(
      "{.fn animate} needs a transition; add {.fn transition_states} or {.fn transition_time} first."
    )
  }
  nframes <- .pos_whole(nframes, "nframes")
  fps <- .pos_num(fps, "fps")
  easing <- if (is.null(plot@ease)) "linear" else plot@ease@default
  easing_aes <- .ease_classes(plot@ease)

  # A faceted animation keeps every panel in every frame (VP-84): the panels
  # are laid out from the whole data, not each state's.
  if (!is.null(plot@facet)) {
    .anim_check_facet(plot)
    .mark_ctx$anim_facet_datas <- c(
      list(plot@data),
      Filter(Negate(is.null), lapply(plot@layers, function(L) L@data))
    )
    on.exit(rm(list = "anim_facet_datas", envir = .mark_ctx), add = TRUE)
  }

  # A reveal is not a state tween: each frame is its own keyframe, compiled
  # from the rows revealed so far (see `.animate_reveal()`).
  if (identical(tr@kind, "reveal")) {
    return(.anim_with_pauses(
      .animate_reveal(plot, pauses$core, fps, easing, easing_aes, env),
      nframes,
      pauses
    ))
  }

  # Enumerate the states and per-segment timing. `transition_states` gives equal
  # segments over a column's levels; `transition_time` treats the column as a
  # numeric time and weights each segment by its time gap, so the animation plays
  # at a constant rate through unevenly spaced times (and never holds or wraps).
  en <- .enumerate_states(tr, plot@data)
  key <- en$key
  states <- en$states
  if (length(states) < 2L) {
    cli::cli_abort(c(
      "A transition needs at least 2 states.",
      i = "The {.field {tr@kind}} column has {length(states)} distinct value{?s}."
    ))
  }

  # Frame variables in the labels (VP-82): each keyframe is compiled with its
  # own state's values (which also sizes the title band); the render then
  # swaps in every frame's string (`.frame_text()`).
  tpl <- .frame_templates(plot)
  fvars <- list(
    kind = tr@kind,
    states = en$labels %||% states,
    times = en$times,
    restore = en$restore,
    nframes = nframes,
    env = env
  )
  # A per-state spec: the base plot (transition stripped) over that state's rows.
  base <- plot
  base@transition <- NULL
  base@ease <- NULL
  state_spec <- function(level) {
    s <- base
    if (length(tpl)) {
      k <- match(level, states)
      s@labels[names(tpl)] <- .glue_frame(
        tpl,
        .keyframe_vars(fvars, k),
        env
      )
    }
    # `which()` (not a bare logical mask): `key` is a factor and rows with a
    # non-finite state value carry `key = NA`, so `key == level` would otherwise
    # inject an all-NA row into every keyframe.
    s@data <- plot@data[which(key == level), , drop = FALSE]
    # A layer with its OWN `data =` must also be filtered to the current state --
    # otherwise every keyframe draws all states' rows of that layer at once (a
    # per-state annotation would pile up). Only filter a layer whose data actually
    # carries the state column; a layer with no state dimension (a static
    # reference line, say) keeps its data whole and persists across every frame.
    s@layers <- lapply(s@layers, function(L) {
      if (is.null(L@data)) {
        return(L)
      }
      lk <- .layer_state_key(tr, L@data)
      if (is.null(lk)) {
        return(L)
      }
      L@data <- L@data[which(lk == level), , drop = FALSE]
      L
    })
    s
  }

  # Freeze the scales: resolve every state's layers (per-state stats included),
  # pool them, and train ONCE over the union so every keyframe shares one frozen
  # set of scales. Training over the pooled resolved values gives the union domain
  # across states — the guardrail that keeps the animation non-reactive.
  # The rows this pass drops are dropped again, and reported, by the keyframe
  # compiles below, against the frozen scales the frames are drawn with.
  removed <- .removed_rows_tally()
  pooled <- removed$muffle(unlist(
    lapply(states, function(level) {
      bp <- .build_panels(state_spec(level))
      unlist(lapply(bp$panels, function(p) p$resolved), recursive = FALSE)
    }),
    recursive = FALSE
  ))
  frozen <- .train_scales(base, pooled)

  # Key every mark by its data identity so the tween pairs marks across states
  # by what they are, not by where they sit in the scene (VR-32).
  .mark_ctx$anim_identity <- TRUE
  on.exit(rm(list = "anim_identity", envir = .mark_ctx), add = TRUE)
  if (length(tpl)) {
    .mark_ctx$anim_bands <- stats::setNames(
      vector("list", length(tpl)),
      names(tpl)
    )
    on.exit(rm(list = "anim_bands", envir = .mark_ctx), add = TRUE)
  }
  # Per-state bookkeeping while the keyframes compile: group sizes for the
  # row-order hint (VP-87), and the keyframe's frame variables and how its
  # templated data labels were drawn (VP-93).
  .mark_ctx$anim_counts <- list()
  .mark_ctx$anim_labels <- list()
  .mark_ctx$anim_env <- env
  on.exit(
    rm(
      list = intersect(
        c("anim_counts", "anim_state", "anim_labels", "anim_env", "anim_kvars", "anim_k"),
        ls(.mark_ctx)
      ),
      envir = .mark_ctx
    ),
    add = TRUE
  )
  scenes <- lapply(seq_along(states), function(k) {
    level <- states[[k]]
    .mark_ctx$anim_state <- as.character(level)
    .mark_ctx$anim_k <- k
    .mark_ctx$anim_kvars <- .keyframe_vars(fvars, k)
    removed$collect(.compile_plot(state_spec(level), frozen_scales = frozen))
  })
  # One "Removed n rows" warning per layer (VP-150). Each state's rows are
  # drawn in one keyframe only, so a layer's counts add up over the states,
  # except a layer whose own data has no state column: it is drawn whole in
  # every keyframe, so its rows count once.
  persists <- vapply(
    base@layers,
    function(L) !is.null(L@data) && is.null(.layer_state_key(tr, L@data)),
    logical(1)
  )
  removed$emit(function(i) if (isTRUE(persists[i])) max else sum)
  .inform_row_order(.mark_ctx$anim_counts)
  labels <- .mark_ctx$anim_labels
  .warn_unpaired(scenes, en$labels %||% states, wrap = en$wrap)
  frame_text <- list()
  bands <- if (length(tpl)) Filter(Negate(is.null), .mark_ctx$anim_bands)
  if (length(bands) || length(labels)) {
    frame_text <- c(
      fvars,
      list(
        templates = tpl[names(bands)],
        bands = bands,
        labels = labels,
        wrap = en$wrap
      )
    )
  }

  a <- vellum_animation(
    scenes = scenes,
    states = states,
    nframes = nframes,
    fps = fps,
    seg_weights = en$seg_weights,
    state_length = en$state_length,
    wrap = en$wrap,
    easing = easing,
    easing_aes = easing_aes,
    frame_text = frame_text,
    width = plot@width,
    height = plot@height,
    dpi = plot@dpi
  )
  .anim_with_pauses(a, nframes, pauses)
}

# ---- pauses and rewind (VP-95) ----------------------------------------------

# Validate the pauses and split `nframes` the gganimate way: with `rewind`
# half the frames (rounded up) play forwards; the pauses come out of those,
# leaving `core` frames of movement (at least 2).
.anim_pauses <- function(nframes, start_pause, end_pause, rewind) {
  nframes <- .pos_whole(nframes, "nframes")
  sp <- .nonneg_whole(start_pause, "start_pause")
  ep <- .nonneg_whole(end_pause, "end_pause")
  if (!is.logical(rewind) || length(rewind) != 1L || is.na(rewind)) {
    cli::cli_abort("{.arg rewind} must be {.code TRUE} or {.code FALSE}.")
  }
  half <- if (rewind) ceiling(nframes / 2) else nframes
  core <- half - sp - ep
  if (core < 2L && (sp > 0 || ep > 0 || rewind)) {
    cli::cli_abort(c(
      "The pauses leave too few frames to animate.",
      i = "{.arg nframes} = {nframes}{if (rewind) ' (halved by rewind)' else ''} with {.arg start_pause} = {sp} and {.arg end_pause} = {ep} leaves {core}; increase {.arg nframes}."
    ))
  }
  list(core = max(core, 1L), start = sp, end = ep, rewind = rewind)
}

.nonneg_whole <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 || x != round(x)) {
    cli::cli_abort("{.arg {arg}} must be a non-negative whole number.")
  }
  as.integer(x)
}

# Attach the pauses to an animation compiled for `pauses$core` frames of
# movement, and restore its total frame count.
.anim_with_pauses <- function(a, nframes, pauses) {
  a@nframes <- as.double(pauses$core)
  a@start_pause <- as.double(pauses$start)
  a@end_pause <- as.double(pauses$end)
  a@rewind <- isTRUE(pauses$rewind)
  a
}

# The schedule an animation renders: its core frames (`.anim_schedule()`),
# then the pauses and the rewind applied by repeating / reversing frames, as
# gganimate does. Every output (GIF, APNG, SVG, the widget) is drawn from this.
.anim_frame_schedule <- function(animation) {
  sched <- .anim_schedule(
    length(animation@scenes),
    animation@nframes,
    .anim_easings(animation),
    animation@seg_weights,
    animation@state_length,
    animation@wrap
  )
  m <- length(sched$seg)
  idx <- c(
    rep(1L, animation@start_pause),
    seq_len(m),
    rep(m, animation@end_pause)
  )
  if (isTRUE(animation@rewind)) {
    idx <- c(idx, rev(idx))
  }
  lapply(sched, function(v) v[idx])
}

# The total number of frames an animation renders.
.anim_total_frames <- function(animation) {
  n <- animation@nframes + animation@start_pause + animation@end_pause
  if (isTRUE(animation@rewind)) 2 * n else n
}

# ---- frame variables (VP-82) ------------------------------------------------

# A faceted animation shares one frozen set of scales across its frames, so
# per-panel scales cannot be honoured: refuse them rather than silently fix
# them (VP-84).
.anim_check_facet <- function(plot) {
  free <- c(
    if (identical(.resolve_for(plot, "x"), "independent")) "x",
    if (identical(.resolve_for(plot, "y"), "independent")) "y",
    .independent_aes(plot)
  )
  if (length(free)) {
    cli::cli_abort(c(
      "{.fn animate} cannot animate per-panel scales ({.val {free}}).",
      i = "An animation trains its scales once over every state and freezes them, so each panel's own scale would not hold still.",
      i = "Use {.code scales = \"fixed\"} in the facet (and shared scales in {.fn resolve_scale})."
    ))
  }
  invisible()
}

# The labels that carry frame variables: the title, subtitle and caption whose
# text holds a `{`.
.frame_templates <- function(plot) {
  out <- list()
  for (slot in c("title", "subtitle", "caption")) {
    x <- plot@labels[[slot]]
    if (is.character(x) && length(x) == 1L && !is.na(x) && grepl("{", x, fixed = TRUE)) {
      out[[slot]] <- x
    }
  }
  out
}

# Substitute frame variables into each template, glue-style: every `{expr}` is
# evaluated with `vars` in scope over `env`; `{{` and `}}` are literal braces.
.glue_frame <- function(templates, vars, env) {
  lapply(templates, function(x) .glue_one(x, vars, env))
}

.glue_one <- function(x, vars, env) {
  open <- "\001"
  close <- "\002"
  x <- gsub("{{", open, x, fixed = TRUE)
  x <- gsub("}}", close, x, fixed = TRUE)
  m <- gregexpr("\\{[^{}]*\\}", x)
  if (m[[1L]][1L] != -1L) {
    exprs <- regmatches(x, m)[[1L]]
    vals <- vapply(
      exprs,
      function(e) {
        code <- substr(e, 2L, nchar(e) - 1L)
        v <- tryCatch(
          rlang::eval_tidy(rlang::parse_expr(code), data = vars, env = env),
          error = function(err) {
            cli::cli_abort(
              "Could not evaluate {.code {e}} in an animation label.",
              parent = err
            )
          }
        )
        paste(.frame_chr(v), collapse = "")
      },
      character(1)
    )
    regmatches(x, m) <- list(vals)
  }
  x <- gsub(open, "{", x, fixed = TRUE)
  gsub(close, "}", x, fixed = TRUE)
}

# A frame variable's text: a date-time by `format()` (no time zone suffix, as
# `as.character()` would print), anything else by `as.character()`.
.frame_chr <- function(v) {
  if (inherits(v, "POSIXt")) format(v) else as.character(v)
}

# The frame variables at keyframe `k` (used to compile that keyframe).
.keyframe_vars <- function(fv, k) {
  common <- list(frame = 1L, nframes = fv$nframes, progress = 0)
  if (identical(fv$kind, "time")) {
    c(common, list(frame_time = fv$restore(fv$times[[k]])))
  } else {
    st <- fv$states[[k]]
    c(
      common,
      list(
        closest_state = st,
        previous_state = st,
        next_state = st,
        transitioning = FALSE
      )
    )
  }
}

# The frame variables of every frame of a schedule (`seg`, eased position
# `frac`), one list per frame.
.schedule_vars <- function(fv, sched) {
  n <- length(sched$seg)
  k <- length(fv$states)
  lapply(seq_len(n), function(f) {
    seg <- sched$seg[[f]]
    t <- sched$frac[[f]]
    nxt <- if (seg + 1L > k) 1L else seg + 1L # a wrap's last move ends on state 1
    common <- list(frame = f, nframes = n, progress = f / n)
    if (identical(fv$kind, "time")) {
      a <- fv$times[[seg]]
      b <- fv$times[[nxt]]
      return(c(common, list(frame_time = fv$restore(a + t * (b - a)))))
    }
    from <- fv$states[[seg]]
    to <- fv$states[[nxt]]
    if (t <= 0) {
      to <- from
    } else if (t >= 1) {
      from <- to
    }
    c(
      common,
      list(
        closest_state = if (t < 0.5) from else to,
        previous_state = from,
        next_state = to,
        transitioning = t > 0 && t < 1
      )
    )
  })
}

# The per-frame text a render needs (VP-82): every frame's title/subtitle/
# caption strings, shaped once per distinct combination in a small scene keyed
# like the keyframes' band text ("vp:title"), and each frame's variant index.
# NULL when the animation has no frame variables.
.frame_text <- function(animation, sched) {
  ft <- animation@frame_text
  if (!length(ft) || (!length(ft$bands) && !length(ft$labels))) {
    return(NULL)
  }
  vars <- .schedule_vars(ft, sched)
  k <- length(ft$states)
  # The keyframe each frame is nearest to (on the position fraction): its
  # templated data labels are the ones the frame shows (VP-93).
  near <- ifelse(sched$frac < 0.5, sched$seg, sched$seg + 1L)
  near[near > k] <- 1L
  strings <- lapply(seq_along(vars), function(f) {
    v <- vars[[f]]
    bands <- unlist(.glue_frame(ft$templates, v, ft$env))
    recs <- ft$labels[[as.character(near[[f]])]]
    labs <- vapply(
      recs,
      function(r) .glue_one(r$template, v, ft$env),
      character(1)
    )
    list(bands = bands, labels = labs, near = near[[f]])
  })
  id <- vapply(
    strings,
    function(x) {
      paste(c(x$bands, names(x$labels), x$labels), collapse = "\037")
    },
    character(1)
  )
  uniq <- unique(id)
  variants <- lapply(match(uniq, id), function(i) {
    sc <- vellum::vl_scene(
      width = animation@width,
      height = animation@height,
      dpi = animation@dpi
    )
    for (slot in names(ft$bands)) {
      b <- ft$bands[[slot]]
      sc <- vellum::draw(
        sc,
        .band_text_grob(
          strings[[i]]$bands[[slot]],
          b$el,
          b$band_w,
          b$default_hjust,
          key = paste0("vp:", slot)
        )
      )
    }
    recs <- ft$labels[[as.character(strings[[i]]$near)]]
    for (key in names(recs)) {
      r <- recs[[key]]
      sc <- vellum::draw(
        sc,
        vellum::text_grob(
          strings[[i]]$labels[[key]],
          just = r$just,
          rot = r$rot,
          gp = r$gp,
          key = key
        )
      )
    }
    sc
  })
  list(variants = variants, frame = match(id, uniq))
}

# Warn, once, when adjacent keyframes cannot tween: marks are matched by
# identity (`.anim_keys()`) and a mark only in one state fades, so what is left
# is a line/path whose vertex count changes and cannot be morphed (it switches
# whole at the midpoint), an unkeyed node that comes and goes, or -- rarely --
# two keyframes that do not
# share a layout at all (a cut). Say which states and why.
.warn_unpaired <- function(scenes, states, wrap = FALSE) {
  ks <- if (wrap) c(scenes, scenes[1L]) else scenes
  labs <- if (wrap) c(states, states[1L]) else states
  p <- vellum::vl_tween_pairing(ks)
  cut <- which(!p$paired)
  snapped <- which(p$paired & !is.na(p$snapped) & p$snapped > 0L)
  if (!length(cut) && !length(snapped)) {
    return(invisible())
  }
  pair_lab <- function(i) paste0(labs[p$from[i]], " -> ", labs[p$to[i]])
  msg <- c("Some keyframes will cut rather than tween.")
  if (length(cut)) {
    msg <- c(
      msg,
      "*" = "{.val {pair_lab(cut)}}: the states' scenes are laid out differently, so the frame switches to the next state halfway through."
    )
  }
  if (length(snapped)) {
    msg <- c(
      msg,
      "*" = "{.val {pair_lab(snapped)}}: some marks cannot be matched between the states (an unkeyed decoration that comes and goes, or a line or path whose number of points changes and cannot be morphed), so they switch halfway through instead of moving."
    )
  }
  msg <- c(
    msg,
    i = "Marks are matched by their identity (see {.fn animate}); one that is only in one state fades in or out."
  )
  cli::cli_warn(msg, class = "vellumplot_anim_cut")
}

# Animation identity (VR-32). The tween matches marks across keyframes by key,
# so each mark of an animation keyframe is keyed by what it IS:
#
# 1. an explicit `data_id` (or a graph mark's vertex / edge key) wins;
# 2. otherwise its group: the `group` aesthetic when mapped or set, else the
#    interaction of the layer's discrete mapped aesthetics -- those that group a
#    layer (`.GROUP_AES`: colour, fill, shape, ...) plus a discrete `x`, `y` or
#    `label` -- as gganimate matches by ggplot2's group;
# 3. and, within that, the row's order among the rows sharing it: the last
#    resort for a layer whose rows are distinct observations of one group (the
#    points of one colour). A repeated `data_id` is told apart the same way.
#
# A line, path or polygon is one mark per series, so it is keyed by its group
# alone (`series`, picked up in `.draw()`), not by its first row.
#
# Keys are prefixed with the layer ("l<layer>|...") so two layers never match
# each other's marks. Returns `key` (one per row) and `series` (one per row; NULL
# when an explicit id is used, in which case a series takes its first row's id).
.anim_keys <- function(L, layer, explicit = NULL) {
  n <- L$n
  pfx <- paste0("l", layer, "|")
  # Faceted (VP-84): the same row may be drawn in several panels and a mark
  # must not slide between panels, so the panel is part of the identity.
  if (!is.null(.mark_ctx$anim_facet_datas) && !is.na(.mark_ctx$panel %||% NA)) {
    pfx <- paste0(.mark_ctx$panel, "|", pfx)
  }
  if (!is.null(explicit) && length(explicit) == n) {
    id <- as.character(explicit)
    id[is.na(id)] <- "NA"
    occ <- .occurrence(id)
    key <- ifelse(occ > 1L, paste0(id, "#", occ), id)
    return(list(key = paste0(pfx, key), series = NULL))
  }
  series <- .anim_group(L, .GROUP_AES)
  id <- .anim_group(L, c(.GROUP_AES, "x", "y", "label"))
  .anim_note_row_order(pfx, L, id)
  list(
    key = paste0(pfx, id, "#", .occurrence(id)),
    series = paste0(pfx, series)
  )
}

# Marks drawn one per series (matched by their group, never by row order).
.SERIES_MARKS <- c(
  "line", "path", "area", "ribbon", "step", "polygon", "smooth", "density",
  "violin", "ridgeline"
)

# Record, for the row-order hint (VP-87), how many rows of each identity group
# layer `pfx` drew in the state being compiled.
.anim_note_row_order <- function(pfx, L, id) {
  st <- .mark_ctx$anim_state
  if (is.null(st) || !length(id) || L$mark %in% .SERIES_MARKS) {
    return(invisible())
  }
  counts <- .mark_ctx$anim_counts %||% list()
  prev <- counts[[pfx]][[st]]
  tab <- table(id)
  if (!is.null(prev)) {
    tab <- tapply(c(prev, tab), names(c(prev, tab)), sum)
  }
  counts[[pfx]][[st]] <- c(tab)
  .mark_ctx$anim_counts <- counts
  invisible()
}

# Tell the user, once per `animate()` call, when marks without a `data_id`
# fall back to row order within a group whose size changes between states:
# the k-th row of a group in one state then pairs with the k-th row in the
# next, which need not be the same observation (VP-87).
.inform_row_order <- function(counts) {
  hit <- FALSE
  for (pfx in names(counts)) {
    per_state <- counts[[pfx]]
    if (length(per_state) < 2L) {
      next
    }
    ids <- unique(unlist(lapply(per_state, names)))
    n <- vapply(
      per_state,
      function(tab) {
        v <- tab[ids]
        v[is.na(v)] <- 0
        as.double(v)
      },
      numeric(length(ids))
    )
    n <- matrix(n, nrow = length(ids))
    # A group with more than one row somewhere whose size is not constant.
    if (any(apply(n, 1L, function(r) max(r) > 1 && length(unique(r)) > 1L))) {
      hit <- TRUE
      break
    }
  }
  if (hit) {
    cli::cli_inform(
      c(
        "i" = "Some marks have no {.arg data_id}, so within each group they are matched by row order, and the groups change size between states.",
        " " = "A mark may then move to another row's place. Give the layer a {.arg data_id} (e.g. {.code mark_point(data_id = id)}) to follow each observation."
      ),
      class = "vellumplot_anim_row_order"
    )
  }
  invisible(hit)
}

# The identity group of each row: the `group` aesthetic, else the discrete
# mapped aesthetics among `aes` pasted together in that fixed order (every
# component kept, so the key is the same in every state whatever subset of the
# levels a state happens to contain).
.anim_group <- function(L, aes) {
  n <- L$n
  g <- L$values$group %||% L$params$group
  if (!is.null(g)) {
    return(rep_len(as.character(g), n))
  }
  parts <- list()
  for (a in aes) {
    v <- L$values[[a]]
    if (length(v) && !.is_continuous_group(v)) {
      parts[[a]] <- rep_len(as.character(v), n)
    }
  }
  if (!length(parts)) {
    return(rep("", n))
  }
  do.call(paste, c(unname(parts), sep = ":"))
}

# The position of each element among the equal elements before it: 1, 2, ...
.occurrence <- function(x) {
  if (!length(x)) {
    return(integer(0))
  }
  stats::ave(seq_along(x), x, FUN = seq_along)
}

# A reveal animation (VP-81): one keyframe per frame, each the plot compiled
# from the rows whose `along` value the frame has reached -- plus, for a
# line-like layer, an interpolated leading row per group -- on scales trained
# once over the whole data and frozen. The frames sit exactly on their
# keyframes (seg = frame, frac = 0; the last at frac = 1 of the final segment),
# so the engine draws each keyframe as compiled; the easing is applied to the
# cutoffs, not to the schedule.
.animate_reveal <- function(
  plot,
  nframes,
  fps,
  easing,
  easing_aes = list(),
  env = rlang::caller_env()
) {
  tr <- plot@transition
  base <- plot
  base@transition <- NULL
  base@ease <- NULL

  # (A faceted reveal's panels are laid out from the whole data: `animate()`
  # set that up before dispatching here.)
  along <- .reveal_along(tr@var, plot@data, required = TRUE)
  lay_along <- lapply(plot@layers, function(L) {
    if (is.null(L@data)) NULL else .reveal_along(tr@var, L@data)
  })
  vals <- c(along, unlist(lay_along, use.names = FALSE))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) {
    cli::cli_abort(
      "{.fn transition_reveal} found no finite {.arg along} values."
    )
  }
  rng <- range(vals)

  # Cutoffs: evenly spread over the range of `along`, shaped by the position
  # easing. At least two keyframes (the engine tweens pairs); a one-frame
  # reveal shows the whole plot.
  n <- max(2L, as.integer(nframes))
  raw <- (seq_len(n) - 1) / (n - 1)
  pos_ease <- (easing_aes$position %||% NA_character_)
  if (is.na(pos_ease)) {
    pos_ease <- easing
  }
  cuts <- rng[1L] + .ease_apply(raw, pos_ease) * diff(rng)
  if (nframes < 2L) {
    cuts <- rep(rng[2L], 2L)
  }

  # Frame variables: each frame is its own keyframe, so its labels are simply
  # compiled with its own values.
  tpl <- .frame_templates(plot)
  restore <- .time_restorer(rlang::eval_tidy(tr@var, plot@data))
  frame_spec <- function(cut, i) {
    s <- base
    if (length(tpl)) {
      s@labels[names(tpl)] <- .glue_frame(
        tpl,
        list(
          frame = i,
          nframes = length(cuts),
          progress = i / length(cuts),
          frame_along = restore(cut)
        ),
        env
      )
    }
    s@data <- .reveal_rows(plot@data, along, cut)
    s@layers <- lapply(seq_along(s@layers), function(i) {
      L <- s@layers[[i]]
      own <- !is.null(L@data)
      a <- if (own) lay_along[[i]] else along
      if (is.null(a)) {
        return(L) # no `along` in this layer's own data: drawn whole
      }
      src <- if (own) L@data else plot@data
      if (.reveal_leads(L)) {
        L@data <- .reveal_rows(src, a, cut, lead = .reveal_groups(L, src, plot@facet))
      } else if (own) {
        L@data <- .reveal_rows(src, a, cut)
      }
      L
    })
    s
  }

  # Freeze the scales over the whole data, so the axes, legends and colours
  # stay put while the rows arrive.
  removed <- .removed_rows_tally()
  bp <- removed$muffle(.build_panels(base))
  pooled <- unlist(lapply(bp$panels, function(p) p$resolved), recursive = FALSE)
  frozen <- .train_scales(base, pooled)
  # A data label's frame variables (VP-93) are filled in per frame too.
  .mark_ctx$anim_env <- env
  on.exit(
    rm(list = intersect(c("anim_kvars", "anim_env"), ls(.mark_ctx)), envir = .mark_ctx),
    add = TRUE
  )
  scenes <- lapply(seq_along(cuts), function(i) {
    .mark_ctx$anim_kvars <- list(
      frame = i,
      nframes = length(cuts),
      progress = i / length(cuts),
      frame_along = restore(cuts[[i]])
    )
    removed$collect(.compile_plot(frame_spec(cuts[[i]], i), frozen_scales = frozen))
  })
  # One "Removed n rows" warning per layer (VP-150). The frames accumulate
  # rows, so a row dropped from one is dropped from every later one: a layer's
  # count is its largest over the frames (the last, which shows every row).
  removed$emit(function(i) max)

  vellum_animation(
    scenes = scenes,
    states = format(cuts),
    nframes = as.double(nframes),
    fps = fps,
    seg_weights = rep(1, length(scenes) - 1L),
    state_length = 0,
    wrap = FALSE,
    easing = "linear",
    easing_aes = list(),
    width = plot@width,
    height = plot@height,
    dpi = plot@dpi
  )
}

# The `along` values of `data` as doubles (a Date or POSIXct by its numeric
# value), or NULL when `data` lacks the variables (`required = FALSE`).
.reveal_along <- function(var, data, required = FALSE) {
  if (!required && !all(.quo_vars(var) %in% names(data))) {
    return(NULL)
  }
  v <- tryCatch(rlang::eval_tidy(var, data), error = function(e) {
    if (required) {
      cli::cli_abort(
        "{.fn transition_reveal} could not evaluate {.arg along} in the plot data.",
        parent = e
      )
    }
    NULL
  })
  if (is.null(v)) {
    return(NULL)
  }
  if (!(is.numeric(v) || inherits(v, c("Date", "POSIXt")))) {
    cli::cli_abort(c(
      "{.fn transition_reveal} needs a numeric, Date or POSIXct {.arg along}.",
      x = "It is {.cls {class(v)[1]}}."
    ))
  }
  rep_len(as.double(unclass(v)), nrow(data))
}

# Does layer `L` grow through its rows (a line, path, area, ribbon or step),
# and so get an interpolated leading point?
.reveal_leads <- function(L) {
  L@mark %in% c("line", "path", "area", "ribbon", "step")
}

# The identity group of each row of `data` for layer `L`: its `group`
# aesthetic, else the interaction of its discrete grouping aesthetics, as the
# layer will be drawn (one line per group), split further by facet panel when
# `facet` is given. The leading point of a group is interpolated toward that
# group's own next row only (VP-94: evaluating the quosure instead of the
# channel's `expr` found no groups, so a series' leading point aimed at
# whichever series came next in `along`).
.reveal_groups <- function(L, data, facet = NULL) {
  n <- nrow(data)
  enc <- L@encoding
  ev <- function(a) {
    ch <- enc[[a]]
    if (is.null(ch) || isTRUE(ch@after)) {
      return(NULL)
    }
    v <- tryCatch(rlang::eval_tidy(ch@expr, data), error = function(e) NULL)
    if (length(v) == n) v else NULL
  }
  parts <- list()
  g <- ev("group")
  if (!is.null(g)) {
    parts$group <- as.character(g)
  } else {
    for (a in .GROUP_AES) {
      v <- ev(a)
      if (!is.null(v) && !.is_continuous_group(v)) {
        parts[[a]] <- as.character(v)
      }
    }
  }
  if (!is.null(facet)) {
    keys <- tryCatch(.facet_keys(facet, data), error = function(e) NULL)
    for (k in names(keys)) {
      if (length(keys[[k]]) == n) {
        parts[[paste0("facet_", k)]] <- as.character(keys[[k]])
      }
    }
  }
  if (!length(parts)) {
    return(rep("", n))
  }
  do.call(paste, c(unname(parts), sep = "\r"))
}

# The rows of `data` revealed at `cut` (`along <= cut`; rows with a missing
# `along` are never revealed). With `lead` (one group id per row), each group
# that has rows on both sides of `cut` gains an interpolated row placed right
# after its last revealed row: numeric (and Date/POSIXct) columns are
# interpolated between that row and the group's next one in `along` order,
# other columns copied from the last revealed row.
.reveal_rows <- function(data, along, cut, lead = NULL) {
  keep <- which(!is.na(along) & along <= cut)
  if (is.null(lead)) {
    return(data[keep, , drop = FALSE])
  }
  ord <- as.double(keep)
  extra <- list()
  extra_ord <- numeric(0)
  for (idx in split(seq_along(along), lead)) {
    idx <- idx[!is.na(along[idx])]
    idx <- idx[order(along[idx], idx)]
    before <- idx[along[idx] <= cut]
    after <- idx[along[idx] > cut]
    if (!length(before) || !length(after)) {
      next
    }
    i <- before[length(before)]
    j <- after[1L]
    a0 <- along[i]
    a1 <- along[j]
    if (a1 <= a0 || cut <= a0) {
      next
    }
    t <- (cut - a0) / (a1 - a0)
    extra[[length(extra) + 1L]] <- .lerp_row(data, i, j, t)
    extra_ord <- c(extra_ord, max(before) + 0.5)
  }
  if (!length(extra)) {
    return(data[keep, , drop = FALSE])
  }
  out <- rbind(data[keep, , drop = FALSE], do.call(rbind, extra))
  out <- out[order(c(ord, extra_ord)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Row `i` of `data` with its numeric/Date/POSIXct columns moved a fraction `t`
# of the way to row `j`'s values.
.lerp_row <- function(data, i, j, t) {
  row <- data[i, , drop = FALSE]
  for (nm in names(data)) {
    v <- data[[nm]]
    if (is.factor(v) || !(is.numeric(v) || inherits(v, c("Date", "POSIXt")))) {
      next
    }
    a <- as.double(unclass(v[i]))
    b <- as.double(unclass(v[j]))
    if (is.finite(a) && is.finite(b)) {
      val <- a + t * (b - a)
      if (is.integer(v)) {
        v <- as.double(v)
        row[[nm]] <- as.double(row[[nm]])
      }
      row[[nm]][1L] <- if (inherits(v, c("Date", "POSIXt"))) {
        structure(val, class = class(v), tzone = attr(v, "tzone"))
      } else {
        val
      }
    }
  }
  row
}

# The compile's "Removed n rows" warnings (`vellumplot_missing_rows`) while an
# animation compiles, given once per layer per animation rather than once per
# pass and keyframe (VP-150). `muffle(expr)` evaluates `expr` silencing them (a
# pass whose rows the keyframes report again); `collect(expr)` evaluates a
# keyframe compile, silencing them and keeping each layer's count; `emit(combine)`
# then warns once per layer, its keyframe counts combined by `combine(i)` (a
# function of the layer index returning `sum` or `max`).
.removed_rows_tally <- function() {
  rec <- list()
  muffle <- function(expr) {
    withCallingHandlers(
      expr,
      vellumplot_missing_rows = function(cnd) invokeRestart("muffleWarning")
    )
  }
  collect <- function(expr) {
    withCallingHandlers(
      expr,
      vellumplot_missing_rows = function(cnd) {
        key <- as.character(cnd$layer)
        r <- rec[[key]] %||% list(mark = cnd$mark, n = integer(0), cens = integer(0))
        r$n <- c(r$n, cnd$n_rows)
        r$cens <- c(r$cens, cnd$n_censored)
        rec[[key]] <<- r
        invokeRestart("muffleWarning")
      }
    )
  }
  emit <- function(combine) {
    for (key in names(rec)[order(as.integer(names(rec)))]) {
      r <- rec[[key]]
      f <- combine(as.integer(key))
      .warn_removed_rows(f(r$n), f(r$cens), as.integer(key), r$mark)
    }
    rec <<- list()
    invisible()
  }
  list(muffle = muffle, collect = collect, emit = emit)
}

# Enumerate a transition's states and the per-segment timing. Returns `key` (a
# factor assigning each data row to a state), `states` (the ordered labels),
# `seg_weights` (a relative duration per inter-keyframe segment), `state_length`
# (the held-pause weight per state), and `wrap`.
# The per-row state label for a layer's own data, aligned to the global state
# labels, or NULL when the layer's data does not carry the transition column (so
# the caller leaves that layer's data whole). Evaluated defensively: a layer whose
# data simply lacks the column must not error the whole animation.
.layer_state_key <- function(tr, data) {
  tryCatch(as.character(.enumerate_states(tr, data)$key), error = function(e) {
    NULL
  })
}

# A function turning a numeric time back into the class of `vals` (a Date or
# POSIXct, keeping its time zone; a plain double otherwise), or NULL when
# `vals` is not a time a transition can run on.
.time_restorer <- function(vals) {
  if (inherits(vals, "Date")) {
    return(function(x) structure(as.double(x), class = "Date"))
  }
  if (inherits(vals, "POSIXct")) {
    tz <- attr(vals, "tzone")
    return(function(x) {
      structure(as.double(x), class = c("POSIXct", "POSIXt"), tzone = tz)
    })
  }
  if (is.numeric(vals) && !is.factor(vals)) {
    return(function(x) as.double(x))
  }
  NULL
}

.enumerate_states <- function(tr, data) {
  if (identical(tr@kind, "time")) {
    vals <- rlang::eval_tidy(tr@var, data)
    # A Date or POSIXct time runs on its numeric value (days / seconds) and
    # keeps its class for the labels and `{frame_time}` (VP-83).
    restore <- .time_restorer(vals)
    if (is.null(restore)) {
      cli::cli_abort(c(
        "{.fn transition_time} needs a numeric, Date or POSIXct time column.",
        x = "It is {.cls {class(vals)[1]}}."
      ))
    }
    vals <- as.double(unclass(vals))
    times <- sort(unique(vals[is.finite(vals)]))
    states <- as.character(times)
    labels <- .frame_chr(restore(times))
    if (anyDuplicated(labels)) {
      labels <- states
    }
    # Segments weighted by the time gaps: constant playback speed through
    # unevenly spaced times. Time flows once, so no hold and no wrap.
    list(
      # Match on the value, not its string: `as.character(100000L)` is
      # "100000" but the level is `as.character(1e5)` = "1e+05", so a string key
      # would drop that keyframe's rows.
      key = factor(states[match(as.double(vals), times)], levels = states),
      states = states,
      seg_weights = if (length(times) > 1L) diff(times) else numeric(0),
      state_length = 0,
      wrap = FALSE,
      times = times,
      restore = restore,
      labels = labels
    )
  } else {
    # A missing state value belongs to no state (it is not animated).
    key <- .facet_key(list(tr@var), data, na_level = FALSE)
    states <- levels(key)
    segs <- if (tr@wrap) length(states) else length(states) - 1L
    list(
      key = key,
      states = states,
      seg_weights = rep(tr@transition_length, max(0L, segs)),
      state_length = tr@state_length,
      wrap = tr@wrap
    )
  }
}

#' Write a keyframe animation to a file
#'
#' `anim_save()` renders a `vellum_animation` (from [animate()]) to an animated
#' image, tweening the frames between keyframes and encoding them in one parallel,
#' streaming pass in vellum's Rust backend. The format follows the file extension:
#'
#' * `.gif` — a looping GIF (universal compatibility).
#' * `.png` or `.apng` — an animated PNG (APNG; lossless, alpha). Browsers
#'   play either; `.png` is the more widely recognised extension, and image
#'   viewers that do not know APNG show its first frame.
#' * `.svg` — a single **animated SVG**: every frame is emitted as vector markup
#'   and shown in turn by a CSS step animation. It is resolution-independent (crisp
#'   at any size, in print and on retina) and honours `prefers-reduced-motion`
#'   (a reader who has asked their system not to animate gets the first frame, held).
#'
#' # Choosing a format
#'
#' Pick by scene complexity, not preference. An animated SVG emits *every frame in
#' full*, so its size grows with the number of marks times the number of frames;
#' a raster format (GIF/APNG) does not. So SVG wins clearly on line art — a few
#' moving marks, the common explanatory case — and loses on a dense scatter, where
#' a raster format is the right answer. `anim_save()` says so when a `.svg` scene
#' is dense enough that a raster format would likely be smaller.
#'
#' Video (`.mp4`, `.webm`, ...) and animated WebP are not written: there is no
#' encoder for them among vellum's dependencies. Save an APNG or GIF and
#' convert it, e.g. `ffmpeg -i anim.png -pix_fmt yuv420p anim.mp4`, or
#' `magick::image_write(magick::image_read("anim.gif"), "anim.webp")`.
#'
#' @param filename Output path; `.gif`, `.png`/`.apng`, or `.svg`.
#' @param animation A `vellum_animation` (from [animate()]).
#' @return `filename`, invisibly.
#' @seealso [animate()]
#' @examples
#' \dontrun{
#' # Four bars in every state, so each bar tweens into its next height.
#' d <- data.frame(t = rep(1:3, each = 4), g = rep(letters[1:4], 3),
#'                 v = c(1, 3, 2, 4, 4, 1, 3, 2, 2, 4, 1, 3))
#' a <- vplot(d) |>
#'   mark_bar(x = g, y = v, fill = g) |>
#'   transition_states(t) |>
#'   animate()
#' anim_save("bars.gif", a)
#' anim_save("bars.svg", a) # resolution-independent
#' }
#' @export
anim_save <- function(filename, animation) {
  if (!S7::S7_inherits(animation, vellum_animation)) {
    cli::cli_abort(
      "{.arg animation} must be a {.cls vellum_animation} from {.fn animate}."
    )
  }
  ext <- tolower(tools::file_ext(filename))
  format <- switch(
    ext,
    gif = "gif",
    png = ,
    apng = "apng",
    svg = "svg",
    .anim_format_error(ext)
  )
  if (identical(format, "svg")) {
    .anim_svg_advice(animation)
  }
  .anim_render(animation, filename, format)
}

#' Save one frame of an animation as a still image
#'
#' `anim_frame()` writes a single frame of a `vellum_animation` to a PNG, SVG
#' or PDF file: the poster for an animation, its thumbnail, or the moment to
#' put in print. The frame is drawn exactly as it appears in the animation --
#' the same frozen scales and colours, the same tween, the same frame
#' variables in the title -- so a poster matches the animation it stands for,
#' which a separately built static plot (its scales trained on one state)
#' need not.
#'
#' Frames are numbered as in the files [anim_save()] writes, pauses and rewind
#' included (see [animate()]).
#'
#' @param filename Output path; `.png`, `.svg` or `.pdf`.
#' @param animation A `vellum_animation` (from [animate()]).
#' @param frame The frame to save: a whole number from 1 to the number of
#'   frames. The default is the frame a reader who asks for reduced motion
#'   sees: the last frame when the animation ends on its final state (a reveal,
#'   [transition_time()], a non-looping [transition_states()]), else the first.
#' @param scale Resolution multiplier (default 1), as in [render_plot()]: `2`
#'   doubles the pixels of a PNG at the same layout.
#' @return `filename`, invisibly.
#' @seealso [anim_save()], [animate()]
#' @examples
#' \dontrun{
#' d <- data.frame(t = rep(1:3, each = 4), g = rep(letters[1:4], 3),
#'                 v = c(1, 3, 2, 4, 4, 1, 3, 2, 2, 4, 1, 3))
#' a <- vplot(d) |>
#'   mark_bar(x = g, y = v, fill = g) |>
#'   transition_states(t, wrap = FALSE) |>
#'   animate()
#' anim_frame("poster.png", a) # the final state
#' anim_frame("middle.svg", a, frame = 25)
#' }
#' @export
anim_frame <- function(filename, animation, frame = NULL, scale = 1) {
  if (!S7::S7_inherits(animation, vellum_animation)) {
    cli::cli_abort(
      "{.arg animation} must be a {.cls vellum_animation} from {.fn animate}."
    )
  }
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("png", "svg", "pdf")) {
    cli::cli_abort(c(
      "{.fn anim_frame} cannot write {.val .{ext}} files.",
      i = "Use {.val .png}, {.val .svg} or {.val .pdf}; {.fn anim_save} writes the whole animation."
    ))
  }
  sched <- .anim_frame_schedule(animation)
  n <- length(sched$seg)
  if (is.null(frame)) {
    k <- length(animation@scenes)
    ends <- !animation@wrap &&
      !isTRUE(animation@rewind) &&
      sched$seg[[n]] + 1L == k &&
      sched$frac[[n]] >= 1
    frame <- if (ends) n else 1L
  }
  if (
    !is.numeric(frame) || length(frame) != 1L || !is.finite(frame) ||
      frame != round(frame) || frame < 1 || frame > n
  ) {
    cli::cli_abort("{.arg frame} must be a whole number in 1:{n}.")
  }
  frame <- as.integer(frame)
  scenes <- animation@scenes
  if (animation@wrap) {
    scenes <- c(scenes, scenes[1L])
  }
  text <- .frame_text(animation, sched)
  variant <- NULL
  if (!is.null(text) && text$frame[[frame]] > 0L) {
    variant <- text$variants[[text$frame[[frame]]]]
  }
  vellum::vl_render_frame(
    scenes,
    sched$seg[[frame]],
    sched$frac[[frame]],
    filename,
    frac_col = sched$frac_col[[frame]],
    frac_size = sched$frac_size[[frame]],
    frac_alpha = sched$frac_alpha[[frame]],
    text_variant = variant,
    scale = scale
  )
}

# Why `anim_save()` cannot write `.<ext>`, with what to do instead (VP-85).
.anim_format_error <- function(ext, call = rlang::caller_env()) {
  video <- c("mp4", "m4v", "mov", "webm", "mkv", "avi", "ogv")
  hint <- if (ext %in% video) {
    c(
      x = "Video is not supported: vellum has no video encoder.",
      i = "Save an APNG and convert it, e.g. {.code ffmpeg -i anim.png -pix_fmt yuv420p anim.{ext}}."
    )
  } else if (identical(ext, "webp")) {
    c(
      x = "Animated WebP is not supported: vellum has no WebP encoder.",
      i = "Save a GIF or APNG and convert it, e.g. {.code magick::image_write(magick::image_read(\"anim.gif\"), \"anim.webp\")}."
    )
  }
  cli::cli_abort(c(
    "{.fn anim_save} cannot write {.val .{ext}} files.",
    hint,
    i = "Use {.val .gif} (any viewer), {.val .png} or {.val .apng} (APNG: lossless, alpha), or {.val .svg} (vector, resolution-independent)."
  ), call = call)
}

# Shared render core: build the eased schedule, wrap the loop, and hand the K
# keyframes to vellum's one-pass animation encoder. Used by anim_save() and by
# the widget's animated-SVG embed (vellumwidget), so both drive identical frames.
.anim_render <- function(animation, filename, format) {
  sched <- .anim_frame_schedule(animation)
  scenes <- animation@scenes
  # Wrap loops the last state back to the first: append a copy of keyframe 1 so
  # the final segment K -> K+1 interpolates back to the start.
  if (animation@wrap) {
    scenes <- c(scenes, scenes[1L])
  }
  text <- .frame_text(animation, sched)
  vellum::vl_render_animation(
    scenes,
    sched$seg,
    sched$frac,
    filename,
    format = format,
    fps = animation@fps,
    frac_col = sched$frac_col,
    frac_size = sched$frac_size,
    frac_alpha = sched$frac_alpha,
    text_variants = text$variants,
    text_frame = text$frame
  )
}

# The four easing names an animation renders with: its per-class overrides where
# set, its single `easing` everywhere else. An animation built before per-class
# easing existed (or by hand) has an empty `easing_aes` and resolves to one curve.
.anim_easings <- function(animation) {
  cls <- animation@easing_aes
  pick <- function(nm) {
    v <- cls[[nm]]
    if (is.null(v) || is.na(v)) animation@easing else v
  }
  list(
    position = pick("position"),
    color = pick("color"),
    size = pick("size"),
    alpha = pick("alpha")
  )
}

# Drawn-element count of a keyframe (marks + guides). The animated-SVG size grows
# with this times the frame count, so it is the signal for the format advice.
.anim_element_count <- function(animation) {
  sc <- animation@scenes[[1L]]
  m <- tryCatch(
    vellum::scene_model(vellum::as_vellum_scene(sc)),
    error = function(e) NULL
  )
  if (is.null(m)) 0L else nrow(m$elements)
}

# Past the point where a raster format is clearly smaller (see the size table in
# vellum::vl_render_animation), advise switching. An advisory, not a block:
# vellum still warns on the finished file if it really is large.
.anim_svg_advice <- function(animation, threshold = 800L) {
  marks <- .anim_element_count(animation)
  if (marks > threshold) {
    cli::cli_inform(c(
      "!" = "This scene has ~{marks} elements and an animated SVG emits every frame in full.",
      "i" = "For a dense scene a raster format is usually smaller -- save to {.val .gif} or {.val .png} instead."
    ))
  }
}

# ---- frame schedule --------------------------------------------------------

# Build the per-frame schedule for K keyframes: for each output frame, the 1-based
# left-keyframe index (`seg`) and the eased interpolation fraction -- one per
# property class (`frac` = position, plus `frac_col`/`frac_size`/`frac_alpha`).
#
# `easing` is either a single easing name (one curve for everything) or the
# four-name list `.ease_classes()` returns.
#
# The animation is a timeline of weighted intervals -- for each state a
# `state_length`-weighted pause on its keyframe, then a move to the next weighted
# by `seg_weights[state]` (equal for `transition_states`; the time gap for
# `transition_time`) -- and exactly `nframes` frames are sampled evenly across it,
# so `animate(nframes = n)` always yields `n` frames. Easing shapes the fraction
# within each transition interval. With `wrap`, the last state moves back to the
# first (keyframe K+1 = a copy of keyframe 1, appended by the caller).
.anim_schedule <- function(
  k,
  nframes,
  easing,
  seg_weights,
  state_length,
  wrap
) {
  segs <- if (wrap) k else k - 1L
  # Each interval: left keyframe `seg`, its fraction endpoints `f0` -> `f1`, and a
  # relative `w`eight. Zero-weight intervals are dropped.
  intervals <- list()
  add <- function(seg, f0, f1, w) {
    if (w > 0) {
      intervals[[length(intervals) + 1L]] <<- list(
        seg = seg,
        f0 = f0,
        f1 = f1,
        w = w
      )
    }
  }
  for (state in seq_len(k)) {
    if (state <= segs) {
      add(state, 0, 0, state_length) # hold on keyframe `state`
      add(state, 0, 1, seg_weights[state]) # move `state` -> `state + 1`
    } else {
      # The last state without wrap has no outgoing move: hold it as the end
      # (fraction 1) of the previous segment.
      add(segs, 1, 1, state_length)
    }
  }
  w <- vapply(intervals, `[[`, numeric(1), "w")
  cw <- cumsum(w)
  starts <- cw - w
  total <- cw[length(cw)]

  # Sample `nframes` positions; frame 1 sits at the very start (keyframe 1). A
  # looping (`wrap`) timeline stops just short of `total` so the final frame does
  # not duplicate the start; a non-looping one lands exactly on `total` so
  # `transition_reveal()` / `transition_time()` (which have no trailing hold)
  # actually reach their final keyframe instead of ending ~one frame short.
  pos <- if (wrap || nframes <= 1L) {
    (seq_len(nframes) - 1L) / nframes * total
  } else {
    (seq_len(nframes) - 1L) / (nframes - 1L) * total
  }
  seg <- integer(nframes)
  raw <- numeric(nframes)
  for (f in seq_len(nframes)) {
    i <- which(pos[f] < cw)[1L]
    if (is.na(i)) {
      i <- length(intervals)
    }
    iv <- intervals[[i]]
    local <- if (iv$w > 0) (pos[f] - starts[i]) / iv$w else 0
    seg[f] <- iv$seg
    raw[f] <- iv$f0 + (iv$f1 - iv$f0) * local
  }
  # One un-eased fraction per frame, then each property class's curve applied to
  # it. Equal curves give four identical vectors -- the single-curve case, which
  # the engine tweens exactly as it did before per-aesthetic easing.
  ease <- if (is.list(easing)) {
    easing
  } else {
    .ease_classes(EaseSpec(default = easing))
  }
  list(
    seg = seg,
    frac = .ease_apply(raw, ease$position),
    frac_col = .ease_apply(raw, ease$color),
    frac_size = .ease_apply(raw, ease$size),
    frac_alpha = .ease_apply(raw, ease$alpha)
  )
}

# ---- easing library --------------------------------------------------------

# Base easings in their "-in" form; `-out` / `-in-out` are derived. Vectorised.
.ease_bases <- list(
  quad = function(t) t^2,
  cubic = function(t) t^3,
  quart = function(t) t^4,
  quint = function(t) t^5,
  sine = function(t) 1 - cos(t * pi / 2),
  expo = function(t) ifelse(t <= 0, 0, 2^(10 * (t - 1))),
  circ = function(t) 1 - sqrt(pmax(0, 1 - t^2)),
  back = function(t) {
    c1 <- 1.70158
    (c1 + 1) * t^3 - c1 * t^2
  },
  elastic = function(t) {
    c4 <- (2 * pi) / 3
    ifelse(
      t <= 0,
      0,
      ifelse(t >= 1, 1, -(2^(10 * t - 10)) * sin((t * 10 - 10.75) * c4))
    )
  },
  bounce = function(t) 1 - .bounce_out(1 - t)
)

.bounce_out <- function(t) {
  n1 <- 7.5625
  d1 <- 2.75
  ifelse(
    t < 1 / d1,
    n1 * t^2,
    ifelse(
      t < 2 / d1,
      n1 * (t - 1.5 / d1)^2 + 0.75,
      ifelse(
        t < 2.5 / d1,
        n1 * (t - 2.25 / d1)^2 + 0.9375,
        n1 * (t - 2.625 / d1)^2 + 0.984375
      )
    )
  )
}

# Parse an easing name into (family, direction).
.parse_ease <- function(name) {
  if (identical(name, "linear")) {
    return(list(family = "linear", dir = NULL))
  }
  m <- regmatches(name, regexec("^([a-z]+)-(in-out|in|out)$", name))[[1L]]
  if (length(m) != 3L || is.null(.ease_bases[[m[2L]]])) {
    return(NULL)
  }
  list(family = m[2L], dir = m[3L])
}

.validate_ease <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.null(.parse_ease(name))) {
    cli::cli_abort(c(
      "Unknown easing {.val {name}}.",
      i = 'Use {.val linear}, or a family ({.val cubic}, {.val sine}, {.val elastic}, ...) with {.val -in}/{.val -out}/{.val -in-out}.'
    ))
  }
  invisible(name)
}

# Apply an easing to a linear fraction vector `t` in [0, 1].
.ease_apply <- function(t, name) {
  p <- .parse_ease(name)
  if (is.null(p) || identical(p$family, "linear")) {
    return(t)
  }
  f <- .ease_bases[[p$family]]
  switch(
    p$dir,
    "in" = f(t),
    "out" = 1 - f(1 - t),
    "in-out" = ifelse(t < 0.5, f(2 * t) / 2, 1 - f(2 - 2 * t) / 2)
  )
}

# ---- methods ---------------------------------------------------------------

# Printing an animation shows the first keyframe as a still preview (in an
# interactive device) and reports how to write it.
S7::method(print, vellum_animation) <- function(x, ...) {
  vellum::display(x@scenes[[1L]])
  total <- .anim_total_frames(x)
  cli::cli_inform(
    "A {.cls vellum_animation}: {length(x@scenes)} state{?s}, {total} frame{?s} at {x@fps} fps. Write it with {.fn anim_save}."
  )
  invisible(x)
}
