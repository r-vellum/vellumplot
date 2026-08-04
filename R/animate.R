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
#' @param plot A [PlotSpec] (from [vplot()]).
#' @param time A numeric column giving each row's time.
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
      "{.fn transition_time} needs a numeric time column, e.g. {.code transition_time(year)}."
    )
  }
  plot@transition <- TransitionSpec(var = var, kind = "time", wrap = FALSE)
  plot
}

#' Reveal a plot progressively along a variable
#'
#' `transition_reveal()` wipes the plot into view left to right — the classic
#' "line draws itself" animation. Unlike [transition_states()], it does not tween
#' between states: the full plot is compiled and a clip rectangle grows across the
#' panel, revealing the marks in `along` order. Best for a line or path over a
#' continuous `along` (typically the x variable, e.g. time).
#'
#' @param plot A [PlotSpec] (from [vplot()]).
#' @param along The variable the reveal follows (mapped to x, increasing left to
#'   right).
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
#' gently. Applies to every interpolated aesthetic.
#'
#' @param plot A [PlotSpec] carrying a [transition_states()].
#' @param ease An easing name: `"linear"`, or a family
#'   (`quad`/`cubic`/`quart`/`quint`/`sine`/`expo`/`circ`/`back`/`elastic`/`bounce`)
#'   with a direction suffix (`-in`, `-out`, `-in-out`), e.g. `"cubic-in-out"`.
#' @return The modified [PlotSpec].
#' @seealso [transition_states()], [animate()]
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   transition_states(cyl) |>
#'   ease_aes("cubic-in-out")
#' @export
ease_aes <- function(plot, ease = "linear") {
  .check_plot(plot)
  ease <- ease[[1L]]
  .validate_ease(ease)
  plot@ease <- EaseSpec(default = ease)
  plot
}

.pos_num <- function(x, arg) {
  x <- x[[1L]]
  if (!is.numeric(x) || !is.finite(x) || x <= 0) {
    cli::cli_abort("{.arg {arg}} must be a positive number.")
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
#' @param plot A [PlotSpec] with a [transition_states()] (and optionally an
#'   [ease_aes()]).
#' @param nframes Total number of frames to render.
#' @param fps Frames per second (the playback rate written into the file).
#' @return A `vellum_animation`.
#' @seealso [transition_states()], [ease_aes()], [anim_save()]
#' @examples
#' \dontrun{
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   transition_states(cyl) |>
#'   animate(nframes = 50, fps = 25)
#' }
#' @export
animate <- function(plot, nframes = 50, fps = 25) {
  .check_plot(plot)
  tr <- plot@transition
  if (is.null(tr)) {
    cli::cli_abort(
      "{.fn animate} needs a transition; add {.fn transition_states} or {.fn transition_time} first."
    )
  }
  if (!is.null(plot@facet)) {
    cli::cli_abort("{.fn animate} does not support facets.")
  }
  nframes <- .pos_num(nframes, "nframes")
  fps <- .pos_num(fps, "fps")
  easing <- if (is.null(plot@ease)) "linear" else plot@ease@default

  # A reveal is not a state tween: compile the plot once (two keyframes that
  # differ only in a clip rectangle) and let the tween grow the clip.
  if (identical(tr@kind, "reveal")) {
    return(.animate_reveal(plot, nframes, fps, easing))
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

  # A per-state spec: the base plot (transition stripped) over that state's rows.
  base <- plot
  base@transition <- NULL
  base@ease <- NULL
  state_spec <- function(level) {
    s <- base
    # `which()` (not a bare logical mask): `key` is a factor and rows with a
    # non-finite state value carry `key = NA`, so `key == level` would otherwise
    # inject an all-NA row into every keyframe.
    s@data <- plot@data[which(key == level), , drop = FALSE]
    s
  }

  # Freeze the scales: resolve every state's layers (per-state stats included),
  # pool them, and train ONCE over the union so every keyframe shares one frozen
  # set of scales. Training over the pooled resolved values gives the union domain
  # across states — the guardrail that keeps the animation non-reactive.
  pooled <- unlist(
    lapply(states, function(level) {
      bp <- .build_panels(state_spec(level))
      unlist(lapply(bp$panels, function(p) p$resolved), recursive = FALSE)
    }),
    recursive = FALSE
  )
  frozen <- .train_scales(base, pooled)

  scenes <- lapply(states, function(level) {
    .compile_plot(state_spec(level), frozen_scales = frozen)
  })

  vellum_animation(
    scenes = scenes,
    states = states,
    nframes = nframes,
    fps = fps,
    seg_weights = en$seg_weights,
    state_length = en$state_length,
    wrap = en$wrap,
    easing = easing,
    width = plot@width,
    height = plot@height,
    dpi = plot@dpi
  )
}

# A reveal animation: compile the plot with a reveal clip at 0% and at 100% —
# same data, same frozen scales, differing only in the clip rectangle — and let
# the tween grow the clip across the panel. The axes/labels sit outside the panel
# viewport, so only the marks wipe in.
.animate_reveal <- function(plot, nframes, fps, easing) {
  base <- plot
  base@transition <- NULL
  base@ease <- NULL
  reveal_at <- function(f) {
    s <- base
    s@clip <- ClipSpec(kind = "reveal", type = "alpha", reveal_frac = f)
    .compile_plot(s)
  }
  vellum_animation(
    scenes = list(reveal_at(0), reveal_at(1)),
    states = c("0", "1"),
    nframes = nframes,
    fps = fps,
    seg_weights = 1,
    state_length = 0,
    wrap = FALSE,
    easing = easing,
    width = plot@width,
    height = plot@height,
    dpi = plot@dpi
  )
}

# Enumerate a transition's states and the per-segment timing. Returns `key` (a
# factor assigning each data row to a state), `states` (the ordered labels),
# `seg_weights` (a relative duration per inter-keyframe segment), `state_length`
# (the held-pause weight per state), and `wrap`.
.enumerate_states <- function(tr, data) {
  if (identical(tr@kind, "time")) {
    vals <- rlang::eval_tidy(tr@var, data)
    if (!is.numeric(vals)) {
      cli::cli_abort("{.fn transition_time} needs a numeric time column.")
    }
    times <- as.double(sort(unique(vals[is.finite(vals)])))
    states <- as.character(times)
    # Segments weighted by the time gaps: constant playback speed through
    # unevenly spaced times. Time flows once, so no hold and no wrap.
    list(
      key = factor(as.character(vals), levels = states),
      states = states,
      seg_weights = if (length(times) > 1L) diff(times) else numeric(0),
      state_length = 0,
      wrap = FALSE
    )
  } else {
    key <- .facet_key(list(tr@var), data)
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
#' * `.png` — an animated PNG (APNG; lossless, alpha).
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
#' @param filename Output path; `.gif`, `.png`, or `.svg`.
#' @param animation A `vellum_animation` (from [animate()]).
#' @return `filename`, invisibly.
#' @seealso [animate()]
#' @examples
#' \dontrun{
#' a <- vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   transition_states(cyl) |>
#'   animate()
#' anim_save("cyl.gif", a)
#' anim_save("cyl.svg", a) # resolution-independent
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
    png = "apng",
    svg = "svg",
    cli::cli_abort(
      "{.arg filename} must end in {.val .gif}, {.val .png}, or {.val .svg} (got {.val .{ext}})."
    )
  )
  if (identical(format, "svg")) {
    .anim_svg_advice(animation)
  }
  .anim_render(animation, filename, format)
}

# Shared render core: build the eased schedule, wrap the loop, and hand the K
# keyframes to vellum's one-pass animation encoder. Used by anim_save() and by
# the widget's animated-SVG embed (vellumwidget), so both drive identical frames.
.anim_render <- function(animation, filename, format) {
  k <- length(animation@scenes)
  sched <- .anim_schedule(
    k,
    animation@nframes,
    animation@easing,
    animation@seg_weights,
    animation@state_length,
    animation@wrap
  )
  scenes <- animation@scenes
  # Wrap loops the last state back to the first: append a copy of keyframe 1 so
  # the final segment K -> K+1 interpolates back to the start.
  if (animation@wrap) {
    scenes <- c(scenes, scenes[1L])
  }
  vellum::vl_render_animation(
    scenes,
    sched$seg,
    sched$frac,
    filename,
    format = format,
    fps = animation@fps
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
# left-keyframe index (`seg`) and the eased interpolation fraction (`frac`).
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
  frac <- numeric(nframes)
  for (f in seq_len(nframes)) {
    i <- which(pos[f] < cw)[1L]
    if (is.na(i)) {
      i <- length(intervals)
    }
    iv <- intervals[[i]]
    local <- if (iv$w > 0) (pos[f] - starts[i]) / iv$w else 0
    seg[f] <- iv$seg
    frac[f] <- .ease_apply(iv$f0 + (iv$f1 - iv$f0) * local, easing)
  }
  list(seg = seg, frac = frac)
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
  cli::cli_inform(
    "A {.cls vellum_animation}: {length(x@scenes)} state{?s}, {x@nframes} frame{?s} at {x@fps} fps. Write it with {.fn anim_save}."
  )
  invisible(x)
}
