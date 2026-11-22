# Keyframe animation: animate() compiles one keyframe per state, trains the scales
# once over every state and freezes them, and anim_save() tweens and encodes the
# frames (GIF/APNG) in vellum's parallel Rust backend. Nothing retrains between
# frames -- the states are fixed at author time.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

# --- 1. the gapminder bubble chart, flowing over year -----------------------
# transition_time() (not transition_states()) is the right idiom for a continuous
# time: no pause on each year and no wrap-around, so the bubbles glide at a steady
# rate. A generous nframes keeps that glide smooth -- here ~22 frames per 5-year
# step (11 steps, 1952..2007) at 25 fps, about 10 seconds.
if (requireNamespace("gapminder", quietly = TRUE)) {
  a <- vplot(gapminder::gapminder) |>
    mark_point(x = gdpPercap, y = lifeExp, size = pop, color = continent) |>
    scale_x_continuous(trans = "log10") |>
    labs(title = "Gapminder", x = "GDP per capita", y = "Life expectancy") |>
    transition_time(year) |>
    animate(nframes = 250, fps = 25)

  anim_save(file.path(outdir, "28-gapminder.gif"), a)
}

# --- 2. discrete states: telephones by region, 1951-1961 --------------------
# transition_states() suits a few discrete states -- it pauses on each so the eye
# can read it, then eases to the next and loops. The marks tween only when every
# state draws the same marks: here each of the 7 regions has one bar in every
# year, so the bars grow rather than cut. (A state with a different set of
# groups or points cuts to the next keyframe halfway through, and animate()
# warns about it.) 7 states over 140 frames is 20 frames per state: about half a
# second of pause and half a second of motion at 25 fps.
phones <- data.frame(
  year = rep(as.integer(rownames(WorldPhones)), times = ncol(WorldPhones)),
  region = rep(colnames(WorldPhones), each = nrow(WorldPhones)),
  phones = as.vector(WorldPhones) / 1000
)
b <- vplot(phones) |>
  mark_bar(x = region, y = phones, fill = region) |>
  labs(title = "Telephones by region, 1951-1961", x = NULL, y = "Millions") |>
  transition_states(year) |>
  ease_aes("cubic-in-out") |>
  animate(nframes = 140, fps = 25)

anim_save(file.path(outdir, "28-phones.gif"), b)

# --- 3. animated SVG: resolution-independent, for line art / few marks -------
# A handful of moving marks is exactly where an animated SVG wins: crisp at any
# size and honours prefers-reduced-motion. (Dense scatters are better as a
# raster GIF -- anim_save() says so.) Twelve phases of one wave, each line with
# the same 60 vertices so it tweens; the twelfth state wraps back to the first
# (a full period), with no pause and linear easing so the wave travels at a
# constant speed. 10 frames per step, so every frame lands on a new position.
sine <- do.call(
  rbind,
  lapply(0:11, function(k) {
    x <- seq(0, 2 * pi, length.out = 60)
    data.frame(x = x, y = sin(x + k * pi / 6), step = k)
  })
)
wave <- vplot(sine) |>
  mark_line(x = x, y = y, linewidth = 1.2, color = "#2563eb") |>
  labs(title = "A travelling wave") |>
  transition_states(step, state_length = 0) |>
  animate(nframes = 120, fps = 30)

anim_save(file.path(outdir, "28-wave.svg"), wave)
