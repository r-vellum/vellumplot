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

# --- 2. discrete states: mtcars over cylinder count -------------------------
# transition_states() suits a few discrete groups -- it pauses on each so the eye
# can read it, then eases to the next and loops. ~30 frames per state.
b <- vplot(mtcars) |>
  mark_point(x = wt, y = mpg, size = hp, color = factor(cyl)) |>
  labs(title = "mtcars by cylinder count") |>
  transition_states(cyl) |>
  ease_aes("cubic-in-out") |>
  animate(nframes = 90, fps = 25)

anim_save(file.path(outdir, "28-mtcars.gif"), b)
