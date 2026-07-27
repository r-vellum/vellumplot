# Keyframe animation: transition_states() compiles one keyframe per level of a
# column, animate() trains the scales once over every state and freezes them, and
# anim_save() tweens and encodes the frames (GIF/APNG) in vellum's parallel Rust
# backend. Nothing retrains between frames -- the states are fixed at author time.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

# --- 1. the gapminder bubble chart, animated over year ----------------------
if (requireNamespace("gapminder", quietly = TRUE)) {
  a <- vplot(gapminder::gapminder) |>
    mark_point(x = gdpPercap, y = lifeExp, size = pop, color = continent) |>
    scale_x_continuous(transform = "log10") |>
    labs(title = "Gapminder", x = "GDP per capita", y = "Life expectancy") |>
    transition_states(year, transition_length = 2, state_length = 1) |>
    ease_aes("cubic-in-out") |>
    animate(nframes = 100, fps = 25)

  anim_save(file.path(outdir, "28-gapminder.gif"), a)
}

# --- 2. a minimal, dependency-free example (mtcars over cylinders) -----------
b <- vplot(mtcars) |>
  mark_point(x = wt, y = mpg, size = hp, color = factor(cyl)) |>
  labs(title = "mtcars by cylinder count") |>
  transition_states(cyl) |>
  ease_aes("cubic-in-out") |>
  animate(nframes = 60, fps = 20)

anim_save(file.path(outdir, "28-mtcars.gif"), b)
