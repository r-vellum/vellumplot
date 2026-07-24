# Pattern (hatch) fills: distinguish filled regions by texture, not only hue --
# the greyscale-print / colour-vision-safe alternative to a fill palette. A
# pattern is an unscaled `fill` *value* (like a gradient), built by pattern_*().

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

set.seed(1)

# --- 1. a hatched bar chart -------------------------------------------------
# One texture per bar via a constant pattern fill (crosshatch here).
bars <- data.frame(method = c("A", "B", "C", "D"), score = c(4, 7, 5, 6))
vplot(bars) |>
  mark_bar(
    x = method,
    y = score,
    fill = pattern_crosshatch(color = "grey20")
  ) |>
  labs(title = "Pattern-filled bars", x = NULL, y = "score") |>
  render_plot(file.path(outdir, "25-bars.png"))

# --- 2. the pattern family --------------------------------------------------
# stripe / crosshatch / grid / dot / checker, each a tile repeated across the
# region. Stripe angle is restricted to 0 / 45 / 90 / 135 degrees. A pattern is
# recognised as a fill *value* when written inline (or held in a bare variable),
# so give each bar its own layer with an inline pattern.
fam <- function(k) {
  data.frame(
    kind = factor(
      k,
      levels = c("stripe", "crosshatch", "grid", "dot", "checker")
    ),
    y = 1
  )
}
vplot(fam("stripe")) |>
  mark_bar(
    x = kind,
    y = y,
    fill = pattern_stripe(color = "#1f6feb", angle = 45)
  ) |>
  mark_bar(
    data = fam("crosshatch"),
    x = kind,
    y = y,
    fill = pattern_crosshatch(color = "#238636")
  ) |>
  mark_bar(
    data = fam("grid"),
    x = kind,
    y = y,
    fill = pattern_grid(color = "grey30")
  ) |>
  mark_bar(
    data = fam("dot"),
    x = kind,
    y = y,
    fill = pattern_dot(color = "#a371f7", spacing = 3)
  ) |>
  mark_bar(
    data = fam("checker"),
    x = kind,
    y = y,
    fill = pattern_checker(color = "grey50", size = 3)
  ) |>
  labs(title = "pattern_*() builders", x = NULL, y = NULL) |>
  render_plot(file.path(outdir, "25-family.png"))

# --- 3. patterns on a distribution plot -------------------------------------
# Textured violins read in greyscale where a colour fill would not.
df <- data.frame(
  g = rep(c("control", "treated"), each = 120),
  y = c(rnorm(120, 0), rnorm(120, 1.2))
)
vplot(df) |>
  mark_violin(
    x = g,
    y = y,
    fill = pattern_stripe(angle = 45, color = "grey25")
  ) |>
  labs(title = "Hatched violins", x = NULL) |>
  render_plot(file.path(outdir, "25-violins.png"))

# --- 4. mapping the pattern aesthetic ---------------------------------------
# Map a variable to textures: each level gets a distinct pattern, with a
# patterned legend. scale_pattern(values=) overrides the palette.
grouped <- data.frame(method = c("A", "B", "C", "D"), score = c(4, 7, 5, 6))
vplot(grouped) |>
  mark_bar(x = method, y = score, pattern = method) |>
  labs(title = "Pattern mapped to a variable", x = NULL, y = "score") |>
  render_plot(file.path(outdir, "25-mapped.png"))
