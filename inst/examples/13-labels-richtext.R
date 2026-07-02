# Labels and rich text: titles, subtitles, captions, tags, and md() markup.

library(quill)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

# --- the full set of plot-level labels --------------------------------------
# title + subtitle + tag stack above the panel; caption sits below.
vplot(peng) |>
  mark_point(x = bill_len, y = flipper_len, color = species, size = 1.4) |>
  labs(
    title = "Palmer penguins",
    subtitle = "Bill length against flipper length, by species",
    caption = "Data: datasets::penguins",
    tag = "Fig. 1",
    x = "Bill length (mm)",
    y = "Flipper length (mm)",
    color = "Species"
  ) |>
  render_plot(file.path(outdir, "13-labels.png"))

# --- rich text via md() -----------------------------------------------------
# md() builds a markdown label usable in titles and axis/legend names:
# **bold**, *italic*, and ^superscript^ / ~subscript~. Superscripts and
# subscripts are handy for units and chemistry, e.g. m^2 or CO~2~.
vplot(peng) |>
  mark_point(x = bill_len, y = body_mass, color = species) |>
  labs(
    title = md("**Body mass** of Pygoscelis penguins"),
    subtitle = md("Axis titles can be rich too"),
    y = md("Body mass (kg x 10^3^)"),
    x = md("Bill length (*mm*)")
  ) |>
  render_plot(file.path(outdir, "13-richtext.png"))

message("13-labels-richtext: wrote 2 figures to ", outdir)
