# Rotating and wrapping long axis tick labels.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

gdp <- data.frame(
  country = c(
    "United States", "United Kingdom", "United Arab Emirates",
    "Republic of Korea", "Czech Republic"
  ),
  score = c(9, 6, 7, 8, 5)
)

# Rotate the tick labels: the gutter reserves the rotated height automatically.
render_plot(
  vplot(gdp) |>
    mark_bar(x = country, y = score) |>
    theme(axis.text.x = element_text(angle = 45)) |>
    labs(title = "Rotated axis labels"),
  file.path(outdir, "32-rotated.png")
)

# Left horizontal, a label wider than its tick spacing wraps onto more lines and
# the row grows to fit. Labels that already fit are unchanged.
render_plot(
  vplot(gdp) |>
    mark_bar(x = country, y = score) |>
    labs(title = "Wrapped axis labels"),
  file.path(outdir, "32-wrapped.png")
)

message("32-axis-labels: wrote rotated + wrapped bar charts to ", outdir)
