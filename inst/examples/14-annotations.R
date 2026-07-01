# One-off annotations: text, label, point, segment, rect drawn from raw values
# (independent of the plot data, so they repeat on every facet panel).

library(quill)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

# --- every annotation geom in one plot --------------------------------------
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = "grey40") |>
  # shaded region of interest
  annotate("rect", xmin = 3, xmax = 4, ymin = 15, ymax = 22, alpha = 0.15) |>
  # a guide segment
  annotate("segment", x = 2, y = 30, xend = 3, yend = 22, color = "#c0392b") |>
  # an emphasised point
  annotate("point", x = 3, y = 22, size = 4, color = "#c0392b") |>
  # plain text and a boxed label
  annotate("text", x = 2, y = 31, label = "outlier zone", color = "#c0392b") |>
  annotate("label", x = 4.5, y = 30, label = "heavy + thirsty", fill = "#fff3cd") |>
  labs(title = "annotate(): rect, segment, point, text, label") |>
  render_plot(file.path(outdir, "14-annotations.png"))

# --- annotations repeat across facets ---------------------------------------
# Because the annotation carries its own one-row data, it is drawn in every
# panel -- handy for a shared reference line or marker.
vplot(mtcars, width = 8, height = 3) |>
  mark_point(x = wt, y = mpg) |>
  annotate("segment", x = 1, y = 20, xend = 5.5, yend = 20, color = "#2980b9") |>
  annotate("text", x = 4.5, y = 22, label = "20 mpg", color = "#2980b9") |>
  facet_wrap(~cyl, ncol = 3) |>
  labs(title = "Annotations repeat on every panel") |>
  render_plot(file.path(outdir, "14-annotations-facets.png"))

message("14-annotations: wrote 2 figures to ", outdir)
