# Themes: built-in looks, legend positioning, and custom element overrides.

library(quill)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

peng <- na.omit(datasets::penguins)

base <- function() {
  vplot(peng) |>
    mark_point(x = bill_len, y = flipper_len, color = species, size = 1.4)
}

# --- built-in themes --------------------------------------------------------
render_plot(base() |> theme_gray()    |> labs(title = "theme_gray (default)"),
            file.path(outdir, "12-gray.png"))
render_plot(base() |> theme_bw()      |> labs(title = "theme_bw"),
            file.path(outdir, "12-bw.png"))
render_plot(base() |> theme_minimal() |> labs(title = "theme_minimal"),
            file.path(outdir, "12-minimal.png"))
render_plot(base() |> theme_classic() |> labs(title = "theme_classic"),
            file.path(outdir, "12-classic.png"))
render_plot(base() |> theme_void()    |> labs(title = "theme_void"),
            file.path(outdir, "12-void.png"))

# --- legend positions -------------------------------------------------------
# legend.position is one of "right" (default), "left", "top", "bottom", "none".
for (pos in c("left", "top", "bottom", "none")) {
  base() |>
    theme(legend.position = pos) |>
    labs(title = paste0("legend.position = \"", pos, "\"")) |>
    render_plot(file.path(outdir, paste0("12-legend-", pos, ".png")))
}

# --- set_theme shortcut -----------------------------------------------------
# The common colour knobs in one call.
base() |>
  set_theme(panel_bg = "#fdf6e3", grid_col = "#eee8d5", label_col = "#586e75") |>
  labs(title = "set_theme() colour shortcut") |>
  render_plot(file.path(outdir, "12-set-theme.png"))

# --- custom element overrides -----------------------------------------------
# theme() takes element_text / element_line / element_rect / element_blank.
base() |>
  theme_minimal() |>
  theme(
    plot.title = element_text(size = 16, face = "bold", colour = "#22313f"),
    axis.title = element_text(size = 12, colour = "#556270"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f7fbff"),
    legend.position = "bottom"
  ) |>
  labs(title = "Custom theme elements") |>
  render_plot(file.path(outdir, "12-custom-elements.png"))

message("12-themes: wrote 12 figures to ", outdir)
