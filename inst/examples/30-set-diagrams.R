# Venn / Euler set diagrams: vvenn().
# Overlapping circles whose disjoint regions are drawn as SOLID geometry (via
# vellum's boolean vl_path_op()), filled by how many elements fall in exactly
# that combination of sets -- crisp in PDF, no alpha-composited overlaps.

library(vellumplot)
outdir <- "figures"
dir.create(outdir, showWarnings = FALSE)

# --- two sets, from named member lists --------------------------------------
vvenn(list(
  Coffee = c("Ann", "Bo", "Cy", "Di", "Ed"),
  Tea = c("Bo", "Di", "Ed", "Fi", "Gy", "Hu")
)) |>
  render_plot(file.path(outdir, "30-venn2.png"))

# --- three sets -------------------------------------------------------------
set.seed(1)
u <- paste0("g", 1:60)
vvenn(list(
  A = sample(u, 34),
  B = sample(u, 30),
  C = sample(u, 26)
)) |>
  render_plot(file.path(outdir, "30-venn3.png"))

# --- from a data frame of logical membership columns ------------------------
# Each column is a set; TRUE means that row (element) is a member.
survey <- data.frame(
  reads_r = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE),
  reads_py = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
  reads_jl = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
)
vvenn(survey) |>
  render_plot(file.path(outdir, "30-venn-df.png"))

message("30-set-diagrams: wrote 3 figures to ", outdir)
