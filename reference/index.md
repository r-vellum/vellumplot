# Package index

## Building a plot

Start a plot from a data frame, an `sf` object, or an `igraph` graph,
then render it or write it to a file.

- [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)
  : Start a plot specification
- [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  : Start a graph (network) plot
- [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  : Render a plot to a file
- [`PlotSpec()`](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  : The plot specification

## Marks

Layers that draw data. Scales train across every mark on the panel.

### Points, lines, and bars

- [`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  [`mark_line()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  [`mark_rule()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  [`mark_bar()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
  : Add marks to a plot
- [`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  : Area, ribbon, and step marks
- [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md)
  : Segment mark

### Distributions and intervals

- [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  [`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  [`mark_summary()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  : Boxplot, error bar, and summary marks
- [`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  [`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  [`mark_dotplot()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
  : Density-shape marks
- [`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  [`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
  : Statistical marks
- [`mark_ecdf()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  [`mark_rug()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  [`mark_qq()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  [`mark_qq_line()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
  : Distribution marks
- [`mark_ellipse()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
  [`mark_hull()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
  : Group summary regions
- [`mark_halfeye()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  [`mark_interval()`](https://r-vellum.github.io/vellumplot/reference/mark_halfeye.md)
  : Uncertainty marks (slab + interval)
- [`mark_tile()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  [`mark_raster()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  [`mark_bin2d()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  [`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  [`mark_hex()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
  : Heatmap marks
- [`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  [`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
  : 2-D density contours

### Text, pie, and large data

- [`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  [`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
  : Text marks
- [`mark_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
  [`mark_donut()`](https://r-vellum.github.io/vellumplot/reference/mark_pie.md)
  : Pie and donut charts
- [`mark_image()`](https://r-vellum.github.io/vellumplot/reference/mark_image.md)
  : Draw images at data points
- [`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md)
  : Datashade a large point cloud

### Spatial and network

- [`mark_sf()`](https://r-vellum.github.io/vellumplot/reference/mark_sf.md)
  : Draw simple-feature (sf) geometries
- [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  [`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  [`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  : Network (graph) marks
- [`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)
  : Edge-width scale

## Scales

Map data values to visual properties, with trained domains and guides.

- [`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  [`scale_y_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  [`scale_x_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  [`scale_y_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
  : Position scales
- [`scale_x_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_x_binned.md)
  [`scale_y_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_x_binned.md)
  : Binned position scales
- [`scale_x_date()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  [`scale_y_date()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  [`scale_x_datetime()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  [`scale_y_datetime()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  [`scale_x_time()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  [`scale_y_time()`](https://r-vellum.github.io/vellumplot/reference/scale_x_date.md)
  : Date and time position scales
- [`scale_color_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_color_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_color_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_color_gradient()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_color_gradient2()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_fill_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_fill_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_fill_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_fill_gradient()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_fill_gradient2()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_colour_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_colour_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_colour_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_colour_gradient()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_colour_gradient2()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  : Colour scales
- [`scale_fill_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  [`scale_color_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  : Binned (classed) colour scales
- [`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md)
  : Shape scale
- [`scale_size()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  [`scale_size_area()`](https://r-vellum.github.io/vellumplot/reference/scale_size.md)
  : Size scale
- [`scale_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md)
  [`scale_alpha_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_alpha.md)
  : Alpha (opacity) scale
- [`scale_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_linetype.md)
  [`scale_linetype_discrete()`](https://r-vellum.github.io/vellumplot/reference/scale_linetype.md)
  : Line-type scale
- [`scale_color_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  [`scale_fill_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  [`scale_colour_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  [`scale_size_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  [`scale_shape_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  [`scale_alpha_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  [`scale_linetype_identity()`](https://r-vellum.github.io/vellumplot/reference/scale_color_identity.md)
  : Identity scales
- [`lims()`](https://r-vellum.github.io/vellumplot/reference/lims.md)
  [`xlim()`](https://r-vellum.github.io/vellumplot/reference/lims.md)
  [`ylim()`](https://r-vellum.github.io/vellumplot/reference/lims.md) :
  Set scale limits with a shortcut
- [`sec_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md)
  [`dup_axis()`](https://r-vellum.github.io/vellumplot/reference/sec_axis.md)
  : Secondary axes
- [`guides()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  [`guide_none()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  [`guide_legend()`](https://r-vellum.github.io/vellumplot/reference/guides.md)
  : Control a scale's legend
- [`resolve_scale()`](https://r-vellum.github.io/vellumplot/reference/resolve_scale.md)
  : Resolve scales as shared or independent across panels
- [`after_stat()`](https://r-vellum.github.io/vellumplot/reference/after_stat.md)
  : Map an aesthetic to a statistic computed by a stat

## Coordinates and facets

- [`coord_cartesian()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  [`coord_fixed()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  [`coord_equal()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  : Coordinate systems
- [`coord_trans()`](https://r-vellum.github.io/vellumplot/reference/coord_trans.md)
  : Transformed coordinate system
- [`coord_polar()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  [`coord_radial()`](https://r-vellum.github.io/vellumplot/reference/coord_polar.md)
  : Polar coordinates
- [`coord_sf()`](https://r-vellum.github.io/vellumplot/reference/coord_sf.md)
  : Map coordinate system
- [`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  [`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  : Facet a plot into a grid of panels
- [`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)
  : Add marginal distributions to a plot
- [`position_nudge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  [`position_jitter()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  [`position_dodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  [`position_dodge2()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  [`position_jitterdodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  : Position adjustments

## Composition

Combine plots into a single figure, or repeat one over a variable.

- [`concat()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
  [`wrap_plots()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
  [`hconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
  [`vconcat()`](https://r-vellum.github.io/vellumplot/reference/concat.md)
  : Arrange plots side by side

- [`inset()`](https://r-vellum.github.io/vellumplot/reference/inset.md)
  : Overlay a plot as an inset

- [`plot_spacer()`](https://r-vellum.github.io/vellumplot/reference/plot_spacer.md)
  : Reserve an empty cell in a composition

- [`area()`](https://r-vellum.github.io/vellumplot/reference/area.md) :

  Define a layout area for `design =`

- [`repeat_()`](https://r-vellum.github.io/vellumplot/reference/repeat_.md)
  : Repeat a view across fields

- [`compose_annotation()`](https://r-vellum.github.io/vellumplot/reference/compose_annotation.md)
  : Annotate a composition

## Themes and annotation

- [`theme_gray()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  [`theme_minimal()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  [`theme_bw()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  [`theme_classic()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  [`theme_void()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  [`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  [`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  [`set_theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  : Plot themes
- [`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)
  : Hand-drawn plot theme
- [`element_text()`](https://r-vellum.github.io/vellumplot/reference/element.md)
  [`element_line()`](https://r-vellum.github.io/vellumplot/reference/element.md)
  [`element_rect()`](https://r-vellum.github.io/vellumplot/reference/element.md)
  [`element_blank()`](https://r-vellum.github.io/vellumplot/reference/element.md)
  : Theme elements
- [`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md) :
  Set plot titles and axis/legend labels
- [`annotate()`](https://r-vellum.github.io/vellumplot/reference/annotate.md)
  : Add a one-off annotation
- [`md()`](https://r-vellum.github.io/vellumplot/reference/md.md) :
  Rich-text labels

## Layer effects

Glow, shadow, outline, motion trails, hand-drawn sketching, and gradient
fills.

- [`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md) :
  Neon glow layer effect
- [`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)
  : Shadow layer effect
- [`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md)
  : Outline (halo) layer effect
- [`motion()`](https://r-vellum.github.io/vellumplot/reference/motion.md)
  [`echo()`](https://r-vellum.github.io/vellumplot/reference/motion.md)
  : Motion-trail layer effects
- [`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
  : Hand-drawn ("sketch") rendering
- [`linear_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
  [`radial_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
  : Gradient fill paints

## Accessibility and provenance

Attach a text alternative for screen readers, or inspect how a spec
compiled into a scene.

- [`plot_alt()`](https://r-vellum.github.io/vellumplot/reference/plot_alt.md)
  : Text alternative (alt text) for a plot
- [`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md)
  : Inspect the compiled-scene provenance of a plot

## Package

- [`vellumplot`](https://r-vellum.github.io/vellumplot/reference/vellumplot-package.md)
  [`vellumplot-package`](https://r-vellum.github.io/vellumplot/reference/vellumplot-package.md)
  : vellumplot: A Grammar of Graphics on the 'vellum' Backend
