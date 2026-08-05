# Package index

## Building a plot

Start a plot from a data frame, an `sf` object, or an `igraph` graph,
then render it or write it to a file.

- [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)
  : Start a plot specification

- [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)
  : Start a graph (network) plot

- [`vdendrogram()`](https://r-vellum.github.io/vellumplot/reference/vdendrogram.md)
  : Dendrogram from a clustering

- [`vsankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  [`mark_sankey()`](https://r-vellum.github.io/vellumplot/reference/vsankey.md)
  : Sankey (flow) diagram

- [`vchord()`](https://r-vellum.github.io/vellumplot/reference/vchord.md)
  [`mark_chord()`](https://r-vellum.github.io/vellumplot/reference/vchord.md)
  : Chord diagram

- [`vhierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md)
  [`mark_hierarchy()`](https://r-vellum.github.io/vellumplot/reference/vhierarchy.md)
  : Hierarchy diagrams: sunburst, icicle, treemap, circle-pack

- [`vvenn()`](https://r-vellum.github.io/vellumplot/reference/vvenn.md)
  : Venn / Euler diagrams

- [`vwaffle()`](https://r-vellum.github.io/vellumplot/reference/vwaffle.md)
  : Waffle chart

- [`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md)
  : Sparklines — tiny word-sized charts

- [`vtable()`](https://r-vellum.github.io/vellumplot/reference/vtable.md)
  : Tables with sparkline columns

- [`gt_vsparkline()`](https://r-vellum.github.io/vellumplot/reference/gt_vsparkline.md)
  :

  Add a vellumplot sparkline column to a `gt` table

- [`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
  : Render a plot to a file

- [`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md)
  : Render a plot to a self-contained SVG string

- [`plot_data_uri()`](https://r-vellum.github.io/vellumplot/reference/plot_data_uri.md)
  : Encode a plot as a data URI

- [`pdf_pages()`](https://r-vellum.github.io/vellumplot/reference/pdf_pages.md)
  : Write a multi-page PDF

- [`render_all()`](https://r-vellum.github.io/vellumplot/reference/render_all.md)
  : Render many plots to separate files, in parallel

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
- [`mark_count()`](https://r-vellum.github.io/vellumplot/reference/mark_count.md)
  : Count overlapping points
- [`mark_area()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  [`mark_ribbon()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  [`mark_step()`](https://r-vellum.github.io/vellumplot/reference/mark_area.md)
  : Area, ribbon, and step marks
- [`mark_segment()`](https://r-vellum.github.io/vellumplot/reference/mark_segment.md)
  : Segment mark
- [`mark_abline()`](https://r-vellum.github.io/vellumplot/reference/mark_abline.md)
  [`mark_function()`](https://r-vellum.github.io/vellumplot/reference/mark_abline.md)
  : Reference lines and function curves

### Distributions and intervals

- [`mark_boxplot()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  [`mark_errorbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  [`mark_linerange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  [`mark_summary()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  [`mark_pointrange()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  [`mark_crossbar()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
  : Boxplot, error bar, and summary marks
- [`mark_signif()`](https://r-vellum.github.io/vellumplot/reference/mark_signif.md)
  : Significance brackets
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
- [`mark_raincloud()`](https://r-vellum.github.io/vellumplot/reference/mark_raincloud.md)
  : Raincloud plot
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
- [`mark_series_label()`](https://r-vellum.github.io/vellumplot/reference/mark_series_label.md)
  : Direct series labels
- [`mark_outlier_label()`](https://r-vellum.github.io/vellumplot/reference/mark_outlier_label.md)
  : Label the outliers
- [`mark_text_path()`](https://r-vellum.github.io/vellumplot/reference/mark_text_path.md)
  : Text set along a path
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
- [`mark_scalebar()`](https://r-vellum.github.io/vellumplot/reference/map_decorations.md)
  [`mark_compass()`](https://r-vellum.github.io/vellumplot/reference/map_decorations.md)
  : Map decorations
- [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  [`mark_edge_bundle()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  [`mark_flow_map()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  [`mark_nodes()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  [`mark_node_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  [`mark_edge_text()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  [`mark_node_hull()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  [`mark_node_pie()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md)
  : Network (graph) marks
- [`scale_edge_width()`](https://r-vellum.github.io/vellumplot/reference/scale_edge_width.md)
  : Edge-width scale
- [`scale_edge_color()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md)
  [`scale_edge_colour()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md)
  [`scale_edge_alpha()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md)
  [`scale_edge_linetype()`](https://r-vellum.github.io/vellumplot/reference/scale_edge.md)
  : Edge colour / alpha / line-type scales

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
  [`scale_color_gradientn()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_fill_gradientn()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
  [`scale_colour_gradientn()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md)
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
- [`scale_color_viridis_c()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_color_viridis_d()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_fill_viridis_c()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_fill_viridis_d()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_color_brewer()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_fill_brewer()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_color_distiller()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_fill_distiller()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_colour_viridis_c()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_colour_viridis_d()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_colour_brewer()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  [`scale_colour_distiller()`](https://r-vellum.github.io/vellumplot/reference/scale_viridis.md)
  : Viridis and ColorBrewer scales
- [`scale_fill_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  [`scale_color_binned()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  [`scale_color_steps()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  [`scale_colour_steps()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  [`scale_fill_steps()`](https://r-vellum.github.io/vellumplot/reference/scale_fill_binned.md)
  : Binned (classed) colour scales
- [`scale_shape()`](https://r-vellum.github.io/vellumplot/reference/scale_shape.md)
  : Shape scale
- [`scale_pattern()`](https://r-vellum.github.io/vellumplot/reference/scale_pattern.md)
  : Pattern (texture) scale
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
  [`position_sina()`](https://r-vellum.github.io/vellumplot/reference/position.md)
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
  [`theme_light()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  [`theme_dark()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  [`theme_linedraw()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
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
/ pattern fills.

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
- [`pattern_stripe()`](https://r-vellum.github.io/vellumplot/reference/patterns.md)
  [`pattern_crosshatch()`](https://r-vellum.github.io/vellumplot/reference/patterns.md)
  [`pattern_grid()`](https://r-vellum.github.io/vellumplot/reference/patterns.md)
  [`pattern_dot()`](https://r-vellum.github.io/vellumplot/reference/patterns.md)
  [`pattern_checker()`](https://r-vellum.github.io/vellumplot/reference/patterns.md)
  : Pattern (hatch) fills
- [`pattern_hatch()`](https://r-vellum.github.io/vellumplot/reference/pattern_hatch.md)
  : Crisp vector hatch fill
- [`vl_pattern()`](https://r-vellum.github.io/vellumplot/reference/vl_pattern.md)
  : Custom tiling-pattern fill
- [`clip_to()`](https://r-vellum.github.io/vellumplot/reference/clip.md)
  [`clip_layer()`](https://r-vellum.github.io/vellumplot/reference/clip.md)
  [`set_mask()`](https://r-vellum.github.io/vellumplot/reference/clip.md)
  : Clip or mask a plot to a geometry

## Interactivity

Declare interactions as part of the plot — selections bound to gestures,
conditional encodings, filters, and scale-domain binds. Inert on a
static render; enacted by a host such as vellumwidget.

- [`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
  [`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
  : Declare an interactive selection
- [`select_neighbours()`](https://r-vellum.github.io/vellumplot/reference/select_neighbours.md)
  : Highlight a node's graph neighbourhood
- [`condition()`](https://r-vellum.github.io/vellumplot/reference/condition.md)
  : Conditional encoding: style by selection membership
- [`add_selection()`](https://r-vellum.github.io/vellumplot/reference/add_selection.md)
  : Register a free-standing selection on a plot
- [`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)
  : Filter a view by a selection
- [`bind_scale()`](https://r-vellum.github.io/vellumplot/reference/bind_scale.md)
  : Bind a panel's view to a selection (overview + detail)
- [`inspect_source()`](https://r-vellum.github.io/vellumplot/reference/inspect_source.md)
  : Inspect the data behind a clicked element (click-to-source)
- [`interaction_model()`](https://r-vellum.github.io/vellumplot/reference/interaction_model.md)
  : The interaction model of a compiled plot

## Animation

Non-reactive keyframe animation: compile a plot into one keyframe per
state, train the scales once over all states and freeze them, then tween
and encode the frames (GIF / APNG) in vellum’s parallel Rust backend.

- [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md)
  : Animate a plot across the states of a variable
- [`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md)
  : Animate a plot along a continuous time
- [`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md)
  : Reveal a plot progressively along a variable
- [`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md)
  : Set the easing of an animation's frames
- [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md)
  : Build a keyframe animation from a plot
- [`anim_save()`](https://r-vellum.github.io/vellumplot/reference/anim_save.md)
  : Write a keyframe animation to a file

## Accessibility and provenance

Attach a text alternative for screen readers, trace each drawn element
back to its data, or verify a figure against the data it was drawn from.

- [`plot_alt()`](https://r-vellum.github.io/vellumplot/reference/plot_alt.md)
  : Text alternative (alt text) for a plot
- [`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)
  : Lint a plot for legibility and accessibility problems
- [`plot_provenance()`](https://r-vellum.github.io/vellumplot/reference/plot_provenance.md)
  : Inspect the compiled-scene provenance of a plot
- [`provenance_join()`](https://r-vellum.github.io/vellumplot/reference/provenance_join.md)
  : Join a plot's provenance to its rendered geometry
- [`provenance_payload()`](https://r-vellum.github.io/vellumplot/reference/provenance_payload.md)
  : A widget-ready provenance payload (click-to-source)
- [`plot_manifest()`](https://r-vellum.github.io/vellumplot/reference/plot_manifest.md)
  : A reproducibility manifest for a plot
- [`plot_verify()`](https://r-vellum.github.io/vellumplot/reference/plot_verify.md)
  : Verify a rendered figure against its data

## Specs, agents, and interoperability

Serialize a plot to a portable spec (and back), generate and validate
plots from an LLM / MCP agent, and translate to and from Vega-Lite.

- [`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  [`from_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)
  : Serialize a plot to a plain spec (and back)
- [`spec_to_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)
  [`spec_from_json()`](https://r-vellum.github.io/vellumplot/reference/spec_to_json.md)
  : Serialize a plot to / from a JSON spec string
- [`spec_schema()`](https://r-vellum.github.io/vellumplot/reference/spec_schema.md)
  : The vellumplot spec JSON Schema
- [`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md)
  : Summarise a data frame's fields for a model
- [`spec_diagnose()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
  [`vplot_from_spec()`](https://r-vellum.github.io/vellumplot/reference/spec_diagnose.md)
  : Diagnose a spec against its data
- [`vplot_ask()`](https://r-vellum.github.io/vellumplot/reference/vplot_ask.md)
  : Generate a plot from a natural-language request
- [`mcp_serve()`](https://r-vellum.github.io/vellumplot/reference/mcp_serve.md)
  : Run the vellumplot MCP server
- [`spec_to_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  [`spec_from_vegalite()`](https://r-vellum.github.io/vellumplot/reference/spec_to_vegalite.md)
  : Convert between a vellumplot spec and a Vega-Lite specification

## Package

- [`vellumplot`](https://r-vellum.github.io/vellumplot/reference/vellumplot-package.md)
  [`vellumplot-package`](https://r-vellum.github.io/vellumplot/reference/vellumplot-package.md)
  : vellumplot: A Grammar of Graphics on the 'vellum' Backend
