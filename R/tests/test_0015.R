# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "test_0015"
initial_seed <- set.seed(578215)
grid_width <- 30
grid_height <- 30
plot_bg <- "#FDF3ED"
plot_res <- 100

# Gradient colours
gradient_source1 <- c("#D1CBC7", "#828FA6", "#335284", "#B96427", "#C67A2F", "#D79847")
gradient_source2 <- c("#D1CBC7", "#828FA6", "#335284", "#C33149", "#D16174", "#DF909D")
gradient_source3 <- c("#D1CBC7", "#828FA6", "#335284", "#3C5967", "#628393", "#869FAC")

# Derived parameters -----------------------------------------------------------

# Set custom colour gradient
gradient_colours1 <- (grDevices::colorRampPalette(gradient_source1))(10)
gradient_colours2 <- (grDevices::colorRampPalette(gradient_source2))(10)
gradient_colours3 <- (grDevices::colorRampPalette(gradient_source3))(10)

palette_list <- tibble::tibble(
  palette_num = 1:3,
  colours = list(gradient_colours1, gradient_colours2, gradient_colours3))

# Create data ------------------------------------------------------------------

data <- density_grid(
  seed = initial_seed, n_cols = grid_width, n_rows = grid_height)

# Build plots ------------------------------------------------------------------

plots_in_grid <- gridded_plots(
  data = data, palette_list = palette_list, n_cols = grid_width,
  n_rows = grid_height, resolution = plot_res, bg_colour = plot_bg)

# Export to file ---------------------------------------------------------------

ggplot2::ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  plots_in_grid, width = 10, height = 10, units = "cm",
  dpi = 600, device = ragg::agg_png)

