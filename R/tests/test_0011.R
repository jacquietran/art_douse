# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "test_0011"
initial_seed <- set.seed(578211)
grid_width <- 15
grid_height <- 10
plot_bg <- "#131313"
plot_res <- 10

# Gradient colours
gradient_source1 <- c("#06889B", "#006B8F", "#011F51", "#AD343E", "#D07234", "#F2AF29")
gradient_source2 <- c("#06889B", "#006B8F", "#011F51", "#F93414", "#FE6C31", "#FEAE6C")
gradient_source3 <- c("#06889B", "#006B8F", "#011F51", "#90C7CB", "#4EA2A6", "#346B6F")

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
  plots_in_grid, width = 15, height = 10, units = "cm", dpi = 600,
  device = ragg::agg_png)

