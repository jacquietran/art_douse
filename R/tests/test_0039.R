# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "test_0039"
initial_seed <- set.seed(578239)
grid_width <- 10
grid_height <- 1
plot_bg <- "#131313"
plot_res <- 100

# Gradient colours
base_colours <- c("#06889B", "#006B8F", "#011F51")
gradient_source1 <- c(base_colours, "#AD343E", "#D07234", "#F2AF29")
gradient_source2 <- c(base_colours, "#F93414", "#FE6C31", "#FEAE6C")
gradient_source3 <- c(base_colours, "#90C7CB", "#4EA2A6", "#346B6F")

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
  n_rows = grid_height, resolution = plot_res, bg_colour = plot_bg,
  visible_grid = FALSE)

# Export to file ---------------------------------------------------------------

ggplot2::ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}_base.png")),
  plots_in_grid, width = 2000, height = 200, units = "px",
  dpi = 600, device = ragg::agg_png)

# Shift to {magick} workflow ---------------------------------------------------

# Read in image
img <- magick::image_read(
  here::here(glue::glue("img/tests/{`iteration_id`}_base.png")))

# Resize and apply filtering
img_filtered <- img |>
  magick::image_scale("1000x1000!") |>
  magick::image_morphology(method = "Dilate", kernel = "Octagon", iter = 6) |>
  magick::image_oilpaint(radius = 30) |>
  # magick::image_blur(radius = 10, sigma = 10) |>
  magick::image_crop("900x900+50+50") |>
  magick::image_scale("1000x1000!") |>
  magick::image_noise(noisetype = "laplacian") |>
  magick::image_noise(noisetype = "laplacian")

# Export
magick::image_write(
  img_filtered,
  path = here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  format = "png")
