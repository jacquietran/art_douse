# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "test_0020"
initial_seed <- set.seed(578220)
grid_width <- 10
grid_height <- 10
plot_bg <- "#FDF3ED"
plot_res <- 100

# Gradient colours
gradient_source1 <- c("#faedcd", "#d4a373", "#F26418", "#F9844A", "#FBC489")
gradient_source2 <- c("#faedcd", "#d4a373", "#C68B80", "#E09A7D", "#F39C6B")
gradient_source3 <- c("#faedcd", "#d4a373", "#C68B80", "#E09A7D", "#F39C6B")

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
  visible_grid = TRUE, outer_margin = 20)

# Export to file ---------------------------------------------------------------

ggplot2::ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}_base.png")),
  plots_in_grid, width = 2000, height = 2000, units = "px",
  dpi = 600, device = ragg::agg_png)

# Shift to {magick} workflow ---------------------------------------------------

# Read in image
img <- magick::image_read(
  here::here(glue::glue("img/tests/{`iteration_id`}_base.png")))

# Resize and apply filtering
img_filtered <- img |>
  magick::image_morphology(method = "Dilate", kernel = "Octagon", iter = 4) |>
  magick::image_oilpaint(radius = 20) |>
  # magick::image_blur(radius = 10, sigma = 10) |>
  magick::image_crop("1900x1900+50+50") |>
  magick::image_noise(noisetype = "laplacian") |>
  magick::image_border("#FFFFFF", "250x250")

# Export
magick::image_write(
  img_filtered,
  path = here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  format = "png")
