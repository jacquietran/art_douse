# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))
source(here::here("R/functions/specks.R"))
source(here::here("R/functions/work_magick.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "douse_0160"
initial_seed <- 570160
grid_width <- 3
grid_height <- 2
plot_bg <- "#131313"
plot_res <- 600
contour_col <- "#122626"

# Gradient colours
gradient_colours <- c("#B9314F", "#143642", "#0F8B8D", "#EC9A29", "#D6DBB2")

# Derived parameters -----------------------------------------------------------

# Randomly reorder colours in vector
set.seed(initial_seed)
gradient_source <- sample(
  gradient_colours, size = length(gradient_colours), replace = FALSE)

# Generate a vector of colours of length 10
gradient_colours_vec <- (grDevices::colorRampPalette(gradient_source))(10)

# Adjustment factor for 2D density plots
set.seed(initial_seed)
adjust_factor <- sample(seq(0.75, 2, by = 0.1), 1)

# Line size for 2D density plots
# set.seed(initial_seed)
# line_size <- sample(seq(0.01, 0.1, by = 0.01), 2)
# line_size <- 0.1

# Set number of iterations for smoothing filter
# set.seed(initial_seed)
# smoothing_iter <- sample(seq(1, 6, by = 1), 1)
smoothing_iter <- 2

# Create data ------------------------------------------------------------------

data <- density_grid(
  seed = initial_seed, n_cols = grid_width, n_rows = grid_height)

# Build plots ------------------------------------------------------------------

p <- ggplot2::ggplot() +
  ggfx::as_reference(
    ggplot2::stat_density_2d(
      data = data |> dplyr::filter(grouping_var == 1),
      ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(density)),
      geom = "raster", contour = FALSE, n = plot_res),
    id = "base") +
  ggfx::as_reference(
    ggfx::with_blend(
      ggplot2::stat_density_2d(
        data = data |> dplyr::filter(grouping_var == 2),
        ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(density)),
        geom = "raster", contour = FALSE, n = plot_res),
      bg_layer = "base",
      blend_type = "soft_light",
      alpha = 1),
    id = "layer2") +
  ggfx::with_blend(
    ggplot2::stat_density_2d(
      data = data |> dplyr::filter(grouping_var == 3),
      ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(density)),
      geom = "raster", contour = FALSE, n = plot_res),
    bg_layer = "layer2",
    blend_type = "hard_light",
    alpha = 0.2) +
  #ggplot2::geom_density_2d(
  #  data = data |> dplyr::filter(grouping_var == 5),
  #  ggplot2::aes(x = x, y = y, group = grouping_var),
  #  n = plot_res, colour = contour_col, adjust = adjust_factor, size = line_size[1]) +
  ggplot2::scale_fill_gradientn(colours = gradient_colours_vec) +
  ggplot2::coord_fixed(expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none")

# Export to file ---------------------------------------------------------------

ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")),
  p, width = 1000, height = 1000, units = "px",
  dpi = 600, device = ragg::agg_png)

# Shift to {magick} workflow ---------------------------------------------------

# Read in image
img <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")))

# Apply filters
img_filtered <- img |>
  magick::image_morphology(method = "Dilate", iterations = 3, kernel = "Disk") |>
  magick::image_morphology(method = "Smooth", iterations = smoothing_iter, kernel = "Disk") |>
  magick::image_crop("800x800+100+100") |>
  magick::image_scale("4000x4000!") |>
  magick::image_noise(noisetype = "Gaussian") |>
  magick::image_noise(noisetype = "Gaussian") |>
  magick::image_noise(noisetype = "Gaussian")

# Export
magick::image_write(
  img_filtered,
  path = here::here(glue::glue("img/painted/{`iteration_id`}.png")),
  format = "png")
