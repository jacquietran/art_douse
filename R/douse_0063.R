# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))
source(here::here("R/functions/specks.R"))
source(here::here("R/functions/work_magick.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "douse_0063"
initial_seed <- 225763
grid_width <- 3
grid_height <- 2
plot_bg <- "#131313"
plot_res <- 600
contour_col <- "#122626"

# Gradient colours
base_colours <- c("#CBA7DC", "#3A0E6C")

gradient_colours1 <- c(base_colours, "#E47F75", "#05A8AA", "#FA7B06")
set.seed(initial_seed)
gradient_source1 <- sample(
  gradient_colours1, size = length(gradient_colours1), replace = FALSE)

gradient_colours2 <- c(base_colours, "#F44708", "#985565")
set.seed(initial_seed)
gradient_source2 <- sample(
  gradient_colours2, size = length(gradient_colours2), replace = FALSE)

gradient_colours3 <- c(base_colours, "#FFAE03", "#3C63C1")
set.seed(initial_seed)
gradient_source3 <- sample(
  gradient_colours3, size = length(gradient_colours3), replace = FALSE)

# Derived parameters -----------------------------------------------------------

# Set custom colour gradient
gradient_colours1 <- (grDevices::colorRampPalette(gradient_source1))(10)
gradient_colours2 <- (grDevices::colorRampPalette(gradient_source2))(10)
gradient_colours3 <- (grDevices::colorRampPalette(gradient_source3))(10)

palette_list <- tibble::tibble(
  palette_num = 1:3,
  colours = list(gradient_colours1, gradient_colours2, gradient_colours3))

# Seed vector for multiple dust runs
set.seed(initial_seed)
seed_vec <- sample(seq(1, 1000000, by = 1), 2, replace = FALSE)

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
  ggplot2::geom_density_2d(
    data = data |> dplyr::filter(grouping_var == 4),
    ggplot2::aes(x = x, y = y),
    n = plot_res, colour = contour_col) +
  ggplot2::scale_fill_gradientn(colours = gradient_colours1) +
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
  magick::image_morphology(method = "Erode", iterations = 2, kernel = "Disk") |>
  magick::image_morphology(method = "Smooth", iterations = 4, kernel = "Disk") |>
  magick::image_scale("4000x4000!") |>
  magick::image_noise(noisetype = "Gaussian") |>
  magick::image_noise(noisetype = "Gaussian") |>
  magick::image_noise(noisetype = "Gaussian") |>
  magick::image_noise(noisetype = "Gaussian") |>
  magick::image_noise(noisetype = "Gaussian") |>
  magick::image_noise(noisetype = "Gaussian")

# Export
magick::image_write(
  img_filtered,
  path = here::here(glue::glue("img/painted/{`iteration_id`}.png")),
  format = "png")
