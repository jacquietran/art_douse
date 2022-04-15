# Set parameters ---------------------------------------------------------------

iteration_id <- "test_0001"
initial_seed <- set.seed(460071)

# Create data ------------------------------------------------------------------

set.seed(initial_seed)
seed_vec <- sample(seq(1, 1000000, by = 1), 3, replace = FALSE)

# Plot 1
set.seed(seed_vec[1])
data1 <- tibble::tibble(
  x = runif(2000, min = 0, max = 1),
  y = runif(2000, min = 0, max = 1))

d1 <- ggplot2::layer_data(
  ggplot2::ggplot() +
    ggplot2::stat_density_2d_filled(
      data = data1, ggplot2::aes(x = x, y = y))) |>
  dplyr::mutate(
    temp_var = sample(1:50, dplyr::n(), replace = TRUE)) |>
  dplyr::filter(temp_var == 5) |>
  dplyr::select(x, y)

# Plot 2
set.seed(seed_vec[2])
data2 <- tibble::tibble(
  x = runif(2000, min = 1, max = 2),
  y = runif(2000, min = 0, max = 1))

d2 <- ggplot2::layer_data(
  ggplot2::ggplot() +
    ggplot2::stat_density_2d_filled(
      data = data2, ggplot2::aes(x = x, y = y))) |>
  dplyr::mutate(
    temp_var = sample(1:50, dplyr::n(), replace = TRUE)) |>
  dplyr::filter(temp_var == 5) |>
  dplyr::select(x, y)

# Plot 3
set.seed(seed_vec[3])
data3 <- tibble::tibble(
  x = runif(2000, min = 2, max = 3),
  y = runif(2000, min = 0, max = 1))

d3 <- ggplot2::layer_data(
  ggplot2::ggplot() +
    ggplot2::stat_density_2d_filled(
      data = data3, ggplot2::aes(x = x, y = y))) |>
  dplyr::mutate(
    temp_var = sample(1:50, dplyr::n(), replace = TRUE)) |>
  dplyr::filter(temp_var == 5) |>
  dplyr::select(x, y)

# Build plot -------------------------------------------------------------------

# Set custom colour gradient
gradient_source <- c("#8ecae6", "#219ebc", "#023047", "#ffb703", "#fb8500")
gradient_colours <- grDevices::colorRampPalette(gradient_source)

# Build plots
n1 <- ggplot2::ggplot() +
  ggplot2::stat_density_2d(
    data = d1,
    ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(density)),
    geom = "raster", contour = FALSE, n = 300) +
  ggplot2::scale_fill_gradientn(colours = gradient_colours(10)) +
  ggplot2::coord_equal() +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none")

n2 <- ggplot2::ggplot() +
  ggplot2::stat_density_2d(
    data = d2,
    ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(density)),
    geom = "raster", contour = FALSE, n = 300) +
  ggplot2::scale_fill_gradientn(colours = gradient_colours(10)) +
  ggplot2::coord_equal() +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none")

n3 <- ggplot2::ggplot() +
  ggplot2::stat_density_2d(
    data = d3,
    ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(density)),
    geom = "raster", contour = FALSE, n = 300) +
  ggplot2::scale_fill_gradientn(colours = gradient_colours(10)) +
  ggplot2::coord_equal() +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none")

library(patchwork)

layout <- c(
  area(1, 1),
  area(2, 2),
  area(3, 1)
)

# Show the layout to make sure it looks as it should
# plot(layout)

n_all <- n1 + n2 + n3 + plot_layout(design = layout)

# Export to file ---------------------------------------------------------------

ggplot2::ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  n_all, width = 10, height = 15, units = "cm", dpi = 600)
