set.seed(460071)
data <- tibble::tibble(
  x = runif(2000, min = 1, max = 2),
  y = runif(2000, min = 1, max = 2))

d <- ggplot2::ggplot() +
  ggplot2::stat_density_2d_filled(
    data = data,
    ggplot2::aes(x = x, y = y))

t <- ggplot2::layer_data(d) |>
  dplyr::mutate(
    temp_var = sample(1:50, dplyr::n(), replace = TRUE)) |>
  dplyr::filter(temp_var == 5)

f <- ggplot2::ggplot() +
  ggplot2::stat_density_2d(
    data = t,
    ggplot2::aes(x = x, y = y, size = ggplot2::after_stat(density), group = group),
    geom = "point", n = 60, contour = FALSE) +
  ggplot2::scale_size_continuous(range = c(0.001, 5))

h <- ggplot2::layer_data(f) |>
  dplyr::mutate(
    hex_fill = sample(
      c("#8ecae6", "#219ebc", "#023047", "#ffb703", "#fb8500"),
      dplyr::n(), replace = TRUE))

g <- ggplot2::ggplot() +
  ggplot2::stat_density_2d(
    data = h,
    ggplot2::aes(
      x = x, y = y, size = ggplot2::after_stat(density), group = group,
      fill = hex_fill, colour = hex_fill),
    geom = "point", contour = FALSE, n = 10, shape = 20) +
  ggplot2::scale_fill_identity() +
  ggplot2::scale_colour_identity() +
  ggplot2::coord_equal() +
  # ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none")

n <- ggplot2::ggplot() +
  ggplot2::stat_density_2d(
    data = t,
    ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(density)),
    geom = "raster", contour = FALSE, n = 300) +
  ggplot2::scale_fill_viridis_c() +
  ggplot2::coord_equal() +
  # ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none")

df <- ggplot2::layer_data(n)