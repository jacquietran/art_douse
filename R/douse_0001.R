# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))
source(here::here("R/functions/specks.R"))
source(here::here("R/functions/weave.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "douse_0001"
initial_seed <- 1057801
grid_width <- 10
grid_height <- 1
plot_bg <- "#131313"
plot_res <- 100

# Gradient colours
base_colours <- c("#06889B", "#006B8F", "#011F51")
gradient_source1 <- c(base_colours, "#AD343E", "#D07234", "#F2AF29")
gradient_source2 <- c(base_colours, "#F93414", "#FE6C31", "#FEAE6C")
gradient_source3 <- c(base_colours, "#90C7CB", "#4EA2A6", "#346B6F")

# For weave
grid_min <- 0
grid_max <- 3
grid_length <- 200
warp_factor <- 60 # lower = more warping
line_colour <- "#000000"
bg_colour <- "#FFFFFF"
overlay_colour <- "#FAF7EF"

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

# Sample for noise type
set.seed(initial_seed)
noise_type <- sample(c("perlin", "simplex", "value", "cubic"), 1)

# Create data ------------------------------------------------------------------

# For 2D density plots
data <- density_grid(
  seed = initial_seed, n_cols = grid_width, n_rows = grid_height)

# For noise "weave
woven_noise <- weave(
  seed = initial_seed, grid_min = grid_min, grid_max = grid_max,
  grid_length = grid_length, warp_factor = warp_factor, geom_size_min = 0.5,
  geom_size_max = 3, noise_type = noise_type)

# Build plots ------------------------------------------------------------------

# Colourful 2D density plots
plots_in_grid <- gridded_plots(
  seed = initial_seed, data = data, palette_list = palette_list,
  n_cols = grid_width, n_rows = grid_height, resolution = plot_res,
  bg_colour = plot_bg, visible_grid = FALSE)

# Dust
dust_diff <- specks(seed = seed_vec[1], dust_clearance = 50, dust_size = 0.5)
dust_plain <- specks(seed = seed_vec[2], dust_clearance = 100)

# Noise weave
weave_plot <- ggplot2::ggplot() +
  ggplot2::geom_curve(
    data = woven_noise,
    ggplot2::aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size),
    colour = line_colour, curvature = -0.2) +
  ggplot2::scale_size_identity() +
  ggplot2::scale_colour_identity() +
  ggplot2::coord_equal(expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    plot.background = ggplot2::element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin     = ggplot2::margin(65,65,65,65, unit = "pt"))

# Export to file ---------------------------------------------------------------

# Colourful 2D density plots
ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")),
  plots_in_grid, width = 2000, height = 200, units = "px",
  dpi = 600, device = ragg::agg_png)

# Dust layers
ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_diff.png")),
  dust_diff, width = 4000, height = 4000, units = "px", dpi = 600,
  device = ragg::agg_png)

ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_plain.png")),
  dust_plain, width = 4000, height = 4000, units = "px", dpi = 600,
  device = ragg::agg_png)

ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_woven_noise.png")),
  weave_plot, width = 4000, height = 4000, units = "px", dpi = 600,
  device = ragg::agg_png)

# Shift to {magick} workflow ---------------------------------------------------

# Read in images
img <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")))
img_dust_diff <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_diff.png")))
img_dust_plain <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_plain.png")))
img_weave <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_woven_noise.png")))
img_overlay <- magick::image_read(
  here::here(glue::glue("img/ingredients/blank_overlay.png")))

img_overlay_filled <- magick::image_background(
  img_overlay, overlay_colour, flatten = TRUE)

# Resize and apply filtering
img_filtered <- img |>
  magick::image_scale("1000x1000!") |>
  magick::image_morphology(method = "Dilate", kernel = "Octagon", iter = 6) |>
  magick::image_oilpaint(radius = 30) |>
  magick::image_crop("900x900+50+50") |>
  magick::image_scale("4000x4000!")

img_blended <- magick::image_flatten(
  c(img_filtered, img_dust_diff), "Difference")

img_blended_again <- magick::image_flatten(
  c(img_blended, img_dust_plain), "LinearDodge") |>
  magick::image_noise(noisetype = "laplacian") |>
  magick::image_noise(noisetype = "laplacian") |>
  magick::image_noise(noisetype = "laplacian")

img_with_weave <- magick::image_flatten(
  c(img_blended_again, img_weave), "Screen")

img_final <- magick::image_flatten(
  c(img_overlay_filled, img_with_weave), "Multiply")

# Export
magick::image_write(
  img_final,
  path = here::here(glue::glue("img/{`iteration_id`}.png")),
  format = "png")
