# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))
source(here::here("R/functions/specks.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "douse_0011"
initial_seed <- 1057811
grid_width <- 10
grid_height <- 1
plot_bg <- "#131313"
plot_res <- 100

# Gradient colours
base_colours <- c("#F6F4F4", "#c6b8b7", "#a8bfc2", "#018dbe", "#e4d8d8")
gradient_source1 <- c(base_colours, "#d79702", "#9e4fa3", "#052a7a")
gradient_source2 <- c(base_colours, "#e87ba4", "#b282af", "#008979")
gradient_source3 <- c(base_colours, "#f32196", "#de5109")

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
seed_vec <- sample(seq(1, 1000000, by = 1), 3, replace = FALSE)

# Create data ------------------------------------------------------------------

data <- density_grid(
  seed = initial_seed, n_cols = grid_width, n_rows = grid_height)

# Build plots ------------------------------------------------------------------

# Colourful 2D density plots
plots_in_grid <- gridded_plots(
  seed = initial_seed, data = data, palette_list = palette_list,
  n_cols = grid_width, n_rows = grid_height, resolution = plot_res,
  bg_colour = plot_bg, visible_grid = FALSE)

# Dust
dust_outlines <- specks(
  seed = seed_vec[1], dust_clearance = 500, dust_size = 30, point_shape = 0)
dust_diff <- specks(seed = seed_vec[2], dust_clearance = 50, dust_size = 0.5)
dust_plain <- specks(seed = seed_vec[3], dust_clearance = 100)

# Export to file ---------------------------------------------------------------

# Colourful 2D density plots
ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")),
  plots_in_grid, width = 2000, height = 200, units = "px",
  dpi = 600, device = ragg::agg_png)

# Dust layers
ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_outlines.png")),
  dust_outlines, width = 4000, height = 4000, units = "px", dpi = 600,
  device = ragg::agg_png)

ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_diff.png")),
  dust_diff, width = 4000, height = 4000, units = "px", dpi = 600,
  device = ragg::agg_png)

ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_plain.png")),
  dust_plain, width = 4000, height = 4000, units = "px", dpi = 600,
  device = ragg::agg_png)

# Shift to {magick} workflow ---------------------------------------------------

# Read in images
img <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")))

img_dust_outlines <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_outlines.png")))

img_dust_diff <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_diff.png")))

img_dust_plain <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_plain.png")))

# Resize and apply filtering
img_filtered <- img |>
  magick::image_scale("1000x1000!") |>
  magick::image_morphology(method = "Dilate", kernel = "Octagon", iter = 6) |>
  magick::image_oilpaint(radius = 30) |>
  # magick::image_blur(radius = 10, sigma = 10) |>
  magick::image_crop("900x900+50+50") |>
  magick::image_scale("4000x4000!")

img_blended <- magick::image_flatten(
  c(img_filtered, img_dust_diff, img_dust_outlines), "LinearDodge")

img_blended_again <- magick::image_flatten(
  c(img_blended, img_dust_plain), "LinearDodge") |>
  magick::image_noise(noisetype = "laplacian") |>
  magick::image_noise(noisetype = "laplacian") |>
  magick::image_noise(noisetype = "laplacian")

# Export
magick::image_write(
  img_blended_again,
  path = here::here(glue::glue("img/painted/{`iteration_id`}.png")),
  format = "png")
