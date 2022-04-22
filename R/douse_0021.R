# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))
source(here::here("R/functions/specks.R"))
source(here::here("R/functions/work_magick.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "douse_0021"
initial_seed <- 1057821
grid_width <- 10
grid_height <- 1
plot_bg <- "#131313"
plot_res <- 100

# Gradient colours
base_colours <- c("#F3E8CD", "#DCDBC9", "#AAC5B4")
gradient_source1 <- c(base_colours, "#6B2E32", "#738972")
gradient_source2 <- c(base_colours, "#E9C8C4", "#954C50", "#40544B")
gradient_source3 <- c(base_colours, "#C1928E", "#4F6069")

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

# Colourful 2D density plots
plots_in_grid <- gridded_plots(
  seed = initial_seed, data = data, palette_list = palette_list,
  n_cols = grid_width, n_rows = grid_height, resolution = plot_res,
  bg_colour = plot_bg, visible_grid = FALSE)

# Dust
dust_diff <- specks(seed = seed_vec[1], dust_clearance = 50, dust_size = 0.5)
dust_plain <- specks(seed = seed_vec[2], dust_clearance = 100)

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

# Shift to {magick} workflow ---------------------------------------------------

# Read in images
img <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")))

img_dust_diff <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_diff.png")))

img_dust_plain <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_dust_plain.png")))

# Perform image manipulation using {magick} functions
img_adjusted <- work_magick(
  img_base = img, dust_layer1 = img_dust_diff, dust_layer2 = img_dust_plain)

# Export
magick::image_write(
  img_adjusted,
  path = here::here(glue::glue("img/painted/{`iteration_id`}.png")),
  format = "png")
