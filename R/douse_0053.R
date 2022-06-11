# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))
source(here::here("R/functions/specks.R"))
source(here::here("R/functions/work_magick.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "douse_0053"
initial_seed <- 225753
grid_width <- 60
grid_height <- 4
plot_bg <- "#131313"
plot_res <- 100

# Gradient colours
base_colours <- c("#c6b8b7", "#a8bfc2", "#018dbe", "#e4d8d8")

gradient_colours1 <- c(base_colours, "#d79702", "#9e4fa3", "#052a7a")
set.seed(initial_seed)
gradient_source1 <- sample(
  gradient_colours1, size = length(gradient_colours1), replace = FALSE)

gradient_colours2 <- c(base_colours, "#e87ba4", "#b282af", "#008979")
set.seed(initial_seed)
gradient_source2 <- sample(
  gradient_colours2, size = length(gradient_colours2), replace = FALSE)

gradient_colours3 <- c(base_colours, "#f32196", "#de5109")
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

# Colourful 2D density plots
plots_in_grid <- gridded_plots(
  seed = initial_seed, data = data, palette_list = palette_list,
  n_cols = grid_width, n_rows = grid_height, resolution = plot_res,
  bg_colour = plot_bg, visible_grid = FALSE)

# Dust
dust_diff <- specks(seed = seed_vec[1], dust_clearance = 200, dust_size = 0.5)
dust_plain <- specks(seed = seed_vec[2], dust_clearance = 200)

# Export to file ---------------------------------------------------------------

# Colourful 2D density plots
ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")),
  plots_in_grid, width = 6000, height = 400, units = "px",
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
