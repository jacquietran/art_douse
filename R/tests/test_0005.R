# Set parameters ---------------------------------------------------------------

# Modifiable params
iteration_id <- "test_0005"
initial_seed <- set.seed(460075)
gradient_source <- c("#8ecae6", "#219ebc", "#023047", "#ffb703", "#fb8500")
grid_width <- 6
grid_height <- 4

# Derived params
# Set custom colour gradient
gradient_colours <- (grDevices::colorRampPalette(gradient_source))(10)
gradient_bw <- (grDevices::colorRampPalette(c("#FFFFFF", "#000000")))(10)

# Determine number of plots based on width and height vars
n_plots <- grid_width * grid_height

# Define custom function -------------------------------------------------------

####################################################
# For creating data for a grid of 2D density plots #
####################################################
density_grid <- function(seed, n_cols, n_rows){
  
  # Requires {purrr}, {tibble}, {ggplot2}, {dplyr}
  
  set.seed(seed)
  seed_vec <- sample(
    seq(1, 10000000, by = 1), (n_cols * n_rows), replace = FALSE)
  
  layer_data <- purrr::map_dfr(seed_vec, function(i){
    
    # simple but effective progress indicator
    cat(".")
    
    data <- tibble::tibble(
      x = runif(2000, min = 0, max = 1),
      y = runif(2000, min = 0, max = 1))
    
    layer_data <- ggplot2::layer_data(
      ggplot2::ggplot() +
        ggplot2::stat_density_2d_filled(
          data = data, ggplot2::aes(x = x, y = y))) |>
      dplyr::mutate(
        seed = i,
        temp_var = sample(1:50, dplyr::n(), replace = TRUE)) |>
      dplyr::filter(temp_var == 5) |>
      dplyr::select(seed, x, y)
    
  })
  
  groups_numbered <- layer_data |>
    dplyr::distinct(seed) |>
    dplyr::arrange(seed) |>
    dplyr::mutate(
      temp_var = 1,
      grouping_var = cumsum(temp_var)) |>
    dplyr::select(-temp_var)
  
  layer_data_tidy <- dplyr::left_join(
    groups_numbered, layer_data, by = "seed")  
  
  return(layer_data_tidy)
  
}

#######################################
# For generating each 2d density plot #
#######################################
density_plot <- function(data, palette){
  
  # Requires {tibble}, {ggplot2}
  
  rounded_square <- tibble::tibble(
    x = c(0,0,1,1,0),
    y = c(0,1,1,0,0))
  
  ggplot2::ggplot() +
    ggfx::as_reference(
      ggforce::geom_shape(
        data = rounded_square,
        ggplot2::aes(x = x, y = y),
        radius = ggplot2::unit(0.1, 'cm')),
      id = "shape") +
    ggfx::with_mask(
      ggplot2::stat_density_2d(
        data = data,
        ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(density)),
        geom = "raster", contour = FALSE, n = 300),
      mask = ggfx::ch_alpha("shape"))+
    ggplot2::scale_fill_gradientn(colours = palette) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none")
  
}

# Create data ------------------------------------------------------------------

data <- density_grid(
  seed = initial_seed, n_cols = grid_width, n_rows = grid_height)

# Build plots ------------------------------------------------------------------

# Build each density plot inside a for loop
palette_list <- tibble::tibble(
  plot_num = 1:n_plots,
  palette_type = sample(c("colour", "bw"), n_plots, replace = TRUE)) |>
  dplyr::mutate(
    colours = dplyr::case_when(
      palette_type == "colour" ~ list(gradient_colours),
      palette_type == "bw"     ~ list(gradient_bw))) |>
  dplyr::pull(colours)

plot_list <- list()
for (i in 1:n_plots) {
  
  p <- density_plot(
    data = data |> dplyr::filter(grouping_var == i),
    palette = palette_list[[i]])
  
  plot_list[[i]] <- p
  
}

# Prepare grid of density plots
plots_in_grid <- patchwork::wrap_plots(
  plot_list, ncol = grid_width, nrow = grid_height)

# Export to file ---------------------------------------------------------------

ggplot2::ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  plots_in_grid, width = 6, height = 4, units = "cm", dpi = 600,
  device = ragg::agg_png)

