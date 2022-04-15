# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "test_0010"
initial_seed <- set.seed(578210)
grid_width <- 12
grid_height <- 8
plot_bg <- "#131313"
plot_res <- 10

# Gradient colours
gradient_source1 <- c("#06889B", "#006B8F", "#011F51", "#AD343E", "#D07234", "#F2AF29")
gradient_source2 <- c("#06889B", "#006B8F", "#011F51", "#F93414", "#FE6C31", "#FEAE6C")
gradient_source3 <- c("#06889B", "#006B8F", "#011F51", "#90C7CB", "#4EA2A6", "#346B6F")

# Derived parameters -----------------------------------------------------------

# Set custom colour gradient
gradient_colours1 <- (grDevices::colorRampPalette(gradient_source1))(10)
gradient_colours2 <- (grDevices::colorRampPalette(gradient_source2))(10)
gradient_colours3 <- (grDevices::colorRampPalette(gradient_source3))(10)

palette_list <- tibble::tibble(
  palette_num = 1:3,
  colours = list(gradient_colours1, gradient_colours2, gradient_colours3))

# Determine number of plots based on width and height vars
#n_plots <- grid_width * grid_height # may not need this once i write the relevant function

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
        seed = i) |>
      #  temp_var = sample(1:50, dplyr::n(), replace = TRUE)) |>
      # dplyr::filter(temp_var == 5) |>
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
density_plot <- function(
  data, palette, rounded = c(TRUE, FALSE), resolution,
  visible_grid = c(TRUE, FALSE)){
  
  # Requires {tibble}, {ggplot2}
  
  if(missing(rounded)){
    
    rounded <- FALSE
    
  }
  
  if(missing(resolution)){
    
    resolution <- 300
    
  }
  
  if(missing(visible_grid)){
    
    visible_grid <- TRUE
    
  }
  
  rounded_square <- tibble::tibble(
    x = c(0,0,1,1,0),
    y = c(0,1,1,0,0))
  
  if(rounded == TRUE){
    
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
          geom = "raster", contour = FALSE, n = resolution),
        mask = ggfx::ch_alpha("shape"))+
      ggplot2::scale_fill_gradientn(colours = palette) +
      ggplot2::coord_equal(
        xlim = c(0,1), ylim = c(0,1), expand = visible_grid) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none")
    
  } else {
    
    ggplot2::ggplot() +
      ggplot2::stat_density_2d(
        data = data,
        ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(density)),
        geom = "raster", contour = FALSE, n = resolution) +
      ggplot2::scale_fill_gradientn(colours = palette) +
      ggplot2::coord_equal(
        xlim = c(0,1), ylim = c(0,1), expand = visible_grid) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none")
    
  }
  
}

##############################
# For building plots in grid #
##############################
gridded_plots <- function(
  data, palette_list, bg_colour, n_cols, n_rows, rounded = c(TRUE, FALSE),
  resolution, visible_grid = c(TRUE, FALSE)){
  
  # Requires {tibble}, {dplyr}, {patchwork}, {ggplot2}
  
  if(missing(rounded)){
    
    rounded <- FALSE
    
  }
  
  if(missing(resolution)){
    
    resolution <- 300
    
  }
  
  if(missing(visible_grid)){
    
    visible_grid <- TRUE
    
  }
  
  n_plots <- n_cols * n_rows
  
  # Build each density plot inside a for loop
  palette_assign <- tibble::tibble(
    plot_num = 1:n_plots,
    palette_num = sample(
      seq(1, 3, by = 1), n_plots, replace = TRUE, prob = c(0.4, 0.4, 0.2)))
  
  palette_ref <- dplyr::left_join(
    palette_list, palette_assign, by = "palette_num") |>
    dplyr::arrange(plot_num) |>
    dplyr::pull(colours)
  
  plot_list <- list()
  for (i in 1:n_plots) {
    
    p <- density_plot(
      data = data |> dplyr::filter(grouping_var == i),
      palette = palette_ref[[i]], rounded = rounded, resolution = resolution,
      visible_grid = visible_grid)
    
    plot_list[[i]] <- p
    
  }
  
  # Prepare grid of density plots
  plots_in_grid <- patchwork::wrap_plots(
    plot_list, ncol = grid_width, nrow = grid_height) +
    patchwork::plot_annotation(
      theme = ggplot2::theme(
        plot.background = ggplot2::element_rect(
          fill = bg_colour, colour = bg_colour)))
  
  return(plots_in_grid)
  
}

# Create data ------------------------------------------------------------------

data <- density_grid(
  seed = initial_seed, n_cols = grid_width, n_rows = grid_height)

# Build plots ------------------------------------------------------------------

plots_in_grid <- gridded_plots(
  data = data, palette_list = palette_list, n_cols = grid_width,
  n_rows = grid_height, resolution = plot_res, bg_colour = plot_bg,
  visible_grid = FALSE)

# Export to file ---------------------------------------------------------------

ggplot2::ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  plots_in_grid, width = 12, height = 8, units = "cm", dpi = 600,
  device = ragg::agg_png)

