##############################
# For building plots in grid #
##############################
gridded_plots <- function(
  data, palette_list, bg_colour, n_cols, n_rows, rounded = c(TRUE, FALSE),
  resolution){
  
  # Requires {tibble}, {dplyr}, {patchwork}, {ggplot2} and
  # custom function density_plot()
  
  if(missing(rounded)){
    
    rounded <- FALSE
    
  }
  
  if(missing(resolution)){
    
    resolution <- 300
    
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
      palette = palette_ref[[i]], rounded = rounded, resolution = resolution)
    
    plot_list[[i]] <- p
    
  }
  
  # Prepare grid of density plots
  plots_in_grid <- patchwork::wrap_plots(
    plot_list, ncol = grid_width, nrow = grid_height) +
    patchwork::plot_annotation(
      theme = ggplot2::theme(
        plot.background = ggplot2::element_rect(
          fill = bg_colour, colour = bg_colour),
        plot.margin = ggplot2::margin(0,0,0,0, unit = "pt")))
  
  return(plots_in_grid)
  
}