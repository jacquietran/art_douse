#######################################
# For generating each 2d density plot #
#######################################
density_plot <- function(
  data, palette, rounded = c(TRUE, FALSE), resolution,
  visible_grid = c(TRUE, FALSE)){
  
  # Requires {tibble}, {ggplot2}, {ggfx}
  
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
    
    p <- ggplot2::ggplot() +
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
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none")
    
  } else {
    
    p <- ggplot2::ggplot() +
      ggplot2::stat_density_2d(
        data = data,
        ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(density)),
        geom = "raster", contour = FALSE, n = resolution) +
      ggplot2::scale_fill_gradientn(colours = palette) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none")
    
  }
  
  if(visible_grid == TRUE){
    
    p <- p + ggplot2::coord_equal(expand = TRUE)
    
  }
  
  if(visible_grid == FALSE){
    
    p <- p + ggplot2::coord_equal(expand = FALSE)
    
  }
  
  return(p)
  
}