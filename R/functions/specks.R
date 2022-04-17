specks <- function(
  seed, grid_min, grid_max, dust_clearance, dust_size, bg_colour, dust_colour){
  
  # Requires {ambient}, {dplyr}, {particles}, {tidygraph}, {ggplot2}
  
  if(missing(grid_min)){
    
    grid_min <- 0
    
  }
  
  if(missing(grid_max)){
    
    grid_max <- 10
    
  }
  
  if(missing(dust_clearance)){
    
    dust_clearance <- 400
    
  }
  
  if(missing(dust_size)){
    
    dust_size <- 0.001
    
  }
  
  if(missing(bg_colour)){
    
    bg_colour <- "#000000"
    
  }
  
  if(missing(dust_colour)){
    
    dust_colour <- "#FFFFFF"
    
  }
  
  # Part 1 START
  # create noise field
  set.seed(seed)
  grid <- ambient::long_grid(
    seq(grid_min, grid_max, length.out = 1000),
    seq(grid_min, grid_max, length.out = 1000)) |>
    dplyr::mutate(noise = ambient::gen_simplex(x, y))
  
  # add curl noise
  curl <- ambient::curl_noise(ambient::gen_perlin, x = grid$x, y = grid$y)
  
  grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y, grid$x)
  
  # convert noise values to a matrix of angles
  field <- as.matrix(grid, x, value = angle)
  
  # Part 2 START
  # particle simulation
  sim <- tidygraph::create_empty(1000) |>
    particles::simulate(
      alpha_decay = 0, setup = particles::aquarium_genesis(vel_max = 0)) |>
    particles::wield(
      particles::reset_force, xvel = 0, yvel = 0) |>
    particles::wield(particles::collision_force) |>
    # particles::wield(
    #  particles::field_force, angle = field, vel = 0.002,
    #  xlim = c(-60, 25), ylim = c(-70, 75)) |>
    particles::evolve(500, particles::record)
  
  set.seed(seed)
  traces <- data.frame(
    do.call(rbind, lapply(sim$history, particles::position)))
  names(traces) <- c('x', 'y')
  traces$particle <- rep(1:1000, 100)
  
  # Subset of traces data
  set.seed(seed)
  traces_subset <- traces |>
    dplyr::mutate(
      temp_var = sample(1:dust_clearance, dplyr::n(), replace = TRUE)) |>
    dplyr::filter(temp_var == 5)
  
  # plot particles as points
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = traces_subset,
      ggplot2::aes(x, y, group = particle),
      size = dust_size, shape = 15, colour = dust_colour, fill = dust_colour) +
    ggplot2::coord_equal(xlim = c(-3,3), ylim = c(-3,3), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(
        fill = bg_colour, colour = bg_colour))
  
  return(p)
  
}