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
    
    set.seed(i)
    data <- tibble::tibble(
      x = runif(2000, min = 0, max = 1),
      y = runif(2000, min = 0, max = 1))
    
    set.seed(i)
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