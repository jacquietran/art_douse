work_magick <- function(img_base, dust_layer1, dust_layer2){
  
  # Requires {magick}
  
  # Resize and apply filtering
  img_filtered <- img_base |>
    magick::image_scale("1000x1000!") |>
    magick::image_morphology(method = "Dilate", kernel = "Octagon", iter = 6) |>
    magick::image_oilpaint(radius = 30) |>
    magick::image_crop("900x900+50+50") |>
    magick::image_scale("4000x4000!")
  
  img_blended <- magick::image_flatten(
    c(img_filtered, dust_layer1), "LinearDodge")
  
  img_blended_again <- magick::image_flatten(
    c(img_blended, dust_layer2), "LinearDodge") |>
    magick::image_noise(noisetype = "laplacian") |>
    magick::image_noise(noisetype = "laplacian") |>
    magick::image_noise(noisetype = "laplacian")
  
  return(img_blended_again)
  
}