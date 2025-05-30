## Fractals #####
# library(dplyr)
# library(purrr)
# library(tibble)
# library(ggplot2)
# library(ggthemes)
# library(ambient)
sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  palettes <- ggthemes::canva_palettes
  selected <- sample(names(palettes), 1)
  palettes[[selected]]
}

plot_painted_canvas <- function(canvas,
                                seed = NULL,
                                palette = NULL) {
  if(is.null(palette)) {
    palette <- sample_canva(seed)
  }
  canvas |> 
    ggplot(aes(x, y, fill = paint)) + 
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
}


fractal_art <- function(fractal, generator,pixels = 2000, palette = NULL, ...) {
  
  
  # Create canvas
  blank_canvas <- long_grid(
    x = seq(from = 0, to = 1, length.out = pixels),
    y = seq(from = 0, to = 1, length.out = pixels)
  )
  
  
  
  blank_canvas |>
    mutate(
      paint = fracture(
        noise = generator,
        fractal = fractal,
        x = x, 
        y = y, 
        ...
      )
    ) |>
    plot_painted_canvas(palette = palette)
}




gen_scope <- function(x, y, ...) {
  worley_cell <-gen_worley(x, y, value = "cell", ...)
  worley_dist <-gen_worley(x, y, value = "distance", ...)
  return(normalise(worley_cell) + 5 * normalise(worley_dist))
}


generate_fractal_art <- function(pixels = 2000,
                                 fractal = NULL,
                                 noise = NULL,
                                 gain = NULL,
                                 octaves = NULL,
                                 seed = NULL,
                                 palette = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Default frequencies with added randomness 
  if (is.null(octaves)) octaves <- sample(2:20, 1)  
   
  
  # Randomly choose noise functions
  noise_functions <- list(gen_simplex, gen_spheres, gen_perlin, gen_scope)
  noise <- sample(noise_functions, 1)[[1]]
  
  # Randomly choose fractal functions
  fractal_functions <- list(ridged, fbm, billow)
  fractal <- sample(fractal_functions, 1)[[1]]
  
  
  
  # Define gain function with reasonable scaling
  gf <- function(x) x * sample(seq(0.4, 0.8, 0.1), 1)  # Adjusted step size
  
  # Generate fractal art
  tryCatch({
    fractal_art(
      generator = noise,
      fractal = fractal,
      pixels = pixels,
      palette = palette,
      octaves = octaves,
      gain = gf
    )
  }, error = function(e) {
    message("An error occurred during fractal generation:")
    message(e$message)
  })
}

## Example usage
## more pixels more detail dont go more than 2000
#generate_fractal_art(pixels = 200)




