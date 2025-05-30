# # Ambient #####
# # library(dplyr)
# # library(purrr)
# # library(tibble)
# # library(ggplot2)
# # library(ggthemes)
# # library(ambient)
# 
# 
# sample_canva <- function(seed = NULL) {
#   if(!is.null(seed)) set.seed(seed)
#   palettes <- ggthemes::canva_palettes
#   selected <- sample(names(palettes), 1)
#   palettes[[selected]]
# }
# 
# plot_painted_canvas <- function(canvas,
#                                 seed = NULL,
#                                 palette = NULL) {
#   if(is.null(palette)) {
#     palette <- sample_canva(seed)
#   }
#   canvas |> 
#     ggplot(aes(x, y, fill = paint)) + 
#     geom_raster(show.legend = FALSE) +
#     theme_void() +
#     coord_equal() +
#     scale_x_continuous(expand = c(0, 0)) +
#     scale_y_continuous(expand = c(0, 0)) +
#     scale_fill_gradientn(colours = palette)
# }
# 
# # 
# # generate_noise_art <- function(pixels = 2000,
# #                                lf_freq = NULL,
# #                                mf_freq = NULL,
# #                                hf_freq = NULL,
# #                                gate_freq = NULL,
# #                                seed = NULL,
# #                                palette = NULL){
# #   
# #   if(!is.null(seed)) set.seed(seed)
# #   
# #   
# #   if(is.null(lf_freq)){
# #     
# #     lf_freq <- sample(1:4,1)
# #     
# #   }
# #   
# #   
# #   if(is.null(mf_freq)){
# #     
# #     mf_freq <- sample(15:40,1)
# #     
# #   }
# #   
# #   if(is.null(hf_freq)){
# #     
# #     hf_freq <- sample(70:100,1)
# #     
# #   }
# #   
# #   
# #   if(is.null(gate_freq)){
# #     
# #     gate_freq <- sample(5:15,1)
# #     
# #   }
# #   
# #   blank_canvas <- long_grid(
# #     x = seq(from = 0, to = 1, length.out = pixels),
# #     y = seq(from = 0, to = 1, length.out = pixels)
# #   ) 
# #   
# #   blank_canvas |>
# #     mutate(
# #       lf_noise = gen_simplex(x, y, frequency = lf_freq),
# #       mf_noise = gen_waves(x, y, frequency = mf_freq),
# #       hf_noise = gen_spheres(x, y, frequency = hf_freq),
# #       gate = gen_simplex(x, y, frequency = gate_freq) |> normalise(),
# #       paint = lf_noise +
# #         (2 + mf_noise) * (gate >= .2 & gate < .8) +
# #         (2 + hf_noise) * (gate >= .1)
# #     ) |>
# #     plot_painted_canvas(palette = palette)
# #   
# # }
# 
# generate_noise_art <- function(pixels = 2000,
#                                lf_freq = NULL,
#                                mf_freq = NULL,
#                                hf_freq = NULL,
#                                gate_freq = NULL,
#                                seed = NULL,
#                                palette = NULL){
#   
#   if(!is.null(seed)) set.seed(seed)
#   
#   # Default frequencies with added randomness
#   if(is.null(lf_freq)) lf_freq <- runif(1, 0.5, 5)
#   if(is.null(mf_freq)) mf_freq <- rexp(1, rate = 0.1)
#   if(is.null(hf_freq)) hf_freq <- rlnorm(1, meanlog = 4, sdlog = 0.5)
#   if(is.null(gate_freq)) gate_freq <- sample(3:20, 1)
#   
#   # Randomly choose noise functions
#   noise_functions <- list(gen_simplex, gen_waves, gen_spheres, gen_perlin)
#   lf_func <- sample(noise_functions, 1)[[1]]
#   mf_func <- sample(noise_functions, 1)[[1]]
#   hf_func <- sample(noise_functions, 1)[[1]]
#   gate_func <- sample(noise_functions, 1)[[1]]
#   
#   
#   
#   # Create canvas
#   blank_canvas <- long_grid(
#     x = seq(from = 0, to = 1, length.out = pixels),
#     y = seq(from = 0, to = 1, length.out = pixels)
#   )
#   
#   # Randomize gate thresholds
#   gate_threshold_low <- runif(1, 0.1, 0.4)
#   gate_threshold_high <- runif(1, 0.5, 0.9)
#   gate_threshold_extra <- runif(1, 0.05, 0.2)
#   
#   # Randomize layer weights
#   mf_weight <- runif(1, 1, 5)
#   hf_weight <- runif(1, 1, 5)
#   
#   
#   
#   # Generate art
#   blank_canvas |>
#     mutate(
#       lf_noise = lf_func(x, y, frequency = lf_freq),
#       mf_noise = mf_func(x, y, frequency = mf_freq),
#       hf_noise = hf_func(x, y, frequency = hf_freq),
#       gate = gate_func(x, y, frequency = gate_freq) |> normalise(),
#       paint = lf_noise +
#         mf_weight * mf_noise * (gate >= gate_threshold_low & gate < gate_threshold_high) +
#         hf_weight * hf_noise * (gate >= gate_threshold_extra)
#     ) |>
#     plot_painted_canvas(palette = palette)
# }
# 
# 
# #generate_noise_art()
