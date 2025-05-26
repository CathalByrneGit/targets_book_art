library(ggplot2)
library(tibble)

# Polar #####
polar_art <- function(seed, n, palette) {
  
  # set the state of the random number generator
  set.seed(seed)
  
  # data frame containing random values for 
  # aesthetics we might want to use in the art
  dat <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2),
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = runif(n), 
    width = runif(n)
  )
  
  # plot segments in various colours, using 
  # polar coordinates and a gradient palette
  dat |> 
    ggplot(aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade,
      linewidth = width
    )) +
    geom_segment(show.legend = FALSE) +
    coord_polar() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_colour_gradientn(colours = palette) + 
    scale_linewidth(range = c(0, 10)) + 
    theme_void()
}

sample_canva <- function(seed = 1) {
  set.seed(seed)
  palettes <- ggthemes::canva_palettes
  selected <- sample(names(palettes), 1)
  palettes[[selected]]
}

sample_data <- function(seed = NULL, n = 100){
  if(!is.null(seed)) set.seed(seed)
  dat <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2),
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = runif(n), 
    size = runif(n),
    shape = factor(sample(0:22, size = n, replace = TRUE))
  )
}

polar_styled_plot <- function(data = NULL, palette) {
  ggplot(
    data = data,
    mapping = aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade,
      size = size
    )) + 
    coord_polar(clip = "off") +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) +
    scale_x_continuous(
      expand = c(0, 0), 
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) + 
    scale_colour_gradientn(colours = palette) + 
    scale_size(range = c(0, 10)) + 
    theme_void() + 
    guides(
      colour = guide_none(),
      size = guide_none(),
      fill = guide_none(),
      shape = guide_none()
    )
}

dat <- sample_data(n = 190, seed = 7) 
pal <- sample_canva(seed = 9)

polar_styled_plot(data = dat, palette = pal) + geom_segment()
polar_styled_plot(data = dat, palette = pal) + geom_path()
polar_styled_plot(data = dat, palette = pal) + geom_point()
# Ambient #####
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)

sample_cross_matrix <- function(n = 10, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  mat <- matrix(data = 0, nrow = n, ncol = n)
  mat[sample(n, 1), ] <- 1
  mat[, sample(n, 1)] <- 1
  return(mat)
}

image(sample_cross_matrix(n = 50), axes = FALSE, useRaster = TRUE)

x_coords <- seq(from = 0, to = 1, length.out = 800)
y_coords <- seq(from = 0, to = 1, length.out = 800)

canvas <- long_grid(x = x_coords, y = y_coords) 

canvas <- canvas |> 
  mutate(paint = gen_perlin(x, y, frequency = 10, seed = 1234))

art <- ggplot(canvas, aes(x, y, fill = paint)) + 
  geom_raster(show.legend = FALSE)

art
art + 
  theme_void() +
  coord_equal()
art + 
  theme_void() +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = sample_canva())

make_noise_art <- function(
    generator = gen_perlin, 
    frequency = 10, 
    seed = 1234,
    pixels = 2000,
    palette = c("#e5ddc8", "#01949a", "#004369", "#db1f48"), 
    ...
) {
  
  # define the grid
  canvas <- long_grid(
    x = seq(from = 0, to = 1, length.out = pixels),
    y = seq(from = 0, to = 1, length.out = pixels)
  ) 
  
  # use the generator to add paint
  canvas <- canvas |>
    mutate(
      paint = generator(
        x, y, 
        frequency = frequency, 
        seed = seed, 
        ...
      )
    )
  
  # use ggplot2 to draw the picture
  art <- canvas |> 
    ggplot(aes(x, y, fill = paint)) + 
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
  
  return(art)
}

make_noise_art(generator = gen_waves) 


plot_painted_canvas <- function(canvas, palette = NULL) {
  if(is.null(palette)) {
    palette <- c("#e5ddc8","#01949a","#004369","#db1f48")
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

blank_canvas <- long_grid(
  x = seq(from = 0, to = 1, length.out = 2000),
  y = seq(from = 0, to = 1, length.out = 2000)
) 

blank_canvas |> 
  mutate(
    lf_noise = gen_simplex(x, y, frequency = 1, seed = 1234),
    mf_noise = gen_simplex(x, y, frequency = 200, seed = 134),
    hf_noise = gen_simplex(x, y, frequency = 99, seed = 1234),
    paint = lf_noise + mf_noise + hf_noise
  ) |>
  plot_painted_canvas()


blank_canvas |> 
  mutate(
    lf_noise = gen_simplex(x, y, frequency = 1),
    mf_noise = gen_simplex(x, y, frequency = 20),
    hf_noise = gen_simplex(x, y, frequency = 99),
    gate = gen_simplex(x, y, frequency = 10) |> normalise(),
    paint = lf_noise +
      (2 + mf_noise) * (gate >= .2 & gate < .8) +
      (2 + hf_noise) * (gate >= .1)
  ) |>
  plot_painted_canvas(palette = sample_canva(seed = 3))

## Fractals #####

gen_sin <- function(x, frequency, ...) {
  sin(x * frequency)
}

fracture(
  x = 1:20, 
  noise = gen_sin, 
  fractal = fbm, 
  octaves = 8
)

dat <- tibble(
  x = seq(0, 10, length.out = 1000), 
  y1 = fracture(x = x, noise = gen_sin, fractal = fbm, octaves = 1),
  y2 = fracture(x = x, noise = gen_sin, fractal = fbm, octaves = 2),
  y8 = fracture(x = x, noise = gen_sin, fractal = fbm, octaves = 8),
  y20 = fracture(x = x, noise = gen_sin, fractal = fbm, octaves = 20)
) 

ggplot(dat) + geom_path(aes(x, y1)) + ggtitle("One iteration")
ggplot(dat) + geom_path(aes(x, y2)) + ggtitle("Two iterations")
ggplot(dat) + geom_path(aes(x, y8)) + ggtitle("Eight iterations")
ggplot(dat) + geom_path(aes(x, y20)) + ggtitle("Twenty iterations")

fractal_art <- function(fractal, generator, palette = NULL, ...) {
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

fractal_art(fbm, gen_waves, seed = 1, octaves = 20)
fractal_art(ridged, gen_simplex, seed = 2, octaves = 20)
fractal_art(fbm, gen_simplex, seed = 2, octaves = 20)

gf <- function(x) x * .8
fractal_art(ridged, gen_simplex, seed = 2, octaves = 1, gain = gf)
fractal_art(ridged, gen_simplex, seed = 2, octaves = 2, gain = gf)
fractal_art(ridged, gen_simplex, seed = 2, octaves = 20, gain = gf)

blank_canvas |>
  mutate(paint = gen_simplex(x, y, seed = 2)) |>
  plot_painted_canvas()

gen_scope <- function(x, y, ...) {
  worley_cell <-gen_worley(x, y, value = "cell", ...)
  worley_dist <-gen_worley(x, y, value = "distance", ...)
  return(normalise(worley_cell) + 5 * normalise(worley_dist))
}

fractal_art(billow, gen_scope, palette = pal, seed = 9, octaves = 8)

## Curl ########
build_art <- function(points = 1000, groups = 10, seed = 1) {
  set.seed(seed)
  df <- data.frame(
    x = runif(points),
    y = runif(points),
    group = sample(1:groups, points, replace = TRUE)
  )
  df
}

smol_grid <- long_grid(x = 1:20, y = 1:20)
ggplot(smol_grid) +
  geom_point(aes(x, y)) + 
  theme_void() + 
  coord_equal()

smol_simplex <- smol_grid |>
  mutate(z = gen_simplex(x, y, seed = 1, frequency = .1)) 

smol_simplex |>
  ggplot(aes(x, y, size = z)) +
  geom_point(show.legend = FALSE) + 
  theme_void() + 
  coord_equal()
smol_simplex |>
  ggplot(aes(x, y, z = z)) +
  geom_contour_filled(show.legend = FALSE, bins = 10) + 
  theme_void() + 
  coord_equal()

eps <- .001
smol_curl <- smol_grid |> mutate(
  x_add = gen_simplex(x + eps, y, seed = 1, frequency = .1),
  x_sub = gen_simplex(x - eps, y, seed = 1, frequency = .1),
  y_add = gen_simplex(x, y + eps, seed = 1, frequency = .1),
  y_sub = gen_simplex(x, y - eps, seed = 1, frequency = .1),
  x_slope = (x_add - x_sub) / (2 * eps), 
  y_slope = (y_add - y_sub) / (2 * eps),
  x_curl = -y_slope, 
  y_curl = x_slope
)

ggplot(smol_curl) + 
  geom_segment(
    mapping = aes(
      x = x, 
      y = y, 
      xend = x + x_slope * 2, 
      yend = y + y_slope * 2
    ), 
    arrow = arrow(length = unit(0.1, "cm"))
  ) + 
  theme_void() + 
  coord_equal()

ggplot(smol_curl) + 
  geom_segment(
    mapping = aes(
      x = x, 
      y = y, 
      xend = x + x_curl * 2, 
      yend = y + y_curl * 2
    ), 
    arrow = arrow(length = unit(0.1, "cm"))
  ) + 
  theme_void() + 
  coord_equal()

curl <- curl_noise(
  generator = gen_simplex,
  seed = 1,
  frequency = .1,
  x = smol_grid$x, 
  y = smol_grid$y
)

as_tibble(curl)

smol_grid |>
  mutate(
    x2 = x + curl$x * 2,
    y2 = y + curl$y * 2
  ) |> 
  ggplot() + 
  geom_segment(
    mapping = aes(x, y, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.1, "cm"))
  ) + 
  theme_void() + 
  coord_equal()

update_curl <- function(current_state, step_size = .0005, ...) {
  curl <- curl_noise(
    x = current_state$x, 
    y = current_state$y,
    ...
  )
  next_state <- current_state |>
    mutate(
      x = x + curl$x * step_size,
      y = y + curl$y * step_size,
      time = time + 1
    )
  return(next_state)
}

coords <- seq(0, 1, length.out = 50)
time_1 <- long_grid(x = coords, y = coords) |> 
  mutate(id = row_number(), time = 1)
time_1

curl_data <- function(
    data, 
    iterations = 50,
    step_size = .001,
    ...
) {
  
  update <- function(current_state, iteration, ...) {
    curl <- curl_noise(
      x = current_state$x, 
      y = current_state$y,
      generator = fracture,
      ...
    )
    next_state <- current_state |>
      mutate(
        x = x + curl$x * step_size,
        y = y + curl$y * step_size,
        time = time + 1
      )
    return(next_state)
  }
  
  data |> 
    mutate(id = row_number(), time = 1) |>
    accumulate(1:iterations, update, .init = _, ...) |>
    bind_rows()
}

curl_art <- function(...) {
  curl_data(...) |> 
    ggplot(aes(x, y, group = id)) + 
    geom_path() +
    theme_void() + 
    coord_equal() 
}

smol_grid |>
  mutate(x = normalise(x), y = normalise(y)) |>
  curl_art(noise = gen_simplex, fractal = fbm, octaves = 4, freq_init = .5)

circle <- function(n = 100) {
  tibble(
    theta = 2 * pi * (1:n) / n, 
    x = cos(theta),
    y = sin(theta)
  )
}

curl_circle <- function(octaves) {
  curl_art(
    data = circle(500),
    iterations = 100, 
    noise = gen_simplex,
    fractal = fbm,
    octaves = octaves, 
    seed = 1, 
    freq_init = 1,
    frequency = ~ . * 1.2,
    gain_init = 1,
    gain = ~ . * .9,
    step_size = .003
  )
}

curl_circle(octaves = 1)
curl_circle(octaves = 3)
curl_circle(octaves = 8)

custom_curl_data <- function(data) {
  curl_data(
    data = data,
    iterations = 80, 
    octaves = 10,
    fractal = ridged,
    noise = gen_cubic,
    freq_init = 1,
    frequency = ~ . * 1.2,
    gain_init = 1,
    gain = ~ . * .9,
    seed = 1
  )
}

dat1 <- circle(4000) |> 
  custom_curl_data()

dat2 <- circle(5000) |>
  mutate(x = x * .99, y = y * .99) |>
  custom_curl_data()

ggplot(mapping = aes(x, y, group = time)) +
  geom_polygon(data = dat1, fill = "white", alpha = .02) +
  geom_polygon(data = dat2, fill = "black", alpha = .02) +
  theme_void() + 
  coord_equal()


draw_art <- function(df, palette = NULL) {
  ggplot(df, aes(x, y, group = group, fill = factor(group))) +
    geom_polygon() +
    scale_fill_manual(values = palette) +
    theme_void()
}


splatter <- function(seed = 1, palette = NULL) {
  set.seed(seed)
  grid <- ambient::long_grid(x = seq(0, 1, length.out = 1000), y = seq(0, 1, length.out = 1000))
  grid$noise <- ambient::gen_perlin(grid$x, grid$y)
  grid$angle <- grid$noise * 2 * pi
  grid$xend <- grid$x + cos(grid$angle) * 0.01
  grid$yend <- grid$y + sin(grid$angle) * 0.01
  ggplot(grid) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = noise)) +
    scale_color_gradientn(colors = palette) +
    theme_void()
}


mosaica <- function(seed = 1, palette = NULL) {
  set.seed(seed)
  n <- 100
  df <- data.frame(
    x = rep(1:n, each = n),
    y = rep(1:n, times = n),
    fill = sample(palette, n * n, replace = TRUE)
  )
  ggplot(df, aes(x, y, fill = fill)) +
    geom_tile() +
    scale_fill_identity() +
    theme_void()
}
