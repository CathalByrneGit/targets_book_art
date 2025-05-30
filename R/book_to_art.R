

isbn_to_seed <- function(isbn) isbn %% (2^31 - 1)

title_to_canva_palette <- function(title, palettes = ggthemes::canva_palettes) {
  # Get the names of the available palettes
  palette_names <- names(palettes)
  
  # Compute the distance between the title and each palette name
  distances <- stringdist(tolower(title), tolower(palette_names), method = "jw")  # Jaro-Winkler distance
  
  # Find the closest match
  best_match_index <- which.min(distances)
  best_match_name <- palette_names[best_match_index]
  
  # Return the corresponding palette
  palettes[[best_match_name]]
}


convert_book_meta_to_art <- function(book_data){
  # Extract book data
  title <- book_data$title
  isbn <- book_data$isbn%>%as.numeric()
  pages <- book_data$pages
  
  
  # title <- 'The Great Gatsby'
  # isbn <- 5
  # pages <- 100
  # Convert variables
  
  seed <- isbn_to_seed(isbn)
  
  palette <- title_to_canva_palette(title)
  
  octaves <- pages %% 20 +1
  
  

  # Generate art
  set.seed(seed)
  
  generate_fractal_art(
    pixels = 600,
    palette = palette,
    octaves = octaves,
  ) +
    ggplot2::ggtitle(paste(title,'-',book_data$author,sep = ' '))+
    theme(
      plot.title=element_text( hjust=0.5 )
    )
  
}