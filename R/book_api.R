# library(httr)
# library(jsonlite)
# library(tibble)

# Helper function to safely extract a scalar value
safe_extract_scalar <- function(data, field_name) {
  if (!is.null(data[[field_name]]) && length(data[[field_name]]) > 0) {
    # Check if it's a list that needs unlisting
    if (is.list(data[[field_name]])) {
      val <- unlist(data[[field_name]])
      if (length(val) > 0) {
        return(as.character(val[1])) # Take the first if multiple values
      }
    } else {
      return(as.character(data[[field_name]][1]))
    }
  }
  return(NA_character_)
}

# Helper function to safely extract and collapse a vector
safe_extract_vector_collapse <- function(data, field_name) {
  if (!is.null(data[[field_name]]) && length(data[[field_name]]) > 0) {
    # Ensure it's a list or a vector before collapsing
    if (is.list(data[[field_name]]) && length(data[[field_name]][[1]]) > 0) {
      return(paste(unlist(data[[field_name]]), collapse = ", "))
    } else if (is.atomic(data[[field_name]]) && length(data[[field_name]]) > 0) {
      return(paste(data[[field_name]], collapse = ", "))
    }
  }
  return(NA_character_)
}


fetch_book_details <- function(book_title, book_author) {
  query_title <- URLencode(book_title)
  query_author <- URLencode(book_author)
  
  api_url <- paste0(
    "https://openlibrary.org/search.json?q=", query_title,
    "&author=", query_author,
    "&fields=title,author_name,subject,isbn,first_sentence,place,time,number_of_pages_median,first_publish_year&limit=1"
  )
  
  response <- GET(api_url)
  
  if (status_code(response) == 200) {
    api_data <- fromJSON(content(response, as = "text"), flatten = TRUE)
    
    if (length(api_data$docs) > 0) {
      doc <- api_data$docs[1, ]
      
      # Ensure all fields are atomic vectors of length 1, or NA
      title <- safe_extract_scalar(doc, "title")
      author <- safe_extract_vector_collapse(doc, "author_name")
      subjects <- safe_extract_vector_collapse(doc, "subject")
      isbn <- safe_extract_scalar(doc, "isbn") # ISBN might be a vector of strings, taking first
      first_sentence <- safe_extract_scalar(doc, "first_sentence")
      place <- safe_extract_vector_collapse(doc, "place")
      time <- safe_extract_vector_collapse(doc, "time")
      publish_year <- safe_extract_scalar(doc, "first_publish_year")
      pages <- safe_extract_scalar(doc, "number_of_pages_median")
      
      # Explicitly convert numeric types where expected
      pages <- as.numeric(pages)
      publish_year <- as.integer(publish_year)
      
      return(tibble(
        title = title,
        author = author,
        subjects = subjects,
        isbn = isbn,
        first_sentence = first_sentence,
        place = place,
        time = time,
        publish_year = publish_year,
        pages = pages
      ))
    }
  }
  
  # Fallback in case of API failure or no result
  warning(paste("No data found or API call failed for", book_title))
  return(tibble(
    title = book_title,
    author = book_author,
    subjects = NA_character_,
    isbn = NA_character_,
    first_sentence = NA_character_,
    place = NA_character_,
    time = NA_character_,
    publish_year = NA_integer_,
    pages = NA_real_ # Use NA_real_ for numeric
  ))
}
