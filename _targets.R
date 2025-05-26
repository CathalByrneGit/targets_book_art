# _targets.R (excerpt)
library(targets)
library(tarchetypes)
# library(tibble)
# library(httr)
# library(jsonlite)
# library(digest) # For string hashing

# Source custom functions
tar_source("R")

tar_option_set(packages = c("tibble", "httr", "jsonlite", "dplyr"))#, "digest", "aRtsy"

list(
  # Input CSV file
  tar_target(books_file, "books_read.csv", format = "file"),
  
  tar_target(books_df, readr::read_csv(books_file, show_col_types = FALSE)),
  
  
  tar_target(
    book_row,
    split(books_df, seq_len(nrow(books_df))),
    iteration = "list"
  ),
  
  tar_target(
    book_meta_data,
    fetch_book_details(book_row$Title, book_row$Author),
    pattern = map(book_row) 
    )
  
   
)
