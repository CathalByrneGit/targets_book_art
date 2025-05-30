#############################################################
## Description: - Example of targets package
##              - Take in csv of books
##              - call book api to get book meta data
##              - Use meta data to create art
##              - Make pdf and rmd report
##              
##
## Author: Cathal Byrne
## Email: Cathal.Byrne@cso.ie
## Date: 2025-05-27
#############################################################

library(targets) # Necessary
library(tarchetypes)# Additional target features


# Source the functions and code which is used in the workflow
tar_source("R")

# set options
tar_option_set(packages = c("tibble", "httr", "jsonlite", "dplyr","ggthemes",
                            "ambient","ggplot2","tibble","purrr","dplyr",'stringdist',
                            'DT') ## 
               )
## List which outlines the plan i.e a recipe
list(
  # Input CSV file
  tar_target(name = books_file, "books_read.csv", format = "file"),
  
  # Read csv
  tar_target(name = books_df,
             command =  readr::read_csv(books_file, show_col_types = FALSE)),
  
  # Split csv by rows for dynamic branching
  tar_target(
    name = book_row,
    command = split(books_df, seq_len(nrow(books_df))),
    iteration = "list"
  ),
  
  # Fetch book meta data for each book i.e row
  tar_target(
    name = book_meta_data,
    command = fetch_book_details(book_row$Title, book_row$Author),
    pattern = map(book_row) 
    ),
  
  # Use each books meta data to create generative art
  tar_target(
   name =  book_meta_art,
    command = convert_book_meta_to_art(book_meta_data),
    pattern = map(book_meta_data),
    iteration = 'list'),
  
  # New target to combine plots
  ## Notice that you can write a function within the command parameter
  tar_target(
    name = combined_plot,
    command = {
      # Set up a multi-panel layout
      par(mfrow = c(ceiling(sqrt(length(book_meta_art))), 
                    ceiling(sqrt(length(book_meta_art)))))
      
      # Loop through each plot and display it
      for (plot_obj in book_meta_art) {
        plot(plot_obj)
      }
    }
  ),
  
  # A Report which updates based on changes
  tar_render(report, "report.Rmd")
   
)
