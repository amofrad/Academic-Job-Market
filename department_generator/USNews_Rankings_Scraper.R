library(tidyverse)
library(jsonlite)

# Using: https://github.com/OvroAbir/USNews-Scrapper/tree/master

# Path to temp files

temp_dir <- "USNews-Scrapper/usnews_scrapper/temp"

# Parse all JSON files
all_schools <- list()

for(i in 1:11) {
  filename <- file.path(temp_dir, sprintf("%03d.txt", i))
  cat("Parsing", basename(filename), "...\n")
  
  # Read and parse JSON
  json_data <- fromJSON(filename)
  
  # Extract school items
  if("data" %in% names(json_data) && "items" %in% names(json_data$data)) {
    items <- json_data$data$items
    
    for(j in 1:nrow(items)) {
      item <- items[j, ]
      
      tryCatch({
        # Extract rank
        rank <- as.integer(item$ranking$display_rank)
        
        # Extract school name
        school <- item$name
        
        # Extract location
        city <- item$city
        state <- item$state
        
        # Extract score (optional)
        score <- NA
        if("schoolData" %in% names(item) && !is.null(item$schoolData)) {
          if("c_avg_acad_rep_score" %in% names(item$schoolData)) {
            score <- as.numeric(item$schoolData$c_avg_acad_rep_score)
          }
        }
        
        all_schools[[length(all_schools) + 1]] <- tibble(
          rank = rank,
          university = school,
          city = city,
          state = state,
          peer_assessment_score = score
        )
        
      }, error = function(e) {
        # Skip problematic entries
      })
    }
  }
  
  # Also check for locked items
  if("data" %in% names(json_data) && "itemsLocked" %in% names(json_data$data)) {
    locked_items <- json_data$data$itemsLocked
    
    if(!is.null(locked_items) && length(locked_items) > 0) {
      for(j in 1:length(locked_items)) {
        item <- locked_items[[j]]
        
        tryCatch({
          rank <- as.integer(item$ranking$display_rank)
          school <- item$name
          city <- item$city
          state <- item$state
          score <- NA
          
          if("schoolData" %in% names(item) && !is.null(item$schoolData)) {
            if("c_avg_acad_rep_score" %in% names(item$schoolData)) {
              score <- as.numeric(item$schoolData$c_avg_acad_rep_score)
            }
          }
          
          all_schools[[length(all_schools) + 1]] <- tibble(
            rank = rank,
            university = school,
            city = city,
            state = state,
            peer_assessment_score = score
          )
          
        }, error = function(e) {
          # Skip problematic entries
        })
      }
    }
  }
  
  cat("  Found", length(all_schools), "schools so far\n")
}

# Convert to dataframe
rankings <- bind_rows(all_schools) %>%
  distinct(rank, university, .keep_all = TRUE) %>%
  arrange(rank)

cat("\nSuccess! Total schools:", nrow(rankings), "\n\n")

# Display the data
print(rankings)

write_csv(rankings, "usnews_statistics.csv")
