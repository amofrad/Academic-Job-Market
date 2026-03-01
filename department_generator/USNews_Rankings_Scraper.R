#!/usr/bin/env Rscript
# =============================================================================
# USNews_Rankings_Scraper.R — Parse US News 2022 Statistics department rankings
#
# Parses JSON output from the USNews-Scrapper tool
# (https://github.com/OvroAbir/USNews-Scrapper) to extract department rankings,
# peer assessment scores, and locations.
#
# Prerequisites:
#   1. Clone https://github.com/OvroAbir/USNews-Scrapper
#   2. Run the Python scraper targeting the statistics rankings URL:
#      python usnews_scrapper.py \
#        --url="https://www.usnews.com/best-graduate-schools/top-science-schools/statistics-rankings" \
#        -o statistics_rankings -p 2
#   3. JSON files will be saved in USNews-Scrapper/usnews_scrapper/temp/
#
# Output:
#   usnews_statistics.csv — ranked list of statistics departments with
#     columns: rank, university, city, state, peer_assessment_score
# =============================================================================
library(tidyverse)
library(jsonlite)

temp_dir <- "USNews-Scrapper/usnews_scrapper/temp"

all_schools <- list()

for (i in 1:11) {
  filename <- file.path(temp_dir, sprintf("%03d.txt", i))
  cat("Parsing", basename(filename), "...\n")

  json_data <- fromJSON(filename)

  # Extract ranked school items
  if ("data" %in% names(json_data) && "items" %in% names(json_data$data)) {
    items <- json_data$data$items

    for (j in 1:nrow(items)) {
      item <- items[j, ]
      tryCatch({
        rank  <- as.integer(item$ranking$display_rank)
        school <- item$name
        city   <- item$city
        state  <- item$state
        score  <- NA
        if ("schoolData" %in% names(item) && !is.null(item$schoolData)) {
          if ("c_avg_acad_rep_score" %in% names(item$schoolData)) {
            score <- as.numeric(item$schoolData$c_avg_acad_rep_score)
          }
        }
        all_schools[[length(all_schools) + 1]] <- tibble(
          rank = rank, university = school, city = city,
          state = state, peer_assessment_score = score
        )
      }, error = function(e) {
        # Skip problematic entries
      })
    }
  }

  # Also extract locked (paywall) items
  if ("data" %in% names(json_data) && "itemsLocked" %in% names(json_data$data)) {
    locked_items <- json_data$data$itemsLocked
    if (!is.null(locked_items) && length(locked_items) > 0) {
      for (j in 1:length(locked_items)) {
        item <- locked_items[[j]]
        tryCatch({
          rank   <- as.integer(item$ranking$display_rank)
          school <- item$name
          city   <- item$city
          state  <- item$state
          score  <- NA
          if ("schoolData" %in% names(item) && !is.null(item$schoolData)) {
            if ("c_avg_acad_rep_score" %in% names(item$schoolData)) {
              score <- as.numeric(item$schoolData$c_avg_acad_rep_score)
            }
          }
          all_schools[[length(all_schools) + 1]] <- tibble(
            rank = rank, university = school, city = city,
            state = state, peer_assessment_score = score
          )
        }, error = function(e) {
          # Skip problematic entries
        })
      }
    }
  }

  cat("  Found", length(all_schools), "schools so far\n")
}

rankings <- bind_rows(all_schools) %>%
  distinct(rank, university, .keep_all = TRUE) %>%
  arrange(rank)

cat("\nSuccess! Total schools:", nrow(rankings), "\n\n")
print(rankings)

write_csv(rankings, "usnews_statistics.csv")
