#!/usr/bin/env Rscript
# =============================================================================
# USNews_Rankings_Scraper.R — Scrape and parse US News 2022 Statistics rankings
#
# Automates the full pipeline:
#   1. Initializes the USNews-Scrapper Git submodule (if needed)
#   2. Sets up a Python virtual environment and installs dependencies
#   3. Runs the Python scraper to download rankings JSON
#   4. Parses the JSON output to extract department rankings
#
# Uses the USNews-Scrapper tool (https://github.com/OvroAbir/USNews-Scrapper),
# included as a Git submodule at USNews-Scrapper/.
#
# Prerequisites: Python 3.8+, Git
#
# Output:
#   usnews_statistics.csv — ranked list of statistics departments with
#     columns: rank, university, city, state, peer_assessment_score
# =============================================================================

# Set working directory to this script's location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(jsonlite)

# =============================================================================
# STEP 1: Initialize submodule if needed
# =============================================================================
if (!dir.exists("USNews-Scrapper/usnews_scrapper")) {
  cat("Initializing USNews-Scrapper submodule...\n")
  status <- system("cd ../.. && git submodule update --init code/department_generator/USNews-Scrapper")
  if (status != 0) stop("Failed to initialize submodule. Ensure Git is installed.")
}

# =============================================================================
# STEP 2: Set up Python virtual environment and install dependencies
# =============================================================================
venv_dir <- "USNews-Scrapper/.venv"
venv_python <- file.path(venv_dir, "bin", "python")

if (!file.exists(venv_python)) {
  cat("Creating Python virtual environment...\n")
  status <- system(sprintf("python3 -m venv %s", venv_dir))
  if (status != 0) stop("Failed to create virtual environment. Ensure Python 3 is installed.")

  cat("Installing Python dependencies...\n")
  pip <- file.path(venv_dir, "bin", "pip")
  system(sprintf("%s install -r USNews-Scrapper/requirements.txt", pip))
  system(sprintf("%s install --upgrade requests urllib3 chardet idna", pip))
}

# =============================================================================
# STEP 3: Run the Python scraper
# =============================================================================
temp_dir <- "USNews-Scrapper/usnews_scrapper/temp"

if (!dir.exists(temp_dir) || length(list.files(temp_dir, pattern = "\\.txt$")) == 0) {
  cat("Running US News scraper...\n")
  abs_activate <- normalizePath(file.path(venv_dir, "bin", "activate"))
  scraper_cmd <- sprintf(
    'source "%s" && cd USNews-Scrapper/usnews_scrapper && python usnews_scrapper.py %s',
    abs_activate,
    '--url="https://www.usnews.com/best-graduate-schools/top-science-schools/statistics-rankings" -o statistics_rankings -p 2'
  )
  system(scraper_cmd)
  # The scraper may exit with an error during its own tablib export step,
  # but the raw JSON files we need are saved to temp/ before that.
  n_files <- length(list.files(temp_dir, pattern = "\\.txt$"))
  if (n_files == 0) stop("Scraper failed: no JSON files found in ", temp_dir)
  cat(sprintf("Scraper downloaded %d JSON files.\n", n_files))
} else {
  cat("Scraped JSON files already exist in", temp_dir, "— skipping scraper.\n")
}

# =============================================================================
# STEP 4: Parse scraped JSON into rankings CSV
# =============================================================================
cat("\nParsing scraped JSON files...\n")
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
