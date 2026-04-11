#!/usr/bin/env Rscript
# =============================================================================
# USNews_Rankings_Scraper.R — Scrape and parse US News Statistics rankings
#
# Initializes the USNews-Scrapper Git submodule, sets up a Python venv, runs
# the scraper, and parses the JSON output to usnews_statistics.csv.
#
# Uses the USNews-Scrapper tool (https://github.com/OvroAbir/USNews-Scrapper),
# included as a Git submodule at USNews-Scrapper/.
#
# Prerequisites: Python 3.8+, Git
#
# Set force_refresh <- TRUE before sourcing to re-scrape instead of using
# cached JSON files.
#
# Output:
#   usnews_statistics.csv — ranked list of statistics departments with
#     columns: rank, university, city, state, peer_assessment_score
# =============================================================================

# Set working directory to this script's location
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[grep("--file=", file_arg)]
  if (length(file_arg) > 0) setwd(dirname(sub("--file=", "", file_arg)))
}

library(tidyverse)
library(jsonlite)

if (!exists("force_refresh")) force_refresh <- TRUE

# Initialize submodule if needed
if (!dir.exists("USNews-Scrapper/usnews_scrapper")) {
  status <- system("cd .. && git submodule update --init department_generator/USNews-Scrapper")
  if (status != 0) stop("Failed to initialize submodule. Ensure Git is installed.")
}

# Set up Python virtual environment
venv_dir <- "USNews-Scrapper/.venv"
venv_python <- file.path(venv_dir, "bin", "python")

# Detect a broken venv (e.g. stale absolute paths after the project moved)
if (file.exists(venv_python)) {
  if (system2(venv_python, "--version", stdout = FALSE, stderr = FALSE) != 0) {
    unlink(venv_dir, recursive = TRUE)
  }
}

if (!file.exists(venv_python)) {
  status <- system(sprintf("python3 -m venv %s", venv_dir))
  if (status != 0) stop("Failed to create virtual environment. Ensure Python 3 is installed.")
}

# Verify Python dependencies are installed (install if missing).
abs_python <- file.path(getwd(), venv_python)
deps_check <- system2(abs_python, c("-c", "'import requests, tablib'"),
                      stdout = FALSE, stderr = FALSE)
if (deps_check != 0) {
  cat("Installing Python dependencies...\n")
  pip <- file.path(venv_dir, "bin", "pip")
  status <- system(sprintf('"%s" install -r USNews-Scrapper/requirements.txt', pip))
  if (status != 0) stop("Failed to install Python dependencies from requirements.txt")
  system(sprintf('"%s" install --upgrade requests urllib3 chardet idna', pip))
}

# Run Python scraper
temp_dir <- "USNews-Scrapper/usnews_scrapper/temp"

if (force_refresh && dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

if (!dir.exists(temp_dir) || length(list.files(temp_dir, pattern = "\\.txt$")) == 0) {
  cat("Running US News scraper...\n")
  scraper_cmd <- sprintf(
    'cd USNews-Scrapper/usnews_scrapper && "%s" usnews_scrapper.py %s',
    abs_python,
    '--url="https://www.usnews.com/best-graduate-schools/top-science-schools/statistics-rankings" -o statistics_rankings -p 2'
  )
  system(scraper_cmd)
  # The scraper may exit with an error during its own tablib export step,
  # but raw JSON files are saved to temp/ before that.
  if (length(list.files(temp_dir, pattern = "\\.txt$")) == 0)
    stop("Scraper failed: no JSON files found in ", temp_dir)
}

# Parse scraped JSON into rankings CSV
all_schools <- list()
json_files <- sort(list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE))

for (filename in json_files) {
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
      }, error = function(e) NULL)
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
        }, error = function(e) NULL)
      }
    }
  }
}

rankings <- bind_rows(all_schools) %>%
  distinct(rank, university, .keep_all = TRUE) %>%
  arrange(rank)

write_csv(rankings, "usnews_statistics.csv")
