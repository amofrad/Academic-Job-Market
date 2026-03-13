#!/usr/bin/env Rscript
# =============================================================================
# Department_Generator.R — Construct department dataset for simulation
#
# Builds a complete department dataset by:
#   1. Loading US News 2022 statistics department rankings (usnews_statistics.csv)
#   2. Matching departments to US College Scorecard data
#   3. Assigning geographic attributes (setting, region, airport, cost of living)
#   4. Estimating compensation, teaching, and research environment attributes
#   5. Creating "hidden gem" departments (Tier 3/4 with standout features)
#
# Input:
#   usnews_statistics.csv              — from USNews_Rankings_Scraper.R
#   supporting_data/cost_of_living_us.csv
#   supporting_data/salary_data.csv
#
# Output:
#   ../../data/departments_dataset.csv — 101 departments with all questionnaire attributes
#
# Required packages: dplyr, tidyverse, collegeScorecard, stringdist
# =============================================================================

# Set working directory to this script's location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(dplyr)
library(tidyverse)
library(collegeScorecard)
library(stringdist)

# Load source data
data(school)
data(scorecard)

departments <- read_csv("usnews_statistics.csv")

# Fix missing city entries
departments <- departments %>%
  mutate(
    city = case_when(
      university == "Boston University"        ~ "Boston",
      university == "University of Nebraska"   ~ "Lincoln",
      university == "University of Alabama"    ~ "Tuscaloosa",
      university == "Portland State University" ~ "Portland",
      TRUE ~ city
    )
  )

#  Tier classification by rank 
set.seed(1)
departments <- departments %>%
  mutate(
    tier = case_when(
      rank <= 10  ~ "Tier 1",   # Top 10
      rank <= 25  ~ "Tier 2",   # 11–25
      rank <= 50  ~ "Tier 3",   # 26–50
      TRUE        ~ "Tier 4"    # Rest
    )
  )

table(departments$tier)

# Initialize all questionnaire variables
departments <- departments %>%
  mutate(
    # Section A: Geographic and Location Preferences
    q1_geographic_setting = NA_character_,
    q2_region = NA_character_,
    q3_airport_proximity = NA_character_,
    q4_cost_of_living = NA_real_,
    q5_dual_career = NA_character_,
    
    # Section B: Compensation and Resources
    q6_typical_salary_range = NA_real_,
    q7_typical_startup = NA_character_,
    q8_guaranteed_summer = NA_character_,
    
    # Section C: Teaching and Mentoring
    q9_typical_teaching_load = NA_character_,
    q10_course_types = NA_character_,
    q11_mentoring_program = NA_character_,
    
    # Section D: Research Environment
    q12_research_culture = NA_character_,
    q13_publication_venues = NA_character_,
    q14_phd_student_ratio = NA_real_,
    q15_medical_school_proximity = NA_integer_,
    
    # Additional info
    department_size_faculty = NA_integer_,
    department_size_phd_students = NA_integer_
  )

# Geographic data (Q1-Q4)

# Q1: Geographic Setting
city_setting <- tribble(
  ~city, ~state, ~q1_geographic_setting,
  # Major metropolitan (>1M)
  "Stanford", "CA", "A", "Berkeley", "CA", "A", "Chicago", "IL", "A",
  "New York", "NY", "A", "Philadelphia", "PA", "A", "Seattle", "WA", "A",
  "Los Angeles", "CA", "A", "Houston", "TX", "A", "Washington", "DC", "A",
  "Dallas", "TX", "A", "La Jolla", "CA", "A", "Irvine", "CA", "A",
  "Riverside", "CA", "A", "Tempe", "AZ", "A", "St. Louis", "MO", "A",
  "Denver", "CO", "A", "Charlotte", "NC", "A", "San Antonio", "TX", "A",
  "Tampa", "FL", "A",
  # Mid-sized city (250K-1M)
  "Cambridge", "MA", "B", "Pittsburgh", "PA", "B", "Ann Arbor", "MI", "B",
  "Minneapolis", "MN", "B", "Madison", "WI", "B", "Columbus", "OH", "B",
  "Austin", "TX", "B", "Raleigh", "NC", "B", "Durham", "NC", "B",
  "Tucson", "AZ", "B", "Baltimore", "MD", "B", "Cleveland", "OH", "B",
  "Cincinnati", "OH", "B", "Milwaukee", "WI", "B", "Albuquerque", "NM", "B",
  "Orlando", "FL", "B", "Reno", "NV", "B", "Boston", "MA", "B",
  # Small city/college town (50K-250K)
  "Chapel Hill", "NC", "C", "Ithaca", "NY", "C", "College Station", "TX", "C",
  "Davis", "CA", "C", "New Haven", "CT", "C", "Ames", "IA", "C",
  "University Park", "PA", "C", "West Lafayette", "IN", "C", "Champaign", "IL", "C",
  "Piscataway", "NJ", "C", "Gainesville", "FL", "C", "Fort Collins", "CO", "C",
  "Tallahassee", "FL", "C", "East Lansing", "MI", "C", "Storrs", "CT", "C",
  "Iowa City", "IA", "C", "Athens", "GA", "C", "Evanston", "IL", "C",
  "Columbia", "MO", "C", "Columbia", "SC", "C", "Blacksburg", "VA", "C",
  "Santa Barbara", "CA", "C", "Santa Cruz", "CA", "C", "Charlottesville", "VA", "C",
  "Rochester", "NY", "C", "Amherst", "MA", "C", "Fairfax", "VA", "C",
  "Bloomington", "IN", "C", "Corvallis", "OR", "C", "Richardson", "TX", "C",
  "Clemson", "SC", "C", "Lexington", "KY", "C", "Waco", "TX", "C",
  "Manhattan", "KS", "C", "Binghamton", "NY", "C", "Bowling Green", "OH", "C",
  "Bethlehem", "PA", "C", "Worcester", "MA", "C", "Logan", "UT", "C",
  "Bozeman", "MT", "C", "Newark", "NJ", "C", "DeKalb", "IL", "C",
  "Stillwater", "OK", "C", "Fayetteville", "AR", "C", "Greensboro", "NC", "C",
  "Toledo", "OH", "C", "Kalamazoo", "MI", "C", "Fargo", "ND", "C",
  "Rochester Hills", "MI", "C", "Greeley", "CO", "C", "Norfolk", "VA", "C",
  "Madison", "SD", "C", "Lincoln", "NE", "C", "Tuscaloosa", "AL", "C",
  "Portland", "OR", "B",
  # Rural/small town (<50K)
  "Notre Dame", "IN", "D", "Auburn", "AL", "D", "Pullman", "WA", "D",
  "Houghton", "MI", "D", "Mount Pleasant", "MI", "D"
)

# Q2: Region
state_region <- tribble(
  ~state, ~q2_region,
  "MA", "Northeast", "NY", "Northeast", "PA", "Northeast", "CT", "Northeast",
  "NJ", "Northeast", "MD", "Northeast", "DC", "Northeast",
  "NC", "Southeast", "VA", "Southeast", "FL", "Southeast", "GA", "Southeast",
  "SC", "Southeast", "KY", "Southeast", "AL", "Southeast",
  "IL", "Midwest", "MI", "Midwest", "OH", "Midwest", "IN", "Midwest",
  "WI", "Midwest", "MN", "Midwest", "IA", "Midwest", "MO", "Midwest",
  "KS", "Midwest", "NE", "Midwest", "SD", "Midwest", "ND", "Midwest",
  "TX", "Southwest", "OK", "Southwest", "AR", "Southwest", "NM", "Southwest", "AZ", "Southwest",
  "CA", "West Coast", "WA", "West Coast", "OR", "West Coast", "NV", "West Coast",
  "CO", "West Coast", "UT", "West Coast", "MT", "West Coast"
)

# Q3: Airport Proximity
airport_proximity <- tribble(
  ~city, ~state, ~q3_airport_proximity,
  # Very close (<1 hour) - A
  "Stanford", "CA", "A", "Berkeley", "CA", "A", "Cambridge", "MA", "A",
  "Chicago", "IL", "A", "Pittsburgh", "PA", "A", "New York", "NY", "A",
  "Philadelphia", "PA", "A", "Seattle", "WA", "A", "Minneapolis", "MN", "A",
  "Los Angeles", "CA", "A", "Austin", "TX", "A", "Houston", "TX", "A",
  "Washington", "DC", "A", "La Jolla", "CA", "A", "Irvine", "CA", "A",
  "Riverside", "CA", "A", "Tempe", "AZ", "A", "Columbus", "OH", "A",
  "Piscataway", "NJ", "A", "St. Louis", "MO", "A", "Denver", "CO", "A",
  "Dallas", "TX", "A", "Richardson", "TX", "A", "Charlotte", "NC", "A",
  "Milwaukee", "WI", "A", "San Antonio", "TX", "A", "Albuquerque", "NM", "A",
  "Orlando", "FL", "A", "Tampa", "FL", "A", "Cleveland", "OH", "A",
  "Cincinnati", "OH", "A", "Baltimore", "MD", "A", "Reno", "NV", "A",
  "Newark", "NJ", "A", "Boston", "MA", "A", "Portland", "OR", "A",
  # Moderately close (1-2 hours) - B
  "Durham", "NC", "B", "Raleigh", "NC", "B", "Ann Arbor", "MI", "B",
  "Madison", "WI", "B", "New Haven", "CT", "B", "Chapel Hill", "NC", "B",
  "Evanston", "IL", "B", "Davis", "CA", "B", "Gainesville", "FL", "B",
  "Tallahassee", "FL", "B", "Santa Barbara", "CA", "B", "Santa Cruz", "CA", "B",
  "Rochester", "NY", "B", "Tucson", "AZ", "B", "Fort Collins", "CO", "B",
  "Fairfax", "VA", "B", "Lexington", "KY", "B", "Amherst", "MA", "B",
  "Worcester", "MA", "B", "Greensboro", "NC", "B", "Norfolk", "VA", "B",
  "Fargo", "ND", "B", "Rochester Hills", "MI", "B", "Binghamton", "NY", "B",
  "Bethlehem", "PA", "B", "DeKalb", "IL", "B", "Toledo", "OH", "B",
  "Lincoln", "NE", "B", "Tuscaloosa", "AL", "B",
  # Further away (2-3 hours) - C
  "Ithaca", "NY", "C", "College Station", "TX", "C", "Ames", "IA", "C",
  "University Park", "PA", "C", "West Lafayette", "IN", "C", "Champaign", "IL", "C",
  "East Lansing", "MI", "C", "Storrs", "CT", "C", "Iowa City", "IA", "C",
  "Athens", "GA", "C", "Columbia", "MO", "C", "Columbia", "SC", "C",
  "Blacksburg", "VA", "C", "Charlottesville", "VA", "C", "Bloomington", "IN", "C",
  "Corvallis", "OR", "C", "Clemson", "SC", "C", "Waco", "TX", "C",
  "Manhattan", "KS", "C", "Bowling Green", "OH", "C", "Logan", "UT", "C",
  "Bozeman", "MT", "C", "Stillwater", "OK", "C", "Fayetteville", "AR", "C",
  "Kalamazoo", "MI", "C", "Greeley", "CO", "C", "Madison", "SD", "C",
  "Mount Pleasant", "MI", "C",
  # Remote (>3 hours) - D
  "Notre Dame", "IN", "D", "Auburn", "AL", "D", "Pullman", "WA", "D",
  "Houghton", "MI", "D"
)

# Q4: Cost of Living
living_cost_mapper <- function(departments) {
  living_cost <- read_csv("supporting_data/cost_of_living_us.csv")
  
  county_costs <- living_cost %>%
    group_by(county, state) %>%
    summarise(County_Cost = mean(total_cost), .groups = "drop")
  
  city_county_mapping <- tribble(
    ~city, ~state, ~county,
    "Stanford", "CA", "Santa Clara County", "Berkeley", "CA", "Alameda County",
    "Davis", "CA", "Yolo County", "Los Angeles", "CA", "Los Angeles County",
    "La Jolla", "CA", "San Diego County", "Irvine", "CA", "Orange County",
    "Riverside", "CA", "Riverside County", "Santa Barbara", "CA", "Santa Barbara County",
    "Santa Cruz", "CA", "Santa Cruz County",
    "Cambridge", "MA", "Middlesex County", "Amherst", "MA", "Hampshire County",
    "Worcester", "MA", "Worcester County", "Boston", "MA", "Suffolk County",
    "New York", "NY", "New York County", "Ithaca", "NY", "Tompkins County",
    "Rochester", "NY", "Monroe County", "Binghamton", "NY", "Broome County",
    "Philadelphia", "PA", "Philadelphia County", "Pittsburgh", "PA", "Allegheny County",
    "University Park", "PA", "Centre County", "Bethlehem", "PA", "Northampton County",
    "Chicago", "IL", "Cook County", "Champaign", "IL", "Champaign County",
    "Evanston", "IL", "Cook County", "DeKalb", "IL", "DeKalb County",
    "Austin", "TX", "Travis County", "Houston", "TX", "Harris County",
    "Dallas", "TX", "Dallas County", "Richardson", "TX", "Dallas County",
    "San Antonio", "TX", "Bexar County", "College Station", "TX", "Brazos County",
    "Waco", "TX", "McLennan County",
    "Durham", "NC", "Durham County", "Raleigh", "NC", "Wake County",
    "Chapel Hill", "NC", "Orange County", "Charlotte", "NC", "Mecklenburg County",
    "Greensboro", "NC", "Guilford County",
    "Ann Arbor", "MI", "Washtenaw County", "East Lansing", "MI", "Ingham County",
    "Kalamazoo", "MI", "Kalamazoo County", "Mount Pleasant", "MI", "Isabella County",
    "Houghton", "MI", "Houghton County", "Rochester Hills", "MI", "Oakland County",
    "Seattle", "WA", "King County", "Pullman", "WA", "Whitman County",
    "Washington", "DC", "District of Columbia",
    "Denver", "CO", "Denver County", "Fort Collins", "CO", "Larimer County",
    "Greeley", "CO", "Weld County",
    "Minneapolis", "MN", "Hennepin County",
    "Madison", "WI", "Dane County", "Milwaukee", "WI", "Milwaukee County",
    "New Haven", "CT", "New Haven County", "Storrs", "CT", "Tolland County",
    "Ames", "IA", "Story County", "Iowa City", "IA", "Johnson County",
    "West Lafayette", "IN", "Tippecanoe County", "Bloomington", "IN", "Monroe County",
    "Notre Dame", "IN", "St. Joseph County",
    "Columbus", "OH", "Franklin County", "Cleveland", "OH", "Cuyahoga County",
    "Cincinnati", "OH", "Hamilton County", "Bowling Green", "OH", "Wood County",
    "Toledo", "OH", "Lucas County",
    "Piscataway", "NJ", "Middlesex County", "Newark", "NJ", "Essex County",
    "Gainesville", "FL", "Alachua County", "Tallahassee", "FL", "Leon County",
    "Tampa", "FL", "Hillsborough County", "Orlando", "FL", "Orange County",
    "St. Louis", "MO", "St. Louis County", "Columbia", "MO", "Boone County",
    "Athens", "GA", "Clarke County",
    "Blacksburg", "VA", "Montgomery County", "Charlottesville", "VA", "Albemarle County",
    "Fairfax", "VA", "Fairfax County", "Norfolk", "VA", "Norfolk city",
    "Baltimore", "MD", "Baltimore County",
    "Tempe", "AZ", "Maricopa County", "Tucson", "AZ", "Pima County",
    "Reno", "NV", "Washoe County",
    "Lexington", "KY", "Fayette County",
    "Columbia", "SC", "Richland County", "Clemson", "SC", "Pickens County",
    "Corvallis", "OR", "Benton County", "Portland", "OR", "Multnomah County",
    "Albuquerque", "NM", "Bernalillo County",
    "Manhattan", "KS", "Riley County",
    "Logan", "UT", "Cache County",
    "Bozeman", "MT", "Gallatin County",
    "Stillwater", "OK", "Payne County",
    "Fayetteville", "AR", "Washington County",
    "Fargo", "ND", "Cass County",
    "Madison", "SD", "Lake County",
    "Auburn", "AL", "Lee County", "Tuscaloosa", "AL", "Tuscaloosa County",
    "Lincoln", "NE", "Lancaster County"
  )
  
  cost_of_living_from_data <- city_county_mapping %>%
    left_join(county_costs, by = c("county", "state")) %>%
    dplyr::select(city, state, q4_cost_of_living = County_Cost)
  
  departments <- departments %>%
    left_join(cost_of_living_from_data, by = c("city", "state"))
  
  return(departments)
}

# Apply geographic data
departments <- departments %>%
  left_join(city_setting, by = c("city", "state")) %>%
  left_join(state_region, by = "state") %>%
  left_join(airport_proximity, by = c("city", "state"))

# Apply cost of living
departments <- living_cost_mapper(departments)

# Clean up duplicates properly
if(any(str_detect(names(departments), "\\.x$|\\.y$"))) {
  departments <- departments %>%
    dplyr::select(-ends_with(".x")) %>%
    rename_with(~str_remove(., "\\.y$"), ends_with(".y"))
}

# Check for any remaining duplicates
if(any(duplicated(names(departments)))) {
  cat("Warning: Duplicate columns still exist:\n")
  print(names(departments)[duplicated(names(departments))])
  
  # Remove duplicates by keeping only first occurrence
  departments <- departments[, !duplicated(names(departments))]
}
# Salary data (Q6)

salary_est <- function(departments) {
  salary_data <- read_lines("supporting_data/salary_data.csv")
  salary_data <- salary_data[5:55]
  
  salary_data <- tibble(salary_data = salary_data) %>%
    separate(salary_data, into = c("state", "salary"), sep = ",", extra = "merge", fill = "right") %>%
    mutate(
      salary = str_remove_all(salary, '["$,]'),
      salary = as.numeric(salary),
      state = str_trim(state),
      state = case_when(
        state == "District of Columbia" ~ "DC",
        state %in% state.name ~ state.abb[match(state, state.name)],
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(state, salary)
  
  departments <- departments %>%
    left_join(salary_data, by = "state") %>%
    mutate(
      q6_typical_salary_range = case_when(
        tier == "Tier 1" ~ salary * 1.5,
        tier == "Tier 2" ~ salary * 1.3,
        tier == "Tier 3" ~ salary * 1.0,
        tier == "Tier 4" ~ salary * 0.9,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(-salary)
  
  return(departments)
}

departments <- salary_est(departments)

# College Scorecard matching

# Prepare scorecard data
scorecard_recent <- scorecard %>%
  mutate(start_year = as.integer(substr(academic_year, 1, 4))) %>%
  group_by(id) %>%
  filter(start_year == max(start_year, na.rm = TRUE)) %>%
  ungroup()

university_data <- school %>%
  left_join(scorecard_recent, by = "id") %>%
  dplyr::select(
    id, name, city, state,
    control, deg_highest, locale_type, locale_size,
    n_undergrads,
    cost_tuition_in, cost_tuition_out, cost_avg,
    rate_admissions, rate_completion, score_sat_avg, amnt_earnings_med_10y
  )

# Matching function
match_university_fuzzy <- function(dept_name, dept_city, dept_state) {
  normalize_name <- function(name) {
    name %>%
      str_to_lower() %>%
      str_replace_all("--", "-") %>%
      str_replace_all(" at ", " ") %>%
      str_replace_all(" & ", " and ") %>%
      str_replace_all("the university of", "university of") %>%
      str_replace_all("[^a-z0-9 ]", "") %>%
      str_squish()
  }
  
  dept_clean <- normalize_name(dept_name)
  
  candidates <- university_data %>%
    filter(state == dept_state) %>%
    mutate(
      name_clean = normalize_name(name),
      exact_match = name_clean == dept_clean,
      contains_match = str_detect(name_clean, fixed(dept_clean)) | str_detect(dept_clean, fixed(name_clean)),
      str_distance = stringdist(name_clean, dept_clean, method = "jw")
    ) %>%
    mutate(
      match_score = case_when(
        exact_match ~ 100,
        contains_match & str_distance < 0.1 ~ 95,
        contains_match & str_distance < 0.2 ~ 90,
        str_distance < 0.15 ~ 85,
        str_distance < 0.25 ~ 75,
        TRUE ~ 0
      )
    ) %>%
    filter(match_score > 0) %>%
    arrange(desc(match_score), str_distance) %>%
    slice(1)
  
  if(nrow(candidates) > 0) {
    return(candidates)
  } else {
    return(NULL)
  }
}

# Manual mappings for tricky cases
manual_matches <- tribble(
  ~dept_name, ~school_name,
  "University of California--Berkeley", "University of California-Berkeley",
  "University of Michigan--Ann Arbor", "University of Michigan-Ann Arbor",
  "University of North Carolina--Chapel Hill", "University of North Carolina at Chapel Hill",
  "Texas A&M University--College Station", "Texas A & M University-College Station",
  "University of California--Davis", "University of California-Davis",
  "University of Minnesota--Twin Cities", "University of Minnesota-Twin Cities",
  "University of Wisconsin--Madison", "University of Wisconsin-Madison",
  "University of California--Los Angeles", "University of California-Los Angeles",
  "Purdue University--West Lafayette", "Purdue University-Main Campus",
  "University of Illinois--Urbana-Champaign", "University of Illinois Urbana-Champaign",
  "Rutgers University--New Brunswick", "Rutgers University-New Brunswick",
  "University of California--Irvine", "University of California-Irvine",
  "University of Texas--Austin", "The University of Texas at Austin",
  "University of Missouri--Columbia", "University of Missouri-Columbia",
  "Virginia Tech", "Virginia Polytechnic Institute and State University",
  "University of California--San Diego", "University of California-San Diego",
  "University of California--Riverside", "University of California-Riverside",
  "University of California--Santa Barbara", "University of California-Santa Barbara",
  "University of California--Santa Cruz", "University of California-Santa Cruz",
  "University of Illinois--Chicago", "University of Illinois Chicago",
  "University of Massachusetts--Amherst", "University of Massachusetts-Amherst",
  "Indiana University--Bloomington", "Indiana University-Bloomington",
  "SMU", "Southern Methodist University",
  "University of Maryland--Baltimore County", "University of Maryland-Baltimore County",
  "Washington University in St. Louis", "Washington University in St Louis",
  "University of Texas--Dallas", "The University of Texas at Dallas",
  "University of Colorado--Denver", "University of Colorado Denver/Anschutz Medical Campus",
  "University of North Carolina--Charlotte", "University of North Carolina at Charlotte",
  "University of Texas--San Antonio", "The University of Texas at San Antonio",
  "University of North Carolina--Greensboro", "University of North Carolina at Greensboro",
  "University of Nevada--Reno", "University of Nevada-Reno"
)

# Enhanced matching with manual mappings
match_university_enhanced <- function(dept_name, dept_city, dept_state) {
  manual_match <- manual_matches %>% filter(dept_name == !!dept_name)
  
  if(nrow(manual_match) > 0) {
    result <- university_data %>%
      filter(state == dept_state, name == manual_match$school_name[1])
    if(nrow(result) > 0) return(result %>% slice(1))
  }
  
  return(match_university_fuzzy(dept_name, dept_city, dept_state))
}

# Match all departments
departments_matched <- departments %>%
  rowwise() %>%
  mutate(matched_data = list(match_university_enhanced(university, city, state))) %>%
  ungroup()

# Extract matched data
departments_enriched <- departments_matched %>%
  mutate(
    has_match = map_lgl(matched_data, ~!is.null(.x) && nrow(.x) > 0),
    
    public_private = map_chr(matched_data, ~{
      if(!is.null(.x) && nrow(.x) > 0) as.character(.x$control) else NA_character_
    }),
    
    public_private = case_when(
      public_private == "Public" ~ "Public",
      public_private %in% c("Private nonprofit", "Private for-profit") ~ "Private",
      TRUE ~ NA_character_
    ),
    
    n_undergrads = map_dbl(matched_data, ~{
      if(!is.null(.x) && nrow(.x) > 0) .x$n_undergrads else NA_real_
    }),
    
    university_size = case_when(
      n_undergrads >= 30000 ~ "Very Large",
      n_undergrads >= 15000 ~ "Large",
      n_undergrads >= 5000 ~ "Medium",
      n_undergrads < 5000 ~ "Small",
      TRUE ~ NA_character_
    ),
    
    cost_avg = map_dbl(matched_data, ~{
      if(!is.null(.x) && nrow(.x) > 0) .x$cost_avg else NA_real_
    }),
    
    rate_admissions = map_dbl(matched_data, ~{
      if(!is.null(.x) && nrow(.x) > 0) .x$rate_admissions else NA_real_
    }),
    
    locale_type = map_chr(matched_data, ~{
      if(!is.null(.x) && nrow(.x) > 0) as.character(.x$locale_type) else NA_character_
    })
  ) %>%
  dplyr::select(-matched_data)

# Fill missing values with fallbacks
departments_enriched <- departments_enriched %>%
  mutate(
    public_private = if_else(
      is.na(public_private),
      if_else(str_detect(university, "^University of|State University$|State College$"), "Public", "Private"),
      public_private
    ),
    
    university_size = if_else(
      is.na(university_size),
      case_when(
        tier %in% c("Tier 1", "Tier 2") ~ "Large",
        tier == "Tier 3" ~ "Medium",
        TRUE ~ "Medium"
      ),
      university_size
    )
  )

# Medical school proximity (Q15)

universities_with_med_schools <- c(
  "Stanford University", "University of California--Berkeley", "Harvard University",
  "University of Chicago", "Columbia University", "Duke University",
  "University of Michigan--Ann Arbor", "University of Pennsylvania", "University of Washington",
  "University of North Carolina--Chapel Hill", "Cornell University",
  "Texas A&M University--College Station", "University of California--Davis",
  "University of Minnesota--Twin Cities", "University of Wisconsin--Madison",
  "Yale University", "Pennsylvania State University", "University of California--Los Angeles",
  "University of Illinois--Urbana-Champaign", "Ohio State University",
  "Rutgers University--New Brunswick", "University of Florida", "University of California--Irvine",
  "University of Texas--Austin", "Florida State University", "Michigan State University",
  "University of Connecticut", "University of Iowa", "University of Georgia",
  "University of Pittsburgh", "New York University", "Northwestern University",
  "University of Missouri--Columbia", "Virginia Tech", "Boston University",
  "George Washington University", "University of California--San Diego", "Temple University",
  "University of California--Riverside", "University of Virginia", "Arizona State University",
  "University of Rochester", "University of Illinois--Chicago", "University of Massachusetts--Amherst",
  "University of South Carolina", "Indiana University--Bloomington",
  "University of Arizona", "University of Maryland--Baltimore County", "Washington University in St. Louis",
  "Case Western Reserve University", "University of Cincinnati", "University of Kentucky",
  "Baylor University", "University of Nebraska", "University of Colorado--Denver",
  "University of North Carolina--Charlotte", "Binghamton University--SUNY", "Lehigh University",
  "University of Central Florida", "University of New Mexico", "Washington State University",
  "Auburn University", "University of Texas--San Antonio", "Utah State University",
  "Montana State University", "University of Alabama", "Marquette University",
  "Oklahoma State University", "University of Arkansas", "University of South Florida",
  "Central Michigan University", "Old Dominion University", "University of Nevada--Reno",
  "University of Toledo", "Western Michigan University", "Oakland University", "University of Northern Colorado"
)

departments_enriched <- departments_enriched %>%
  mutate(q15_medical_school_proximity = if_else(university %in% universities_with_med_schools, 1L, 0L))


# Estimate remaining questionnaire responses (Q5, Q7-Q14)

#  Q5: Dual Career Programs 
estimate_dual_career <- function(tier, q2_region, university_size, public_private, n_undergrads) {
  # Larger institutions more likely to have dual career programs
  # Particularly common at R1 publics in Midwest/Northeast
  
  if(!is.na(n_undergrads) && n_undergrads >= 20000) {
    if(tier %in% c("Tier 1", "Tier 2")) {
      return("Y")  # Top large schools almost always have them
    } else if(public_private == "Public" && q2_region %in% c("Midwest", "Northeast")) {
      return(sample(c("Y", "N"), 1, prob = c(0.75, 0.25)))
    } else {
      return(sample(c("Y", "N"), 1, prob = c(0.6, 0.4)))
    }
  }
  
  if(university_size %in% c("Large", "Very Large")) {
    if(tier %in% c("Tier 1", "Tier 2")) {
      return("Y")
    } else if(q2_region %in% c("Midwest", "Northeast") && public_private == "Public") {
      return(sample(c("Y", "N"), 1, prob = c(0.65, 0.35)))
    } else {
      return(sample(c("Y", "N"), 1, prob = c(0.5, 0.5)))
    }
  } else {
    return(sample(c("Y", "N"), 1, prob = c(0.25, 0.75)))
  }
}

#  Q7: Startup Funding 
estimate_startup <- function(tier, q1_geographic_setting, public_private, cost_avg, rate_admissions) {
  # Base on tier
  base_startup <- case_when(
    tier == "Tier 1" ~ 4.5,
    tier == "Tier 2" ~ 3.5,
    tier == "Tier 3" ~ 2.5,
    tier == "Tier 4" ~ 2.0
  )
  
  # Major metros have more resources
  if(!is.na(q1_geographic_setting) && q1_geographic_setting == "A") {
    base_startup <- base_startup * 1.2
  }
  
  # Wealthy institutions (high avg cost) have more startup
  if(!is.na(cost_avg) && cost_avg > 40000) {
    base_startup <- base_startup * 1.15
  } else if(!is.na(cost_avg) && cost_avg > 30000) {
    base_startup <- base_startup * 1.05
  }
  
  # Highly selective schools have more funding
  if(!is.na(rate_admissions) && rate_admissions < 0.3) {
    base_startup <- base_startup * 1.25
  } else if(!is.na(rate_admissions) && rate_admissions < 0.5) {
    base_startup <- base_startup * 1.1
  }
  
  # Private universities often have more startup funds
  if(!is.na(public_private) && public_private == "Private" && tier %in% c("Tier 1", "Tier 2")) {
    base_startup <- base_startup * 1.2
  }
  
  # Convert to categories
  case_when(
    base_startup >= 5.5 ~ "E",  # >$500k
    base_startup >= 4.0 ~ "D",  # $250-500k
    base_startup >= 3.0 ~ "C",  # $150-250k
    base_startup >= 2.0 ~ "B",  # $50-150k
    TRUE ~ "A"                   # <$50k
  )
}

#  Q8: Guaranteed Summer Support 
estimate_summer_support <- function(tier, university_size, n_undergrads, public_private) {
  # Top institutions provide more summer support
  tier_score <- case_when(
    tier == "Tier 1" ~ 4,
    tier == "Tier 2" ~ 3,
    tier == "Tier 3" ~ 2,
    tier == "Tier 4" ~ 1
  )
  
  # Larger universities have more resources
  size_boost <- 0
  if(!is.na(n_undergrads)) {
    if(n_undergrads >= 30000) size_boost <- 1
    else if(n_undergrads >= 15000) size_boost <- 0.5
  } else if(!is.na(university_size)) {
    if(university_size == "Very Large") size_boost <- 1
    else if(university_size == "Large") size_boost <- 0.5
  }
  
  # Private institutions often provide more support
  if(!is.na(public_private) && public_private == "Private" && tier %in% c("Tier 1", "Tier 2")) {
    tier_score <- tier_score + 0.5
  }
  
  total_score <- tier_score + size_boost
  
  case_when(
    total_score >= 5 ~ "A",  # 3+ years guaranteed
    total_score >= 3.5 ~ "B",  # 2 years guaranteed
    total_score >= 2 ~ "C",  # Some support
    TRUE ~ "D"               # Limited/no guaranteed support
  )
}

#  Q9: Teaching Load 
estimate_teaching_load <- function(tier, n_undergrads, university_size, public_private) {
  # Base teaching load by tier
  base_load <- case_when(
    tier == "Tier 1" ~ 2.0,
    tier == "Tier 2" ~ 2.3,
    tier == "Tier 3" ~ 2.8,
    tier == "Tier 4" ~ 3.3
  )
  
  # Very large universities often have higher teaching needs
  if(!is.na(n_undergrads) && n_undergrads > 35000) {
    base_load <- base_load + 0.6
  } else if(!is.na(n_undergrads) && n_undergrads > 25000) {
    base_load <- base_load + 0.4
  } else if(!is.na(university_size) && university_size == "Very Large") {
    base_load <- base_load + 0.5
  }
  
  # Private universities typically have lower teaching loads
  if(!is.na(public_private) && public_private == "Private") {
    base_load <- base_load - 0.4
  }
  
  # Convert to categories
  case_when(
    base_load <= 1.5 ~ "A",  # 0-1 course/year
    base_load <= 2.5 ~ "B",  # 2 courses/year
    base_load <= 3.5 ~ "C",  # 3 courses/year
    TRUE ~ "D"               # 4+ courses/year
  )
}

#  Q10: Course Types 
estimate_course_types <- function(tier, university_size, n_undergrads) {
  # Top research universities focus on graduate courses
  if(tier == "Tier 1") {
    return("A")  # Primarily graduate-level
  }
  
  if(tier == "Tier 2") {
    return("B")  # Mix of graduate and advanced undergrad
  }
  
  # Very large universities need lots of service courses
  if(!is.na(n_undergrads) && n_undergrads > 30000) {
    return("D")  # Flexible across all levels
  } else if(!is.na(university_size) && university_size == "Very Large") {
    return("D")
  }
  
  # Default for mid-tier schools
  if(tier == "Tier 3") {
    return("B")  # Mix of graduate and advanced undergrad
  }
  
  return("D")  # Tier 4: Flexible across all levels
}

#  Q11: Mentoring Program 
estimate_mentoring <- function(tier, university_size, public_private, n_undergrads) {
  # Top private institutions have best mentoring structures
  if(!is.na(public_private) && public_private == "Private" && tier == "Tier 1") {
    return("A")  # Very important (structured program)
  }
  
  # Large institutions at top tiers have formal programs
  if(tier %in% c("Tier 1", "Tier 2")) {
    if(!is.na(n_undergrads) && n_undergrads >= 15000) {
      return("A")
    } else if(!is.na(university_size) && university_size %in% c("Large", "Very Large")) {
      return("A")
    } else {
      return("B")  # Moderately important
    }
  }
  
  # Mid-tier publics often have some structure
  if(tier == "Tier 3") {
    if(!is.na(public_private) && public_private == "Public") {
      return("B")
    } else {
      return("C")  # Slightly important
    }
  }
  
  # Lower tier schools
  return("C")  # Slightly important (informal mentoring)
}

#  Q12: Research Culture 
research_culture_lookup <- tribble(
  ~university, ~q12_research_culture,
  "University of Chicago", "A",
  "Stanford University", "D",
  "University of California--Berkeley", "D",
  "Harvard University", "A",
  "Duke University", "C",
  "Boston University", "C",
  "University of Washington", "C",
  "University of North Carolina--Chapel Hill", "C",
  "Johns Hopkins University", "C",
  "Carnegie Mellon University", "B",
  "George Mason University", "B",
  "Worcester Polytechnic Institute", "B",
  "Michigan Technological University", "B",
  "University of Maryland--Baltimore County", "B",
  "Columbia University", "D",
  "University of Pennsylvania", "D",
  "University of Michigan--Ann Arbor", "D",
  "Northwestern University", "D",
  "University of Wisconsin--Madison", "D",
  "University of Minnesota--Twin Cities", "D"
)

infer_research_culture <- function(university, tier, q15_medical_school_proximity) {
  # Check lookup table first
  culture <- research_culture_lookup %>%
    filter(university == !!university) %>%
    pull(q12_research_culture)
  
  if(length(culture) > 0) return(culture)
  
  # Infer based on characteristics
  if(!is.na(q15_medical_school_proximity) && q15_medical_school_proximity == 1) {
    if(tier %in% c("Tier 1", "Tier 2", "Tier 3")) {
      return("C")  # Biostatistics focus
    }
  }
  
  # Check if department name suggests focus
  if(str_detect(university, "Tech|Polytechnic")) {
    return("B")  # Applied/data science
  }
  
  # Top tier without med school tends toward theory or balanced
  if(tier == "Tier 1") {
    return(sample(c("A", "D"), 1, prob = c(0.3, 0.7)))
  }
  
  # Default: balanced approach
  return("D")
}

#  Q13: Publication Venues 
estimate_publication_venues <- function(tier, q12_research_culture) {
  # Top theoretical departments
  if(tier == "Tier 1" && !is.na(q12_research_culture) && q12_research_culture == "A") {
    return("A")  # Top-4 journals
  }
  
  # Biostatistics programs
  if(!is.na(q12_research_culture) && q12_research_culture == "C") {
    return("B")  # Applied statistics journals
  }
  
  # Applied/data science focus
  if(!is.na(q12_research_culture) && q12_research_culture == "B") {
    if(tier %in% c("Tier 1", "Tier 2")) {
      return("C")  # ML venues
    } else {
      return("E")  # Mix
    }
  }
  
  # Top tier balanced programs
  if(tier == "Tier 1" && !is.na(q12_research_culture) && q12_research_culture == "D") {
    return(sample(c("A", "E"), 1, prob = c(0.4, 0.6)))
  }
  
  # Most departments have mixed publication profiles
  return("E")
}

#  Q14: PhD Student Ratio 
estimate_phd_ratio <- function(tier, university_size, n_undergrads, q15_medical_school_proximity) {
  # Base ratio by tier
  base_ratio <- case_when(
    tier == "Tier 1" ~ 3.0,
    tier == "Tier 2" ~ 2.5,
    tier == "Tier 3" ~ 2.0,
    tier == "Tier 4" ~ 1.5
  )
  
  # Larger universities tend to have more PhD students
  if(!is.na(n_undergrads)) {
    if(n_undergrads > 35000) {
      base_ratio <- base_ratio + 0.6
    } else if(n_undergrads > 25000) {
      base_ratio <- base_ratio + 0.4
    } else if(n_undergrads > 15000) {
      base_ratio <- base_ratio + 0.2
    }
  } else if(!is.na(university_size)) {
    if(university_size == "Very Large") {
      base_ratio <- base_ratio + 0.5
    } else if(university_size == "Large") {
      base_ratio <- base_ratio + 0.25
    }
  }
  
  # Biostatistics/medical programs often larger
  if(!is.na(q15_medical_school_proximity) && q15_medical_school_proximity == 1) {
    base_ratio <- base_ratio + 0.4
  }
  
  # Add realistic variation
  ratio <- base_ratio + rnorm(1, 0, 0.25)
  
  # Ensure reasonable bounds (0.5 to 5.0)
  return(max(0.5, min(5.0, ratio)))
}

# Apply all estimation functions

set.seed(2026)

departments_final <- departments_enriched %>%
  rowwise() %>%
  mutate(
    # Q5: Dual career programs
    q5_dual_career = estimate_dual_career(tier, q2_region, university_size, 
                                          public_private, n_undergrads),
    
    # Q7: Startup funding
    q7_typical_startup = estimate_startup(tier, q1_geographic_setting, 
                                          public_private, cost_avg, rate_admissions),
    
    # Q8: Summer support
    q8_guaranteed_summer = estimate_summer_support(tier, university_size, 
                                                   n_undergrads, public_private),
    
    # Q9: Teaching load
    q9_typical_teaching_load = estimate_teaching_load(tier, n_undergrads, 
                                                      university_size, public_private),
    
    # Q10: Course types
    q10_course_types = estimate_course_types(tier, university_size, n_undergrads),
    
    # Q11: Mentoring program
    q11_mentoring_program = estimate_mentoring(tier, university_size, 
                                               public_private, n_undergrads),
    
    # Q12: Research culture
    q12_research_culture = infer_research_culture(university, tier, q15_medical_school_proximity),
    
    # Q14: PhD student ratio
    q14_phd_student_ratio = estimate_phd_ratio(tier, university_size, 
                                               n_undergrads, q15_medical_school_proximity)
  ) %>%
  ungroup()

# Q13 requires Q12, so do it separately
departments_final <- departments_final %>%
  rowwise() %>%
  mutate(
    q13_publication_venues = estimate_publication_venues(tier, q12_research_culture)
  ) %>%
  ungroup()


departments_final <- departments_final %>%
  mutate(
    s_j = (peer_assessment_score - 1) / (5 - 1)
  )


# Create "hidden gem" departments
# Some Tier 3/4 departments have 1-2 attributes that rival Tier 1/2 departments
# This reflects real-world variation (e.g., a teaching school in a great city,
# or a lower-ranked program with unusually high salary due to cost of living)

create_hidden_gems <- function(departments_df, gem_rate = 0.25, seed = 2026) {
  set.seed(seed)
  
  # Identify Tier 3/4 departments eligible for "hidden gem" status
  eligible_idx <- which(departments_df$tier %in% c("Tier 3", "Tier 4"))
  n_gems <- ceiling(length(eligible_idx) * gem_rate)
  gem_idx <- sample(eligible_idx, n_gems)
  
  cat("\n=== CREATING HIDDEN GEM DEPARTMENTS ===\n")
  cat("Total Tier 3/4 departments:", length(eligible_idx), "\n")
  cat("Hidden gems to create:", n_gems, "\n\n")
  
  # Track what gems we create
  gem_log <- tibble(
    university = character(),
    tier = character(),
    gem_type = character(),
    original_value = character(),
    new_value = character()
  )
  
  for (idx in gem_idx) {
    # Each gem gets 1-2 standout attributes
    n_attributes <- sample(1:2, 1, prob = c(0.6, 0.4))
    
    # Possible gem types with weights
    gem_types <- c(
      "salary",           # Unusually high salary for tier
      "location",         # Urban location (A or B)
      "teaching_load",    # Light teaching load
      "startup",          # Good startup package
      "summer_support",   # Strong summer support
      "phd_ratio"         # Good PhD student ratio
    )
    
    # Weight toward attributes that matter most to candidates
    gem_weights <- c(0.25, 0.25, 0.20, 0.12, 0.10, 0.08)
    
    selected_gems <- sample(gem_types, n_attributes, prob = gem_weights, replace = FALSE)
    
    for (gem_type in selected_gems) {
      uni_name <- departments_df$university[idx]
      tier <- departments_df$tier[idx]
      
      if (gem_type == "salary") {
        # Boost salary to Tier 1/2 levels
        original <- departments_df$q6_typical_salary_range[idx]
        # Get base state salary and apply Tier 1/2 multiplier
        state_salary <- original / ifelse(tier == "Tier 3", 1.0, 0.9)
        multiplier <- runif(1, 1.25, 1.45)
        departments_df$q6_typical_salary_range[idx] <- state_salary * multiplier
        
        gem_log <- bind_rows(gem_log, tibble(
          university = uni_name, tier = tier, gem_type = "High Salary",
          original_value = sprintf("$%.0f", original),
          new_value = sprintf("$%.0f", departments_df$q6_typical_salary_range[idx])
        ))
      }
      
      if (gem_type == "location") {
        # Only upgrade if currently C or D
        original <- departments_df$q1_geographic_setting[idx]
        if (original %in% c("C", "D")) {
          new_setting <- sample(c("A", "B"), 1, prob = c(0.3, 0.7))
          departments_df$q1_geographic_setting[idx] <- new_setting
          
          gem_log <- bind_rows(gem_log, tibble(
            university = uni_name, tier = tier, gem_type = "Urban Location",
            original_value = original, new_value = new_setting
          ))
        }
      }
      
      if (gem_type == "teaching_load") {
        # Upgrade to B (2-2 load) - rare for Tier 3/4
        original <- departments_df$q9_typical_teaching_load[idx]
        if (original %in% c("C", "D")) {
          departments_df$q9_typical_teaching_load[idx] <- "B"
          
          gem_log <- bind_rows(gem_log, tibble(
            university = uni_name, tier = tier, gem_type = "Light Teaching (2-2)",
            original_value = original, new_value = "B"
          ))
        }
      }
      
      if (gem_type == "startup") {
        # Boost startup to C or D level
        original <- departments_df$q7_typical_startup[idx]
        if (original %in% c("A", "B")) {
          new_startup <- sample(c("C", "D"), 1, prob = c(0.7, 0.3))
          departments_df$q7_typical_startup[idx] <- new_startup
          
          gem_log <- bind_rows(gem_log, tibble(
            university = uni_name, tier = tier, gem_type = "Good Startup",
            original_value = original, new_value = new_startup
          ))
        }
      }
      
      if (gem_type == "summer_support") {
        # Upgrade to A or B
        original <- departments_df$q8_guaranteed_summer[idx]
        if (original %in% c("C", "D")) {
          new_summer <- sample(c("A", "B"), 1, prob = c(0.3, 0.7))
          departments_df$q8_guaranteed_summer[idx] <- new_summer
          
          gem_log <- bind_rows(gem_log, tibble(
            university = uni_name, tier = tier, gem_type = "Strong Summer Support",
            original_value = original, new_value = new_summer
          ))
        }
      }
      
      if (gem_type == "phd_ratio") {
        # Boost PhD ratio to Tier 1/2 levels
        original <- departments_df$q14_phd_student_ratio[idx]
        if (original < 2.5) {
          new_ratio <- runif(1, 2.5, 3.5)
          departments_df$q14_phd_student_ratio[idx] <- new_ratio
          
          gem_log <- bind_rows(gem_log, tibble(
            university = uni_name, tier = tier, gem_type = "High PhD Ratio",
            original_value = sprintf("%.2f", original),
            new_value = sprintf("%.2f", new_ratio)
          ))
        }
      }
    }
  }
  
  # Print summary
  cat("Hidden gems created:\n")
  print(gem_log %>% arrange(tier, university))
  
  cat("\nGem type distribution:\n")
  print(table(gem_log$gem_type))
  
  # Store gem info as attribute
  attr(departments_df, "hidden_gems") <- gem_log
  
  return(departments_df)
}

# Apply hidden gems

departments_final <- create_hidden_gems(departments_final, gem_rate = 0.25, seed = 2026)


# Remove intermediate columns and order cleanly
departments_final <- departments_final %>%
  dplyr::select(-any_of(c("has_match", "n_undergrads", "cost_avg", "rate_admissions",
                    "locale_type", "department_size_faculty",
                    "department_size_phd_students"))) %>%
  dplyr::select(rank, university, city, state, peer_assessment_score, tier, s_j,
                q1_geographic_setting, q2_region, q3_airport_proximity,
                q4_cost_of_living, q5_dual_career, q6_typical_salary_range,
                q7_typical_startup, q8_guaranteed_summer, q9_typical_teaching_load,
                q10_course_types, q11_mentoring_program, q12_research_culture,
                q13_publication_venues, q14_phd_student_ratio,
                q15_medical_school_proximity,
                everything())

#  Save final dataset
write_csv(departments_final, "../../data/departments_dataset.csv")
cat(sprintf("\nSaved ../departments_dataset.csv (%d departments)\n", nrow(departments_final)))
