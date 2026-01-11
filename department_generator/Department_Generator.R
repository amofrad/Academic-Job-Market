library(dplyr)
library(tidyverse)
# Read Department Ranking Data
departments <- read_csv("Webscraping Tool/usnews_statistics.csv")


# Schools missing their respective city
departments <- departments %>%
  mutate(
    city = case_when(
      university == "Boston University"              ~ "Boston",
      university == "University of Nebraska"          ~ "Lincoln",
      university == "University of Alabama"           ~ "Tuscaloosa",
      university == "Portland State University"       ~ "Portland",
      TRUE                                            ~ city
    )
  )



# Now do stratified sampling
set.seed(1)

departments <- departments %>%
  mutate(tier = case_when(
    rank <= 20 ~ "Tier 1",
    rank <= 40 ~ "Tier 2",
    rank <= 70 ~ "Tier 3",
    TRUE ~ "Tier 4"
  ))

departments <- departments %>%
  mutate(
    # Section A: Geographic and Location Preferences
    q1_geographic_setting = NA_character_,  # A/B/C/D/E
    q2_region = NA_character_,  # Multiple: Northeast/Southeast/Midwest/Southwest/West Coast
    q3_airport_proximity = NA_character_,  # A/B/C/D
    q4_cost_of_living = NA_real_,  # Actual cost of living index or categorical
    q5_dual_career = NA_character_,  # Does department have dual-career program? Y/N
    
    # Section B: Compensation and Resources (Department typical offers)
    q6_typical_salary_range = NA_character_,  # A/B/C/D/E/F
    q7_typical_startup = NA_character_,  # A/B/C/D/E/F
    q8_guaranteed_summer = NA_character_,  # A/B/C/D
    
    # Section C: Teaching and Mentoring
    q9_typical_teaching_load = NA_character_,  # A/B/C/D/E (courses per year)
    q10_course_types = NA_character_,  # A/B/C/D
    q11_mentoring_program = NA_character_,  # A/B/C/D
    
    # Section D: Research Environment
    q12_research_culture = NA_character_,  # A/B/C/D/E
    q13_publication_venues = NA_character_,  # A/B/C/D/E
    q14_phd_student_ratio = NA_real_,  # Actual ratio
    q15_medical_school_proximity = NA_character_,  # A/B/C/D
    
    # Additional useful info
    department_size_faculty = NA_integer_,
    department_size_phd_students = NA_integer_,
    has_biostat_program = NA_character_,  # Y/N
    primary_department_type = NA_character_  # Statistics/Biostatistics/Math/Other
  )

# # Save template
# write_csv(sampled_departments, "data/department_characteristics_template.csv")
# 
# cat("Template created! Now you need to fill in the data.\n")





library(tidyverse)

# ===== Q1: Geographic Setting (based on metro area population) =====
city_setting <- tribble(
  ~city, ~state, ~q1_geographic_setting,
  # Major metropolitan (>1M)
  "Stanford", "CA", "A",
  "Berkeley", "CA", "A",
  "Chicago", "IL", "A",
  "New York", "NY", "A",
  "Philadelphia", "PA", "A",
  "Seattle", "WA", "A",
  "Los Angeles", "CA", "A",
  "Houston", "TX", "A",
  "Washington", "DC", "A",
  "Dallas", "TX", "A",
  "La Jolla", "CA", "A",
  "Irvine", "CA", "A",
  "Riverside", "CA", "A",
  "Tempe", "AZ", "A",
  "St. Louis", "MO", "A",
  "Denver", "CO", "A",
  "Charlotte", "NC", "A",
  "San Antonio", "TX", "A",
  "Tampa", "FL", "A",
  
  # Mid-sized city (250K-1M)
  "Cambridge", "MA", "B",
  "Pittsburgh", "PA", "B",
  "Ann Arbor", "MI", "B",
  "Minneapolis", "MN", "B",
  "Madison", "WI", "B",
  "Columbus", "OH", "B",
  "Austin", "TX", "B",
  "Raleigh", "NC", "B",
  "Durham", "NC", "B",
  "Tucson", "AZ", "B",
  "Baltimore", "MD", "B",
  "Cleveland", "OH", "B",
  "Cincinnati", "OH", "B",
  "Milwaukee", "WI", "B",
  "Albuquerque", "NM", "B",
  "Orlando", "FL", "B",
  "Reno", "NV", "B",
  
  # Small city/college town (50K-250K)
  "Chapel Hill", "NC", "C",
  "Ithaca", "NY", "C",
  "College Station", "TX", "C",
  "Davis", "CA", "C",
  "New Haven", "CT", "C",
  "Ames", "IA", "C",
  "University Park", "PA", "C",
  "West Lafayette", "IN", "C",
  "Champaign", "IL", "C",
  "Piscataway", "NJ", "C",
  "Gainesville", "FL", "C",
  "Fort Collins", "CO", "C",
  "Tallahassee", "FL", "C",
  "East Lansing", "MI", "C",
  "Storrs", "CT", "C",
  "Iowa City", "IA", "C",
  "Athens", "GA", "C",
  "Evanston", "IL", "C",
  "Columbia", "MO", "C",
  "Columbia", "SC", "C",
  "Blacksburg", "VA", "C",
  "Santa Barbara", "CA", "C",
  "Santa Cruz", "CA", "C",
  "Charlottesville", "VA", "C",
  "Rochester", "NY", "C",
  "Amherst", "MA", "C",
  "Fairfax", "VA", "C",
  "Bloomington", "IN", "C",
  "Corvallis", "OR", "C",
  "Richardson", "TX", "C",
  "Clemson", "SC", "C",
  "Lexington", "KY", "C",
  "Waco", "TX", "C",
  "Manhattan", "KS", "C",
  "Binghamton", "NY", "C",
  "Bowling Green", "OH", "C",
  "Bethlehem", "PA", "C",
  "Worcester", "MA", "C",
  "Logan", "UT", "C",
  "Bozeman", "MT", "C",
  "Newark", "NJ", "C",
  "DeKalb", "IL", "C",
  "Stillwater", "OK", "C",
  "Fayetteville", "AR", "C",
  "Greensboro", "NC", "C",
  "Toledo", "OH", "C",
  "Kalamazoo", "MI", "C",
  "Fargo", "ND", "C",
  "Rochester Hills", "MI", "C",
  "Greeley", "CO", "C",
  "Norfolk", "VA", "C",
  "Madison", "SD", "C",
  
  # Rural/small town (<50K)
  "Notre Dame", "IN", "D",
  "Auburn", "AL", "D",
  "Pullman", "WA", "D",
  "Houghton", "MI", "D",
  "Mount Pleasant", "MI", "D"
)

# ===== Q2: Region =====
state_region <- tribble(
  ~state, ~q2_region,
  "MA", "Northeast",
  "NY", "Northeast",
  "PA", "Northeast",
  "CT", "Northeast",
  "NJ", "Northeast",
  "MD", "Northeast",
  "NC", "Southeast",
  "VA", "Southeast",
  "FL", "Southeast",
  "GA", "Southeast",
  "SC", "Southeast",
  "KY", "Southeast",
  "AL", "Southeast",
  "IL", "Midwest",
  "MI", "Midwest",
  "OH", "Midwest",
  "IN", "Midwest",
  "WI", "Midwest",
  "MN", "Midwest",
  "IA", "Midwest",
  "MO", "Midwest",
  "KS", "Midwest",
  "NE", "Midwest",
  "SD", "Midwest",
  "ND", "Midwest",
  "TX", "Southwest",
  "OK", "Southwest",
  "AR", "Southwest",
  "NM", "Southwest",
  "AZ", "Southwest",
  "CA", "West Coast",
  "WA", "West Coast",
  "OR", "West Coast",
  "NV", "West Coast",
  "CO", "West Coast",
  "UT", "West Coast",
  "MT", "West Coast",
  "DC", "Northeast"
)

# ===== Q3: Airport Proximity =====
airport_proximity <- tribble(
  ~city, ~state, ~q3_airport_proximity,
  "Stanford", "CA", "A",
  "Berkeley", "CA", "A",
  "Cambridge", "MA", "A",
  "Chicago", "IL", "A",
  "Pittsburgh", "PA", "A",
  "New York", "NY", "A",
  "Philadelphia", "PA", "A",
  "Seattle", "WA", "A",
  "Minneapolis", "MN", "A",
  "Los Angeles", "CA", "A",
  "Austin", "TX", "A",
  "Houston", "TX", "A",
  "Washington", "DC", "A",
  "La Jolla", "CA", "A",
  "Irvine", "CA", "A",
  "Riverside", "CA", "A",
  "Tempe", "AZ", "A",
  "Columbus", "OH", "A",
  "Piscataway", "NJ", "A",
  "St. Louis", "MO", "A",
  "Denver", "CO", "A",
  "Dallas", "TX", "A",
  "Richardson", "TX", "A",
  "Charlotte", "NC", "A",
  "Milwaukee", "WI", "A",
  "San Antonio", "TX", "A",
  "Albuquerque", "NM", "A",
  "Orlando", "FL", "A",
  "Tampa", "FL", "A",
  "Cleveland", "OH", "A",
  "Cincinnati", "OH", "A",
  "Baltimore", "MD", "A",
  "Reno", "NV", "A",
  "Newark", "NJ", "A",
  
  "Durham", "NC", "B",
  "Raleigh", "NC", "B",
  "Ann Arbor", "MI", "B",
  "Madison", "WI", "B",
  "New Haven", "CT", "B",
  "Chapel Hill", "NC", "B",
  "Evanston", "IL", "B",
  "Davis", "CA", "B",
  "Gainesville", "FL", "B",
  "Tallahassee", "FL", "B",
  "Santa Barbara", "CA", "B",
  "Santa Cruz", "CA", "B",
  "Rochester", "NY", "B",
  "Tucson", "AZ", "B",
  "Fort Collins", "CO", "B",
  "Fairfax", "VA", "B",
  "Lexington", "KY", "B",
  "Amherst", "MA", "B",
  "Worcester", "MA", "B",
  "Greensboro", "NC", "B",
  "Norfolk", "VA", "B",
  "Fargo", "ND", "B",
  "Rochester Hills", "MI", "B",
  "Binghamton", "NY", "B",
  "Bethlehem", "PA", "B",
  "DeKalb", "IL", "B",
  "Toledo", "OH", "B",
  
  "Ithaca", "NY", "C",
  "College Station", "TX", "C",
  "Ames", "IA", "C",
  "University Park", "PA", "C",
  "West Lafayette", "IN", "C",
  "Champaign", "IL", "C",
  "East Lansing", "MI", "C",
  "Storrs", "CT", "C",
  "Iowa City", "IA", "C",
  "Athens", "GA", "C",
  "Columbia", "MO", "C",
  "Columbia", "SC", "C",
  "Blacksburg", "VA", "C",
  "Charlottesville", "VA", "C",
  "Bloomington", "IN", "C",
  "Corvallis", "OR", "C",
  "Clemson", "SC", "C",
  "Waco", "TX", "C",
  "Manhattan", "KS", "C",
  "Bowling Green", "OH", "C",
  "Logan", "UT", "C",
  "Bozeman", "MT", "C",
  "Stillwater", "OK", "C",
  "Fayetteville", "AR", "C",
  "Kalamazoo", "MI", "C",
  "Greeley", "CO", "C",
  "Madison", "SD", "C",
  "Mount Pleasant", "MI", "C",
  
  "Notre Dame", "IN", "D",
  "Auburn", "AL", "D",
  "Pullman", "WA", "D",
  "Houghton", "MI", "D"
)




# ===== Q4: Cost of Living =====
living_cost_mapper <- function(departments)  {

  # Read the cost of living data (https://www.kaggle.com/datasets/asaniczka/us-cost-of-living-dataset-3171-counties?resource=download&select=cost_of_living_us.csv)
  living_cost <- read_csv("Webscraping Tool/supporting_data/cost_of_living_us.csv")

  # Calculate county-level costs
  county_costs <- living_cost %>% 
    group_by(county, state) %>% 
    summarise(County_Cost = mean(total_cost), .groups = "drop")

  # Create city to county mapping
  city_county_mapping <- tribble(
    ~city, ~state, ~county,
    # California
    "Stanford", "CA", "Santa Clara County",
    "Berkeley", "CA", "Alameda County",
    "Davis", "CA", "Yolo County",
    "Los Angeles", "CA", "Los Angeles County",
    "La Jolla", "CA", "San Diego County",
    "Irvine", "CA", "Orange County",
    "Riverside", "CA", "Riverside County",
    "Santa Barbara", "CA", "Santa Barbara County",
    "Santa Cruz", "CA", "Santa Cruz County",
    
    # Massachusetts
    "Cambridge", "MA", "Middlesex County",
    "Amherst", "MA", "Hampshire County",
    "Worcester", "MA", "Worcester County",
    "Boston", "MA","Suffolk County",
  
  
    # New York
    "New York", "NY", "New York County",
    "Ithaca", "NY", "Tompkins County",
    "Rochester", "NY", "Monroe County",
    "Binghamton", "NY", "Broome County",
    
    # Pennsylvania
    "Philadelphia", "PA", "Philadelphia County",
    "Pittsburgh", "PA", "Allegheny County",
    "University Park", "PA", "Centre County",
    "Bethlehem", "PA", "Northampton County",
    
    # Illinois
    "Chicago", "IL", "Cook County",
    "Champaign", "IL", "Champaign County",
    "Evanston", "IL", "Cook County",
    "DeKalb", "IL", "DeKalb County",
    
    # Texas
    "Austin", "TX", "Travis County",
    "Houston", "TX", "Harris County",
    "Dallas", "TX", "Dallas County",
    "Richardson", "TX", "Dallas County",
    "San Antonio", "TX", "Bexar County",
    "College Station", "TX", "Brazos County",
    "Waco", "TX", "McLennan County",
    
    # North Carolina
    "Durham", "NC", "Durham County",
    "Raleigh", "NC", "Wake County",
    "Chapel Hill", "NC", "Orange County",
    "Charlotte", "NC", "Mecklenburg County",
    "Greensboro", "NC", "Guilford County",
    
    # Michigan
    "Ann Arbor", "MI", "Washtenaw County",
    "East Lansing", "MI", "Ingham County",
    "Kalamazoo", "MI", "Kalamazoo County",
    "Mount Pleasant", "MI", "Isabella County",
    "Houghton", "MI", "Houghton County",
    "Rochester Hills", "MI", "Oakland County",
  
    # Washington
    "Seattle", "WA", "King County",
    "Pullman", "WA", "Whitman County",
    
    # DC
    "Washington", "DC", "District of Columbia",
    
    # Colorado
    "Denver", "CO", "Denver County",
    "Fort Collins", "CO", "Larimer County",
    "Greeley", "CO", "Weld County",
    
    # Minnesota
    "Minneapolis", "MN", "Hennepin County",
    
    # Wisconsin
    "Madison", "WI", "Dane County",
    "Milwaukee", "WI", "Milwaukee County",
    
    # Connecticut
    "New Haven", "CT", "New Haven County",
    "Storrs", "CT", "Tolland County",
    
    # Iowa
    "Ames", "IA", "Story County",
    "Iowa City", "IA", "Johnson County",
    
    # Indiana
    "West Lafayette", "IN", "Tippecanoe County",
    "Bloomington", "IN", "Monroe County",
    "Notre Dame", "IN", "St. Joseph County",
    
    # Ohio
    "Columbus", "OH", "Franklin County",
    "Cleveland", "OH", "Cuyahoga County",
    "Cincinnati", "OH", "Hamilton County",
    "Bowling Green", "OH", "Wood County",
    "Toledo", "OH", "Lucas County",
    
    # New Jersey
    "Piscataway", "NJ", "Middlesex County",
    "Newark", "NJ", "Essex County",
    
    # Florida
    "Gainesville", "FL", "Alachua County",
    "Tallahassee", "FL", "Leon County",
    "Tampa", "FL", "Hillsborough County",
    "Orlando", "FL", "Orange County",
    
    # Missouri
    "St. Louis", "MO", "St. Louis County",
    "Columbia", "MO", "Boone County",
    
    # Georgia
    "Athens", "GA", "Clarke County",
    
    # Virginia
    "Blacksburg", "VA", "Montgomery County",
    "Charlottesville", "VA", "Albemarle County",
    "Fairfax", "VA", "Fairfax County",
    "Norfolk", "VA", "Norfolk city",
  
    # Maryland
    "Baltimore", "MD", "Baltimore County",
    
    # Arizona
    "Tempe", "AZ", "Maricopa County",
    "Tucson", "AZ", "Pima County",
    
    # Nevada
    "Reno", "NV", "Washoe County",
    
    # Kentucky
    "Lexington", "KY", "Fayette County",
    
    # South Carolina
    "Columbia", "SC", "Richland County",
    "Clemson", "SC", "Pickens County",
    
    # Oregon
    "Corvallis", "OR", "Benton County",
    "Portland", "OR", "Multnomah County",
    
    # New Mexico
    "Albuquerque", "NM", "Bernalillo County",
    
    # Kansas
    "Manhattan", "KS", "Riley County",
    
    # Utah
    "Logan", "UT", "Cache County",
    
    # Montana
    "Bozeman", "MT", "Gallatin County",
    
    # Oklahoma
    "Stillwater", "OK", "Payne County",
    
    # Arkansas
    "Fayetteville", "AR", "Washington County",
    
    # North Dakota
    "Fargo", "ND", "Cass County",
    
    # South Dakota
    "Madison", "SD", "Lake County",
    
    # Alabama
    "Auburn", "AL", "Lee County",
    "Tuscaloosa", "AL", "Tuscaloosa County",
    
    # Nebraska
    "Lincoln", "NE", "Lancaster County"
  )

# Join to get actual county costs
  cost_of_living_from_data <- city_county_mapping %>%
    left_join(county_costs, by = c("county", "state")) %>%
    dplyr::select(city, state, q4_cost_of_living = County_Cost)



  departments <- departments %>%
    left_join(
      cost_of_living_from_data %>% dplyr::select(city, state, q4_cost_of_living),
      by = c("city", "state")
  )

  

}
departments <- living_cost_mapper(departments)


# ===== Apply all mappings using city AND state =====
departments <- departments %>%
  left_join(city_setting, by = c("city", "state")) %>%
  left_join(state_region, by = "state") %>%
  left_join(airport_proximity, by = c("city", "state"))


# Remove the duplicate columns and keep only the .y versions (most recent)
departments <- departments %>%
  dplyr::select(-ends_with(".x")) %>%
  rename(
    q1_geographic_setting = q1_geographic_setting.y,
    q2_region = q2_region.y,
    q3_airport_proximity = q3_airport_proximity.y,
    q4_cost_of_living = q4_cost_of_living.y
  )

# # Now check results
departments %>%
  dplyr::select(rank, university, city, state,
         q1_geographic_setting, q2_region,
         q3_airport_proximity, q4_cost_of_living) %>%
  print(n = 20)



##############################################################################
# Q6: Salary est.

salary_est <- function(departments)  {

  # Read the salary data (https://nces.ed.gov/ipeds/trendgenerator/app/build-table/5/50?cid=161&rid=37&ridv=0%7C1%7C2%7C3%7C4%7C5%7C6%7C7%7C8&cidv=3)
  salary_data <- read_lines("Webscraping Tool/supporting_data/salary_data.csv")
  salary_data <- salary_data[c(5:55)]
  
  salary_data <- tibble(salary_data = salary_data) %>%
    separate(
      salary_data,
      into  = c("state", "salary"),
      sep   = ",",
      extra = "merge",
      fill  = "right"
    ) %>%
    mutate(
      salary = str_remove_all(salary, '["$,]'),
      salary = as.numeric(salary),
      state  = str_trim(state),
      
      # convert full state names to abbreviations
      state = case_when(
        state == "District of Columbia" ~ "DC",
        state %in% state.name ~ state.abb[match(state, state.name)],
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(state, salary)
  
  
  departments <- departments %>%
    left_join(
      salary_data %>% dplyr::select(state, salary),
      by = "state"
    )
  
  ### Add something depending on Tier and/or whether in Business school
  departments <- departments %>%
    mutate(
      q6_typical_salary_range = case_when(
        tier == "Tier 1" ~ salary * 1.5,
        tier == "Tier 2" ~ salary * 1.3,
        tier == "Tier 3" ~ salary * 1.0,
        tier == "Tier 4" ~ salary * 0.9,
        TRUE             ~ NA_real_
      )
    ) %>% dplyr::select(-salary)
  
}

departments <- salary_est(departments)
##########################################################################################################

# ===== Q6: Typical Salary Range (based on tier and region) =====
# estimate_salary <- function(tier, state) {
#   # High cost-of-living states
#   high_col_states <- c("CA", "NY", "MA", "CT", "DC")
#   
#   base_salary <- case_when(
#     tier == "Tier 1" ~ "E",  # Above $160k
#     tier == "Tier 2" ~ "D",  # $140-160k
#     tier == "Tier 3" ~ "C",  # $120-140k
#     tier == "Tier 4" ~ "B"   # $100-120k
#   )
#   
#   # Adjust down for low COL if lower tier
#   if(!(state %in% high_col_states) && tier %in% c("Tier 3", "Tier 4")) {
#     base_salary <- case_when(
#       base_salary == "C" ~ "B",
#       base_salary == "B" ~ "B",
#       TRUE ~ base_salary
#     )
#   }
#   
#   return(base_salary)
# }

# ===== Q7: Typical Startup (based on tier and research focus) =====
estimate_startup <- function(tier) {
  case_when(
    tier == "Tier 1" ~ "D",  # $250-500k
    tier == "Tier 2" ~ "C",  # $150-250k
    tier == "Tier 3" ~ "B",  # $50-150k
    tier == "Tier 4" ~ "B"   # $50-150k
  )
}

# ===== Q8: Guaranteed Summer (based on tier) =====
estimate_summer_support <- function(tier) {
  case_when(
    tier == "Tier 1" ~ "A",  # Essential (3+ years)
    tier == "Tier 2" ~ "B",  # Very important (2+ years)
    tier == "Tier 3" ~ "C",  # Moderately important (some support)
    tier == "Tier 4" ~ "C"   # Moderately important
  )
}

# ===== Q9: Typical Teaching Load (based on tier) =====
estimate_teaching_load <- function(tier) {
  case_when(
    tier == "Tier 1" ~ "B",  # Light (2 courses/year)
    tier == "Tier 2" ~ "B",  # Light (2 courses/year)
    tier == "Tier 3" ~ "C",  # Moderate (3 courses/year)
    tier == "Tier 4" ~ "D"   # Substantial (4+ courses/year)
  )
}

# ===== Q10: Course Types (based on tier and program) =====
estimate_course_types <- function(tier, university) {
  # Check if it's a major research university
  if(tier %in% c("Tier 1", "Tier 2")) {
    return("B")  # Mix of graduate and advanced undergrad
  } else {
    return("D")  # Flexible across all levels
  }
}

# ===== Q11: Mentoring Program (based on tier) =====
estimate_mentoring <- function(tier) {
  case_when(
    tier == "Tier 1" ~ "A",  # Very important (structured)
    tier == "Tier 2" ~ "B",  # Moderately important
    tier == "Tier 3" ~ "B",  # Moderately important
    tier == "Tier 4" ~ "C"   # Slightly important
  )
}

# ===== Q12: Research Culture (based on department characteristics) =====
# This requires manual input or web research
# For now, let's create a lookup for known departments
research_culture_lookup <- tribble(
  ~university, ~q12_research_culture,
  "University of Chicago", "A",  # Theoretical focus
  "Duke University", "C",  # Biostatistics focus
  "Purdue University--West Lafayette", "D",  # Balanced
  "University of Illinois--Urbana-Champaign", "D",  # Balanced
  "Michigan State University", "D",  # Balanced
  "Boston University", "C",  # Biostatistics focus
  "George Mason University", "B",  # Applied/data science
  "University of Arizona", "D",  # Balanced
  "University of Maryland--Baltimore County", "B",  # Applied
  "Kansas State University", "D",  # Balanced
  "Worcester Polytechnic Institute", "B",  # Applied/data science
  "Utah State University", "D",  # Balanced
  "University of Texas--San Antonio", "B",  # Applied
  "University of Alabama", "D",  # Balanced
  "Marquette University", "D",  # Balanced
  "Central Michigan University", "D",  # Balanced
  "Michigan Technological University", "B",  # Applied
  "University of Northern Colorado", "D",  # Balanced
  "North Dakota State University", "D",  # Balanced
  "Oakland University", "D"  # Balanced
)

# ===== Q13: Publication Venues (based on tier and culture) =====
estimate_publication_venues <- function(tier, research_culture) {
  if(tier == "Tier 1") {
    return("A")  # Top-4 journals
  } else if(research_culture == "C") {
    return("B")  # Applied statistics journals
  } else if(research_culture == "B") {
    return("E")  # Mix of venues
  } else {
    return("E")  # Mix of venues
  }
}

# ===== Q14: PhD Student Ratio =====
# This requires looking up actual numbers
# For estimation: Tier 1/2 usually have 2-3 students/faculty, Tier 3/4 have 1-2
estimate_phd_ratio <- function(tier) {
  case_when(
    tier == "Tier 1" ~ runif(1, 2.5, 3.5),
    tier == "Tier 2" ~ runif(1, 2.0, 3.0),
    tier == "Tier 3" ~ runif(1, 1.5, 2.5),
    tier == "Tier 4" ~ runif(1, 1.0, 2.0)
  )
}

# ===== Q15: Medical School Proximity =====
# Universities known to have medical schools

## NOT CMU, NC STATE, IOWA STATE, PURDUE, RICE, COLORADO STATE, UCSB, UCSC, GEORGE MASON, OREGON STATE, SMU, CLEMSON, NOTRE DAME,
## KANSAS STATE, UT DALLAS, BOWLING GREEN, WORCESTER POLYTECHNIC, NEW JERSEY INSTITUTE OF TECH., NORTHERN ILLINOIS, PORTLAND STATE,
## UNC GREENSBORO, MICHIGAN TECH, NORTH DAKOTA STATE, or SOUTH DAKOTA STATE
universities_with_med_schools <- c(
  "Stanford University", "University of California--Berkeley",
  "Harvard University", "University of Chicago", "Columbia University", "Duke University",
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

med_school_proximity <- function(university) {
  if(university %in% universities_with_med_schools) {
    return(1)  # Essential (has medical school)
  } else {
    return(0)  # Not important (no medical school)
  }
}

# ===== Apply all estimations =====
set.seed(2026)  # For reproducibility of random ratios

departments <- departments %>%
  rowwise() %>%
  mutate(
    # Q6: Salary
    q6_typical_salary_range = estimate_salary(tier, state),
    
    # Q7: Startup
    q7_typical_startup = estimate_startup(tier),
    
    # Q8: Summer support
    q8_guaranteed_summer = estimate_summer_support(tier),
    
    # Q9: Teaching load
    q9_typical_teaching_load = estimate_teaching_load(tier),
    
    # Q10: Course types
    q10_course_types = estimate_course_types(tier, university),
    
    # Q11: Mentoring
    q11_mentoring_program = estimate_mentoring(tier),
    
    # Q14: PhD ratio (random within tier range)
    q14_phd_student_ratio = estimate_phd_ratio(tier),
    
    # Q15: Medical school proximity
    q15_medical_school_proximity = med_school_proximity(university)
  ) %>%
  ungroup() %>%
  # Q12: Research culture (from lookup)
  left_join(research_culture_lookup, by = "university") %>%
  # Q13: Publication venues (based on tier and culture)
  rowwise() %>%
  mutate(
    q13_publication_venues = estimate_publication_venues(tier, q12_research_culture)
  ) %>%
  ungroup()

# View results
departments %>%
  dplyr::select(rank, tier, university, 
         q6_typical_salary_range, q7_typical_startup, q8_guaranteed_summer,
         q9_typical_teaching_load, q10_course_types, q11_mentoring_program,
         q12_research_culture, q13_publication_venues, 
         q14_phd_student_ratio, q15_medical_school_proximity) %>%
  print(n = 20)

# Save the complete dataset
write_csv(departments, "data/departments_complete_questionnaire_data.csv")

cat("\n✓ All questionnaire variables estimated!\n")
cat("Review the data and refine specific departments as needed.\n")

























# AT THE END:
# Sample from each tier separately
tier1_sample <- departments %>% filter(tier == "Tier 1") %>% slice_sample(n = 2)
tier2_sample <- departments %>% filter(tier == "Tier 2") %>% slice_sample(n = 3)
tier3_sample <- departments %>% filter(tier == "Tier 3") %>% slice_sample(n = 5)
tier4_sample <- departments %>% filter(tier == "Tier 4") %>% slice_sample(n = 10)

sampled_departments <- bind_rows(tier1_sample, tier2_sample, tier3_sample, tier4_sample) %>%
  arrange(rank)
# 
# cat("\nTotal sampled:", nrow(sampled_departments), "departments\n\n")

# Display the sampled schools
cat("Sampled departments:\n")
sampled_departments |> 
  dplyr::select(rank, tier, university, city, state) |>
  print(n = 20)
