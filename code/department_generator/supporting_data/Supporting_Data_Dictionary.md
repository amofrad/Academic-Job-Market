# Supporting Data Dictionary

This file describes the two supporting data files used to construct `departments_dataset.csv`.

## `salary_data.csv`

State-level adjusted 9-month average faculty salary data from the National Center for Education Statistics (NCES), 2023. Downloaded from the IPEDS Trend Generator (https://nces.ed.gov/ipeds/trendgenerator/app/build-table/5), with Column set to Academic Rank (filtered to Assistant Professor) and rows set to State. Used to estimate starting salaries (`q6_typical_salary_range`) for each department.

| Variable | Description |
|----------|-------------|
| `State` | U.S. state name |
| `Salary` | Adjusted 9-month average salary for assistant professors (USD) |

## `cost_of_living_us.csv`

County-level cost-of-living estimates from the Economic Policy Institute (EPI) Family Budget Calculator, sourced from Kaggle (https://www.kaggle.com/datasets/asaniczka/us-cost-of-living-dataset-3171-counties). Used to assign cost-of-living values (`q4_cost_of_living`) to each department based on its county/metropolitan area.

| Variable | Description |
|----------|-------------|
| `state` | U.S. state |
| `areaname` | Metropolitan statistical area name |
| `county` | County name |
| `family_member_count` | Household composition (e.g., 1 adult, 0 children) |
| `housing_cost` | Annual housing cost (USD) |
| `food_cost` | Annual food cost (USD) |
| `transportation_cost` | Annual transportation cost (USD) |
| `healthcare_cost` | Annual healthcare cost (USD) |
| `other_necessities_cost` | Annual cost of other necessities (USD) |
| `total_cost` | Total annual cost of living (USD) |
| `median_family_income` | Median family income for the area (USD) |
