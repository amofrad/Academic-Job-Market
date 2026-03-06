# Data Dictionary: `departments_dataset.csv`

This file describes all variables in `departments_dataset.csv`, the primary input dataset containing 101 U.S. statistics departments used in the simulation.

| Variable | Type | Description |
|----------|------|-------------|
| `rank` | Integer | US News 2022 graduate program ranking (1–101) |
| `university` | Character | University name |
| `city` | Character | City where the department is located |
| `state` | Character | State where the department is located |
| `peer_assessment_score` | Numeric | US News peer assessment score (1–5 scale) |
| `tier` | Character | Prestige tier based on rank: Tier 1 (top 10%), Tier 2 (top 25%), Tier 3 (top 50%), Tier 4 (bottom 50%) |
| `s_j` | Numeric | Department prestige parameter $s_j \in [0, 1]$, derived from peer assessment score |
| `q1_geographic_setting` | Character | A = Major metro, B = Mid-size city, C = College town, D = Rural |
| `q2_region` | Character | US region: Northeast, Southeast, Midwest, Southwest, West Coast |
| `q3_airport_proximity` | Character | A = < 1 hr, B = 1–2 hr, C = 2–3 hr, D = > 3 hr |
| `q4_cost_of_living` | Numeric | Estimated annual cost of living (USD), based on EPI Family Budget Calculator data |
| `q5_dual_career` | Character | Dual career support program available: Y = Yes, N = No |
| `q6_typical_salary_range` | Numeric | Estimated assistant professor starting salary (USD), based on NCES state-level data scaled by tier |
| `q7_typical_startup` | Character | Typical startup package: A = < $50K, B = $50–150K, C = $150–250K, D = $250–500K, E = > $500K |
| `q8_guaranteed_summer` | Character | Guaranteed summer salary: A = 3+ years, B = 2 years, C = Some, D = Limited |
| `q9_typical_teaching_load` | Character | Typical teaching load per year: A = 0–1 course, B = 2 courses, C = 3 courses, D = 4+ courses |
| `q10_course_types` | Character | Course types taught: A = Graduate only, B = Mix of graduate and undergraduate, C = Mostly undergraduate, D = Flexible |
| `q11_mentoring_program` | Character | Faculty mentoring program: A = Structured, B = Moderate, C = Informal, D = Minimal |
| `q12_research_culture` | Character | Research culture: A = Theory, B = Applied/Data Science, C = Biostatistics, D = Collaborative, E = Highly collaborative |
| `q13_publication_venues` | Character | Primary publication venues: A = Top-4 journals, B = Applied journals, C = ML/CS venues, D = Domain-specific, E = Mixed |
| `q14_phd_student_ratio` | Numeric | Number of PhD students per faculty member |
| `q15_medical_school_proximity` | Integer | Proximity to medical school: 1 = Yes, 0 = No |
| `public_private` | Character | Institution control: Public or Private |
| `university_size` | Character | Institution enrollment size: Small, Medium, Large, Very Large |
