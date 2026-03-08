# A Statistical Market-Design Framework for the Academic Job Market

This repository contains the replication code and data for:

> **A Statistical Market-Design Framework for the Academic Job Market**
> 
> Ali Kaazempur-Mofrad, Xiaowu Dai, and Xuming He
>
> Project website: [academic-markets.github.io](https://academic-markets.github.io)

The paper frames interview allocation as a statistical ranking problem under uncertainty and proposes a market-design framework that incorporates structured preference signaling into interview selection. Candidates submit a standardized questionnaire describing preferences over job characteristics, which departments combine with traditional application materials and historical hiring data to estimate candidate offer acceptance probabilities and expected utilities. The simulation framework evaluates market outcomes under varying participation rates.

## Computational Requirements

### Software

- **R** (>= 4.2.0)
- **Python** 3.8+ (for web scraping only; not needed for simulation)

### R Packages

| Package | Version | Purpose |
|---------|---------|---------|
| `tidyverse` | >= 2.0.0 | Data manipulation and visualization |
| `torch` | >= 0.12.0 | Neural network models for acceptance prediction |
| `MASS` | >= 7.3 | Statistical functions |
| `Matrix` | >= 1.5 | Sparse matrix operations |
| `future` | >= 1.33.0 | Parallel execution backend |
| `future.apply` | >= 1.11.0 | Parallel `lapply` via futures |
| `viridis` | >= 0.6.0 | Color palettes for heatmap figures |
| `paletteer` | >= 0.6.0 | Color palettes for figures |
| `patchwork` | >= 1.2.0 | Combining ggplot panels |
| `scales` | >= 1.3.0 | Axis scale formatting |
| `gridExtra` | >= 2.3 | Grid-based plot arrangement |
| `janitor` | >= 2.2.0 | Data cleaning utilities |
| `collegeScorecard` | >= 0.1.0 | US College Scorecard data (department generation only) |
| `stringdist` | >= 0.9.0 | String matching (department generation only) |
| `jsonlite` | >= 1.8.0 | JSON parsing (web scraping only) |

Install all packages with:

```r
install.packages(c("tidyverse", "torch", "MASS", "Matrix", "future",
                   "future.apply", "viridis", "paletteer", "patchwork", "scales",
                   "gridExtra", "janitor", "collegeScorecard", "stringdist",
                   "jsonlite"))
torch::install_torch()
```

### Hardware

- **Memory**: >= 16 GB RAM recommended (32 GB for full 200-replicate runs)
- **CPU**: Multi-core processor recommended; each simulation batch uses 20 parallel workers
- **Runtime estimates**:
  - `BurnIn.R` (Stage 1): ~10-15 minutes
  - `sim_batch.R` (Stage 2): ~2.5-3.5 hours per batch (20 replicates each). The 10 batches can be run in parallel on an HPC cluster, so wall-clock time for the full 200 replicates is approximately 2.5-3.5 hours when parallelized.
  - `Result_processer.R` (Stage 3): ~5 minutes

## Repository Structure

```
Academic-Job-Market/
├── README.md
├── code/
│   ├── Sim_Functions.R                 # Core simulation functions
│   ├── BurnIn.R                        # Stage 1: Burn-in phase
│   ├── sim_batch.R                     # Stage 2: Simulation batches (HPC-ready)
│   ├── sim_array.sh                    # SGE job array script for HPC
│   ├── Result_processer.R              # Stage 3: Combine results and generate figures
│   └── department_generator/           # Department dataset construction (Stage 0)
│       ├── USNews_Rankings_Scraper.R   # Scrape and parse US News 2022 rankings
│       ├── Department_Generator.R      # Build department attributes
│       ├── USNews-Scrapper/            # Git submodule: Python web scraper
│       ├── usnews_statistics.csv       # Scraped rankings (101 departments)
│       └── supporting_data/
│           ├── cost_of_living_us.csv   # County-level cost of living (EPI Family Budget)
│           └── salary_data.csv         # State-level faculty salary data (NCES 2023)
├── data/
│   └── departments_dataset.csv         # Pre-built department dataset (101 departments)
├── manuscript/                         # LaTeX source
│   └── fig/                            # Generated figures
└── output/                             # Generated artifacts
```

## Workflow: Order of Operations

The simulation proceeds in three stages. All stage scripts are run from the project root directory and source `code/Sim_Functions.R`, which contains the complete set of shared functions.

### Stage 0: Department Dataset Generation (Optional)

The department dataset (`data/departments_dataset.csv`) is provided pre-built. To regenerate it from source data:

1. **Scrape and parse US News rankings** (requires Python 3.8+):

   Run `code/department_generator/USNews_Rankings_Scraper.R`. The script automatically initializes the [USNews-Scrapper](https://github.com/OvroAbir/USNews-Scrapper) Git submodule, sets up a Python virtual environment, runs the web scraper, and parses the resulting JSON into a structured CSV.

   Output: `code/department_generator/usnews_statistics.csv`

2. **Generate department attributes** by matching to College Scorecard data and estimating questionnaire responses:

   Run `code/department_generator/Department_Generator.R`.

   Output: `data/departments_dataset.csv`

   This script:
   - Loads the US News 2022 rankings of 101 statistics departments
   - Matches each department to the US College Scorecard database to obtain institutional characteristics (enrollment, selectivity, public/private status, locale)
   - Assigns geographic attributes (setting, region, airport proximity) via hand-coded lookup tables
   - Computes cost-of-living from county-level EPI Family Budget data
   - Estimates faculty salary from NCES state-level data, scaled by department tier
   - Infers remaining questionnaire responses (teaching load, startup, research culture, etc.) from institutional characteristics and tier
   - Creates "hidden gem" departments: ~25% of Tier 3/4 departments receive 1-2 attributes that rival higher-tier departments

### Stage 1: Burn-In Phase

The burn-in phase runs 20 years of baseline simulation (without strategic participation) to establish department learned priors. This must be run once before any simulation batches.

```bash
Rscript code/BurnIn.R
# Output: output/burn_in_artifacts.rds (~2-3 minutes)
```

**Key parameters** (set in `code/BurnIn.R`):
- `n_candidates = 300` — candidates per cohort per year
- `burn_in_years = 20` — years of burn-in
- `sim_years = 10` — years of simulation (determines hiring schedule)
- `base_seed = 14` — master random seed
- `n_bootstrap = 100` — bootstrap replicates for ranking uncertainty
- `max_offer_rounds = 2` — one regular offer round plus a scramble round
- `participation_rates = c(0, 0.05, 0.2, 0.5, 0.9, 1.0)` — participation rates to evaluate

### Stage 2: Simulation Batches

Each batch processes 20 independent replicates for all participation rates. Run 10 batches for 200 total replicates. Each batch takes approximately 2.5-3.5 hours.

```bash
# Run sequentially (~15-25 hours total):
for i in $(seq 1 10); do
  Rscript code/sim_batch.R $i
done

# Or submit as an HPC job array for parallel execution (~2.5-3.5 hours total).
# The paper's results were generated on an HPC cluster using
# SGE job arrays with 20 cores and 4 GB per core per task:
# qsub -t 1-10 -pe shared 20 -l h_rt=24:00:00,h_data=4G code/sim_array.sh
```

Each batch:
1. Loads `output/burn_in_artifacts.rds`
2. Reconstructs neural network models from saved historical data
3. Runs 20 replicates in parallel (via `future::multisession` with 20 workers)
4. For each replicate, simulates 10 years of job market operation across all participation rates
5. Saves results to `output/sim_batch_<TASK_ID>.rds`

### Stage 3: Combine Results and Generate Figures

```bash
Rscript code/Result_processer.R
```

This script:
1. Loads all 10 batch files and the burn-in artifacts
2. Merges results into a single `output/all_sim_results.rds` structure
3. Generates PDF figures in `manuscript/fig/`:

   **Main text figures:**
   - `fig_dept_hiring_heatmap.pdf` — Hiring outcomes across tiers
   - `fig_department_welfare.pdf` — Department welfare per hiring position by tier
   - `fig_candidate_utility_by_tier.pdf` — Matched candidate utility $\bar{V}_{ij}$ by quality tier
   - `fig_candidate_by_participation.pdf` — Welfare comparison of participating vs. non-participating candidates

   **Supplement figures:**
   - `fig_candidate_welfare.pdf` — Mean candidate welfare across all candidates
   - `fig_candidate_welfare_unconditional.pdf` — Mean candidate welfare by quality tier
   - `fig_dept_interview_heatmap.pdf` — Interview allocation across department and candidate tiers
   - `fig_blocking_pairs.pdf` — Blocking pair rate by participation rate

## Data Description

A data dictionary for `departments_dataset.csv` is provided in `data/Data_Dictionary.md`.

### `departments_dataset.csv`

The primary input dataset containing 101 US statistics departments with the following variables:

| Variable | Type | Description |
|----------|------|-------------|
| `rank` | Integer | US News 2022 ranking (1-101) |
| `university` | Character | University name |
| `city`, `state` | Character | Location |
| `peer_assessment_score` | Numeric | US News peer assessment (1-5 scale) |
| `tier` | Character | Prestige tier (Tier 1-4, by rank) |
| `s_j` | Numeric | Department prestige parameter $s_j \in [0, 1]$ |
| `q1_geographic_setting` | Character | A=Major metro, B=Mid-size, C=College town, D=Rural |
| `q2_region` | Character | Northeast, Southeast, Midwest, Southwest, West Coast |
| `q3_airport_proximity` | Character | A=<1hr, B=1-2hr, C=2-3hr, D=>3hr |
| `q4_cost_of_living` | Numeric | Annual cost of living (USD) |
| `q5_dual_career` | Character | Dual career program: Y/N |
| `q6_typical_salary_range` | Numeric | Estimated starting salary (USD) |
| `q7_typical_startup` | Character | A=<\$50K, B=\$50-150K, C=\$150-250K, D=\$250-500K, E=>\$500K |
| `q8_guaranteed_summer` | Character | A=3+ yr, B=2 yr, C=Some, D=Limited |
| `q9_typical_teaching_load` | Character | A=0-1 course/yr, B=2, C=3, D=4+ |
| `q10_course_types` | Character | A=Graduate, B=Mix, C=Mostly undergrad, D=Flexible |
| `q11_mentoring_program` | Character | A=Structured, B=Moderate, C=Informal, D=Minimal |
| `q12_research_culture` | Character | A=Theory, B=Applied/DS, C=Biostats, D=Collaborative, E=Highly collaborative |
| `q13_publication_venues` | Character | A=Top-4, B=Applied, C=ML, D=Domain, E=Mixed |
| `q14_phd_student_ratio` | Numeric | PhD students per faculty member |
| `q15_medical_school_proximity` | Integer | 0/1 indicator |
| `public_private` | Character | Public or Private institution |
| `university_size` | Character | Small/Medium/Large/Very Large |

### Supporting Data

- **`cost_of_living_us.csv`**: County-level cost of living from the Economic Policy Institute (EPI) Family Budget Calculator, including housing, food, transportation, healthcare, and other costs.
- **`salary_data.csv`**: State-level adjusted 9-month average faculty salary from the National Center for Education Statistics (NCES), 2023.

A data dictionary for these supporting data files is provided in `code/department_generator/supporting_data/Supporting_Data_Dictionary.md`.

## Key Functions in `Sim_Functions.R`

The simulation is built on the following core components. Notation follows the paper: $U_{ji}$ denotes department $j$'s utility from hiring candidate $i$, $V_{ij}$ denotes candidate $i$'s utility from joining department $j$, $\pi_{ji}$ is the acceptance probability, $f_{ji}$ is the preference alignment function, and $s_j$ is the department prestige parameter.

| Function | Description |
|----------|-------------|
| `candidate_utility()` | Candidate utility $V_{ij}$ over department prestige and fit |
| `department_utility()` | Department utility $U_{ji}$ over candidate quality and fit |
| `compute_f_j()` | Vectorized alignment score $f_{ji}$ computation (questionnaire-based) |
| `generate_candidates()` | Generate candidate cohorts with heterogeneous preferences |
| `prepare_departments()` | Initialize department attributes ($s_j$, weight vectors, questionnaire responses) |
| `acceptance_net` | Torch neural network for estimating acceptance probability $\pi_{ji}$ |
| `predict_acceptance_probability()` | Bootstrap acceptance prediction $\hat{\pi}_{ji}$ |
| `compute_pairwise_lower_ranks()` | Confidence-calibrated ranking via pairwise expected utility $U_{ji} \cdot \pi_{ji}$ comparisons |
| `select_interviews_sure_screening()` | Interview selection via sure screening |
| `resolve_offers_sequential()` | Two-round offer resolution with scramble |
| `run_burn_in_phase()` | Burn-in phase to learn department priors |
| `run_job_market_sim_with_learned_prior()` | Simulation with learned priors |
| `reconstruct_learned_prior_models()` | Rebuild torch models from historical data |
| `fig_hiring_heatmap()` | Figure: hiring outcomes heatmap |
| `fig_department_welfare()` | Figure: department welfare by tier |
| `fig_candidate_utility()` | Figure: candidate utility $\bar{V}_{ij}$ by tier (conditional on matching) |
| `fig_participation_welfare()` | Figure: participating vs. non-participating welfare |
| `fig_candidate_welfare()` | Figure: mean candidate welfare across all candidates (supplement) |
| `fig_candidate_welfare_unconditional()` | Figure: mean candidate welfare by quality tier (supplement) |
| `fig_interview_heatmap()` | Figure: interview allocation heatmap (supplement) |
| `fig_blocking_pairs()` | Figure: blocking pair rate by participation rate (supplement) |

## Reproducing Results

To reproduce all results from the paper:

```bash
# Step 1: Burn-in (run once, ~2-3 minutes)
Rscript code/BurnIn.R

# Step 2: Simulation batches (10 batches x 20 replicates = 200 total)
# ~2.5-3.5 hours per batch; run in parallel on HPC for fastest results
for i in $(seq 1 10); do
  Rscript code/sim_batch.R $i
done

# Step 3: Combine and generate figures
Rscript code/Result_processer.R
```

The department dataset (`data/departments_dataset.csv`) is provided and does not need to be regenerated unless modifications to the department construction are desired.

## Notes on Reproducibility

- All random number generation is seeded. The master seed (`base_seed = 14`) is set in `code/BurnIn.R` and propagated through all stages.
- Torch models are non-deterministic across different hardware/CUDA versions. Results may differ slightly across platforms but should be qualitatively consistent.
- The `future` package is used for parallelism. Set `future::plan(future::sequential)` to run single-threaded for debugging.

## License

This code is provided for academic reproducibility purposes accompanying the paper "A Statistical Market-Design Framework for the Academic Job Market for New Statisticians" (Kaazempur-Mofrad, Dai, and He).
