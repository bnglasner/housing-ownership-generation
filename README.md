# Homeownership by Generation

This repository analyzes how homeownership rates evolve across generations over the life cycle using CPS ASEC microdata from IPUMS. The project constructs consistent, weighted homeownership rates by age and birth cohort from 1962–2024 and produces summary datasets and visualizations suitable for publication and interactive applications.

---

## Data Source

**IPUMS CPS – Annual Social and Economic Supplement (ASEC)**  

Citation requirement (per IPUMS license):

Flood, Sarah, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren, Daniel Backman, Annie Chen, Grace Cooper, Stephanie Richards, Megan Schouweiler, and Michael Westberry. 2024. *IPUMS CPS: Version 12.0 [dataset]*. Minneapolis, MN: IPUMS. https://doi.org/10.18128/D030.V12.0

### Core Variables Used

- `YEAR` – Survey year  
- `MONTH` – Interview month  
- `AGE` – Age at last birthday  
- `RELATE` – Relationship to householder  
- `OWNERSHP` – Tenure status  
- `ASECWT` – Person-level supplement weight  
- `ASECWTH` – Household-level supplement weight  
- `MARST`, `SEX`, `RACE` – Demographic splits  
- `METRO`, `STATEFIP`, `REGION` – Geographic splits  

The extract includes all ASEC samples from March 1962 through March 2024.

---

## Project Structure

.
├── 00a_ipums_api_extract.R   # Defines and downloads IPUMS CPS extract via API
├── Generational Comp.R       # Core cleaning, cohort construction, and analysis
├── run_all.R                 # Master script to reproduce outputs
├── theme_eig.R               # Plot styling
├── data/                     # Raw IPUMS extract (not tracked in Git)
├── output/                   # Clean datasets
├── figures/                  # Static visualizations
└── README.md


---

## Process Flow

### Step 1: Define and Download the Extract

`00a_ipums_api_extract.R`:

- Uses the `ipumsr` package.
- Reads the IPUMS API key from an environment variable (`IPUMS_API_KEY`).
- Defines the extract to include only variables necessary for:
  - tenure classification
  - age and cohort construction
  - weighting
  - demographic and geographic splits
- Submits the extract request and downloads the `.dat` and `.xml` files.

This step only needs to be run when updating to new CPS years.

---

### Step 2: Load and Clean Microdata

`Generational Comp.R` performs the core analysis:

1. **Reads the fixed-width CPS extract**
   - Uses the IPUMS DDI file to correctly parse the data.

2. **Applies universe restrictions**
   - Individuals aged 19 and older.
   - ASEC sample only (if filtering using `ASECFLAG`).

3. **Constructs the homeownership indicator**

An individual is coded as a homeowner if:

- They are a householder or spouse  
  (`RELATE` codes corresponding to head/spouse), and  
- The dwelling is owned or being bought (`OWNERSHP == 10`).

This defines homeownership at the household level and assigns it to eligible adult individuals.

4. **Constructs birth year**

birth_year = YEAR - AGE


5. **Assigns generational cohorts**

Cohorts are defined using birth-year cutoffs (e.g., Silent Generation, Baby Boomers, Gen X, Millennials, Gen Z). These bins are fixed across years to enable life-cycle comparisons.

6. **Applies person-level weights**

All reported homeownership rates use `ASECWT` (person-level ASEC weight), producing nationally representative estimates of individuals living in owner-occupied housing.

---

### Step 3: Aggregation

The script produces weighted homeownership rates by:

- Age  
- Generation  
- Year  

Optional subgroup summaries include:

- Marital status  
- Sex  
- Race  
- Metro status  
- State or region  

All rates are calculated as weighted means:

homeownership_rate = weighted.mean(homeowner, ASECWT)


---

### Step 4: Outputs

#### Clean Summary Datasets

Saved to `/output/`, including:

- `age_averages.csv`
- `age_averages_married.csv`
- `age_averages_unmarried.csv`
- `age_averages_sex.csv`
- `age_averages_race.csv`
- `age_averages_metro.csv`

These datasets are structured for:

- Publication-quality static figures  
- Shiny app integration  
- External sharing  

#### Figures

Saved to `/figures/`:

- Life-cycle homeownership curves by generation  
- Married vs. unmarried comparisons  
- Metro vs. non-metro splits  
- Demographic breakdowns  

Plots use a standardized EIG theme defined in `theme_eig.R`.

---

## Assumptions and Methodological Notes

### Ownership Definition

- Ownership is determined at the household level using `OWNERSHP`.
- Only householders and spouses are used to define homeownership status.
- Other household members are excluded from the ownership classification.

### Age Restriction

- Sample limited to individuals aged 19 and older.
- Ensures comparability over time and focuses on economically meaningful ages.

### Weights

- Person-level ASEC weight (`ASECWT`) used for all individual-level estimates.
- Ensures national representativeness.
- Household weights (`ASECWTH`) are not used for primary estimates.

### Cohort Construction

- Cohorts are defined by birth year.
- No dynamic reclassification over time.
- Enables life-cycle comparisons across generations at the same age.

### Inflation

This repository focuses on tenure rates (shares), not income levels.  
No inflation adjustment is required for homeownership rates.

---

## Reproducibility

To reproduce results:

1. Obtain an IPUMS CPS extract matching `cps_00051`.
2. Set your API key in your environment: IPUMS_API_KEY=your_key_here
3. Run:
source("run_all.R")

All outputs will regenerate in `/output/`.

---

## Intended Use

This repository supports:

- Research papers  
- Policy briefs  
- Public-facing visualizations  
- Interactive dashboards  
- Shiny applications  

The structured outputs are designed for downstream integration without additional reshaping.

---

## License and Data Use

Users must comply with IPUMS CPS data-use conditions.  
Raw microdata are not distributed in this repository.

---

## Author

Ben Glasner  
Economic Innovation Group


