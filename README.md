# Homeownership by Generation

This project analyzes how homeownership rates have changed across generations and age using CPS ASEC microdata from IPUMS. The analysis focuses on individuals aged 19 and older and classifies homeownership based on household relationship and tenure status.

## Data Source

- **CPS ASEC Microdata (IPUMS-CPS)**
- Variables used: `AGE`, `YEAR`, `RELATE`, `OWNERSHP`, `ASECWTH`

## Key Definitions

- **Homeowner**: Individuals aged 19+ who are householders or spouses (`RELATE` codes 101, 201, 202, 203) and live in owned or mortgaged homes (`OWNERSHP == 10`).
- **Generation**: Derived from birth year (e.g., Boomers, Gen-X, Millennials, Gen-Z).

## Output

- A cleaned dataset summarizing weighted average homeownership by generation and age.

## Usage

To run the analysis:
1. Download CPS data and corresponding DDI from [IPUMS-CPS](https://cps.ipums.org/).
2. Place the files in the `/data` folder.
3. Run the `R` script to generate summary data and interactive plots.

## Author

Ben Glasner – Economic Innovation Group
