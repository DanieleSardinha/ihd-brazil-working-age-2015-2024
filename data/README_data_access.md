# Data access and required local inputs

This repository does **not** redistribute individual-level mortality records,
raw SIH/SUS microdata, DBC files, or other source microdata.

The analytical code is designed so that source files remain local under the
`local_data/` directory, which is excluded by `.gitignore`.

## 1. SIM mortality data

Source: Brazilian Mortality Information System (SIM), Ministry of Health /
DATASUS.

Place the annual national mortality DBC files in:

```text
local_data/SIM/
```

Expected filenames:

```text
DOBR2015.dbc
DOBR2016.dbc
DOBR2017.dbc
DOBR2018.dbc
DOBR2019.dbc
DOBR2020.dbc
DOBR2021.dbc
DOBR2022.dbc
DOBR2023.dbc
DOBR2024.dbc
```

`R/01_data_preparation.R` reads these files and creates local analytical RDS
objects. These derived individual-level files must not be committed.

## 2. IBGE population denominators

Source: 2024 Revision of the Brazilian population projections.

Create the local file:

```text
local_data/IBGE/population_2024_revision_age_sex_region.csv
```

Required columns:

```text
year
age
sex
region
population
```

The file should contain disaggregated annual population counts by single year
of age, sex, and Brazilian geographic region for the years required by the
analysis.

## 3. SIH/SUS hospital series

Source: SIH/SUS / DATASUS.

The hospital subanalysis is restricted to SUS-funded admissions among adults
aged 20–59 years with principal diagnosis ICD-10 I21, including all available
four-character subcategories.

Create the local aggregated file:

```text
local_data/SIH_SUS/ami_I21_20_59_2015_2024_aggregated.csv
```

Required columns:

```text
year
hospitalizations
in_hospital_deaths
hospital_days
valor_total_nominal
```

`valor_total_nominal` corresponds to the SIH/SUS field **Valor total**.

## 4. IPCA inflation adjustment

Source: IBGE Broad Consumer Price Index (IPCA).

Create:

```text
local_data/IBGE/ipca_adjustment_to_2024.csv
```

Required columns:

```text
year
adjustment_to_2024
```

`adjustment_to_2024` is the multiplicative factor used to express nominal
annual SIH/SUS reimbursements in constant 2024 Brazilian reais.

## 5. PNAD Contínua earnings parameter

The productivity analysis uses the 2024 national average monthly real labor
income of R$3,225 from PNAD Contínua, corresponding to annual earnings of
R$38,700. This parameter is explicitly stored in
`R/08_productivity_losses.R`.

## Reproducibility

Run the workflow from the repository root:

```r
source("run_all.R")
```

The code writes only aggregated reproducible results to `outputs/`.
