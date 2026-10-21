# Working-age mortality from ischemic heart disease in Brazil, 2015–2024

## Overview

This repository contains the reproducible R code associated with the study:

**Working-age mortality from ischemic heart disease in Brazil, 2015–2024:
temporal trends and years of potential life lost**

The study evaluates mortality from ischemic heart disease (IHD) among
working-age adults in Brazil between 2015 and 2024, including age-standardized
mortality rates, temporal trends, sex and regional patterns, years of potential
life lost (YPLL), data-quality indicators, acute myocardial infarction (AMI)
hospital utilization, and mortality-related productivity losses.

## Data sources

The study uses publicly accessible Brazilian health and demographic sources:

- SIM – Mortality Information System / DATASUS
- SIH/SUS – Hospital Information System / DATASUS
- IBGE – 2024 Revision of population projections
- PNAD Contínua
- IPCA

Raw individual-level records are not redistributed in this repository.

## Main analytical definitions

- Primary mortality population: ages 20–59 years, IHD ICD-10 I20–I25
- Sensitivity mortality population: ages 30–69 years
- Hospital subanalysis: principal diagnosis AMI ICD-10 I21
- Primary YPLL threshold: age 60
- YPLL sensitivity threshold: age 70
- Productivity analysis: all IHD deaths I20–I25
- Base-case productivity discount rate: 5%
- Sensitivity discount rates: 3% and 0%

## Repository structure

```text
R/                 Reproducible R scripts
data/              Source-data access instructions
outputs/           Aggregated reproducible outputs
supplementary/     RECORD checklist and documentation
run_all.R          Complete workflow runner
CITATION.cff       Software citation metadata
```

## Execution order

```text
00_setup.R
01_data_preparation.R
02_mortality_rates_standardization.R
03_temporal_trends.R
04_sex_regional_analyses.R
05_ypll.R
06_data_quality_R00_R99.R
07_hospital_SIH_SUS.R
08_productivity_losses.R
```

From the repository root:

```r
source("run_all.R")
```

See `data/README_data_access.md` for the required local inputs.

## Reporting guideline

The study follows STROBE and the RECORD extension for studies using routinely
collected health data. The completed RECORD checklist is available under
`supplementary/`.

## Code availability

The analytical code is publicly available in this repository. A permanent DOI
for release `v1.0.0` will be added after archival in Zenodo.

## License

MIT License.
