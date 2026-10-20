# Working-age mortality from ischemic heart disease in Brazil, 2015–2024

## Overview

This repository contains the reproducible R code associated with the study:

**Working-age mortality from ischemic heart disease in Brazil, 2015–2024: temporal trends and years of potential life lost**

The study evaluates temporal trends in ischemic heart disease (IHD) mortality among working-age adults in Brazil between 2015 and 2024, including age-standardized mortality rates, years of potential life lost (YPLL), regional and sex-specific patterns, acute myocardial infarction (AMI) hospital utilization, and mortality-related productivity losses.

## Data sources

The analyses use publicly accessible Brazilian health and demographic data:

- **SIM – Mortality Information System**, Brazilian Ministry of Health / DATASUS
- **SIH/SUS – Hospital Information System**, Brazilian Unified Health System / DATASUS
- **IBGE – 2024 Revision of Brazilian population projections**
- **PNAD Contínua – Continuous National Household Sample Survey**
- **IPCA – Broad Consumer Price Index**

SIM mortality records and SIH/SUS hospital data were analyzed independently and were not linked at the individual level.

## Study population

The primary mortality analysis includes Brazilian residents aged 20–59 years whose underlying cause of death was classified as ischemic heart disease according to ICD-10 codes **I20–I25**.

The hospital subanalysis includes SUS-funded admissions among adults aged 20–59 years with acute myocardial infarction as the principal diagnosis (**ICD-10 I21**).

A sensitivity analysis of mortality trends was conducted among adults aged 30–69 years.

## Main analyses

The repository contains R code for:

1. Data preparation and case identification
2. Direct age standardization using the WHO World Standard Population 2000–2025
3. Variance estimation and 95% confidence intervals
4. Sex-specific and regional analyses
5. Weighted segmented log-linear temporal trend models
6. Autocorrelation assessment
7. Years of potential life lost (YPLL)
8. Sensitivity analysis for ages 30–69 years
9. Assessment of ill-defined causes of death (ICD-10 R00–R99)
10. SIH/SUS hospital utilization and reimbursement analyses
11. Mortality-related productivity-loss estimation

## Reproducibility

The analytical workflow was implemented in R.

The scripts will be organized in the order required to reproduce the analyses reported in the manuscript.

Raw individual-level mortality records are **not redistributed in this repository**. Instructions for accessing the original publicly available data sources are provided separately.

No directly identifying personal information is included in this repository.

## Repository structure

```text
R/                 R scripts used in the analyses
data/              Instructions for obtaining and preparing source data
outputs/           Reproducible aggregated outputs where applicable
supplementary/     Reporting checklists and supplementary documentation
