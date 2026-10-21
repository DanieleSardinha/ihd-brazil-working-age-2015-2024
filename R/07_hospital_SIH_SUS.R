# 07_hospital_SIH_SUS.R
# Acute myocardial infarction (ICD-10 I21) hospital burden, SIH/SUS, age 20-59.
#
# Reconstructs national totals directly from the four TabNet Excel exports:
#   local_data/SIH_SUS/Internações agregado.xlsx
#   local_data/SIH_SUS/Óbitos.xlsx
#   local_data/SIH_SUS/Dias de permanencia.xlsx
#   local_data/SIH_SUS/Valor total.xlsx
#
# "Valor total" is SIH/SUS reimbursement, not a comprehensive health-system
# resource-cost measure. Nominal reimbursements are converted to constant
# 2024 BRL using the ratio of the annual mean monthly IPCA number index in 2024
# to the corresponding annual mean index for each year.

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

find_repo_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(wd, "run_all.R"))) return(wd)
  parent <- normalizePath(file.path(wd, ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(parent, "run_all.R"))) return(parent)
  stop("Repository root not found.")
}

repo <- find_repo_root()
sih_dir <- file.path(repo, "local_data", "SIH_SUS")
out_dir <- file.path(repo, "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

required <- file.path(
  sih_dir,
  c("Internações agregado.xlsx","Óbitos.xlsx","Dias de permanencia.xlsx","Valor total.xlsx")
)
if (any(!file.exists(required))) {
  stop("Missing SIH/SUS local input files:\n", paste(required[!file.exists(required)], collapse = "\n"))
}

read_tabnet_brazil <- function(file) {
  raw <- as.data.table(read_excel(file, sheet = 1, col_names = FALSE))

  first <- trimws(as.character(raw[[1]]))
  header_row <- which(first == "Região/Unidade da Federação")
  if (!length(header_row)) {
    stop("Header 'Região/Unidade da Federação' not found in: ", basename(file))
  }
  header_row <- header_row[1]

  header <- trimws(as.character(unlist(raw[header_row], use.names = FALSE)))
  years <- as.character(2015:2024)
  year_cols <- match(years, header)
  if (anyNA(year_cols)) {
    stop("Could not locate all 2015-2024 columns in: ", basename(file))
  }

  dat <- raw[(header_row + 1):nrow(raw)]
  territory <- trimws(as.character(dat[[1]]))
  idx_region <- grepl("^Região ", territory)
  regions <- dat[idx_region]

  if (nrow(regions) != 5L) {
    stop("Expected exactly 5 regional rows in ", basename(file), "; found ", nrow(regions))
  }

  vals <- sapply(seq_along(years), function(i) {
    v <- suppressWarnings(as.numeric(as.character(regions[[year_cols[i]]])))
    sum(v, na.rm = TRUE)
  })

  data.table(year = as.integer(years), value = as.numeric(vals))
}

adm <- read_tabnet_brazil(file.path(sih_dir, "Internações agregado.xlsx"))
setnames(adm, "value", "hospitalizations")

deaths <- read_tabnet_brazil(file.path(sih_dir, "Óbitos.xlsx"))
setnames(deaths, "value", "in_hospital_deaths")

days <- read_tabnet_brazil(file.path(sih_dir, "Dias de permanencia.xlsx"))
setnames(days, "value", "hospital_days")

value <- read_tabnet_brazil(file.path(sih_dir, "Valor total.xlsx"))
setnames(value, "value", "valor_total_nominal")

hospital <- Reduce(
  function(x, y) merge(x, y, by = "year", all = TRUE),
  list(adm, deaths, days, value)
)
setorder(hospital, year)

hospital[, in_hospital_mortality_percent :=
           100 * in_hospital_deaths / hospitalizations]
hospital[, mean_length_of_stay_days :=
           hospital_days / hospitalizations]

# Annual mean IPCA number index values used in the final analysis.
ipca <- data.table(
  year = 2015:2024,
  ipca_index_mean = c(
    4310.1191667,
    4686.7866667,
    4848.3108333,
    5025.9941667,
    5213.6133333,
    5381.0625000,
    5827.7800000,
    6368.6041667,
    6661.1500000,
    6952.0733333
  )
)
ipca[, adjustment_to_2024 :=
       ipca[year == 2024, ipca_index_mean] / ipca_index_mean]

hospital <- merge(hospital, ipca, by = "year", all.x = TRUE)
hospital[, reimbursement_2024_brl :=
           valor_total_nominal * adjustment_to_2024]
hospital[, reimbursement_per_admission_2024_brl :=
           reimbursement_2024_brl / hospitalizations]

audit_raw <- data.table(
  indicator = c("Hospitalizations","In-hospital deaths","Hospital days"),
  observed = c(
    sum(hospital$hospitalizations),
    sum(hospital$in_hospital_deaths),
    sum(hospital$hospital_days)
  ),
  expected = c(511937, 26009, 3378856)
)
audit_raw[, difference := observed - expected]
audit_raw[, match := difference == 0]

expected_million <- c(234,230,235,240,253,255,253,283,303,349)
expected_per_admission <- c(5736,5362,5337,5238,5056,5108,4756,4747,4884,5474)

audit_annual <- hospital[, .(
  year,
  reimbursement_million = reimbursement_2024_brl / 1e6,
  reimbursement_per_admission = reimbursement_per_admission_2024_brl
)]
audit_annual[, expected_million := expected_million]
audit_annual[, expected_per_admission := expected_per_admission]
audit_annual[, rounded_million := round(reimbursement_million)]
audit_annual[, rounded_per_admission := round(reimbursement_per_admission)]
audit_annual[, match :=
               rounded_million == expected_million &
               rounded_per_admission == expected_per_admission]

total_adjusted <- sum(hospital$reimbursement_2024_brl)
total_nominal <- sum(hospital$valor_total_nominal)

summary_hospital <- data.table(
  hospitalizations = sum(hospital$hospitalizations),
  in_hospital_deaths = sum(hospital$in_hospital_deaths),
  hospital_days = sum(hospital$hospital_days),
  in_hospital_mortality_percent =
    100 * sum(hospital$in_hospital_deaths) / sum(hospital$hospitalizations),
  mean_length_of_stay_days =
    sum(hospital$hospital_days) / sum(hospital$hospitalizations),
  nominal_reimbursement_brl = total_nominal,
  constant_2024_reimbursement_brl = total_adjusted,
  constant_2024_reimbursement_billion = total_adjusted / 1e9
)

fwrite(hospital, file.path(out_dir, "hospital_I21_20_59_2015_2024.csv"))
fwrite(summary_hospital, file.path(out_dir, "hospital_I21_20_59_summary.csv"))
fwrite(ipca, file.path(out_dir, "ipca_adjustment_to_2024.csv"))
fwrite(audit_raw, file.path(out_dir, "audit_hospital_raw_counts.csv"))
fwrite(audit_annual, file.path(out_dir, "audit_hospital_reimbursement.csv"))

cat("\nHOSPITAL VALIDATION\n")
print(summary_hospital)
cat("\nRaw count audit:\n")
print(audit_raw)
cat("\nAnnual reimbursement audit:\n")
print(audit_annual)

if (!all(audit_raw$match)) stop("Raw SIH/SUS count audit failed.")
if (!all(audit_annual$match)) stop("Annual reimbursement audit failed.")

if (abs(total_adjusted - 2634149149.1363) > 1000) {
  stop("Constant-2024 reimbursement total differs from validated result.")
}

cat("07_hospital_SIH_SUS.R completed successfully.\n")
