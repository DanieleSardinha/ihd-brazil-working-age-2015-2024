# 02_mortality_rates_standardization.R
# Working-age IHD mortality in Brazil, 2015-2024
# Validated workflow for direct age standardization and corrected variance.
#
# Requires:
#   local_data/derived/sim_ihd_20_59_2015_2024.rds
#   local_data/derived/sim_ihd_30_69_2015_2024.rds
#   local_data/IBGE/pop_IBGE_Brasil_idade5_2015_2024.csv
#   local_data/IBGE/pop_IBGE_regiao_idade5_2015_2024.csv
#   local_data/IBGE/pop_IBGE_sexo_idade5_2015_2024.csv
#   local_data/IBGE/pop_IBGE_revisao2024_idade5_sexo_UF_2015_2024.rds
#
# Notes:
# - WHO 2000-2025 standard weights are renormalized within each analyzed age range.
# - Correct variance:
#   Var(ASMR) = 100000^2 * sum(w_i^2 * d_i / P_i^2)

suppressPackageStartupMessages({
  library(data.table)
})

find_repo_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(wd, "run_all.R"))) return(wd)
  parent <- normalizePath(file.path(wd, ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(parent, "run_all.R"))) return(parent)
  stop("Repository root not found. Run from the repository root or R/ directory.")
}

repo <- find_repo_root()
dir.create(file.path(repo, "outputs"), recursive = TRUE, showWarnings = FALSE)

derived_dir <- file.path(repo, "local_data", "derived")
ibge_dir <- file.path(repo, "local_data", "IBGE")

required_files <- c(
  file.path(derived_dir, "sim_ihd_20_59_2015_2024.rds"),
  file.path(derived_dir, "sim_ihd_30_69_2015_2024.rds"),
  file.path(ibge_dir, "pop_IBGE_Brasil_idade5_2015_2024.csv"),
  file.path(ibge_dir, "pop_IBGE_regiao_idade5_2015_2024.csv"),
  file.path(ibge_dir, "pop_IBGE_sexo_idade5_2015_2024.csv"),
  file.path(ibge_dir, "pop_IBGE_revisao2024_idade5_sexo_UF_2015_2024.rds")
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop(
    "Missing required local files:\n",
    paste(missing_files, collapse = "\n")
  )
}

d20 <- as.data.table(readRDS(file.path(derived_dir, "sim_ihd_20_59_2015_2024.rds")))
d30 <- as.data.table(readRDS(file.path(derived_dir, "sim_ihd_30_69_2015_2024.rds")))

# Flexible year variable name.
year_col20 <- intersect(c("ANO_ARQUIVO", "year", "ano"), names(d20))[1]
year_col30 <- intersect(c("ANO_ARQUIVO", "year", "ano"), names(d30))[1]
if (is.na(year_col20) || is.na(year_col30)) stop("Year variable not found in derived SIM files.")

if (!"idade5" %in% names(d20)) {
  if (!"idade_anos" %in% names(d20)) stop("Neither idade5 nor idade_anos found in 20-59 SIM file.")
  d20[, idade5 := cut(
    idade_anos,
    breaks = c(20,25,30,35,40,45,50,55,60),
    right = FALSE,
    labels = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59")
  )]
}
if (!"idade5" %in% names(d30)) {
  if (!"idade_anos" %in% names(d30)) stop("Neither idade5 nor idade_anos found in 30-69 SIM file.")
  d30[, idade5 := cut(
    idade_anos,
    breaks = c(30,35,40,45,50,55,60,65,70),
    right = FALSE,
    labels = c("30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69")
  )]
}

setnames(d20, year_col20, "ANO_ARQUIVO")
setnames(d30, year_col30, "ANO_ARQUIVO")

# Ensure expected category labels if available.
if ("SEXO" %in% names(d20) && !"sexo_cat" %in% names(d20)) {
  d20[, sexo_cat := fifelse(as.character(SEXO) == "1", "Male",
                     fifelse(as.character(SEXO) == "2", "Female", NA_character_))]
}

if (!"regiao" %in% names(d20) && "CODMUNRES" %in% names(d20)) {
  uf <- suppressWarnings(as.integer(substr(as.character(CODMUNRES), 1, 2)))
  d20[, regiao := fcase(
    uf %in% c(11,12,13,14,15,16,17), "North",
    uf %in% c(21,22,23,24,25,26,27,28,29), "Northeast",
    uf %in% c(31,32,33,35), "Southeast",
    uf %in% c(41,42,43), "South",
    uf %in% c(50,51,52,53), "Central-West",
    default = NA_character_
  )]
}

# WHO weights
who20 <- data.table(
  idade5 = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"),
  peso_bruto = c(8.22,7.93,7.61,7.15,6.59,6.04,5.37,4.55)
)
who20[, peso := peso_bruto / sum(peso_bruto)]

who30 <- data.table(
  idade5 = c("30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69"),
  peso_bruto = c(7.61,7.15,6.59,6.04,5.37,4.55,3.72,2.96)
)
who30[, peso := peso_bruto / sum(peso_bruto)]

standardize <- function(num, den, weights, strata = NULL) {
  keys <- c("ANO_ARQUIVO", strata, "idade5")
  x <- merge(num, den, by = keys, all = FALSE)
  x <- merge(x, weights[, .(idade5, peso)], by = "idade5", all.x = TRUE)

  if (anyNA(x$obitos) || anyNA(x$populacao) || anyNA(x$peso)) {
    stop("Missing values after numerator/denominator/weight merge.")
  }

  x[, specific_rate := obitos / populacao]
  x[, component_asmr := peso * specific_rate]
  x[, component_variance := peso^2 * obitos / populacao^2]

  by_vars <- c("ANO_ARQUIVO", strata)
  out <- x[, .(
    deaths = sum(obitos),
    population = sum(populacao),
    crude_rate = 100000 * sum(obitos) / sum(populacao),
    ASMR = 100000 * sum(component_asmr),
    variance = 100000^2 * sum(component_variance)
  ), by = by_vars]

  out[, SE := sqrt(variance)]
  out[, CI_lower := ASMR - 1.96 * SE]
  out[, CI_upper := ASMR + 1.96 * SE]
  setorderv(out, by_vars)
  out[]
}

# ------------------------------------------------------------------
# 20-59 national
# ------------------------------------------------------------------
num_br <- d20[, .(obitos = .N), by = .(ANO_ARQUIVO, idade5)]
pop_br <- fread(file.path(ibge_dir, "pop_IBGE_Brasil_idade5_2015_2024.csv"))

asmr_20_59 <- standardize(num_br, pop_br, who20)

expected_national <- data.table(
  ANO_ARQUIVO = 2015:2024,
  ASMR_expected = c(
    23.0749129,23.2050395,21.9146421,21.2643253,20.9859674,
    19.2589157,19.7275536,20.5451029,19.8886304,19.6330647
  ),
  SE_expected = c(
    0.141386129,0.140379838,0.135161199,0.131971040,0.130023030,
    0.123709514,0.124413497,0.126237779,0.123461053,0.121942797
  )
)

audit_national <- merge(asmr_20_59, expected_national, by = "ANO_ARQUIVO")
audit_national[, match := abs(ASMR - ASMR_expected) < 1e-6 & abs(SE - SE_expected) < 1e-6]

# ------------------------------------------------------------------
# 20-59 regional
# ------------------------------------------------------------------
num_reg <- d20[!is.na(regiao), .(obitos = .N), by = .(ANO_ARQUIVO, regiao, idade5)]
pop_reg <- fread(file.path(ibge_dir, "pop_IBGE_regiao_idade5_2015_2024.csv"))
asmr_reg <- standardize(num_reg, pop_reg, who20, "regiao")

regional_expected <- data.table(
  ANO_ARQUIVO = rep(2015:2024, each = 5),
  regiao = rep(c("Central-West","North","Northeast","South","Southeast"), times = 10),
  ASMR_expected = c(
    22.24649,18.49730,23.11267,20.42059,24.76803,
    23.12042,18.01272,23.36754,20.21759,24.93099,
    22.18032,18.84572,22.71874,18.82678,22.93298,
    21.86541,17.85218,22.56615,17.81113,22.14052,
    20.14748,17.82901,22.09118,16.63020,22.51350,
    19.88672,16.77965,19.53462,15.99550,20.55669,
    22.37855,18.12712,19.58168,15.86625,20.95271,
    21.78094,16.44438,20.72023,17.05054,22.15349,
    20.31231,17.36528,20.95625,15.80293,21.03804,
    20.10482,17.27263,20.80696,15.42509,20.76015
  ),
  lower_expected = c(
    21.24463,17.53491,22.55559,19.76381,24.34113,
    22.11442,17.07807,22.81458,19.56898,24.50583,
    21.20921,17.90398,22.18007,18.20605,22.52799,
    20.91438,16.94959,22.03552,17.21032,21.74519,
    19.24651,16.94015,21.57214,16.05402,22.11675,
    19.00287,15.93006,19.05155,15.43304,20.17849,
    21.45042,17.25787,19.10242,15.30854,20.57241,
    20.87471,15.62606,20.23117,16.47336,21.76380,
    19.44564,16.53512,20.46806,15.25038,20.65989,
    19.25160,16.45609,20.32449,14.87963,20.38567
  ),
  upper_expected = c(
    23.24834,19.45969,23.66975,21.07736,25.19493,
    24.12641,18.94737,23.92050,20.86620,25.35616,
    23.15144,19.78745,23.25742,19.44751,23.33796,
    22.81645,18.75476,23.09678,18.41194,22.53586,
    21.04846,18.71787,22.61021,17.20638,22.91025,
    20.77056,17.62924,20.01768,16.55796,20.93488,
    23.30667,18.99636,20.06093,16.42396,21.33301,
    22.68718,17.26270,21.20928,17.62772,22.54318,
    21.17898,18.19545,21.44443,16.35549,21.41619,
    20.95805,18.08918,21.28942,15.97056,21.13464
  )
)

audit_regional <- merge(asmr_reg, regional_expected, by = c("ANO_ARQUIVO","regiao"))
audit_regional[, match :=
  round(ASMR,5) == ASMR_expected &
  round(CI_lower,5) == lower_expected &
  round(CI_upper,5) == upper_expected
]

# ------------------------------------------------------------------
# 20-59 sex-specific rates + male/female rate ratio
# ------------------------------------------------------------------
num_sex <- d20[sexo_cat %in% c("Male","Female"),
               .(obitos = .N), by = .(ANO_ARQUIVO, sexo_cat, idade5)]
pop_sex <- fread(file.path(ibge_dir, "pop_IBGE_sexo_idade5_2015_2024.csv"))
asmr_sex <- standardize(num_sex, pop_sex, who20, "sexo_cat")

male <- asmr_sex[sexo_cat == "Male",
                 .(ANO_ARQUIVO, ASMR_male = ASMR, var_male = variance)]
female <- asmr_sex[sexo_cat == "Female",
                   .(ANO_ARQUIVO, ASMR_female = ASMR, var_female = variance)]
rr_sex <- merge(male, female, by = "ANO_ARQUIVO")
rr_sex[, RR := ASMR_male / ASMR_female]
rr_sex[, SE_log_RR := sqrt(var_male / ASMR_male^2 + var_female / ASMR_female^2)]
rr_sex[, RR_lower := exp(log(RR) - 1.96 * SE_log_RR)]
rr_sex[, RR_upper := exp(log(RR) + 1.96 * SE_log_RR)]

rr_expected <- data.table(
  ANO_ARQUIVO = 2015:2024,
  RR_expected = c(2.482,2.419,2.536,2.498,2.547,2.626,2.572,2.628,2.664,2.726),
  lower_expected = c(2.418,2.358,2.470,2.433,2.480,2.554,2.503,2.559,2.594,2.654),
  upper_expected = c(2.548,2.482,2.603,2.565,2.615,2.699,2.642,2.698,2.737,2.801)
)
audit_rr <- merge(rr_sex, rr_expected, by = "ANO_ARQUIVO")
audit_rr[, match :=
  round(RR,3) == RR_expected &
  round(RR_lower,3) == lower_expected &
  round(RR_upper,3) == upper_expected
]

# ------------------------------------------------------------------
# 30-69 sensitivity
# ------------------------------------------------------------------
num_30_69 <- d30[, .(obitos = .N), by = .(ANO_ARQUIVO, idade5)]

pop_full <- as.data.table(readRDS(
  file.path(ibge_dir, "pop_IBGE_revisao2024_idade5_sexo_UF_2015_2024.rds")
))
pop_30_69 <- pop_full[
  sigla == "BR" & sexo_cat == "Both" &
    idade5 %in% who30$idade5 & ano %in% 2015:2024,
  .(ANO_ARQUIVO = ano, idade5, populacao)
]

asmr_30_69 <- standardize(num_30_69, pop_30_69, who30)

expected30 <- data.table(
  ANO_ARQUIVO = 2015:2024,
  deaths_expected = c(51566,53626,52674,52663,53441,49312,51651,54239,53630,53827),
  ASMR_expected = c(55.37,55.86,53.30,51.80,51.14,46.05,47.18,48.50,46.89,46.05),
  lower_expected = c(54.89,55.39,52.84,51.36,50.71,45.64,46.77,48.09,46.49,45.66),
  upper_expected = c(55.84,56.34,53.75,52.24,51.58,46.45,47.59,48.91,47.29,46.44)
)
audit30 <- merge(asmr_30_69, expected30, by = "ANO_ARQUIVO")
audit30[, match :=
  deaths == deaths_expected &
  round(ASMR,2) == ASMR_expected &
  round(CI_lower,2) == lower_expected &
  round(CI_upper,2) == upper_expected
]

# ------------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------------
fwrite(asmr_20_59, file.path(repo, "outputs", "ASMR_Brazil_20_59_corrected_variance.csv"))
fwrite(asmr_reg, file.path(repo, "outputs", "ASMR_by_region_20_59_corrected_variance.csv"))
fwrite(asmr_sex, file.path(repo, "outputs", "ASMR_by_sex_20_59_corrected_variance.csv"))
fwrite(rr_sex, file.path(repo, "outputs", "male_female_rate_ratio_corrected_CI.csv"))
fwrite(asmr_30_69, file.path(repo, "outputs", "ASMR_Brazil_30_69_sensitivity_corrected_variance.csv"))

fwrite(audit_national, file.path(repo, "outputs", "audit_ASMR_Brazil_20_59.csv"))
fwrite(audit_regional, file.path(repo, "outputs", "audit_ASMR_by_region.csv"))
fwrite(audit_rr, file.path(repo, "outputs", "audit_male_female_rate_ratio.csv"))
fwrite(audit30, file.path(repo, "outputs", "audit_ASMR_30_69.csv"))

cat("\nVALIDATION SUMMARY\n")
cat("National 20-59:", all(audit_national$match), "\n")
cat("Regional 20-59:", all(audit_regional$match),
    "(", sum(audit_regional$match), "/", nrow(audit_regional), ")\n")
cat("Male/Female RR:", all(audit_rr$match), "\n")
cat("Sensitivity 30-69:", all(audit30$match), "\n")

if (!all(c(
  all(audit_national$match),
  all(audit_regional$match),
  all(audit_rr$match),
  all(audit30$match)
))) {
  stop("At least one mortality-rate validation failed.")
}

cat("02_mortality_rates_standardization.R completed successfully.\n")
