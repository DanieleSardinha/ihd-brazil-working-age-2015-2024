# ==============================================================================
# run_all.R
# Study: Working-age mortality from ischemic heart disease in Brazil, 2015-2024
#
# Run the complete analytical workflow from the repository root.
# ==============================================================================

scripts <- c(
  "00_setup.R",
  "01_data_preparation.R",
  "02_mortality_rates_standardization.R",
  "03_temporal_trends.R",
  "04_sociodemographic_distribution.R",
  "05_ypll.R",
  "06_data_quality_R00_R99.R",
  "07_hospital_SIH_SUS.R",
  "08_productivity_losses.R"
)

cat("\n============================================================\n")
cat("FULL ANALYTICAL WORKFLOW\n")
cat("============================================================\n")

for (s in scripts) {

  script_path <- file.path("R", s)

  if (!file.exists(script_path)) {
    stop("Required script not found: ", script_path)
  }

  message("\n========== Running ", script_path, " ==========")

  source(
    script_path,
    echo = FALSE,
    chdir = FALSE
  )
}

cat("\n============================================================\n")
cat("FULL ANALYTICAL WORKFLOW COMPLETED SUCCESSFULLY\n")
cat("============================================================\n")
