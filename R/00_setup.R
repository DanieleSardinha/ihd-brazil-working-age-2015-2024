# ==============================================================================
# 00_setup.R
# Repository setup and reproducibility checks
# ==============================================================================

options(stringsAsFactors = FALSE)

required_packages <- c(
  "data.table",
  "read.dbc",
  "readxl",
  "segmented",
  "nlme"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them before running the workflow."
  )
}

required_dirs <- c(
  file.path("local_data", "SIM"),
  file.path("local_data", "SIH_SUS"),
  file.path("local_data", "IBGE"),
  file.path("local_data", "derived"),
  "outputs"
)

invisible(lapply(
  required_dirs,
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

capture.output(
  sessionInfo(),
  file = file.path("outputs", "sessionInfo.txt")
)

cat("\nRepository setup completed.\n")
cat("Working directory:", normalizePath("."), "\n")
