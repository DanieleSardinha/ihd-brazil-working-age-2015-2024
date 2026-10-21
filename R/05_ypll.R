# 05_ypll.R
# Years of Potential Life Lost (YPLL) for IHD deaths, Brazil, 2015-2024.
#
# Primary analysis:
#   upper age limit = 60 years
# Sensitivity analysis:
#   upper age limit = 70 years
#
# Age-at-death midpoint correction:
#   completed age x is represented as x + 0.5 years.
#
# Expected validated totals:
#   deaths = 260,493
#   YPLL60 = 2,438,458.5
#   mean YPLL60 = 9.36093676222
#   median YPLL60 = 7.5
#   YPLL70 = 5,043,388.5
#   mean YPLL70 = 19.3609367622

suppressPackageStartupMessages({
  library(data.table)
})

find_repo_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(wd, "run_all.R"))) return(wd)

  parent <- normalizePath(
    file.path(wd, ".."),
    winslash = "/",
    mustWork = FALSE
  )

  if (file.exists(file.path(parent, "run_all.R"))) return(parent)

  stop(
    "Repository root not found. Run this script from the repository root ",
    "or from the R/ directory."
  )
}

repo <- find_repo_root()

derived_dir <- file.path(
  repo,
  "local_data",
  "derived"
)

out_dir <- file.path(
  repo,
  "outputs"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

input_file <- file.path(
  derived_dir,
  "sim_ihd_20_59_2015_2024.rds"
)

if (!file.exists(input_file)) {
  stop(
    "Required local file not found:\n",
    input_file,
    "\nRun 01_data_preparation.R first."
  )
}

d <- as.data.table(
  readRDS(input_file)
)

# ------------------------------------------------------------
# Harmonize expected variables
# ------------------------------------------------------------

year_var <- intersect(
  c("ANO_ARQUIVO", "year", "ano"),
  names(d)
)[1]

if (is.na(year_var)) {
  stop("Year variable not found.")
}

if (year_var != "ANO_ARQUIVO") {
  setnames(
    d,
    year_var,
    "ANO_ARQUIVO"
  )
}

if (!"idade_anos" %in% names(d)) {
  stop(
    "Variable 'idade_anos' was not found in the derived SIM file."
  )
}

# Derive region if not already available.
if (!"regiao" %in% names(d)) {

  if (!"CODMUNRES" %in% names(d)) {
    stop(
      "Neither 'regiao' nor 'CODMUNRES' was found."
    )
  }

  uf <- suppressWarnings(
    as.integer(
      substr(
        as.character(d$CODMUNRES),
        1,
        2
      )
    )
  )

  d[
    ,
    regiao := fcase(
      uf %in% c(11,12,13,14,15,16,17),
      "North",

      uf %in% c(21,22,23,24,25,26,27,28,29),
      "Northeast",

      uf %in% c(31,32,33,35),
      "Southeast",

      uf %in% c(41,42,43),
      "South",

      uf %in% c(50,51,52,53),
      "Central-West",

      default = NA_character_
    )
  ]
}

# ------------------------------------------------------------
# Midpoint correction and YPLL
# ------------------------------------------------------------

d[
  ,
  age_midpoint := idade_anos + 0.5
]

d[
  ,
  YPLL60 := pmax(
    60 - age_midpoint,
    0
  )
]

d[
  ,
  YPLL70 := pmax(
    70 - age_midpoint,
    0
  )
]

# ------------------------------------------------------------
# Overall summary
# ------------------------------------------------------------

summary_ypll <- data.table(
  deaths = nrow(d),

  YPLL60 = sum(
    d$YPLL60
  ),

  mean_YPLL60 = mean(
    d$YPLL60
  ),

  median_YPLL60 = median(
    d$YPLL60
  ),

  YPLL70 = sum(
    d$YPLL70
  ),

  mean_YPLL70 = mean(
    d$YPLL70
  )
)

# ------------------------------------------------------------
# Annual results
# ------------------------------------------------------------

ypll_annual <- d[
  ,
  .(
    deaths = .N,

    YPLL60 = sum(
      YPLL60
    ),

    mean_YPLL60 = mean(
      YPLL60
    ),

    YPLL70 = sum(
      YPLL70
    ),

    mean_YPLL70 = mean(
      YPLL70
    )
  ),
  by = ANO_ARQUIVO
]

setorder(
  ypll_annual,
  ANO_ARQUIVO
)

# ------------------------------------------------------------
# Regional results
# ------------------------------------------------------------

ypll_region <- d[
  !is.na(regiao),
  .(
    deaths = .N,

    YPLL60 = sum(
      YPLL60
    ),

    mean_YPLL60 = mean(
      YPLL60
    )
  ),
  by = regiao
]

ypll_region[
  ,
  share_percent :=
    100 * YPLL60 /
    sum(YPLL60)
]

setorder(
  ypll_region,
  -YPLL60
)

ypll_region_year <- d[
  !is.na(regiao),
  .(
    deaths = .N,

    YPLL60 = sum(
      YPLL60
    ),

    mean_YPLL60 = mean(
      YPLL60
    )
  ),
  by = .(
    ANO_ARQUIVO,
    regiao
  )
]

setorder(
  ypll_region_year,
  ANO_ARQUIVO,
  regiao
)

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

expected_annual <- data.table(
  ANO_ARQUIVO = 2015:2024,

  deaths_expected = c(
    26644,
    27339,
    26310,
    25993,
    26093,
    24288,
    25208,
    26566,
    26036,
    26016
  ),

  YPLL60_expected = c(
    249274.0,
    254720.5,
    241989.0,
    237733.5,
    239067.5,
    229412.0,
    239082.0,
    251182.0,
    247589.0,
    248409.0
  )
)

audit_annual <- merge(
  ypll_annual,
  expected_annual,
  by = "ANO_ARQUIVO"
)

audit_annual[
  ,
  match :=
    deaths == deaths_expected &
    abs(
      YPLL60 -
      YPLL60_expected
    ) < 1e-10
]

expected_region <- data.table(
  regiao = c(
    "Southeast",
    "Northeast",
    "South",
    "Central-West",
    "North"
  ),

  deaths_expected = c(
    122595,
    68119,
    33576,
    20541,
    15662
  ),

  YPLL60_expected = c(
    1142443.5,
    662871.5,
    283352.0,
    191263.5,
    158528.0
  )
)

audit_region <- merge(
  ypll_region,
  expected_region,
  by = "regiao"
)

audit_region[
  ,
  match :=
    deaths == deaths_expected &
    abs(
      YPLL60 -
      YPLL60_expected
    ) < 1e-10
]

overall_match <- with(
  summary_ypll,
  deaths == 260493 &
  abs(YPLL60 - 2438458.5) < 1e-10 &
  abs(mean_YPLL60 - 9.36093676222) < 1e-9 &
  abs(median_YPLL60 - 7.5) < 1e-10 &
  abs(YPLL70 - 5043388.5) < 1e-10 &
  abs(mean_YPLL70 - 19.3609367622) < 1e-9
)

# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------

fwrite(
  summary_ypll,
  file.path(
    out_dir,
    "YPLL_primary_sensitivity_summary.csv"
  )
)

fwrite(
  ypll_annual,
  file.path(
    out_dir,
    "YPLL60_annual_results.csv"
  )
)

fwrite(
  ypll_region,
  file.path(
    out_dir,
    "YPLL60_by_region_total.csv"
  )
)

fwrite(
  ypll_region_year,
  file.path(
    out_dir,
    "YPLL60_by_region_year.csv"
  )
)

fwrite(
  audit_annual,
  file.path(
    out_dir,
    "audit_YPLL60_annual.csv"
  )
)

fwrite(
  audit_region,
  file.path(
    out_dir,
    "audit_YPLL60_region.csv"
  )
)

cat(
  "\n============================================\n"
)

cat(
  "YPLL VALIDATION\n"
)

cat(
  "============================================\n"
)

print(
  summary_ypll
)

cat(
  "\nAnnual validation:",
  all(audit_annual$match),
  "(",
  sum(audit_annual$match),
  "/",
  nrow(audit_annual),
  ")\n"
)

cat(
  "Regional validation:",
  all(audit_region$match),
  "(",
  sum(audit_region$match),
  "/",
  nrow(audit_region),
  ")\n"
)

cat(
  "Overall validation:",
  overall_match,
  "\n"
)

if (
  !overall_match ||
  !all(audit_annual$match) ||
  !all(audit_region$match)
) {
  stop(
    "At least one YPLL validation failed."
  )
}

cat(
  "05_ypll.R completed successfully.\n"
)
