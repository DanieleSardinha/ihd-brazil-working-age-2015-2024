# 08_productivity_losses.R
# Productivity losses associated with working-age IHD mortality,
# Brazil, 2015-2024.
#
# Human-capital approach using:
#   monthly labour income = BRL 3,225
#   annual labour income  = BRL 38,700
#   YPLL threshold        = 60 years
#   midpoint correction   = completed age + 0.5 years
#
# Discount-rate scenarios:
#   5% = base case
#   3% = sensitivity
#   0% = sensitivity
#
# Primary scope:
#   all IHD deaths, ICD-10 I20-I25
#
# Legacy audit:
#   AMI deaths only, ICD-10 I21
#
# Validated final totals:
#   I20-I25:
#     260,493 deaths
#     YPLL60 = 2,438,458.5
#     BRL 65.49389 billion at 5%
#     BRL 74.81119 billion at 3%
#     BRL 94.36834 billion at 0%
#
#   I21 legacy audit:
#     224,425 deaths
#     approx. BRL 57.05911 billion at 5%
#     approx. BRL 65.22466 billion at 3%
#     approx. BRL 82.37336 billion at 0%

suppressPackageStartupMessages({
  library(data.table)
})

find_repo_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

  if (file.exists(file.path(wd, "run_all.R"))) {
    return(wd)
  }

  parent <- normalizePath(
    file.path(wd, ".."),
    winslash = "/",
    mustWork = FALSE
  )

  if (file.exists(file.path(parent, "run_all.R"))) {
    return(parent)
  }

  stop(
    "Repository root not found. Run from the repository root ",
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
# Harmonize variables
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
    "Variable 'idade_anos' was not found."
  )
}

if (!"cid3" %in% names(d)) {

  if (!"CAUSABAS" %in% names(d)) {
    stop(
      "Neither 'cid3' nor 'CAUSABAS' was found."
    )
  }

  d[
    ,
    cid3 :=
      substr(
        toupper(
          trimws(
            as.character(
              CAUSABAS
            )
          )
        ),
        1,
        3
      )
  ]
}

# ------------------------------------------------------------
# YPLL60
# ------------------------------------------------------------

d[
  ,
  age_midpoint :=
    idade_anos + 0.5
]

d[
  ,
  YPLL60 :=
    pmax(
      60 - age_midpoint,
      0
    )
]

# ------------------------------------------------------------
# Human-capital parameters
# ------------------------------------------------------------

monthly_income_brl <- 3225

annual_income_brl <-
  monthly_income_brl * 12

pv_income <- function(
  ypll,
  discount_rate,
  annual_income = annual_income_brl
) {

  if (discount_rate == 0) {

    return(
      annual_income *
        ypll
    )
  }

  annual_income *
    (
      1 -
        (1 + discount_rate)^(-ypll)
    ) /
    discount_rate
}

# ------------------------------------------------------------
# Individual productivity losses
# ------------------------------------------------------------

d[
  ,
  loss_5 :=
    pv_income(
      YPLL60,
      0.05
    )
]

d[
  ,
  loss_3 :=
    pv_income(
      YPLL60,
      0.03
    )
]

d[
  ,
  loss_0 :=
    pv_income(
      YPLL60,
      0
    )
]

# ------------------------------------------------------------
# Primary analysis: all IHD I20-I25
# ------------------------------------------------------------

primary <- d[
  cid3 %in%
    c(
      "I20",
      "I21",
      "I22",
      "I23",
      "I24",
      "I25"
    )
]

prod_total <- primary[
  ,
  .(
    deaths = .N,

    YPLL60 = sum(
      YPLL60
    ),

    loss_5_brl = sum(
      loss_5
    ),

    loss_3_brl = sum(
      loss_3
    ),

    loss_0_brl = sum(
      loss_0
    ),

    loss_5_billion = sum(
      loss_5
    ) / 1e9,

    loss_3_billion = sum(
      loss_3
    ) / 1e9,

    loss_0_billion = sum(
      loss_0
    ) / 1e9,

    mean_loss_5_per_death = mean(
      loss_5
    )
  )
]

# ------------------------------------------------------------
# Annual primary analysis
# ------------------------------------------------------------

prod_annual <- primary[
  ,
  .(
    deaths = .N,

    YPLL60 = sum(
      YPLL60
    ),

    loss_5_brl = sum(
      loss_5
    ),

    loss_3_brl = sum(
      loss_3
    ),

    loss_0_brl = sum(
      loss_0
    ),

    loss_5_billion = sum(
      loss_5
    ) / 1e9,

    loss_3_billion = sum(
      loss_3
    ) / 1e9,

    loss_0_billion = sum(
      loss_0
    ) / 1e9
  ),
  by = ANO_ARQUIVO
]

setorder(
  prod_annual,
  ANO_ARQUIVO
)

# ------------------------------------------------------------
# Validation against final Table 5
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
  ),

  loss5_expected = c(
    6.71,
    6.85,
    6.54,
    6.43,
    6.45,
    6.14,
    6.39,
    6.72,
    6.62,
    6.65
  ),

  loss3_expected = c(
    7.65,
    7.82,
    7.45,
    7.33,
    7.35,
    7.02,
    7.31,
    7.68,
    7.58,
    7.61
  ),

  loss0_expected = c(
    9.65,
    9.86,
    9.36,
    9.20,
    9.25,
    8.88,
    9.25,
    9.72,
    9.58,
    9.61
  )
)

audit_annual <- merge(
  prod_annual,
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
    ) < 1e-10 &
    round(
      loss_5_billion,
      2
    ) == loss5_expected &
    round(
      loss_3_billion,
      2
    ) == loss3_expected &
    round(
      loss_0_billion,
      2
    ) == loss0_expected
]

overall_match <- with(
  prod_total,

  deaths == 260493 &

    abs(
      YPLL60 -
      2438458.5
    ) < 1e-10 &

    abs(
      loss_5_billion -
      65.49389
    ) < 1e-4 &

    abs(
      loss_3_billion -
      74.81119
    ) < 1e-4 &

    abs(
      loss_0_billion -
      94.36834
    ) < 1e-4 &

    abs(
      mean_loss_5_per_death -
      251422.9
    ) < 1
)

# ------------------------------------------------------------
# Legacy audit: I21 only
# ------------------------------------------------------------

legacy_I21 <- d[
  cid3 == "I21",
  .(
    deaths = .N,

    YPLL60 = sum(
      YPLL60
    ),

    loss_5_billion = sum(
      loss_5
    ) / 1e9,

    loss_3_billion = sum(
      loss_3
    ) / 1e9,

    loss_0_billion = sum(
      loss_0
    ) / 1e9
  )
]

legacy_match <- with(
  legacy_I21,

  deaths == 224425 &

    abs(
      loss_5_billion -
      57.05911
    ) < 1e-4 &

    abs(
      loss_3_billion -
      65.22466
    ) < 1e-4 &

    abs(
      loss_0_billion -
      82.37336
    ) < 1e-4
)

# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------

fwrite(
  prod_total,
  file.path(
    out_dir,
    "productivity_losses_I20_I25_summary.csv"
  )
)

fwrite(
  prod_annual,
  file.path(
    out_dir,
    "productivity_losses_I20_I25_annual.csv"
  )
)

fwrite(
  audit_annual,
  file.path(
    out_dir,
    "audit_productivity_losses_annual.csv"
  )
)

fwrite(
  legacy_I21,
  file.path(
    out_dir,
    "productivity_losses_I21_legacy_audit.csv"
  )
)

cat(
  "\n============================================\n"
)

cat(
  "PRODUCTIVITY LOSS VALIDATION\n"
)

cat(
  "============================================\n"
)

cat(
  "\nPrimary analysis: I20-I25\n"
)

print(
  prod_total
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
  "Overall validation:",
  overall_match,
  "\n"
)

cat(
  "\nLegacy I21 audit:\n"
)

print(
  legacy_I21
)

cat(
  "Legacy validation:",
  legacy_match,
  "\n"
)

if (
  !all(audit_annual$match) ||
  !overall_match ||
  !legacy_match
) {

  cat(
    "\nDivergent annual rows:\n"
  )

  print(
    audit_annual[
      match == FALSE
    ]
  )

  stop(
    "At least one productivity-loss validation failed."
  )
}

cat(
  "\n08_productivity_losses.R completed successfully.\n"
)
