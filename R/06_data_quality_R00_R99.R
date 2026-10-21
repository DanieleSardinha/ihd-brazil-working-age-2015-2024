# 06_data_quality_R00_R99.R
# Data-quality analysis: ill-defined causes of death (ICD-10 R00-R99)
# among all non-fetal resident deaths aged 20-59 years, Brazil, 2015-2024.
#
# Outputs:
#   - annual regional counts and percentages
#   - annual Brazil counts and percentages
#   - validation tables against the final revised analysis
#
# Source:
#   SIM national DBC files DOBR2015.dbc ... DOBR2024.dbc

suppressPackageStartupMessages({
  library(data.table)
  library(read.dbc)
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

sim_dir <- file.path(
  repo,
  "local_data",
  "SIM"
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

years <- 2015:2024

sim_files <- file.path(
  sim_dir,
  paste0(
    "DOBR",
    years,
    ".dbc"
  )
)

missing_files <- sim_files[
  !file.exists(sim_files)
]

if (length(missing_files)) {
  stop(
    "Missing SIM files:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}

decode_age_years <- function(x) {

  x <- as.character(x)

  unit <- suppressWarnings(
    as.integer(
      substr(
        x,
        1,
        1
      )
    )
  )

  value <- suppressWarnings(
    as.integer(
      substr(
        x,
        2,
        4
      )
    )
  )

  ifelse(
    unit == 4,
    value,
    NA_real_
  )
}

classify_region <- function(codmun) {

  uf <- suppressWarnings(
    as.integer(
      substr(
        as.character(codmun),
        1,
        2
      )
    )
  )

  fcase(
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
}

# ------------------------------------------------------------
# Read and aggregate SIM
# ------------------------------------------------------------

regional_list <- vector(
  "list",
  length(sim_files)
)

for (i in seq_along(sim_files)) {

  year_i <- years[i]

  cat(
    "Reading ",
    year_i,
    "... ",
    sep = ""
  )

  x <- as.data.table(
    read.dbc(
      sim_files[i]
    )
  )

  required <- c(
    "TIPOBITO",
    "IDADE",
    "CODMUNRES",
    "CAUSABAS"
  )

  if (!all(required %in% names(x))) {
    stop(
      "Missing required variables in ",
      basename(sim_files[i])
    )
  }

  x <- x[
    ,
    ..required
  ]

  x[
    ,
    idade_anos :=
      decode_age_years(
        IDADE
      )
  ]

  x[
    ,
    causa_basica :=
      toupper(
        trimws(
          as.character(
            CAUSABAS
          )
        )
      )
  ]

  x[
    ,
    cid3 :=
      substr(
        causa_basica,
        1,
        3
      )
  ]

  x[
    ,
    regiao :=
      classify_region(
        CODMUNRES
      )
  ]

  # Non-fetal deaths, residents aged 20-59 years
  x <- x[
    as.character(TIPOBITO) == "2" &
      idade_anos >= 20 &
      idade_anos <= 59 &
      !is.na(regiao)
  ]

  x[
    ,
    R00_R99 :=
      grepl(
        "^R[0-9]{2}",
        cid3
      )
  ]

  regional_list[[i]] <- x[
    ,
    .(
      total_deaths = .N,

      R00_R99_deaths = sum(
        R00_R99,
        na.rm = TRUE
      )
    ),
    by = regiao
  ][
    ,
    ANO_ARQUIVO := year_i
  ]

  rm(x)
  gc()

  cat(
    "OK\n"
  )
}

r00_regional <- rbindlist(
  regional_list,
  use.names = TRUE
)

r00_regional[
  ,
  percent_R00_R99 :=
    100 *
    R00_R99_deaths /
    total_deaths
]

setorder(
  r00_regional,
  ANO_ARQUIVO,
  regiao
)

# ------------------------------------------------------------
# Brazil totals
# ------------------------------------------------------------

r00_brasil <- r00_regional[
  ,
  .(
    total_deaths = sum(
      total_deaths
    ),

    R00_R99_deaths = sum(
      R00_R99_deaths
    )
  ),
  by = ANO_ARQUIVO
]

r00_brasil[
  ,
  percent_R00_R99 :=
    100 *
    R00_R99_deaths /
    total_deaths
]

setorder(
  r00_brasil,
  ANO_ARQUIVO
)

# ------------------------------------------------------------
# Validation: regional results
# ------------------------------------------------------------

expected_regional <- data.table(
  ANO_ARQUIVO = rep(
    2015:2024,
    each = 5
  ),

  regiao = rep(
    c(
      "Central-West",
      "North",
      "Northeast",
      "South",
      "Southeast"
    ),
    times = 10
  ),

  total_expected = c(
    27618,27107,101120,51221,154784,
    27935,27980,104175,53048,157816,
    26892,28268,103768,50769,152304,
    26565,28623,101012,49598,150532,
    26642,28027,98647,49056,150865,
    31983,34073,118164,53658,173829,
    45165,41433,133558,78337,228210,
    30010,30913,109841,54774,158445,
    29010,30528,106942,51451,152894,
    30469,30661,109211,53250,157645
  ),

  R00_expected = c(
    857,1699,5890,1766,9422,
    948,1786,6159,2025,9971,
    896,1545,5555,1877,9876,
    783,1659,5228,1856,10081,
    986,1883,5685,2082,10525,
    1105,2299,7242,2215,13208,
    1011,2069,6968,2394,13725,
    973,1803,6040,2239,11085,
    972,1753,5121,1985,10211,
    992,1769,4984,1853,10230
  ),

  percent_expected_2dp = c(
    3.10,6.27,5.82,3.45,6.09,
    3.39,6.38,5.91,3.82,6.32,
    3.33,5.47,5.35,3.70,6.48,
    2.95,5.80,5.18,3.74,6.70,
    3.70,6.72,5.76,4.24,6.98,
    3.45,6.75,6.13,4.13,7.60,
    2.24,4.99,5.22,3.06,6.01,
    3.24,5.83,5.50,4.09,7.00,
    3.35,5.74,4.79,3.86,6.68,
    3.26,5.77,4.56,3.48,6.49
  )
)

audit_regional <- merge(
  r00_regional,
  expected_regional,
  by = c(
    "ANO_ARQUIVO",
    "regiao"
  )
)

audit_regional[
  ,
  match :=
    total_deaths == total_expected &
    R00_R99_deaths == R00_expected &
    round(
      percent_R00_R99,
      2
    ) == percent_expected_2dp
]

# ------------------------------------------------------------
# Validation: Brazil results
# ------------------------------------------------------------

expected_brazil <- data.table(
  ANO_ARQUIVO = 2015:2024,

  total_expected = c(
    361850,
    370954,
    362001,
    356330,
    353237,
    411707,
    526703,
    383983,
    370825,
    381236
  ),

  R00_expected = c(
    19634,
    20889,
    19749,
    19607,
    21161,
    26069,
    26167,
    22140,
    20042,
    19828
  ),

  percent_expected_2dp = c(
    5.43,
    5.63,
    5.46,
    5.50,
    5.99,
    6.33,
    4.97,
    5.77,
    5.40,
    5.20
  )
)

audit_brazil <- merge(
  r00_brasil,
  expected_brazil,
  by = "ANO_ARQUIVO"
)

audit_brazil[
  ,
  match :=
    total_deaths == total_expected &
    R00_R99_deaths == R00_expected &
    round(
      percent_R00_R99,
      2
    ) == percent_expected_2dp
]

# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------

fwrite(
  r00_regional,
  file.path(
    out_dir,
    "R00_R99_by_region_year.csv"
  )
)

fwrite(
  r00_brasil,
  file.path(
    out_dir,
    "R00_R99_Brazil_year.csv"
  )
)

fwrite(
  audit_regional,
  file.path(
    out_dir,
    "audit_R00_R99_by_region_year.csv"
  )
)

fwrite(
  audit_brazil,
  file.path(
    out_dir,
    "audit_R00_R99_Brazil_year.csv"
  )
)

cat(
  "\n============================================\n"
)

cat(
  "R00-R99 VALIDATION\n"
)

cat(
  "============================================\n"
)

cat(
  "Regional:",
  all(audit_regional$match),
  "(",
  sum(audit_regional$match),
  "/",
  nrow(audit_regional),
  ")\n"
)

cat(
  "Brazil:",
  all(audit_brazil$match),
  "(",
  sum(audit_brazil$match),
  "/",
  nrow(audit_brazil),
  ")\n"
)

cat(
  "\n2024 regional results:\n"
)

print(
  r00_regional[
    ANO_ARQUIVO == 2024
  ]
)

cat(
  "\nBrazil annual results:\n"
)

print(
  r00_brasil
)

if (
  !all(audit_regional$match) ||
  !all(audit_brazil$match)
) {

  cat(
    "\nDivergent regional rows:\n"
  )

  print(
    audit_regional[
      match == FALSE
    ]
  )

  cat(
    "\nDivergent Brazil rows:\n"
  )

  print(
    audit_brazil[
      match == FALSE
    ]
  )

  stop(
    "At least one R00-R99 validation failed."
  )
}

cat(
  "\n06_data_quality_R00_R99.R completed successfully.\n"
)
