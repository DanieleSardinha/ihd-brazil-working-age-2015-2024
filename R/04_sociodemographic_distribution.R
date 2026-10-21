# 04_sociodemographic_distribution.R
# Sociodemographic distribution of working-age IHD deaths,
# Brazil, 2015-2024.
#
# Primary descriptive Table 2 compares 2015 and 2024.
# Percentages use all IHD deaths in the corresponding year as the denominator,
# retaining Unknown categories.
#
# Source:
#   local_data/derived/sim_ihd_20_59_2015_2024.rds
#
# Validated final annual totals:
#   2015 = 26,644 deaths
#   2024 = 26,016 deaths

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
# Harmonize year variable
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

required <- c(
  "idade_anos",
  "sexo_cat",
  "raca_cor",
  "escolaridade"
)

missing_vars <- setdiff(
  required,
  names(d)
)

if (length(missing_vars)) {
  stop(
    "Missing required variables:\n",
    paste(
      missing_vars,
      collapse = ", "
    )
  )
}

# ------------------------------------------------------------
# Age group
# ------------------------------------------------------------

d[
  ,
  age_group :=
    cut(
      idade_anos,
      breaks = c(
        20,
        30,
        40,
        50,
        60
      ),
      right = FALSE,
      labels = c(
        "20-29 years",
        "30-39 years",
        "40-49 years",
        "50-59 years"
      )
    )
]

d[
  ,
  age_group :=
    as.character(
      age_group
    )
]

# ------------------------------------------------------------
# Helper
# ------------------------------------------------------------

distribution_by_year <- function(
  data,
  variable_col,
  variable_label
) {

  x <- data[
    ,
    .(
      n = .N
    ),
    by = c(
      "ANO_ARQUIVO",
      variable_col
    )
  ]

  setnames(
    x,
    variable_col,
    "categoria"
  )

  totals <- data[
    ,
    .(
      denominator = .N
    ),
    by = ANO_ARQUIVO
  ]

  x <- merge(
    x,
    totals,
    by = "ANO_ARQUIVO",
    all.x = TRUE
  )

  x[
    ,
    percentual :=
      100 *
      n /
      denominator
  ]

  x[
    ,
    variavel :=
      variable_label
  ]

  x[
    ,
    .(
      variavel,
      categoria,
      ANO_ARQUIVO,
      n,
      percentual
    )
  ]
}

age_year <- distribution_by_year(
  d,
  "age_group",
  "Age group"
)

sex_year <- distribution_by_year(
  d,
  "sexo_cat",
  "Sex"
)

race_year <- distribution_by_year(
  d,
  "raca_cor",
  "Race/skin color"
)

education_year <- distribution_by_year(
  d,
  "escolaridade",
  "Educational attainment"
)

# ------------------------------------------------------------
# Explicit category order used in the final manuscript
# ------------------------------------------------------------

category_order <- data.table(
  variavel = c(
    rep(
      "Age group",
      4
    ),
    rep(
      "Sex",
      3
    ),
    rep(
      "Race/skin color",
      6
    ),
    rep(
      "Educational attainment",
      4
    )
  ),

  categoria = c(
    "20-29 years",
    "30-39 years",
    "40-49 years",
    "50-59 years",

    "Male",
    "Female",
    "Unknown",

    "White",
    "Mixed-race",
    "Black",
    "Asian",
    "Indigenous",
    "Unknown",

    "0-7 years",
    "8-11 years",
    "12+ years",
    "Unknown"
  ),

  display_order = 1:17
)

# ------------------------------------------------------------
# Long endpoint table: 2015 and 2024
# ------------------------------------------------------------

soc_long <- rbindlist(
  list(
    age_year,
    sex_year,
    race_year,
    education_year
  ),
  use.names = TRUE
)[
  ANO_ARQUIVO %in%
    c(
      2015,
      2024
    )
]

soc_long <- merge(
  category_order,
  soc_long,
  by = c(
    "variavel",
    "categoria"
  ),
  all.x = TRUE
)

setorder(
  soc_long,
  display_order,
  ANO_ARQUIVO
)

soc_long_out <- soc_long[
  ,
  .(
    variavel,
    categoria,
    ANO_ARQUIVO,
    n,
    percentual
  )
]

# ------------------------------------------------------------
# Endpoint change in percentage points
# ------------------------------------------------------------

soc_changes <- dcast(
  soc_long_out,
  variavel +
    categoria ~
    ANO_ARQUIVO,
  value.var = c(
    "n",
    "percentual"
  )
)

soc_changes[
  ,
  diferenca_pp :=
    percentual_2024 -
    percentual_2015
]

soc_changes <- merge(
  category_order,
  soc_changes,
  by = c(
    "variavel",
    "categoria"
  ),
  all.x = TRUE
)

setorder(
  soc_changes,
  display_order
)

format_n_pct <- function(
  n,
  pct
) {

  paste0(
    formatC(
      n,
      format = "d",
      big.mark = ","
    ),
    " (",
    sprintf(
      "%.1f",
      pct
    ),
    "%)"
  )
}

table2 <- soc_changes[
  ,
  .(
    Variable = variavel,
    Category = categoria,

    `2015, n (%)` =
      format_n_pct(
        n_2015,
        percentual_2015
      ),

    `2024, n (%)` =
      format_n_pct(
        n_2024,
        percentual_2024
      )
  )
]

soc_changes_out <- soc_changes[
  ,
  .(
    variavel,
    categoria,

    n_pct_2015 =
      format_n_pct(
        n_2015,
        percentual_2015
      ),

    n_pct_2024 =
      format_n_pct(
        n_2024,
        percentual_2024
      ),

    diferenca_pp
  )
]

# ------------------------------------------------------------
# Annual supplementary tables
# ------------------------------------------------------------

sex_year_out <- sex_year[
  ,
  .(
    ANO_ARQUIVO,
    sexo_cat = categoria,
    n,
    percentual
  )
]

setorder(
  sex_year_out,
  ANO_ARQUIVO,
  sexo_cat
)

race_year_out <- race_year[
  ,
  .(
    ANO_ARQUIVO,
    raca_cor = categoria,
    n,
    percentual
  )
]

setorder(
  race_year_out,
  ANO_ARQUIVO,
  raca_cor
)

education_year_out <- education_year[
  ,
  .(
    ANO_ARQUIVO,
    escolaridade = categoria,
    n,
    percentual
  )
]

setorder(
  education_year_out,
  ANO_ARQUIVO,
  escolaridade
)

# ------------------------------------------------------------
# Validation against final Table 2
# ------------------------------------------------------------

expected <- data.table(
  display_order = 1:17,

  Variable = c(
    rep(
      "Age group",
      4
    ),
    rep(
      "Sex",
      3
    ),
    rep(
      "Race/skin color",
      6
    ),
    rep(
      "Educational attainment",
      4
    )
  ),

  Category = c(
    "20-29 years",
    "30-39 years",
    "40-49 years",
    "50-59 years",

    "Male",
    "Female",
    "Unknown",

    "White",
    "Mixed-race",
    "Black",
    "Asian",
    "Indigenous",
    "Unknown",

    "0-7 years",
    "8-11 years",
    "12+ years",
    "Unknown"
  ),

  n2015 = c(
    641,
    2202,
    7026,
    16775,

    18487,
    8155,
    2,

    12447,
    10715,
    2542,
    100,
    44,
    796,

    14736,
    5544,
    1711,
    4653
  ),

  n2024 = c(
    643,
    2246,
    7239,
    15888,

    18569,
    7444,
    3,

    10577,
    12216,
    2829,
    89,
    61,
    244,

    12037,
    8876,
    2224,
    2879
  ),

  pct2015 = c(
    2.40579493,
    8.26452485,
    26.36991443,
    62.95976580,

    69.38522744,
    30.60726618,
    0.00750638,

    46.71595856,
    40.21543312,
    9.54060952,
    0.37531902,
    0.16514037,
    2.98753941,

    55.30701096,
    20.80768653,
    6.42170845,
    17.46359405
  ),

  pct2024 = c(
    2.47155597,
    8.63314883,
    27.82518450,
    61.07011070,

    71.37530750,
    28.61316113,
    0.01153137,

    40.65575031,
    46.95571956,
    10.87407749,
    0.34209717,
    0.23447109,
    0.93788438,

    46.26768143,
    34.11746617,
    8.54858549,
    11.06626691
  )
)

audit <- merge(
  expected,
  soc_changes[
    ,
    .(
      display_order,
      Variable = variavel,
      Category = categoria,
      n2015_obs = n_2015,
      n2024_obs = n_2024,
      pct2015_obs = percentual_2015,
      pct2024_obs = percentual_2024,
      diferenca_pp
    )
  ],
  by = c(
    "display_order",
    "Variable",
    "Category"
  ),
  all = TRUE
)

audit[
  ,
  match :=
    n2015 == n2015_obs &
    n2024 == n2024_obs &
    abs(
      pct2015 -
      pct2015_obs
    ) < 1e-6 &
    abs(
      pct2024 -
      pct2024_obs
    ) < 1e-6
]

annual_totals <- d[
  ,
  .N,
  by = ANO_ARQUIVO
]

total_2015 <- annual_totals[
  ANO_ARQUIVO == 2015,
  N
]

total_2024 <- annual_totals[
  ANO_ARQUIVO == 2024,
  N
]

totals_match <-
  total_2015 == 26644 &&
  total_2024 == 26016

# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------

fwrite(
  table2,
  file.path(
    out_dir,
    "Table_2_sociodemographic_distribution.csv"
  )
)

fwrite(
  soc_changes_out,
  file.path(
    out_dir,
    "Supplementary_sociodemographic_changes_pp.csv"
  )
)

fwrite(
  soc_long_out,
  file.path(
    out_dir,
    "Supplementary_sociodemographic_distribution_long.csv"
  )
)

fwrite(
  sex_year_out,
  file.path(
    out_dir,
    "supplementary_sex_by_year.csv"
  )
)

fwrite(
  race_year_out,
  file.path(
    out_dir,
    "supplementary_race_skin_color_by_year.csv"
  )
)

fwrite(
  education_year_out,
  file.path(
    out_dir,
    "supplementary_educational_attainment_by_year.csv"
  )
)

fwrite(
  audit,
  file.path(
    out_dir,
    "audit_Table_2_sociodemographic_distribution.csv"
  )
)

cat(
  "\n============================================\n"
)

cat(
  "SOCIODEMOGRAPHIC TABLE VALIDATION\n"
)

cat(
  "============================================\n"
)

cat(
  "2015 total:",
  total_2015,
  "\n"
)

cat(
  "2024 total:",
  total_2024,
  "\n"
)

cat(
  "Annual totals valid:",
  totals_match,
  "\n"
)

cat(
  "Table 2 rows valid:",
  all(audit$match),
  "(",
  sum(audit$match),
  "/",
  nrow(audit),
  ")\n"
)

cat(
  "\nFinal Table 2:\n"
)

print(
  table2
)

if (
  !totals_match ||
  !all(audit$match)
) {

  cat(
    "\nDivergent Table 2 rows:\n"
  )

  print(
    audit[
      match == FALSE |
      is.na(match)
    ]
  )

  stop(
    "Sociodemographic Table 2 validation failed."
  )
}

cat(
  "\n04_sociodemographic_distribution.R completed successfully.\n"
)
