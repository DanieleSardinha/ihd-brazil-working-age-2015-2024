# ==============================================================================
# 01_data_preparation.R
# Study: Working-age mortality from ischemic heart disease in Brazil, 2015-2024
#
# Purpose:
#   Reconstruct the individual-level analytical mortality datasets used in the
#   revised manuscript directly from the public SIM/DATASUS national DBC files.
#
# Public-repository policy:
#   - Raw DBC files are NOT redistributed.
#   - Individual-level derived RDS files are written under local_data/derived/
#     and must remain excluded from GitHub.
#
# Required local inputs:
#   local_data/SIM/DOBR2015.dbc ... DOBR2024.dbc
#
# Main local outputs:
#   local_data/derived/sim_ihd_20_59_2015_2024.rds
#   local_data/derived/sim_ihd_30_69_2015_2024.rds
#
# Validated final counts:
#   IHD I20-I25, age 20-59: 260,493 deaths
#   AMI I21, age 20-59:      224,425 deaths
#   IHD I20-I25, age 30-69: 526,629 deaths
#   20-59 output also matches the validated GOLD dataset storage classes.
# ==============================================================================

required_packages <- c(
  "data.table",
  "read.dbc"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages)) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them before running this script."
  )
}

suppressPackageStartupMessages({
  library(data.table)
})

# ------------------------------------------------------------------------------
# 1. Repository paths
# ------------------------------------------------------------------------------

find_repo_root <- function() {

  wd <- normalizePath(
    getwd(),
    winslash = "/",
    mustWork = FALSE
  )

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
    "Repository root not found. Run this script from the repository root ",
    "or from the R/ directory."
  )
}

repo <- find_repo_root()

years <- 2015:2024

sim_raw_dir <- Sys.getenv(
  "IHD_SIM_RAW_DIR",
  unset = file.path(
    repo,
    "local_data",
    "SIM"
  )
)

derived_dir <- file.path(
  repo,
  "local_data",
  "derived"
)

dir.create(
  derived_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

expected_files <- file.path(
  sim_raw_dir,
  paste0(
    "DOBR",
    years,
    ".dbc"
  )
)

missing_files <- expected_files[
  !file.exists(expected_files)
]

if (length(missing_files)) {
  stop(
    "SIM DBC file(s) not found:\n",
    paste(missing_files, collapse = "\n")
  )
}

# ------------------------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------------------------

decode_age_components <- function(x) {

  x <- trimws(
    as.character(x)
  )

  x[
    is.na(x) |
      x == ""
  ] <- NA_character_

  unidade <- suppressWarnings(
    as.integer(
      substr(
        x,
        1,
        1
      )
    )
  )

  valor <- suppressWarnings(
    as.integer(
      substr(
        x,
        2,
        4
      )
    )
  )

  idade_anos <- ifelse(
    unidade == 4,
    valor,
    NA_real_
  )

  list(
    unidade_idade = ifelse(
      is.na(unidade),
      NA_character_,
      as.character(unidade)
    ),
    valor_idade = as.numeric(valor),
    idade_anos = as.numeric(idade_anos)
  )
}

normalize_cid <- function(x) {

  out <- toupper(
    trimws(
      as.character(x)
    )
  )

  out <- gsub(
    "\\.",
    "",
    out
  )

  out[
    is.na(out) |
      out == ""
  ] <- NA_character_

  out
}

recode_sex <- function(x) {

  x <- trimws(
    as.character(x)
  )

  fcase(
    x == "1",
    "Male",

    x == "2",
    "Female",

    default =
      "Unknown"
  )
}

recode_race <- function(x) {

  x <- trimws(
    as.character(x)
  )

  fcase(
    x == "1",
    "White",

    x == "2",
    "Black",

    x == "3",
    "Asian",

    x == "4",
    "Mixed-race",

    x == "5",
    "Indigenous",

    default =
      "Unknown"
  )
}

# Manuscript grouping based on the original SIM ESC field:
#   1 = none
#   2 = 1-3 years
#   3 = 4-7 years
#   4 = 8-11 years
#   5 = 12+ years
# Other/missing values are retained as Unknown.
recode_education <- function(x) {

  x <- trimws(
    as.character(x)
  )

  fcase(
    x %in% c(
      "1",
      "2",
      "3"
    ),
    "0-7 years",

    x == "4",
    "8-11 years",

    x == "5",
    "12+ years",

    default =
      "Unknown"
  )
}

derive_region <- function(codmun) {

  codmun_limpo <- trimws(
    as.character(codmun)
  )

  cod_uf <- suppressWarnings(
    as.integer(
      substr(
        codmun_limpo,
        1,
        2
      )
    )
  )

  fcase(
    cod_uf %in%
      c(
        11,12,13,14,15,16,17
      ),
    "North",

    cod_uf %in%
      c(
        21,22,23,24,25,26,27,28,29
      ),
    "Northeast",

    cod_uf %in%
      c(
        31,32,33,35
      ),
    "Southeast",

    cod_uf %in%
      c(
        41,42,43
      ),
    "South",

    cod_uf %in%
      c(
        50,51,52,53
      ),
    "Central-West",

    default =
      NA_character_
  )
}

make_age_group10 <- function(age) {

  cut(
    age,
    breaks = c(
      20,
      30,
      40,
      50,
      60,
      70
    ),
    right = FALSE,
    labels = c(
      "20-29 years",
      "30-39 years",
      "40-49 years",
      "50-59 years",
      "60-69 years"
    )
  )
}

make_age_group5 <- function(age) {

  cut(
    age,
    breaks = seq(
      20,
      70,
      by = 5
    ),
    right = FALSE,
    labels = c(
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69"
    )
  )
}

# ------------------------------------------------------------------------------
# 3. Variables retained from each national SIM file
# ------------------------------------------------------------------------------

raw_fields <- c(
  "TIPOBITO",
  "DTOBITO",
  "IDADE",
  "SEXO",
  "RACACOR",
  "ESC",
  "ESC2010",
  "ESCFALAGR1",
  "CODMUNRES",
  "LOCOCOR",
  "CODMUNOCOR",
  "CAUSABAS",
  "CAUSABAS_O"
)

essential_fields <- c(
  "TIPOBITO",
  "IDADE",
  "SEXO",
  "RACACOR",
  "ESC",
  "CODMUNRES",
  "CAUSABAS"
)

# ------------------------------------------------------------------------------
# 4. Read one year, derive variables, and retain only IHD records needed
# ------------------------------------------------------------------------------

prepare_year <- function(
  file,
  year
) {

  message(
    "Reading SIM ",
    year,
    ": ",
    basename(file)
  )

  x <- as.data.table(
    read.dbc::read.dbc(
      file
    )
  )

  absent_essential <- setdiff(
    essential_fields,
    names(x)
  )

  if (length(absent_essential)) {

    stop(
      "Required field(s) missing from ",
      basename(file),
      ": ",
      paste(
        absent_essential,
        collapse = ", "
      )
    )
  }

  # Add optional fields as NA when a particular annual file does not contain them.
  optional_missing <- setdiff(
    raw_fields,
    names(x)
  )

  if (length(optional_missing)) {

    for (v in optional_missing) {
      x[
        ,
        (v) := NA_character_
      ]
    }
  }

  x <- x[
    ,
    ..raw_fields
  ]

  # Preserve the same storage classes as the validated analytical dataset.
  # read.dbc may return these source fields as factors; the original analysis
  # stored the raw SIM variables as character.
  x[
    ,
    (raw_fields) := lapply(
      .SD,
      as.character
    ),
    .SDcols = raw_fields
  ]

  age_parts <- decode_age_components(
    x$IDADE
  )

  x[
    ,
    unidade_idade :=
      age_parts$unidade_idade
  ]

  x[
    ,
    valor_idade :=
      age_parts$valor_idade
  ]

  x[
    ,
    idade_anos :=
      age_parts$idade_anos
  ]

  causa_norm <- normalize_cid(
    x$CAUSABAS
  )

  x[
    ,
    cid3 :=
      substr(
        causa_norm,
        1,
        3
      )
  ]

  x[
    ,
    dic_i20_i25 :=
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

  x[
    ,
    ANO_ARQUIVO :=
      as.integer(year)
  ]

  x[
    ,
    sexo_cat :=
      recode_sex(
        SEXO
      )
  ]

  x[
    ,
    raca_cor :=
      recode_race(
        RACACOR
      )
  ]

  x[
    ,
    escolaridade :=
      recode_education(
        ESC
      )
  ]

  x[
    ,
    codmun_limpo :=
      trimws(
        as.character(
          CODMUNRES
        )
      )
  ]

  x[
    ,
    cod_uf :=
      fifelse(
        is.na(codmun_limpo) |
          nchar(codmun_limpo) < 2,
        NA_character_,
        substr(
          codmun_limpo,
          1,
          2
        )
      )
  ]

  x[
    ,
    regiao :=
      derive_region(
        CODMUNRES
      )
  ]

  x[
    ,
    faixa_etaria :=
      as.character(
        make_age_group10(
          idade_anos
        )
      )
  ]

  x[
    ,
    idade5 :=
      make_age_group5(
        idade_anos
      )
  ]

  non_fetal <-
    trimws(
      as.character(
        x$TIPOBITO
      )
    ) == "2"

  ihd <-
    x$dic_i20_i25 %in%
    TRUE

  base20 <- x[
    non_fetal &
      ihd &
      !is.na(idade_anos) &
      idade_anos >= 20 &
      idade_anos <= 59
  ]

  base30 <- x[
    non_fetal &
      ihd &
      !is.na(idade_anos) &
      idade_anos >= 30 &
      idade_anos <= 69
  ]

  # Exact variable structure used by the validated 20-59 analytical dataset.
  final_fields <- c(
    "TIPOBITO",
    "DTOBITO",
    "IDADE",
    "SEXO",
    "RACACOR",
    "ESC",
    "ESC2010",
    "ESCFALAGR1",
    "CODMUNRES",
    "LOCOCOR",
    "CODMUNOCOR",
    "CAUSABAS",
    "CAUSABAS_O",
    "ANO_ARQUIVO",
    "unidade_idade",
    "valor_idade",
    "idade_anos",
    "cid3",
    "dic_i20_i25",
    "faixa_etaria",
    "sexo_cat",
    "raca_cor",
    "escolaridade",
    "codmun_limpo",
    "cod_uf",
    "regiao",
    "idade5"
  )

  list(
    base20 = base20[
      ,
      ..final_fields
    ],

    base30 = base30[
      ,
      ..final_fields
    ]
  )
}

# ------------------------------------------------------------------------------
# 5. Process 2015-2024
# ------------------------------------------------------------------------------

list20 <- vector(
  "list",
  length(years)
)

list30 <- vector(
  "list",
  length(years)
)

for (i in seq_along(years)) {

  z <- prepare_year(
    expected_files[i],
    years[i]
  )

  list20[[i]] <- z$base20
  list30[[i]] <- z$base30

  message(
    "  20-59: ",
    format(
      nrow(z$base20),
      big.mark = ","
    ),
    " deaths | 30-69: ",
    format(
      nrow(z$base30),
      big.mark = ","
    ),
    " deaths"
  )

  rm(z)
  invisible(gc())
}

sim_ihd_20_59 <- rbindlist(
  list20,
  use.names = TRUE,
  fill = TRUE
)

sim_ihd_30_69 <- rbindlist(
  list30,
  use.names = TRUE,
  fill = TRUE
)

rm(
  list20,
  list30
)

invisible(
  gc()
)

# ------------------------------------------------------------------------------
# 6. Validation against the final revised analysis
# ------------------------------------------------------------------------------

expected_year20 <- data.table(
  ANO_ARQUIVO = 2015:2024,
  expected = c(
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
  )
)

audit_year20 <- sim_ihd_20_59[
  ,
  .(
    observed = .N
  ),
  by = ANO_ARQUIVO
]

audit_year20 <- merge(
  expected_year20,
  audit_year20,
  by = "ANO_ARQUIVO",
  all = TRUE
)

audit_year20[
  ,
  match :=
    observed == expected
]

expected_year30 <- data.table(
  ANO_ARQUIVO = 2015:2024,
  expected = c(
    51566,
    53626,
    52674,
    52663,
    53441,
    49312,
    51651,
    54239,
    53630,
    53827
  )
)

audit_year30 <- sim_ihd_30_69[
  ,
  .(
    observed = .N
  ),
  by = ANO_ARQUIVO
]

audit_year30 <- merge(
  expected_year30,
  audit_year30,
  by = "ANO_ARQUIVO",
  all = TRUE
)

audit_year30[
  ,
  match :=
    observed == expected
]

expected_sex <- data.table(
  sexo_cat = c(
    "Unknown",
    "Male",
    "Female"
  ),
  expected = c(
    11,
    182567,
    77915
  )
)

audit_sex <- sim_ihd_20_59[
  ,
  .(
    observed = .N
  ),
  by = sexo_cat
]

audit_sex <- merge(
  expected_sex,
  audit_sex,
  by = "sexo_cat",
  all = TRUE
)

audit_sex[
  ,
  match :=
    observed == expected
]

expected_race <- data.table(
  raca_cor = c(
    "White",
    "Black",
    "Asian",
    "Mixed-race",
    "Indigenous",
    "Unknown"
  ),
  expected = c(
    112741,
    26117,
    911,
    114974,
    614,
    5136
  )
)

audit_race <- sim_ihd_20_59[
  ,
  .(
    observed = .N
  ),
  by = raca_cor
]

audit_race <- merge(
  expected_race,
  audit_race,
  by = "raca_cor",
  all = TRUE
)

audit_race[
  ,
  match :=
    observed == expected
]

expected_education <- data.table(
  escolaridade = c(
    "0-7 years",
    "8-11 years",
    "12+ years",
    "Unknown"
  ),
  expected = c(
    134540,
    70803,
    18825,
    36325
  )
)

audit_education <- sim_ihd_20_59[
  ,
  .(
    observed = .N
  ),
  by = escolaridade
]

audit_education <- merge(
  expected_education,
  audit_education,
  by = "escolaridade",
  all = TRUE
)

audit_education[
  ,
  match :=
    observed == expected
]

expected_age <- data.table(
  faixa_etaria = c(
    "20-29 years",
    "30-39 years",
    "40-49 years",
    "50-59 years"
  ),
  expected = c(
    6421,
    21572,
    69114,
    163386
  )
)

audit_age <- sim_ihd_20_59[
  ,
  .(
    observed = .N
  ),
  by = faixa_etaria
]

audit_age <- merge(
  expected_age,
  audit_age,
  by = "faixa_etaria",
  all = TRUE
)

audit_age[
  ,
  match :=
    observed == expected
]

expected_icd <- data.table(
  cid3 = c(
    "I20",
    "I21",
    "I22",
    "I23",
    "I24",
    "I25"
  ),
  expected = c(
    1148,
    224425,
    440,
    0,
    4680,
    29800
  )
)

audit_icd <- sim_ihd_20_59[
  ,
  .(
    observed = .N
  ),
  by = cid3
]

# Add zero-count I23 explicitly for validation.
audit_icd <- merge(
  expected_icd,
  audit_icd,
  by = "cid3",
  all.x = TRUE
)

audit_icd[
  is.na(observed),
  observed := 0L
]

audit_icd[
  ,
  match :=
    observed == expected
]

overall_match <-
  nrow(sim_ihd_20_59) == 260493 &&
  nrow(sim_ihd_30_69) == 526629 &&
  sim_ihd_20_59[
    cid3 == "I21",
    .N
  ] == 224425 &&
  all(audit_year20$match) &&
  all(audit_year30$match) &&
  all(audit_sex$match) &&
  all(audit_race$match) &&
  all(audit_education$match) &&
  all(audit_age$match) &&
  all(audit_icd$match)

# ------------------------------------------------------------------------------
# 7. Save LOCAL individual-level derived objects
# ------------------------------------------------------------------------------

saveRDS(
  sim_ihd_20_59,
  file.path(
    derived_dir,
    "sim_ihd_20_59_2015_2024.rds"
  ),
  compress = "xz"
)

saveRDS(
  sim_ihd_30_69,
  file.path(
    derived_dir,
    "sim_ihd_30_69_2015_2024.rds"
  ),
  compress = "xz"
)

# Validation files remain local as well.
fwrite(
  audit_year20,
  file.path(
    derived_dir,
    "audit_20_59_by_year.csv"
  )
)

fwrite(
  audit_year30,
  file.path(
    derived_dir,
    "audit_30_69_by_year.csv"
  )
)

fwrite(
  audit_sex,
  file.path(
    derived_dir,
    "audit_20_59_sex.csv"
  )
)

fwrite(
  audit_race,
  file.path(
    derived_dir,
    "audit_20_59_race_skin_color.csv"
  )
)

fwrite(
  audit_education,
  file.path(
    derived_dir,
    "audit_20_59_education.csv"
  )
)

fwrite(
  audit_age,
  file.path(
    derived_dir,
    "audit_20_59_age_group.csv"
  )
)

fwrite(
  audit_icd,
  file.path(
    derived_dir,
    "audit_20_59_icd.csv"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    derived_dir,
    "sessionInfo_01_data_preparation.txt"
  )
)

# ------------------------------------------------------------------------------
# 8. Final report
# ------------------------------------------------------------------------------

cat(
  "\n============================================================\n"
)

cat(
  "01_data_preparation.R VALIDATION\n"
)

cat(
  "============================================================\n"
)

cat(
  "IHD deaths age 20-59:",
  nrow(sim_ihd_20_59),
  "\n"
)

cat(
  "AMI I21 deaths age 20-59:",
  sim_ihd_20_59[
    cid3 == "I21",
    .N
  ],
  "\n"
)

cat(
  "IHD deaths age 30-69:",
  nrow(sim_ihd_30_69),
  "\n"
)

cat(
  "20-59 annual counts:",
  all(audit_year20$match),
  "\n"
)

cat(
  "30-69 annual counts:",
  all(audit_year30$match),
  "\n"
)

cat(
  "Sex recoding:",
  all(audit_sex$match),
  "\n"
)

cat(
  "Race/skin-color recoding:",
  all(audit_race$match),
  "\n"
)

cat(
  "Educational-attainment recoding:",
  all(audit_education$match),
  "\n"
)

cat(
  "Age-group recoding:",
  all(audit_age$match),
  "\n"
)

cat(
  "ICD distribution:",
  all(audit_icd$match),
  "\n"
)

cat(
  "OVERALL VALIDATION:",
  overall_match,
  "\n"
)

if (!overall_match) {

  cat(
    "\n20-59 annual audit:\n"
  )
  print(audit_year20)

  cat(
    "\n30-69 annual audit:\n"
  )
  print(audit_year30)

  cat(
    "\nSex audit:\n"
  )
  print(audit_sex)

  cat(
    "\nRace/skin-color audit:\n"
  )
  print(audit_race)

  cat(
    "\nEducation audit:\n"
  )
  print(audit_education)

  cat(
    "\nAge-group audit:\n"
  )
  print(audit_age)

  cat(
    "\nICD audit:\n"
  )
  print(audit_icd)

  stop(
    "01_data_preparation.R validation failed."
  )
}

cat(
  "\nDerived files written to:\n",
  normalizePath(
    derived_dir,
    winslash = "/"
  ),
  "\n",
  sep = ""
)

cat(
  "\n01_data_preparation.R completed successfully.\n"
)
