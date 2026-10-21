# 03_temporal_trends.R
# Weighted log-linear and segmented time-trend models.
#
# Validated against the final minor-revision analysis.
# Rate variances are used to construct RELATIVE regression weights.
# Coefficient uncertainty is estimated from the weighted regression model;
# rate variances are not treated as fixed known variances.
#
# AAPC for the selected segmented 20-59 model is obtained with
# segmented::aapc(), which reproduces the final manuscript CI.

suppressPackageStartupMessages({
  library(data.table)
  library(segmented)
  library(nlme)
})

find_repo_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(wd, "run_all.R"))) return(wd)
  parent <- normalizePath(file.path(wd, ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(parent, "run_all.R"))) return(parent)
  stop("Repository root not found.")
}

repo <- find_repo_root()
out_dir <- file.path(repo, "outputs")

f20 <- file.path(out_dir, "ASMR_Brazil_20_59_corrected_variance.csv")
f30 <- file.path(out_dir, "ASMR_Brazil_30_69_sensitivity_corrected_variance.csv")
if (!file.exists(f20) || !file.exists(f30)) {
  stop("Run 02_mortality_rates_standardization.R first.")
}

to_trend_input <- function(x) {
  data.table(
    Year = x$ANO_ARQUIVO,
    Rate = x$ASMR,
    Standard_Error = x$SE,
    Variance = x$variance,
    CI_low = x$CI_lower,
    CI_high = x$CI_upper
  )
}

prep <- function(input) {
  d <- as.data.frame(input)
  d$log_rate <- log(d$Rate)
  d$var_log <- d$Variance / d$Rate^2
  d$inv_var_log <- 1 / d$var_log
  d$weight_rel <- d$inv_var_log / mean(d$inv_var_log)
  d$variance_multiplier <- 1 / d$weight_rel
  d
}

fit_trend <- function(input, label) {
  d <- prep(input)

  m0 <- lm(log_rate ~ Year, data = d, weights = weight_rel)

  m1 <- segmented(
    m0,
    seg.Z = ~ Year,
    psi = list(Year = 2020),
    control = seg.control(it.max = 200, n.boot = 0)
  )

  bic0 <- as.numeric(BIC(m0))
  bic1 <- as.numeric(BIC(m1))
  delta_bic <- bic0 - bic1

  # Pre-specified parsimony rule used in the final manuscript:
  # BIC improvements <2 points are considered negligible.
  selected_breakpoints <- ifelse(delta_bic >= 2, 1L, 0L)

  davies <- davies.test(m0, seg.Z = ~ Year, k = 10)

  # Overall slope for the no-breakpoint model.
  b0 <- coef(m0)[["Year"]]
  se0 <- coef(summary(m0))["Year", "Std. Error"]
  crit0 <- qt(0.975, df = df.residual(m0))
  ci0 <- b0 + c(-1, 1) * crit0 * se0
  overall_APC <- 100 * (exp(b0) - 1)
  overall_low <- 100 * (exp(ci0[1]) - 1)
  overall_high <- 100 * (exp(ci0[2]) - 1)
  overall_p <- coef(summary(m0))["Year", "Pr(>|t|)"]

  # Segmented slopes and APCs.
  sl <- segmented::slope(m1)$Year
  slope1 <- unname(sl["slope1", "Est."])
  slope1_low <- unname(sl["slope1", "CI(95%).l"])
  slope1_high <- unname(sl["slope1", "CI(95%).u"])
  slope2 <- unname(sl["slope2", "Est."])
  slope2_low <- unname(sl["slope2", "CI(95%).l"])
  slope2_high <- unname(sl["slope2", "CI(95%).u"])

  APC1 <- 100 * (exp(slope1) - 1)
  APC1_low <- 100 * (exp(slope1_low) - 1)
  APC1_high <- 100 * (exp(slope1_high) - 1)

  APC2 <- 100 * (exp(slope2) - 1)
  APC2_low <- 100 * (exp(slope2_low) - 1)
  APC2_high <- 100 * (exp(slope2_high) - 1)

  bp <- as.numeric(m1$psi[1, "Est."])

  # AAPC with the package method used for the final analysis.
  aapc_obj <- segmented::aapc(
    m1,
    parm = "Year",
    exp.it = FALSE,
    conf.level = 0.95
  )

  AAPC_beta <- unname(aapc_obj["Est."])
  AAPC_beta_SE <- unname(aapc_obj["St.Err"])
  AAPC_beta_low <- unname(aapc_obj["CI(95%).l"])
  AAPC_beta_high <- unname(aapc_obj["CI(95%).u"])

  AAPC <- 100 * (exp(AAPC_beta) - 1)
  AAPC_low <- 100 * (exp(AAPC_beta_low) - 1)
  AAPC_high <- 100 * (exp(AAPC_beta_high) - 1)
  AAPC_wrong_SE <- attr(aapc_obj, "wrong.se")

  # AR(1) sensitivity.
  gls_ind <- gls(
    log_rate ~ Year,
    data = d,
    weights = varFixed(~ variance_multiplier),
    method = "ML"
  )

  gls_ar1 <- gls(
    log_rate ~ Year,
    data = d,
    weights = varFixed(~ variance_multiplier),
    correlation = corAR1(form = ~ Year),
    method = "ML"
  )

  ar_comp <- anova(gls_ind, gls_ar1)
  phi <- as.numeric(coef(gls_ar1$modelStruct$corStruct, unconstrained = FALSE))

  list(
    label = label,
    data = d,
    m0 = m0,
    m1 = m1,
    BIC0 = bic0,
    BIC1 = bic1,
    delta_BIC = delta_bic,
    selected_breakpoints = selected_breakpoints,
    breakpoint = bp,
    davies_p = davies$p.value,
    overall_APC = overall_APC,
    overall_low = overall_low,
    overall_high = overall_high,
    overall_p = overall_p,
    APC1 = APC1,
    APC1_low = APC1_low,
    APC1_high = APC1_high,
    APC2 = APC2,
    APC2_low = APC2_low,
    APC2_high = APC2_high,
    AAPC = AAPC,
    AAPC_low = AAPC_low,
    AAPC_high = AAPC_high,
    AAPC_beta = AAPC_beta,
    AAPC_beta_SE = AAPC_beta_SE,
    AAPC_wrong_SE = AAPC_wrong_SE,
    AR1_phi = phi,
    AIC_ind = AIC(gls_ind),
    AIC_AR1 = AIC(gls_ar1),
    BIC_ind = BIC(gls_ind),
    BIC_AR1 = BIC(gls_ar1),
    LR_p = ar_comp$`p-value`[2],
    davies = davies,
    gls_ind = gls_ind,
    gls_ar1 = gls_ar1
  )
}

asmr20 <- fread(f20)
asmr30 <- fread(f30)

trend20_input <- to_trend_input(asmr20)
trend30_input <- to_trend_input(asmr30)

res20 <- fit_trend(trend20_input, "20-59")
res30 <- fit_trend(trend30_input, "30-69")

# Final model summaries used in the manuscript.
trend_summary <- rbind(
  data.table(
    age_range = "20-59",
    BIC_0_breakpoints = res20$BIC0,
    BIC_1_breakpoint = res20$BIC1,
    delta_BIC = res20$delta_BIC,
    selected_breakpoints = res20$selected_breakpoints,
    breakpoint = res20$breakpoint,
    Davies_p = res20$davies_p,
    APC_1 = res20$APC1,
    APC_1_low = res20$APC1_low,
    APC_1_high = res20$APC1_high,
    APC_2 = res20$APC2,
    APC_2_low = res20$APC2_low,
    APC_2_high = res20$APC2_high,
    AAPC = res20$AAPC,
    AAPC_low = res20$AAPC_low,
    AAPC_high = res20$AAPC_high,
    overall_APC = NA_real_,
    overall_low = NA_real_,
    overall_high = NA_real_,
    overall_p = NA_real_,
    AR1_phi = res20$AR1_phi,
    LR_p_AR1 = res20$LR_p
  ),
  data.table(
    age_range = "30-69",
    BIC_0_breakpoints = res30$BIC0,
    BIC_1_breakpoint = res30$BIC1,
    delta_BIC = res30$delta_BIC,
    selected_breakpoints = res30$selected_breakpoints,
    breakpoint = NA_real_, # candidate breakpoint not retained
    Davies_p = res30$davies_p,
    APC_1 = NA_real_,
    APC_1_low = NA_real_,
    APC_1_high = NA_real_,
    APC_2 = NA_real_,
    APC_2_low = NA_real_,
    APC_2_high = NA_real_,
    AAPC = NA_real_,
    AAPC_low = NA_real_,
    AAPC_high = NA_real_,
    overall_APC = res30$overall_APC,
    overall_low = res30$overall_low,
    overall_high = res30$overall_high,
    overall_p = res30$overall_p,
    AR1_phi = res30$AR1_phi,
    LR_p_AR1 = res30$LR_p
  ),
  fill = TRUE
)

# Audit against final revised results.
audit <- data.table(
  metric = c(
    "20-59 BIC0","20-59 BIC1","20-59 delta BIC","20-59 Davies p",
    "20-59 APC1","20-59 APC1 low","20-59 APC1 high",
    "20-59 APC2","20-59 APC2 low","20-59 APC2 high",
    "20-59 AAPC","20-59 AAPC low","20-59 AAPC high",
    "20-59 AR1 phi","20-59 AR1 LR p",
    "30-69 BIC0","30-69 BIC1","30-69 delta BIC","30-69 Davies p",
    "30-69 overall APC","30-69 overall low","30-69 overall high","30-69 overall p",
    "30-69 AR1 phi","30-69 AR1 LR p"
  ),
  observed = c(
    res20$BIC0,res20$BIC1,res20$delta_BIC,res20$davies_p,
    res20$APC1,res20$APC1_low,res20$APC1_high,
    res20$APC2,res20$APC2_low,res20$APC2_high,
    res20$AAPC,res20$AAPC_low,res20$AAPC_high,
    res20$AR1_phi,res20$LR_p,
    res30$BIC0,res30$BIC1,res30$delta_BIC,res30$davies_p,
    res30$overall_APC,res30$overall_low,res30$overall_high,res30$overall_p,
    res30$AR1_phi,res30$LR_p
  ),
  expected = c(
    -34.51659,-39.72000,5.203406,0.09549779,
    -3.298649,-5.072065,-1.492103,
    0.03249394,-1.846247,1.947195,
    -1.83194,-2.391632,-1.269038,
    0.3224764,0.2919502,
    -36.32165,-37.31515,0.9935007,0.3039517,
    -2.236444,-3.003914,-1.462901,0.000166244,
    0.1475028,0.6325172
  )
)
audit[, abs_diff := abs(observed - expected)]
audit[, match := abs_diff < 1e-4]

fwrite(trend20_input, file.path(out_dir, "Trend_input_Brazil_ASMR_SE_corrected.csv"))
fwrite(trend30_input, file.path(out_dir, "Trend_input_Brazil_30_69_ASMR_SE_corrected.csv"))
fwrite(trend_summary, file.path(out_dir, "temporal_trend_summary.csv"))
fwrite(audit, file.path(out_dir, "audit_temporal_trends.csv"))

saveRDS(
  list(data = res20$data, m0 = res20$m0, m1 = res20$m1,
       selected_breakpoints = res20$selected_breakpoints,
       results = trend_summary[age_range == "20-59"],
       davies = res20$davies),
  file.path(out_dir, "models_trend_20_59.rds")
)

saveRDS(
  list(data = res30$data, m0 = res30$m0, m1 = res30$m1,
       selected_breakpoints = res30$selected_breakpoints,
       results = trend_summary[age_range == "30-69"],
       davies = res30$davies),
  file.path(out_dir, "models_trend_30_69.rds")
)

cat("\nTEMPORAL TREND VALIDATION\n")
print(trend_summary)
cat("\nAll audited trend quantities within tolerance:", all(audit$match), "\n")
if (!all(audit$match)) print(audit[match == FALSE])

cat("03_temporal_trends.R completed successfully.\n")
