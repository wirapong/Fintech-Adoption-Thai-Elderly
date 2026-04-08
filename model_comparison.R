# ============================================================
#  Model Fit & AIC/BIC Comparison — UTAUT2 FinTech Study
#  Five competing CFA models + Four competing SEM models
#  For: Scopus Q1 reviewer response / Table Model Fit
# ============================================================
options(scipen = 999, digits = 4)

library(haven); library(dplyr); library(lavaan)

out_dir   <- "D:/ARIS/Auto-claude-code-research-in-sleep/output"
DATA_PATH <- "E:/ทุนFundamental Research Fund-2569/Data/dataset.sav"

df_raw   <- read_sav(DATA_PATH)
item_vars <- c(
  "PE1","PE2","PE3", "EE1","EE2","EE3", "SI1","SI2","SI3",
  "FC1","FC2","FC3","FC4", "HM1","HM2","HM3", "HB1","HB2","HB3",
  "PT1","PT2","PT3", "TA1","TA2","TA3","TA4",
  "BI1","BI2","BI3","BI4",
  "UB1","UB2","UB3","UB4","UB5","UB6","UB7",
  "DL1","DL2","DL3","DL4","DL5"
)
df <- df_raw %>% select(all_of(item_vars)) %>%
  mutate(across(everything(), as.numeric))

N <- nrow(df)

# ── Helper: extract fit row ──────────────────────────────────
get_fit <- function(fit_obj, label, model_desc) {
  fm  <- fitMeasures(fit_obj, c(
    "chisq","df","pvalue","cfi","tli",
    "rmsea","rmsea.ci.lower","rmsea.ci.upper",
    "srmr","aic","bic","bic2"
  ))
  # Scaled chi-sq for MLR
  fm2 <- tryCatch(fitMeasures(fit_obj, c("chisq.scaled","df.scaled","pvalue.scaled")),
                  error = function(e) c(chisq.scaled=NA, df.scaled=NA, pvalue.scaled=NA))
  data.frame(
    Model       = label,
    Description = model_desc,
    chi2_scaled = round(fm2["chisq.scaled"], 2),
    df          = as.integer(fm2["df.scaled"]),
    p_chi2      = round(fm2["pvalue.scaled"], 3),
    CFI         = round(fm["cfi"],   3),
    TLI         = round(fm["tli"],   3),
    RMSEA       = round(fm["rmsea"], 3),
    RMSEA_lo    = round(fm["rmsea.ci.lower"], 3),
    RMSEA_hi    = round(fm["rmsea.ci.upper"], 3),
    SRMR        = round(fm["srmr"],  3),
    AIC         = round(fm["aic"],   1),
    BIC         = round(fm["bic"],   1),
    BIC2        = round(fm["bic2"],  1),
    stringsAsFactors = FALSE,
    row.names   = NULL
  )
}

# ── MEASUREMENT MODEL (common to all CFA models) ────────────
mm_base <- "
  PE =~ PE1 + PE2 + PE3
  EE =~ EE1 + EE2 + EE3
  SI =~ SI1 + SI2 + SI3
  FC =~ FC1 + FC2 + FC3 + FC4
  HM =~ HM1 + HM2 + HM3
  HB =~ HB1 + HB2 + HB3
  PT =~ PT1 + PT2 + PT3
  TA =~ TA1 + TA2 + TA3 + TA4
  BI =~ BI1 + BI2 + BI3 + BI4
  UB =~ UB1 + UB2 + UB3 + UB4 + UB5 + UB6 + UB7
  DL =~ DL1 + DL2 + DL3 + DL4 + DL5
"

# ============================================================
#  SECTION A: CFA MODEL COMPARISON
#  M1: Hypothesized 11-factor (target)
#  M2: Single-factor (all items → 1 factor)
#  M3: Two-factor (UTAUT exogenous + BI/UB endogenous)
#  M4: Merged FC+HM+HB (based on multicollinearity finding)
#  M5: Higher-order (2nd-order Technology Acceptance factor)
# ============================================================
cat(strrep("=", 70), "\n")
cat("SECTION A: CFA MODEL COMPARISON\n")
cat(strrep("=", 70), "\n")

## M1: Hypothesized 11-factor model
cat("\nFitting M1: 11-factor hypothesized CFA...\n")
m1_cfa <- cfa(mm_base, data = df, estimator = "MLR",
               std.lv = FALSE, missing = "fiml")
fit_m1 <- get_fit(m1_cfa, "M1", "Hypothesized: 11-factor reflective (UTAUT2)")
cat("  CFI =", fit_m1$CFI, "| RMSEA =", fit_m1$RMSEA, "| AIC =", fit_m1$AIC, "\n")

## M2: Single general factor (all items load on 1 latent)
cat("\nFitting M2: Single-factor model (strict null)...\n")
all_items_str <- paste(item_vars, collapse = " + ")
m2_model <- paste0("GF =~ ", all_items_str)
m2_cfa <- tryCatch(
  cfa(m2_model, data = df, estimator = "MLR",
      std.lv = FALSE, missing = "fiml"),
  error = function(e) { cat("  M2 failed:", e$message, "\n"); NULL }
)
fit_m2 <- if (!is.null(m2_cfa)) {
  get_fit(m2_cfa, "M2", "Single general factor (all 41 items)")
} else {
  data.frame(Model="M2", Description="Single-factor (failed)", chi2_scaled=NA,
             df=NA, p_chi2=NA, CFI=NA, TLI=NA, RMSEA=NA,
             RMSEA_lo=NA, RMSEA_hi=NA, SRMR=NA, AIC=NA, BIC=NA, BIC2=NA)
}

## M3: Two-factor model (Exogenous predictors vs Outcomes)
cat("\nFitting M3: Two-factor model (Predictors vs Outcomes)...\n")
m3_model <- "
  PRED =~ PE1+PE2+PE3+EE1+EE2+EE3+SI1+SI2+SI3+
           FC1+FC2+FC3+FC4+HM1+HM2+HM3+HB1+HB2+HB3+
           PT1+PT2+PT3+TA1+TA2+TA3+TA4+DL1+DL2+DL3+DL4+DL5
  OUT  =~ BI1+BI2+BI3+BI4+UB1+UB2+UB3+UB4+UB5+UB6+UB7
"
m3_cfa <- tryCatch(
  cfa(m3_model, data = df, estimator = "MLR",
      std.lv = FALSE, missing = "fiml"),
  error = function(e) { cat("  M3 failed:", e$message, "\n"); NULL }
)
fit_m3 <- if (!is.null(m3_cfa)) {
  get_fit(m3_cfa, "M3", "Two-factor: Predictors vs Outcomes")
} else {
  data.frame(Model="M3", Description="Two-factor (failed)", chi2_scaled=NA,
             df=NA, p_chi2=NA, CFI=NA, TLI=NA, RMSEA=NA,
             RMSEA_lo=NA, RMSEA_hi=NA, SRMR=NA, AIC=NA, BIC=NA, BIC2=NA)
}

## M4: 9-factor merged model (FC+HM+HB merged → FMH)
# Rationale: addresses multicollinearity (r > 0.93 between FC, HM, HB)
cat("\nFitting M4: 9-factor merged model (FC+HM+HB → FMH)...\n")
m4_model <- "
  PE  =~ PE1 + PE2 + PE3
  EE  =~ EE1 + EE2 + EE3
  SI  =~ SI1 + SI2 + SI3
  FMH =~ FC1 + FC2 + FC3 + FC4 + HM1 + HM2 + HM3 + HB1 + HB2 + HB3
  PT  =~ PT1 + PT2 + PT3
  TA  =~ TA1 + TA2 + TA3 + TA4
  BI  =~ BI1 + BI2 + BI3 + BI4
  UB  =~ UB1 + UB2 + UB3 + UB4 + UB5 + UB6 + UB7
  DL  =~ DL1 + DL2 + DL3 + DL4 + DL5
"
m4_cfa <- tryCatch(
  cfa(m4_model, data = df, estimator = "MLR",
      std.lv = FALSE, missing = "fiml"),
  error = function(e) { cat("  M4 failed:", e$message, "\n"); NULL }
)
fit_m4 <- if (!is.null(m4_cfa)) {
  get_fit(m4_cfa, "M4", "9-factor: FC+HM+HB merged (FMH composite)")
} else {
  data.frame(Model="M4", Description="9-factor merged (failed)", chi2_scaled=NA,
             df=NA, p_chi2=NA, CFI=NA, TLI=NA, RMSEA=NA,
             RMSEA_lo=NA, RMSEA_hi=NA, SRMR=NA, AIC=NA, BIC=NA, BIC2=NA)
}

## M5: 10-factor with second-order Technology Acceptance
# FC, HM, HB load on a 2nd-order "Technology Acceptance" (TAcc)
cat("\nFitting M5: 10-factor + 2nd-order TAcc (FC, HM, HB)...\n")
m5_model <- "
  PE  =~ PE1 + PE2 + PE3
  EE  =~ EE1 + EE2 + EE3
  SI  =~ SI1 + SI2 + SI3
  FC  =~ FC1 + FC2 + FC3 + FC4
  HM  =~ HM1 + HM2 + HM3
  HB  =~ HB1 + HB2 + HB3
  PT  =~ PT1 + PT2 + PT3
  TA  =~ TA1 + TA2 + TA3 + TA4
  BI  =~ BI1 + BI2 + BI3 + BI4
  UB  =~ UB1 + UB2 + UB3 + UB4 + UB5 + UB6 + UB7
  DL  =~ DL1 + DL2 + DL3 + DL4 + DL5
  TAcc =~ FC + HM + HB
"
m5_cfa <- tryCatch(
  cfa(m5_model, data = df, estimator = "MLR",
      std.lv = FALSE, missing = "fiml"),
  error = function(e) { cat("  M5 failed:", e$message, "\n"); NULL }
)
fit_m5 <- if (!is.null(m5_cfa)) {
  get_fit(m5_cfa, "M5", "2nd-order: TAcc factor over FC, HM, HB")
} else {
  data.frame(Model="M5", Description="2nd-order TAcc (failed)", chi2_scaled=NA,
             df=NA, p_chi2=NA, CFI=NA, TLI=NA, RMSEA=NA,
             RMSEA_lo=NA, RMSEA_hi=NA, SRMR=NA, AIC=NA, BIC=NA, BIC2=NA)
}

# Combine CFA comparison table
cfa_comparison <- bind_rows(fit_m1, fit_m2, fit_m3, fit_m4, fit_m5)

cat("\n", strrep("=", 90), "\n")
cat("CFA MODEL COMPARISON TABLE\n")
cat(strrep("=", 90), "\n")
print(cfa_comparison[, c("Model","Description","CFI","TLI","RMSEA","SRMR","AIC","BIC","BIC2")],
      row.names = FALSE)

# Chi-square difference tests (M1 vs nested models where applicable)
cat("\n--- Chi-Square Difference Tests (Scaled, MLR correction) ---\n")
if (!is.null(m2_cfa)) {
  cat("\nM1 vs M2 (11-factor vs 1-factor):\n")
  tryCatch(print(lavTestLRT(m2_cfa, m1_cfa)), error = function(e) cat("  Error:", e$message, "\n"))
}
if (!is.null(m4_cfa)) {
  cat("\nM1 vs M4 (11-factor vs 9-factor merged):\n")
  tryCatch(print(lavTestLRT(m4_cfa, m1_cfa)), error = function(e) cat("  Error:", e$message, "\n"))
}
if (!is.null(m5_cfa)) {
  cat("\nM1 vs M5 (11-factor vs 2nd-order TAcc):\n")
  tryCatch(print(lavTestLRT(m5_cfa, m1_cfa)), error = function(e) cat("  Error:", e$message, "\n"))
}

# ============================================================
#  SECTION B: SEM MODEL COMPARISON
#  S1: Full UTAUT2 model (12 structural paths — target)
#  S2: Trimmed model (remove ns paths from BI equation)
#  S3: Parsimonious (significant paths only)
#  S4: Saturated structural (all exogenous → UB directly)
# ============================================================
cat("\n\n", strrep("=", 70), "\n")
cat("SECTION B: SEM MODEL COMPARISON\n")
cat(strrep("=", 70), "\n")

## S1: Full model (hypothesized)
cat("\nFitting S1: Full UTAUT2 SEM (12 paths)...\n")
s1_model <- paste0(mm_base, "
  BI ~ PE + EE + SI + FC + HM + HB + PT + TA + DL
  UB ~ BI + FC + HB
")
s1_sem <- sem(s1_model, data = df, estimator = "MLR",
               std.lv = FALSE, missing = "fiml")
fit_s1 <- get_fit(s1_sem, "S1", "Full UTAUT2 (12 structural paths)")
cat("  CFI =", fit_s1$CFI, "| RMSEA =", fit_s1$RMSEA,
    "| AIC =", fit_s1$AIC, "| BIC =", fit_s1$BIC, "\n")

## S2: Trimmed — remove 6 non-significant paths to BI
# (PE, EE, SI, FC, HM, HB → BI all ns in S1)
cat("\nFitting S2: Trimmed SEM (remove PE+EE+SI+FC+HM+HB → BI)...\n")
s2_model <- paste0(mm_base, "
  BI ~ PT + TA + DL
  UB ~ BI + FC + HB
")
s2_sem <- sem(s2_model, data = df, estimator = "MLR",
               std.lv = FALSE, missing = "fiml")
fit_s2 <- get_fit(s2_sem, "S2", "Trimmed: PT+TA+DL → BI only")
cat("  CFI =", fit_s2$CFI, "| RMSEA =", fit_s2$RMSEA,
    "| AIC =", fit_s2$AIC, "| BIC =", fit_s2$BIC, "\n")

## S3: Parsimonious — only paths significant in BOTH CB-SEM and PLS-SEM
# TA→BI*, DL→BI*, BI→UB*, HB→UB**
cat("\nFitting S3: Parsimonious SEM (robust paths only)...\n")
s3_model <- paste0(mm_base, "
  BI ~ TA + DL
  UB ~ BI + HB
")
s3_sem <- sem(s3_model, data = df, estimator = "MLR",
               std.lv = FALSE, missing = "fiml")
fit_s3 <- get_fit(s3_sem, "S3", "Parsimonious: robust paths only (TA/DL→BI; BI/HB→UB)")
cat("  CFI =", fit_s3$CFI, "| RMSEA =", fit_s3$RMSEA,
    "| AIC =", fit_s3$AIC, "| BIC =", fit_s3$BIC, "\n")

## S4: Extended — add direct paths from all exogenous → UB
cat("\nFitting S4: Extended SEM (+ direct exogenous → UB)...\n")
s4_model <- paste0(mm_base, "
  BI ~ PE + EE + SI + FC + HM + HB + PT + TA + DL
  UB ~ BI + PE + EE + SI + FC + HM + HB + PT + TA + DL
")
s4_sem <- tryCatch(
  sem(s4_model, data = df, estimator = "MLR",
      std.lv = FALSE, missing = "fiml"),
  error = function(e) { cat("  S4 failed:", e$message, "\n"); NULL }
)
fit_s4 <- if (!is.null(s4_sem)) {
  get_fit(s4_sem, "S4", "Extended: all exogenous → UB directly")
} else {
  data.frame(Model="S4", Description="Extended (failed/non-identifiable)",
             chi2_scaled=NA, df=NA, p_chi2=NA, CFI=NA, TLI=NA, RMSEA=NA,
             RMSEA_lo=NA, RMSEA_hi=NA, SRMR=NA, AIC=NA, BIC=NA, BIC2=NA)
}

## S5: Mediation-only (PT, TA, DL → BI → UB, no direct FC/HB → UB)
cat("\nFitting S5: Full mediation (BI fully mediates → UB)...\n")
s5_model <- paste0(mm_base, "
  BI ~ PE + EE + SI + FC + HM + HB + PT + TA + DL
  UB ~ BI
")
s5_sem <- sem(s5_model, data = df, estimator = "MLR",
               std.lv = FALSE, missing = "fiml")
fit_s5 <- get_fit(s5_sem, "S5", "Full mediation: all → BI → UB (no direct FC/HB → UB)")
cat("  CFI =", fit_s5$CFI, "| RMSEA =", fit_s5$RMSEA,
    "| AIC =", fit_s5$AIC, "| BIC =", fit_s5$BIC, "\n")

# Combine SEM comparison
sem_comparison <- bind_rows(fit_s1, fit_s2, fit_s3, fit_s4, fit_s5)

cat("\n", strrep("=", 90), "\n")
cat("SEM MODEL COMPARISON TABLE\n")
cat(strrep("=", 90), "\n")
print(sem_comparison[, c("Model","Description","CFI","TLI","RMSEA","SRMR","AIC","BIC","BIC2")],
      row.names = FALSE)

# Chi-square difference tests (nested models only)
cat("\n--- Chi-Square Difference Tests ---\n")
cat("\nS1 vs S2 (Full vs Trimmed — nested: S2 is constrained S1):\n")
tryCatch(print(lavTestLRT(s2_sem, s1_sem)), error = function(e) cat("  Error:", e$message, "\n"))

cat("\nS2 vs S3 (Trimmed vs Parsimonious — nested):\n")
tryCatch(print(lavTestLRT(s3_sem, s2_sem)), error = function(e) cat("  Error:", e$message, "\n"))

cat("\nS1 vs S5 (Full paths vs Full mediation — nested):\n")
tryCatch(print(lavTestLRT(s5_sem, s1_sem)), error = function(e) cat("  Error:", e$message, "\n"))

if (!is.null(s4_sem)) {
  cat("\nS1 vs S4 (Full vs Extended — nested: S1 is constrained S4):\n")
  tryCatch(print(lavTestLRT(s1_sem, s4_sem)), error = function(e) cat("  Error:", e$message, "\n"))
}

# ── AIC/BIC interpretation guide ───────────────────────────
cat("\n", strrep("=", 70), "\n")
cat("AIC/BIC INTERPRETATION\n")
cat(strrep("=", 70), "\n")
cat("Lower AIC/BIC = better model (penalizes complexity)\n")
cat("ΔAIC > 2  : meaningful difference\n")
cat("ΔAIC > 10 : strong evidence\n")
cat("ΔBIC > 2  : positive evidence for simpler model\n")
cat("ΔBIC > 10 : very strong evidence for simpler model\n\n")

# ΔAIC and ΔBIC relative to best model
cat("--- CFA ΔAIC (vs best) ---\n")
best_aic_cfa <- min(cfa_comparison$AIC, na.rm=TRUE)
for (i in 1:nrow(cfa_comparison)) {
  if (!is.na(cfa_comparison$AIC[i])) {
    delta <- cfa_comparison$AIC[i] - best_aic_cfa
    cat(sprintf("  %s: ΔAIC = %+.1f %s\n",
        cfa_comparison$Model[i], delta,
        ifelse(delta == 0, "<-- best", ifelse(delta > 10, "(strongly worse)", ""))))
  }
}

cat("\n--- SEM ΔAIC (vs best) ---\n")
best_aic_sem <- min(sem_comparison$AIC, na.rm=TRUE)
best_bic_sem <- min(sem_comparison$BIC, na.rm=TRUE)
for (i in 1:nrow(sem_comparison)) {
  if (!is.na(sem_comparison$AIC[i])) {
    daic <- sem_comparison$AIC[i] - best_aic_sem
    dbic <- sem_comparison$BIC[i] - best_bic_sem
    cat(sprintf("  %s: ΔAIC = %+.1f | ΔBIC = %+.1f %s\n",
        sem_comparison$Model[i], daic, dbic,
        ifelse(daic == 0 & dbic == 0, "<-- best on both",
        ifelse(daic == 0, "<-- best AIC",
        ifelse(dbic == 0, "<-- best BIC", "")))))
  }
}

# ── Save all tables ──────────────────────────────────────────
cat("\n\nSaving tables...\n")

write.csv(cfa_comparison,
          file.path(out_dir, "13a_CFA_Model_Comparison.csv"),
          row.names = FALSE)
cat("  Saved: 13a_CFA_Model_Comparison.csv\n")

write.csv(sem_comparison,
          file.path(out_dir, "13b_SEM_Model_Comparison.csv"),
          row.names = FALSE)
cat("  Saved: 13b_SEM_Model_Comparison.csv\n")

# Combined publication table
pub_table_cfa <- cfa_comparison %>%
  mutate(
    chi2_str  = ifelse(is.na(chi2_scaled), "—",
                       sprintf("%.2f (df=%d, p=%s)",
                               chi2_scaled, df,
                               ifelse(p_chi2 < .001, "<.001",
                               sprintf("%.3f", p_chi2)))),
    RMSEA_str = ifelse(is.na(RMSEA), "—",
                       sprintf("%.3f [%.3f, %.3f]",
                               RMSEA, RMSEA_lo, RMSEA_hi)),
    Best_AIC  = (AIC == min(AIC, na.rm=TRUE)),
    Best_BIC  = (BIC == min(BIC, na.rm=TRUE))
  ) %>%
  select(Model, Description, chi2_str, CFI, TLI, RMSEA_str, SRMR, AIC, BIC, Best_AIC, Best_BIC)

write.csv(pub_table_cfa,
          file.path(out_dir, "13c_CFA_PubTable.csv"),
          row.names = FALSE)

pub_table_sem <- sem_comparison %>%
  mutate(
    chi2_str  = ifelse(is.na(chi2_scaled), "—",
                       sprintf("%.2f (df=%d, p=%s)",
                               chi2_scaled, df,
                               ifelse(p_chi2 < .001, "<.001",
                               sprintf("%.3f", p_chi2)))),
    RMSEA_str = ifelse(is.na(RMSEA), "—",
                       sprintf("%.3f [%.3f, %.3f]",
                               RMSEA, RMSEA_lo, RMSEA_hi)),
    Best_AIC  = (AIC == min(AIC, na.rm=TRUE)),
    Best_BIC  = (BIC == min(BIC, na.rm=TRUE))
  ) %>%
  select(Model, Description, chi2_str, CFI, TLI, RMSEA_str, SRMR, AIC, BIC, Best_AIC, Best_BIC)

write.csv(pub_table_sem,
          file.path(out_dir, "13d_SEM_PubTable.csv"),
          row.names = FALSE)

cat("  Saved: 13c_CFA_PubTable.csv\n")
cat("  Saved: 13d_SEM_PubTable.csv\n")
cat("\nModel comparison complete.\n")
