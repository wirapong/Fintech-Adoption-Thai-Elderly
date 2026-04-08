# ============================================================
#  Pre-Submission Verification — Structural Paths, p-values,
#  CIs, df, and Model Fit
#  Compares fresh R output vs stored CSVs (manuscript values)
# ============================================================
options(scipen = 999, digits = 10)  # full precision for verification

library(haven); library(dplyr); library(lavaan)

out_dir   <- "D:/ARIS/Auto-claude-code-research-in-sleep/output"
DATA_PATH <- "E:/ทุนFundamental Research Fund-2569/Data/dataset.sav"

df_raw    <- read_sav(DATA_PATH)
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

PASS <- "\u2713 PASS"
FAIL <- "\u2717 FAIL"
WARN <- "\u26a0 WARN"

tol_beta <- 1e-6   # tolerance for path coefficients
tol_fit  <- 1e-3   # tolerance for fit indices

cat(strrep("=", 72), "\n")
cat("PRE-SUBMISSION VERIFICATION REPORT\n")
cat(sprintf("R session: %s | lavaan: %s | N = %d\n",
            R.version$version.string,
            as.character(packageVersion("lavaan")), N))
cat(strrep("=", 72), "\n\n")

# ── RE-RUN MODELS (fresh, from scratch) ─────────────────────

mm <- "
  PE =~ PE1+PE2+PE3
  EE =~ EE1+EE2+EE3
  SI =~ SI1+SI2+SI3
  FC =~ FC1+FC2+FC3+FC4
  HM =~ HM1+HM2+HM3
  HB =~ HB1+HB2+HB3
  PT =~ PT1+PT2+PT3
  TA =~ TA1+TA2+TA3+TA4
  BI =~ BI1+BI2+BI3+BI4
  UB =~ UB1+UB2+UB3+UB4+UB5+UB6+UB7
  DL =~ DL1+DL2+DL3+DL4+DL5
"
set.seed(42)
cfa_fit <- cfa(mm, data = df, estimator = "MLR",
               std.lv = FALSE, missing = "fiml")

set.seed(42)
sem_fit <- sem(paste0(mm, "
  BI ~ PE+EE+SI+FC+HM+HB+PT+TA+DL
  UB ~ BI+FC+HB
"), data = df, estimator = "MLR", std.lv = FALSE, missing = "fiml")

# ── SECTION 1: MODEL FIT — CFA ───────────────────────────────
cat(strrep("-", 72), "\n")
cat("SECTION 1: CFA MODEL FIT (M1 — Hypothesized 11-factor)\n")
cat(strrep("-", 72), "\n")

cfa_fm   <- fitMeasures(cfa_fit, c(
  "chisq.scaled","df.scaled","pvalue.scaled",
  "cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr","aic","bic"))

# Stored values from 13a_CFA_Model_Comparison.csv (M1 row)
stored_cfa <- list(
  chi2_scaled = 1436.38, df = 764,   p_chi2 = 0,
  CFI = 0.907, TLI = 0.895, RMSEA = 0.060,
  RMSEA_lo = 0.057, RMSEA_hi = 0.064, SRMR = 0.043,
  AIC = 45836.1, BIC = 46571.7
)

fresh_cfa <- list(
  chi2_scaled = round(cfa_fm["chisq.scaled"], 2),
  df          = as.integer(cfa_fm["df.scaled"]),
  p_chi2      = round(cfa_fm["pvalue.scaled"], 3),
  CFI         = round(cfa_fm["cfi"],   3),
  TLI         = round(cfa_fm["tli"],   3),
  RMSEA       = round(cfa_fm["rmsea"], 3),
  RMSEA_lo    = round(cfa_fm["rmsea.ci.lower"], 3),
  RMSEA_hi    = round(cfa_fm["rmsea.ci.upper"], 3),
  SRMR        = round(cfa_fm["srmr"],  3),
  AIC         = round(cfa_fm["aic"],   1),
  BIC         = round(cfa_fm["bic"],   1)
)

cat(sprintf("\n%-20s %14s %14s %12s %s\n",
            "Index", "Stored(CSV)", "Fresh(R)", "Diff", "Check"))
cat(strrep("-", 72), "\n")

cfa_checks <- names(stored_cfa)
all_cfa_pass <- TRUE
for (k in cfa_checks) {
  sv  <- stored_cfa[[k]]
  fv  <- fresh_cfa[[k]]
  tol <- if (k %in% c("AIC","BIC","chi2_scaled")) 0.5 else tol_fit
  ok  <- !is.na(fv) && abs(fv - sv) <= tol
  if (!ok) all_cfa_pass <- FALSE
  cat(sprintf("%-20s %14s %14s %12.4f %s\n",
      k, as.character(sv), as.character(fv),
      ifelse(is.na(fv), NA, fv - sv),
      ifelse(ok, PASS, FAIL)))
}

# ── SECTION 2: MODEL FIT — SEM (S1) ──────────────────────────
cat("\n", strrep("-", 72), "\n")
cat("SECTION 2: SEM MODEL FIT (S1 — Full UTAUT2, 12 paths)\n")
cat(strrep("-", 72), "\n")

sem_fm <- fitMeasures(sem_fit, c(
  "chisq.scaled","df.scaled","pvalue.scaled",
  "cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr","aic","bic"))

# Stored values from 13b_SEM_Model_Comparison.csv (S1 row)
stored_sem <- list(
  chi2_scaled = 1521.37, df = 771,   p_chi2 = 0,
  CFI = 0.899, TLI = 0.887, RMSEA = 0.063,
  RMSEA_lo = 0.059, RMSEA_hi = 0.066, SRMR = 0.054,
  AIC = 45926.8, BIC = 46633.9
)

fresh_sem <- list(
  chi2_scaled = round(sem_fm["chisq.scaled"], 2),
  df          = as.integer(sem_fm["df.scaled"]),
  p_chi2      = round(sem_fm["pvalue.scaled"], 3),
  CFI         = round(sem_fm["cfi"],   3),
  TLI         = round(sem_fm["tli"],   3),
  RMSEA       = round(sem_fm["rmsea"], 3),
  RMSEA_lo    = round(sem_fm["rmsea.ci.lower"], 3),
  RMSEA_hi    = round(sem_fm["rmsea.ci.upper"], 3),
  SRMR        = round(sem_fm["srmr"],  3),
  AIC         = round(sem_fm["aic"],   1),
  BIC         = round(sem_fm["bic"],   1)
)

cat(sprintf("\n%-20s %14s %14s %12s %s\n",
            "Index", "Stored(CSV)", "Fresh(R)", "Diff", "Check"))
cat(strrep("-", 72), "\n")

all_sem_fit_pass <- TRUE
for (k in names(stored_sem)) {
  sv  <- stored_sem[[k]]
  fv  <- fresh_sem[[k]]
  tol <- if (k %in% c("AIC","BIC","chi2_scaled")) 0.5 else tol_fit
  ok  <- !is.na(fv) && abs(fv - sv) <= tol
  if (!ok) all_sem_fit_pass <- FALSE
  cat(sprintf("%-20s %14s %14s %12.4f %s\n",
      k, as.character(sv), as.character(fv),
      ifelse(is.na(fv), NA, fv - sv),
      ifelse(ok, PASS, FAIL)))
}

# ── SECTION 3: STRUCTURAL PATHS — full precision verification ─
cat("\n", strrep("-", 72), "\n")
cat("SECTION 3: STRUCTURAL PATH COEFFICIENTS\n")
cat("          β | SE | Z-stat | p-value | 95% CI\n")
cat(strrep("-", 72), "\n")

# Fresh from R
fresh_paths <- standardizedsolution(sem_fit) %>%
  filter(op == "~") %>%
  select(lhs, rhs, est.std, se, z, pvalue, ci.lower, ci.upper) %>%
  rename(Outcome=lhs, Predictor=rhs,
         Beta=est.std, SE=se, Z=z, p=pvalue,
         CI_lo=ci.lower, CI_hi=ci.upper) %>%
  mutate(Path = paste0(Predictor, "->", Outcome))

# Stored from CSV
stored_paths_csv <- read.csv(file.path(out_dir, "07_sem_paths.csv"),
                              stringsAsFactors = FALSE)
names(stored_paths_csv)[1] <- "Row"
stored_paths_csv <- stored_paths_csv %>%
  mutate(Path = paste0(Predictor, "->", Outcome))

# Significance labels for manuscript
sig_label <- function(p) {
  ifelse(p < .001, "***",
  ifelse(p < .01,  "**",
  ifelse(p < .05,  "*",
  ifelse(p < .10,  "†", "ns"))))
}

cat(sprintf("\n%-12s  %8s  %8s  %8s  %12s  %10s  %10s  %6s  %s\n",
    "Path", "β", "SE", "Z", "p-value", "CI_lo", "CI_hi", "Sig", "Status"))
cat(strrep("-", 95), "\n")

all_path_pass <- TRUE
path_results  <- list()

for (i in seq_len(nrow(fresh_paths))) {
  fp   <- fresh_paths[i, ]
  sprow <- stored_paths_csv %>% filter(Path == fp$Path)

  if (nrow(sprow) == 0) {
    cat(sprintf("  %-12s  PATH NOT FOUND IN STORED CSV — %s\n",
                fp$Path, WARN))
    all_path_pass <- FALSE
    next
  }

  beta_ok <- abs(fp$Beta - sprow$Beta) < tol_beta
  se_ok   <- abs(fp$SE   - sprow$SE)   < tol_beta
  z_ok    <- abs(fp$Z    - sprow$Z)    < tol_beta
  p_ok    <- abs(fp$p    - sprow$p)    < 1e-8
  ci_lo_ok <- abs(fp$CI_lo - sprow$CI_lower) < tol_beta
  ci_hi_ok <- abs(fp$CI_hi - sprow$CI_upper) < tol_beta
  all_ok  <- beta_ok && se_ok && z_ok && p_ok && ci_lo_ok && ci_hi_ok

  if (!all_ok) all_path_pass <- FALSE

  status <- ifelse(all_ok, PASS,
            paste(FAIL,
              ifelse(!beta_ok, "[β]", ""),
              ifelse(!se_ok,   "[SE]",""),
              ifelse(!z_ok,    "[Z]", ""),
              ifelse(!p_ok,    "[p]", ""),
              ifelse(!ci_lo_ok,"[CI_lo]",""),
              ifelse(!ci_hi_ok,"[CI_hi]","")))

  # Manuscript-ready p formatting
  p_str <- if (fp$p < .001) {
    sprintf("%.3e", fp$p)
  } else {
    sprintf("%.4f", fp$p)
  }

  cat(sprintf("%-12s  %8.4f  %8.4f  %8.4f  %12s  %10.4f  %10.4f  %6s  %s\n",
      fp$Path, fp$Beta, fp$SE, fp$Z, p_str,
      fp$CI_lo, fp$CI_hi, sig_label(fp$p), status))

  path_results[[i]] <- data.frame(
    Path=fp$Path, Beta=fp$Beta, SE=fp$SE, Z=fp$Z, p=fp$p,
    CI_lo=fp$CI_lo, CI_hi=fp$CI_hi, Sig=sig_label(fp$p),
    Beta_match=beta_ok, SE_match=se_ok, Z_match=z_ok,
    p_match=p_ok, CI_ok=ci_lo_ok & ci_hi_ok, All_OK=all_ok
  )
}

# ── SECTION 4: R² VALUES ─────────────────────────────────────
cat("\n", strrep("-", 72), "\n")
cat("SECTION 4: R² (ENDOGENOUS CONSTRUCTS)\n")
cat(strrep("-", 72), "\n")

r2_fresh <- inspect(sem_fit, "r2")
cat(sprintf("\n  R²(BI) = %.6f  [manuscript reports: %.3f]\n",
            r2_fresh["BI"], round(r2_fresh["BI"], 3)))
cat(sprintf("  R²(UB) = %.6f  [manuscript reports: %.3f]\n",
            r2_fresh["UB"], round(r2_fresh["UB"], 3)))

# Check for implausible R² (should be 0 < R² < 1)
r2_bi_ok <- r2_fresh["BI"] > 0 & r2_fresh["BI"] < 1
r2_ub_ok <- r2_fresh["UB"] > 0 & r2_fresh["UB"] < 1
cat(sprintf("  R²(BI) range check: %s\n", ifelse(r2_bi_ok, PASS, FAIL)))
cat(sprintf("  R²(UB) range check: %s\n", ifelse(r2_ub_ok, PASS, FAIL)))

# ── SECTION 5: CFA FACTOR LOADINGS verification ───────────────
cat("\n", strrep("-", 72), "\n")
cat("SECTION 5: CFA FACTOR LOADINGS (all 41 items)\n")
cat(strrep("-", 72), "\n")

fresh_loads <- standardizedsolution(cfa_fit) %>%
  filter(op == "=~") %>%
  select(lhs, rhs, est.std, se, z, pvalue) %>%
  rename(Factor=lhs, Item=rhs, StdLoading=est.std, SE=se, Z=z, p=pvalue)

stored_loads <- read.csv(file.path(out_dir, "05_cfa_loadings.csv"),
                          stringsAsFactors = FALSE)
names(stored_loads)[1] <- "Row"

n_load_fail <- 0
low_load_items <- character(0)

cat(sprintf("\n%-6s %-4s  %12s  %12s  %10s  %s\n",
            "Item","Fac","Stored_λ","Fresh_λ","Diff","Check"))
cat(strrep("-", 60), "\n")

for (i in seq_len(nrow(fresh_loads))) {
  fl   <- fresh_loads[i, ]
  sl   <- stored_loads %>% filter(Item == fl$Item & Factor == fl$Factor)
  if (nrow(sl) == 0) next
  diff <- fl$StdLoading - sl$StdLoading
  ok   <- abs(diff) < tol_beta
  low  <- fl$StdLoading < 0.70   # flag items below threshold
  if (!ok) n_load_fail <- n_load_fail + 1
  if (low)  low_load_items <- c(low_load_items,
                                 sprintf("%s(%s=%.3f)", fl$Item, fl$Factor, fl$StdLoading))
  flag <- ifelse(low, sprintf("%s %s", ifelse(ok, PASS, FAIL), WARN),
                       ifelse(ok, PASS, FAIL))
  cat(sprintf("%-6s %-4s  %12.8f  %12.8f  %10.6f  %s\n",
      fl$Item, fl$Factor, sl$StdLoading, fl$StdLoading, diff, flag))
}

if (length(low_load_items) > 0) {
  cat(sprintf("\n%s Items with loading < 0.70 (review for deletion):\n  %s\n",
      WARN, paste(low_load_items, collapse = "\n  ")))
}

# ── SECTION 6: DEGREES OF FREEDOM AUDIT ──────────────────────
cat("\n", strrep("-", 72), "\n")
cat("SECTION 6: DEGREES OF FREEDOM AUDIT\n")
cat(strrep("-", 72), "\n")

# CFA df: p*(p+1)/2 - free parameters
# p = 41 observed variables
p_obs <- length(item_vars)
known <- p_obs * (p_obs + 1) / 2

# Count free params in CFA: lavaan summary
cfa_summ <- lavInspect(cfa_fit, "free")
n_free_cfa <- lavInspect(cfa_fit, "npar")
df_cfa_calc <- known - n_free_cfa

cat(sprintf("\nCFA:\n"))
cat(sprintf("  Observed vars (p)         : %d\n", p_obs))
cat(sprintf("  Known data points p(p+1)/2: %d\n", known))
cat(sprintf("  Free parameters (lavaan)  : %d\n", n_free_cfa))
cat(sprintf("  Calculated df             : %d\n", df_cfa_calc))
cat(sprintf("  Reported df (scaled)      : %d\n", as.integer(cfa_fm["df.scaled"])))
cat(sprintf("  df match: %s\n",
    ifelse(df_cfa_calc == as.integer(cfa_fm["df.scaled"]), PASS, FAIL)))

n_free_sem <- lavInspect(sem_fit, "npar")
df_sem_calc <- known - n_free_sem

cat(sprintf("\nSEM (S1):\n"))
cat(sprintf("  Free parameters (lavaan)  : %d\n", n_free_sem))
cat(sprintf("  Calculated df             : %d\n", df_sem_calc))
cat(sprintf("  Reported df (scaled)      : %d\n", as.integer(sem_fm["df.scaled"])))
cat(sprintf("  Additional df from S1 paths: %d  (= SEM df - CFA df = %d - %d)\n",
    df_sem_calc - df_cfa_calc, df_sem_calc, df_cfa_calc))
cat(sprintf("  df match: %s\n",
    ifelse(df_sem_calc == as.integer(sem_fm["df.scaled"]), PASS, FAIL)))

# ── SECTION 7: MANUSCRIPT-READY TABLE (for copy-paste) ────────
cat("\n", strrep("=", 95), "\n")
cat("SECTION 7: MANUSCRIPT-READY STRUCTURAL PATHS TABLE\n")
cat("(Copy these values directly into the Results section)\n")
cat(strrep("=", 95), "\n")

hypotheses <- c(
  "PE->BI" = "H1: PE → BI",
  "EE->BI" = "H2: EE → BI",
  "SI->BI" = "H3: SI → BI",
  "FC->BI" = "H4: FC → BI",
  "HM->BI" = "H5: HM → BI",
  "HB->BI" = "H6: HB → BI",
  "PT->BI" = "H7: PT → BI",
  "TA->BI" = "H8: TA → BI",
  "DL->BI" = "H9: DL → BI",
  "BI->UB" = "H10: BI → UB",
  "FC->UB" = "H11: FC → UB",
  "HB->UB" = "H12: HB → UB"
)

cat(sprintf("\n%-16s  %7s  %7s  %7s  %14s  %22s  %8s  %s\n",
    "Hypothesis", "β", "SE", "z", "p-value",
    "95% CI [ll, ul]", "Sig", "Decision"))
cat(strrep("-", 105), "\n")

for (pth in names(hypotheses)) {
  fp <- fresh_paths %>% filter(Path == pth)
  if (nrow(fp) == 0) next

  p_fmt <- if (fp$p < .001) "< .001   " else sprintf("= %.4f", fp$p)
  ci_str <- sprintf("[%+.3f, %+.3f]", fp$CI_lo, fp$CI_hi)
  decision <- if (fp$p < .05) "Supported" else "Not Supported"
  sig <- sig_label(fp$p)

  cat(sprintf("%-16s  %7.3f  %7.3f  %7.3f  %14s  %22s  %8s  %s\n",
      hypotheses[pth], fp$Beta, fp$SE, fp$Z, p_fmt, ci_str, sig, decision))
}

r2_bi <- round(r2_fresh["BI"], 3)
r2_ub <- round(r2_fresh["UB"], 3)
cat(strrep("-", 105), "\n")
cat(sprintf("  Model fit: χ²(df=%d) = %.2f, p < .001 | CFI = %.3f | TLI = %.3f |\n",
    as.integer(sem_fm["df.scaled"]),
    sem_fm["chisq.scaled"],
    sem_fm["cfi"], sem_fm["tli"]))
cat(sprintf("  RMSEA = %.3f [%.3f, %.3f] | SRMR = %.3f | AIC = %.1f | BIC = %.1f\n",
    sem_fm["rmsea"], sem_fm["rmsea.ci.lower"], sem_fm["rmsea.ci.upper"],
    sem_fm["srmr"], sem_fm["aic"], sem_fm["bic"]))
cat(sprintf("  R²(BI) = %.3f | R²(UB) = %.3f\n", r2_bi, r2_ub))
cat(sprintf("  Estimator: MLR (Satorra-Bentler scaled χ²) | Missing: FIML | N = %d\n", N))

# ── SECTION 8: FINAL PASS/FAIL SUMMARY ───────────────────────
cat("\n", strrep("=", 72), "\n")
cat("SECTION 8: VERIFICATION SUMMARY\n")
cat(strrep("=", 72), "\n")

checks <- list(
  "CFA fit indices (11 values)"       = all_cfa_pass,
  "SEM fit indices (11 values)"       = all_sem_fit_pass,
  "Structural paths (all 12 paths)"   = all_path_pass,
  "CFA factor loadings (all 41)"      = (n_load_fail == 0),
  "R² range validity (BI & UB)"       = (r2_bi_ok & r2_ub_ok),
  "df audit (CFA & SEM)"              = (df_cfa_calc == as.integer(cfa_fm["df.scaled"]) &&
                                          df_sem_calc == as.integer(sem_fm["df.scaled"]))
)

all_passed <- all(unlist(checks))
cat("\n")
for (nm in names(checks)) {
  cat(sprintf("  %-45s %s\n", nm, ifelse(checks[[nm]], PASS, FAIL)))
}

cat(strrep("-", 72), "\n")
cat(sprintf("  OVERALL: %s\n\n", ifelse(all_passed,
    paste(PASS, "— All values verified. Safe to submit."),
    paste(FAIL, "— Discrepancies found. Review FAIL items above."))))

# ── Save verification report ─────────────────────────────────
path_results_df <- bind_rows(path_results)
write.csv(path_results_df,
          file.path(out_dir, "14_verification_report.csv"),
          row.names = FALSE)

# Save manuscript-ready table
ms_table <- fresh_paths %>%
  mutate(
    Hypothesis = hypotheses[Path],
    Beta_r     = round(Beta,  3),
    SE_r       = round(SE,    3),
    Z_r        = round(Z,     3),
    p_fmt      = ifelse(p < .001, "< .001", sprintf("%.4f", p)),
    CI_str     = sprintf("[%+.3f, %+.3f]", CI_lo, CI_hi),
    Sig        = sig_label(p),
    Decision   = ifelse(p < .05, "Supported", "Not Supported"),
    p_exact    = p
  ) %>%
  select(Hypothesis, Path, Beta_r, SE_r, Z_r, p_exact, p_fmt, CI_str, Sig, Decision)

write.csv(ms_table,
          file.path(out_dir, "14b_manuscript_paths_table.csv"),
          row.names = FALSE)

cat("Saved: 14_verification_report.csv\n")
cat("Saved: 14b_manuscript_paths_table.csv\n")
