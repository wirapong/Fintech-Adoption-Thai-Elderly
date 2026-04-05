# ============================================================
#  Multicollinearity Diagnostics — UTAUT2 SEM
#  Methods: VIF, Tolerance, Condition Index, Determinant,
#           Eigenvalue, Correlation Matrix Inspection
# ============================================================
options(scipen = 999, digits = 4)

library(haven)
library(lavaan)
library(dplyr)
library(car)       # vif()
library(corrplot)  # correlation heatmap (check)

out_dir <- "E:/data/output"
DATA_PATH <- "E:/data/dataset.sav"

# ── 1. Load data & refit CFA/SEM ─────────────────────────────
df_raw <- read_sav(DATA_PATH)

item_vars <- c(
  "PE1","PE2","PE3","EE1","EE2","EE3","SI1","SI2","SI3",
  "FC1","FC2","FC3","FC4","HM1","HM2","HM3","HB1","HB2","HB3",
  "PT1","PT2","PT3","TA1","TA2","TA3","TA4",
  "BI1","BI2","BI3","BI4","UB1","UB2","UB3","UB4","UB5","UB6","UB7",
  "DL1","DL2","DL3","DL4","DL5"
)
df_items <- df_raw %>% select(all_of(item_vars))

# CFA model
cfa_model <- "
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

cat("Fitting CFA to extract factor scores...\n")
cfa_fit <- cfa(cfa_model, data = df_items, estimator = "MLR", missing = "fiml")

# ── 2. Extract latent factor scores ──────────────────────────
cat("\nExtracting latent factor scores (lavPredict)...\n")
fs <- as.data.frame(lavPredict(cfa_fit, type = "lv"))
cat("Factor score dimensions:", nrow(fs), "x", ncol(fs), "\n")
cat("Variables:", names(fs), "\n\n")

# ── 3. Correlation matrix of predictors ──────────────────────
cat(strrep("=", 60), "\n")
cat("STEP A: INTER-CONSTRUCT CORRELATION MATRIX\n")
cat(strrep("=", 60), "\n")

pred_names <- c("PE","EE","SI","FC","HM","HB","PT","TA","DL")
predictors  <- fs[, pred_names]
outcome_BI  <- fs[, "BI"]
outcome_UB  <- fs[, "UB"]

R_pred <- cor(predictors, use = "complete.obs")
cat("\nCorrelation matrix among 9 exogenous predictors (→ BI):\n")
print(round(R_pred, 3))

# ── 4. Determinant of R ──────────────────────────────────────
cat("\n--- Determinant of correlation matrix ---\n")
det_R <- det(R_pred)
cat(sprintf("det(R) = %.6f\n", det_R))
cat("Interpretation:\n")
if (det_R < 0.00001) {
  cat("  det(R) < 0.00001 → SEVERE multicollinearity\n")
} else if (det_R < 0.001) {
  cat("  det(R) < 0.001 → Moderate-severe multicollinearity\n")
} else if (det_R < 0.01) {
  cat("  det(R) < 0.01  → Moderate multicollinearity\n")
} else {
  cat("  det(R) >= 0.01  → Acceptable (low multicollinearity)\n")
}

# ── 5. VIF & Tolerance for BI outcome ────────────────────────
cat("\n", strrep("=", 60), "\n")
cat("STEP B: VIF & TOLERANCE — BI as outcome\n")
cat(strrep("=", 60), "\n")

lm_BI <- lm(outcome_BI ~ ., data = predictors)
vif_BI <- car::vif(lm_BI)
tol_BI <- 1 / vif_BI

vif_tbl_BI <- data.frame(
  Predictor = names(vif_BI),
  VIF       = round(vif_BI, 3),
  Tolerance = round(tol_BI, 3),
  Status    = case_when(
    vif_BI >= 10  ~ "SEVERE (VIF ≥ 10)",
    vif_BI >= 5   ~ "Moderate (5 ≤ VIF < 10)",
    vif_BI >= 2.5 ~ "Mild (2.5 ≤ VIF < 5)",
    TRUE          ~ "Acceptable (VIF < 2.5)"
  )
)
cat("\nVIF and Tolerance (Outcome: BI):\n")
print(vif_tbl_BI, row.names = FALSE)
cat(sprintf("\nMean VIF = %.3f | Max VIF = %.3f\n",
            mean(vif_BI), max(vif_BI)))

# ── 6. VIF for UB outcome (predictors: BI + FC + HB) ─────────
cat("\n", strrep("=", 60), "\n")
cat("STEP C: VIF & TOLERANCE — UB as outcome\n")
cat(strrep("=", 60), "\n")

preds_UB <- fs[, c("BI","FC","HB")]
lm_UB    <- lm(outcome_UB ~ ., data = preds_UB)
vif_UB   <- car::vif(lm_UB)
tol_UB   <- 1 / vif_UB

vif_tbl_UB <- data.frame(
  Predictor = names(vif_UB),
  VIF       = round(vif_UB, 3),
  Tolerance = round(tol_UB, 3),
  Status    = case_when(
    vif_UB >= 10 ~ "SEVERE",
    vif_UB >= 5  ~ "Moderate",
    vif_UB >= 2.5~ "Mild",
    TRUE         ~ "Acceptable"
  )
)
cat("\nVIF and Tolerance (Outcome: UB):\n")
print(vif_tbl_UB, row.names = FALSE)

# ── 7. Eigenvalue & Condition Index ──────────────────────────
cat("\n", strrep("=", 60), "\n")
cat("STEP D: EIGENVALUE & CONDITION INDEX\n")
cat(strrep("=", 60), "\n")

# Scale predictors first
X_scaled <- scale(predictors)
Xc <- cbind(1, X_scaled)                       # add intercept column
XtX <- t(Xc) %*% Xc
ev  <- eigen(XtX)$values
CI  <- sqrt(max(ev) / ev)

eigen_tbl <- data.frame(
  Component    = paste0("C", seq_along(ev)),
  Eigenvalue   = round(ev, 4),
  ConditionIdx = round(CI, 2),
  Status       = case_when(
    CI >= 30  ~ "SEVERE multicollinearity",
    CI >= 15  ~ "Moderate multicollinearity",
    CI >= 10  ~ "Mild multicollinearity",
    TRUE      ~ "Acceptable"
  )
)
cat("\nEigenvalue & Condition Index (predictors of BI, intercept included):\n")
print(eigen_tbl, row.names = FALSE)
cat(sprintf("\nMax Condition Index = %.2f\n", max(CI)))
cat("Interpretation: CI ≥ 30 = severe; CI ≥ 15 = moderate; CI ≥ 10 = mild\n")

# ── 8. Average VIF summary ───────────────────────────────────
cat("\n", strrep("=", 60), "\n")
cat("STEP E: OVERALL MULTICOLLINEARITY SUMMARY\n")
cat(strrep("=", 60), "\n")

cat(sprintf("\n%-30s %8s %10s %8s\n", "Predictor","VIF","Tolerance","Status"))
cat(strrep("-", 60), "\n")
for (i in seq_len(nrow(vif_tbl_BI))) {
  cat(sprintf("%-30s %8.3f %10.3f   %s\n",
              vif_tbl_BI$Predictor[i],
              vif_tbl_BI$VIF[i],
              vif_tbl_BI$Tolerance[i],
              vif_tbl_BI$Status[i]))
}
cat(strrep("-", 60), "\n")
cat(sprintf("%-30s %8.3f %10.3f\n", "MEAN", mean(vif_BI), mean(tol_BI)))
cat(sprintf("%-30s %8.3f %10.3f\n", "MAX",  max(vif_BI),  min(tol_BI)))

# ── 9. Pairwise correlation thresholds ───────────────────────
cat("\n", strrep("=", 60), "\n")
cat("STEP F: BIVARIATE CORRELATIONS ABOVE THRESHOLD\n")
cat(strrep("=", 60), "\n")

cat("\nCorrelation pairs |r| ≥ 0.70 (high correlation):\n")
high_corr <- which(abs(R_pred) >= 0.70 & upper.tri(R_pred), arr.ind = TRUE)
if (nrow(high_corr) == 0) {
  cat("  None found\n")
} else {
  for (k in seq_len(nrow(high_corr))) {
    i <- high_corr[k, 1]; j <- high_corr[k, 2]
    cat(sprintf("  %s — %s : r = %.3f\n",
                rownames(R_pred)[i], colnames(R_pred)[j], R_pred[i, j]))
  }
}

cat("\nCorrelation pairs |r| ≥ 0.60:\n")
mod_corr <- which(abs(R_pred) >= 0.60 & abs(R_pred) < 0.70 & upper.tri(R_pred), arr.ind = TRUE)
if (nrow(mod_corr) == 0) {
  cat("  None\n")
} else {
  for (k in seq_len(nrow(mod_corr))) {
    i <- mod_corr[k, 1]; j <- mod_corr[k, 2]
    cat(sprintf("  %s — %s : r = %.3f\n",
                rownames(R_pred)[i], colnames(R_pred)[j], R_pred[i, j]))
  }
}

# ── 10. Correlation heatmap ───────────────────────────────────
if (requireNamespace("corrplot", quietly = TRUE)) {
  library(corrplot)
  png(file.path(out_dir, "10_multicollinearity_heatmap.png"),
      width = 1200, height = 1100, res = 150)
  corrplot(R_pred,
           method    = "color",
           type      = "upper",
           order     = "hclust",
           addCoef.col = "black",
           number.cex  = 0.85,
           tl.col    = "black",
           tl.cex    = 1.0,
           col       = colorRampPalette(c("#B71C1C","#FFFFFF","#1565C0"))(200),
           title     = "Inter-Construct Correlation Matrix (Exogenous Predictors)",
           mar       = c(0, 0, 2, 0),
           diag      = FALSE,
           cl.lim    = c(-1, 1))
  dev.off()
  cat("\nSaved: 10_multicollinearity_heatmap.png\n")
}

# ── 11. Composite Reliability (AVE) comparison ───────────────
cat("\n", strrep("=", 60), "\n")
cat("STEP G: AVE vs. SQUARED CORRELATIONS (Fornell-Larcker)\n")
cat(strrep("=", 60), "\n")
cat("Discriminant validity: sqrt(AVE) > max correlation with other constructs\n\n")

AVE <- c(PE=0.756, EE=0.629, SI=0.557, FC=0.508,
         HM=0.707, HB=0.733, PT=0.700, TA=0.628, DL=0.631)
sqrtAVE <- sqrt(AVE)

# For each construct, find max |r| with other exogenous predictors
fl_tbl <- data.frame(
  Construct = names(AVE),
  AVE       = round(AVE, 3),
  sqrt_AVE  = round(sqrtAVE, 3)
)
cat("Fornell-Larcker Criterion (sqrt(AVE) should exceed all inter-construct r):\n")
for (fn in names(AVE)) {
  if (fn %in% rownames(R_pred)) {
    other <- setdiff(rownames(R_pred), fn)
    max_r <- max(abs(R_pred[fn, other]))
    ok <- sqrtAVE[fn] > max_r
    cat(sprintf("  %-4s: sqrt(AVE)=%.3f | max|r|=%.3f | %s\n",
                fn, sqrtAVE[fn], max_r,
                ifelse(ok, "OK", "CONCERN: sqrt(AVE) < max|r|")))
  }
}

# ── 12. Save summary tables ───────────────────────────────────
write.csv(vif_tbl_BI, file.path(out_dir, "10a_VIF_BI.csv"), row.names = FALSE)
write.csv(vif_tbl_UB, file.path(out_dir, "10b_VIF_UB.csv"), row.names = FALSE)
write.csv(eigen_tbl,  file.path(out_dir, "10c_ConditionIndex.csv"), row.names = FALSE)
write.csv(round(R_pred, 3), file.path(out_dir, "10d_Correlation_Predictors.csv"))

# ── 13. Final verdict ─────────────────────────────────────────
cat("\n", strrep("=", 60), "\n")
cat("FINAL MULTICOLLINEARITY VERDICT\n")
cat(strrep("=", 60), "\n")

max_vif <- max(vif_BI)
max_ci  <- max(CI)

cat(sprintf("\nMax VIF           : %.3f  (threshold: 5 = moderate, 10 = severe)\n", max_vif))
cat(sprintf("Min Tolerance     : %.3f  (threshold: < 0.20 = concern)\n", min(tol_BI)))
cat(sprintf("Max Condition Idx : %.2f  (threshold: 15 = moderate, 30 = severe)\n", max_ci))
cat(sprintf("det(R)            : %.6f (threshold: < 0.001 = concern)\n", det_R))

cat("\nConclusion:\n")
if (max_vif >= 10 | max_ci >= 30) {
  cat("  SEVERE multicollinearity detected.\n")
  cat("  Recommend: Ridge regression, PLS-SEM, or factor combination.\n")
} else if (max_vif >= 5 | max_ci >= 15) {

  cat("  MODERATE multicollinearity detected.\n")
  cat("  The high HTMT values (FC-HM=0.893, HB-FC=0.875) and large SEs\n")
  cat("  in SEM paths are consistent with moderate multicollinearity.\n")
  cat("  This may explain the non-significant paths in the full SEM model.\n")
  cat("  Recommend: Report in limitations; consider PLS-SEM as supplementary.\n")
} else {
  cat("  Multicollinearity is within acceptable limits.\n")
  cat("  Proceed with SEM results as reported.\n")
}
cat("\n")
