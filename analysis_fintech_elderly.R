# ============================================================
#  FinTech Adoption Among Thai Elderly — Full Analysis Pipeline
#  Dataset: dataset.sav (430 obs, 61 vars) — UTAUT2 framework
#  R 4.4.1  |  Author: Wirapong Chansanam
# ============================================================

# ---------- 0. Setup -----------------------------------------
options(scipen = 999, digits = 4)
out_dir <- "E:/data/output"
dir.create(out_dir, showWarnings = FALSE)

library(haven)
library(dplyr)
library(psych)
library(naniar)
library(VIM)
library(mice)
library(lavaan)
library(semTools)
library(semPlot)
library(ggplot2)
library(rstatix)
library(knitr)

DATA_PATH <- "E:/Data/dataset.sav"

# ---------- STEP 1: Data Import & Inspection -----------------
cat("\n", strrep("=", 60), "\n")
cat("STEP 1: DATA IMPORT & INSPECTION\n")
cat(strrep("=", 60), "\n")

df_raw <- read_sav(DATA_PATH)
cat("Dimensions:", nrow(df_raw), "rows x", ncol(df_raw), "cols\n\n")

# Item variables only (exclude totals & demographics)
item_vars <- c(
  "PE1","PE2","PE3",
  "EE1","EE2","EE3",
  "SI1","SI2","SI3",
  "FC1","FC2","FC3","FC4",
  "HM1","HM2","HM3",
  "HB1","HB2","HB3",
  "PT1","PT2","PT3",
  "TA1","TA2","TA3","TA4",   # already reverse-coded in SPSS
  "BI1","BI2","BI3","BI4",
  "UB1","UB2","UB3","UB4","UB5","UB6","UB7",
  "DL1","DL2","DL3","DL4","DL5"
)

demo_vars <- c("Age","Gender","Education","Status","Income",
               "FinTech_Experience","Device","Internet_Experience")

df_items <- df_raw %>% select(all_of(item_vars))
df_demo  <- df_raw %>% select(all_of(demo_vars))

cat("Item variables:", length(item_vars), "\n")
cat("Demographic variables:", length(demo_vars), "\n\n")
cat("Value ranges (min/max per item):\n")
print(round(sapply(df_items, function(x) c(min=min(x,na.rm=T), max=max(x,na.rm=T))), 0))

# ---------- STEP 2: Missing Value Analysis -------------------
cat("\n", strrep("=", 60), "\n")
cat("STEP 2: MISSING VALUE ANALYSIS\n")
cat(strrep("=", 60), "\n")

cat("\nMissing values per variable (all 61 vars):\n")
miss_summary <- miss_var_summary(df_raw)
print(miss_summary %>% filter(n_miss > 0))

cat("\nNote: Only 'Age' has missing values (",
    miss_summary$pct_miss[miss_summary$variable=="Age"], "%)\n")
cat("Item variables: 0 missing — No imputation needed for scale items.\n")
cat("Age (demographic): 48 missing (11.2%) — excluded from group comparisons.\n")

# Missing pattern plot
p_miss <- vis_miss(df_raw %>% select(all_of(c(item_vars, "Age"))),
                   warn_large_data = FALSE) +
  labs(title = "Missing Value Pattern", subtitle = "Item variables + Age") +
  theme(axis.text.x = element_text(angle = 90, size = 7))
ggsave(file.path(out_dir, "01_missing_pattern.png"), p_miss, width=14, height=6, dpi=150)
cat("Saved: 01_missing_pattern.png\n")

# Little's MCAR test on full dataset
cat("\nLittle's MCAR Test:\n")
tryCatch({
  mcar_result <- mcar_test(df_raw %>% select(all_of(c(item_vars, "Age"))))
  print(mcar_result)
  if (mcar_result$p.value > 0.05) {
    cat("Result: p =", round(mcar_result$p.value, 3), "-> Fail to reject MCAR\n")
  } else {
    cat("Result: p =", round(mcar_result$p.value, 3), "-> Reject MCAR (MAR or MNAR)\n")
  }
}, error = function(e) cat("MCAR test error:", conditionMessage(e), "\n"))

# ---------- STEP 3: Multiple Imputation (Age only) -----------
cat("\n", strrep("=", 60), "\n")
cat("STEP 3: MULTIPLE IMPUTATION (Age only)\n")
cat(strrep("=", 60), "\n")

cat("Only Age has missing data — using mice with m=5 for sensitivity.\n")
cat("Main scale analyses use complete item data (n=430).\n")

df_for_imp <- df_raw %>% select(all_of(c(demo_vars)))
set.seed(42)
imp <- mice(df_for_imp, m = 5, method = "pmm", maxit = 20, printFlag = FALSE)
cat("Imputation converged. Checking convergence...\n")

png(file.path(out_dir, "02_mice_convergence.png"), width=1200, height=600, res=120)
plot(imp, main = "MICE Convergence Plot (Age imputation)")
dev.off()
cat("Saved: 02_mice_convergence.png\n")

df_complete_demo <- complete(imp, 1)  # first imputed dataset for demos
cat("Age after imputation — Mean:", round(mean(df_complete_demo$Age), 1),
    "| SD:", round(sd(df_complete_demo$Age), 1), "\n")

# ---------- STEP 4: Descriptive Statistics -------------------
cat("\n", strrep("=", 60), "\n")
cat("STEP 4: DESCRIPTIVE STATISTICS\n")
cat(strrep("=", 60), "\n")

desc <- describe(df_items)
desc_out <- desc %>%
  select(n, mean, sd, min, max, skew, kurtosis, se) %>%
  round(3)
cat("\nAPA-style Descriptive Statistics (Scale Items):\n")
print(desc_out)
write.csv(desc_out, file.path(out_dir, "03_descriptive_stats.csv"))
cat("Saved: 03_descriptive_stats.csv\n")

# Check normality (skew < |2|, kurtosis < |7| for SEM)
cat("\nNormality check (|skew| < 2, |kurtosis| < 7):\n")
non_normal <- desc_out %>%
  filter(abs(skew) > 2 | abs(kurtosis) > 7)
if (nrow(non_normal) == 0) {
  cat("All items within acceptable range for SEM.\n")
} else {
  cat("Items exceeding thresholds:\n")
  print(non_normal)
}

# Frequency tables for demographics
cat("\nDemographic Frequencies:\n")
for (v in c("Gender","Education","Status","Income","Device",
            "FinTech_Experience","Internet_Experience")) {
  cat("\n---", v, "---\n")
  print(table(df_demo[[v]]))
}

# Composite score descriptives
total_vars <- c("PET","EET","SIT","FCT","HMT","HBT","PTT","TAT","BIT","UBT","DLT")
desc_totals <- describe(df_raw %>% select(all_of(total_vars))) %>%
  select(n, mean, sd, min, max, skew, kurtosis) %>%
  round(3)
cat("\nComposite Score Descriptives:\n")
print(desc_totals)
write.csv(desc_totals, file.path(out_dir, "03b_composite_descriptives.csv"))

# ---------- STEP 5: Parallel Analysis ------------------------
cat("\n", strrep("=", 60), "\n")
cat("STEP 5: PARALLEL ANALYSIS\n")
cat(strrep("=", 60), "\n")

set.seed(42)
png(file.path(out_dir, "04_parallel_analysis.png"), width=1000, height=700, res=120)
pa_result <- fa.parallel(df_items, fm = "ml", fa = "both",
                          n.iter = 100, show.legend = TRUE,
                          main = "Parallel Analysis Scree Plot")
dev.off()
cat("Saved: 04_parallel_analysis.png\n")
cat("PA suggests", pa_result$nfact, "factors /", pa_result$ncomp, "components\n")

# ---------- STEP 6: Confirmatory Factor Analysis (CFA) -------
cat("\n", strrep("=", 60), "\n")
cat("STEP 6: CONFIRMATORY FACTOR ANALYSIS (CFA)\n")
cat(strrep("=", 60), "\n")

cfa_model <- "
  # Performance Expectancy
  PE =~ PE1 + PE2 + PE3
  # Effort Expectancy
  EE =~ EE1 + EE2 + EE3
  # Social Influence
  SI =~ SI1 + SI2 + SI3
  # Facilitating Conditions
  FC =~ FC1 + FC2 + FC3 + FC4
  # Hedonic Motivation
  HM =~ HM1 + HM2 + HM3
  # Habit (Behavior)
  HB =~ HB1 + HB2 + HB3
  # Personal Trust
  PT =~ PT1 + PT2 + PT3
  # Technology Anxiety (reverse-coded items)
  TA =~ TA1 + TA2 + TA3 + TA4
  # Behavioral Intention
  BI =~ BI1 + BI2 + BI3 + BI4
  # Use Behavior
  UB =~ UB1 + UB2 + UB3 + UB4 + UB5 + UB6 + UB7
  # Digital Literacy
  DL =~ DL1 + DL2 + DL3 + DL4 + DL5
"

cfa_fit <- cfa(cfa_model, data = df_items, estimator = "MLR",
               std.lv = FALSE, missing = "fiml")

cat("\nCFA Model Fit Indices:\n")
fit_indices <- fitMeasures(cfa_fit, c(
  "chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr",
  "aic","bic"
))
print(round(fit_indices, 3))

cat("\nFit Interpretation:\n")
cfi_val   <- fit_indices["cfi"]
tli_val   <- fit_indices["tli"]
rmsea_val <- fit_indices["rmsea"]
srmr_val  <- fit_indices["srmr"]
cat("CFI  =", round(cfi_val, 3),   ifelse(cfi_val >= 0.95, "[Good >=0.95]", ifelse(cfi_val >= 0.90, "[Acceptable >=0.90]", "[Poor]")), "\n")
cat("TLI  =", round(tli_val, 3),   ifelse(tli_val >= 0.95, "[Good >=0.95]", ifelse(tli_val >= 0.90, "[Acceptable >=0.90]", "[Poor]")), "\n")
cat("RMSEA=", round(rmsea_val, 3), ifelse(rmsea_val <= 0.05, "[Good <=0.05]", ifelse(rmsea_val <= 0.08, "[Acceptable <=0.08]", "[Poor]")), "\n")
cat("SRMR =", round(srmr_val, 3),  ifelse(srmr_val <= 0.08, "[Good <=0.08]", "[Poor]"), "\n")

cat("\nFactor Loadings (standardized):\n")
loadings_std <- standardizedsolution(cfa_fit) %>%
  filter(op == "=~") %>%
  select(lhs, rhs, est.std, se, z, pvalue) %>%
  rename(Factor=lhs, Item=rhs, StdLoading=est.std, SE=se, Z=z, p=pvalue)
loadings_print <- loadings_std %>%
  mutate(across(where(is.numeric), ~round(., 3)))
print(loadings_print)
write.csv(loadings_std, file.path(out_dir, "05_cfa_loadings.csv"))

# AVE and Composite Reliability
cat("\nAVE and Composite Reliability:\n")
reliability_out <- reliability(cfa_fit)
print(round(reliability_out, 3))
write.csv(as.data.frame(reliability_out), file.path(out_dir, "05b_reliability_AVE_CR.csv"))

# Modification indices (top 10)
mi <- modindices(cfa_fit, sort. = TRUE, maximum.number = 10)
cat("\nTop 10 Modification Indices:\n")
print(mi)

# CFA Path Diagram
png(file.path(out_dir, "06_cfa_path_diagram.png"), width=1400, height=1200, res=120)
semPaths(cfa_fit, whatLabels = "std", layout = "tree2",
         style = "ram", edge.label.cex = 0.7,
         residuals = FALSE, intercepts = FALSE,
         title = TRUE, title.color = "darkblue",
         mar = c(4,4,4,4))
title("CFA: UTAUT2 Measurement Model (Standardized Loadings)", cex.main=1.2)
dev.off()
cat("Saved: 06_cfa_path_diagram.png\n")

# Discriminant Validity (HTMT)
cat("\nHTMT Ratios (Discriminant Validity — threshold < 0.85):\n")
htmt_result <- htmt(cfa_model, data = df_items)
print(round(htmt_result, 3))
write.csv(as.data.frame(as.matrix(htmt_result)), file.path(out_dir, "05c_htmt.csv"))

# ---------- STEP 7: Structural Equation Modeling (SEM) ------
cat("\n", strrep("=", 60), "\n")
cat("STEP 7: STRUCTURAL EQUATION MODELING (SEM)\n")
cat(strrep("=", 60), "\n")

sem_model <- "
  # ---- Measurement Model ----
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

  # ---- Structural Paths ----
  # H1-H8: Predictors of Behavioral Intention
  BI ~ PE + EE + SI + FC + HM + HB + PT + TA + DL
  # H9-H11: Predictors of Use Behavior
  UB ~ BI + FC + HB
"

sem_fit <- sem(sem_model, data = df_items, estimator = "MLR",
               std.lv = FALSE, missing = "fiml")

cat("\nSEM Model Fit Indices:\n")
sem_fit_idx <- fitMeasures(sem_fit, c(
  "chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr",
  "aic","bic"
))
print(round(sem_fit_idx, 3))

cat("\nStructural Path Coefficients (standardized):\n")
sem_paths <- standardizedsolution(sem_fit) %>%
  filter(op == "~") %>%
  select(lhs, rhs, est.std, se, z, pvalue, ci.lower, ci.upper) %>%
  rename(Outcome=lhs, Predictor=rhs, Beta=est.std, SE=se, Z=z,
         p=pvalue, CI_lower=ci.lower, CI_upper=ci.upper) %>%
  mutate(Sig = case_when(p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "ns"))
sem_paths_print <- sem_paths %>% mutate(across(where(is.numeric), ~round(., 3)))
print(sem_paths_print %>% select(-Sig))
print(sem_paths_print %>% select(Outcome, Predictor, Beta, p, Sig))
write.csv(sem_paths, file.path(out_dir, "07_sem_paths.csv"))

cat("\nR-squared values:\n")
r2 <- inspect(sem_fit, "r2")
print(round(r2, 3))

# SEM Path Diagram
png(file.path(out_dir, "08_sem_path_diagram.png"), width=1600, height=1200, res=120)
semPaths(sem_fit, whatLabels = "std", layout = "tree2",
         style = "ram", edge.label.cex = 0.65,
         residuals = FALSE, intercepts = FALSE,
         what = "std", nCharNodes = 3,
         title = TRUE, title.color = "darkblue",
         mar = c(4,4,4,4),
         edge.color = "darkblue")
title("SEM: UTAUT2 FinTech Adoption (Standardized)", cex.main = 1.2)
dev.off()
cat("Saved: 08_sem_path_diagram.png\n")

# ---------- STEP 8: Comparative Statistics ------------------
cat("\n", strrep("=", 60), "\n")
cat("STEP 8: COMPARATIVE STATISTICS\n")
cat(strrep("=", 60), "\n")

df_analysis <- bind_cols(
  df_raw %>% select(all_of(total_vars)),
  df_raw %>% select(all_of(demo_vars))
)

cat("\n--- Gender Differences (t-test) ---\n")
gender_clean <- df_analysis %>% filter(!is.na(Gender))
t_gender <- t_test(gender_clean, BIT ~ Gender, var.equal = FALSE)
cd_gender <- cohens_d(gender_clean, BIT ~ Gender)
cat("BIT by Gender:\n"); print(t_gender); print(cd_gender)

t_ubi_gender <- t_test(gender_clean, UBT ~ Gender, var.equal = FALSE)
cd_ubi_gender <- cohens_d(gender_clean, UBT ~ Gender)
cat("UBT by Gender:\n"); print(t_ubi_gender); print(cd_ubi_gender)

cat("\n--- Education Differences (ANOVA) ---\n")
edu_clean <- df_analysis %>% filter(!is.na(Education)) %>%
  mutate(Education = factor(Education))
anova_bi_edu <- anova_test(edu_clean, BIT ~ Education)
cat("BIT by Education:\n"); print(anova_bi_edu)

cat("\n--- Age Group Differences ---\n")
df_age <- bind_cols(
  df_raw %>% select(all_of(total_vars)),
  df_complete_demo
) %>%
  mutate(AgeGroup = cut(Age, breaks = c(59, 65, 70, 75, 100),
                        labels = c("60-65","66-70","71-75","76+")))
cat("Age group distribution:\n")
print(table(df_age$AgeGroup))

anova_bi_age <- anova_test(df_age, BIT ~ AgeGroup)
cat("BIT by Age Group:\n"); print(anova_bi_age)

anova_ub_age <- anova_test(df_age, UBT ~ AgeGroup)
cat("UBT by Age Group:\n"); print(anova_ub_age)

# Post-hoc Tukey if significant
if (anova_bi_age$p < 0.05) {
  cat("Post-hoc Tukey (BIT ~ AgeGroup):\n")
  tukey_bi <- tukey_hsd(df_age, BIT ~ AgeGroup)
  print(tukey_bi)
}

# Correlation matrix of composites
cat("\n--- Correlation Matrix (Composite Scores) ---\n")
corr_mat <- cor(df_raw %>% select(all_of(total_vars)), use = "complete.obs")
print(round(corr_mat, 3))
write.csv(round(corr_mat, 3), file.path(out_dir, "09_correlation_matrix.csv"))

# ---------- SUMMARY OUTPUT ----------------------------------
cat("\n", strrep("=", 60), "\n")
cat("ANALYSIS COMPLETE — Output files saved to:\n", out_dir, "\n")
cat(strrep("=", 60), "\n")

cat("\nOutput files:\n")
for (f in list.files(out_dir)) cat(" -", f, "\n")

# Save session info
sink(file.path(out_dir, "session_info.txt"))
sessionInfo()
sink()

# ============================================================
#  Publication-Quality CFA & SEM Path Diagrams
#  UTAUT2 FinTech Adoption Among Thai Elderly
#  n = 430 | R 4.4.1 | lavaan MLR | FIML
# ============================================================
library(ggplot2)
library(ggforce)   # geom_ellipse, geom_circle
library(ggrepel)
library(patchwork)
library(dplyr)
library(grid)

out_dir <- "E:/data/output"

# ─── Shared aesthetics ───────────────────────────────────────
COL <- c(
  PE="#1565C0", EE="#2E7D32", SI="#880E4F",
  FC="#4A148C", HM="#BF360C", HB="#004D40",
  PT="#311B92", TA="#263238", BI="#01579B",
  UB="#1B5E20", DL="#B71C1C"
)
COL_LIGHT <- setNames(paste0(COL, "22"), names(COL))
BASE_FONT  <- "serif"   # Times-like for academic figures

theme_pub <- theme_void(base_family = BASE_FONT) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.margin      = margin(14, 14, 14, 14),
    plot.title       = element_text(size = 12, face = "bold",
                                    hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9,  hjust = 0.5,
                                    colour = "#555555", margin = margin(b = 2)),
    plot.caption     = element_text(size = 8,  hjust = 0.5,
                                    colour = "#555555", margin = margin(t = 6))
  )

# ─── Helper: draw_node_ellipse ────────────────────────────────
# Returns a data.frame of polygon vertices for one ellipse
make_ellipse <- function(cx, cy, rx, ry, id = "", n = 120) {
  th <- seq(0, 2*pi, length.out = n + 1)[-1]
  data.frame(x = cx + rx*cos(th), y = cy + ry*sin(th), id = as.character(id))
}

# ─── Helper: arrow segment with offset from circle edge ──────
seg <- function(x0, y0, x1, y1, rx0 = 0, ry0 = 0, rx1 = 0, ry1 = 0) {
  # shrink start point to circle edge
  dx <- x1 - x0; dy <- y1 - y0; d <- sqrt(dx^2 + dy^2)
  ux <- dx/d; uy <- dy/d
  data.frame(
    x    = x0 + ux * rx0,
    y    = y0 + uy * ry0,
    xend = x1 - ux * rx1,
    yend = y1 - uy * ry1
  )
}

# ============================================================
# FIGURE 1 — CFA MEASUREMENT MODEL
# Layout  : Items (rectangles) at y=1 | Factors (circles) at y=7
# Canvas  : 22 × 9 inches, 400 DPI
# ============================================================

# ── Factor / item definitions ────────────────────────────────
fdef <- list(
  PE = list(it = c("PE1","PE2","PE3"),        ld = c(0.796, 0.903, 0.914)),
  EE = list(it = c("EE1","EE2","EE3"),        ld = c(0.803, 0.746, 0.828)),
  SI = list(it = c("SI1","SI2","SI3"),         ld = c(0.775, 0.825, 0.625)),
  FC = list(it = c("FC1","FC2","FC3","FC4"),   ld = c(0.717, 0.644, 0.741, 0.733)),
  HM = list(it = c("HM1","HM2","HM3"),        ld = c(0.826, 0.859, 0.839)),
  HB = list(it = c("HB1","HB2","HB3"),        ld = c(0.888, 0.871, 0.801)),
  PT = list(it = c("PT1","PT2","PT3"),         ld = c(0.838, 0.868, 0.804)),
  TA = list(it = c("TA1","TA2","TA3","TA4"),  ld = c(0.720, 0.790, 0.867, 0.786)),
  BI = list(it = c("BI1","BI2","BI3","BI4"),  ld = c(0.775, 0.824, 0.789, 0.845)),
  UB = list(it = c("UB1","UB2","UB3","UB4","UB5","UB6","UB7"),
            ld = c(0.486, 0.805, 0.838, 0.790, 0.723, 0.735, 0.758)),
  DL = list(it = c("DL1","DL2","DL3","DL4","DL5"),
            ld = c(0.669, 0.819, 0.862, 0.795, 0.802))
)

item_gap  <- 0.90   # horizontal spacing between items within same factor
group_gap <- 0.65   # extra gap between factor groups
ITEM_Y    <- 1.0
FACTOR_Y  <- 7.2
RX_F      <- 0.72   # factor ellipse x-radius
RY_F      <- 0.42   # factor ellipse y-radius
ITEM_W    <- 0.50   # item rectangle half-width
ITEM_H    <- 0.28   # item rectangle half-height

# ── Compute coordinates ──────────────────────────────────────
cursor <- 0.5
items_df   <- data.frame()
factors_df <- data.frame()
arrows_df  <- data.frame()

for (fn in names(fdef)) {
  fd  <- fdef[[fn]]
  n   <- length(fd$it)
  xs  <- cursor + (0:(n-1)) * item_gap
  fcx <- mean(xs)
  
  # items
  items_df <- rbind(items_df, data.frame(
    item = fd$it, loading = fd$ld,
    x = xs, y = ITEM_Y, factor = fn,
    stringsAsFactors = FALSE
  ))
  
  # factor
  factors_df <- rbind(factors_df, data.frame(
    factor = fn, x = fcx, y = FACTOR_Y,
    stringsAsFactors = FALSE
  ))
  
  # arrows: from factor bottom edge to item top edge
  for (i in seq_along(fd$it)) {
    ix <- xs[i]; iy <- ITEM_Y
    dx <- ix - fcx; dy <- iy - FACTOR_Y; d <- sqrt(dx^2 + dy^2)
    ux <- dx/d; uy <- dy/d
    arrows_df <- rbind(arrows_df, data.frame(
      factor  = fn,
      x       = fcx + ux * RX_F,
      y       = FACTOR_Y + uy * RY_F,
      xend    = ix,
      yend    = iy + ITEM_H,
      midx    = (fcx + ix)/2 + ux * 0.05,
      midy    = (FACTOR_Y + iy)/2,
      loading = fd$ld[i],
      angle   = atan2(FACTOR_Y - iy, fcx - ix) * 180/pi,
      stringsAsFactors = FALSE
    ))
  }
  
  cursor <- max(xs) + item_gap + group_gap
}

total_width <- cursor - group_gap

# ── Group shading backgrounds ────────────────────────────────
shade_df <- items_df %>%
  group_by(factor) %>%
  summarise(xmin = min(x) - 0.38, xmax = max(x) + 0.38,
            ymin = ITEM_Y - 0.55,  ymax = FACTOR_Y + RY_F + 0.15,
            .groups = "drop")

# ── Build factor ellipses ─────────────────────────────────────
ell_df <- bind_rows(lapply(seq_len(nrow(factors_df)), function(i) {
  row <- factors_df[i,]
  make_ellipse(row$x, row$y, RX_F, RY_F, id = row$factor)
}))

# ── Factor full name labels (subtitle below circles) ─────────
fname_map <- c(
  PE="Performance\nExpectancy", EE="Effort\nExpectancy",
  SI="Social\nInfluence",       FC="Facilitating\nConditions",
  HM="Hedonic\nMotivation",     HB="Habit",
  PT="Personal\nTrust",         TA="Technology\nAnxiety",
  BI="Behavioral\nIntention",   UB="Use\nBehavior",
  DL="Digital\nLiteracy"
)
factors_df$fullname <- fname_map[factors_df$factor]

# ── Plot CFA ─────────────────────────────────────────────────
p_cfa <- ggplot() +
  
  # Light column shading per factor
  geom_rect(data = shade_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = factor),
            alpha = 0.06, colour = NA) +
  
  # Vertical divider lines between groups
  geom_segment(data = shade_df %>% filter(factor != last(factor)),
               aes(x = xmax + 0.02, xend = xmax + 0.02,
                   y = ITEM_Y - 0.45, yend = FACTOR_Y - RY_F - 0.1),
               colour = "#CCCCCC", linewidth = 0.3, linetype = "dashed") +
  
  # Loading arrows (behind everything)
  geom_segment(data = arrows_df,
               aes(x = x, y = y, xend = xend, yend = yend,
                   colour = factor),
               arrow     = arrow(length = unit(0.08, "inches"),
                                 type = "closed"),
               linewidth = 0.35,
               alpha     = 0.70) +
  
  # Loading value labels — positioned at 35% along arrow from item
  geom_label(data = arrows_df,
             aes(x    = x * 0.38 + xend * 0.62,
                 y    = y * 0.38 + yend * 0.62,
                 label = sprintf("%.3f", loading),
                 colour = factor),
             size       = 2.2,
             label.size = 0,
             fill       = "white",
             fontface   = "plain",
             alpha      = 0.92,
             family     = BASE_FONT) +
  
  # Factor ellipses — filled
  geom_polygon(data = ell_df,
               aes(x = x, y = y, group = id, fill = id),
               colour    = "white",
               linewidth = 1.0,
               alpha     = 0.88) +
  
  # Factor abbreviation labels
  geom_text(data = factors_df,
            aes(x = x, y = y + 0.06, label = factor, colour = factor),
            size     = 3.6,
            fontface = "bold",
            colour   = "white",
            family   = BASE_FONT) +
  
  # Factor full-name subtitle (below circle)
  geom_text(data = factors_df,
            aes(x = x, y = FACTOR_Y - RY_F - 0.28, label = fullname),
            size     = 1.85,
            colour   = "#333333",
            vjust    = 1,
            lineheight = 0.82,
            family   = BASE_FONT) +
  
  # Item rectangles
  geom_rect(data = items_df,
            aes(xmin = x - ITEM_W, xmax = x + ITEM_W,
                ymin = ITEM_Y - ITEM_H, ymax = ITEM_Y + ITEM_H),
            fill      = "white",
            colour    = "#333333",
            linewidth = 0.55) +
  
  # Item labels
  geom_text(data = items_df,
            aes(x = x, y = ITEM_Y, label = item, colour = factor),
            size     = 2.6,
            fontface = "bold",
            family   = BASE_FONT) +
  
  scale_fill_manual(values   = COL, guide = "none") +
  scale_colour_manual(values = COL, guide = "none") +
  coord_fixed(ratio = 1,
              xlim = c(-0.2, total_width + 0.3),
              ylim = c(0.1, 8.1)) +
  labs(
    title    = "Figure 1. Confirmatory Factor Analysis: UTAUT2 Measurement Model",
    subtitle = "Standardized factor loadings shown on paths (all p < .001) | MLR estimator, FIML | n = 430",
    caption  = paste0(
      "Model fit: \u03C7\u00B2(764) = 1962.35, p < .001 | ",
      "CFI = 0.907 | TLI = 0.895 | RMSEA = 0.060 [90% CI: 0.057, 0.064] | SRMR = 0.043\n",
      "AVE range: 0.508\u20130.756 | Composite Reliability (\u03C9) range: 0.788\u20130.905 | All AVE > 0.50"
    )
  ) +
  theme_pub

ggsave(file.path(out_dir, "CFA_Publication_Quality.png"),
       p_cfa, width = 22, height = 9, dpi = 400, bg = "white")
cat("Saved: CFA_Publication_Quality.png\n")


# ============================================================
# FIGURE 2 — SEM STRUCTURAL MODEL (Publication Quality)
# Layout : Exogenous factors → BI → UB (hierarchical)
# Shows  : Latent variables only (clean structural diagram)
#          + compact measurement loadings as annotations
# Canvas : 14 × 11 inches, 400 DPI
# ============================================================
#
# SEM structural paths (standardized):
#   PE→BI: 0.060 ns   EE→BI: -0.110 ns   SI→BI: 0.058 ns
#   FC→BI: 0.142 ns   HM→BI: 0.160 ns    HB→BI: 0.180 ns
#   PT→BI: 0.227*     TA→BI: -0.237***   DL→BI: 0.164*
#   BI→UB: 0.301*     FC→UB: -0.109 ns   HB→UB: 0.557 ns
#   R²(BI) = 0.734    R²(UB) = 0.510

# ── Node positions (manually laid out) ───────────────────────
# Exogenous layer y=8.5 (5 nodes) and y=6.0 (4 nodes)
# BI at y=3.5 centre, UB at y=1.0 right

node_df <- data.frame(
  id = c("PE","EE","SI","FC","HM","HB","PT","TA","DL","BI","UB"),
  x  = c(2.0, 4.5, 7.0, 9.5, 12.0,   # row 1 — 5 nodes
         3.0, 6.0, 9.0, 12.0,          # row 2 — 4 nodes
         7.0,                           # BI
         10.0),                         # UB
  y  = c(9.0, 9.0, 9.0, 9.0, 9.0,
         6.0, 6.0, 6.0, 6.0,
         3.0,
         0.5),
  rx = 0.80, ry = 0.50,
  type = c(rep("exo",9), "endo","endo"),
  stringsAsFactors = FALSE
)

# Loadings summary (min–max) per factor
load_range <- c(
  PE="0.80\u20130.91", EE="0.75\u20130.83", SI="0.63\u20130.83",
  FC="0.64\u20130.74", HM="0.83\u20130.86", HB="0.80\u20130.89",
  PT="0.80\u20130.87", TA="0.72\u20130.87", BI="0.78\u20130.85",
  UB="0.49\u20130.84", DL="0.67\u20130.86"
)
item_count <- c(PE=3,EE=3,SI=3,FC=4,HM=3,HB=3,PT=3,TA=4,BI=4,UB=7,DL=5)

# ── Structural paths data ─────────────────────────────────────
path_df <- data.frame(
  from  = c("PE","EE","SI","FC","HM","HB","PT","TA","DL","BI","FC","HB"),
  to    = c("BI","BI","BI","BI","BI","BI","BI","BI","BI","UB","UB","UB"),
  beta  = c(0.060,-0.110,0.058,0.142,0.160,0.180,0.227,-0.237,0.164,0.301,-0.109,0.557),
  sig   = c("ns","ns","ns","ns","ns","ns","*","***","*","*","ns","ns"),
  stringsAsFactors = FALSE
) %>%
  left_join(node_df %>% select(id,x0=x,y0=y,rx0=rx,ry0=ry), by=c("from"="id")) %>%
  left_join(node_df %>% select(id,x1=x,y1=y,rx1=rx,ry1=ry), by=c("to"="id")) %>%
  mutate(
    dx    = x1 - x0, dy = y1 - y0, d = sqrt(dx^2+dy^2),
    ux    = dx/d,    uy = dy/d,
    xs    = x0 + ux * rx0,
    ys    = y0 + uy * ry0,
    xe    = x1 - ux * rx1,
    ye    = y1 - uy * ry1,
    midx  = (xs+xe)/2,
    midy  = (ys+ye)/2,
    is_sig = sig != "ns",
    lw    = ifelse(sig == "***", 1.4, ifelse(sig %in% c("*","**"), 1.0, 0.5)),
    ltype = ifelse(sig == "ns", "dashed", "solid"),
    lbl   = sprintf("\u03B2 = %+.3f%s",beta,
                    ifelse(sig=="***","***",ifelse(sig=="**","**",
                                                   ifelse(sig=="*","*","")))),
    lbl_col = ifelse(sig == "ns", "#999999",
                     ifelse(beta < 0, "#B71C1C", "#1B5E20"))
  )

# ── Indicator blocks (compact summary) ───────────────────────
# Show items as tiny stacked rectangles beside each factor node
# Helper: build indicator mini-block coordinates
make_ind_block <- function(fcx, fcy, items, loads, side = "above") {
  n  <- length(items)
  bw <- 0.28; bh <- 0.18; gap <- 0.02
  # stack vertically above or below factor node
  y_off <- if (side=="above") fcy + 0.52 + (n-1)*(bh+gap)/2 else fcy
  ys <- if (side=="above")
    fcy + 0.52 + seq(0, (n-1)) * (bh+gap)
  else
    fcy - 0.52 - seq(0, (n-1)) * (bh+gap)
  # center the column
  data.frame(
    x    = fcx,
    ymin = ys - bh/2, ymax = ys + bh/2,
    xmin = fcx - bw,  xmax = fcx + bw,
    item = items,
    ld   = loads,
    ax   = fcx,       ay   = if(side=="above") fcy + 0.52 else fcy - 0.52,
    ix   = fcx,       iy   = ys
  )
}

# Build indicator positions for all factors
ind_list <- lapply(names(fdef), function(fn) {
  row <- node_df[node_df$id == fn,]
  fd  <- fdef[[fn]]
  make_ind_block(row$x, row$y, fd$it, fd$ld, side="above")
})
all_ind <- bind_rows(setNames(ind_list, names(fdef)), .id="factor") %>%
  mutate(col = COL[factor])

# Arrows from factor center to each item box
ind_arr <- all_ind %>%
  left_join(node_df %>% select(id, fcx=x, fcy=y, rx=rx, ry=ry),
            by=c("factor"="id")) %>%
  mutate(
    dx = ix - fcx, dy = iy - fcy, d = sqrt(dx^2+dy^2),
    ux = dx/d,     uy = dy/d,
    xs = fcx + ux*rx,
    ys = fcy + uy*ry,
    xe = ix,
    ye = iy - 0.10,  # top of rect
    col = COL[factor]
  )

# ── Build factor ellipses ─────────────────────────────────────
sem_ell <- bind_rows(lapply(seq_len(nrow(node_df)), function(i) {
  r <- node_df[i,]
  make_ellipse(r$x, r$y, r$rx, r$ry, id = r$id)
}))
sem_ell$type  <- node_df$type[match(sem_ell$id, node_df$id)]
sem_ell$col   <- ifelse(sem_ell$type == "endo", "#1A237E",
                        COL[sem_ell$id])

# R² annotations
r2_df <- data.frame(
  id = c("BI","UB"),
  x  = c(7.0, 10.0),
  y  = c(3.0, 0.5),
  lbl = c("R\u00B2 = .734", "R\u00B2 = .510")
)

# ── Plot SEM ─────────────────────────────────────────────────
# Determine axis limits
xlim_sem <- c(0.5, 14.0)
ylim_sem <- c(-0.8, 11.2)

p_sem <- ggplot() +
  
  # ── Measurement model: item boxes ──────────────────────────
  geom_rect(data = all_ind,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = col),
            colour = "#888888", linewidth = 0.25, alpha = 0.15) +
  
  # Loading arrows (measurement)
  geom_segment(data = ind_arr,
               aes(x = xs, y = ys, xend = xe, yend = ye,
                   colour = col),
               arrow     = arrow(length = unit(0.055,"inches"), type="closed"),
               linewidth = 0.25, alpha = 0.55) +
  
  # Item labels
  geom_text(data = all_ind,
            aes(x = x, y = (ymin+ymax)/2, label = item,
                colour = col),
            size     = 1.55,
            fontface = "bold",
            family   = BASE_FONT) +
  
  # Loading values (tiny, on arrows)
  geom_text(data = ind_arr,
            aes(x = (xs+xe)/2 + 0.14, y = (ys+ye)/2,
                label = sprintf("%.2f", ld),
                colour = col),
            size   = 1.4,
            hjust  = 0,
            family = BASE_FONT) +
  
  # ── Structural paths ────────────────────────────────────────
  geom_segment(data = path_df,
               aes(x = xs, y = ys, xend = xe, yend = ye,
                   linewidth = lw,
                   colour = lbl_col,
                   linetype = ltype),
               arrow = arrow(length = unit(0.13,"inches"), type = "closed")) +
  
  # Path coefficient labels
  geom_label(data = path_df,
             aes(x = midx, y = midy, label = lbl,
                 colour = lbl_col),
             size       = 2.45,
             label.size = 0.15,
             fill       = "white",
             fontface   = "bold",
             alpha      = 0.93,
             family     = BASE_FONT) +
  
  # ── Latent variable ellipses ─────────────────────────────────
  geom_polygon(data = sem_ell,
               aes(x = x, y = y, group = id, fill = col),
               colour    = "white",
               linewidth = 1.0,
               alpha     = 0.90) +
  
  # Factor abbreviation labels (white, in ellipse)
  geom_text(data = node_df,
            aes(x = x, y = y + 0.06, label = id),
            size     = 3.8,
            fontface = "bold",
            colour   = "white",
            family   = BASE_FONT) +
  
  # Number of items annotation below exogenous factors
  geom_text(data = node_df %>% filter(type=="exo"),
            aes(x = x, y = y - 0.62,
                label = paste0("(", item_count[id], " items)")),
            size   = 2.0,
            colour = "#666666",
            family = BASE_FONT) +
  
  # Standardized loading range below factor
  geom_text(data = node_df %>% filter(type=="exo"),
            aes(x = x, y = y - 0.85,
                label = paste0("\u03BB: ", load_range[id])),
            size   = 1.9,
            colour = "#888888",
            family = BASE_FONT) +
  
  # R² in endogenous nodes
  geom_label(data = r2_df,
             aes(x = x, y = y - 0.60, label = lbl),
             size       = 2.55,
             fontface   = "bold",
             fill       = "#E8EAF6",
             colour     = "#1A237E",
             label.size = 0.3,
             family     = BASE_FONT) +
  
  scale_fill_identity(guide = "none") +
  scale_colour_identity() +
  scale_linewidth_identity() +
  scale_linetype_identity() +
  coord_fixed(ratio = 1, xlim = xlim_sem, ylim = ylim_sem) +
  
  # Legend for path significance
  annotate("segment", x=0.6, xend=1.4, y=-0.3, yend=-0.3,
           colour="#1B5E20", linewidth=1.1) +
  annotate("text", x=1.5, y=-0.3, hjust=0, size=2.4,
           label="Significant path (p < .05)", family=BASE_FONT) +
  annotate("segment", x=0.6, xend=1.4, y=-0.6, yend=-0.6,
           colour="#999999", linewidth=0.5, linetype="dashed") +
  annotate("text", x=1.5, y=-0.6, hjust=0, size=2.4,
           label="Non-significant path", family=BASE_FONT) +
  annotate("text", x=7.0, y=-0.42, hjust=0.5, size=2.4,
           label="*** p < .001   ** p < .01   * p < .05",
           fontface="italic", colour="#333333", family=BASE_FONT) +
  
  labs(
    title    = "Figure 2. Structural Equation Model: UTAUT2 FinTech Adoption",
    subtitle = "Standardized path coefficients (\u03B2) | MLR estimator, FIML | n = 430",
    caption  = paste0(
      "Model fit: \u03C7\u00B2(771) = 2067.07, p < .001 | ",
      "CFI = 0.899 | TLI = 0.887 | RMSEA = 0.063 [90% CI: 0.059, 0.066] | SRMR = 0.054\n",
      "Exogenous construct correlations not shown for clarity. ",
      "Dashed paths = non-significant. \u03B2 = standardized coefficient."
    )
  ) +
  theme_pub

ggsave(file.path(out_dir, "SEM_Publication_Quality.png"),
       p_sem, width = 14, height = 11, dpi = 400, bg = "white")
cat("Saved: SEM_Publication_Quality.png\n")

# ── Summary ───────────────────────────────────────────────────
cat("\n=== Publication-Quality Diagrams Complete ===\n")
cat("CFA: output/CFA_Publication_Quality.png  (22 x 9 in, 400 DPI)\n")
cat("SEM: output/SEM_Publication_Quality.png  (14 x 11 in, 400 DPI)\n")

