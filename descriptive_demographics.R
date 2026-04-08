# ============================================================
#  Participant Demographic Table — Full Descriptive Statistics
#  FinTech Adoption Among Thai Elderly (n=430)
#  UTAUT2 Framework | Fundamental Research Fund 2569
# ============================================================
options(scipen = 999)

library(haven)
library(dplyr)

out_dir   <- "D:/ARIS/Auto-claude-code-research-in-sleep/output"
DATA_PATH <- "E:/ทุนFundamental Research Fund-2569/Data/dataset.sav"

df_raw <- read_sav(DATA_PATH)
N <- nrow(df_raw)
cat(sprintf("Total sample: N = %d\n\n", N))

# ── Helper: frequency table builder ─────────────────────────
freq_tbl <- function(var_vec, labels, var_name, group = NULL) {
  tbl <- table(factor(var_vec, levels = names(labels)), useNA = "no")
  df  <- data.frame(
    Variable  = c(var_name, rep("", length(labels) - 1)),
    Category  = as.character(labels),
    Code      = names(labels),
    n         = as.integer(tbl),
    stringsAsFactors = FALSE
  )
  df$Percent <- round(df$n / N * 100, 1)
  df
}

# ── Age: continuous summary + grouped ───────────────────────
age     <- as.numeric(df_raw$Age)
age_n   <- sum(!is.na(age))
age_m   <- sum(is.na(age))
age_mn  <- mean(age, na.rm = TRUE)
age_sd  <- sd(age,   na.rm = TRUE)
age_min <- min(age,  na.rm = TRUE)
age_max <- max(age,  na.rm = TRUE)

# WHO geriatric grouping for elderly
age_grp <- cut(age,
  breaks = c(59, 64, 69, 74, 79, 84, 100),
  labels = c("60-64", "65-69", "70-74", "75-79", "80-84", "85+"),
  right  = TRUE
)
age_grp_tbl <- table(age_grp, useNA = "no")

age_df <- data.frame(
  Variable = c("Age (years)", rep("", length(age_grp_tbl) + 1)),
  Category = c(
    sprintf("  Mean (SD) = %.2f (%.2f), Range: %d-%d; Missing: n=%d",
            age_mn, age_sd, age_min, age_max, age_m),
    paste("  Age group:", names(age_grp_tbl)),
    ""
  ),
  Code    = c("", names(age_grp_tbl), ""),
  n       = c(age_n, as.integer(age_grp_tbl), NA_integer_),
  Percent = c(round(age_n/N*100,1),
              round(as.integer(age_grp_tbl)/N*100, 1),
              NA_real_),
  stringsAsFactors = FALSE
)

# ── Gender ───────────────────────────────────────────────────
# Values: 1=Male, 2=Female, 3=Other/Third Gender, 4=Prefer not to say
# (Thai surveys increasingly include LGBTQ+ categories; confirmed by distribution)
gender_lbl <- c(
  "1" = "Male (ชาย)",
  "2" = "Female (หญิง)",
  "3" = "Other / Third Gender (เพศทางเลือก)",
  "4" = "Prefer not to say (ไม่ระบุ)"
)
gender_df <- freq_tbl(df_raw$Gender, gender_lbl, "Gender")

# ── Education ────────────────────────────────────────────────
# 1=Primary, 2=Secondary, 3=Vocational, 4=Bachelor, 5=Postgraduate
edu_lbl <- c(
  "1" = "Primary school (ประถมศึกษา)",
  "2" = "Secondary school (มัธยมศึกษา)",
  "3" = "Vocational certificate (อาชีวศึกษา/ปวช./ปวส.)",
  "4" = "Bachelor's degree (ปริญญาตรี)",
  "5" = "Postgraduate (สูงกว่าปริญญาตรี)"
)
edu_df <- freq_tbl(df_raw$Education, edu_lbl, "Education Level")

# ── Occupation / Employment Status ───────────────────────────
# 0=Other, 1=Government, 2=Private sector, 3=Retired, 4=Self-employed
occ_lbl <- c(
  "0" = "Other (อื่นๆ)",
  "1" = "Government / State enterprise employee (ข้าราชการ/รัฐวิสาหกิจ)",
  "2" = "Private sector employee (พนักงานบริษัทเอกชน)",
  "3" = "Retired (เกษียณอายุ)",
  "4" = "Self-employed / Business owner (ประกอบอาชีพส่วนตัว/ธุรกิจ)"
)
occ_df <- freq_tbl(df_raw$Status, occ_lbl, "Occupation / Employment")

# ── Monthly Income (THB) ─────────────────────────────────────
# 1=<5k, 2=5k-10k, 3=10k-20k, 4=20k-30k, 5=>30k
inc_lbl <- c(
  "1" = "Less than 5,000 THB (< 5,000 บาท)",
  "2" = "5,001–10,000 THB",
  "3" = "10,001–20,000 THB",
  "4" = "20,001–30,000 THB",
  "5" = "More than 30,000 THB (> 30,000 บาท)"
)
inc_df <- freq_tbl(df_raw$Income, inc_lbl, "Monthly Income")

# ── FinTech Experience ───────────────────────────────────────
# 0=None, 1=<1yr, 2=1-3yr, 3=3-5yr, 4=>5yr
ft_lbl <- c(
  "0" = "No experience (ไม่มีประสบการณ์)",
  "1" = "Less than 1 year (< 1 ปี)",
  "2" = "1–3 years (1–3 ปี)",
  "3" = "3–5 years (3–5 ปี)",
  "4" = "More than 5 years (> 5 ปี)"
)
ft_df <- freq_tbl(df_raw$FinTech_Experience, ft_lbl, "FinTech Experience")

# ── Device Used ──────────────────────────────────────────────
# 1=Smartphone, 2=Tablet, 3=Computer, 4=Multiple
dev_lbl <- c(
  "1" = "Smartphone (สมาร์ทโฟน)",
  "2" = "Tablet (แท็บเล็ต)",
  "3" = "Computer / Laptop (คอมพิวเตอร์/โน้ตบุ๊ก)",
  "4" = "Multiple devices (หลายอุปกรณ์)"
)
dev_df <- freq_tbl(df_raw$Device, dev_lbl, "Primary Device")

# ── Internet Experience ───────────────────────────────────────
# 0=None, 1=<1yr, 2=1-3yr, 3=3-5yr, 4=>5yr
inet_lbl <- c(
  "0" = "No experience (ไม่มีประสบการณ์)",
  "1" = "Less than 1 year (< 1 ปี)",
  "2" = "1–3 years (1–3 ปี)",
  "3" = "3–5 years (3–5 ปี)",
  "4" = "More than 5 years (> 5 ปี)"
)
inet_df <- freq_tbl(df_raw$Internet_Experience, inet_lbl, "Internet Experience")

# ── Combine all ───────────────────────────────────────────────
spacer <- data.frame(Variable="", Category="", Code="",
                     n=NA_integer_, Percent=NA_real_,
                     stringsAsFactors=FALSE)

demo_table <- bind_rows(
  age_df,   spacer,
  gender_df, spacer,
  edu_df,   spacer,
  occ_df,   spacer,
  inc_df,   spacer,
  ft_df,    spacer,
  dev_df,   spacer,
  inet_df
)

# ── Print formatted table ─────────────────────────────────────
cat(strrep("=", 80), "\n")
cat(sprintf("TABLE: PARTICIPANT DEMOGRAPHIC CHARACTERISTICS (N = %d)\n", N))
cat(strrep("=", 80), "\n")
cat(sprintf("%-35s %-42s %6s %8s\n",
            "Variable", "Category", "n", "%"))
cat(strrep("-", 80), "\n")

for (i in seq_len(nrow(demo_table))) {
  row <- demo_table[i, ]
  if (is.na(row$n)) {
    cat(sprintf("%-35s %-42s\n", row$Variable, row$Category))
  } else {
    cat(sprintf("%-35s %-42s %6d %7.1f%%\n",
                row$Variable, row$Category, row$n, row$Percent))
  }
}
cat(strrep("=", 80), "\n")

# ── Descriptive stats for continuous constructs ───────────────
cat("\n\nCOMPOSITE CONSTRUCT DESCRIPTIVE STATISTICS\n")
cat(strrep("=", 80), "\n")

construct_vars <- list(
  "PE" = paste0("PE", 1:3),
  "EE" = paste0("EE", 1:3),
  "SI" = paste0("SI", 1:3),
  "FC" = paste0("FC", 1:4),
  "HM" = paste0("HM", 1:3),
  "HB" = paste0("HB", 1:3),
  "PT" = paste0("PT", 1:3),
  "TA" = paste0("TA", 1:4),
  "BI" = paste0("BI", 1:4),
  "UB" = paste0("UB", 1:7),
  "DL" = paste0("DL", 1:5)
)

construct_names <- c(
  PE = "Performance Expectancy",
  EE = "Effort Expectancy",
  SI = "Social Influence",
  FC = "Facilitating Conditions",
  HM = "Hedonic Motivation",
  HB = "Habit",
  PT = "Personal Trust",
  TA = "Technology Anxiety",
  BI = "Behavioral Intention",
  UB = "Use Behavior",
  DL = "Digital Literacy"
)

cat(sprintf("%-4s %-26s %6s %6s %6s %6s %8s %8s\n",
            "Code", "Construct", "n", "M", "SD", "Min", "Skew", "Kurt"))
cat(strrep("-", 75), "\n")

construct_df_list <- list()
for (code in names(construct_vars)) {
  items <- construct_vars[[code]]
  vals  <- rowMeans(df_raw[, items, drop=FALSE] %>%
                      mutate(across(everything(), as.numeric)),
                    na.rm = TRUE)
  n_c   <- sum(!is.na(vals))
  M     <- mean(vals, na.rm = TRUE)
  SD    <- sd(vals,   na.rm = TRUE)
  Mn    <- min(vals,  na.rm = TRUE)
  Mx    <- max(vals,  na.rm = TRUE)
  Sk    <- (sum((vals - M)^3, na.rm=TRUE) / n_c) / SD^3
  Kt    <- (sum((vals - M)^4, na.rm=TRUE) / n_c) / SD^4 - 3
  cat(sprintf("%-4s %-26s %6d %6.3f %6.3f %6.3f %8.3f %8.3f\n",
              code, construct_names[code], n_c, M, SD, Mn, Sk, Kt))
  construct_df_list[[code]] <- data.frame(
    Code=code, Construct=construct_names[code],
    n=n_c, M=round(M,3), SD=round(SD,3),
    Min=round(Mn,3), Max=round(Mx,3),
    Skewness=round(Sk,3), Kurtosis=round(Kt,3)
  )
}
cat(strrep("-", 75), "\n")
cat("Note: Scale 1-5 (Likert). Acceptable range: |Skew| < 2.0, |Kurt| < 7.0\n\n")

construct_stats_df <- bind_rows(construct_df_list)

# ── Save outputs ─────────────────────────────────────────────
cat("Saving files...\n")

write.csv(demo_table,
          file.path(out_dir, "12a_Demographic_Table.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("  Saved: 12a_Demographic_Table.csv\n")

write.csv(construct_stats_df,
          file.path(out_dir, "12b_Construct_Descriptives.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("  Saved: 12b_Construct_Descriptives.csv\n")

# ── Item-level descriptives ───────────────────────────────────
all_items <- unlist(construct_vars)
item_stats <- lapply(all_items, function(v) {
  x   <- as.numeric(df_raw[[v]])
  n_i <- sum(!is.na(x))
  M   <- mean(x, na.rm=TRUE)
  SD  <- sd(x,   na.rm=TRUE)
  Mn  <- min(x,  na.rm=TRUE)
  Mx  <- max(x,  na.rm=TRUE)
  Sk  <- ifelse(SD>0, (sum((x-M)^3,na.rm=TRUE)/n_i)/SD^3, NA)
  Kt  <- ifelse(SD>0, (sum((x-M)^4,na.rm=TRUE)/n_i)/SD^4 - 3, NA)
  data.frame(Item=v, n=n_i, M=round(M,3), SD=round(SD,3),
             Min=Mn, Max=Mx, Skewness=round(Sk,3), Kurtosis=round(Kt,3))
})
item_stats_df <- bind_rows(item_stats)

write.csv(item_stats_df,
          file.path(out_dir, "12c_Item_Descriptives.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("  Saved: 12c_Item_Descriptives.csv\n")

cat("\nDone.\n")
