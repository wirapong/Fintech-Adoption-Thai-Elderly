library(haven)
df <- read_sav("E:/ทุนFundamental Research Fund-2569/Data/dataset.sav")

# Check SPSS metadata more carefully
cat("=== CHECKING VALUE LABELS (as_factor approach) ===\n")
demo_vars <- c("Gender", "Education", "Status", "Income",
               "FinTech_Experience", "Device", "Internet_Experience")

for (v in demo_vars) {
  cat(sprintf("\n=== %s ===\n", v))
  # Raw values
  tbl_raw <- table(df[[v]], useNA = "always")
  cat("Raw values:\n")
  print(tbl_raw)
  # Try as_factor
  tryCatch({
    fct <- as_factor(df[[v]])
    tbl_fct <- table(fct, useNA = "always")
    cat("As factor:\n")
    print(tbl_fct)
  }, error = function(e) cat("  (no factor labels)\n"))
}

# Age summary
cat("\n=== Age ===\n")
cat(sprintf("  N non-missing : %d\n", sum(!is.na(df$Age))))
cat(sprintf("  N missing     : %d\n", sum(is.na(df$Age))))
cat(sprintf("  Min           : %.0f\n", min(df$Age, na.rm=TRUE)))
cat(sprintf("  Max           : %.0f\n", max(df$Age, na.rm=TRUE)))
cat(sprintf("  Mean          : %.2f\n", mean(df$Age, na.rm=TRUE)))
cat(sprintf("  SD            : %.2f\n", sd(df$Age, na.rm=TRUE)))

# Age group distribution (60-74 = young-old, 75-84 = old-old, 85+ = oldest-old)
df$Age_group <- cut(df$Age,
  breaks = c(54, 59, 64, 69, 74, 79, 84, 100),
  labels = c("54-59","60-64","65-69","70-74","75-79","80-84","85+"),
  right  = TRUE
)
cat("\nAge group distribution:\n")
print(table(df$Age_group, useNA = "always"))

# Also standard geriatric grouping
df$Age_g3 <- cut(df$Age,
  breaks = c(0, 59, 74, 100),
  labels = c("<60","60-74 (Young-Old)","75+ (Old-Old)")
)
cat("\nBroad age grouping:\n")
print(table(df$Age_g3, useNA = "always"))
