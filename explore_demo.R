library(haven)
df <- read_sav("E:/ทุนFundamental Research Fund-2569/Data/dataset.sav")

cat("=== ALL VARIABLE NAMES ===\n")
print(names(df))

cat("\n=== VARIABLE LABELS ===\n")
for (v in names(df)) {
  lbl <- attr(df[[v]], "label")
  if (!is.null(lbl)) cat(sprintf("  %-20s : %s\n", v, lbl))
}

cat("\n=== NROW ===\n")
cat(nrow(df), "\n")

cat("\n=== FIRST 5 NON-ITEM COLUMNS (head) ===\n")
item_pat <- "^(PE|EE|SI|FC|HM|HB|PT|TA|BI|UB|DL)[0-9]"
demo_cols <- names(df)[!grepl(item_pat, names(df))]
cat("Demo columns:", paste(demo_cols, collapse = ", "), "\n")

cat("\n=== UNIQUE VALUES for each demo column ===\n")
for (v in demo_cols) {
  cat(sprintf("\n-- %s --\n", v))
  lbl <- attr(df[[v]], "label")
  if (!is.null(lbl)) cat("  Label:", lbl, "\n")
  vals <- sort(unique(df[[v]]))
  cat("  Values:", paste(vals, collapse = ", "), "\n")
  # Value labels
  vlbls <- attr(df[[v]], "labels")
  if (!is.null(vlbls)) {
    cat("  Value labels:\n")
    for (i in seq_along(vlbls)) {
      cat(sprintf("    %s = %s\n", vlbls[i], names(vlbls)[i]))
    }
  }
  cat("  N non-missing:", sum(!is.na(df[[v]])), "\n")
}
