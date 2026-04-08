library(haven); library(lavaan); library(dplyr)
df <- read_sav("E:/ทุนFundamental Research Fund-2569/Data/dataset.sav")
items <- c("PE1","PE2","PE3","EE1","EE2","EE3","SI1","SI2","SI3",
           "FC1","FC2","FC3","FC4","HM1","HM2","HM3","HB1","HB2","HB3",
           "PT1","PT2","PT3","TA1","TA2","TA3","TA4","BI1","BI2","BI3","BI4",
           "UB1","UB2","UB3","UB4","UB5","UB6","UB7","DL1","DL2","DL3","DL4","DL5")
df2 <- df %>% select(all_of(items)) %>% mutate(across(everything(), as.numeric))
p <- length(items)
cat("p (observed vars) =", p, "\n")
cat("Standard formula  p*(p+1)/2        =", p*(p+1)/2, "\n")
cat("FIML formula      p*(p+1)/2 + p    =", p*(p+1)/2 + p, "(= p*(p+3)/2 =", p*(p+3)/2, ")\n\n")

mm <- "PE=~PE1+PE2+PE3
EE=~EE1+EE2+EE3
SI=~SI1+SI2+SI3
FC=~FC1+FC2+FC3+FC4
HM=~HM1+HM2+HM3
HB=~HB1+HB2+HB3
PT=~PT1+PT2+PT3
TA=~TA1+TA2+TA3+TA4
BI=~BI1+BI2+BI3+BI4
UB=~UB1+UB2+UB3+UB4+UB5+UB6+UB7
DL=~DL1+DL2+DL3+DL4+DL5"

cfa_fit <- cfa(mm, data=df2, estimator="MLR", missing="fiml")
sem_fit <- sem(paste0(mm,"
  BI~PE+EE+SI+FC+HM+HB+PT+TA+DL
  UB~BI+FC+HB"), data=df2, estimator="MLR", missing="fiml")

npar_cfa <- lavInspect(cfa_fit, "npar")
npar_sem <- lavInspect(sem_fit, "npar")
df_cfa   <- fitMeasures(cfa_fit, "df.scaled")
df_sem   <- fitMeasures(sem_fit, "df.scaled")

cat("=== CFA ===\n")
cat("Free parameters (npar)          :", npar_cfa, "\n")
cat("FIML moments p*(p+3)/2          :", p*(p+3)/2, "\n")
cat("Calculated df = moments - npar  :", p*(p+3)/2 - npar_cfa, "\n")
cat("lavaan df.scaled                :", df_cfa, "\n")
cat("MATCH:", (p*(p+3)/2 - npar_cfa) == df_cfa, "\n\n")

cat("=== SEM (S1) ===\n")
cat("Free parameters (npar)          :", npar_sem, "\n")
cat("FIML moments p*(p+3)/2          :", p*(p+3)/2, "\n")
cat("Calculated df = moments - npar  :", p*(p+3)/2 - npar_sem, "\n")
cat("lavaan df.scaled                :", df_sem, "\n")
cat("MATCH:", (p*(p+3)/2 - npar_sem) == df_sem, "\n\n")

cat("=== df BREAKDOWN (SEM vs CFA) ===\n")
cat("Additional structural paths     :", npar_cfa - npar_sem, "(free params removed)\n")
cat("df increase (SEM - CFA)         :", df_sem - df_cfa, "\n")
cat("These", df_sem - df_cfa, "df come from constraining",
    npar_cfa - npar_sem, "free covariances to be structural paths\n")
