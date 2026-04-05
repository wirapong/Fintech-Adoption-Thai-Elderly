# FinTech Adoption Among Thai Elderly: UTAUT2 Study

> **"การศึกษาการยอมรับและส่งเสริมการใช้เทคโนโลยีทางการเงินในกลุ่มผู้สูงอายุไทย"**  
> A quantitative study of FinTech adoption determinants among Thai older adults using UTAUT2 extended with Technology Anxiety and Digital Literacy constructs.

---

## Overview

This repository contains the analysis outputs, figures, and supplementary materials for a research project investigating the factors influencing FinTech adoption among Thai elderly populations. The study extends the UTAUT2 framework by incorporating **Technology Anxiety (TA)** and **Digital Literacy (DL)** as additional constructs, validated through both CB-SEM and PLS-SEM approaches.

| Item | Detail |
|------|--------|
| Sample size | n = 430 |
| Estimator | MLR (CB-SEM), Bootstrap n=5,000 (PLS-SEM) |
| Missing data | FIML |
| Software | R (lavaan), SmartPLS |
| Framework | UTAUT2 (Venkatesh et al., 2012) extended |

---

## Research Constructs

| Code | Construct | Items | AVE | ω |
|------|-----------|-------|-----|---|
| PE | Performance Expectancy | 3 | ~0.79 | ~0.92 |
| EE | Effort Expectancy | 3 | ~0.72 | ~0.88 |
| SI | Social Influence | 3 | ~0.57 | ~0.80 |
| FC | Facilitating Conditions | 4 | ~0.51 | ~0.80 |
| HM | Hedonic Motivation | 3 | ~0.75 | ~0.90 |
| HB | Habit | 3 | ~0.70 | ~0.87 |
| PT | Performance Trust | 3 | ~0.76 | ~0.90 |
| TA | Technology Anxiety *(extended)* | 4 | ~0.59 | ~0.85 |
| BI | Behavioral Intention | 4 | ~0.63 | ~0.87 |
| UB | Use Behavior | 7 | ~0.78 | ~0.96 |
| DL | Digital Literacy *(extended)* | 5 | ~0.61 | ~0.88 |

> All AVE > 0.50 and ω > 0.70 confirming convergent validity (Fornell & Larcker, 1981; Hair et al., 2014).

---

## Key Findings

### CB-SEM Model Fit (Figure 1 — CFA; Figure 2 — SEM)

| Index | CFA | SEM | Criterion | Assessment |
|-------|-----|-----|-----------|------------|
| χ²/df | 2.57 | 2.68 | ≤ 3.0 | Acceptable |
| CFI | 0.907 | 0.899 | ≥ 0.90 | Acceptable / Marginal |
| TLI | 0.895 | 0.887 | ≥ 0.90 | Marginal |
| RMSEA | 0.060 | 0.063 | ≤ 0.08 | Acceptable |
| SRMR | 0.043 | 0.054 | ≤ 0.08 | Good / Acceptable |

### Structural Paths — CB-SEM (→ Behavioral Intention)

| Path | β | p | Direction |
|------|---|---|-----------|
| TA → BI | −0.237 | < .001 | Negative |
| PT → BI | +0.227 | < .05  | Positive  |
| DL → BI | +0.164 | < .05  | Positive  |
| BI → UB | +0.301 | < .05  | Positive  |

- **R² (BI) = 0.734** — 73.4% variance explained  
- **R² (UB) = 0.510** — 51.0% variance explained

### PLS-SEM Supplementary Results (Bootstrap n=5,000)

| Path | β (PLS) | β (CB) | p |
|------|---------|--------|---|
| HB → BI | +0.518 | n.s. | < .01 |
| TA → BI | −0.249 | −0.237 | < .05 |
| HM → BI | +0.257 | n.s. | < .10 |
| PT → BI | +0.139 | +0.227 | < .05 |
| DL → UB | +0.518 | — | < .01 |
| BI → UB | +0.242 | +0.301 | < .05 |

> PLS-SEM revealed **direct effect of DL → UB** (β = +0.518) not present in CB-SEM, indicating Digital Literacy independently drives actual use behavior.

---

## Repository Structure

```
├── figures/
│   ├── Figure1_CFA_Publication_Quality.png   # CFA measurement model
│   └── Figure2_SEM_Publication_Quality.png   # SEM structural model
├── supplementary/
│   └── PLS_PathDiagram.png                   # PLS-SEM path diagram
├── tables/
│   ├── Table6_CFA_ModelFit.md                # CFA fit indices
│   ├── Table10_SEM_ModelFit.md               # SEM fit indices
│   └── Table14_CorrelationMatrix.md          # Pearson correlation matrix
├── README.md
└── LICENSE
```

---

## Theoretical Contribution

This study makes three primary contributions:

1. **Framework extension** — Augments UTAUT2 with TA and DL constructs validated for Thai elderly context, addressing gaps in prior literature.
2. **Methodological triangulation** — Convergent findings across CB-SEM and PLS-SEM strengthen confidence in identified paths.
3. **Population specificity** — Documents divergence from general-population UTAUT2 results: hedonic and social factors carry less weight, while anxiety reduction and digital skill-building are dominant levers.

---

## Policy Implications

- **Reduce TA first** — Technology Anxiety is the strongest barrier (β = −0.237 to −0.249 across both methods). UX simplification and anxiety-reduction programmes should precede feature expansion.
- **Build Digital Literacy** — DL directly predicts both intention (CB-SEM) and actual use (PLS-SEM), supporting hands-on community training over classroom-only instruction.
- **Leverage trust messaging** — Performance Trust is consistently significant; FinTech providers should prioritise transparent, elderly-friendly trust communication.

---

## Citation

If you use these materials, please cite:

```
[Author(s)]. (2025). การศึกษาการยอมรับและส่งเสริมการใช้เทคโนโลยีทางการเงิน
ในกลุ่มผู้สูงอายุไทย [FinTech Adoption Among Thai Elderly: A UTAUT2 Study].
[Institution]. n = 430, MLR/FIML estimator.
```

---

## References

- Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models with unobservable variables and measurement error. *Journal of Marketing Research, 18*(1), 39–50.
- Hair, J. F., Ringle, C. M., & Sarstedt, M. (2022). *A primer on partial least squares structural equation modeling (PLS-SEM)* (3rd ed.). Sage.
- Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for assessing discriminant validity in variance-based structural equation modeling. *Journal of the Academy of Marketing Science, 43*(1), 115–135.
- Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis. *Structural Equation Modeling, 6*(1), 1–55.
- Kline, R. B. (2016). *Principles and practice of structural equation modeling* (4th ed.). Guilford.
- Venkatesh, V., Thong, J. Y. L., & Xu, X. (2012). Consumer acceptance and use of information technology: Extending the unified theory of acceptance and use of technology. *MIS Quarterly, 36*(1), 157–178.

---

*Standardized path coefficients (β) | MLR estimator, FIML | n = 430*  
*\*\*\* p < .001 · \*\* p < .01 · \* p < .05 · † p < .10*
