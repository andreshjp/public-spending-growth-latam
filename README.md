# Public Spending and Economic Growth in Latin America

> Undergraduate thesis examining the relationship between public expenditure by functional category and economic growth across Latin American countries, using panel data econometrics. Combines data from CEPAL, IMF, and Eurostat into a unified multi-source panel spanning several decades. Graduated *Summa Cum Laude*.

---

## Research Question

**Does the functional composition of public spending — how governments allocate expenditure across education, health, social protection, defense, economic affairs, and other categories — explain cross-country differences in economic growth in Latin America?**

This question connects public finance theory to empirical fiscal economics: the prediction that productive spending (education, health, infrastructure) should generate positive growth externalities, while the aggregate level alone provides insufficient information about fiscal effectiveness.

---

## Data Sources

| Source | Coverage | Variables |
|--------|----------|-----------|
| **CEPAL** (ECLAC) | Latin American countries | Public expenditure by COFOG function |
| **IMF Government Finance Statistics** | Global (filtered to LatAm) | Public expenditure by function (10 categories) in domestic currency |
| **Eurostat** | Reference benchmarks | Public expenditure by function (European comparators) |
| **IMF World Economic Outlook** | Panel | GDP, GDP growth rate |
| **Own construction** | Panel | Gross Fixed Capital Formation, Consumption, Imports, Exports |

**Spending categories (COFOG classification):**
Education · Health · Social Protection · Defense · Economic Affairs · General Public Services · Public Order & Safety · Housing & Community Amenities · Environmental Protection · Recreation, Culture & Religion

---

## Methodology

### Data Engineering

Constructing the panel required significant multi-source harmonization:

- **IMF consolidation:** For countries where General Government data was unavailable, the spending of sub-central government levels (Budgetary Central, Extrabudgetary Central, Social Security Funds, State Governments) were summed to approximate the General Government aggregate.
- **Country name standardization:** CEPAL uses Spanish names, IMF uses English names; country codes (`countrycode`) were used to bridge across sources.
- **Panel completion:** Missing years for certain country-function combinations were filled via `tidyr::complete` before pivoting to wide format.
- **COFOG harmonization:** Spending functions were mapped to a common Spanish-language taxonomy across all three data sources.
- **NA treatment:** Zeroes in financial data were recoded to NA to distinguish between true-zero expenditure and missing observations.

### Econometric Framework

**Panel data models** estimated using the `plm` package:

| Model | Specification | Rationale |
|-------|-------------|-----------|
| Pooled OLS | Ignores individual effects | Baseline |
| Fixed Effects (Within) | Controls for time-invariant country characteristics | Preferred when heterogeneity is correlated with regressors |
| Random Effects (GLS) | Assumes heterogeneity uncorrelated with regressors | Compared via Hausman test |

**Robustness and diagnostics:**
- Hausman test to choose between FE and RE
- Heteroskedasticity and autocorrelation-consistent (HAC) standard errors via `sandwich`
- Multicollinearity diagnostics (`mctest`)
- Structural break detection for non-linear relationships (`segmented`)
- Multiple imputation for missing panel observations (`mice`)
- Variance Inflation Factor (VIF) analysis (`car`)

**Controls:** GDP lagged, Gross Fixed Capital Formation (% GDP), trade openness, government consumption as a share of GDP.

---

## Key Findings

The thesis finds differential effects across spending categories:

- **Productive spending** (education, health, economic affairs) shows positive associations with growth, with statistical significance varying by model specification and time horizon.
- **Redistributive spending** (social protection) shows mixed results consistent with the theoretical ambiguity — positive through human capital channels, neutral or negative through labor market distortion channels at high levels.
- **Defense spending** shows no significant relationship with growth in the LatAm sample.
- The **composition** of spending matters more than the aggregate level, supporting the fiscal quality argument over fiscal size arguments.
- Results are robust to alternative panel estimators and HAC standard errors, but sensitivity to the balanced vs. unbalanced panel specification is noted.

*For full results and statistical tables, refer to the analysis script.*

---

## Tech Stack

| Package | Role |
|---------|------|
| `plm` | Panel data econometrics (FE, RE, pooled OLS) |
| `lmtest` | Hausman test, hypothesis testing |
| `sandwich` | HAC robust standard errors |
| `car` | VIF, multicollinearity |
| `mctest` | Multicollinearity diagnostics |
| `mice` | Multiple imputation |
| `segmented` | Structural break / segmented regression |
| `broom` | Tidy model output |
| `readxl` | Excel data ingestion (multi-sheet) |
| `tidyverse` | Data wrangling and pivoting |
| `countrycode` | Country name harmonization |

---

## Project Structure

```
public-spending-growth-latam/
├── analysis.R                      # Full data pipeline + econometric models
├── data/
│   ├── Datos CEPAL.xlsx            # CEPAL public expenditure data
│   ├── Datos FMI.xlsx              # IMF Government Finance Statistics
│   ├── Datos Eurostat.xlsx         # Eurostat expenditure data
│   ├── Datos PIB.csv               # GDP panel data
│   ├── Datos FBKF.csv              # Gross Fixed Capital Formation
│   ├── Datos Consumo.csv           # Consumption data
│   ├── Datos Importaciones.csv     # Imports
│   └── Datos Exportaciones.csv     # Exports
└── Datos Instituciones.xlsx        # Institutional quality controls
```

---

## How to Run

```r
install.packages(c("readr", "readxl", "dplyr", "tidyverse", "tibble",
                   "purrr", "countrycode", "lmtest", "plm", "car",
                   "broom", "sandwich", "mctest", "mice", "caret", "segmented"))

source("analysis.R")
```

---

## Academic Context

Undergraduate thesis submitted for the Bachelor's degree in Business Economics (minor in Data Management) at **Universidad Metropolitana (UNIMET)**, Caracas, Venezuela. Graduated **Summa Cum Laude**.

---

## Skills Demonstrated

`Panel Data Econometrics` · `Fixed Effects` · `Random Effects` · `Hausman Test` · `HAC Standard Errors` · `Multiple Imputation` · `Multi-Source Data Harmonization` · `Fiscal Economics` · `Public Finance` · `R` · `plm` · `tidyverse`
