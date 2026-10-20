# Labor Market Institutions and Income Inequality: A Two-Way Fixed Effects Panel Analysis (2000–2019)

This personal research project was driven primarily by my personal interest in learning and applying econometrics to study the evolution of international political economy. I chose to study income inequality, because I wanted to examine how governance institutions and deliberate policy choices by government's contribute to the issue - all in the hopes of finding better solutions to managing distributional compromise.

This project was made with the help of Antigravity - a coding AI agent. Initially, I wrote the entire code myself, however I used AI explicitly to debug, verify the code, annotate the code (I kept forgetting to do it), and importantly, overcome a major issue concerning data availability across countries and test the models to ensure they are robust. 

##  Research Overview & Executive Summary

How do different labor market institutions: **collective bargaining coverage**, **trade union density**, and **works councils**, influence nationwide income inequality? 

Using a panel of **54 countries** over a 20-year timeline (**2000–2019**, $N = 878$ country-years), this study estimates two-way fixed effects (TWFE) and Mundlak correlated random effects models combining data from the **World Bank** (*Poverty and Inequality Platform*) and the **OECD/AIAS ICTWSS database**.

### Key Findings:
1. **Bargaining Coverage is the Strongest Within-Country Driver:** A 10 percentage point expansion in collective bargaining coverage significantly reduces the national Gini coefficient by **0.31 points** ($\beta = -0.0314^{***}, p < 0.001$).
2. **Private vs. Public Sector Unionism:** Private sector union density has a strong equalizing effect ($\beta = -0.2658^{**}, p < 0.05$), compressing market wage dispersion. Public sector union density has no marginal effect, as public pay scales are already compressed by civil-service wage grids.
3. **The Works Councils Institutional Paradox:** In TWFE, statutory works councils appear insignificant because 45 out of 53 countries experienced no legal reform over the 20-year period. However, between-country estimations (Pooled OLS and Mundlak decomposition) reveal that mandatory works councils are a massive structural equalizer, associated with a **4.6 to 4.8 point lower Gini index** ($p < 0.001$).

## Visual Highlights

* Visual highlights may be found in the outputs section of the Repository ***
* <img width="2250" height="1650" alt="figure1_works_councils_gini" src="https://github.com/user-attachments/assets/9d24e388-f4ed-4ca1-aa47-d374a86b6a1f" />
<img width="2400" height="1650" alt="figure4_within_country_dynamics" src="https://github.com/user-attachments/assets/23b68d94-6f85-4969-b1af-a54a0eff2e89" />
<img width="2850" height="1650" alt="figure3_sectoral_unions_gini" src="https://github.com/user-attachments/assets/2a12cdcb-5e01-4bae-b2e6-e4f5319a5fe8" />
<img width="2400" height="1650" alt="figure2_bargaining_coverage_gini" src="https://github.com/user-attachments/assets/1b74bdf3-e06c-4173-8ec6-28b56c666572" />


## Econometric Methodology

### 1. Two-Way Fixed Effects (TWFE)
To isolate within-country institutional variations over time while controlling for all unobserved country traits and global shocks:

Gini = alpha + gamma + b1Cov + b2UD_hist + b3WC + e

* alpha: Country Fixed Effects (controls for permanent cultural, geographic, and institutional factors).
* gamma: Year Fixed Effects (controls for common macroeconomic shocks, e.g., the 2008 Financial Crisis).
* Standard errors are clustered at the country level (Cluster = ISO) to account for arbitrary serial correlation and heteroskedasticity.

### 2. Resolving Time-Invariance: The Mundlak Decomposition
Because works councils laws rarely change (45/53 countries had zero within-country variation), country fixed effects absorb 100% of the cross-country variance. I deployed a **Mundlak (1978) Correlated Random Effects Model** decomposing variables into country means and annual deviations:

Gini = gamma + b(within_countries)((Xii-X_meani)  + b(between_countries)(mean_WC) + e

This confirms that works councils operate as a **long-run structural anchor** (b(between_countries) = -4.83^{***}$) rather than a volatile annual lever.

---

## Core Regression Results

### Table 1: Two-Way Fixed Effects Estimates
*Dependent Variable: Gini Index (0–100 scale). Country-clustered SEs in parentheses.*

| Independent Variable | Model 1 (Union Density) | Model 2 (Bargaining Cov) | Model 3 (Dual Model) | Model 5 (Sectoral Density) |
| :--- | :---: | :---: | :---: | :---: |
| **Adjusted Bargaining Coverage (`AdjCov_hist`)** | — | **-0.0314\*\*\*** (0.0084) | -0.0041 (0.0163) | — |
| **Overall Union Density (`UD_hist`)** | -0.0197 (0.0516) | — | -0.0247 (0.0654) | — |
| **Private Sector Union Density (`UD_priv`)** | — | — | — | **-0.2658\*\*** (0.1096) |
| **Public Sector Union Density (`UD_pub`)** | — | — | — | 0.0030 (0.0164) |
| **Country & Year Fixed Effects** | Yes | Yes | Yes | Yes |
| **Observations ($N$)** | 646 | 477 | 399 | 269 |
| **Within $R^2$** | 0.0011 | **0.0317** | 0.0032 | **0.1148** |

*\* $p < 0.10$, \*\* $p < 0.05$, \*\*\* $p < 0.01$*

---

## 🛠️ Reproduction & Code Usage

### Prerequisites
Make sure R (>= 4.2) and the following packages are installed:
```r
install.packages(c("tidyverse", "readxl", "fixest", "zoo"))
