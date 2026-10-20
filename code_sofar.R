# ==============================================================================
# Master Academic Panel Regression Pipeline: Labor Institutions & Income Inequality
# Author: Cambridge Distribution Research
# Research Question: How do Trade Union Density, Collective Bargaining Coverage,
#                    and Works Councils affect the Gini Coefficient?
# Data Sources: 
#   1. World Bank (World Development Indicators - Gini Index, SI.POV.GINI)
#   2. OECD and AIAS (ICTWSS Database - Core Indicators)
# Sample: 54 Countries, Timeline: 2000 - 2019
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Environment Setup & Dependencies
# ------------------------------------------------------------------------------
# Ensure the required packages are loaded
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(fixest)
  library(zoo)
})

# Set working directory to project directory
setwd("C:/Users/User/Desktop/Cambs/R shi/distribution")

cat("\n======================================================================\n")
cat("Starting Academic Panel Analysis: Labor Institutions and Inequality\n")
cat("======================================================================\n\n")

# ------------------------------------------------------------------------------
# 1. Load and Clean OECD / AIAS ICTWSS Labor Institutions Data
# ------------------------------------------------------------------------------
cat(">> Step 1: Loading and processing OECD/AIAS ICTWSS database (tu.xlsx)...\n")

# Sheet 5 contains the standardized 'Core indicators'
tu_raw <- read_xlsx("tu.xlsx", sheet = 5, skip = 5)

# Convert ICTWSS missing value notation ("..") to NA
tu_clean <- tu_raw %>% 
  mutate(across(where(is.character), ~ na_if(.x, "..")))

# Select key institutional variables and convert types
tudf <- tu_clean %>%
  select(
    Country,
    ISO,
    Year,
    UD_hist,           # Overall trade union density (%)
    UD_private_hist,   # Private sector trade union density (%)
    UD_public_hist,    # Public sector trade union density (%)
    AdjCov_hist,       # Adjusted collective bargaining coverage rate (%)
    WC,                # Status of works councils (0=none, 1=voluntary, 2=mandated)
    WC_negot,          # Works council role in wage negotiations (0/1/2)
    Coord,             # Coordination of wage setting (1-5)
    Level              # Predominant level of wage bargaining (1=enterprise...5=central)
  ) %>%
  mutate(
    Year            = as.numeric(Year),
    UD_hist         = as.numeric(UD_hist),
    UD_private_hist = as.numeric(UD_private_hist),
    UD_public_hist  = as.numeric(UD_public_hist),
    AdjCov_hist     = as.numeric(AdjCov_hist),
    WC              = as.numeric(WC),
    WC_negot        = as.numeric(WC_negot),
    Coord           = as.numeric(Coord),
    Level           = as.numeric(Level),
    # Harmonize Kosovo's code with World Bank standard (XKX)
    ISO             = ifelse(ISO == "XKV", "XKX", ISO)
  ) %>%
  filter(!is.na(ISO), !is.na(Year))

cat("   [OK] Loaded ICTWSS data: ", nrow(tudf), " country-year rows across ", 
    n_distinct(tudf$ISO), " countries.\n\n")

# ------------------------------------------------------------------------------
# 2. Load and Clean World Bank Gini Coefficient Data
# ------------------------------------------------------------------------------
cat(">> Step 2: Loading and pivoting World Bank Gini index (gini.csv)...\n")

# Skip the first 4 metadata rows in standard World Bank CSV downloads
gini_raw <- read.csv("gini.csv", skip = 4, check.names = FALSE)

# World Bank exports often have trailing empty column names; drop them
gini_clean_cols <- gini_raw[, names(gini_raw) != ""]

# Pivot from wide (year-columns) to long panel format (country-year)
gini_long <- gini_clean_cols %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "gini"
  ) %>%
  mutate(
    year       = as.numeric(year),
    gini       = as.numeric(gini),
    ISO        = `Country Code`,
    Country_WB = `Country Name`
  ) %>%
  filter(!is.na(ISO), !is.na(year), !is.na(gini)) %>%
  select(ISO, Country_WB, year, gini)

cat("   [OK] Loaded World Bank Gini: ", nrow(gini_long), " observed country-year points.\n\n")

# ------------------------------------------------------------------------------
# 3. Merge Datasets into an Unbalanced Panel (2000 - 2019)
# ------------------------------------------------------------------------------
cat(">> Step 3: Merging datasets into a panel for 2000 - 2019...\n")

# METHODOLOGICAL NOTE ON UNBALANCED PANELS:
# In previous attempts, applying filter(!any(is.na(value))) or filter(!any(is.na(AdjCov_hist)))
# enforced a strictly balanced panel, dropping any country that missed even a single year.
# Because World Bank household surveys and collective bargaining censuses are conducted 
# every 2-3 years in most economies, requiring full balance dropped >90% of countries.
# In modern econometrics (Wooldridge, Stock & Watson), UNBALANCED PANELS ARE FULLY VALID.
# Two-Way Fixed Effects estimators (fixest::feols) handle unbalanced panels without bias.

master_panel <- inner_join(
  tudf %>% filter(Year >= 2000, Year <= 2019),
  gini_long,
  by = c("ISO", "Year" = "year")
) %>%
  arrange(ISO, Year)

cat("   [OK] Merged Panel Successfully Created:\n")
cat("        - Total observations: ", nrow(master_panel), " country-years\n")
cat("        - Total countries:    ", n_distinct(master_panel$ISO), "\n")
cat("        - Time span:          2000 - 2019\n\n")

# ------------------------------------------------------------------------------
# 4. Construct Robustness Panels: Interpolation & Dynamic Lags
# ------------------------------------------------------------------------------
cat(">> Step 4: Building robustness datasets (linear interpolation & lagged panel)...\n")

# Robustness A: Linear interpolation for small biennial gaps (maxgap = 2 years)
# Countries like Australia, UK, and France conduct collective bargaining surveys
# every 2 years. Linear interpolation bridges 1-2 year gaps without fabricating voids.
panel_interp <- tudf %>%
  filter(Year >= 1998, Year <= 2020) %>%
  group_by(ISO) %>%
  arrange(Year) %>%
  mutate(
    AdjCov_interp = na.approx(AdjCov_hist, Year, maxgap = 2, na.rm = FALSE),
    UD_interp     = na.approx(UD_hist, Year, maxgap = 2, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  filter(Year >= 2000, Year <= 2019) %>%
  inner_join(gini_long, by = c("ISO", "Year" = "year")) %>%
  arrange(ISO, Year)

# Robustness B: Panel object for dynamic time lags
panel_pdata <- panel(master_panel, ~ISO + Year)

# ------------------------------------------------------------------------------
# 5. Core Academic Regressions: Two-Way Fixed Effects (TWFE)
# ------------------------------------------------------------------------------
cat(">> Step 5: Estimating Core Two-Way Fixed Effects (TWFE) models...\n")
# Model equation: Gini_{i,t} = alpha_i + gamma_t + beta * X_{i,t} + epsilon_{i,t}
# - alpha_i: Country Fixed Effects (controls for all unobserved time-invariant traits)
# - gamma_t: Year Fixed Effects (controls for common global macro shocks)
# - Standard Errors: Clustered at the country level (cluster = ~ISO)

# Model 1: Trade Union Density only
m1_ud <- feols(gini ~ UD_hist | ISO + Year, 
               data = master_panel, cluster = ~ISO)

# Model 2: Adjusted Collective Bargaining Coverage only
m2_cov <- feols(gini ~ AdjCov_hist | ISO + Year, 
                data = master_panel, cluster = ~ISO)

# Model 3: Dual Model (Union Density + Bargaining Coverage)
m3_dual <- feols(gini ~ UD_hist + AdjCov_hist | ISO + Year, 
                 data = master_panel, cluster = ~ISO)

# Model 4: Full Institutional Model (UD + Bargaining Coverage + Works Councils)
m4_full <- feols(gini ~ UD_hist + AdjCov_hist + WC | ISO + Year, 
                 data = master_panel, cluster = ~ISO)

# Model 5: Disaggregated Union Density (Private Sector vs Public Sector)
m5_sectors <- feols(gini ~ UD_private_hist + UD_public_hist | ISO + Year, 
                    data = master_panel, cluster = ~ISO)

cat("\n======================================================================\n")
cat("TABLE 1: TWO-WAY FIXED EFFECTS (TWFE) ESTIMATES OF GINI ON LABOR INSTITUTIONS\n")
cat("======================================================================\n")
etable(
  m1_ud, m2_cov, m3_dual, m4_full, m5_sectors,
  headers = c("Union Density", "Bargaining Cov", "Dual Model", "Full Inst", "Sectoral UD"),
  se.below = TRUE,
  digits = 4,
  fitstat = ~ n + r2 + wr2
)

# ------------------------------------------------------------------------------
# 6. Academic Nuance: Resolving the Works Council (WC) Identification
# ------------------------------------------------------------------------------
cat("\n>> Step 6: Estimating Between-Country and Mundlak Models for Works Councils...\n")
# ECONOMETRIC INSIGHT ON WORKS COUNCILS:
# Works Council status (WC: 0=None, 1=Voluntary, 2=Mandated) is strictly constant 
# in 45 out of 53 countries over 2000-2019. 
# Country fixed effects absorb all cross-sectional differences, leaving WC identified 
# solely from the 8 switchers (Albania, Estonia, UK, Indonesia, Ireland, Lithuania, Poland, Romania).
# To assess the cross-national institutional effect, we estimate:
# A) Pooled OLS with Year Fixed Effects and Clustered Standard Errors
# B) Mundlak Correlated Random Effects Decomposition (Separates Within and Between effects)

# Model A: Pooled OLS with Year FE (Cross-country institutional comparison)
m_pooled <- feols(gini ~ WC + UD_hist + AdjCov_hist | Year, 
                  data = master_panel, cluster = ~ISO)

# Model B: Mundlak Decomposition
panel_mundlak <- master_panel %>%
  group_by(ISO) %>%
  mutate(
    mean_ud   = mean(UD_hist, na.rm = TRUE),
    mean_cov  = mean(AdjCov_hist, na.rm = TRUE),
    mean_wc   = mean(WC, na.rm = TRUE),
    dev_ud    = UD_hist - mean_ud,
    dev_cov   = AdjCov_hist - mean_cov,
    dev_wc    = WC - mean_wc
  ) %>%
  ungroup()

m_mundlak <- feols(gini ~ dev_cov + dev_ud + dev_wc + mean_cov + mean_ud + mean_wc | Year,
                   data = panel_mundlak, cluster = ~ISO)

cat("\n======================================================================\n")
cat("TABLE 2: INSTITUTIONAL BETWEEN-COUNTRY & MUNDLAK DECOMPOSITION\n")
cat("======================================================================\n")
etable(
  m_pooled, m_mundlak,
  headers = c("Pooled OLS (Between)", "Mundlak (Within/Between)"),
  se.below = TRUE,
  digits = 4,
  fitstat = ~ n + r2
)

# ------------------------------------------------------------------------------
# 7. Robustness Checks: Dynamic Lags (t-1) & Linear Interpolation
# ------------------------------------------------------------------------------
cat("\n>> Step 7: Estimating Dynamic Lags (t-1) and Interpolation Robustness...\n")
# Dynamic 1-year lags: labor market contracts and institutions operate with a delay
m_lag_cov  <- feols(gini ~ l(AdjCov_hist, 1) | ISO + Year, data = panel_pdata, cluster = ~ISO)
m_lag_dual <- feols(gini ~ l(UD_hist, 1) + l(AdjCov_hist, 1) | ISO + Year, data = panel_pdata, cluster = ~ISO)

# Interpolated models: addresses potential biennial survey timing artifacts
m_int_cov  <- feols(gini ~ AdjCov_interp | ISO + Year, data = panel_interp, cluster = ~ISO)
m_int_dual <- feols(gini ~ UD_interp + AdjCov_interp | ISO + Year, data = panel_interp, cluster = ~ISO)

cat("\n======================================================================\n")
cat("TABLE 3: ROBUSTNESS CHECKS - DYNAMIC LAGS & INTERPOLATION\n")
cat("======================================================================\n")
etable(
  m_lag_cov, m_lag_dual, m_int_cov, m_int_dual,
  headers = c("Lagged Cov (t-1)", "Lagged Dual (t-1)", "Interp Cov", "Interp Dual"),
  se.below = TRUE,
  digits = 4,
  fitstat = ~ n + r2 + wr2
)

# ------------------------------------------------------------------------------
# 8. Export Publication Tables
# ------------------------------------------------------------------------------
cat("\n>> Step 8: Exporting formatted tables to CSV and LaTeX files...\n")

# CSV exports (Clean spreadsheets for Excel / Word)
write.csv(
  as.data.frame(etable(m1_ud, m2_cov, m3_dual, m4_full, m5_sectors,
                       headers = c("Union Density", "Bargaining Cov", "Dual Model", "Full Inst", "Sectoral UD"),
                       se.below = TRUE, digits = 4, fitstat = ~ n + r2 + wr2, tex = FALSE)),
  "table1_twfe.csv", row.names = FALSE
)

write.csv(
  as.data.frame(etable(m_pooled, m_mundlak,
                       headers = c("Pooled OLS (Between)", "Mundlak (Within/Between)"),
                       se.below = TRUE, digits = 4, fitstat = ~ n + r2, tex = FALSE)),
  "table2_institutional.csv", row.names = FALSE
)

write.csv(
  as.data.frame(etable(m_lag_cov, m_lag_dual, m_int_cov, m_int_dual,
                       headers = c("Lagged Cov (t-1)", "Lagged Dual (t-1)", "Interp Cov", "Interp Dual"),
                       se.below = TRUE, digits = 4, fitstat = ~ n + r2 + wr2, tex = FALSE)),
  "table3_robustness.csv", row.names = FALSE
)

# LaTeX exports for Overleaf / Academic papers
etable(m1_ud, m2_cov, m3_dual, m4_full, m5_sectors,
       headers = c("Union Density", "Bargaining Cov", "Dual Model", "Full Inst", "Sectoral UD"),
       se.below = TRUE, digits = 4, fitstat = ~ n + r2 + wr2,
       file = "table1_twfe.tex")

etable(m_pooled, m_mundlak,
       headers = c("Pooled OLS (Between)", "Mundlak (Within/Between)"),
       se.below = TRUE, digits = 4, fitstat = ~ n + r2,
       file = "table2_institutional.tex")

cat("   [OK] Tables saved to:\n")
cat("        - table1_twfe.csv / .tex\n")
cat("        - table2_institutional.csv / .tex\n")
cat("        - table3_robustness.csv\n\n")

# ------------------------------------------------------------------------------
# 9. Publication-Quality Coefficient Plot
# ------------------------------------------------------------------------------
cat(">> Step 9: Generating academic coefficient comparison plot...\n")

extract_coefs <- function(model, model_name) {
  ct <- coeftable(model)
  data.frame(
    term = rownames(ct),
    estimate = ct[, 1],
    std_error = ct[, 2],
    p_value = ct[, 4],
    model = model_name,
    stringsAsFactors = FALSE
  )
}

coef_df <- bind_rows(
  extract_coefs(m1_ud, "Model 1: Union Density"),
  extract_coefs(m2_cov, "Model 2: Bargaining Coverage"),
  extract_coefs(m3_dual, "Model 3: Dual Model"),
  extract_coefs(m5_sectors, "Model 5: Sectoral Density")
)

term_labels <- c(
  "UD_hist"         = "Overall Union Density",
  "AdjCov_hist"     = "Bargaining Coverage",
  "UD_private_hist" = "Private Sector Union Density",
  "UD_public_hist"  = "Public Sector Union Density"
)

coef_df$term_clean <- term_labels[coef_df$term]
coef_df <- coef_df %>% filter(!is.na(term_clean))

p_coef <- ggplot(coef_df, aes(x = estimate, y = term_clean, 
                              xmin = estimate - 1.96 * std_error, 
                              xmax = estimate + 1.96 * std_error)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_pointrange(color = "#1f77b4", size = 0.7) +
  geom_text(aes(label = sprintf("%.4f%s", estimate, 
                                ifelse(p_value < 0.01, "***", 
                                       ifelse(p_value < 0.05, "**", 
                                              ifelse(p_value < 0.1, "*", ""))))),
            vjust = -1, size = 3.5, fontface = "bold") +
  facet_wrap(~model, scales = "free_y", ncol = 1) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#f0f2f5"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 13)
  ) +
  labs(
    title = "Within-Country Effects of Labor Institutions on Income Inequality (Gini)",
    subtitle = "Two-Way Fixed Effects Estimates (Country & Year Fixed Effects) with 95% Confidence Intervals",
    x = "Marginal Effect on Gini Index (percentage points)",
    y = "",
    caption = "Data: World Bank (Gini) & OECD/AIAS ICTWSS (2000-2019). Clustered SEs by Country. * p<0.1, ** p<0.05, *** p<0.01"
  )

ggsave("regression_coefficients_plot.png", plot = p_coef, width = 8.5, height = 7, dpi = 300)
cat("   [OK] Plot saved to regression_coefficients_plot.png\n\n")

cat("======================================================================\n")
cat("PIPELINE EXECUTION COMPLETE! READY FOR ACADEMIC WRITE-UP.\n")
cat("======================================================================\n")
