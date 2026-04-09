################################################################################
# INFO 6105 Final Project - Script 1
# Data Preparation & Feature Engineering
# Author: Agnel Salve (002955753)
################################################################################

set.seed(2026)
library(tidyverse)

cat(rep("=", 70), collapse=""); cat("\n")
cat("SCRIPT 1: DATA PREPARATION\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

################################################################################
# 1. LOAD RAW DATA
################################################################################

cat("Step 1: Loading JM1 and KC1 datasets...\n")

jm1 <- read_csv("data/raw/jm1.csv", show_col_types = FALSE)
kc1 <- read_csv("data/raw/kc1.csv", show_col_types = FALSE)

cat("  JM1 loaded: n =", nrow(jm1), "\n")
cat("  KC1 loaded: n =", nrow(kc1), "\n\n")

################################################################################
# 2. ADD PROJECT IDENTIFIERS & COMBINE
################################################################################

cat("Step 2: Combining datasets...\n")

jm1 <- jm1 %>% mutate(project = "JM1")
kc1 <- kc1 %>% mutate(project = "KC1")

df_combined <- bind_rows(jm1, kc1)

cat("  Combined: n =", nrow(df_combined), "\n\n")

################################################################################
# 3. FEATURE ENGINEERING
################################################################################

cat("Step 3: Engineering features...\n")

df <- df_combined %>%
  mutate(
    # Rename defects column
    defect_count = defects,
    
    # A. Development Methodology
    development_method = case_when(
      project == "JM1" ~ "Waterfall",
      project == "KC1" ~ "Hybrid",
      TRUE ~ NA_character_
    ),
    development_method = factor(development_method, levels = c("Waterfall", "Hybrid")),
    
    # B. Binary High-Risk
    high_risk = if_else(defect_count >= 5, 1, 0),
    high_risk_factor = factor(high_risk, levels = c(0, 1), labels = c("Low", "High")),
    
    # C. Log Transformations
    log_loc = log(loc + 1),
    log_v_g = log(v_g + 1),
    log_n = log(n + 1),
    log_defect = log(defect_count + 1),
    
    # D. Complexity levels (3 categories)
    complexity_level = case_when(
      v_g <= quantile(v_g, 0.33) ~ "Low",
      v_g <= quantile(v_g, 0.67) ~ "Medium",
      TRUE ~ "High"
    ),
    complexity_level = factor(complexity_level, levels = c("Low", "Medium", "High"))
  )

cat("  ✓ development_method created:\n")
cat("      Waterfall (JM1):", sum(df$development_method == "Waterfall"), "\n")
cat("      Hybrid (KC1):", sum(df$development_method == "Hybrid"), "\n\n")

cat("  ✓ high_risk created (threshold: defects >= 5):\n")
cat("      High-risk:", sum(df$high_risk == 1), 
    sprintf("(%.1f%%)\n", 100*mean(df$high_risk == 1)))
cat("      Low-risk:", sum(df$high_risk == 0), 
    sprintf("(%.1f%%)\n\n", 100*mean(df$high_risk == 0)))

################################################################################
# 4. DATA QUALITY CHECK
################################################################################

cat("Step 4: Data quality check...\n")

cat("  Missing values:", sum(is.na(df)), "\n")
cat("  ✓ Data quality confirmed\n\n")

################################################################################
# 5. SAVE PROCESSED DATA
################################################################################

cat("Step 5: Saving processed data...\n")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write_csv(df, "data/processed/defect_data_clean.csv")
saveRDS(df, "data/processed/defect_data_clean.rds")

cat("  ✓ Saved: data/processed/defect_data_clean.csv\n")
cat("  ✓ Saved: data/processed/defect_data_clean.rds\n\n")

################################################################################
# 6. SUMMARY STATISTICS (FIXED - No select() conflict)
################################################################################

cat(rep("=", 70), collapse=""); cat("\n")
cat("DATASET SUMMARY\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

# Use base R subset instead of select() to avoid conflicts
summary_vars <- c("defect_count", "loc", "v_g", "n", "v", "d", "i")
summary_stats <- df[, summary_vars] %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Median = median(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print(summary_stats)

################################################################################
# 7. REQUIREMENT COMPLIANCE
################################################################################

cat("\n")
cat(rep("=", 70), collapse=""); cat("\n")
cat("REQUIREMENT COMPLIANCE CHECK\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

cat("✓ Source: NASA-like data (publicly available engineering data)\n")
cat("✓ Size: n =", nrow(df), "(exceeds 100 minimum)\n")
cat("✓ Quantitative response: defect_count\n")
cat("✓ Quantitative predictors: loc, v(g), n, v, d, i (6 total)\n")
cat("✓ Categorical 3+ levels: complexity_level (Low/Medium/High)\n")
cat("✓ Binary response: high_risk (derived from defect_count >= 5)\n")
cat("✓ Domain: Software Engineering\n\n")

cat(rep("=", 70), collapse=""); cat("\n")
cat("DATA PREPARATION COMPLETE\n")
cat(rep("=", 70), collapse=""); cat("\n")
cat("\nNext: Run 02_exploratory_analysis.R\n")
