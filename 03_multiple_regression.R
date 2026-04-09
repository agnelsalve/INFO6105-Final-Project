################################################################################
# INFO 6105 Final Project - Script 3
# Multiple Linear Regression
# Author: Agnel Salve (002955753)
################################################################################

library(tidyverse)
library(car)
library(broom)

set.seed(2026)

cat(rep("=", 70), collapse=""); cat("\n")
cat("SCRIPT 3: MULTIPLE LINEAR REGRESSION\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

df <- readRDS("data/processed/defect_data_clean.rds")

cat("Research Question:\n")
cat("How do code complexity metrics jointly predict defect density?\n\n")

################################################################################
# FIT MODEL
################################################################################

cat("Fitting regression model...\n")

model <- lm(log_defect ~ log_loc + log_v_g + log_n + v + d + i, data = df)

cat("\n")
cat(rep("=", 70), collapse=""); cat("\n")
cat("MODEL SUMMARY\n")
cat(rep("=", 70), collapse=""); cat("\n")
print(summary(model))

################################################################################
# EXTRACT KEY STATISTICS
################################################################################

cat("\n\n")
cat(rep("#", 70), collapse=""); cat("\n")
cat("*** COPY THESE VALUES INTO YOUR REPORT ***\n")
cat(rep("#", 70), collapse=""); cat("\n\n")

r2 <- summary(model)$r.squared
adj_r2 <- summary(model)$adj.r.squared
fstat <- summary(model)$fstatistic

cat(sprintf("R² = %.4f\n", r2))
cat(sprintf("Adjusted R² = %.4f\n", adj_r2))
cat(sprintf("F(%d, %d) = %.2f, p < 0.001\n", fstat[2], fstat[3], fstat[1]))
cat(sprintf("\nModel explains %.1f%% of variance in log(defects)\n\n", 100*adj_r2))

cat("COEFFICIENTS (Holding All Else Constant):\n\n")
coef_df <- broom::tidy(model, conf.int = TRUE)
write_csv(coef_df, "outputs/tables/regression_coefficients.csv")

for(i in 2:nrow(coef_df)) {
  term <- coef_df$term[i]
  beta <- coef_df$estimate[i]
  pval <- coef_df$p.value[i]
  sig <- ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", 
                ifelse(pval < 0.05, "*", "ns")))
  
  cat(sprintf("%s:\n", term))
  cat(sprintf("  β = %.4f, p = %.4f %s\n", beta, pval, sig))
  
  if(grepl("log_", term)) {
    cat(sprintf("  → 1%% increase in %s → %.2f%% change in defects\n\n",
                gsub("log_", "", term), abs(beta)))
  } else {
    cat(sprintf("  → 1-unit increase → %.4f change in log(defects)\n\n", beta))
  }
}

################################################################################
# VIF
################################################################################

cat(rep("=", 70), collapse=""); cat("\n")
cat("VIF - MULTICOLLINEARITY CHECK\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

vif_vals <- vif(model)
vif_df <- data.frame(
  Variable = names(vif_vals),
  VIF = round(vif_vals, 2),
  Status = case_when(
    vif_vals < 5 ~ "✓ No concern",
    vif_vals < 10 ~ "⚠ Moderate",
    TRUE ~ "✗ High"
  )
)

print(vif_df)
write_csv(vif_df, "outputs/tables/regression_vif.csv")

cat("\n")

################################################################################
# DIAGNOSTIC PLOTS
################################################################################

cat("Creating diagnostic plots...\n")

pdf("outputs/figures/Fig7_regression_diagnostics.pdf", width = 12, height = 10)
par(mfrow = c(2, 2))
plot(model, which = 1:4)
dev.off()

cat("  ✓ Figure 7 saved\n\n")

################################################################################
# PREDICTION EXAMPLE (FIXED - No select())
################################################################################

cat("Example predictions:\n\n")

examples <- data.frame(
  Scenario = c("Small Simple", "Medium", "Large Complex"),
  loc = c(100, 500, 2000),
  v_g = c(5, 20, 80),
  n = c(150, 600, 3000),
  v = c(800, 3500, 18000),
  d = c(50, 150, 400),
  i = c(100, 250, 600)
) %>%
  mutate(log_loc = log(loc+1), log_v_g = log(v_g+1), log_n = log(n+1))

preds <- predict(model, newdata = examples, interval = "prediction")
examples$predicted_defects <- round(exp(preds[,"fit"]) - 1, 2)
examples$lower_bound <- round(exp(preds[,"lwr"]) - 1, 2)
examples$upper_bound <- round(exp(preds[,"upr"]) - 1, 2)

# Use base R subset instead of select()
results <- examples[, c("Scenario", "loc", "v_g", "predicted_defects", "lower_bound", "upper_bound")]
print(results)

cat("\n")
cat(rep("=", 70), collapse=""); cat("\n")
cat("MULTIPLE REGRESSION COMPLETE\n")
cat(rep("=", 70), collapse=""); cat("\n")
cat("\nNext: Run 04_anova.R\n")
