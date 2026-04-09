################################################################################
# INFO 6105 Final Project - Script 5
# Logistic Regression
# Author: Agnel Salve (002955753)
################################################################################

library(tidyverse)
library(pROC)
library(caret)
library(broom)

set.seed(2026)

cat(rep("=", 70), collapse=""); cat("\n")
cat("SCRIPT 5: LOGISTIC REGRESSION\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

df <- readRDS("data/processed/defect_data_clean.rds")

cat("Research Question:\n")
cat("Which metrics best predict high-risk modules (defects >= 5)?\n\n")

cat("High-Risk Prevalence:", sprintf("%.1f%%\n\n", 100*mean(df$high_risk)))

################################################################################
# FIT MODEL
################################################################################

cat("Fitting logistic regression...\n")

logit_model <- glm(high_risk ~ log_v_g + log_loc + d + development_method,
                   data = df, family = binomial)

print(summary(logit_model))

################################################################################
# ODDS RATIOS
################################################################################

cat("\n")
cat(rep("#", 70), collapse=""); cat("\n")
cat("*** COPY THESE VALUES INTO YOUR REPORT ***\n")
cat(rep("#", 70), collapse=""); cat("\n\n")

cat("ODDS RATIOS (Holding All Else Constant):\n\n")

or_df <- broom::tidy(logit_model, exponentiate = TRUE, conf.int = TRUE)
write_csv(or_df, "outputs/tables/logistic_odds_ratios.csv")

for(i in 2:nrow(or_df)) {
  term <- or_df$term[i]
  or_val <- or_df$estimate[i]
  ci_low <- or_df$conf.low[i]
  ci_high <- or_df$conf.high[i]
  pval <- or_df$p.value[i]
  
  cat(sprintf("%s:\n", term))
  cat(sprintf("  OR = %.4f, 95%% CI: [%.4f, %.4f], p = %.4f\n", 
              or_val, ci_low, ci_high, pval))
  
  if(or_val > 1) {
    pct_change <- (or_val - 1) * 100
    cat(sprintf("  → Increases odds of high-risk by %.1f%%\n\n", pct_change))
  } else {
    pct_change <- (1 - or_val) * 100
    cat(sprintf("  → Decreases odds of high-risk by %.1f%%\n\n", pct_change))
  }
}

################################################################################
# PREDICTIONS & CONFUSION MATRIX
################################################################################

df$pred_prob <- predict(logit_model, type = "response")
df$pred_class <- ifelse(df$pred_prob >= 0.5, 1, 0)

cm <- confusionMatrix(factor(df$pred_class, levels = c(0,1)),
                      factor(df$high_risk, levels = c(0,1)),
                      positive = "1")

cat(rep("=", 70), collapse=""); cat("\n")
cat("CONFUSION MATRIX (Threshold = 0.5)\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

print(cm)

cat("\n** EXTRACT THESE VALUES **\n\n")
cat(sprintf("Accuracy = %.1f%%\n", 100 * cm$overall["Accuracy"]))
cat(sprintf("Sensitivity = %.1f%%\n", 100 * cm$byClass["Sensitivity"]))
cat(sprintf("Specificity = %.1f%%\n", 100 * cm$byClass["Specificity"]))
cat(sprintf("Precision = %.1f%%\n\n", 100 * cm$byClass["Pos Pred Value"]))

################################################################################
# ROC CURVE
################################################################################

cat("Computing ROC curve...\n")

roc_obj <- roc(df$high_risk, df$pred_prob, quiet = TRUE)
auc_val <- as.numeric(auc(roc_obj))

cat(sprintf("\n** AUC = %.4f **\n", auc_val))

# Fixed if-else structure
if(auc_val >= 0.9) {
  cat("→ EXCELLENT discrimination\n\n")
} else if(auc_val >= 0.8) {
  cat("→ GOOD discrimination\n\n")
} else if(auc_val >= 0.7) {
  cat("→ ACCEPTABLE discrimination\n\n")
} else {
  cat("→ POOR discrimination\n\n")
}

pdf("outputs/figures/Fig9_roc_curve.pdf", width = 8, height = 8)
plot(roc_obj, main = "ROC Curve: High-Risk Module Prediction",
     col = "#e74c3c", lwd = 3, print.auc = TRUE, 
     print.auc.x = 0.4, print.auc.y = 0.3, auc.polygon = TRUE,
     auc.polygon.col = "#fce4ec", grid = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)
legend("bottomright", legend = c("Model", "Random"),
       col = c("#e74c3c", "gray50"), lwd = c(3, 2), lty = c(1, 2))
dev.off()

png("outputs/figures/Fig9_roc_curve.png", width = 800, height = 800, res = 150)
plot(roc_obj, main = "ROC Curve: High-Risk Module Prediction",
     col = "#e74c3c", lwd = 3, print.auc = TRUE, 
     print.auc.x = 0.4, print.auc.y = 0.3, auc.polygon = TRUE,
     auc.polygon.col = "#fce4ec", grid = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)
legend("bottomright", legend = c("Model", "Random"),
       col = c("#e74c3c", "gray50"), lwd = c(3, 2), lty = c(1, 2))
dev.off()

cat("  ✓ Figure 9 saved\n\n")

cat(rep("=", 70), collapse=""); cat("\n")
cat("LOGISTIC REGRESSION COMPLETE\n")
cat(rep("=", 70), collapse=""); cat("\n")
cat("\nNext: Run 06_bootstrap_permutation.R\n")
