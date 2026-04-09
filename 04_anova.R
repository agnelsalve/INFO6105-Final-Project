################################################################################
# INFO 6105 Final Project - Script 4
# One-Way ANOVA
# Author: Agnel Salve (002955753)
################################################################################

library(tidyverse)
library(multcomp)
library(car)

set.seed(2026)

cat(rep("=", 70), collapse=""); cat("\n")
cat("SCRIPT 4: ONE-WAY ANOVA\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

df <- readRDS("data/processed/defect_data_clean.rds")

cat("Research Question:\n")
cat("Do Waterfall and Hybrid methodologies produce different defect rates?\n\n")

################################################################################
# 1. DESCRIPTIVE STATISTICS
################################################################################

cat("Descriptive Statistics by Methodology:\n\n")

group_stats <- df %>%
  group_by(development_method) %>%
  summarise(
    n = n(),
    Mean = mean(defect_count),
    Median = median(defect_count),
    SD = sd(defect_count),
    Min = min(defect_count),
    Max = max(defect_count)
  ) %>%
  mutate(across(where(is.numeric) & !n, ~round(., 2)))

print(group_stats)
write_csv(group_stats, "outputs/tables/anova_descriptive.csv")

cat("\n")

################################################################################
# 2. ANOVA TEST
################################################################################

cat(rep("=", 70), collapse=""); cat("\n")
cat("ANOVA RESULTS\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

anova_model <- aov(defect_count ~ development_method, data = df)
anova_sum <- summary(anova_model)

print(anova_sum)

f_val <- anova_sum[[1]]$`F value`[1]
p_val <- anova_sum[[1]]$`Pr(>F)`[1]
df1 <- anova_sum[[1]]$Df[1]
df2 <- anova_sum[[1]]$Df[2]

cat("\n")
cat(rep("#", 70), collapse=""); cat("\n")
cat("*** COPY THESE VALUES INTO YOUR REPORT ***\n")
cat(rep("#", 70), collapse=""); cat("\n\n")

cat(sprintf("F(%d, %d) = %.2f\n", df1, df2, f_val))
cat(sprintf("p-value = %.6f\n", p_val))

if(p_val < 0.05) {
  cat("\nDecision: REJECT H₀ at α=0.05\n")
  cat("→ Methodologies produce significantly different mean defect rates\n\n")
} else {
  cat("\nDecision: FAIL TO REJECT H₀\n")
  cat("→ No significant difference detected\n\n")
}

################################################################################
# 3. TUKEY HSD
################################################################################

cat("TUKEY HSD (Pairwise Comparison):\n\n")

tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

tukey_df <- broom::tidy(tukey_result)
write_csv(tukey_df, "outputs/tables/anova_tukey.csv")

mean_diff <- tukey_df$estimate[1]
ci_lower <- tukey_df$conf.low[1]
ci_upper <- tukey_df$conf.high[1]

cat(sprintf("\nMean Difference (Hybrid - Waterfall) = %.2f defects\n", mean_diff))
cat(sprintf("95%% CI: [%.2f, %.2f]\n", ci_lower, ci_upper))
cat(sprintf("p = %.4f\n\n", tukey_df$adj.p.value[1]))

################################################################################
# 4. EFFECT SIZE (ETA-SQUARED)
################################################################################

cat(rep("=", 70), collapse=""); cat("\n")
cat("EFFECT SIZE\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

ss_between <- anova_sum[[1]]$`Sum Sq`[1]
ss_total <- sum(anova_sum[[1]]$`Sum Sq`)
eta_sq <- ss_between / ss_total

cat(sprintf("η² (eta-squared) = %.4f\n", eta_sq))
cat(sprintf("→ Methodology explains %.1f%% of variance in defect count\n\n", 100*eta_sq))

if(eta_sq < 0.01) {
  cat("Effect size: SMALL\n\n")
} else if(eta_sq < 0.06) {
  cat("Effect size: MEDIUM\n\n")
} else {
  cat("Effect size: LARGE\n\n")
}

################################################################################
# 5. ASSUMPTION CHECKS
################################################################################

cat("Assumption Checks:\n\n")

# Normality
cat("1. Normality (Shapiro-Wilk by group):\n")
for(method in levels(df$development_method)) {
  data_sub <- df %>% filter(development_method == method) %>% pull(defect_count)
  sw <- shapiro.test(sample(data_sub, min(5000, length(data_sub))))
  cat(sprintf("   %s: W = %.4f, p = %.4f\n", method, sw$statistic, sw$p.value))
}
cat("   Note: Large n → CLT applies despite non-normality\n\n")

# Equal variance
cat("2. Homogeneity of Variance (Levene's Test):\n")
levene <- leveneTest(defect_count ~ development_method, data = df)
print(levene)
cat("\n")

################################################################################
# 6. VISUALIZATION
################################################################################

pdf("outputs/figures/Fig8_anova_violin.pdf", width = 9, height = 6)
ggplot(df, aes(x = development_method, y = defect_count, fill = development_method)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "red", color = "darkred") +
  scale_fill_manual(values = c("Waterfall" = "#e74c3c", "Hybrid" = "#3498db")) +
  labs(title = "Defect Distribution by Methodology (Violin Plot)",
       subtitle = "Red diamond = mean | Box = median & IQR | Violin = density",
       x = "Development Methodology", y = "Defect Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))
dev.off()

cat("✓ Figure 8: Violin plot saved\n\n")

cat(rep("=", 70), collapse=""); cat("\n")
cat("ANOVA COMPLETE\n")
cat(rep("=", 70), collapse=""); cat("\n")
cat("\nNext: Run 05_logistic_regression.R\n")
