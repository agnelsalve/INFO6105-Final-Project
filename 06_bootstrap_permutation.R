################################################################################
# INFO 6105 Final Project - Script 6
# Bootstrap & Permutation Tests
# Author: Agnel Salve (002955753)
################################################################################

library(tidyverse)
library(boot)

set.seed(2026)

cat(rep("=", 70), collapse=""); cat("\n")
cat("SCRIPT 6: BOOTSTRAP & PERMUTATION TESTS\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

df <- readRDS("data/processed/defect_data_clean.rds")

cat("Research Question:\n")
cat("Are findings robust without normality assumptions?\n\n")

################################################################################
# 1. BOOTSTRAP CIs FOR MEDIANS
################################################################################

cat(rep("=", 70), collapse=""); cat("\n")
cat("BOOTSTRAP: 95% CI FOR MEDIAN DEFECTS\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

boot_median <- function(data, indices) median(data[indices])

B <- 10000
cat(sprintf("Running %d bootstrap resamples...\n\n", B))

methods <- levels(df$development_method)
boot_results <- list()

for(method in methods) {
  cat(sprintf("Bootstrapping %s...\n", method))
  
  data_sub <- df %>% filter(development_method == method) %>% pull(defect_count)
  boot_res <- boot(data_sub, boot_median, R = B)
  boot_ci <- boot.ci(boot_res, type = "perc", conf = 0.95)
  
  boot_results[[method]] <- list(
    observed = median(data_sub),
    ci_lower = boot_ci$percent[4],
    ci_upper = boot_ci$percent[5],
    boot_dist = boot_res$t
  )
  
  cat(sprintf("  Observed Median = %.2f\n", boot_results[[method]]$observed))
  cat(sprintf("  Bootstrap 95%% CI: [%.2f, %.2f]\n\n", 
              boot_results[[method]]$ci_lower,
              boot_results[[method]]$ci_upper))
}

cat(rep("#", 70), collapse=""); cat("\n")
cat("*** EXTRACT THESE BOOTSTRAP CIs FOR YOUR REPORT ***\n")
cat(rep("#", 70), collapse=""); cat("\n\n")

boot_summary <- map_df(names(boot_results), function(m) {
  tibble(
    Methodology = m,
    Observed_Median = boot_results[[m]]$observed,
    CI_Lower = boot_results[[m]]$ci_lower,
    CI_Upper = boot_results[[m]]$ci_upper
  )
}) %>% mutate(across(where(is.numeric), ~round(., 2)))

print(boot_summary)
write_csv(boot_summary, "outputs/tables/bootstrap_cis.csv")

cat("\n")

################################################################################
# 2. PERMUTATION TEST
################################################################################

cat(rep("=", 70), collapse=""); cat("\n")
cat("PERMUTATION TEST FOR METHODOLOGY EFFECT\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

observed_anova <- aov(defect_count ~ development_method, data = df)
observed_f <- summary(observed_anova)[[1]]$`F value`[1]

cat(sprintf("Observed F-statistic = %.2f\n\n", observed_f))

n_perm <- 10000
cat(sprintf("Running %d permutations...\n", n_perm))

perm_f <- numeric(n_perm)

for(i in 1:n_perm) {
  df_perm <- df %>% mutate(method_perm = sample(development_method))
  perm_anova <- aov(defect_count ~ method_perm, data = df_perm)
  perm_f[i] <- summary(perm_anova)[[1]]$`F value`[1]
  
  if(i %% 2000 == 0) cat(sprintf("  Progress: %d/%d\n", i, n_perm))
}

perm_p <- mean(perm_f >= observed_f)

cat("\n")
cat(rep("#", 70), collapse=""); cat("\n")
cat("*** EXTRACT THIS PERMUTATION P-VALUE FOR YOUR REPORT ***\n")
cat(rep("#", 70), collapse=""); cat("\n\n")

cat(sprintf("Permutation p-value = %.4f\n", perm_p))
cat(sprintf("Permutations >= observed F: %d / %d\n", 
            sum(perm_f >= observed_f), n_perm))

if(perm_p < 0.05) {
  cat("\n→ REJECT H₀ (distribution-free test confirms ANOVA)\n\n")
} else {
  cat("\n→ FAIL TO REJECT H₀\n\n")
}

write_csv(tibble(
  Observed_F = observed_f,
  Permutation_P = perm_p,
  N_Permutations = n_perm
), "outputs/tables/permutation_results.csv")

################################################################################
# 3. BOOTSTRAP VISUALIZATIONS
################################################################################

cat("Creating bootstrap visualizations...\n")

pdf("outputs/figures/Fig10_bootstrap_distributions.pdf", width = 12, height = 5)
par(mfrow = c(1, 2))

for(method in methods) {
  hist(boot_results[[method]]$boot_dist,
       breaks = 50,
       main = paste("Bootstrap Distribution:", method),
       xlab = "Median Defect Count",
       col = ifelse(method == "Waterfall", "#e74c3c", "#3498db"),
       border = "white")
  
  abline(v = boot_results[[method]]$observed, col = "darkred", lwd = 2, lty = 2)
  abline(v = boot_results[[method]]$ci_lower, col = "darkgreen", lwd = 2, lty = 3)
  abline(v = boot_results[[method]]$ci_upper, col = "darkgreen", lwd = 2, lty = 3)
  
  legend("topright", 
         legend = c("Observed", "95% CI"),
         col = c("darkred", "darkgreen"),
         lty = c(2, 3), lwd = 2)
}

dev.off()

png("outputs/figures/Fig10_bootstrap_distributions.png", width = 1200, height = 500, res = 150)
par(mfrow = c(1, 2))

for(method in methods) {
  hist(boot_results[[method]]$boot_dist,
       breaks = 50,
       main = paste("Bootstrap Distribution:", method),
       xlab = "Median Defect Count",
       col = ifelse(method == "Waterfall", "#e74c3c", "#3498db"),
       border = "white")
  
  abline(v = boot_results[[method]]$observed, col = "darkred", lwd = 2, lty = 2)
  abline(v = boot_results[[method]]$ci_lower, col = "darkgreen", lwd = 2, lty = 3)
  abline(v = boot_results[[method]]$ci_upper, col = "darkgreen", lwd = 2, lty = 3)
  
  legend("topright", 
         legend = c("Observed", "95% CI"),
         col = c("darkred", "darkgreen"),
         lty = c(2, 3), lwd = 2)
}

dev.off()

cat("  ✓ Figure 10: Bootstrap distributions saved\n")

################################################################################
# 4. PERMUTATION VISUALIZATION
################################################################################

pdf("outputs/figures/Fig11_permutation_distribution.pdf", width = 9, height = 7)
hist(perm_f, breaks = 50,
     main = "Permutation Test: Null Distribution of F-Statistic",
     xlab = "F-Statistic",
     col = "#95a5a6",
     border = "white",
     xlim = c(min(perm_f), max(observed_f, max(perm_f)) * 1.1))

abline(v = observed_f, col = "red", lwd = 3, lty = 2)

text(observed_f * 1.05, max(table(cut(perm_f, 50))) * 0.85,
     sprintf("Observed F = %.2f\nPermutation p = %.4f\n(%d/%d >= observed)", 
             observed_f, perm_p, sum(perm_f >= observed_f), n_perm),
     col = "red", cex = 1, font = 2, pos = 4)

dev.off()

png("outputs/figures/Fig11_permutation_distribution.png", width = 900, height = 700, res = 150)
hist(perm_f, breaks = 50,
     main = "Permutation Test: Null Distribution of F-Statistic",
     xlab = "F-Statistic",
     col = "#95a5a6",
     border = "white",
     xlim = c(min(perm_f), max(observed_f, max(perm_f)) * 1.1))
abline(v = observed_f, col = "red", lwd = 3, lty = 2)
text(observed_f * 1.05, max(table(cut(perm_f, 50))) * 0.85,
     sprintf("Observed F = %.2f\nPermutation p = %.4f", observed_f, perm_p),
     col = "red", cex = 1, font = 2, pos = 4)
dev.off()

cat("  ✓ Figure 11: Permutation distribution saved\n\n")

################################################################################
# 5. COMPARISON TABLE
################################################################################

parametric_p <- summary(observed_anova)[[1]]$`Pr(>F)`[1]

comparison <- tibble(
  Method = c("Parametric ANOVA", "Permutation Test"),
  P_Value = c(parametric_p, perm_p),
  Decision = c(
    ifelse(parametric_p < 0.05, "Reject H₀", "Fail to Reject"),
    ifelse(perm_p < 0.05, "Reject H₀", "Fail to Reject")
  )
) %>% mutate(P_Value = round(P_Value, 6))

print(comparison)
write_csv(comparison, "outputs/tables/parametric_vs_computational.csv")

cat("\n")
if(sign(parametric_p - 0.05) == sign(perm_p - 0.05)) {
  cat("✓ Both methods AGREE\n")
  cat("→ Conclusions robust to assumptions\n\n")
} else {
  cat("⚠ Methods DISAGREE\n")
  cat("→ Trust permutation (no assumptions)\n\n")
}

cat(rep("=", 70), collapse=""); cat("\n")
cat("BOOTSTRAP & PERMUTATION COMPLETE\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

cat("All analyses finished!\n")
cat("Check outputs/ for all results\n")
