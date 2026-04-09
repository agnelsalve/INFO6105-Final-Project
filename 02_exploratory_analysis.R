################################################################################
# INFO 6105 Final Project - Script 2
# Exploratory Data Analysis
# Author: Agnel Salve (002955753)
################################################################################

library(tidyverse)
library(gridExtra)
library(corrplot)

set.seed(2026)

cat(rep("=", 70), collapse=""); cat("\n")
cat("SCRIPT 2: EXPLORATORY DATA ANALYSIS\n")
cat(rep("=", 70), collapse=""); cat("\n\n")

df <- readRDS("data/processed/defect_data_clean.rds")
cat("Loaded:", nrow(df), "observations\n\n")

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1. DESCRIPTIVE STATISTICS (FIXED - No select() conflict)
################################################################################

cat("Computing descriptive statistics...\n")

desc_vars <- c("defect_count", "loc", "v_g", "n", "v", "d", "i")
desc_stats <- df[, desc_vars] %>%
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

print(desc_stats)
write_csv(desc_stats, "outputs/tables/descriptive_statistics.csv")

cat("\n✓ Saved: descriptive_statistics.csv\n\n")

################################################################################
# 2. CORRELATION MATRIX (FIXED)
################################################################################

cat("Computing correlation matrix...\n")

cor_vars <- c("log_loc", "log_v_g", "log_n", "v", "d", "i")
cor_data <- df[, cor_vars]
cor_matrix <- cor(cor_data, use = "complete.obs")

print(round(cor_matrix, 3))
write_csv(as.data.frame(cor_matrix) %>% rownames_to_column("Variable"),
          "outputs/tables/correlation_matrix.csv")

cat("\n✓ Saved: correlation_matrix.csv\n\n")

################################################################################
# 3. VISUALIZATIONS
################################################################################

cat("Creating visualizations...\n")

# Figure 1: Defect distributions
p1 <- ggplot(df, aes(x = defect_count)) +
  geom_histogram(bins = 30, fill = "#3498db", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 5, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Defect Count Distribution",
       x = "Defect Count", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

p2 <- ggplot(df, aes(x = log_defect)) +
  geom_histogram(bins = 30, fill = "#e74c3c", color = "white", alpha = 0.8) +
  labs(title = "Log-Transformed Defects",
       x = "log(Defect Count + 1)", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/figures/Fig1_defect_distributions.pdf", 
       arrangeGrob(p1, p2, ncol = 2), width = 14, height = 5)

cat("  ✓ Figure 1 saved\n")

# Figure 2: Correlation heatmap
pdf("outputs/figures/Fig2_correlation_heatmap.pdf", width = 9, height = 8)
corrplot(cor_matrix, method = "color", type = "lower",
         addCoef.col = "black", number.cex = 0.8,
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix",
         mar = c(0,0,2,0))
dev.off()

cat("  ✓ Figure 2 saved\n")

# Figure 3: Scatterplots
p3 <- ggplot(df, aes(x = log_v_g, y = log_defect)) +
  geom_point(alpha = 0.3, color = "#3498db") +
  geom_smooth(method = "lm", color = "#e74c3c") +
  labs(title = "Defects vs v(g)", x = "log(v(g)+1)", y = "log(Defects+1)") +
  theme_minimal()

p4 <- ggplot(df, aes(x = log_loc, y = log_defect)) +
  geom_point(alpha = 0.3, color = "#9b59b6") +
  geom_smooth(method = "lm", color = "#e74c3c") +
  labs(title = "Defects vs LOC", x = "log(LOC+1)", y = "log(Defects+1)") +
  theme_minimal()

p5 <- ggplot(df, aes(x = log_n, y = log_defect)) +
  geom_point(alpha = 0.3, color = "#2ecc71") +
  geom_smooth(method = "lm", color = "#e74c3c") +
  labs(title = "Defects vs n", x = "log(n+1)", y = "log(Defects+1)") +
  theme_minimal()

ggsave("outputs/figures/Fig3_scatterplots.pdf",
       arrangeGrob(p3, p4, p5, ncol = 3), width = 16, height = 5)

cat("  ✓ Figure 3 saved\n")

# Figure 4: ANOVA boxplot
p_box <- ggplot(df, aes(x = development_method, y = defect_count, fill = development_method)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "red") +
  scale_fill_manual(values = c("Waterfall" = "#e74c3c", "Hybrid" = "#3498db")) +
  labs(title = "Defects by Methodology", x = "Methodology", y = "Defects") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/figures/Fig4_anova_boxplot.pdf", p_box, width = 8, height = 6)

cat("  ✓ Figure 4 saved\n")

# Figure 5: Density by risk
p_dens <- ggplot(df, aes(x = log_v_g, fill = high_risk_factor)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Low" = "#2ecc71", "High" = "#e74c3c")) +
  labs(title = "Complexity by Risk Level", x = "log(v(g)+1)", fill = "Risk") +
  theme_minimal()

ggsave("outputs/figures/Fig5_density_by_risk.pdf", p_dens, width = 10, height = 6)

cat("  ✓ Figure 5 saved\n")

# Figure 6: Risk rate by methodology
risk_stats <- df %>%
  group_by(development_method) %>%
  summarise(high_risk_pct = 100 * mean(high_risk == 1))

p_risk <- ggplot(risk_stats, aes(x = development_method, y = high_risk_pct, fill = development_method)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", high_risk_pct)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Waterfall" = "#e74c3c", "Hybrid" = "#3498db")) +
  labs(title = "High-Risk Rate by Methodology", x = "Methodology", y = "High-Risk (%)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/figures/Fig6_risk_by_methodology.pdf", p_risk, width = 8, height = 6)

cat("  ✓ Figure 6 saved\n\n")

cat(rep("=", 70), collapse=""); cat("\n")
cat("EDA COMPLETE - 6 figures created\n")
cat(rep("=", 70), collapse=""); cat("\n")
cat("\nNext: Run 03_multiple_regression.R\n")
