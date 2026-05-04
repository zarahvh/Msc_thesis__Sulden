# ##############################################################
# Linear Regression: Age vs Soil and Vegetation Properties
# by Lithology, with Outlier Diagnostics
# ##############################################################
# This script tests the relationship between terrain age and
# each soil/vegetation property, split by lithology. It includes
# residual diagnostics, Cook's distance outlier detection, and
# interaction tests for slope differences between lithologies.
#
# 1. Residual normality checks and transformations for TOC and N
# 2. Cook's distance outlier analysis for limestone TOC and N
# 3. Linear regression table: intercept, slope, p-value, R²,
#    and p(b1=b2) per property per lithology, saved as Word table
# 4. Scatterplots with regression lines per lithology and
#    combined 
#
# Input: pioneer_data_with_functional_groups.xlsx
# Output: regression_table.docx
#         regression_age_table.csv
#         age_vs_soil_by_lithology.png
#         age_vs_vegetation_by_lithology.png
# ##############################################################

library(tidyverse)
library(readxl)
library(gridExtra)
library(flextable)
library(officer)

df <- read_excel("pioneer_data_with_functional_groups.xlsx")

lith_colors <- c("Limestone" = "grey50", "Metamorphic" = "#8B4513")

all_vars <- c("ph_15", "TOC_15", "N_15", "depth_Ah",
              "vegetation_cover", "species_richness", "rootdepth",
              "root_abundance_top15", "species_diversity_shannon", "species_abundance")

var_labels <- c("pH", "TOC (%)", "N (%)", "Ah depth (cm)",
                "Vegetation cover (%)", "Species richness", "Root depth (cm)",
                "Root abundance", "Shannon Diversity", "Species Abundance")

### 1. Residual normality and outlier diagnostics ###

# Check sqrt transform for TOC and N
df$sqrt_TOC_15 <- sqrt(df$TOC_15 - min(df$TOC_15, na.rm = TRUE))
df$sqrt_N_15 <- sqrt(df$N_15 - min(df$N_15, na.rm = TRUE))

shapiro.test(residuals(lm(sqrt_TOC_15 ~ age, data = df)))
shapiro.test(residuals(lm(sqrt_N_15 ~ age, data = df)))

# Check residuals by lithology
m_toc_lim <- lm(TOC_15 ~ age, data = subset(df, lithology == "Limestone"))
m_toc_met <- lm(TOC_15 ~ age, data = subset(df, lithology == "Metamorphic"))

shapiro.test(residuals(m_toc_lim))
shapiro.test(residuals(m_toc_met))

# Cook's distance for limestone TOC and N
df_lim <- subset(df, lithology == "Limestone")

m_toc <- lm(TOC_15 ~ age, data = df_lim)
cd_toc <- cooks.distance(m_toc)

m_n <- lm(N_15 ~ age, data = df_lim)
cd_n <- cooks.distance(m_n)

# Identify outlier (observation 7 = A113)
which(cd_toc > 4 / length(cd_toc))
which(cd_n > 4 / length(cd_n))

# Sensitivity check: refit without outlier
df_lim_clean <- df_lim[cd_toc <= 4 / length(cd_toc), ]

m_toc_clean <- lm(TOC_15 ~ age, data = df_lim_clean)
shapiro.test(residuals(m_toc_clean))
summary(m_toc)$coefficients
summary(m_toc_clean)$coefficients
summary(m_toc)$r.squared
summary(m_toc_clean)$r.squared

m_n_clean <- lm(N_15 ~ age, data = df_lim_clean)
shapiro.test(residuals(m_n_clean))
summary(m_n)$coefficients
summary(m_n_clean)$coefficients
summary(m_n)$r.squared
summary(m_n_clean)$r.squared

### 2. Regression table ###

properties <- all_vars
results <- data.frame()

for (prop in properties) {
  if (!prop %in% names(df)) next
  
  m_all <- lm(as.formula(paste(prop, "~ age")), data = df)
  s_all <- summary(m_all)
  
  m_lim <- lm(as.formula(paste(prop, "~ age")),
              data = subset(df, lithology == "Limestone"))
  s_lim <- summary(m_lim)
  
  m_met <- lm(as.formula(paste(prop, "~ age")),
              data = subset(df, lithology == "Metamorphic"))
  s_met <- summary(m_met)
  
  # Interaction model for p(b1 = b2)
  m_int <- lm(as.formula(paste(prop, "~ age * lithology")), data = df)
  s_int <- summary(m_int)
  int_row <- grep("age:lithology", rownames(s_int$coefficients))
  p_interaction <- if (length(int_row) > 0) round(s_int$coefficients[int_row, 4], 4) else NA
  
  results <- rbind(results, data.frame(
    property = prop, group = "All",
    a = round(s_all$coefficients[1, 1], 4),
    b = round(s_all$coefficients[2, 1], 4),
    p_b = round(s_all$coefficients[2, 4], 4),
    R2 = round(s_all$r.squared, 3),
    p_b1_eq_b2 = p_interaction
  ))
  
  results <- rbind(results, data.frame(
    property = prop, group = "Metamorphic",
    a = round(s_met$coefficients[1, 1], 4),
    b = round(s_met$coefficients[2, 1], 4),
    p_b = round(s_met$coefficients[2, 4], 4),
    R2 = round(s_met$r.squared, 3),
    p_b1_eq_b2 = NA
  ))
  
  results <- rbind(results, data.frame(
    property = prop, group = "Limestone",
    a = round(s_lim$coefficients[1, 1], 4),
    b = round(s_lim$coefficients[2, 1], 4),
    p_b = round(s_lim$coefficients[2, 4], 4),
    R2 = round(s_lim$r.squared, 3),
    p_b1_eq_b2 = NA
  ))
}

write.csv(results, "regression_age_table.csv",
          row.names = FALSE)

# Save as Word table
ft <- flextable(results)
ft <- set_header_labels(ft,
                        property = "Property", group = "Group",
                        a = "a (intercept)", b = "b (slope)",
                        p_b = "p(b=0)", R2 = "R²", p_b1_eq_b2 = "p(b1=b2)")
ft <- bold(ft, i = ~ p_b < 0.05, j = "p_b")
ft <- bold(ft, i = ~ !is.na(p_b1_eq_b2) & p_b1_eq_b2 < 0.05, j = "p_b1_eq_b2")
ft <- hline(ft, i = seq(3, nrow(results), by = 3))
ft <- autofit(ft)
ft <- theme_vanilla(ft)

doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "regression_table.docx")

# ### 3. Scatterplots ###

df <- df %>% filter(lithology %in% c("Limestone", "Metamorphic"))

plots <- list()
for (i in 1:length(all_vars)) {
  v <- all_vars[i]
  lab <- var_labels[i]
  
  plots[[i]] <- ggplot(df, aes(x = age, y = .data[[v]], color = lithology)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
    geom_smooth(aes(group = 1), method = "lm", se = TRUE, alpha = 0.1,
                color = "black", linetype = "dashed") +
    scale_color_manual(values = lith_colors) +
    labs(x = "Terrain age (years)", y = lab, color = "Lithology") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

soil_plot <- grid.arrange(grobs = plots[1:4], ncol = 2)
ggsave("age_vs_soil_by_lithology.png",
       soil_plot, width = 10, height = 8, dpi = 300)

veg_plot <- grid.arrange(grobs = plots[5:10], ncol = 2)
ggsave("age_vs_vegetation_by_lithology.png",
       veg_plot, width = 10, height = 12, dpi = 300)