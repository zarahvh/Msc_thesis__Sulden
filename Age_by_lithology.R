###############################################################################
# Age vs Soil and Vegetation Properties by Lithology
###############################################################################
# This script compares soil and vegetation development over terrain age
# between limestone and metamorphic sites in the Sulden glacier foreland.
#
# 1. Age distribution comparison between lithologies (Wilcoxon test + boxplot)
# 2. Spearman correlation table: age vs each property, per lithology
# 3. Scatterplots with linear regression lines: age vs soil properties
# 4. Scatterplots with linear regression lines: age vs vegetation properties
#
# Input: pioneer_data_with_functional_groups.xlsx
# Output: age_distribution_by_lithology.png
#         age_vs_soil_by_lithology.png
#         age_vs_vegetation_by_lithology.png
##############################################################################



library(tidyverse)
library(readxl)
library(gridExtra)


df <- read_excel("pioneer_data_with_functional_groups.xlsx") %>%
  filter(lithology %in% c("Limestone", "Metamorphic"))

# Test normaility
df %>% filter(lithology == "Limestone") %>% pull(age) %>% shapiro.test() 
df %>% filter(lithology == "Metamorphic") %>% pull(age) %>% shapiro.test()

# Metamorphic is not normally distributed, so we use wilcox
wilcox.test(age ~ lithology, data = df %>% filter(lithology %in% c("Limestone", "Metamorphic")))

# Define colors
lith_colors <- c("Limestone" = "grey50", "Metamorphic" = "#8B4513")

# Calculate summary stats
stats <- df %>%
  filter(lithology %in% c("Limestone", "Metamorphic")) %>%
  group_by(lithology) %>%
  summarise(
    n = n(),
    mean = round(mean(age, na.rm = TRUE), 1),
    sd = round(sd(age, na.rm = TRUE), 1),
    median = median(age, na.rm = TRUE),
    min = min(age, na.rm = TRUE),
    max = max(age, na.rm = TRUE)
  )

print(stats)

df %>%
  filter(lithology %in% c("Limestone", "Metamorphic")) %>%
  ggplot(aes(x = lithology, y = age, fill = lithology)) +
  geom_boxplot(outlier.shape = 1) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  scale_fill_manual(values = c("Limestone" = "grey50", "Metamorphic" = "#8B4513")) +
  labs(x = "Lithology", y = "Terrain age (years)", title = "Age Distribution by Lithology") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("age_distribution_by_lithology.png", width = 6, height = 5, dpi = 300)


# Variables to test
soil_vars <- c("ph_15", "TOC_15", "N_15", "depth_Ah")
veg_vars <- c("vegetation_cover", "species_richness", "rootdepth", "root_abundance_top15", "species_diversity_shannon", "species_abundance")

all_vars <- c(soil_vars, veg_vars)
var_labels <- c("pH", "TOC (%)", "N (%)", "Ah depth (cm)", 
                "Vegetation cover (%)", "Species richness", "Root depth (cm)", "Root abundance", "Shannon Diversity", "Species Abundance")

## CORRELATION TABLE ##
cor_table <- tibble()

for (i in 1:length(all_vars)) {
  v <- all_vars[i]
  lab <- var_labels[i]
  
  lime <- df %>% filter(lithology == "Limestone") %>% drop_na(age, !!sym(v))
  meta <- df %>% filter(lithology == "Metamorphic") %>% drop_na(age, !!sym(v))
  
  lime_cor <- cor.test(lime$age, lime[[v]], method = "spearman", exact = FALSE)
  meta_cor <- cor.test(meta$age, meta[[v]], method = "spearman", exact = FALSE)
  
  cor_table <- bind_rows(cor_table, tibble(
    Variable = lab,
    Lime_rho = round(lime_cor$estimate, 2),
    Lime_p = round(lime_cor$p.value, 4),
    Lime_sig = ifelse(lime_cor$p.value < 0.001, "***", 
                      ifelse(lime_cor$p.value < 0.01, "**",
                             ifelse(lime_cor$p.value < 0.05, "*", ""))),
    Meta_rho = round(meta_cor$estimate, 2),
    Meta_p = round(meta_cor$p.value, 4),
    Meta_sig = ifelse(meta_cor$p.value < 0.001, "***", 
                      ifelse(meta_cor$p.value < 0.01, "**",
                             ifelse(meta_cor$p.value < 0.05, "*", "")))
  ))
}

print(cor_table)

## PLOTS ##
plots <- list()

for (i in 1:length(all_vars)) {
  v <- all_vars[i]
  lab <- var_labels[i]
  
  # Correlations
  lime <- df %>% filter(lithology == "Limestone")
  meta <- df %>% filter(lithology == "Metamorphic")
  
  lime_cor <- cor.test(lime$age, lime[[v]], method = "spearman", exact = FALSE)
  meta_cor <- cor.test(meta$age, meta[[v]], method = "spearman", exact = FALSE)
  
  annot <- sprintf("Limestone: ρ=%.2f%s\nMetamorphic: ρ=%.2f%s",
                   lime_cor$estimate, ifelse(lime_cor$p.value < 0.05, "*", ""),
                   meta_cor$estimate, ifelse(meta_cor$p.value < 0.05, "*", ""))
  
  plots[[i]] <- ggplot(df, aes(x = age, y = .data[[v]], color = lithology)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
    scale_color_manual(values = lith_colors) +
    labs(x = "Terrain age (years)", y = lab, color = "Lithology") +
    annotate("text", x = max(df$age, na.rm = TRUE), y = max(df[[v]], na.rm = TRUE), 
             label = annot, hjust = 1, vjust = 1, size = 2.5) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
print(plots)

soil_plot <- grid.arrange(grobs = plots[1:4], ncol = 2)
ggsave("age_vs_soil_by_lithology.png", soil_plot, width = 10, height = 8, dpi = 300)

veg_plot <- grid.arrange(grobs = plots[5:10], ncol = 2)
ggsave("age_vs_vegetation_by_lithology.png", veg_plot, width = 10, height = 12, dpi = 300)

