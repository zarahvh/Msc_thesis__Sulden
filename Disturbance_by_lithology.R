# ##############################################################
# Geomorphic Disturbance Analysis: ANCOVA and Visualisation
# ##############################################################
# This script analyses the role of geomorphic disturbance in soil
# and vegetation development, and separates the effects of age,
# lithology, and disturbance in the Sulden glacier foreland.
#
# 1. Disturbance distribution across lithologies (Fisher's test
#    + stacked bar chart)
# 2. ANCOVA (age + lithology + disturbance) separating the
#    independent contribution of each factor, saved as Word table
# 3. Scatterplots of age vs properties coloured by disturbance,
#    showing independent effects of age and disturbance
# 4. Boxplots of properties by disturbance level
#    (Kruskal-Wallis tests)
#
# Input: Sulden_data.xlsx
# Output: disturbance_distribution_lithology.png
#         ancova_table.docx
#         soil_age_disturbance.png
#         veg_age_disturbance.png
#         soil_disturbance.png
#         veg_disturbance.png
# ##############################################################

library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(flextable)
library(officer)

theme_set(
  theme_minimal(base_family = "sans", base_size = 11) +
    theme(
      plot.title = element_text(size = 13),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 13),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13)
    )
)

df <- read_excel("Sulden_data.xlsx")

lith_colors <- c("Limestone" = "grey50", "Metamorphic" = "#8B4513")

all_vars <- c("ph_15", "TOC_15", "N_15", "depth_Ah",
              "vegetation_cover", "species_richness", "rootdepth",
              "root_abundance_top15", "species_diversity_shannon", "species_abundance")
var_labels <- c("pH", "TOC", "N", "Ah depth",
                "Vegetation cover", "Species richness", "Root depth",
                "Root abundance top 15cm", "Shannon diversity", "Species abundance")

# ##############################################################
# 1. Disturbance distribution across lithologies
# ##############################################################

table(df$lithology, df$geom_disturbance)
fisher.test(table(df$lithology, df$geom_disturbance))

ggplot(df, aes(x = lithology, fill = geom_disturbance)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  labs(x = "Lithology", y = "Proportion", fill = "Disturbance") +
  theme_minimal(base_family = "sans", base_size = 18)

ggsave("disturbance_distribution_lithology.png",
       width = 8, height = 6, dpi = 300)

# ##############################################################
# 2. ANCOVA: age + lithology + disturbance
# ##############################################################

df_lm <- df %>%
  filter(lithology %in% c("Limestone", "Metamorphic")) %>%
  mutate(
    lithology = as.factor(lithology),
    geom_disturbance = as.factor(geom_disturbance)
  )

# Fisher's test without mixed
table(df_lm$lithology, df_lm$geom_disturbance)
prop.table(table(df_lm$lithology, df_lm$geom_disturbance), margin = 1)
fisher.test(table(df_lm$lithology, df_lm$geom_disturbance))

# Run ANCOVA and create table
properties <- all_vars
results <- data.frame()

for (i in 1:length(properties)) {
  v <- properties[i]
  if (!v %in% names(df_lm)) next
  
  m <- lm(as.formula(paste(v, "~ age + lithology + geom_disturbance")), data = df_lm)
  s <- summary(m)
  co <- s$coefficients
  
  add_stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01) return("**")
    if (p < 0.05) return("*")
    if (p < 0.1) return(".")
    return("")
  }
  
  format_coef <- function(est, p) {
    paste0(formatC(round(est, 4), format = "f", digits = 4), add_stars(p))
  }
  
  results <- rbind(results, data.frame(
    Property = var_labels[i],
    Age = format_coef(co["age", 1], co["age", 4]),
    Lithology = format_coef(co["lithologyMetamorphic", 1], co["lithologyMetamorphic", 4]),
    Low_disturbance = format_coef(co["geom_disturbancelow", 1], co["geom_disturbancelow", 4]),
    Moderate_disturbance = format_coef(co["geom_disturbancemoderate", 1], co["geom_disturbancemoderate", 4]),
    R2 = round(s$r.squared, 3)
  ))
}

ft <- flextable(results)
ft <- set_header_labels(ft,
                        Property = "Property",
                        Age = "Age (b)",
                        Lithology = "Lithology (Metamorphic)",
                        Low_disturbance = "Low disturbance",
                        Moderate_disturbance = "Moderate disturbance",
                        R2 = "R²")
ft <- add_footer_lines(ft, "*** p<0.001, ** p<0.01, * p<0.05, . p<0.1")
ft <- add_footer_lines(ft, "Reference categories: Limestone, High disturbance")
ft <- autofit(ft)
ft <- theme_vanilla(ft)

doc <- read_docx()
doc <- body_add_paragraph(doc, "ANCOVA results: property ~ age + lithology + geom_disturbance", style = "heading 1")
doc <- body_add_flextable(doc, ft)
print(doc, target = "ancova_table.docx")

# ##############################################################
# 3. Scatterplots: age vs properties by disturbance
# ##############################################################

plots <- list()
for (i in 1:length(all_vars)) {
  v <- all_vars[i]
  lab <- var_labels[i]
  
  plots[[i]] <- ggplot(df_lm, aes(x = age, y = .data[[v]], color = geom_disturbance)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_brewer(palette = "YlOrRd", direction = -1) +
    labs(x = "Terrain age (years)", y = lab, color = "Disturbance") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

soil_plot <- grid.arrange(grobs = plots[1:4], ncol = 2,
                          top = "Soil properties: age × disturbance")
ggsave("soil_age_disturbance.png",
       soil_plot, width = 10, height = 8, dpi = 300)

veg_plot <- grid.arrange(grobs = plots[5:10], ncol = 2,
                         top = "Vegetation properties: age × disturbance")
ggsave("veg_age_disturbance.png",
       veg_plot, width = 10, height = 12, dpi = 300)

# ##############################################################
# 4. Boxplots: properties by disturbance level
# ##############################################################

df_lm$geom_disturbance <- factor(
  df_lm$geom_disturbance,
  levels = c("high", "moderate", "low")
)

plots <- list()
for (i in 1:length(all_vars)) {
  v <- all_vars[i]
  lab <- var_labels[i]
  
  test <- kruskal.test(as.formula(paste(v, "~ geom_disturbance")), data = df_lm)
  p_val <- test$p.value
  p_label <- if (p_val < 0.001) {
    "p < 0.001"
  } else if (p_val < 0.01) {
    paste0("p = ", formatC(p_val, format = "f", digits = 3))
  } else {
    paste0("p = ", formatC(p_val, format = "f", digits = 2))
  }
  if (p_val < 0.05) p_label <- paste0(p_label, " *")
  
  plots[[i]] <- ggplot(df_lm, aes(x = geom_disturbance, y = .data[[v]],
                                  color = geom_disturbance)) +
    geom_boxplot(fill = "white", linewidth = 0.7,
                 outlier.shape = 16, outlier.size = 1.5) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3) +
    scale_color_brewer(palette = "YlOrRd", direction = -1) +
    annotate("text", x = 1.5, y = max(df_lm[[v]], na.rm = TRUE),
             label = p_label, size = 4, family = "sans") +
    labs(x = "Disturbance", y = lab) +
    theme_minimal() +
    theme(legend.position = "none")
}

soil_dist_figure <- ggarrange(
  plotlist = plots[1:4],
  ncol = 2, nrow = 2,
  labels = LETTERS[1:4],
  font.label = list(size = 12, face = "bold", family = "sans")
)
ggsave("soil_disturbance.png",
       soil_dist_figure, width = 12, height = 8, dpi = 300, bg = "white")

veg_dist_figure <- ggarrange(
  plotlist = plots[5:10],
  ncol = 3, nrow = 2,
  labels = LETTERS[1:6],
  font.label = list(size = 12, face = "bold", family = "sans")
)
ggsave("veg_disturbance.png",
       veg_dist_figure, width = 12, height = 8, dpi = 300, bg = "white")
