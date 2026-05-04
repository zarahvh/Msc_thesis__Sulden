## post hoc analysis of isopam nmds results


library(tidyverse)
library(readxl)
library(ggplot2)

theme_set(
  theme_minimal(base_family = "sans", base_size = 11) +
    theme(
      plot.title = element_text(size = 13),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )
)


df_class <- read_excel("data_veg_classes.xlsx")
ggplot(df_class, aes(x = veg_class, y = age)) +
  geom_boxplot(aes(fill = veg_class), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  labs(x = "Vegetation class", y = "Terrain age (years)",
       title = "Age distribution per vegetation class") +
  theme(legend.position = "none")

library(gridExtra)

# Make sure factors are ordered
df_class$veg_class <- factor(df_class$veg_class)
df_class$geom_disturbance <- factor(df_class$geom_disturbance, levels = c("high", "moderate", "low", "stable"))

# Filter to limestone and metamorphic only
df_sub <- df_class %>% filter(lithology %in% c("Limestone", "Metamorphic"))
df_sub$age_class <- factor(df_sub$age_class, levels = c("0-22", "22-56", "56-86", ">86"))

# Recode vegetation classes
df_sub$veg_class_name <- recode(df_sub$veg_class,
                                "0" = "No vegetation",
                                "1" = "Pioneer",
                                "2" = "Early successional",
                                "3" = "Intermediate-late",
                                "4" = "Late successional")

veg_colors <- c("0" = "grey", "1" = "#E41A1C", "2" = "#4DAF4A", "3" = "#377EB8", "4" = "#FF7F00")

df_sub$veg_class_name <- factor(df_sub$veg_class_name)
# Panel A: Age vs vegetation class, split by lithology
p1 <- ggplot(df_sub, aes(x = age, y = veg_class, color = geom_disturbance, shape = lithology)) +
  geom_jitter(height = 0.15, size = 3, alpha = 0.8) +
  scale_color_manual(values = c("high" = "#d73027", "moderate" = "#fee090", "low" = "#fc8d59", "stable" = "#91bfdb")) +
  scale_shape_manual(values = c("Limestone" = 16, "Metamorphic" = 17)) +
  labs(x = "Terrain age (years)", y = "Vegetation class",
       color = "Disturbance", shape = "Lithology",
       title = "A) Vegetation class by age, lithology and disturbance") +
  big_theme

# Panel B: Stacked bar - vegetation class proportions per age class, faceted by lithology
p2 <- ggplot(df_sub, aes(x = age_class, fill = veg_class)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = veg_colors, name = "Vegetation class") +
  facet_wrap(~ lithology) +
  labs(x = "Age class", y = "Proportion", fill = "Vegetation class",
       title = "B) Vegetation class distribution per age class and lithology") +
  big_theme

# Panel C: Stacked bar - disturbance per vegetation class, faceted by lithology
p3 <- ggplot(df_sub, aes(x = veg_class, fill = geom_disturbance)) +
  geom_bar(position = "fill") +
  facet_wrap(~ lithology) +
  scale_fill_manual(values = c("high" = "#d73027", "moderate" = "#fee090", "low" = "#fc8d59", "stable" = "#91bfdb")) +
  labs(x = "Vegetation class", y = "Proportion", fill = "Disturbance",
       title = "C) Disturbance distribution per vegetation class and lithology") +
  big_theme

# Combine
combined <- grid.arrange(p1, p2, p3, ncol = 1, heights = c(1.2, 1, 1))

ggsave("successional_trajectories_combined.png",
       combined, width = 12, height = 16, dpi = 600)
