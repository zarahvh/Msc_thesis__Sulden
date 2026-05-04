# ##############################################################
# Post-hoc Analysis of Vegetation Classes and Successional
# Trajectories
# ##############################################################
# This script visualises the relationships between ISOPAM
# vegetation classes, terrain age, lithology, and geomorphic
# disturbance to examine successional trajectories on limestone
# and metamorphic substrates.
#
# 1. Age distribution per vegetation class (boxplot)
# 2. Combined figure with three panels:
#    A) Individual plots by age, vegetation class, disturbance
#       and lithology
#    B) Vegetation class proportions per age class, split by
#       lithology
#    C) Disturbance proportions per vegetation class, split by
#       lithology
#
# Input: Sulden_data.xlsx
# Output: successional_trajectories_combined.png
# ##############################################################

library(tidyverse)
library(readxl)
library(gridExtra)

big_theme <- theme_bw(base_size = 16) +
  theme(
    text              = element_text(family = "sans"),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white"),
    panel.border      = element_blank(),
    axis.line         = element_blank(),
    panel.grid.major  = element_line(color = "grey88", linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(size = 18, face = "bold", family = "sans"),
    axis.title        = element_text(size = 16, family = "sans"),
    axis.text         = element_text(size = 14, color = "grey20", family = "sans"),
    legend.title      = element_text(size = 12, face = "bold", family = "sans"),
    legend.text       = element_text(size = 12, family = "sans"),
    legend.background = element_blank(),
    legend.key        = element_rect(fill = "white"),
    plot.margin       = margin(8, 8, 8, 8)
  )

# ### 1. Load and prepare data ###

df_class <- read_excel("Sulden_data.xlsx")

df_class$veg_class <- factor(df_class$veg_class)
df_class$geom_disturbance <- factor(df_class$geom_disturbance,
                                    levels = c("high", "moderate", "low", "stable"))

# Filter to limestone and metamorphic only
df_sub <- df_class %>% filter(lithology %in% c("Limestone", "Metamorphic"))
df_sub$age_class <- factor(df_sub$age_class, levels = c("0-22", "22-56", "56-86", ">86"))

veg_colors <- c("0" = "grey", "1" = "#E41A1C", "2" = "#4DAF4A",
                "3" = "#377EB8", "4" = "#FF7F00")

# ### 2. Combined successional trajectories figure ###

# Panel A: individual plots by age, class, disturbance and lithology
p1 <- ggplot(df_sub, aes(x = age, y = veg_class,
                         color = geom_disturbance, shape = lithology)) +
  geom_jitter(height = 0.15, size = 3, alpha = 0.8) +
  scale_color_manual(values = c("high" = "#d73027", "moderate" = "#fee090",
                                "low" = "#fc8d59", "stable" = "#91bfdb")) +
  scale_shape_manual(values = c("Limestone" = 16, "Metamorphic" = 17)) +
  labs(x = "Terrain age (years)", y = "Vegetation class",
       color = "Disturbance", shape = "Lithology",
       title = "A) Vegetation class by age, lithology and disturbance") +
  big_theme

# Panel B: vegetation class proportions per age class by lithology
p2 <- ggplot(df_sub, aes(x = age_class, fill = veg_class)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = veg_colors, name = "Vegetation class") +
  facet_wrap(~ lithology) +
  labs(x = "Age class", y = "Proportion", fill = "Vegetation class",
       title = "B) Vegetation class distribution per age class and lithology") +
  big_theme

# Panel C: disturbance proportions per vegetation class by lithology
p3 <- ggplot(df_sub, aes(x = veg_class, fill = geom_disturbance)) +
  geom_bar(position = "fill") +
  facet_wrap(~ lithology) +
  scale_fill_manual(values = c("high" = "#d73027", "moderate" = "#fee090",
                               "low" = "#fc8d59", "stable" = "#91bfdb")) +
  labs(x = "Vegetation class", y = "Proportion", fill = "Disturbance",
       title = "C) Disturbance distribution per vegetation class and lithology") +
  big_theme

# Combine and save
combined <- grid.arrange(p1, p2, p3, ncol = 1, heights = c(1.2, 1, 1))
ggsave("successional_trajectories_combined.png",
       combined, width = 12, height = 16, dpi = 600)
