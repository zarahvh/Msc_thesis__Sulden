# ##############################################################
# Soil and Vegetation Properties by Lithology
# ##############################################################
# This script compares soil and vegetation properties between
# limestone and metamorphic sites using Wilcoxon rank-sum tests
# and boxplots.
#
# It produces:
# 1. Boxplots of soil properties (pH, TOC, N, Ah depth) by
#    lithology with Wilcoxon p-values
# 2. Boxplots of vegetation properties (cover, richness, root
#    depth, root abundance, Shannon diversity, species abundance)
#    by lithology with Wilcoxon p-values
#
# Input: pioneer_data_with_functional_groups.xlsx
# Output: soil_lithology.png
#         veg_lithology.png
# ##############################################################

library(ggplot2)
library(gridExtra)

df <- read_excel("pioneer_data_with_functional_groups.xlsx") %>%
  filter(lithology %in% c("Limestone", "Metamorphic")) 

big_theme <- theme_bw(base_size = 16) +
  theme(
    text              = element_text(family = "sans"),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white"),
    panel.border      = element_blank(),
    axis.line         = element_blank(),          # removes axis lines
    panel.grid.major  = element_line(color = "grey88", linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(size = 14, face = "bold", family = "sans"),
    axis.title        = element_text(size = 13, family = "sans"),
    axis.text         = element_text(size = 12, color = "grey20", family = "sans"),
    legend.title      = element_text(size = 12, face = "bold", family = "sans"),
    legend.text       = element_text(size = 11, family = "sans"),
    legend.background = element_blank(),
    legend.key        = element_rect(fill = "white"),
    plot.margin       = margin(8, 8, 8, 8)
  )

box_theme <- theme_bw(base_size = 13) +
  theme(
    text              = element_text(family = "sans"),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white"),
    panel.border      = element_blank(),
    axis.line         = element_blank(),          # removes axis lines
    panel.grid.major  = element_line(color = "grey88", linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(size = 14, face = "bold", family = "sans"),
    axis.title        = element_text(size = 14, family = "sans"),
    axis.text         = element_text(size = 14, color = "grey20", family = "sans"),
    legend.position   = "none",
    plot.margin       = margin(8, 8, 8, 8)
  )
all_vars <- c("ph_15", "TOC_15", "N_15", "depth_Ah",
              "vegetation_cover", "species_richness", "rootdepth",
              "root_abundance_top15", "species_diversity_shannon", "species_abundance")

var_labels <- c("pH", "TOC (%)", "N (%)", "Ah depth (cm)",
                "Vegetation cover (%)", "Species richness", "Root depth (cm)",
                "Root abundance top 15cm", "Shannon diversity", "Species abundance")

lith_colors <- c("Limestone" = "grey50", "Metamorphic" = "#8B4513")

plots <- list()
for (i in 1:length(all_vars)) {
  v   <- all_vars[i]
  lab <- var_labels[i]
  
  test    <- wilcox.test(df[[v]] ~ df$lithology, exact = FALSE)
  p_val   <- test$p.value
  p_label <- if (p_val < 0.001) {
    "p < 0.001"
  } else if (p_val < 0.01) {
    paste0("p = ", formatC(p_val, format = "f", digits = 3))
  } else {
    paste0("p = ", formatC(p_val, format = "f", digits = 2))
  }
  if (p_val < 0.05) p_label <- paste0(p_label, " *")
  
  plots[[i]] <- ggplot(df, aes(x = lithology, y = .data[[v]], color = lithology)) +
    geom_boxplot(fill = "white", linewidth = 0.7,
                 outlier.shape = 16, outlier.size = 1.5) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3) +
    scale_color_manual(values = lith_colors) +
    annotate("text", x = 1.5, y = max(df[[v]], na.rm = TRUE),
             label = p_label, size = 4, family = "sans") +
    labs(x = "Lithology", y = lab) +
    box_theme +
    theme(legend.position = "none")
}

# Soil figure
soil_lith_figure <- ggarrange(
  plotlist = plots[1:4],
  ncol = 4, nrow = 1,
  labels = LETTERS[1:4],
  font.label = list(size = 12, face = "bold", family = "sans")
)
ggsave("soil_lithology.png",
       soil_lith_figure, width = 12, height = 4, dpi = 300, bg = "white")
print(soil_lith_figure)

# Vegetation figure
veg_lith_figure <- ggarrange(
  plotlist = plots[5:10],
  ncol = 3, nrow = 2,
  labels = LETTERS[1:6],
  font.label = list(size = 12, face = "bold", family = "sans")
)

ggsave("veg_lithology.png", veg_lith_figure, 
       width = 12, height = 8, dpi = 300, bg = "white")
print(soil_lith_figure)


