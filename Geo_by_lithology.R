##############################################################################
# Geomorphometric Variables vs Soil and Vegetation Properties by Lithology
##############################################################################
# This script examines the relationships between topographic variables
# (slope, elevation, northness, TWI, SPI) and soil/vegetation properties
# on limestone and metamorphic sites in the Sulden glacier foreland.
#
# 1. Normality checks for each geomorphometric variable per lithology
# 2. T-tests comparing geomorphometric variables between lithologies
# 3. Boxplots of geomorphometric variables by lithology
# 4. Spearman correlation table: each terrain variable vs each property,
#    per lithology
# 5. Scatterplots with regression lines: terrain variables vs soil properties
# 6. Scatterplots with regression lines: terrain variables vs vegetation
#    properties
#
# Input: Sulden_data.xlsx
# Output: northness.png
#         geomorph_vs_soil_by_lithology.png
#         geomorph_vs_vegetation_by_lithology.png
#
##############################################################################


library(tidyverse)
library(readxl)
library(gridExtra)
theme_set(
  theme_minimal(base_family = "sans", base_size = 18) +
    theme(
      plot.title = element_text(size = 18),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18)
    )
)


df <- read_excel("Sulden_data.xlsx") %>%
  filter(lithology %in% c("Limestone", "Metamorphic")) 

lith_colors <- c("Limestone" = "grey50", "Metamorphic" = "#8B4513")
geomorph_vars <- c("Slope_3_3m", "Elevation", "Northing", "TWI_correct3_3m", "SPI3_3m")
geomorph_labels <- c("Slope (°)", "Elevation (m)", "Northness", "TWI", "SPI")

# check normality per variable
for (i in 1:length(geomorph_vars)) {
  v <- geomorph_vars[i]
  lab <- geomorph_labels[i]
  
  lime <- df %>% filter(lithology == "Limestone") %>% pull(v) %>% na.omit()
  meta <- df %>% filter(lithology == "Metamorphic") %>% pull(v) %>% na.omit()
  
  lime_shap <- shapiro.test(lime)
  meta_shap <- shapiro.test(meta)
  
  cat(sprintf("%-15s: Limestone p=%.4f %s | Metamorphic p=%.4f %s\n",
              lab, 
              lime_shap$p.value, ifelse(lime_shap$p.value < 0.05, "(non-normal)", "(normal)"),
              meta_shap$p.value, ifelse(meta_shap$p.value < 0.05, "(non-normal)", "(normal)")))
}

## Check if significantly different using t test or mann whitney?
for (i in 1:length(geomorph_vars)) {
  v <- geomorph_vars[i]
  lab <- geomorph_labels[i]
  
  lime <- df %>% filter(lithology == "Limestone") %>% pull(v)
  meta <- df %>% filter(lithology == "Metamorphic") %>% pull(v)
  
  test <- t.test(lime, meta, exact = FALSE)
  sig <- ifelse(test$p.value < 0.05, "*", "")
  
  cat(sprintf("%-15s %12.2f %12.2f %9.4f %s\n",
              lab, mean(lime, na.rm = TRUE), mean(meta, na.rm = TRUE), test$p.value, sig))
}

### BOXPLOTS ###
plots_box <- list()

for (i in 1:length(geomorph_vars)) {
  v <- geomorph_vars[i]
  lab <- geomorph_labels[i]
  
  # Calculate stats
  stats <- df %>%
    group_by(lithology) %>%
    summarise(
      n = n(),
      mean = round(mean(.data[[v]], na.rm = TRUE), 2),
      sd = round(sd(.data[[v]], na.rm = TRUE), 2),
      median = round(median(.data[[v]], na.rm = TRUE), 2),
      min = round(min(.data[[v]], na.rm = TRUE), 2),
      max = round(max(.data[[v]], na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  # t test
  # t test
  test <- t.test(df[[v]] ~ df$lithology)
  p_val <- test$p.value
  
  # format p-value
  p_label <- if (p_val < 0.001) {
    "p < 0.001"
  } else if (p_val < 0.01) {
    paste0("p = ", formatC(p_val, format = "f", digits = 3))
  } else {
    paste0("p = ", formatC(p_val, format = "f", digits = 2))
  }
  
  if (p_val < 0.05) {
    p_label <- paste0(p_label, "*")
  }
  
  plots_box[[i]] <- ggplot(df, aes(x = lithology, y = .data[[v]], color = lithology)) +
    geom_boxplot(outlier.shape = 1) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4) +
    scale_color_manual(values = lith_colors) +
    labs(x = "Lithology", y = lab)+
    annotate("text", x = 1.5, y = max(df[[v]], na.rm = TRUE), 
             label = p_label, size = 4) +
    theme_minimal()+
    theme(legend.position = "none")
}
plots_box[[3]]


combined_box <- grid.arrange(grobs = plots_box, ncol = 2)
c <- plots_box[[3]]
ggsave("northness.png", 
       c, width = 4, height = 3, dpi = 300)

##### VS SOIL AND VEG using Spearman
soil_vars <- c("ph_15", "TOC_15", "N_15", "depth_Ah")
veg_vars <- c("vegetation_cover", "species_richness", "rootdepth", "root_abundance_top15", 
              "species_diversity_shannon", "species_abundance")
response_vars <- c(soil_vars, veg_vars)
response_labels <- c("pH", "TOC (%)", "N (%)", "Ah depth (cm)", 
                     "Vegetation cover (%)", "Species richness", "Root depth (cm)", 
                     "Root abundance", "Shannon Diversity", "Species Abundance")

# Create correlation table
cor_table <- tibble()

for (g in 1:length(geomorph_vars)) {
  gv <- geomorph_vars[g]
  glab <- geomorph_labels[g]
  
  for (r in 1:length(response_vars)) {
    rv <- response_vars[r]
    rlab <- response_labels[r]
    
    lime <- df %>% filter(lithology == "Limestone") %>% drop_na(!!sym(gv), !!sym(rv))
    meta <- df %>% filter(lithology == "Metamorphic") %>% drop_na(!!sym(gv), !!sym(rv))
    
    lime_cor <- cor.test(lime[[gv]], lime[[rv]], method = "spearman", exact = FALSE)
    meta_cor <- cor.test(meta[[gv]], meta[[rv]], method = "spearman", exact = FALSE)
    
    cor_table <- bind_rows(cor_table, tibble(
      Geomorph = glab,
      Response = rlab,
      Lime_rho = round(lime_cor$estimate, 2),
      Lime_p = round(lime_cor$p.value, 4),
      Lime_sig = ifelse(lime_cor$p.value < 0.05, "*", ""),
      Meta_rho = round(meta_cor$estimate, 2),
      Meta_p = round(meta_cor$p.value, 4),
      Meta_sig = ifelse(meta_cor$p.value < 0.05, "*", "")
    ))
  }
}

print(cor_table, n = 40)


### SCATTERPLOTS ####
soil_plots <- list()
idx <- 1

for (g in 1:length(geomorph_vars)) {
  gv <- geomorph_vars[g]
  glab <- geomorph_labels[g]
  
  for (s in 1:length(soil_vars)) {
    sv <- soil_vars[s]
    slab <- response_labels[s]
    
    # Get correlations from table
    row <- cor_table %>% filter(Geomorph == glab, Response == slab)
    annot <- sprintf("Lime: ρ=%.2f%s\nMeta: ρ=%.2f%s",
                     row$Lime_rho, row$Lime_sig, row$Meta_rho, row$Meta_sig)
    
    soil_plots[[idx]] <- ggplot(df, aes(x = .data[[gv]], y = .data[[sv]], color = lithology)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
      scale_color_manual(values = lith_colors) +
      labs(x = glab, y = slab) +
      annotate("text", x = max(df[[gv]], na.rm = TRUE), y = max(df[[sv]], na.rm = TRUE),
               label = annot, hjust = 1, vjust = 1, size = 2) +
      theme_minimal(base_family = "sans", base_size = 14) +
      theme(legend.position = "none")
    
    idx <- idx + 1
  }
}

soil_combined <- grid.arrange(grobs = soil_plots, ncol = 4)
ggsave("geomorph_vs_soil_by_lithology.png",
       soil_combined, width = 14, height = 12, dpi = 300)

veg_plots <- list()
idx <- 1

for (g in 1:length(geomorph_vars)) {
  gv <- geomorph_vars[g]
  glab <- geomorph_labels[g]
  
  for (v in 1:length(veg_vars)) {
    vv <- veg_vars[v]
    vlab <- response_labels[v + 4]  # offset by 4 soil vars
    
    # Get correlations from table
    row <- cor_table %>% filter(Geomorph == glab, Response == vlab)
    annot <- sprintf("Lime: ρ=%.2f%s\nMeta: ρ=%.2f%s",
                     row$Lime_rho, row$Lime_sig, row$Meta_rho, row$Meta_sig)
    
    veg_plots[[idx]] <- ggplot(df, aes(x = .data[[gv]], y = .data[[vv]], color = lithology)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
      scale_color_manual(values = lith_colors) +
      labs(x = glab, y = vlab) +
      annotate("text", x = max(df[[gv]], na.rm = TRUE), y = max(df[[vv]], na.rm = TRUE),
               label = annot, hjust = 1, vjust = 1, size = 2) +
      theme_minimal(base_family = "sans", base_size = 14) +
      theme(legend.position = "none")
    
    idx <- idx + 1
  }
}

veg_combined <- grid.arrange(grobs = veg_plots, ncol = 6)
ggsave("C:/Users/Zaar/OneDrive - UvA (1)/Thesis 2/Version2/geomorph_vs_vegetation_by_lithology.png",
       veg_combined, width = 14, height = 16, dpi = 300)
