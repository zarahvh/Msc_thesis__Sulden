# Msc thesis
# Soil and vegetation development on two different lithologies in the proglacial area of the Sulden Glacier, Italy

## Overview

This repository contains the R code, data, and GIS files for the MSc thesis by Zarah van Hout on soil and vegetation development on two different lithologies in the proglacial area of the Sulden Glacier, Italy.

## Repository structure

```
├── README.md
├── data/
│   ├── Sulden_data.xlsx
│   ├── clhs_data.xlsx
│   └── PointstableToExcel.xlsx
├── scripts/
│   ├── cLHS.R
│   ├── Age_by_lithology.R
│   ├── Linear_regression_age.R
│   ├── Geo_by_lithology.R
│   ├── Disturbance_by_lithology.R
│   ├── Isopam_nmds.R
│   └── NMDS_analysis.R
├── gis/
│   ├── GIS_package.aprx 
```

## Data

| File | Description |
|------|-------------|
| `Sulden_data.xlsx` | Complete dataset containing plot metadata, topographic variables (aspect, slope, TWI, SPI, elevation), terrain age, lithology, geomorphic disturbance classification, lab results (pH, TOC, N), soil properties (Ah depth, root depth, root abundance), vegetation properties (richness, abundance, Shannon diversity, cover), individual species cover for 38 species, and ISOPAM vegetation class assignments with age classes ||
| `clhs_data.xlsx` | Raster-extracted terrain variables (slope, NDVI, TWI, age) used for conditional Latin Hypercube sampling prior to fieldwork |
| `PointstableToExcel.xlsx` | Final 53 sampled plot locations with terrain variables |

### GIS data

The ArcGIS Pro project file contains the following layers:

| Layer | Description |
|-------|-------------|
| `samples_sulden_final` | Final 53 sampling plot locations |
| `fieldwork_area` | Study area boundary of the Sulden glacier foreland |
| `GlacierOutlines` | Flacier extent outlines used for terrain age determination |
| `Buffers_Merge` | Buffer zones around sampling locations |
| `2023_dem_data` | Digital Elevation Model (2023) |
| `2006_dem_data` | Digital Elevation Model (2006) |
| `DigitalElevationModel-2_5m.tif` | 2.5m resolution DEM |
| `NDVI_clip` | NDVI raster clipped to the study area |
| `AgeRasterFinal` | Raster of terrain age derived from glacier outlines |

## Scripts

| Script | Description |
|--------|-------------|
| `cLHS.R` | Conditional Latin Hypercube sampling for plot selection (run prior to fieldwork) | 
| `Age_by_lithology.R` | Comparing soil and vegetation properties between lithologies | 
| `Linear_regression_age.R` | Linear regression of terrain age vs properties by lithology, including outlier diagnostics and Cook's distance analysis | 
| `Geo_by_lithology.R` | Relationships between topographic variables and soil/vegetation properties by lithology |
| `Disturbance_by_lithology.R` | ANCOVA separating effects of age, lithology, and disturbance; Kruskal-Wallis tests; age x disturbance scatterplots | 
| `Isopam_nmds.R` | ISOPAM classification, indicator species analysis, NMDS ordination with environmental fitting, and property boxplots by vegetation class |
| `NMDS_analysis.R` | Post-hoc analysis of successional trajectories by lithology and disturbance | `

## GIS workflow

Geospatial analysis was performed in ArcGIS Pro. The workflow included:

1. **Terrain age determination:** Terrain age was determined from combined glacier extent outlines from the years 1818, 1927, 1945, 1969, 1989, 1997, 2005, 2013, 2016 and 2021, which were converted to point features and interpolated to create a continuous age raster. 
2. **Sampling design:** Terrain variables were extracted at raster cell level for conditional Latin Hypercube sampling
3. **Geomorphometric variables:** Slope, plan and profile curvature, NDVI, TWI, and SPI were derived from the DEM at 3x3m resolution
4. **Lithology assignment:** Each plot was assigned a lithology (limestone, metamorphic, or mixed) based on the geological map
5. **Map production:** Study area, lithology, vegetation class, and disturbance maps were produced 


## R packages used

`tidyverse`, `readxl`, `vegan`, `isopam`, `indicspecies`, `gridExtra`, `ggpubr`, `ggrepel`, `flextable`, `officer`, `writexl`, `car`, `clhs`
