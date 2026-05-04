# ##################################################################
# Conditional Latin Hypercube Sampling (cLHS) for Plot Selection
# ##################################################################
# This script is an adjustment to the original script cLHS_Martell.R 
# shared by Proffessor A. Temme. It uses conditional Latin Hypercube 
# sampling to select field sampling locations that cover variation 
# in slope, NDVI, and TWI across the Sulden glacier foreland. It is 
# different from its original file in that it selects sampling
# locations for one glacier instead of two. 
#
# 1. Selects 60 high priority and 20 low priority sampling
#    locations using cLHS
# 2. Exports selected locations to CSV
# 3. Checks distribution of terrain variables for the final
#     sampled plots
#
# Input: data_sulden_v1.xlsx
#        PointstableToExcel.xlsx
# Output: sampling_locations.csv
# ##################################################################

library(clhs)
library(readxl)

### 1. Load data and run cLHS ###

data <- read_excel("data_sulden_v1.xlsx")

dA <- data.frame(data$Slope, data$NDVI, data$TWI)

set.seed(123)
selected_high_prio <- clhs(dA, size = 60, iter = 2000, progress = FALSE, simple = TRUE)
selected_low_prio <- clhs(dA, size = 20, iter = 2000, progress = FALSE, simple = TRUE)

### 2. Extract and export selected locations ###

highAdata <- df1[selected_high_prio, ]
highAdata$name <- "high prio A"

lowAdata <- df1[selected_low_prio, ]
lowAdata$name <- "low prio A"

selected_locations <- rbind(highAdata, lowAdata)

write.csv(selected_locations[, c(4, 5, 6, 8, 10, 12, 13)],
          file = "sampling_locations.csv")

### 3. Check final sampled plots ###

points <- read_excel("PointstableToExcel.xlsx")