# Calculate Grass Cover and Threshold Dominance per WUI unit

# Load Libraries
library(terra)

# File Paths 
wui_path   <- "C:/Users/Sk Abidur Rahman/Documents/ArcGIS/Projects/AHP/WUI/WUI_Polys.shp"
grass_path <- "C:/UHM/Thesis/fuel continuity_proximity/unmix_grass_2020_2023.tif"
out_dir    <- "C:/UHM/Thesis/Grass_Cover"


# Load Data
wui   <- vect(wui_path)
grass <- rast(grass_path)

# Spatial Alignment 
wui <- project(wui, crs(grass))

# Crop the grass raster to the WUI 
grass_cropped <- crop(grass, wui)

# Extract Average Grass Cover
# fun=mean calculates the average fractional grass value per WUI
grass_mean   <- extract(grass_cropped, wui, fun = mean,   na.rm = TRUE)
grass_median <- extract(grass_cropped, wui, fun = median, na.rm = TRUE)

# Assign the results back to WUI attribute table
# Column 2 contains the actual values from the extraction
# use [, 2]:Keep all rows, but only look at the 2nd column.
# multiply by 100 to turn a decimal (0.50) into a percentage (50%).
# This represents the 'Spatial Dominance' or the total area covered by grass.
wui$grass_mean   <- grass_mean[, 2]
wui$grass_median <- grass_median[, 2]

# Calculate Dominance (Threshold)
# define "Dominance" as pixels where grass represents > 50% of the pixel
threshold <- 0.50
grass_binary <- grass_cropped > threshold

# Extract the mean of the binary raster (this gives proportion)
prop_dom <- extract(grass_binary, wui, fun = mean, na.rm = TRUE)

# Convert proportion (0-1) to a percentage (0-100) 
wui$pct_dom_grass <- prop_dom[, 2] * 100


# Export as a Shapefile and a CSV 
writeVector(wui, file.path(out_dir, "WUI_Grass_Metrics_Final.shp"), overwrite = TRUE)
write.csv(as.data.frame(wui), file.path(out_dir, "WUI_Grass_Summary.csv"), row.names = FALSE)

