# Load libraries
library(terra)
library(dplyr)

# Step 1: Load inputs
dem <- rast("C:/UHM/Thesis/Slope/HI_DEM_10m_clean_no_niiahu.tif")
land_mask <- rast("C:/UHM/Thesis/Slope/max_current_fire_prob_annual_statewide_waterMasked.tif")
wui <- vect("C:/Users/Sk Abidur Rahman/Documents/ArcGIS/Projects/AHP/WUI/WUI_Polys.shp")

# Step 2: Calculate slope (in degrees)
slope <- terrain(dem, v = "slope", unit = "degrees")

# Step 3: Calculate slope roughness (standard deviation in 3x3 window)
window <- matrix(1, 3, 3)
slope_roughness <- focal(slope, w = window, fun = sd, na.rm = TRUE)

# Step 4: Match CRS of land mask with slope roughness
land_mask <- project(land_mask, crs(slope_roughness))

# Step 5: Resample land mask and mask slope roughness
land_mask_resampled <- resample(land_mask, slope_roughness, method = "near")
slope_roughness_masked <- mask(slope_roughness, land_mask_resampled)

# Step 6: Assign WUI IDs and extract roughness per polygon
wui$WUI_ID <- 1:nrow(wui)
roughness_values <- extract(slope_roughness_masked, wui, fun = mean, na.rm = TRUE)

# Step 7: Join extracted values back to WUI
names(roughness_values)[2] <- "mean_slope_roughness"
wui$mean_slope_roughness <- roughness_values$mean_slope_roughness

# Step 8: Save updated WUI with slope roughness
writeVector(wui, "C:/UHM/Thesis/Slope/WUI_with_Slope_Roughness.shp", overwrite = TRUE)
write.csv(as.data.frame(wui), "C:/UHM/Thesis/Slope/WUI_Slope_Roughness.csv", row.names = FALSE)

# Optional: Plot result
plot(slope_roughness_masked, main = "Slope Roughness (Standard Deviation)")
