library(terra)

# -- Step 1: Compute Slope from DEM
# Load 10m DEM
dem <- rast("C:/UHM/Thesis/Slope/HI_DEM_10m_clean_no_niiahu.tif")

# Calculate slope in degrees
slope <- terrain(dem, v = "slope", unit = "degrees")

# --- Step 2: Mask Slope with Land Raster
# Load land mask 
land_mask <- rast("C:/UHM/Thesis/Slope/max_current_fire_prob_annual_statewide_waterMasked.tif")

# Resample land mask to match slope resolution and extent
land_mask_resampled <- resample(land_mask, slope, method = "near")

# Apply land mask
slope_masked <- mask(slope, land_mask_resampled)

# ---Step 3: Save & Plot Slope Raster
# Plot slope map
plot(slope_masked, main = "Slope (degrees) – Hawai‘i")

# Save slope raster
writeRaster(slope_masked, "C:/UHM/Thesis/Slope/slope_hawaii_10m.tif", overwrite = TRUE)

# Print mean slope across state
mean_slope <- global(slope_masked, mean, na.rm = TRUE)
print(mean_slope)


# =-- Step 4: Extract Mean Slope per WUI Polygon
# Load WUI polygons
wui <- vect("C:/Users/Sk Abidur Rahman/Documents/ArcGIS/Projects/AHP/WUI/WUI_Polys.shp")

# Reproject WUI to match raster CRS
wui <- project(wui, crs(slope_masked))

# Extract mean slope per polygon
slope_values <- extract(slope_masked, wui, fun = mean, na.rm = TRUE)

# Join extracted values back to WUI
wui$mean_slope <- slope_values[, 2]  # 2nd column = mean value


# --- Step 5: Export Results
# Save updated WUI shapefile with slope
writeVector(wui, "C:/UHM/Thesis/Slope/WUI_with_Slope.shp", overwrite = TRUE)

# Save extracted values as CSV
write.csv(as.data.frame(wui), "C:/UHM/Thesis/Slope/WUI_Mean_Slope.csv", row.names = FALSE)
