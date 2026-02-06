################################################################################
# THESIS: Hawaiʻi Wildfire Risk Assessment Index (WRAI)
# Objective: Extract Mean Annual Rainfall for all 214 Statewide WUI Units
################################################################################

library(terra)
library(sf)

# File PATHS & DIRECTORIES 
wui_path   <- "C:/Users/Sk Abidur Rahman/Documents/ArcGIS/Projects/AHP/WUI/WUI_Polys.shp"
base_rain_dir <- "C:/UHM/Thesis/Drought/HCDP_data_unzipped/rainfall/new/month/statewide/data_map"
out_dir       <- "C:/UHM/Thesis/Data/Output/Rainfall"
years         <- 2000:2024

if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Annual sum
annual_list <- list() #create annual totals for each year to get a "Mean Annual" value

for (yr in years) { # Construct path to specific year folder
  year_path <- file.path(base_rain_dir, yr)
  files <- list.files(year_path, pattern = "\\.tif$", full.names = TRUE) # Find all 12 monthly tif files for that year
  
  if(length(files) >= 12) {
    print(paste("Processing Year:", yr))
    # Sum 12 months to get Annual Total Rainfall for that year
    annual_list[[as.character(yr)]] <- app(rast(files), sum, na.rm = TRUE)
  }
}

# LONG-TERM MEAN: 25-YEAR AVERAGE
# Stack all annual totals and calculate the mean per pixel
print("Calculating long-term Mean Annual Rainfall")
mean_annual_rain_raster <- app(rast(annual_list), mean, na.rm = TRUE)

# Save the raster result
writeRaster(mean_annual_rain_raster, file.path(out_dir, "Mean_Annual_Rainfall_2000_2024.tif"), overwrite=TRUE)
plot(mean_annual_rain_raster)
# Spatial extraction for WUI
print("Loading WUI and extracting rainfall values...")
wui <- vect(wui_path)

# Check CRS: between Raster and Vector
wui <- project(wui, crs(mean_annual_rain_raster))

# Extract the spatial mean for each of the 214 WUI polygons
rain_stats <- extract(mean_annual_rain_raster, wui, fun = mean, na.rm = TRUE)

# Join to WUI attribute table
wui$mean_ann_rainfall_mm <- rain_stats[, 2]

# Save
writeVector(wui, file.path(out_dir, "WUI_Rainfall_Results.shp"), overwrite = TRUE)
write.csv(as.data.frame(wui), file.path(out_dir, "WUI_Rainfall_Summary.csv"), row.names = FALSE)

