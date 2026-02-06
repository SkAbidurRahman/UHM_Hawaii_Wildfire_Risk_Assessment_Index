library(terra)
library(sf)
library(dplyr)

# Load data
wui_path    <- "C:/Users/Sk Abidur Rahman/Documents/ArcGIS/Projects/AHP/WUI/WUI_Polys.shp"
base_dir    <- "C:/UHM/Thesis/Drought/"
output_dir  <- "C:/UHM/Thesis/Drought"
years       <- 2000:2024 

# Load WUI and define Hawaii extent for rasterization
wui <- vect(wui_path)
hi_ext <- ext(-161, -154, 18, 23) # Longitude and Latitude for Hawaii
template_rast <- rast(hi_ext, res = 0.005, crs = "EPSG:4326") # ~500m resolution

# Weekly to annual processing loop
annual_any_list <- list() #loop processes weekly shapefiles into annual week-counts
annual_sev_list <- list()

# Processing year by year
for (yr in years) {
  print(paste("Processing Year:", yr))
  
  # Find all weekly shapefiles for the year
  year_dir <- file.path(base_dir, paste0(yr, "_DM_unzips"))
  week_files <- list.files(year_dir, pattern = "\\.shp$", full.names = TRUE)
  
  # If the folder is empty or missing, skip to the next year
  if(length(week_files) == 0) {
    print(paste("No data found for", yr))
    next
  }
  
  # Lists to hold binary weekly rasters
  weekly_any_bin <- list()
  weekly_sev_bin <- list()
  
  # Process every week
  for (wf in week_files) {
    # Load and rasterize the DM column (values 0-5)
    shp <- vect(wf)
    # DM column is numeric
    r_dm <- rasterize(shp, template_rast, field = "DM", fun = "max", background = 0)
    
    # Any Drought (D0-D4) -> Categories 1-5 => 1
    r_any <- r_dm >= 1 
    
    # Severe or Worse (D2-D4) -> Categories 3-5 => 1 
    r_sev <- r_dm >= 3  # (drawing "3-5 => 1" refers to D2, D3, D4)
    
    weekly_any_bin[[length(weekly_any_bin) + 1]] <- r_any
    weekly_sev_bin[[length(weekly_sev_bin) + 1]] <- r_sev
  }
  
  # Sum weeks for the current year
  annual_any_list[[as.character(yr)]] <- app(rast(weekly_any_bin), sum, na.rm = TRUE)
  annual_sev_list[[as.character(yr)]] <- app(rast(weekly_sev_bin), sum, na.rm = TRUE)
}

# Calculate longterm mean annual weeks
# Stack all years and take the mean per pixel
mean_annual_weeks_any <- app(rast(annual_any_list), mean, na.rm = TRUE)
mean_annual_weeks_sev <- app(rast(annual_sev_list), mean, na.rm = TRUE)

# Save the pixel-level results
writeRaster(mean_annual_weeks_any, file.path(output_dir, "Mean_Annual_Weeks_Any_Drought.tif"), overwrite=TRUE)
writeRaster(mean_annual_weeks_sev, file.path(output_dir, "Mean_Annual_Weeks_Severe_Drought.tif"), overwrite=TRUE)

# Spatial extraction for WUI
# Check CRS matches
wui <- project(wui, crs(mean_annual_weeks_any))

# Extract spatial mean for each WUI polygon
wui_stats <- extract(c(mean_annual_weeks_any, mean_annual_weeks_sev), wui, fun = mean, na.rm = TRUE)

# Assign names and join to WUI 
wui$avg_weeks_any_drought <- wui_stats[, 2]
wui$avg_weeks_sev_drought <- wui_stats[, 3]

# Save output
writeVector(wui, file.path(output_dir, "WUI_Drought_Analysis_Final.shp"), overwrite = TRUE)
write.csv(as.data.frame(wui), file.path(output_dir, "WUI_Drought_Summary.csv"), row.names = FALSE)

# Plot results
plot(mean_annual_weeks_any, main = "Mean Annual Weeks in Any Drought (D0+)")
plot(wui, add = TRUE, border = "black")