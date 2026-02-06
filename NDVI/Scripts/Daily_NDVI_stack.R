library(terra)

# Define year and month
year <- 2025
month <- 07
base_dir <- "C:/UHM/Thesis/NDVI/HCDP_data/ndvi_modis/day/statewide/data_map/2025/07"

# Generate file paths for days 1 to 31
ndvi_files <- file.path(base_dir, sprintf("ndvi_modis_day_statewide_data_map_%d_%02d_%02d.tif", year, month, 1:31))

# Filter files that exist AND are readable
ndvi_rasters <- list()
for (f in ndvi_files) {
  if (file.exists(f)) {
    tryCatch({
      r <- rast(f)
      ndvi_rasters[[length(ndvi_rasters) + 1]] <- r
    }, error = function(e) {
      warning(sprintf("File not readable: %s\nReason: %s", f, e$message))
    })
  }
}

# Stop if nothing could be loaded
if (length(ndvi_rasters) == 0) {
  stop("No valid NDVI raster files found for stacking.")
}

# Stack rasters
ndvi_monthly_stack <- rast(ndvi_rasters)

# Save stacked file
output_file <- file.path(base_dir, sprintf("NDVI_Monthly_Stack_%d_%02d.tif", year, month))
writeRaster(ndvi_monthly_stack, output_file, overwrite = TRUE)

# Plot first day
plot(ndvi_monthly_stack[[1]], main = paste("NDVI -", month, year))
