library(terra)

# Set year to process
year <- 2025

# Directory where monthly NDVI stacks are saved
base_dir <- "C:/UHM/Thesis/NDVI/HCDP_data/ndvi_modis/day/statewide/data_map/2025"

# Create list of 12 monthly file paths (assuming you already created these monthly stacks)
monthly_files <- file.path(base_dir, sprintf("NDVI_Monthly_Stack_%d_%02d.tif", year, 1:6))

# Check which monthly files actually exist
monthly_files <- monthly_files[file.exists(monthly_files)]

# Stop if not all 12 months are present
if (length(monthly_files) < 6) {
  warning(sprintf("Only %d monthly NDVI files found for year %d. Some months may be missing.", length(monthly_files), year))
}

# Load all available monthly rasters and stack
monthly_stacks <- lapply(monthly_files, rast)
annual_stack <- rast(monthly_stacks)

# Compute mean NDVI across 12 months
annual_ndvi <- app(annual_stack, mean, na.rm = TRUE)

# Save the annual raster
output_file <- file.path(base_dir, sprintf("NDVI_Annual_Mean_%d.tif", year))
writeRaster(annual_ndvi, output_file, overwrite = TRUE)

# Optional: Plot
plot(annual_ndvi, main = paste("Mean Annual NDVI -", year))
