library(terra)

# Directory of annual NDVI rasters
annual_dir <- "C:/UHM/Thesis/NDVI/HCDP_data/ndvi_modis/day/statewide/data_map"

# Create file paths
annual_files <- file.path(annual_dir, sprintf("NDVI_Annual_Mean_%d.tif", 2001:2025))
annual_files <- annual_files[file.exists(annual_files)]

# Stack all years
ndvi_yearly_stack <- rast(annual_files)

# Save as multi-band raster
writeRaster(ndvi_yearly_stack, file.path(annual_dir, "NDVI_Annual_Stack_2001_2025.tif"), overwrite = TRUE)

plot(ndvi_yearly_stack)
