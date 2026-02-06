
# Load Libraries
library(terra)
library(sf)

# File Paths 
wui_path   <- "C:/UHM/Thesis/Wind_Speed/WUI/WUI_Polys.shp" 
wind_path  <- "C:/UHM/Thesis/Wind_Speed/USA_wind-speed_10m.tif"
out_dir    <- "C:/UHM/Thesis/Wind_Speed/Outputs"

# Import Data 
wui_all  <- st_read(wui_path)
usa_wind <- rast(wind_path)
usa_wind

# Coordinate Alignment: CRS (WGS84)
wui_aligned <- st_transform(wui_all, crs(usa_wind))

# Crop 
# Crop the USA raster to the Hawaii bounding box first.
hi_extent <- ext(wui_aligned)
hi_wind_box <- crop(usa_wind, hi_extent)

#  Mask 
# Remove all wind data outside of your specific 214 WUI polygons.
hi_wind_masked <- mask(hi_wind_box, wui_aligned)

# Zonal Statistics 
# Calculates the Mean 10m Wind Speed per WUI unit 
wind_stats <- terra::extract(hi_wind_masked, wui_aligned, fun = mean, na.rm = TRUE)

# Merge wind variables back into the attribute table 
wui_all$Wind_ms  <- wind_stats[,2]
wui_all$Wind_mph <- wind_stats[,2] * 2.237 # Conversion factor Meters per Second (m/s) to Miles per Hour (mph)

# Export
st_write(wui_all, file.path(out_dir, "Statewide_WUI_Wind_Final.shp"), delete_dsn = TRUE)
write.csv(st_drop_geometry(wui_all), file.path(out_dir, "Statewide_WUI_Wind_Final.csv"), row.names = FALSE)
