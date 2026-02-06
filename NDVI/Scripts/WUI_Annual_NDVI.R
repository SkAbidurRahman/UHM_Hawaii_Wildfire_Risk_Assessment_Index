library(terra)
library(dplyr)

# Step 1: Load WUI shapefile 
wui <- vect("C:/Users/Sk Abidur Rahman/Documents/ArcGIS/Projects/AHP/WUI/WUI_Polys.shp")

# Step 2: Load stacked annual NDVI raster (2001–2025) 
ndvi_stack <- rast("C:/UHM/Thesis/NDVI/HCDP_data/ndvi_modis/day/statewide/data_map/NDVI_Annual_Stack_2001_2025.tif")

# Step 3: Reproject WUI to match NDVI CRS 
wui <- project(wui, crs(ndvi_stack))

# Step 4: Extract mean NDVI per year (per band) for each WUI polygon
ndvi_extracted <- extract(ndvi_stack, wui, fun = mean, na.rm = TRUE)

# Step 5: Join extracted NDVI back to WUI polygons
# The first column is ID; drop it when binding
wui_ndvi <- cbind(wui, ndvi_extracted[ , -1])

# Step 6: Assign proper band names (e.g., NDVI_2001 to NDVI_2025) 
names(wui_ndvi)[(ncol(wui) + 1):ncol(wui_ndvi)] <- paste0("NDVI_", 2001:2025)

# Step 7: Save output
writeVector(wui_ndvi, "C:/UHM/Thesis/NDVI/WUI_with_Annual_NDVI.shp", overwrite = TRUE)
write.csv(as.data.frame(wui_ndvi), "C:/UHM/Thesis/NDVI/WUI_Annual_NDVI.csv", row.names = FALSE)
