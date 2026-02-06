library(terra)

# Step 1: Load your NDVI raster
ndvi <- vect("C:/UHM/Thesis/NDVI/WUI_with_Annual_NDVI.shp")

# Step 2: Threshold NDVI to binary (e.g., NDVI > 0.25)
ndvi_bin <- classify(ndvi, matrix(c(-Inf, 0.25, 0, 0.25, Inf, 1), ncol = 3, byrow = TRUE))

# Step 3: Define 3x3 window
window <- matrix(1, 3, 3)

# Step 4: Custom function to calculate adjacency
adjacency_fun <- function(x) {
  center <- x[5]
  if (is.na(center) || center == 0) return(0)
  sum(x[-5] == center, na.rm = TRUE)
}

# Step 5: Apply focal
adj_index <- focal(ndvi_bin, w = window, fun = adjacency_fun, na.rm = TRUE)

# Step 6: Save or extract per WUI
writeRaster(adj_index, "NDVI_Adjacency_Index.tif", overwrite = TRUE)

# Extract per WUI if needed
wui <- vect("C:/Users/Sk Abidur Rahman/Documents/ArcGIS/Projects/AHP/WUI/WUI_Polys.shp")
wui$adj_index <- extract(adj_index, wui, fun = mean, na.rm = TRUE)[,2]
writeVector(wui, "WUI_with_Adjacency_Index.shp", overwrite = TRUE)
