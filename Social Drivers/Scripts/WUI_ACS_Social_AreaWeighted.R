##############
# AREA-WEIGHTED ACS SOCIAL VARIABLES -> WUI POLYGONS 
# Output: WUI shapefile + CSV with tract-based social variables summarized per WUI
# Method: area-weighted mean using tract-WUI intersection
############

library(sf)
library(dplyr)
library(units)

# PATHS 
wui_path <- "C:/Users/Sk Abidur Rahman/Desktop/UHM_Wildfire_Vulnerbility_Analysis-/Scoial Drivers/Data/WUI/WUI_Polys.shp"

out_dir  <- "C:/Users/Sk Abidur Rahman/Desktop/UHM_Wildfire_Vulnerbility_Analysis-/Scoial Drivers/Outputs"
out_shp  <- file.path(out_dir, "WUI_ACS_Social_AreaWeighted.shp")
out_csv  <- file.path(out_dir, "WUI_ACS_Social_AreaWeighted.csv")

# READ WUI 
wui <- st_read(wui_path, quiet = TRUE)
View(wui)

# CHECK hi_clean get from previous data extraction
# hi_clean an sf object (ACS polygons) with:
# GEOID, geometry, and cleaned variables like pct_poverty, med_hh_income, etc.
stopifnot(exists("hi_clean"))
stopifnot(inherits(hi_clean, "sf"))
View(hi_clean)

# PROJECT, CRS
# Use NAD83 / UTM Zone 4N for correct area weighting
target_crs <- 26904

wui_utm     <- st_transform(wui, target_crs)
hi_clean_utm <- st_transform(hi_clean, target_crs)

# FIX GEOMETRIES 
wui_utm      <- st_make_valid(wui_utm)
hi_clean_utm <- st_make_valid(hi_clean_utm)

# MAKE A UNIQUE WUI ID: so join is stable
wui_utm$WUI_Riskar <- seq_len(nrow(wui_utm))

# INTERSECT: TRACTS x WUI 
# This creates pieces where each piece has tract attributes + belongs to 1 WUI
int <- st_intersection(
  hi_clean_utm %>% select(GEOID, everything()),
  wui_utm %>% select(WUI_Riskar)
)

# If intersection is empty, CRS/extents are wrong
if (nrow(int) == 0) stop("Intersection is empty. Check CRS and that layers overlap.")

# COMPUTE AREA WEIGHTS 
# Weight = intersection_area / total_area_of_that_WUI
int$int_area <- st_area(int)

wui_area <- wui_utm %>%
  st_drop_geometry() %>%
  mutate(wui_area = st_area(wui_utm))

# join WUI total area back
int <- int %>%
  left_join(wui_area %>% select(WUI_Riskar, wui_area), by = "WUI_Riskar") %>%
  mutate(wt = as.numeric(int_area / wui_area))

# AREA-WEIGHTED SUMMARY PER WUI 
# Choose which variables to transfer from ACS to WUI
vars <- c(
  "total_pop",
  "med_hh_income",
  "pct_poverty",
  "pct_unemployed",
  "pct_less_hs",
  "pct_bachelors_plus",
  "pct_disability",
  "pct_renter",
  "pct_no_vehicle",
  "pct_no_internet",
  "pct_under5",
  "pct_65plus"
)

# check columns exist
missing_vars <- setdiff(vars, names(int))
if (length(missing_vars) > 0) {
  stop(paste("These variables are missing in hi_clean/int:", paste(missing_vars, collapse=", ")))
}

# Weighted mean function 
wmean <- function(x, w) {
  if (all(is.na(x))) return(NA_real_)
  sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}

wui_social <- int %>%
  st_drop_geometry() %>%
  group_by(WUI_Riskar) %>%
  summarise(
    across(all_of(vars), ~ wmean(.x, wt)),
    .groups = "drop"
  )

# JOIN BACK TO WUI + EXPORT 
wui_out <- wui_utm %>%
  left_join(wui_social, by = "WUI_Riskar")

# Write shapefile
st_write(wui_out, out_shp, delete_dsn = TRUE, quiet = TRUE)

# Write CSV 
write.csv(st_drop_geometry(wui_out), out_csv, row.names = FALSE)

cat("DONE!\n")
cat("Shapefile:", out_shp, "\n")
cat("CSV:", out_csv, "\n")
cat("Rows (WUI):", nrow(wui_out), "\n")
 