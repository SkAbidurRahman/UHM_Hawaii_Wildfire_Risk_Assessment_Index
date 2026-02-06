# Package
library(sf)      # read + work with spatial data
library(dplyr)   # data wrangling
library(readr)   # write_csv()

# Input data
wui_path   <- "C:/Users/Sk Abidur Rahman/Documents/ArcGIS/Projects/AHP/WUI/WUI_Polys.shp"
fire_path  <- "C:/UHM/Thesis/Ignition_density/2022_1999_Hawaii_Large_Fire_Perimeters_UH_NREM/2022_1999_Hawaii_Large_Fire_Perimeters_UH_NREM/fires_1999_2022.shp"

# study window in years
total_years <- 24

# Outputs
dir.create("C:/UHM/Thesis/Fire_Response")


# Read shapefiles
wui   <- st_read(wui_path)
fires <- st_read(fire_path)

# WUI ID field is WUI_Riskar
names(wui)  # check quickly
names(fires)


# both layers in the same projected CRS (EPSG:6933) & areas are in m²
wui_6933   <- st_transform(wui,   6933)
fires_6933 <- st_transform(fires, 6933)


# Compute WUI area in m²
wui_6933 <- wui_6933 |>
  mutate(
    WUI_Area_m2 = as.numeric(st_area(geometry))  # area of each WUI polygon
  )


# Intersect fires with WUI polygons:Each row = piece of a fire perimeter inside a WUI
fire_wui_int <- st_intersection(
  wui_6933 |> select(WUI_Riskar),   # keep only WUI ID + geometry
  fires_6933 |> select(UH_ID, Year, area_ha) # keep fire ID + info
)

# area of the burned piece inside that WUI (m²)
fire_wui_int <- fire_wui_int |>
  mutate(
    BurnArea_m2 = as.numeric(st_area(geometry))
  )


# Summarize per WUI + fire #avoid duplicates for a fire
per_fire_wui <- fire_wui_int |> #Total burned area for each WUI–fire combination
  st_drop_geometry() |>
  group_by(WUI_Riskar, UH_ID) |>
  summarise(
    BurnArea_m2 = sum(BurnArea_m2, na.rm = TRUE),
    .groups = "drop"
  )

# sum over fires to get total burned area and fire count per WUI
fire_summary <- per_fire_wui |>
  group_by(WUI_Riskar) |>
  summarise(
    SUM_BurnArea_m2 = sum(BurnArea_m2, na.rm = TRUE), # total burned area
    COUNT_FIRE_UID  = n(),                            # number of fires
    .groups = "drop"
  )


# Join fire summary back to WUI layer # calculate BP_prop, Freq_perYr, FRI_years
wui_response <- wui_6933 |>
  left_join(fire_summary, by = "WUI_Riskar") |>
  # Replace NAs (no fire in that WUI) with 0
  mutate(
    SUM_BurnArea_m2 = ifelse(is.na(SUM_BurnArea_m2), 0, SUM_BurnArea_m2),
    COUNT_FIRE_UID  = ifelse(is.na(COUNT_FIRE_UID),  0, COUNT_FIRE_UID)
  ) |>
  # Response variables
mutate(
  # Burn Probability / Proportion of WUI burned
  BP_prop   = SUM_BurnArea_m2 / WUI_Area_m2,
  
  # Fire frequency (fires per year)
  Freq_perYr = COUNT_FIRE_UID / total_years,
  
  # Fire Return Interval (years per fire)
  FRI_years = ifelse(
    COUNT_FIRE_UID > 0,
    total_years / COUNT_FIRE_UID,
    NA_real_     # NA when there were no fires
  )
)


# Check attribute-only table
wui_response_tbl <- wui_response |>
  st_drop_geometry() |>
  select(
    WUI_Riskar,
    WUI_Area_m2,
    SUM_BurnArea_m2,
    BP_prop,
    COUNT_FIRE_UID,
    Freq_perYr,
    FRI_years
  )

head(wui_response_tbl)


# Save/write response table per WUI as CSV 
write_csv(
  wui_response_tbl,
  "C:/UHM/Thesis/Fire_Response/wui_fire_response.csv"
)

# save as spatial file (GeoPackage, shapefile)
st_write(wui_response, ("C:/UHM/Thesis/Fire_Response/wui_fire_response.gpkg"))
# layer = "wui_response", delete_layer = TRUE)

