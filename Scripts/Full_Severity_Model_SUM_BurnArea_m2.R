########################
# Modling Hawaiʻi Wildfire Risk Assessment (HRAI)
#
# What this script does:
# Build modeling table (SUM_BurnArea_m2 as response)
# Correlation + heatmap + pairs plot
# PCA (scree + biplot)
# Fit Poisson GLMM with OLRE (glmmTMB)
# Predict expected ignitions
# Summarize predictions by WUI (spatial unit)
# Export WUI summary + Top10 high/low
# join to WUI polygons + rasterize to GeoTIFF
############################################################

rm(list = ls())
# PACKAGES
suppressPackageStartupMessages({
  library(glmmTMB)
  library(ggplot2)
  library(dplyr)
  library(sf) 
  library(terra)
})

# PATHS 
csv_path <- "C:/UHM/Thesis/Data_analysis/data/master_wildfire_dataset.csv"

# the polygon layer + reference raster
wui_shp_path  <- "C:/UHM/Thesis/Wind_Speed/WUI/WUI_Polys.shp"
ref_rast_path <- "C:/UHM/Thesis/fuel continuity_proximity/unmix_grass_2020_2023.tif"

out_dir <- "C:/UHM/Thesis/Data_analysis/Severity_Model"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Output files
cor_csv      <- file.path(out_dir, "predictor_correlations.csv")
cor_png      <- file.path(out_dir, "correlation_heatmap.png")
pairs_png    <- file.path(out_dir, "pairs_plot_predictors.png")
pca_scree_png <- file.path(out_dir, "pca_scree_plot.png")
pca_biplot_png <- file.path(out_dir, "pca_biplot.png")

coef_csv     <- file.path(out_dir, "glmm_model_coefficients.csv")
model_txt    <- file.path(out_dir, "full_model_summary.txt")

wui_sum_csv  <- file.path(out_dir, "WUI_vulnerability_summary.csv")
top10_hi_csv <- file.path(out_dir, "Top10_HighRisk_WUI.csv")
top10_lo_csv <- file.path(out_dir, "Top10_LowRisk_WUI.csv")

wui_shp_out  <- file.path(out_dir, "WUI_Vulnerability_GLMM.shp")
wui_tif_out  <- file.path(out_dir, "WUI_Vulnerability_GLMM.tif")

# LOAD CSV 
stopifnot(file.exists(csv_path))
dat <- read.csv(csv_path, check.names = FALSE)

cat("Rows, Cols:", nrow(dat), ncol(dat), "\n")

# COLUMNS NAME CHECK
required_cols <- c(
  "WUI_Riskar",
  "SUM_BurnArea_m2",
  "Wind_mph",
  "NDVI_mean",
  "mean_ann_rainfall_mm",
  "avg_weeks_any_drought",
  "avg_weeks_sev_drought",
  "mean_slope_roughness",
  "grass_mean",
  "pct_dom_grass",
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
  "pct_65plus",
  "Freq_perYr",
  "FRI_years",
  "Avg_Fuel_Value"
)

missing <- setdiff(required_cols, names(dat))
if (length(missing) > 0) {
  stop("Missing columns in CSV:\n", paste(missing, collapse = ", "))
}

# IMPORTANT SWITCH (LEAKAGE CHECK)
USE_FREQ_PERYR <- TRUE  


# MODELING DATAFRAME
mod <- data.frame(
  y       = as.integer(dat$SUM_BurnArea_m2),
  WUI_ID  = dat$WUI_Riskar,
  
  # Fire environment / fuels
  wind_mph   = as.numeric(dat$Wind_mph),
  ndvi       = as.numeric(dat$NDVI_mean),
  rain_mm    = as.numeric(dat$mean_ann_rainfall_mm),
  drought_any = as.numeric(dat$avg_weeks_any_drought),
  drought_sev = as.numeric(dat$avg_weeks_sev_drought),
  slope_rough = as.numeric(dat$mean_slope_roughness),
  grass_mean  = as.numeric(dat$grass_mean),
  pct_dom_grass = as.numeric(dat$pct_dom_grass),
  fuel_avg    = as.numeric(dat$Avg_Fuel_Value),
  
  # Fire regime metrics
  fri_years  = as.numeric(dat$FRI_years),
  freq_peryr = as.numeric(dat$Freq_perYr),
  
  # Social drivers 
  total_pop        = as.numeric(dat$total_pop),
  med_hh_income    = as.numeric(dat$med_hh_income),
  pct_poverty      = as.numeric(dat$pct_poverty),
  pct_unemployed   = as.numeric(dat$pct_unemployed),
  pct_less_hs      = as.numeric(dat$pct_less_hs),
  pct_bachelors_plus = as.numeric(dat$pct_bachelors_plus),
  pct_disability   = as.numeric(dat$pct_disability),
  pct_renter       = as.numeric(dat$pct_renter),
  pct_no_vehicle   = as.numeric(dat$pct_no_vehicle),
  pct_no_internet  = as.numeric(dat$pct_no_internet),
  pct_under5       = as.numeric(dat$pct_under5),
  pct_65plus       = as.numeric(dat$pct_65plus)
)

# If you don't want to use Freq_perYr due to leakage:
if (!USE_FREQ_PERYR) {
  mod$freq_peryr <- NULL
}

# Drop missing values
mod <- na.omit(mod)
cat("Rows after NA removal:", nrow(mod), "\n")


# SCALE PREDICTORS (mean=0, sd=1)
pred_cols <- setdiff(names(mod), c("y", "WUI_ID"))

for (nm in pred_cols) {
  mod[[paste0(nm, "_z")]] <- as.numeric(scale(mod[[nm]]))
}

# CORRELATION + HEATMAP + PAIRS
predictors <- mod[, pred_cols]
cor_matrix <- cor(predictors, use = "complete.obs")
write.csv(cor_matrix, cor_csv, row.names = TRUE)

# Heatmap
cor_long <- as.data.frame(as.table(cor_matrix))
names(cor_long) <- c("Var1", "Var2", "Correlation")

p_cor <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap (Predictors)", x = "", y = "")

ggsave(cor_png, p_cor, width = 11, height = 9, dpi = 300)

# Pairs plot
png(pairs_png, width = 1600, height = 1600, res = 200)
pairs(predictors,
      main = "Pairs Plot of Predictors",
      pch = 19,
      col = rgb(0, 0, 1, 0.25))
dev.off()


# PCA (Scree + Biplot)
pca_result <- prcomp(predictors, scale. = TRUE)

scree_data <- data.frame(
  PC = paste0("PC", seq_along(pca_result$sdev)),
  Variance = (pca_result$sdev)^2 / sum((pca_result$sdev)^2)
)

p_scree <- ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_col() +
  theme_minimal() +
  labs(title = "PCA Scree Plot", y = "Proportion of Variance", x = "")

ggsave(pca_scree_png, p_scree, width = 8, height = 5, dpi = 300)

png(pca_biplot_png, width = 1600, height = 1100, res = 200)
biplot(pca_result, main = "PCA Biplot", cex = 0.8)
dev.off()

# Poisson WITH OLRE
# OLRE handles overdispersion by giving each observation its own random effect.
mod$obs_id <- factor(seq_len(nrow(mod)))

scaled_cols <- paste0(pred_cols, "_z")
fixed_part  <- paste(scaled_cols, collapse = " + ")
form_full   <- as.formula(paste("y ~", fixed_part, "+ (1|obs_id)"))

m_null <- glmmTMB(y ~ 1 + (1|obs_id), family = poisson(), data = mod)
m_full <- glmmTMB(form_full, family = poisson(), data = mod)

cat("\nAIC comparison:\n")
print(AIC(m_null, m_full))

# Save coefficients (Relative Risk = exp(beta))
coef_table <- as.data.frame(summary(m_full)$coefficients$cond)
coef_table$Relative_Risk <- exp(coef_table$Estimate)
write.csv(coef_table, coef_csv, row.names = TRUE)

sink(model_txt)
print(summary(m_full))
sink()


# PREDICT + WUI SUMMARY
mod$pred <- predict(m_full, newdata = mod, type = "response")

wui_summary <- mod %>%
  group_by(WUI_ID) %>%
  summarise(
    pred_mean  = mean(pred, na.rm = TRUE),
    pred_total = sum(pred, na.rm = TRUE),
    obs_total  = sum(y, na.rm = TRUE),
    n_obs      = n(),
    .groups = "drop"
  )

write.csv(wui_summary, wui_sum_csv, row.names = FALSE)

top10_hi <- wui_summary %>% arrange(desc(pred_mean)) %>% slice(1:10)
top10_lo <- wui_summary %>% arrange(pred_mean) %>% slice(1:10)

write.csv(top10_hi, top10_hi_csv, row.names = FALSE)
write.csv(top10_lo, top10_lo_csv, row.names = FALSE)

cat("\nTop10 High-risk saved:", top10_hi_csv, "\n")
cat("Top10 Low-risk saved:", top10_lo_csv, "\n")


# JOIN TO WUI SHAPEFILE + RASTERIZE
if (file.exists(wui_shp_path) && file.exists(ref_rast_path)) {
  
  wui_polys <- st_read(wui_shp_path, quiet = TRUE)
  
  # join summary back to polygons
  # NOTE: update "WUI_Riskar" if your shapefile uses a different ID field
  wui_joined <- wui_polys %>%
    left_join(wui_summary, by = c("WUI_Riskar" = "WUI_ID"))
  
  st_write(wui_joined, wui_shp_out, delete_dsn = TRUE, quiet = TRUE)
  
  ref_rast <- rast(ref_rast_path)
  
  # make CRS consistent
  wui_joined_r <- st_transform(wui_joined, crs(ref_rast))
  wui_vect <- vect(wui_joined_r)
  
  # rasterize predicted mean risk
  wui_risk_raster <- rasterize(
    wui_vect,
    ref_rast,
    field = "pred_mean",
    filename = wui_tif_out,
    overwrite = TRUE
  )
  
  plot(wui_risk_raster, main = "WUI Predicted Ignition Risk (pred_mean)")
  cat("\nWUI shapefile + GeoTIFF written to:\n", wui_shp_out, "\n", wui_tif_out, "\n")
} else {
  cat("\nSkipping shapefile join/rasterize (missing wui_shp_path or ref_rast_path).\n")
}

cat("\nDONE. Outputs in:", out_dir, "\n")
plot(wui_tif_out)

