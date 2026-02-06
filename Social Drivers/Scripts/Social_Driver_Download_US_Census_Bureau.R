###################
# ACS Social Drivers 
# Census API key
# Downloads wildfire-relevant social variables (ACS 5-year 2023)
#######

# PACKAGES 
pkgs <- c("tidycensus","dplyr","sf","readr")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if(length(to_install) > 0) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)

# PROJECT FOLDER 
proj_dir <- "C:/Users/Sk Abidur Rahman/Desktop/UHM_Wildfire_Vulnerbility_Analysis-/Scoial Drivers"

data_dir   <- file.path(proj_dir, "Data")
script_dir <- file.path(proj_dir, "Scripts")
out_dir    <- file.path(proj_dir, "Outputs")


# SET CENSUS API KEY 
my_key <- "c466402024f762869b162c4cb524ebedd1a75fc1"

# store key in your R environment 
# install=TRUE writes to .Renviron so it persists
census_api_key(my_key, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")  # reload key this session

# SETTINGS 
acs_year <- 2023   # ACS 5-year “2023” = 2019–2023
state_abbr <- "HI" # Hawaii

# Output file names
out_csv <- file.path(out_dir, paste0("HI_ACS_social_drivers_", acs_year, "_tract.csv"))
out_shp <- file.path(out_dir, paste0("HI_ACS_social_drivers_", acs_year, "_tract.shp"))

# VARIABLES 
# All are ACS 5-year tables; E = estimate

vars <- c(
  # population total
  total_pop = "B01003_001",
  
  # poverty
  pov_total = "B17001_001",
  pov_below = "B17001_002",
  
  # income
  med_hh_income = "B19013_001",
  
  # unemployment
  labor_force = "B23025_003",
  unemployed  = "B23025_005",
  
  # education (25+)
  edu_total_25plus = "B15003_001",
  # less than HS = B15003_002 ... B15003_016 (sum after download)
  edu_no_school    = "B15003_002",
  edu_elem         = "B15003_003",
  edu_mid          = "B15003_004",
  edu_9            = "B15003_005",
  edu_10           = "B15003_006",
  edu_11           = "B15003_007",
  edu_12_nodipl    = "B15003_008",
  edu_hs_grad      = "B15003_017",
  edu_bachelors    = "B15003_022",
  edu_gradprof     = "B15003_025",
  
  # age (children + elderly)
  pop_under5_m  = "B01001_003",
  pop_under5_f  = "B01001_027",
  pop_65_66_m   = "B01001_020",
  pop_67_69_m   = "B01001_021",
  pop_70_74_m   = "B01001_022",
  pop_75_79_m   = "B01001_023",
  pop_80_84_m   = "B01001_024",
  pop_85plus_m  = "B01001_025",
  pop_65_66_f   = "B01001_044",
  pop_67_69_f   = "B01001_045",
  pop_70_74_f   = "B01001_046",
  pop_75_79_f   = "B01001_047",
  pop_80_84_f   = "B01001_048",
  pop_85plus_f  = "B01001_049",
  
  # disability (simple and stable)
  dis_total = "C18108_001",
  dis_with  = "C18108_002",
  
  # renters
  occ_total = "B25003_001",
  renters   = "B25003_003",
  
  # no vehicle (evacuation constraint)
  veh_total  = "B25044_001",
  no_vehicle = "B25044_003",
  
  # internet access (info/warnings)
  net_total_hh     = "B28002_001",
  net_no_internet  = "B28002_013"
)

# DOWNLOAD (tract-level + geometry) 
# output="wide" gives columns like B01003_001E
hi_acs <- get_acs(
  geography = "tract",
  state = state_abbr,
  year = acs_year,
  survey = "acs5",
  variables = vars,
  geometry = TRUE,
  output = "wide"
)

# BUILD CLEAN INDICATORS (percents) 
hi_clean <- hi_acs %>%
  mutate(
    # population
    total_pop = total_popE,
    
    # age groups
    pop_under5 = pop_under5_mE + pop_under5_fE,
    pop_65plus =
      pop_65_66_mE + pop_67_69_mE + pop_70_74_mE + pop_75_79_mE +
      pop_80_84_mE + pop_85plus_mE +
      pop_65_66_fE + pop_67_69_fE + pop_70_74_fE + pop_75_79_fE +
      pop_80_84_fE + pop_85plus_fE,
    
    pct_under5 = ifelse(total_pop > 0, pop_under5 / total_pop * 100, NA_real_),
    pct_65plus = ifelse(total_pop > 0, pop_65plus / total_pop * 100, NA_real_),
    
    # poverty
    pct_poverty = ifelse(pov_totalE > 0, pov_belowE / pov_totalE * 100, NA_real_),
    
    # unemployment
    pct_unemployed = ifelse(labor_forceE > 0, unemployedE / labor_forceE * 100, NA_real_),
    
    # education: less than high school
    edu_less_hs =
      edu_no_schoolE + edu_elemE + edu_midE + edu_9E + edu_10E +
      edu_11E + edu_12_nodiplE,
    
    pct_less_hs = ifelse(
      edu_total_25plusE > 0,
      edu_less_hs / edu_total_25plusE * 100,
      NA_real_
    ),
    
    pct_bachelors_plus = ifelse(
      edu_total_25plusE > 0,
      (edu_bachelorsE + edu_gradprofE) / edu_total_25plusE * 100,
      NA_real_
    ),
    
    # disability
    pct_disability = ifelse(dis_totalE > 0, dis_withE / dis_totalE * 100, NA_real_),
    
    # housing
    pct_renter = ifelse(occ_totalE > 0, rentersE / occ_totalE * 100, NA_real_),
    
    # transportation
    pct_no_vehicle = ifelse(veh_totalE > 0, no_vehicleE / veh_totalE * 100, NA_real_),
    
    # internet
    pct_no_internet = ifelse(
      net_total_hhE > 0,
      net_no_internetE / net_total_hhE * 100,
      NA_real_
    )
  ) %>%
  transmute(
    GEOID, NAME,
    total_pop,
    med_hh_income = med_hh_incomeE,
    pct_poverty,
    pct_unemployed,
    pct_less_hs,
    pct_bachelors_plus,
    pct_disability,
    pct_renter,
    pct_no_vehicle,
    pct_no_internet,
    pct_under5,
    pct_65plus,
    geometry
  )


# EXPORT 
write_csv(st_drop_geometry(hi_clean), out_csv)
st_write(hi_clean, out_shp, delete_layer = TRUE)

cat("\nDONE \nSaved:\n",
    "CSV: ", out_csv, "\n",
    "SHP: ", out_shp, "\n")
