



# General settings -------------------------------------------------------------

# Global R setup script
source("./src/global.R")

# Functions
srcPath  <- "C:/Users/taner/OneDrive - Stichting Deltares/_DELTARES/_WS/GITHUB/my-repo/R/"

source(paste0(srcPath, "ggQQPlot.R"))
source(paste0(srcPath, "lmPvalue.R"))  
source(paste0(srcPath, "ggECDF.R")) 


# Variable labels
climvar_label <- list(
  precip = expression("Precipitation (mm)"),
  tasavg = expression("Avg. Temperature (" * degree * C *")")
)

climdata1_col <- "steelblue"
climdata2_col <- "firebrick"

################################################################################

# Observed Data ----------------------------------------------------------------


# Observed ground metereological data: 
# date, location, precip, tasmax, tasmin, tasavg
ground_data_tidy <- read_csv("./area/Bhutan/data/station_data_tidy.csv")
ground_metadata  <- read_csv("./area/Bhutan/data/station_metadata.csv")  

# Daily data
ground_data_daily <- ground_data_tidy

# Monthly data
ground_data_monthly <- ground_data_daily %>%
  group_by(yr = year(date), mon = month(date), location) %>% 
  summarise(precip = sum(precip, na.rm = TRUE), 
            tasmax = mean(tasmax, na.rm = TRUE), 
            tasmin = mean(tasmin, na.rm = TRUE),
            tasavg = mean(tasavg, na.rm = TRUE)) %>%
  unite("date", yr:mon, sep="-") %>% 
  mutate(date = as.Date(paste0(date,"-01"))) %>%
  ungroup()  

# Annual data
ground_data_annual <- ground_data_daily %>%
  group_by(yr = year(date), location) %>% 
  summarise(precip = sum(precip, na.rm = TRUE), 
            tasmax = mean(tasmax, na.rm = TRUE), 
            tasmin = mean(tasmin, na.rm = TRUE),
            tasavg = mean(tasavg, na.rm = TRUE))  %>%
  rename(date = yr) %>% 
  mutate(date = as.Date(paste0(date,"-01-01"))) %>%
  ungroup()  

  
ground_data_list <- list(
  daily = ground_data_daily,
  monthly = ground_data_monthly,
  annual = ground_data_annual
)

################################################################################

# GCM Projections ---------------------------------------------------------

gcm_hist_tidy <- readRDS("./area/Bhutan/data/gcm_historical_tidy.rds")

################################################################################

# Gridded datasets  ------------------------------------------------------------

# ERA5
era5_daily <- read_csv("./area/Bhutan/data/era5_daily_tidy_updated.csv")  

# Chirps
chirps_daily <- read_csv("./area/Bhutan/data/chirps_daily_tidy.csv")  

################################################################################

