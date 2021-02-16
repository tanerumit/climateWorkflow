

# OBSERVVED WEATHER STATION DATA -----------------------------------------------


base_dir <- 'C:/Users/taner/OneDrive - Stichting Deltares/_DELTARES/02 Projects/11206254 Bhutan/Data/RAW/'


weather_data_series <- read_xlsx(paste0(base_dir,"AllDataTogether.xlsx"))
weather_data_metadata <- read_xlsx(paste0(base_dir,"weather_stations_metadata.xlsx")) 

observed_precip <- weather_data_series %>% select(1, seq(2, 61, 3)) %>%
  mutate(Year = as.Date(Year, format = "%d-%m-%Y"))
colnames(observed_precip) <- unname(sapply(colnames(observed_precip), function(x) strsplit(x," ")[[1]][1]))
colnames(observed_precip)[1] <- "date" 
observed_precip %<>% 
  gather(key = location, value = precip, -date) %>%
  mutate(location = as.factor(location)) %>%
  mutate(location = fct_recode(location, "Mongar"  = "Monger"))

observed_tasmax <- weather_data_series %>% select(1, seq(3, 61, 3)) %>%
  mutate(Year = as.Date(Year, format = "%d-%m-%Y"))
colnames(observed_tasmax) <- unname(sapply(colnames(observed_tasmax), function(x) strsplit(x," ")[[1]][1]))
colnames(observed_tasmax)[1] <- "date" 
observed_tasmax %<>%  
  gather(key = location, value = tasmax, -date) %>%
  mutate(location = as.factor(location)) %>%
  mutate(location = fct_recode(location, "Mongar"  = "Monger"))

observed_tasmin <- weather_data_series %>% select(1, seq(4, 61, 3)) %>%
  mutate(Year = as.Date(Year, format = "%d-%m-%Y"))
colnames(observed_tasmin) <- unname(sapply(colnames(observed_tasmin), function(x) strsplit(x," ")[[1]][1]))
colnames(observed_tasmin)[1] <- "date" 
observed_tasmin %<>%  
  gather(key = location, value = tasmin, -date) %>%
  mutate(location = as.factor(location)) %>%
  mutate(location = fct_recode(location, "Mongar"  = "Monger"))

  
observed_weather_data_tidy <- observed_precip %>% 
  left_join(observed_tasmax) %>% 
  left_join(observed_tasmin) %>%
  mutate(tasmax = as.numeric(tasmax), tasmin = as.numeric(tasmin)) %>%
  mutate(tasavg = (tasmax + tasmin)/2)

weather_data_series_coverage <- observed_precip %>%
  na.omit() %>%
  group_by(location) %>%
  summarize(start = min(date), end = max(date), .groups = 'keep')

write_csv(x = weather_data_series_coverage, file = "./Bhutan/data/weather_data_series_coverage.csv")
write_csv(x = observed_weather_data_tidy, file = "./Bhutan/data/observed_weather_tidy.csv")
#write_csv(x = weather_data_metadata, file = "./Bhutan/data/observed_weather_series_metadata.csv")



################################################################################

# GCM DATA ---------------------------------------------------------------------


#mutate(date = sub("^[^_]*_([^_]*).*", "\\1", date)) %>%

base_dir <- 'C:/Users/taner/OneDrive - Stichting Deltares/_DELTARES/02 Projects/11206254 Bhutan/Data/RAW/GCM/'
climate_models <- c("CNRM-CM5", "IPSL-CM5A-LR", "MIROC5", "MPI-ESM-MR", "MRI-CGCM3")
station_subset <- c("Bhur", "Chamkhar", "Dagana", "Damphu", "Deothang", "Gasa", "Haa", "Kanglung", "Mongar", "Paro", "Pemagatshel", "Phuntsholing", "Punakha", "Simtokha", "Sipsu", "Tangmachu", "Trashiyangtse", "Trongsa", "Wangdue", "Zhemgang")
length(station_subset)

gcm_hist <- list()


for (i in 1:length(climate_models)) {
  
  model_name <- climate_models[i]
  model_dir  <- paste0(base_dir, model_name,"/")

  # Precipitation
  precip_files <- list.files(path = paste0(model_dir, "precipitation"))
  precip_hist  <- read_csv(paste0(model_dir,"precipitation/", precip_files[1]), skip = 1)[-1,] %>% 
    rename(Points = 1) %>%
    gather(key   = location, value = precip, -Points) %>%
    rename(date  = Points) %>%
    mutate(date  = as.Date(date, format = "%d/%m/%Y")) 
  
  # Precipitation
  tasmin_files <- list.files(path = paste0(model_dir, "tasmin"))
  tasmin_hist  <- read_csv(paste0(model_dir,"tasmin/", tasmin_files[1]), skip = 1)[-1,] %>% 
    rename(Points = 1) %>%
    gather(key = location, value = tasmin, -Points) %>%
    rename(date = Points) %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y")) 
    
  # Precipitation
  tasmax_files <- list.files(path = paste0(model_dir, "tasmax"))
  tasmax_hist  <- read_csv(paste0(model_dir,"tasmax/", tasmax_files[1]), skip = 1)[-1,] %>% 
    rename(Points = 1) %>%
    gather(key = location, value = tasmax, -Points) %>%
    rename(date = Points)  %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y"))  

  gcm_hist[[i]] <- precip_hist %>% mutate(tasmin = tasmin_hist$tasmin, tasmax = tasmax_hist$tasmax)
  
}
  
names(gcm_hist) <- climate_models

gcm_hist_tidy <- bind_rows(gcm_hist, .id = "model") %>%
  mutate(tasmin = as.numeric(tasmin), tasmax = as.numeric(tasmax)) %>%
  mutate(tasavg = (tasmin + tasmax)/2) %>%
  mutate(location = ifelse(location == "Gasakhatey", "Gasa", location)) %>%
  mutate(location = ifelse(location == "ParoDSC", "Paro", location)) %>%
  mutate(location = ifelse(location == "Sibsoo", "Sipsu", location)) %>%
  mutate(location = ifelse(location == "WangdueRNR", "Wangdue", location)) %>%
  filter(location %in% station_subset)
  

saveRDS(gcm_hist_tidy, file = "./Bhutan/data/gcm_hist_tidy.csv")

  

################################################################################

# ERA5 data (old) --------------------------------------------------------------

files_list <- list.files(path = "./Bhutan/data/era5_daily/", full.names = TRUE)
observed_weather_series_metadata <- read_csv("./Bhutan/data/observed_weather_metadata.csv")  
observed_weather_series_tidy <- read_csv("./Bhutan/data/observed_weather_tidy.csv")

era5_data <- list()
for (i in 1:length(files_list)) {
  era5_data[[i]] <- arrow::read_feather(files_list[i])
}
names(era5_data) <- observed_weather_series_metadata$station
era5_df <- bind_rows(era5_data, .id = "location")  

era5_df2 <- era5_df %>%
  mutate(date = as.Date(format(as.Date(date), "%Y-%m-%d"))) %>%
  select(date, location, prcp = tp, tavg = t2m, tmin, tmax) %>%
  mutate(tmin = tmin - 273.15, tavg = tavg - 273.15, tmax = tmax - 273.15) %>%
  mutate(prcp = prcp * 1000)


observed_weather_series_tidy <- write_csv(era5_df2,
                                          "./Bhutan/data/historical_weather_era5_daily_tidy.csv")
################################################################################

# ERA5 data (updated) ----------------------------------------------------------


files_list <- list.files(path = "./Bhutan/data/era5_updated/", full.names = TRUE)
observed_weather_series_metadata <- read_csv("./Bhutan/data/observed_weather_metadata.csv")  
observed_weather_series_tidy <- read_csv("./Bhutan/data/observed_weather_tidy.csv")


era5_data <- list()
for (i in 1:length(files_list)) {
  era5_data[[i]] <- arrow::read_feather(files_list[i])
}

names(era5_data) <- observed_weather_series_metadata$station
era5_df <- bind_rows(era5_data, .id = "location")  

era5_tidy_df <- era5_df %>%
  select(date, location, prcp = P, tavg = TEMP, pet = PET) %>%
  mutate(date = as.Date(format(as.Date(date), "%Y-%m-%d")))  
  
 
write_csv(era5_tidy_df, "./Bhutan/data/weather_era5_daily_updated_tidy.csv")


################################################################################

# CHIRPS dataset


files_list <- list.files(path = "./Bhutan/data/chirps/", full.names = TRUE)
observed_weather_series_metadata <- read_csv("./Bhutan/data/station_metadata.csv")  
observed_weather_series_tidy <- read_csv("./Bhutan/data/station_data_tidy.csv")

chirps_data <- list()
for (i in 1:length(files_list)) {
  chirps_data[[i]] <- arrow::read_feather(files_list[i])
}

names(chirps_data) <- observed_weather_series_metadata$station
chirps_df <- bind_rows(chirps_data, .id = "location")  

chirps_tidy_df <- chirps_df %>%
  select(date, location, prcp = P, tavg = TEMP, pet = PET) %>%
  mutate(date = as.Date(format(as.Date(date), "%Y-%m-%d")))  

write_csv(chirps_tidy_df, "./Bhutan/data/weather_chirps_daily_tidy.csv")

