
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Gridded Climate Dataset comparisons
#  Date: Feb, 2021
#  By: Umit Taner
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Setup ------------------------------------------------------------------------

source("./src/read_in_data.R")

# Path to save the results
savePath <- "./Bhutan/results/02_analyze_gridded_data/"

# Datasets
climdata1_label <- "NCHM" 
climdata1 <- ground_data_daily


#climdata2_label <- "ERA5"
#climdata2 <- era5_daily


climdata2_label <- "CHIRPS"
climdata2 <- chirps_daily


################################################################################

# Prepare datasets for comparison ----------------------------------------------

# Common stations between the datasets
#met_stations_init <- sort(intersect(unique(era5_daily$location), 
#  unique(observed_daily$location)))

# Select stations for comparison 
met_stations <- c("Haa", "Paro", "Kanglung", "Zhemgang",
                     "Trashiyangtse", "Pemagatshel","Mongar",
                     "Damphu", "Punakha", "Bhur", "Sipsu",
                     "Phuntsholing")

# Prepare Datasets
dataset1 <- climdata1 %>%
  filter(location %in% met_stations) %>%
  mutate(location = factor(location, levels = met_stations))  %>%
  mutate(precip = ifelse(precip < 0, 0, precip)) %>%
  mutate(tasavg = ifelse(tasavg < -90, NA, tasavg))

dataset2 <- climdata2 %>%
  rename(precip = prcp, tasavg = tavg) %>%
  select(date, location, precip, tasavg) %>%
  filter(location %in% met_stations) %>%
  mutate(location = factor(location, levels = met_stations)) %>%
  # Remove possible wrong data
  mutate(precip = ifelse(precip < 0, 0, precip)) %>%
  mutate(tasavg = ifelse(tasavg < -90, NA, tasavg)) 


##### Daily values
dataset1_daily <- dataset1
dataset2_daily <- dataset2

dataset1 %>% filter(location == "Haa") %>% pull(precip) %>% range(na.rm = TRUE)
dataset2 %>% filter(location == "Haa") %>% pull(precip) %>% range(na.rm = TRUE)



##### 5-day averages
agg_period <- 5

dataset1_5day <- dataset1 %>%
  mutate(grp = (yday(date) - 1) %/% agg_period) %>%
  group_by(yr = year(date), location, grp) %>%
  mutate(precip = mean(precip, na.rm = TRUE)*agg_period, 
         tasavg = mean(tasavg, na.rm = TRUE)) %>%
  slice(n()) %>% ungroup() %>% select(-grp, -yr)

dataset2_5day <- dataset2 %>%
  mutate(grp = (yday(date) - 1) %/% agg_period) %>%
  group_by(yr = year(date), location, grp) %>%
  mutate(precip = mean(precip, na.rm = TRUE)*agg_period, 
         tasavg = mean(tasavg, na.rm = TRUE)) %>%
  slice(n()) %>% ungroup() %>% select(-grp, -yr)

###### Monthly averages

dataset1_monthly <- dataset1 %>%
  group_by(yr = year(date), month = month(date), location) %>%
  summarize(precip = sum(precip, na.rm = TRUE), tasavg = mean(tasavg, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(date = as.Date(paste(yr,month,"01",sep="-"))) %>%
  select(date, location, precip, tasavg)

dataset2_monthly <- dataset2 %>%
  group_by(yr = year(date), month = month(date), location) %>%
  summarize(precip = sum(precip, na.rm = TRUE), tasavg = mean(tasavg, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(date = as.Date(paste(yr,month,"01",sep="-"))) %>%
  select(date, location, precip, tasavg)


### Annual averages
dataset1_annual <- dataset1 %>%
  group_by(yr = year(date), location) %>%
  summarize(precip = mean(precip, na.rm = TRUE)*365.25, tasavg = mean(tasavg, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(date = as.Date(paste(yr,"01","01",sep="-"))) %>%
  select(date, location, precip, tasavg)

dataset2_annual <- dataset2 %>%
  group_by(yr = year(date), location) %>%
  summarize(precip = mean(precip, na.rm = TRUE)*365.25, tasavg = mean(tasavg, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(date = as.Date(paste(yr,"01","01",sep="-"))) %>%
  select(date, location, precip, tasavg) 


climdata1_list <- list(daily = dataset1_daily, `5day` = dataset1_5day, 
                     monthly = dataset1_monthly, annual = dataset1_annual)

climdata2_list <- list(daily = dataset2_daily, `5day` = dataset2_5day, 
                     monthly = dataset2_monthly, annual = dataset2_annual)


################################################################################

# Scatterplot comparison -------------------------------------------------------

tstep <- "monthly"
climvarc  <- "precip"

dat1 <- climdata1_list[tstep][[1]]  
dat2 <- climdata2_list[tstep][[1]]  
 
# Precipitation
p <- ggQQPlot(
  data1 = dat1, 
  data2 = dat2,
  data1.label = climdata1_label,
  data2.label = climdata2_label,
  var = climvarc, 
  var.lab = climvar_label[climvarc][[1]],
  locations = met_stations,
  show.r2 = TRUE, 
  show.pval = FALSE,
  ncol = 3)  

filename <- paste0(savePath, "scatterplot_", climvarc, "_", tstep,".png")
ggsave(filename, height = 11*0.8, width = 8*0.8)  


################################################################################

# Compare time-series on same plot ---------------------------------------------

tstep <- "annual"
climvarc  <- "precip"

dat1 <- climdata1_list[tstep][[1]]  
dat2 <- climdata2_list[tstep][[1]]  
 
df <- dat1 %>% select(date, location, var1 = all_of(climvarc)) %>%
  left_join(select(dat2, date, location, var2 = all_of(climvarc)), by = c("date","location")) %>%
  pivot_longer(var1:var2, names_to = "variables", values_to = "values") %>%
  group_by(location, variables)  

p <- ggplot(df, aes(x=date, y = values, group = variables, color = variables)) +
  theme_bw(base_size = 12) +
  geom_line() +
  facet_wrap(~ location, scales = "free_y", ncol = 3) +
  labs(x = "", y = climvar_label[climvarc][[1]], color="Dataset")  +
  scale_color_manual(labels = c(climdata1_label, climdata2_label), 
                     values = c(climdata1_col, climdata2_col),
                     breaks = c("var1", "var2"))


filename <- paste0(savePath, "annualseries_", climvarc, "_", tstep,".png")
ggsave(filename, height = 9, width = 11) 


################################################################################


# Compare annual cycles --------------------------------------------------------

tstep <- "daily"
climvarc  <- "precip"

dat1 <- climdata1_list[tstep][[1]]  
dat2 <- climdata2_list[tstep][[1]]  


df1 <- climdata1_list[tstep][[1]] %>%
  filter(location %in% met_stations) %>%
  select(date, location, var = all_of(climvarc)) %>%
  mutate(mon = factor(month(date), levels = 1:12)) %>%
  group_by(location, mon) %>%
  summarize(var1 = mean(var, na.rm = TRUE)) 

df2 <- climdata2_list[tstep][[1]] %>%
  filter(location %in% met_stations) %>%
  select(date, location, var = all_of(climvarc)) %>%
  mutate(mon = factor(month(date), levels = 1:12)) %>%
  group_by(location, mon) %>%
  summarize(var2 = mean(var, na.rm = TRUE))

df <- df1 %>% left_join(df2, by = c("location", "mon")) %>%
  gather(key = variable, value =values, var1:var2)

# Ribbon-style variations 
p <- ggplot(df, aes(x = mon, group = variable, color = variable)) +
  theme_light(base_size = 14) +
  geom_line(aes(y = values)) +
  facet_wrap(~ location, ncol = 3, scales = "free") +
  labs(x = "Months", y = climvar_label[[climvarc]], color = "Stations") +
  scale_color_manual(labels = c(climdata1_label, climdata2_label), 
                     values = c(climdata1_col, climdata2_col),
                     breaks = c("var1", "var2")) +
  theme(legend.position="bottom")

filename <- paste0(savePath, "annual_cycle_", climvarc, ".png")
ggsave(filename, height = 12*0.9, width = 8*0.9)

################################################################################


# Extreme value analysis -------------------------------------------------------

# Calculate annual maximas

dat1_ev <- dataset1_daily %>%
  group_by(yr = year(date), location) %>%
  summarize(precip = max(precip, na.rm = TRUE)) %>%
  filter(!is.infinite(precip)) %>%  
  mutate(date = as.Date(paste(yr,"01","01",sep="-")))

dat2_ev <- dataset2_daily %>%
  group_by(yr = year(date), location) %>%
  summarize(precip = max(precip, na.rm = TRUE)) %>%
  filter(!is.infinite(precip)) %>%  
  mutate(date = as.Date(paste(yr,"01","01",sep="-")))

dist <- "GEV"    # "GEV", "GP", "PP", "Gumbel", "Exponential"
locs <- met_stations
rperiods <- c(5,10,50,100,200)
rperiod_labs <- paste0(rperiods," yrs")

# For dataset1
dat1_rl <- dat1_ev %>% 
  filter(location %in% met_stations) %>%
  group_by(location) %>%
  nest() %>%
  mutate(
    fitD = map(.x = data, .f=~fevd(.x$precip, type = dist)),
    rl = map(.x = fitD, .f=~return.level(.x, rperiods))
  ) %>% unnest(rl) %>%
  group_by(location) %>%
  mutate(var = rperiod_labs)  %>%
  select(location, var, obs = rl)  

dat2_rl <- dat2_ev %>% 
  filter(location %in% met_stations) %>%
  group_by(location) %>%
  nest() %>%
  mutate(
    fitD = map(.x = data, .f=~fevd(.x$precip, type = dist)),
    rl = map(.x = fitD, .f=~return.level(.x, rperiods))
  ) %>% unnest(rl) %>%
  group_by(location) %>%
  mutate(var = rperiod_labs) %>%
  select(location, var, era5 = rl)   

df_rl <- dat1_rl %>%
  left_join(dat2_rl, by = c("location", "var")) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  pivot_longer(obs:era5, names_to = "dataset", values_to = "values") %>%
  unite("variable", var, dataset) %>%
  pivot_wider(names_from=variable, values_from = values) 

typology <- tibble(col_keys = colnames(df_rl)) %>%
  mutate(what = word(col_keys,1,sep = "\\_")) %>%
  mutate(measure = word(col_keys,2,sep = "\\_")) 

rl_table <- df_rl %>%
  flextable() %>%
  set_header_df(mapping = typology, key = "col_keys") %>%
  merge_h(part = "header") %>%
  theme_zebra() %>%
  border_inner_v(part = "all") %>%
  align(align = "center", part = "all")

filename <- paste0(savePath, "rtlevels_dailymaxima.png")
save_as_image(rl_table, filename, zoom = 3, expand = 10, webshot = "webshot")


################################################################################

# Taylor diagram - compare stats -----------------------------------------------



### Tailor diagram

library(viridis)
colset <- viridis_pal(option = "D")(length(df))

df1x <- dataset1_daily %>% select(date, location, precip) %>% 
  rename(df1 = precip)
df2x <- dataset2_daily %>% select(date, location, precip) %>% 
  rename(df2 = precip)

df <- df1x %>% left_join(df2x, by = c("date", "location")) %>% 
  droplevels() %>%
  split(.$location) 

taylor.diagram(df[[1]]$df1, df[[1]]$df2, col = colset[1], pch = 2, add = FALSE)

sapply(2:11, function(x) 
  taylor.diagram(df[[x]]$df1, df[[x]]$df2, col = colset[x], pch = 2, add = TRUE))


################################################################################

