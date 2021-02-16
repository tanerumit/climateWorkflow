
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Climatology analysis and plots
#  Date: Feb, 2021
#  By: Umit Taner
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# General setup ----------------------------------------------------------------

source("./src/read_in_data.R")

# Path to save the results
savePath <- "./area/Bhutan/results/01 climate_features/"

# Set climate datasets for the analysis
climdata_meta <- ground_metadata
climdata_list <- ground_data_list
climdata_daily <- climdata_list["daily"][[1]]
climdata_monthly <- climdata_list["monthly"][[1]]
climdata_monthly <- climdata_list["annual"][[1]]

# Select stations/locations to analyze
met_stations <- c("Haa", "Paro", "Kanglung", 
   "Zhemgang","Trashiyangtse", "Pemagatshel",
   "Mongar","Damphu", "Punakha", "Bhur", 
   "Sipsu", "Phuntsholing"
)

################################################################################

# Gage station coverage (Length of records) ------------------------------------

climvarc <- "precip"

df <-  climdata_daily %>%
  select(date, location, var = all_of(climvarc)) %>%
  group_by(yr = year(date), mon = month(date), location) %>% 
  summarise(var = sum(var)) %>%
  #mutate(mon = "01") %>%
  unite("date", yr,mon, sep="-") %>% 
  mutate(date = as.Date(paste0(date,"-01"))) %>%
  ungroup() %>% 
  mutate(var = ifelse(!is.na(var), 1, NA)) %>%
  na.omit() %>%
  left_join(select(climdata_meta, location = station, elev = elevation_m), 
            by = "location")  


p <- ggplot(df, aes(y = fct_reorder(location, elev), color = elev)) +
  theme_bw(base_size = 11) +
  #facet_wrap(. ~ variable, scales = "free", drop = TRUE, nrow = 2) +
  geom_point(aes(x=date), size = 1.4, shape = 16,  alpha = 0.8) +
  #geom_segment(aes(x = start, xend = end, yend = station, color = elevation_m), size = 4) +
  scale_colour_viridis_c(direction=-1) +
  scale_x_date(breaks = "5 years", minor_breaks = "1 year", labels=date_format("%Y")) +
  #scale_color_discrete_sequential(palette = "Viridis") +
  #scale_x_continuous(limits = c(1900, 2020), breaks = seq(1200, 2020, 20)) +
  #scale_color_manual(values = c("#0072B2", "#D55E00", "#CC79A7")) +
  labs(x = "", y = "", color = "Altitude\n(m)")

filename <- paste0(savePath,"station_data_coverage.png")
ggsave(filename, height = 5, width = 8)


################################################################################

# Spatial Correlations ---------------------------------------------------------

tstep <- "monthly"
climvarc <- "tasavg"

df <- climdata_list[[tstep]] %>%
  select(date, location, var = all_of(climvarc)) %>%
  spread(location, var) %>% select(-date) %>%
  cor(use="pairwise.complete.obs") 

p <-  ggcorrplot(df, outline.col="white", 
                 lab_size = 3, 
                 lab = TRUE, 
                 type = "upper") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, color = 'grey20'),
        axis.text.y = element_text(angle = 0, color = 'grey20')) +  
  scale_fill_gradient2(low = muted("red"), mid = "white", high = muted("blue"), 
                       midpoint = 0.5, limits = c(0,1), 
                       breaks = seq(0, 1, 0.5), guide = "colourbar") +
  labs(x = "", y="", fill = "") +
  guides(fill = FALSE)

filename <- paste0(savePath, "corrplot_", tstep, "_", climvarc, ".png")
ggsave(filename, width = 8, height = 8)


################################################################################

# Seasonal patterns ---------------------------------------------------------------

tstep <- "monthly"
climvarc <- "tasavg"

df <- climdata_list[tstep][[1]] %>%
  filter(location %in% met_stations) %>%
  select(date, location, var = all_of(climvarc)) %>%
  mutate(mon = factor(month(date), levels = 1:12))  

df_stats <- df %>%
  group_by(location, mon) %>%
  summarize(ymean = mean(var, na.rm = TRUE),
            ymin = min(var, na.rm = TRUE),
            ymax = max(var, na.rm = TRUE))
  
# Box-plot of monthly variations
p <- ggplot(df, aes(x = mon, y = var,  group = mon)) +
  theme_bw(base_size = 12) +
  geom_boxplot() +
  theme(legend.key.size = unit(0.4, "cm")) +
  facet_wrap(~ location, ncol = 3, scales = "free") +
  labs(x = "Months", y = climvar_label[[climvarc]], color = "Stations") 

filename <- paste0(savePath, "seasonal_boxplot_", climvarc, ".png")
ggsave(filename, height = 10, width = 8)

# Ribbon-style variations 
p <- ggplot(df_stats, aes(x = mon, group = location)) +
  theme_bw(base_size = 12) +
  geom_line(aes(y = ymean)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2) +
  theme(legend.key.size = unit(0.4, "cm")) +
  facet_wrap(~ location, ncol = 3, scales = "free") +
  labs(x = "Months", y = climvar_label[[climvarc]], color = "Stations") 

filename <- paste0(savePath, "seasonal_ribbon_", climvarc, ".png")
ggsave(filename, height = 10, width = 8)


################################################################################

# Annual data analysis ---------------------------------------------------------

tstep <- "annual"
climvarc <- "tasavg"

df <- climdata_list[tstep][[1]] %>%
  filter(location %in% met_stations) %>%
  select(date, location, var = all_of(climvarc))  

# Annual series of data
p <- ggplot(df, aes(x = date, y = var)) +
  theme_light(base_size = 12) +
  geom_line(color = "black", alpha = 0.8) +
  facet_wrap(~location, nrow = 4, ncol = 3, scales = "free_y") +
  labs(x = "Year", y = climvar_label[[climvarc]]) +
  # Add linear trends
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) 
  # Add smoothed average 
  #geom_smooth(method = "loess", se = FALSE) 

filename <- paste0(savePath, "annual_trends_", climvarc, ".png")
ggsave(filename, height = 10, width = 8)

mk <- climdata_list[["annual"]] %>% 
  rename(value = all_of(climvarc)) %>% 
  select(date, location, value) %>%
  na.omit() %>%
  group_by(location) %>%
  summarize(statistic = trend::mk.test(value)$estimates[[3]], 
            `p-value` = trend::mk.test(value)$p.value) %>%
  select(station=location, statistic, `p-value`) %>% 
  mutate(across(where(is.numeric), round, 3)) %>%
  flextable() #%>%
  #add_header_lines(paste0("Mann-Kendall trend test"))

filename <- paste0(savePath, "annual_mk_", climvarc, ".png")
save_as_image(mk, filename, zoom = 2, expand = 5, webshot = "webshot")

################################################################################