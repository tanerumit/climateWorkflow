
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Gridded Climate Dataset comparisons
#  Date: Feb, 2021
#  By: Umit Taner
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Setup ------------------------------------------------------------------------

source("./src/read_in_data.R")

# Path to save the results
savePath <- "./Bhutan/results/03_analyze_gcm_data/"

# Datasets
climdata1_label <- "NCHM Stations"  
climdata1 <- ground_data_daily

climdata2_label <- "GCMs"
climdata2 <- gcm_hist_tidy

gcms_all <- unique(gcm_hist_tidy$model)
  
################################################################################

# Prepare datasets for comparison ----------------------------------------------

# Select stations for comparison 
met_stations <- c("Haa", "Paro", "Zhemgang",
                  "Mongar", #"Pemagatshel",
                  "Punakha", "Bhur", "Sipsu",
                  "Phuntsholing")

# Prepare Datasets
dataset1 <- climdata1 %>%
  filter(location %in% met_stations) %>%
  mutate(location = factor(location, levels = met_stations)) %>%
  mutate(precip = ifelse(precip < 0, 0, precip)) %>%
  mutate(tasavg = ifelse(tasavg < -90, NA, tasavg)) %>%
  add_column(model = "Obs", .before = "date")

dataset2 <- climdata2 %>%
  select(model, date, location, precip:tasavg) %>%
  filter(location %in% met_stations) %>%
  mutate(location = factor(location, levels = met_stations)) %>%
  # Remove possible wrong data
  mutate(precip = ifelse(precip < 0, 0, precip)) %>%
  mutate(tasavg = ifelse(tasavg < -90, NA, tasavg)) %>%
  mutate(precip = as.numeric(precip))


##### Daily values
dataset1_daily <- dataset1
dataset2_daily <- dataset2

###### Monthly averages

dataset1_monthly <- dataset1 %>%
  group_by(model, yr = year(date), month = month(date), location) %>%
  summarize(precip = sum(precip, na.rm = TRUE), 
            tasavg = mean(tasavg, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(date = as.Date(paste(yr,month,"01",sep="-"))) %>%
  select(model, date, location, precip, tasavg)

dataset2_monthly <- dataset2 %>%
  group_by(model, yr = year(date), month = month(date), location) %>%
  summarize(precip = sum(precip, na.rm = TRUE), 
            tasavg = mean(tasavg, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(date = as.Date(paste(yr,month,"01",sep="-"))) %>%
  select(model, date, location, precip, tasavg)


### Annual averages
dataset1_annual <- dataset1 %>%
  group_by(model, yr = year(date), location) %>%
  summarize(precip = mean(precip, na.rm = TRUE), 
            tasavg = mean(tasavg, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(date = as.Date(paste(yr,"01","01",sep="-"))) %>%
  select(model, date, location, precip, tasavg)

dataset2_annual <- dataset2 %>%
  group_by(model, yr = year(date), location) %>%
  summarize(precip = mean(precip, na.rm = TRUE), 
            tasavg = mean(tasavg, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(date = as.Date(paste(yr,"01","01",sep="-"))) %>%
  select(model, date, location, precip, tasavg)


climdata1_list <- list(daily = dataset1_daily, 
                       monthly = dataset1_monthly, 
                       annual = dataset1_annual)

climdata2_list <- list(daily = dataset2_daily,
                       monthly = dataset2_monthly, 
                       annual = dataset2_annual)


####### PLOT BOX-PLOTS ---------------------------------------------------------

tstep <- "monthly"
climvarc  <- "precip"

dat1 <- climdata1_list[tstep][[1]]  %>%
  select(model, date, location, value = all_of(climvarc)) %>%
  mutate(mon = factor(month(date), levels = 1:12)) 

dat2 <- climdata2_list[tstep][[1]] %>%
  select(model, date, location, value = all_of(climvarc)) %>%
  mutate(mon = factor(month(date), levels = 1:12)) 

dat <- bind_rows(dat1, dat2)

for (x in 1:length(gcms_all)) {
  
  datc <- dat %>% filter(model %in% c("Obs",gcm_all[x]))
  
  # Box-plot of monthly variations
  p <- ggplot(dfXc, 
              mapping = aes(x = mon, y = value,  group = interaction(mon, model))) +
    #ggtitle(label = gcm_all[x]) +
    theme_bw(base_size = 14) +
    geom_boxplot(aes(fill = model), alpha = 0.8, outlier.shape = NA) +
    theme(legend.key.size = unit(0.4, "cm")) +
    facet_wrap(~ location, ncol = 2, scales = "free") +
    labs(x = "Months", y = climvar_label[[climvarc]], fill = "") +
    scale_fill_manual(values = c("steelblue", "firebrick"),
                      breaks = c("Obs",gcm_all[x])) +
    guides(fill = FALSE)
  
  filename <- paste0(savePath, "boxplot_", gcm_all[x],"_", climvarc,".png")
  ggsave(filename, height = 11, width = 9*0.8)
  
}

################################################################################

####### COMPARE MEAN BIAS ------------------------------------------------------

dat1_mb <- dat1 %>%
  group_by(model, mon) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  select(-model) %>%
  rename(Obs = value)

dat2_mb <- dat2 %>%
  group_by(model, mon) %>%
  summarize(value = mean(value))

dat_mb <- dat2_mb %>% 
  left_join(dat1_mb, by = c("mon")) %>%
  mutate(mb = (value-Obs))

# Absolute values
p <- ggplot(dat_mb, aes(x = mon, y = value)) +
  theme_bw(base_size=12) +
  geom_line(aes(color = model, group=1), size = 1) +
  geom_line(aes(y = Obs, x = mon, group=1), size = 1) +
  facet_wrap(~ model, ncol = 3) +
  scale_color_brewer(palette="Dark2") +
  labs(x = "Months", y = climvar_label[[climvarc]])  +
  guides(color = FALSE)

filename <- paste0(savePath, "means_", climvarc, ".png")
ggsave(filename, height = 6, width = 7)


# Mean bias (%)
p <- ggplot(dat_mb, aes(x = mon, y = mb)) +
  theme_bw(base_size=12) +
  geom_bar(aes(fill = model), stat="identity", size = 1) +
  facet_wrap(~ model, ncol = 3) +
  scale_fill_brewer(palette="Dark2") +
  labs(x = "Months", y = "Average bias in Precipitation (mm)") +
  guides(fill = FALSE)

filename <- paste0(savePath, "bias_", climvarc, ".png")
ggsave(filename, height = 6, width = 7)



df1_sorted <- df1_monthly %>% select(-date) %>%
  group_by(location, model) %>%
  mutate(value = sort(value, decreasing = FALSE, na.last = TRUE), 
         x = (1:n())/(n() + 1)) %>% ungroup()

df2_sorted <- df2_monthly %>% select(-date) %>%
  group_by(location, model) %>%
  mutate(value = sort(value, decreasing = FALSE, na.last = TRUE), 
         x = (1:n())/(n() + 1)) %>% ungroup()

p <- ggplot() + 
  geom_line(aes(x = value, y = x, color = model),
            data = df2_sorted, size = 1, alpha = 0.8) +
  geom_line(aes(x = value, y = x), color = "black",
            data = df1_sorted, size = 1, alpha = 0.8) +
  scale_x_log10() +
  facet_wrap(~ location, ncol = 3, scales = "free") +
  guides(color = FALSE)
  
filename <- paste0(savePath, "cdf_", climvarc, ".png")
ggsave(filename, height = 5, width = 8)



