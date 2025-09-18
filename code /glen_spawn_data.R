# ##############################################################################

# Mapping herring spawn distribution using length, width, intensity, spawn start date, and spawn index from 1936 - 2005
# Athour: Hana Burdge

# ##############################################################################

library(ggplot2)
library(dplyr)
library(lubridate)
library(ggforce)
library(viridis)
library(readr)
library(ggpubr)

#-------------------------------------------------------------------------------

# CLEANING GLEN'S DATA

#-------------------------------------------------------------------------------

# Read CSV (treat blanks and "NA" as missing values)
glen_data <- read_csv("data/glen_spawn_data.csv", na = c("", "NA", "na"))

# Remove first column and last two columns (not needed)
glen_data <- glen_data %>%
  select(-1, -(ncol(.)-1), -ncol(.))

# Look at all possible names for regions 
unique(glen_data$region)
#[1] "Adeane Point"                    "Baronet Pass"                   
#[3] "Beaver Harbour"                  "Belleisle Sound"                
#[5] "Bond Sound"                      "Boughey Bay"                    
#[7] "Franklin Flats"                  "Hardy Bay"                      
#[9] "Kingcome Inlet"                  "Kingcome Inlet, Wakeman Sound"  
#[11] "Knight Inlet"                    "Mackenzie C"                    
#[13] "Masterman Island"                "Port McNeill"                   
#[15] "Reid Bay"                        "Sargeaunt Pass"                 
#[17] "Thompson Sound"                  "Thomspon Sound"                 
#[19] "Tribune Channel"                 "Tribune Channel, Thompson Sound"
#[21] "Wakeman Sound"    

# Rename some of the regions so that they are considered the same
glen_data <- glen_data %>%
  mutate(region = recode(region,
                         "Kingcome Inlet, Wakeman Sound" = "Wakeman Sound",
                         "Thomspon Sound" = "Thompson Sound",
                         "Tribune Channel, Thompson Sound" = "Thompson Sound"))
unique(glen_data$region)
#[1] "Adeane Point"     "Baronet Pass"     "Beaver Harbour"   "Belleisle Sound"  "Bond Sound"      
#[6] "Boughey Bay"      "Franklin Flats"   "Hardy Bay"        "Kingcome Inlet"   "Wakeman Sound"   
#[11] "Knight Inlet"     "Mackenzie C"      "Masterman Island" "Port McNeill"     "Reid Bay"        
#[16] "Sargeaunt Pass"   "Thompson Sound"   "Tribune Channel" 

# Organize these regions into Sections within Area 12
glen_data <- glen_data %>%
  mutate(section = case_when(
    region %in% c("Franklin Flats", "Adeane Point", "Knight Inlet") ~ "127",
    region %in% c("Baronet Pass", "Boughey Bay") ~ "123",
    region %in% c("Mackenzie C") ~ "124",
    region %in% c("Sargeaunt Pass", "Thompson Sound", "Tribune Channel", "Bond Sound") ~ "125",
    region %in% c("Beaver Harbour", "Hardy Bay", "Masterman Island") ~ "122",
    region %in% c("Belleisle Sound", "Kingcome Inlet", "Wakeman Sound", "Reid Bay") ~ "126",
    region %in% c("Port McNeill") ~ "121"
  )) %>%
  relocate(section, .after = region)  # moves 'section' right after 'region'

# Convert survey_date from month/day/year to Date format
glen_data <- glen_data %>%
  mutate(survey_date = mdy(survey_date))

# Arrange so same section and regions are together, ordered by year then date -> arranged by survey date 
# and not spawn date because survey date does not have any NAs
glen_data <- glen_data %>%
  arrange(section, region, year(survey_date), survey_date)

### Summarizing total number or records, total length, average width
spawn_summary <- glen_data %>%
  group_by(section, year = year(survey_date)) %>%
  summarise(
    total_length   = sum(length_m, na.rm = TRUE),
    n_spawns       = n(),
    average_width  = mean(average_width_m, na.rm = TRUE),
    .groups = "drop"
  ) 
# Filter to look at length and width per spawn
filtered_spawn_summary <- spawn_summary %>%
  mutate(length_per_spawn = total_length/n_spawns, width_per_spawn = average_width/n_spawns)
  
# Plot summed spawn length over time for each section and the number of spawns being summed
spawn_length <- ggplot(filtered_spawn_summary, aes(x = factor(year), y = length_per_spawn, color = section, group = section)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(0, ceiling(max(filtered_spawn_summary$length_per_spawn)), by = 100)) +
  scale_color_viridis_d(option = "viridis") +
  labs(
    x = "Year",
    y = "Total Spawn Length (m)"
  ) +
  theme_classic()
  
spawn_length

# Save plot as PNG
ggsave("figures/spawn_length.png", plot = , width = 10, height = 6, dpi = 300)


### Now for Average Width in each Section by year

# Plot average spawn width over time for each section and show number of spawns
spawn_width <- ggplot(filtered_spawn_summary, aes(x = factor(year), y = width_per_spawn, color = section, group = section)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(0, ceiling(max(filtered_spawn_summary$width_per_spawn)), by = 1)) +
  scale_color_viridis_d(option = "viridis") +
  labs(
    x = "Year",
    y = "Average Spawn Width (m)"
  ) +
  theme_classic()

spawn_width

# Save plot as PNG
ggsave("figures/spawn_width.png", plot = , width = 10, height = 6, dpi = 300)


#-------------------------------------------------------------------------------

# CLEANING BILLY'S BOOK DATA 

#-------------------------------------------------------------------------------

billy_data <- read_csv("data/billy_data.csv")

# Clean the data to add it to glen_data to make combined data
# Turn section into character 
billy_data <- billy_data %>%
  mutate(section = as.character(section)) %>%
  rename("n_spawns" = "total records", "total_length" = "sum_length", "average_width" = "av_width", "spawn_index" = "spawn index", "first_spawn" = "min_date", "last_spawn" = "max_date")


###  COMBINE WITH GLEN'S DATA TO GET FULL TIME SERIES  ###
# Now stack the rows
combined_data <- bind_rows(spawn_summary, billy_data)

#-------------------------------------------------------------------------------

# PLOT LENGTH AND WIDTH (entire time series)

#-------------------------------------------------------------------------------

# Make sure year is numeric for sorting, then convert to ordered factor
combined_data <- combined_data %>%
  arrange(section, year) %>%
  mutate(year_factor = factor(year, levels = sort(unique(year))))

# Divide the length by number of spawns to account for the number of surveys that were 
# recorded each year in each section
filtered_combined_data <- combined_data %>%
  filter(n_spawns != 0) %>%
  mutate(length_per_spawn = total_length/n_spawns, width_per_spawn = average_width/n_spawns)

## Look at the Data
# Plot length per spawn for all section over time * IS THIS DATA CONTINUOUS?? *
ggplot(filtered_combined_data, aes(x = factor(year), y = length_per_spawn, colour = section, group = section)) +
  geom_line(alpha = 0.5, size = 1) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ section, scales = "free_y", ncol = 2) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, max(combined_data$total_length, na.rm = TRUE), by = 1000), expand = c(0, 0)) +
  labs(x = "Year", y = "Length per Spawn (m)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot average width per spawn for all section over time 
ggplot(filtered_combined_data, aes(x = factor(year), y = width_per_spawn, group = section)) +
  geom_line(alpha = 0.5, size = 1) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ section, scales = "free_y", ncol = 1) +
  theme_classic() +
  labs(x = "Year", y = "Average Width per Spawn (m)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-------------------------------------------------------------------------------

# RUN SOME STATS ON LENGTH AND WIDTH PER SPAWN

#-------------------------------------------------------------------------------
#--------------------
# LM LENGTH PER SPAWN 
#--------------------
# Finding the trend line for each section and find the p values 
lm_length_section <- filtered_combined_data %>% 
  group_by(section) %>% 
  # take log and scale x and y to make them comparable 
  mutate(log_length_per_spawn = log(length_per_spawn + 1)) %>% 
  mutate(length_per_spawn_scaled = scale(log_length_per_spawn), year_scaled = scale(year)) %>%
  # adding the intercept, slope, and p value into the lm_length_per_section data frame 
  mutate(lm_intercept = lm(length_per_spawn_scaled ~ year_scaled)$coefficients[1],
         lm_slope = lm(length_per_spawn_scaled ~ year_scaled)$coefficients[2],
         p_value = summary(lm(length_per_spawn_scaled ~ year_scaled))$coefficients[2,4],
         # predicting according to the model what the length_per_spawn is in different years -> this is in order to de-scale later
         predicted_length_per_spawn_scaled = predict(lm(length_per_spawn_scaled ~ year_scaled)),
         # finding the upper and lower 95% confidence intervals 
         predicted_length_per_spawn_upper_scaled = predict(lm(length_per_spawn_scaled ~ year_scaled), interval = "confidence", level = 0.95)[,3],
         predicted_length_per_spawn_lower_scaled = predict(lm(length_per_spawn_scaled ~ year_scaled), interval = "confidence", level = 0.95)[,2]) %>% 
  # de-scaling the values for plotting
  mutate(predicted_length_per_spawn =  predicted_length_per_spawn_scaled * sd(length_per_spawn) + mean(length_per_spawn),
         predicted_length_per_spawn_upper =  predicted_length_per_spawn_upper_scaled * sd(length_per_spawn) + mean(length_per_spawn),
         predicted_length_per_spawn_lower =  predicted_length_per_spawn_lower_scaled * sd(length_per_spawn) + mean(length_per_spawn))

#lm_length_section %>% View()

lm_length_summary <- lm_length_section %>% 
  select(section, lm_slope, p_value) %>% 
 distinct() %>% 
  group_by(section) 
  
#options(digits = 8)
#lm_length_summary
#section lm_slope   p_value
# <chr>      <dbl>     <dbl>
#  1 121     0.245   0.559    
#  2 122    -0.485   0.000417 
#  3 123    -0.275   0.0712   
#  4 124    -0.159   0.353    
#  5 125    -0.364   0.00796  
#  6 126    -0.288   0.0312   
#  7 127    -0.585   0.0000126

# Plotting again with the lm line and confidence intervals 
lm_length_plot <- ggplot(lm_length_section, aes(x = factor(year), y = length_per_spawn, group = section)) +
  geom_line(alpha = 0.5, size = 1) +
  geom_point(alpha = 0.5) +
  #geom_text(aes(label = n_spawns), vjust = -0.5, size = 3) +
  geom_line(aes(x = factor(year), y = predicted_length_per_spawn)) +
  geom_ribbon(aes(ymin = predicted_length_per_spawn_lower, ymax = predicted_length_per_spawn_upper), alpha = 0.2) +
  facet_wrap(~ section, scales = "free_y", ncol = 1) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 14, face = "bold")  # bigger facet labels
  ) +
  labs(x = "Year", y = "Length per Spawn (m)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
                                   axis.title.x = element_text(size = 14),
                                   axis.title.y = element_text(size = 14))

lm_length_plot # model looks good -> use this plot
ggsave("figures/lm_length.png", plot = , width = 14, height = 15, dpi = 300)


#-------------------------------------------------------
# RUN MODEL DIAGNOSTICS ON LENGTH (on the scaled version)
#-------------------------------------------------------

library(car)
library(tidyverse)
# fit a regression model
lm_length <- lm(length_per_spawn_scaled ~ year_scaled, data = lm_length_section)

summary(lm_length)

# checking for normally distributed residuals with hist
lm_length_section$lm_length_resids <-resid(lm_length) 

hist(lm_length_section$lm_length_resids) 

# look again with QQ Plot
qqPlot(lm_length_section$lm_length_resids, id = list(labels = lm_length_section$section)) 

# checking for relationships between residuals and predictors
residualPlot(lm_length, tests = FALSE) # this looks good!!



#---------------------------
# LM AVERAGE WIDTH PER SPAWN
#---------------------------

lm_width_section <- filtered_combined_data %>% 
  group_by(section) %>% 
  # take log and scale x and y to make them comparable 
  mutate(log_width_per_spawn = log(width_per_spawn + 1)) %>% 
  mutate(width_per_spawn_scaled = scale(log_width_per_spawn), year_scaled = scale(year)) %>%
  # adding the intercept, slope, and p value into the lm_width_per_section data frame 
  mutate(lm_intercept = lm(width_per_spawn_scaled ~ year_scaled)$coefficients[1],
         lm_slope = lm(width_per_spawn_scaled ~ year_scaled)$coefficients[2],
         p_value = summary(lm(width_per_spawn_scaled ~ year_scaled))$coefficients[2,4],
         # predicting according to the model what the width_per_spawn is in different years -> this is in orde to de-scale later
         predicted_width_per_spawn_scaled = predict(lm(width_per_spawn_scaled ~ year_scaled)),
         # finding the upper and lower 95% confidence intervals 
         predicted_width_per_spawn_upper_scaled = predict(lm(width_per_spawn_scaled ~ year_scaled), interval = "confidence", level = 0.95)[,3],
         predicted_width_per_spawn_lower_scaled = predict(lm(width_per_spawn_scaled ~ year_scaled), interval = "confidence", level = 0.95)[,2]) %>% 
  # de-scaling the values for plotting
  mutate(predicted_width_per_spawn =  predicted_width_per_spawn_scaled * sd(width_per_spawn) + mean(width_per_spawn),
         predicted_width_per_spawn_upper =  predicted_width_per_spawn_upper_scaled * sd(width_per_spawn) + mean(width_per_spawn),
         predicted_width_per_spawn_lower =  predicted_width_per_spawn_lower_scaled * sd(width_per_spawn) + mean(width_per_spawn))

#lm_width_section %>% View()

# Plotting again with the lm line and confidence intervals 
lm_width_plot <- ggplot(lm_width_section, aes(x = factor(year), y = width_per_spawn, group = section)) +
  geom_line(alpha = 0.5, size = 1) +
  geom_point(alpha = 0.5) +
  #geom_text(aes(label = n_spawns), vjust = -0.5, size = 3) +
  geom_line(aes(x = factor(year), y = predicted_width_per_spawn)) +
  geom_ribbon(aes(ymin = predicted_width_per_spawn_lower, ymax = predicted_width_per_spawn_upper), alpha = 0.2) +
  facet_wrap(~ section, scales = "free_y", ncol = 1) +
  theme_classic() +
  #scale_y_continuous(breaks = seq(0, 56, by = 2)) +
  labs(x = "Year", y = "Average Width per Spawn (m)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lm_width_plot
#ggsave("figures/lm_width.png", plot = , width = 18, height = 8, dpi = 300)


#-------------------------------
# RUN MODEL DIAGNOSTICS ON WIDTH
#-------------------------------

# fit a regression model
lm_width <- lm(width_per_spawn_scaled ~ year_scaled, data = lm_width_section)

summary(lm_width)

# checking for normally distributed residuals with hist
lm_width_section$lm_width_resids <-resid(lm_width) 

hist(lm_width_section$lm_width_resids) 

# look again with QQ Plot
qqPlot(lm_width_section$lm_width_resids, id = list(labels = lm_width_section$section)) 

# checking for relationships between residuals and predictors
residualPlot(lm_width, tests = FALSE) # even with log this looks bad



#-------------------------------------------------------------------------------

# CALCULATE SPAWN INDEX

#-------------------------------------------------------------------------------
## Filtering/cleaning data 
# get rid of unneeded columns
glen_data_index <- glen_data %>% 
  select(-c(side, weather, time_start, time_end, tide_start, tide_end, method, weather, water_conditions, 
            max_vert_visibility_m, sea_conditions, tide_time_start, tide_time_end))
# get rid of NAs
glen_data_index <- glen_data_index %>% mutate(across(everything(), ~replace(., is.na(.), 0)))

## Calculating Cofficient and Index
# summing layers across sections and years 
glen_data_index <- glen_data_index %>%
  mutate(year = year(survey_date)) %>%  # extract year first
  group_by(section, year) %>%
  # To sum the records from all types od vegetation in one survey
  mutate(sum_layer = rowSums(across(c(grass_layers, RW_layers, kelp_layers, BA_layers, LR_layers, 
                                      SR_layers, RK_layers, O)), na.rm = TRUE)) %>%
  # To count the number of surveys in a year in a section
  mutate(n_spawns = n()) %>% 
  # calculate 
  mutate(spawn_coefficient = (1/n_spawns) * sum(average_width_m * sum_layer)) %>% 
  mutate(spawn_index = sum(length_m) * spawn_coefficient) %>% 
  ungroup()
  
## Now combine with Billy data
glen_billy_index <- bind_rows(glen_data_index, billy_data)

# Plot spawn index to see what it looks like
ggplot(glen_billy_index, aes(x = factor(year), y = spawn_index, group = section)) +
  geom_line(alpha = 0.5, size = 1) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ section, scales = "free_y", ncol = 1) +
  theme_classic() +
  labs(x = "Year", y = "Spawn Index (m^2)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
#-------------------------------------------------------------------------------

# RUN SOME STATS ON SPAWN INDEX 

#-------------------------------------------------------------------------------

lm_spawn_index <- glen_billy_index %>% 
  group_by(section) %>% 
  # scale x and y to make them comparable 
  mutate(log_spawn_index = log(spawn_index + 1)) %>% 
  mutate(spawn_index_scaled = scale(log_spawn_index), year_scaled = scale(year)) %>%
  # adding the intercept, slope, and p value into the lm_width_per_section data frame 
  mutate(lm_intercept = lm(spawn_index_scaled ~ year_scaled)$coefficients[1],
         lm_slope = lm(spawn_index_scaled ~ year_scaled)$coefficients[2],
         p_value = summary(lm(spawn_index_scaled ~ year_scaled))$coefficients[2,4],
         # predicting according to the model what the spawn_index is in different years -> this is in orde to de-scale later
         predicted_spawn_index_scaled = predict(lm(spawn_index_scaled ~ year_scaled)),
         # finding the upper and lower 95% confidence intervals 
         predicted_spawn_index_upper_scaled = predict(lm(spawn_index_scaled ~ year_scaled), interval = "confidence", level = 0.95)[,3],
         predicted_spawn_index_lower_scaled = predict(lm(spawn_index_scaled ~ year_scaled), interval = "confidence", level = 0.95)[,2]) %>% 
  # de-scaling the values for plotting
  mutate(predicted_spawn_index =  predicted_spawn_index_scaled * sd(spawn_index) + mean(spawn_index),
         predicted_spawn_index_upper =  predicted_spawn_index_upper_scaled * sd(spawn_index) + mean(spawn_index),
         predicted_spawn_index_lower =  predicted_spawn_index_lower_scaled * sd(spawn_index) + mean(spawn_index))

#lm_spawn_index %>% View()

# Plotting again with the lm line and confidence intervals 
lm_spawn_index_plot <- ggplot(lm_spawn_index, aes(x = factor(year), y = spawn_index, group = section)) +
  geom_line(alpha = 0.5, size = 1) +
  geom_point(alpha = 0.5) +
  #geom_text(aes(label = n_spawns), vjust = -0.5, size = 3) +
  geom_line(aes(x = factor(year), y = predicted_spawn_index)) +
  geom_ribbon(aes(ymin = predicted_spawn_index_lower, ymax = predicted_spawn_index_upper), alpha = 0.2) +
  facet_wrap(~ section, scales = "free_y", ncol = 1) +
  theme_classic() +
  labs(x = "Year", y = "Spawn Index (Total Spawn Deposition m^2)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lm_spawn_index_plot
ggsave("figures/lm_spawn_index.png", plot = , width = 10, height = 20, dpi = 300)

#-------------------------------
# RUN MODEL DIAGNOSTICS ON INDEX
#-------------------------------

# fit a regression model
lm_index <- lm(spawn_index_scaled ~ year_scaled, data = lm_spawn_index)

summary(lm_index)

# checking for normally distributed residuals with hist
lm_spawn_index$lm_index_resids <-resid(lm_index) 

hist(lm_spawn_index$lm_index_resids) 

# look again with QQ Plot
qqPlot(lm_spawn_index$lm_index_resids) 

# checking for relationships between residuals and predictors
residualPlot(lm_index, tests = FALSE) # log and non-log look bad 

#-------------------------------------------------------------------------------

# PLOTTING SPAWN START AND END DATES

#-------------------------------------------------------------------------------

billy_spawn_dates <- read_csv("data/billy_start_end_dates.csv")

# filtering for the earliest and latest spawn dates 
glen_spawn_dates <- glen_data %>% 
  mutate(year = year(survey_date)) %>%   
  group_by(section, year) %>% 
  mutate(min_date = min(spawn_start_date),
         max_date = max(spawn_end_date)) 

# taking only the columns that we need 
glen_spawn_dates <- glen_spawn_dates %>% 
 select(year, section, min_date, max_date) 

# changing the date format of date from year/month/day to year-month-day
glen_spawn_dates$min_date <- format(as.Date(glen_spawn_dates$min_date, format = "%m/%d/%Y"), "%Y-%m-%d")
glen_spawn_dates$max_date <- format(as.Date(glen_spawn_dates$max_date, format = "%m/%d/%Y"), "%Y-%m-%d")

# combine billy and glen date data 
glen_billy_dates <- rbind(billy_spawn_dates, glen_spawn_dates)

# Make any 0 into NAs and then get rid of any rows with NAs
glen_billy_dates$min_date[glen_billy_dates$min_date == "0"] <- NA
glen_billy_dates$max_date[glen_billy_dates$min_date == "0"] <- NA
glen_billy_dates <- glen_billy_dates %>% 
  filter(!is.na(min_date)) %>%
  filter(!is.na(max_date))

# now take only the month and date from min and max spawn dates 
glen_billy_dates$min_date <- as.Date(as.character(glen_billy_dates$min_date), format = "%Y-%m-%d")
glen_billy_dates$min_month_day <- format(glen_billy_dates$min_date, "%m-%d")
glen_billy_dates$max_date <- as.Date(as.character(glen_billy_dates$max_date), format = "%Y-%m-%d")
glen_billy_dates$max_month_day <- format(glen_billy_dates$max_date, "%m-%d")

# plot to see what earlist spawn start date looks like 
ggplot(glen_billy_dates, aes(x = factor(year), y = min_month_day)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ section, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = "Earliest Spawn Start Date") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot to see what latest spawn end date looks like 
ggplot(glen_billy_dates, aes(x = factor(year), y = max_month_day)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ section, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = "Latest Spawn End Date") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-------------------------------------------------------------------------------

# RUN SOME STATS ON START AND END DATES # how to properly scale dates?

#-------------------------------------------------------------------------------

lm_min_month_day <- glen_billy_dates %>% 
  group_by(section) %>% 
  # scale x and y to make them comparable 
  #mutate(log_min_month_day = log(min_month_day + 1)) %>% 
  mutate(min_month_day_scaled = scale(min_month_day), year_scaled = scale(year)) %>%
  # adding the intercept, slope, and p value into the lm_width_per_section data frame 
  mutate(lm_intercept = lm(min_month_day_scaled ~ year_scaled)$coefficients[1],
         lm_slope = lm(min_month_day_scaled ~ year_scaled)$coefficients[2],
         p_value = summary(lm(min_month_day_scaled ~ year_scaled))$coefficients[2,4],
         # predicting according to the model what the min_month_day is in different years -> this is in orde to de-scale later
         predicted_min_month_day_scaled = predict(lm(min_month_day_scaled ~ year_scaled)),
         # finding the upper and lower 95% confidence intervals 
         predicted_min_month_day_upper_scaled = predict(lm(min_month_day_scaled ~ year_scaled), interval = "confidence", level = 0.95)[,3],
         predicted_min_month_day_lower_scaled = predict(lm(min_month_day_scaled ~ year_scaled), interval = "confidence", level = 0.95)[,2]) %>% 
  # de-scaling the values for plotting
  mutate(predicted_min_month_day =  predicted_min_month_day_scaled * sd(min_month_day) + mean(min_month_day),
         predicted_min_month_day_upper =  predicted_min_month_day_upper_scaled * sd(min_month_day) + mean(min_month_day),
         predicted_min_month_day_lower =  predicted_min_month_day_lower_scaled * sd(min_month_day) + mean(min_month_day))

# Plotting again with the lm line and confidence intervals 
lm_min_month_day_plot <- ggplot(lm_min_month_day, aes(x = factor(year), y = min_month_day, group = section)) +
  geom_line(alpha = 0.5, size = 1) +
  geom_point(alpha = 0.5) +
  #geom_text(aes(label = n_spawns), vjust = -0.5, size = 3) +
  geom_line(aes(x = factor(year), y = predicted_min_month_day)) +
  geom_ribbon(aes(ymin = predicted_min_month_day_lower, ymax = predicted_min_month_day_upper), alpha = 0.2) +
  facet_wrap(~ section, scales = "free_y", ncol = 2) +
  theme_classic() +
  labs(x = "Year", y = "Earliest Spawn Start Date") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

lm_min_month_day_plot
