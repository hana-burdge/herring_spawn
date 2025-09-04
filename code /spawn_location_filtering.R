# Filtering Spawn Location Coordiantes by Month

library(dplyr)
library(lubridate)

#Filtering for just March
target_month <- 3   

setwd("~/Desktop/Herring/QGIS/Spawn Locations/")

spawn_location_2003 <- read_csv("~/Desktop/Herring/QGIS/Spawn Locations/2023_spawn_locations.csv")
spawn_location_2004 <- read_csv("~/Desktop/Herring/QGIS/Spawn Locations/2004_spawn_locations.csv")
spawn_location_2005 <-  read_csv("~/Desktop/Herring/QGIS/Spawn Locations/2005_spawn_locations.csv")

# Converts to proper Date object
spawn_location_2003$survey_date <- mdy(spawn_location_2003$survey_date) 
spawn_location_2004$survey_date <- mdy(spawn_location_2004$survey_date) 
spawn_location_2005$survey_date <- mdy(spawn_location_2005$survey_date) 

#Filter for March
march_data_2003 <- spawn_location_2003 %>%
  filter(month(survey_date) == 3)
write.csv(march_data_2003, "march2003_coords.csv", row.names = FALSE)

march_data_2004 <- spawn_location_2004 %>%
  filter(month(survey_date) == 3)
write.csv(march_data_2004, "march2004_coords.csv", row.names = FALSE)

march_data_2005 <- spawn_location_2005 %>%
  filter(month(survey_date) == 3)
write.csv(march_data_2005, "march2005_coords.csv", row.names = FALSE)

