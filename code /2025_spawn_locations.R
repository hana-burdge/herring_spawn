# Load libraries
library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(units)

# Read spawn data
spawn_data <- read_csv("~/Desktop/2025_herring_sapwn_data.csv")

# Convert to spatial object (point data)
spawn_sf <- spawn_data %>%
  mutate(
    spawn_start_date = mdy(spawn_start_date),
    spawn_end_date = mdy(spawn_end_date),  # Add this line if applicable
    month = month(spawn_start_date, label = TRUE),
    year = year(spawn_start_date)
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# OPTIONAL: Filter by a specific month (e.g., March)
spawn_sf_march <- spawn_sf %>%
  filter(month == "Mar")

# ---- Create Approximate Spawn Polygons ----
# To simulate a rectangular polygon centered at each point
# We'll project to meters first so length/width are meaningful

# Step 1: Project to UTM (or other suitable metric CRS)
spawn_march_proj <- st_transform(spawn_sf_march, crs = 32609)  # UTM zone 9N (BC coast); adjust if needed

# Step 2: Create polygons using width and length
# Assumes rectangles aligned E-W (simple assumption)
spawn_polygons <- spawn_march_proj %>%
  rowwise() %>%
  mutate(geometry = list({
    # Half dimensions in meters
    half_length <- length_m / 2
    half_width <- average_width_m / 2

# 📦 1. FILTER OUT ROWS WITH MISSING LENGTH/WIDTH
# Ensures that only complete records (with length and width) are used to create polygons
    
spawn_march_proj_clean <- spawn_march_proj %>%
      filter(!is.na(length_m), !is.na(average_width_m))
    
# 📐 2. CREATE RECTANGULAR POLYGONS FROM CENTER POINTS
# Simulates spawning area as rectangles centered on each point with given length and width
spawn_polygons <- spawn_march_proj_clean %>%
  rowwise() %>%
  mutate(geometry = list({
    # Half dimensions in meters
    half_length <- length_m / 2
    half_width <- average_width_m / 2
    
    # Get x and y of center point
    center <- st_coordinates(geometry)
    x <- center[1]
    y <- center[2]
    
    # Create rectangle coordinates (clockwise)
    coords <- matrix(c(
      x - half_length, y + half_width,
      x + half_length, y + half_width,
      x + half_length, y - half_width,
      x - half_length, y - half_width,
      x - half_length, y + half_width  # close polygon
    ), ncol = 2, byrow = TRUE)
    
    st_polygon(list(coords))
  })) %>%
  ungroup() %>%
  st_as_sf(crs = 32609)  

# Create final sf object

# After creating the spawn_polygons object
st_crs(spawn_polygons) <- 32609  # Explicitly assign the correct CRS

# Now reproject to WGS84
spawn_polygons_wgs84 <- st_transform(spawn_polygons, crs = 4326)

# Step 4: Add popup info
spawn_polygons_wgs84 <- spawn_polygons_wgs84 %>%
  mutate(
    location = if_else(is.na(location), "Unknown", location),
    region = if_else(is.na(region), "Unknown", region),
    spawn_start_date = if_else(is.na(spawn_start_date), "Unknown", as.character(spawn_start_date)),
    length_m = if_else(is.na(length_m), 0, length_m),
    average_width_m = if_else(is.na(average_width_m), 0, average_width_m),
    popup_info = paste0(
      "<b>Location:</b> ", location, "<br>",
      "<b>Region:</b> ", region, "<br>",
      "<b>Start:</b> ", spawn_start_date, "<br>",
      "<b>Length (m):</b> ", round(length_m, 1), "<br>",
      "<b>Width (m):</b> ", round(average_width_m, 1)
    )
  )
# Step 5: Map polygons
mapview(spawn_polygons_wgs84,
        zcol = "region",
        popup = ~popup_info,      # ← Fix: use formula to refer to column
        legend = TRUE,
        alpha.regions = 0.6)
