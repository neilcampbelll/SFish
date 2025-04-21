# Load required libraries
library(methods)  # For S4 classes

# Create an empty SFish object
sfish <- create_empty_sfish(gridType = "C-square")

# Add a spatial unit (C-square)
csquare_code <- "1500:467:363:4"
wkt_polygon <- "POLYGON((15.0 46.7, 15.05 46.7, 15.05 46.75, 15.0 46.75, 15.0 46.7))"
sfish <- add_spatial_unit(sfish, csquare_code, wkt_polygon)

# Add another spatial unit
csquare_code2 <- "1500:467:364:4"
wkt_polygon2 <- "POLYGON((15.05 46.7, 15.1 46.7, 15.1 46.75, 15.05 46.75, 15.05 46.7))"
sfish <- add_spatial_unit(sfish, csquare_code2, wkt_polygon2)

# Add data for the first spatial unit
sfish <- add_data(
  sfish,
  spatial_id = "1500:467:363:4",
  data_values = list(
    year = "2022",
    month = "1",
    country = "DK",
    metier4 = "GNS",
    metier5 = "DEF",
    effort = 1,
    catch = 1214.225,
    value = 6066.186,
    kwHours = 112
  )
)

# Add data for the second spatial unit
sfish <- add_data(
  sfish,
  spatial_id = "1500:467:364:4",
  data_values = list(
    year = "2022",
    month = "1",
    country = "DK",
    metier4 = "GNS",
    metier5 = "DEF",
    effort = 1,
    catch = 0,
    value = 0,
    kwHours = 112
  )
)

# Print the SFish object
sfish  # This will call the show method