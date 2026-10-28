library(sf)
library(dplyr)

# 1. Define your file names
gpx_filepath <- "final_port_to_port_dredge_track_2026-06-27.gpx"

# Read the GPX waypoints layer
# The 'sf' package automatically parses the XML structure into a spatial dataframe
waypoints <- st_read(gpx_filepath, layer = "waypoints", quiet = FALSE)

output_file <- "fina;_cruise_track_coastal_explorer_2026-06-27.gpx"    # The new file containing the route line



# 3. Convert the ordered points into a continuous line (The Navigation Track)
# 'st_combine' merges them in their current dataframe row order
track_line <- waypoints %>% 
  st_combine() %>% 
  st_cast("LINESTRING") %>% 
  st_sf() %>% 
  mutate(name = "Planned Cruise Track") # Name that will show up in Coastal Explorer



# 4. Open a single virtual connection to the GPX file
# This treats the file like an open database so we can inject layers freely
gpx_dsn <- st_write(track_line, dsn = output_file, driver = "GPX", layer = "trackline", quiet = FALSE)

# 5. Add the tracks track_line# 5. Add the tracks layer onto that same open database connection
# This avoids 'append' row crashes and bypasses 'update' deprecations
st_write(waypoints, dsn = gpx_dsn, layer = "waypoints", quiet = FALSE)


