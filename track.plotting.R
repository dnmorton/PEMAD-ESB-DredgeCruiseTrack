library(sf)
library(ggplot2)
library(ggspatial)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,  tmap, 
               tidyr, stringr, readxl, scales,
               xtable, geosphere,  pivottabler, reshape2, ggplot2, ggrepel,
               ggspatial, here, dplyr, dbplyr, 
               magick, terra
               )


dir <- "C:/Users/dana.morton/Documents/GitHub/PEMAD-ESB-StrataShape"


Prj_DD="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
Prj_UTM19="+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"
Prj_UTM18="+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

coast_shape <- st_read(paste0(dir,'/EastCoast.shp'), quiet=T) %>% 
  st_transform(st_crs(Prj_DD))

hague_shape <- st_read(paste0(dir,"/Hague_Line.shp"), quiet=T) %>% 
  st_transform(st_crs(Prj_DD))

scallop_st <- st_read(paste0(dir,"/Scallop/NEFSC_Shellfish/Shellfish_Strata_NAD83.shp"), quiet=T) 

SAMS_GB_st <- st_read(paste0(dir,"/Scallop/SAMS/GB_Estimation_Areas_2024_UTM19_PDT/GB_Estimation_Areas_2024_UTM19_PDT.shp"), quiet=T) %>% 
  st_transform(st_crs(Prj_DD))

gpx_filepath <- "CL1_GSC_port_to_port_dredge_track_2026-06-24.gpx" 

# Read the GPX waypoints layer
# The 'sf' package automatically parses the XML structure into a spatial dataframe
waypoints <- st_read(gpx_filepath, layer = "waypoints", quiet = FALSE)

# Add a numeric index to match the chronological order of the voyage
waypoints$sequence <- 1:nrow(waypoints)

#read in dredge stations with comments
sta <- read.csv("Base_Dredge_Stations.csv")
colnames(sta)[1] <- "name"

waypoints <- left_join(waypoints, sta) %>% 
  mutate(is_rock = grepl("Rock-Chain", COMMENTS, ignore.case = TRUE))

# Compile individual waypoints into a continuous line feature (The Voyage Track)
# This connects the dots from Starting Port -> Dredge Stations -> Ending Port
track_line <- waypoints %>% 
  st_combine() %>% 
  st_cast("LINESTRING")


map <- ggplot()+
  geom_sf(data=coast_shape, fill= "grey", color ="grey")+
  geom_sf(data=scallop_st, color = "grey", alpha=.2, linewidth=.3)+
  geom_sf(data= SAMS_GB_st, color = "red", alpha=.3,linewidth=.4)+
  geom_sf(data=hague_shape, color="black", linewidth=.6)+
  annotation_north_arrow(location = "tl", height = unit(.6, "cm"),
                         width = unit(.65, "cm"), which_north = "true")+
  geom_sf(data = track_line, color = "blue3", size = .7, linetype = "solid") +
  geom_sf(data = waypoints, aes(fill = is_rock), size = 1.5, shape = 21) +
  scale_fill_manual(values=c("green", "magenta"))+
  geom_sf_text(data = waypoints, aes(label = sequence), 
               nudge_x = 0.01, nudge_y = 0.01, 
               fontface = "bold", color = "#111111", size = 1.8)+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(), 
        plot.caption = element_text(size=12)) +
  annotation_scale(location="br")+
  coord_sf(xlim = c(-70, -67.05), ylim = c(40.3, 42.1))
 

ggsave(
  filename = "CL1_GSC_port_to_port_dredge_track_2026-06-24.pdf", 
  plot = map, 
  width = 11, 
  height = 8.5, 
  dpi = 300 # High-resolution print quality
)
