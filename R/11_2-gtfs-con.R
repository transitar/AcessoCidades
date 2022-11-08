#GTFS Contagem

source('R/fun/setup.R')


# Paradas - stops.txt -----------------------------------------------------

# a
install.packages('sf')
library(sf)
stops <- read_sf('../11 - GTFS/muni_con/Base de dados PED.shp')

mapview(stops)
head(stops)

stops$stop_id <- seq.int(nrow(stops))
stops2 <- stops %>% select(stop_id) %>% st_transform(crs = "+proj=longlat + datum=WGS84") %>%
  mutate(stop_code = NA,
         stop_name = Endere_o,
         stop_desc = NA,
         stop_lat = unlist(map(.$geometry,2)),
         stop_lon = unlist(map(.$geometry,1)),
         zone_id = NA,
         stop_URL = NA,
         location_type = 0,
         parent_station = NA,
         stop_timezone = NA,
         wheelchair_boarding = NA,
         level_id = NA,
         platform_code = NA
  )
# crs(stops2)

stops2 <- stops2 %>% st_drop_geometry()

write.table(stops2, file = '../11 - GTFS/muni_con/gtfs_files/stops.txt',sep = ',', na = "",
            row.names = F, quote = F)

# Itinerários - shapes.txt ------------------------------------------------

shapes <- st_read('../11 - GTFS/muni_pal/Linhas_Onibus_Mar_2020.kmz')
mapview(shapes)




