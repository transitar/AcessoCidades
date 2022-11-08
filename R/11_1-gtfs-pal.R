#GTFS Palmas

source('R/fun/setup.R')


# Paradas - stops.txt -----------------------------------------------------


stops <- read_sf('../11 - GTFS/muni_pal/Pontos_Onibus_Out_2018.shp')

mapview(stops)
head(stops)

stops$stop_id <- seq.int(nrow(stops))
stops2 <- stops %>% select(stop_id) %>% st_transform(crs = "+proj=longlat + datum=WGS84") %>%
  mutate(stop_code = NA,
         stop_name = NA,
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

write.table(stops2, file = '../11 - GTFS/muni_pal/gtfs_files/stops.txt',sep = ',', na = "",
            row.names = F, quote = F)

# Itinerários - shapes.txt ------------------------------------------------

shapes <- st_read('../11 - GTFS/muni_pal/Linhas_Onibus_Mar_2020.shp') %>% st_zm(drop = T) %>%
  st_transform(4326)

# shapes <- st_read('../11 - GTFS/muni_pal/itinerarios.gpkg') %>% st_transform(4326)
# mydata2 <- st_collection_extract(shapes, "LINESTRING")

mapview(shapes)

routes <- shapes %>% mutate(route_id = substr(Name_1, start = 1L, stop = 3L),
                            agency_id = NA,
                            route_short_name = ITINERARIO,
                            route_long_name = NA,
                            route_desc = NA,
                            route_type = 3,
                            route_url = NA,
                            route_color = NA,
                            route_text_color = NA,
                            route_text_color = NA) %>%
  select(-Name_1,-ITINERARIO, -Exten_Km) %>% st_drop_geometry() %>% distinct(route_id, .keep_all = T)

write.table(routes, file = '../11 - GTFS/muni_pal/gtfs_files/routes.txt',sep = ',', na = "",
            row.names = F, quote = F)


# Agency ------------------------------------------------------------------
agency <- data.frame(agency_id = 1,
           agency_name = "SETURB",
           agency_url = "https://seturb.com.br/",
           agency_timezone = "Brazil/East",
           agency_lang = "pt",
           agency_phone = "63 3225-1248",
           agency_fare_url = NA)

write.table(agency, file = '../11 - GTFS/muni_pal/gtfs_files/agency.txt',sep = ',', na = "",
            row.names = F, quote = F)
