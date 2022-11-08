#GTFS Contagem

install.packages('osmdata')
install.packages('ggthemes')
install.packages('purrr')
install.packages('geobr')
install.packages('pbapply')
install.packages('readr')
install.packages('fasttime')


library(osmdata)
library(ggthemes)
library(mapview)
library(dplyr)
library(purrr)
library(geobr)
library(pbapply)
library(readr)
library(fasttime)

source('R/fun/setup.R')


# Paradas - stops.txt -----------------------------------------------------

# c
install.packages('sf')
library(sf)
stops <- read_sf('../11 - GTFS/muni_con/Base de dados PED_03.shp') %>% st_zm (drop=T)

mapview(stops)
head(stops)

stops$stop_id <- seq.int(nrow(stops))
stops2 <- stops %>% select(stop_id, Endere__o) %>% st_transform(crs = "+proj=longlat + datum=WGS84") %>%
  mutate(stop_code = NA,
         stop_name = Endere__o,
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
  ) %>% select (-Endere__o)
# crs(stops2)

stops2 <- stops2 %>% st_drop_geometry()

write.table(stops2, file = '../11 - GTFS/muni_con/gtfs_files/stops.txt',sep = ',', na = "",
            row.names = F, quote = T)

# Itinerários - shapes.txt ------------------------------------------------

shapes <- st_read('../11 - GTFS/muni_con/Base de dados LINHAS_02.shp') %>% st_zm(drop = T) %>%
  st_transform(4326)

# shapes <- st_read('../11 - GTFS/muni_pal/itinerarios.gpkg') %>% st_transform(4326)
# mydata2 <- st_collection_extract(shapes, "LINESTRING")

mapview(shapes)

routes <- shapes %>% mutate(route_id = substr(route_id, start = 1L, stop = 3L),
                            agency_id = NA,
                            route_short_name = route_shor,
                            route_long_name = NA,
                            route_desc = NA,
                            route_type = 3,
                            route_url = NA,
                            route_color = NA,
                            route_text_color = NA,
                            route_text_color = NA) %>%
   st_drop_geometry() %>% distinct(route_id, .keep_all = T) %>% dplyr::select(-route_shor) 

write.table(routes, file = '../11 - GTFS/muni_con/gtfs_files/routes.txt',sep = ',', na = "",
            row.names = F, quote = F)


# Agency ------------------------------------------------------------------
agency <- data.frame(agency_id = 1,
                     agency_name = "TRANSCON",
                     agency_url = "https://www.transcon.contagem.mg.gov.br/",
                     agency_timezone = "Brazil/East",
                     agency_lang = "pt",
                     agency_phone = "31 3329-3390",
                     agency_fare_url = NA)

write.table(agency, file = '../11 - GTFS/muni_con/gtfs_files/agency.txt',sep = ',', na = "",
            row.names = F, quote = F)





