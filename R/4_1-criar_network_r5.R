#criação de graphs no r5r
rm(list = ls(all.names = T)); gc(full = T)

options(java.parameters = '-Xmx5G')
# system("java -version")
library(r5r)
library(gtfstools)
library(dplyr)

gtfs <- gtfstools::read_gtfs("../r5r/network/2022/gtfs_cit_202305_v01.zip")
# gtfs <- gtfstools::read_gtfs('../data-raw/gtfs/muni_con/2022/gtfs_con.zip')
# gtfs <- gtfstools::read_gtfs('../r5r/network/2022/muni_dou/gtfs_dourados_202212.zip')
`%nin%` = Negate(`%in%`)

linhas_teste <-  gtfs$routes$route_id[c(10,101)]

linhas_remover <- gtfs$routes$route_id[c(81,84)]


routes_teste <- gtfstools::filter_by_route_id(gtfs,route_id = linhas_teste)
trips_teste <- routes_teste$trips$trip_id
trips_teste <- gtfstools::filter_by_trip_id(routes_teste,trip_id = trips_teste)

routes <- gtfs$routes %>% filter(route_id %nin% linhas_remover) %>% pull(route_id)
routes <- routes[1:106]
trips <- gtfs$trips$trip_id
stops <- gtfs$stops$stop_id

gtfs_filter <- gtfstools::filter_by_agency_id(gtfs,"1")
gtfs_filter <- gtfstools::filter_by_route_id(gtfs_filter,route_id = routes)
gtfs_filter <- gtfstools::filter_by_trip_id(gtfs_filter,trip_id = trips)
gtfs_filter <- gtfstools::filter_by_stop_id(gtfs_filter,stop_id = stops) %>%
  write_gtfs("../r5r/network/2022/muni_cit/gtfs_cit_202305_v01_filtrado.zip")


a <- gtfs$routes$route_id

gtfs %>%
  filter_by_route_id(route_id = a[2]) %>%
  write_gtfs("gtfs_dourados_teste.zip")


# latest_validator <- gtfstools::download_validator(tempdir())
# latest_validator
# head(gtfs$trips[, .(trip_id, trip_headsign, shape_id)])
#palmas linhas problemáticas: 640, 650, 630, 80T, 
rurais <- c("450","640","650","630", "80T", "20L", "1LUZIM", "10L")
# rurais <- c("670", "450", "640", "650", "630", "80T", "1LUZIM", "20L", "10L")
# rurais <- c("80T", "1LUZIM", "20L", "10L")
# teste2 <- filter_by_route_id(gtfs, "450")
# teste2$trips
# smaller_gtfs <- filter_by_time_of_day(gtfs, from = "05:00:00", to = "23:59:00")
# smaller_gtfs$trips]
# filter_by_trip_id(teste2, "U450-V029-V")

teste <- filter_by_route_id(gtfs = gtfs,rurais, keep = FALSE)
write_gtfs(teste, "../r5r/network/2022/muni_pal/gtfs_pal.zip")





# FUNCAO PARA CONSTRUIR network -------------------------
# graph.obj é salvo na pasta './otp/graphs/ano/cidade
sigla_muni <- 'man'
ano <- 2022

construir_graph_muni <- function(sigla_muni, ano) {
  
  
  path <- sprintf("../r5r/network/%s/muni_%s/", ano, sigla_muni)
  r5r::setup_r5(data_path = path, elevation = "TOBLER", overwrite = TRUE,
                verbose = TRUE)
  
}
construir_graph_muni('man',2022)
r5r::stop_r5()

# aplicar funcao ------------------------------------------------------------------------------
# lapply(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, construir_graph_muni, ano = 2017)
# lapply(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, construir_graph_muni, ano = 2018)
lapply(munis_list$munis_metro$abrev_muni, construir_graph_muni, ano = 2022)