#criação de graphs no r5r

options(java.parameters = '-Xmx4G')
system("java -version")
library(r5r)
library(gtfstools)

gtfs <- gtfstools::read_gtfs('../11 - GTFS/muni_pal/gtfs_files/gtfs_pal.zip')
latest_validator <- gtfstools::download_validator(tempdir())
latest_validator
head(gtfs$trips[, .(trip_id, trip_headsign, shape_id)])


rurais <- c("670", "450", "640", "650", "630", "80T", "1LUZIM", "20L", "10L")
# rurais <- c("80T", "1LUZIM", "20L", "10L")

teste <- filter_by_route_id(gtfs = gtfs,rurais, keep = FALSE)
write_gtfs(teste, "teste_gtfs_pal.zip")





# FUNCAO PARA CONSTRUIR network -------------------------
# graph.obj é salvo na pasta './otp/graphs/ano/cidade
sigla_muni <- 'pal'
ano <- 2022

construir_graph_muni <- function(sigla_muni, ano) {
  
  
  path <- sprintf("../r5r/network/%s/muni_%s/", ano, sigla_muni)
  r5r::setup_r5(data_path = path, elevation = "TOBLER", overwrite = TRUE,
                verbose = TRUE)
  
}

r5r::stop_r5()

# aplicar funcao ------------------------------------------------------------------------------
# lapply(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, construir_graph_muni, ano = 2017)
# lapply(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, construir_graph_muni, ano = 2018)
lapply(munis_list$munis_metro$abrev_muni, construir_graph_muni, ano = 2022)