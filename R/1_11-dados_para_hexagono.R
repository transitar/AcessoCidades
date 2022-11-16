#Levar empregos
rm(list = ls())

source('./R/fun/setup.R')

#leitura dos dados de empregos

sigla_muni <- 'poa'; ano <- 2018;

infos_to_hex <- function(sigla_muni, ano) {
  
  #empregos
  
  #leitura dos dados de empregos
  
  file <- sprintf('../data-raw/empregos/%s_empregos%s.gpkg', sigla_muni, ano)
  empregos <- read_sf(file)
  mapview(empregos)
  #leitura dos hexágonos
  
  ano <- 2019
  file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)
  hex <- read_rds(file_hex)
  
  #leitura do shape do município
  file_shape <- sprintf('../data-raw/municipios/%s/municipio_%s_%s.rds', ano, sigla_muni, ano)
  shape_muni <- read_rds(file_shape)
  mapview(shape_muni)
  
  
  intersection <- st_intersection(x = hex, y = empregos)
  plot(polygon, graticule = st_crs(4326), key.pos = 1)
  
  
  join_jobs_hex <- sf::st_join(empregos, hex)
  
  hex <- join_jobs_hex %>%
    st_drop_geometry() %>%
    group_by(id_hex) %>% 
    summarise(n_jobs = sum(jobs)) %>%
    left_join(hex, by = "id_hex") %>% st_as_sf()
  
  #saúde
  
  
  
  teste <- hex_jobs %>% filter(h3_resolution > 0)
  mapview(teste, zcol = 'n_jobs', alpha.regions = 0.6)
  
}