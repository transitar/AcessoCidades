#salvamento dos dados de paraciclos no nível dos hexagonos

source('./R/fun/setup.R')


# sigla_muni <- 'poa'


consolida_paraciclos <- function(sigla_munii){
  
  
  sigla_municipio <- sigla_muni
  decisao_muni <- read_excel('../planilha_municipios.xlsx',
                             sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
  
  if (is.logical(decisao_muni$fonte_paraciclos)){
  message("Por favor, informe a fonte dos dados de paraciclos e o EPSG do DATUM SIRGAS 2000 da cidade:")
  fonte_paraciclos <- readline("Fonte dos dados de paraciclos : ")
  muni_epsg <- readline("EPSG : ")
  }
  
  if (decisao_muni$fonte_paraciclos == "muni_e_osm"){
    
    dados_paraciclos_muni <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                             sigla_muni, sigla_muni),
                                     layer = "paraciclos"
    ) %>% mutate(Tipo = "Público", id=1) %>% st_transform(decisao_muni$epsg) %>%
      select(name = Name, id, Tipo) %>% st_zm(drop = T)
    
    dados_paraciclos_osm <- read_sf(sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                                            sigla_muni, sigla_muni),
                                    layer = "paraciclos") %>% st_transform(decisao_muni$epsg) %>%
      select(name, id = osm_id, Tipo)
    paraciclos <- rbind(dados_paraciclos_muni, dados_paraciclos_osm) 
    
  } else if (decisao_muni$fonte_paraciclos == "muni"){
    
    dados_paraciclos_muni <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                             sigla_muni, sigla_muni),
                                     layer = "paraciclos"
    ) %>% mutate(Tipo = "Público", id=1) %>% st_transform(decisao_muni$epsg) %>%
      select(name = Name, id, Tipo) %>% st_zm(drop = T)
    
    paraciclos <- dados_paraciclos_muni
    
  } else if (decisao_muni$fonte_paraciclos == "osm") {
  
    dados_paraciclos_osm <- read_sf(sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                                            sigla_muni, sigla_muni),
                                    layer = "paraciclos") %>% st_transform(decisao_muni$epsg) %>%
      select(name, id = osm_id, Tipo)
    
    paraciclos <- dados_paraciclos_osm
    
  } else {
    
    message('Por favor informe a fonte dos dados de paraciclos na planilha')
    
  }
  # mapview(dados_bikecomp)
  
  paraciclos <- paraciclos %>% st_transform(decisao_muni$epsg)
  
  
  #hexágonos
  
  ano <- 2019
  file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)
  hex <- read_rds(file_hex) %>% select(id_hex) %>% st_transform(decisao_muni$epsg)
  
  
  join_paraciclos_hex <- sf::st_join(paraciclos, hex)
  
  hex2 <- join_paraciclos_hex %>%
    st_drop_geometry() %>%
    group_by(id_hex) %>% drop_na(id_hex) %>%
    summarise(paraciclos = n())
  
  hex3 <- left_join(hex, hex2, by = "id_hex") %>%
    mutate(paraciclos = ifelse(is.na(paraciclos)==T,
                           0,
                           paraciclos)) %>% st_drop_geometry()
  
  
  
  suppressWarnings(dir.create(sprintf('../data/paraciclos/muni_%s/', sigla_munii), recursive = T))
  
  write_rds(hex3 ,sprintf('../data/paraciclos/muni_%s/paraciclos_%s.rds', sigla_munii, sigla_munii))
  
  
}


consolida_paracilcos('poa')