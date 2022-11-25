#Levar empregos
rm(list = ls())

source('./R/fun/setup.R')
library(aopdata)

create_diretorios <- function(sigla_muni){
  
  dir.create(sprintf("../data/dados_hex/muni_%s/", sigla_muni), recursive = TRUE)
  
}

walk(munis_list$munis_df$abrev_muni, create_diretorios)


#leitura dos dados de empregos

sigla_muni <- 'poa'; ano <- 2018; source <- 'aop'; source_lazer <- 'osm'

infos_to_hex <- function(sigla_muni, ano) {
  
  
  
  #dados da microssimulação
  
  data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                 sigla_muni, sigla_muni))
  #ajeitar o formato
  grid_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/grid_muni_%s.RDS',
                                 sigla_muni, sigla_muni))
  
  #checar setores com todos os renda_class_pc == n_col
  
  lista_tract <- data_micro %>% group_by(code_tract, renda_class_pc) %>%
    summarise(n = n()) %>% ungroup() %>%
    group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
                                       n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
    filter(n_classes > n_classes_col) %>% pull(code_tract)
  
  data_micro2 <- data_micro %>% filter(code_tract %in% lista_tract)
  
  
  

  
  
  
  
  
  
  
  
  
  
  #empregos
  
  #leitura dos dados de empregos
  
  file <- sprintf('../data-raw/empregos/%s_empregos%s.gpkg', sigla_muni, ano)
  empregos <- read_sf(file)
  # mapview(empregos)
  #leitura dos hexágonos
  
  ano <- 2019
  file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)
  hex <- read_rds(file_hex)
  
  # mapview(hex)
  
  #leitura do shape do município
  file_shape <- sprintf('../data-raw/municipios/%s/municipio_%s_%s.rds', ano, sigla_muni, ano)
  shape_muni <- read_rds(file_shape)
  # mapview(shape_muni)
  
  
  # intersection <- st_intersection(x = hex, y = empregos)
  # plot(polygon, graticule = st_crs(4326), key.pos = 1)
  
  
  join_jobs_hex <- sf::st_join(empregos, hex)
  
  hex2 <- join_jobs_hex %>%
    st_drop_geometry() %>%
    group_by(id_hex) %>% 
    summarise(n_jobs = sum(jobs))
  
  hex3 <- left_join(hex, hex2, by = "id_hex") %>%
    mutate(n_jobs = ifelse(is.na(n_jobs)==T,
                           0,
                           n_jobs)) %>% st_drop_geometry()
  

  # mapview(hex3, zcol = 'n_jobs')
  
  #saúde
  
  file_saude <- sprintf('../data/saude/%s/muni_%s_saude_%s/muni_%s.rds',source,  sigla_muni, source,  sigla_muni)
  dados_saude <- read_rds(file_saude)
  # mapview(dados_saude, zcol = "S001")
  
  

  hex4 <- left_join(hex3 , dados_saude %>% st_drop_geometry(), by = 'id_hex')

  #educação
  
  file_educacao <- sprintf('../data/educacao/%s/muni_%s_educacao_%s/muni_%s.rds',source,  sigla_muni, source,  sigla_muni)
  dados_educacao <- read_rds(file_educacao) %>% st_drop_geometry()
  # mapview(dados_saude, zcol = "S001")
  
  hex5 <- left_join(hex4, dados_educacao, by = "id_hex")
  
  # mapview(hex5, zcol = 'M001')
  
  #falta lazer
  
  file_lazer <- sprintf('../data-raw/lazer/%s/muni_%s_lazer_%s/muni_%s_lazer_%s.rds',source_lazer,
                        sigla_muni, source_lazer,  sigla_muni, source_lazer)
  dados_lazer <- read_rds(file_lazer)
  # mapview(dados_saude, zcol = "S001")
  
  #contar lazer no hexagono
  
  ano <- 2019
  file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)
  hex_empty <- read_rds(file_hex) %>% select(id_hex)
  
  hex_lazer <- sf::st_join(hex_empty, dados_lazer)
  
  hex_lazer2 <- hex_lazer %>% mutate(count = ifelse(is.na(osm_id)== TRUE,
                                         0,
                                         1)) %>%
    st_drop_geometry() %>%
    group_by(id_hex) %>% 
    summarise(lazer_tot = sum(count)) %>% distinct(id_hex, .keep_all = T)
  
  hex_lazer3 <- left_join(hex,hex_lazer2 , by = "id_hex") %>% select(id_hex, lazer_tot) %>%
    st_drop_geometry()
  
  
  hex_total <- left_join(hex5, hex_lazer3, by = "id_hex")
  
  # mapview(hex_total_sf, zcol = 'lazer_tot')
  
  #paraciclos
  
  paraciclos_hex <- read_rds(sprintf('../data/paraciclos/muni_%s/paraciclos_%s.rds', sigla_muni, sigla_muni))
  
  # hex_total <- read_rds('../data/dados_hex/muni_poa/dados_hex_poa.rds') %>% st_drop_geometry()
  
  hex_total <- left_join(hex_total, paraciclos_hex, by = "id_hex")
  
  
  #bikes compartilhadas
  #dados de bikes
  sigla_municipio <- sigla_muni
  decisao_muni <- read_excel('../planilha_municipios.xlsx',
                             sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
 
   if (decisao_muni$bike_comp == 1) {
    
     if (decisao_muni$fonte_bikecomp == "muni"){
       
       dados_bikecomp <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                         sigla_muni, sigla_muni),
                                 layer = "bike_comp"
       )
     }
    
   }
  
  dados_bikecomp <- dados_bikecomp %>% st_transform(decisao_muni$epsg)
  
  join_bikecomp_hex <- sf::st_join(dados_bikecomp,st_transform(hex,decisao_muni$epsg ))
  
  hex2 <- join_bikecomp_hex %>%
    st_drop_geometry() %>%
    group_by(id_hex) %>% 
    summarise(n_bikes = n())
  
  hex_total <- left_join(hex_total, hex2, by = "id_hex") %>%
    mutate(n_bikes = ifelse(is.na(n_bikes)==T,
                           0,
                           n_bikes)) %>% st_drop_geometry()
  

  
  hex_total_sf <- left_join(hex_total, hex_empty, by = "id_hex")
  
  #
  
  # teste <- hex_jobs %>% filter(h3_resolution > 0)
  # mapview(teste, zcol = 'n_jobs', alpha.regions = 0.6)
  
  
  #ainda faltando os dados socioeconomicos da microssimulaçao
  
  write_rds(hex_total_sf, sprintf("../data/dados_hex/muni_%s/dados_hex_%s.rds", sigla_muni, sigla_muni))
  

  

  
  
}