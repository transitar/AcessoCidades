#Levar empregos
rm(list = ls())

source('./R/fun/setup.R')
library(aopdata)

create_diretorios <- function(sigla_muni){
  
  dir.create(sprintf("../data/dados_hex/muni_%s/", sigla_muni), recursive = TRUE)
  
}

walk(munis_list$munis_df$abrev_muni, create_diretorios)


#leitura dos dados de empregos

sigla_muni <- 'dou'; ano <- 2018; source_saude <- 'cnes'; source_lazer <- 'osm';source_escolas <- "censo_escolar"

infos_to_hex <- function(sigla_muni, ano) {
  
  
  
  #dados da microssimulação
  
  data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                 sigla_muni, sigla_muni))
  #ajeitar o formato
  grid_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/grid_muni_%s.RDS',
                                 sigla_muni, sigla_muni))
  
  #checar setores com todos os renda_class_pc == n_col
  
  # lista_tract <- data_micro %>% group_by(code_tract, renda_class_pc) %>%
  #   summarise(n = n()) %>% ungroup() %>%
  #   group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
  #                                      n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
  #   filter(n_classes > n_classes_col) %>% pull(code_tract)
  # 
  # data_micro2 <- data_micro %>% filter(code_tract %in% lista_tract)
  
  
  

  
  
  
  
  
  
  
  
  
  
  #empregos
  
  #leitura dos dados de empregos
  
  file <- sprintf('../data-raw/empregos/%s_empregos%s.gpkg', sigla_muni, ano)
  empregos <- read_sf(file)
  sum(empregos$jobs)
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
  
  if (sigla_muni == "pal"){
    file_saude <- '../data-raw/dados_municipais_recebidos/muni_pal/muni_pal.gpkg'
    dados_saude <- read_sf(file_saude, layer = "saude") %>%
      mutate(health_low = ifelse(ATUACAO == "Atencao Basica",
                                 1, health_low)
             ) %>%
      mutate(S001 = 1) %>%
      select(S001, S002 = health_low, S003 = health_med, S004 = health_hig)
    # dados_saude2 <- read_sf(file_saude, layer = "saude_cnes")
    # dados_saude2 <- dados_saude2 %>% filter(health_med == 1)
    
    join_saude_hex <- sf::st_join(dados_saude, hex)
    
    hex2 <- join_saude_hex %>%
      st_drop_geometry() %>%
      group_by(id_hex) %>% 
      summarise(S001 = sum(S001),
                S002 = sum(S002),
                S003 = sum(S003),
                S004 = sum(S004))
    
    hex_saude <- left_join(hex, hex2, by = "id_hex") %>%
      mutate(S001 = ifelse(is.na(S001)==T, 0, S001),
             S002 = ifelse(is.na(S002)==T, 0, S002),
             S003 = ifelse(is.na(S003)==T, 0, S003),
             S004 = ifelse(is.na(S004)==T, 0, S004)) %>% st_drop_geometry() %>%
      select(-h3_resolution, -sigla_muni)
    dados_saude <- hex_saude
    
  } else if (source == "cnes"){
  
  file_saude <- sprintf('../data/saude/%s/muni_%s_saude_%s/muni_%s_%s_geocoded_2019.rds',source_saude,  sigla_muni,
                        source_saude,  sigla_muni, source_saude)
  dados_saude <- read_rds(file_saude) %>% mutate(S001 = 1) %>%
    select(S001, S002 = health_low, S003 = health_med, S004 = health_high)
  mapview(dados_saude)
  
  
  if (muni == "dou"){
    
    saude_muni <- read_sf('../data-raw/dados_municipais_recebidos/muni_dou/muni_dou.gpkg',
                          layer = "saude") %>%
      select(S001, S002, S003, S004)
    dados_saude <- saude_muni
    # mapview(saude_muni) + mapview(dados_saude)
  }
  
  join_saude_hex <- sf::st_join(dados_saude %>% st_transform(decisao_muni$epsg),
                                st_transform(hex, decisao_muni$epsg))
  
  hex2 <- join_saude_hex %>%
    st_drop_geometry() %>%
    group_by(id_hex) %>% 
    summarise(S001 = sum(S001),
              S002 = sum(S002),
              S003 = sum(S003),
              S004 = sum(S004))
  
  hex_saude <- left_join(hex, hex2, by = "id_hex") %>%
    mutate(S001 = ifelse(is.na(S001)==T, 0, S001),
           S002 = ifelse(is.na(S002)==T, 0, S002),
           S003 = ifelse(is.na(S003)==T, 0, S003),
           S004 = ifelse(is.na(S004)==T, 0, S004)) %>% st_drop_geometry() %>%
    select(-h3_resolution, -sigla_muni)
  dados_saude <- hex_saude
  
  # mapview(dados_saude, zcol = "S001")
  } else if (source == "aop"){
    
    file_saude <- sprintf('../data/saude/%s/muni_%s_saude_%s/muni_%s_%s_geocoded_2019.rds',source,  sigla_muni,
                          source,  sigla_muni, source)
    dados_saude <- read_rds(file_saude)
    
  }
  
  

  hex4 <- left_join(hex3 , dados_saude %>% st_drop_geometry(), by = 'id_hex')

  #educação
  
  if (sigla_muni == "pal"){
    ano = 2021
    file_educacao <- read_rds(sprintf("../data-raw/educacao/censo_escolar/%s/muni_%s_educacao_%s/muni_%s_geocode_%s.rds",
                             ano,
                             sigla_muni,
                             ano,
                             sigla_muni,
                             ano))
    dados_educacao <- file_educacao %>%
      # mutate(health_low = ifelse(ATUACAO == "Atencao Basica",
      #                            1, health_low)
      # ) %>%
      mutate(E001 = 1,
             E002 = ifelse(TP_ETAPA_ENSINO == 1,1,0),
             E003 = ifelse(TP_ETAPA_ENSINO == 2,1,0),
             E004 = ifelse(TP_ETAPA_ENSINO == 3,1,0),
             M001 = QT_MAT_BAS + QT_MAT_FUND + QT_MAT_INF+QT_MAT_MED+QT_MAT_PROF,
             M002 = QT_MAT_BAS+QT_MAT_INF,
             M003 = QT_MAT_FUND,
             M004 = QT_MAT_MED+QT_MAT_PROF) %>%
                            #                            1, health_low)) %>%
      select(E001, E002, E003, E004, M001, M002,M003,M004)
    # dados_saude2 <- read_sf(file_saude, layer = "saude_cnes")
    # dados_saude2 <- dados_saude2 %>% filter(health_med == 1)
    
    join_educacao_hex <- sf::st_join(dados_educacao, hex)
    
    hex2 <- join_educacao_hex %>%
      st_drop_geometry() %>%
      group_by(id_hex) %>% 
      summarise(E001 = sum(E001),
                E002 = sum(E002),
                E003 = sum(E003),
                E004 = sum(E004),
                M001 = sum(M001),
                M002 = sum(M002),
                M003 = sum(M003),
                M004 = sum(M004))
    
    hex_educacao <- left_join(hex, hex2, by = "id_hex") %>%
      mutate(E001 = ifelse(is.na(E001)==T, 0, E001),
             E002 = ifelse(is.na(E002)==T, 0, E002),
             E003 = ifelse(is.na(E003)==T, 0, E003),
             E004 = ifelse(is.na(E004)==T, 0, E004),
             M001 = ifelse(is.na(M001)==T, 0, M001),
             M002 = ifelse(is.na(M002)==T, 0, M002),
             M003 = ifelse(is.na(M003)==T, 0, M003),
             M004 = ifelse(is.na(M004)==T, 0, M004)) %>% st_drop_geometry() %>%
      select(-h3_resolution, -sigla_muni)
    dados_educacao <- hex_educacao
    
  } else if (source_escolas == "censo_escolar"){
    
    file_educacao <- sprintf('../data-raw/educacao/%s/2021/muni_%s_educacao_2021/muni_%s_geocode_2021.rds',source_escolas,  sigla_muni,  sigla_muni)
    dados_educacao <- read_rds(file_educacao) %>%
      # mutate(health_low = ifelse(ATUACAO == "Atencao Basica",
      #                            1, health_low)
      # ) %>%
      mutate(E001 = 1,
             E002 = ifelse(TP_ETAPA_ENSINO == 1,1,0),
             E003 = ifelse(TP_ETAPA_ENSINO == 2,1,0),
             E004 = ifelse(TP_ETAPA_ENSINO == 3,1,0),
             M001 = QT_MAT_BAS + QT_MAT_FUND + QT_MAT_INF+QT_MAT_MED+QT_MAT_PROF,
             M002 = QT_MAT_BAS+QT_MAT_INF,
             M003 = QT_MAT_FUND,
             M004 = QT_MAT_MED+QT_MAT_PROF) %>%
      #                            1, health_low)) %>%
      select(E001, E002, E003, E004, M001, M002,M003,M004)
    
    join_educacao_hex <- sf::st_join(dados_educacao, hex)
    
    hex2 <- join_educacao_hex %>%
      st_drop_geometry() %>%
      group_by(id_hex) %>% 
      summarise(E001 = sum(E001),
                E002 = sum(E002),
                E003 = sum(E003),
                E004 = sum(E004),
                M001 = sum(M001),
                M002 = sum(M002),
                M003 = sum(M003),
                M004 = sum(M004))
    
    hex_educacao <- left_join(hex, hex2, by = "id_hex") %>%
      mutate(E001 = ifelse(is.na(E001)==T, 0, E001),
             E002 = ifelse(is.na(E002)==T, 0, E002),
             E003 = ifelse(is.na(E003)==T, 0, E003),
             E004 = ifelse(is.na(E004)==T, 0, E004),
             M001 = ifelse(is.na(M001)==T, 0, M001),
             M002 = ifelse(is.na(M002)==T, 0, M002),
             M003 = ifelse(is.na(M003)==T, 0, M003),
             M004 = ifelse(is.na(M004)==T, 0, M004)) %>% st_drop_geometry() %>%
      select(-h3_resolution, -sigla_muni)
    dados_educacao <- hex_educacao
    
    
    
  } else {
  
  
  
  file_educacao <- sprintf('../data/educacao/%s/muni_%s_educacao_%s/muni_%s.rds',source,  sigla_muni, source,  sigla_muni)
  dados_educacao <- read_rds(file_educacao) %>% st_drop_geometry()
  
  }
  # mapview(dados_saude, zcol = "S001")
  
  hex5 <- left_join(hex4, dados_educacao, by = "id_hex")
  
  # mapview(hex5, zcol = 'M001')
  
  #falta lazer
  
  file_lazer <- sprintf('../data-raw/lazer/%s/muni_%s_lazer_%s/muni_%s_lazer_%s.rds',source_lazer,
                        sigla_muni, source_lazer,  sigla_muni, source_lazer)
  
  sigla_municipio <- sigla_muni
  decisao_muni <- read_excel('../planilha_municipios.xlsx',
                             sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
  
  dados_lazer <- read_rds(file_lazer) %>% st_transform(decisao_muni$epsg)
  
  if (sigla_muni == "dou"){
    
    dados_lazer_muni <- read_sf('../data-raw/dados_municipais_recebidos/muni_dou/muni_dou.gpkg',
                                layer = "lazer")
    # mapview( dados_lazer_muni) + mapview(dados_lazer)
    dados_lazer <- dados_lazer_muni
    
  }
  
  # dados_lazer2 <- read_sf('../data-raw/dados_municipais_recebidos/muni_pal/muni_pal.gpkg',
  #                        layer = "lazer") %>%
  #   mutate(name = "prefeitura", type = "municipal") %>%
  #   select(name, type, osm_id = Id, geometry = geom) %>% st_transform(decisao_muni$epsg)
  
  # dados_lazer <- rbind(dados_lazer, dados_lazer2)
  
  # mapview(dados_lazer)
  # mapview(dados_saude, zcol = "S001")
  
  #contar lazer no hexagono
  
  ano <- 2019
  file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)
  hex_empty <- read_rds(file_hex) %>% select(id_hex)
  
  hex_lazer <- sf::st_join(hex_empty %>% st_transform(decisao_muni$epsg), dados_lazer)
  
  hex_lazer2 <- hex_lazer %>% mutate(count = ifelse(is.na(osm_id)== TRUE,
                                         0,
                                         1)) %>%
    st_drop_geometry() %>%
    group_by(id_hex) %>% 
    summarise(lazer_tot = sum(count)) %>% distinct(id_hex, .keep_all = T)
  
  if (silga_muni == "dou"){
    hex_lazer2 <- hex_lazer %>% mutate(count = ifelse(is.na(id)== TRUE,
                                                      0,
                                                      1)) %>%
      st_drop_geometry() %>%
      group_by(id_hex) %>% 
      summarise(lazer_tot = sum(count)) %>% distinct(id_hex, .keep_all = T)
  }
  
  
  hex_lazer3 <- left_join(hex,hex_lazer2 , by = "id_hex") %>% select(id_hex, lazer_tot) %>%
    st_drop_geometry()
  
  
  hex_total <- left_join(hex5, hex_lazer3, by = "id_hex")
  
  # mapview(hex_total_sf, zcol = 'lazer_tot')
  
  #paraciclos
  if (sigla_muni == "poa"){
  paraciclos_hex <- read_rds(sprintf('../data/paraciclos/muni_%s/paraciclos_%s.rds', sigla_muni, sigla_muni))
  } else if (sigla_muni == "dou"){
    
    paraciclos <- st_read(
    sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
            sigla_muni,
            sigla_muni),
    layer = "paraciclos")
    
    join_paraciclos_hex <- sf::st_join(paraciclos, hex)
    
    paraciclos_hex <- join_paraciclos_hex %>%
      st_drop_geometry() %>%
      group_by(id_hex) %>% 
      summarise(paraciclos = n())
    
    # hex_paraciclos <- left_join(hex, hex2, by = "id_hex") %>%
    #   mutate(n_jobs = ifelse(is.na(n_jobs)==T,
    #                          0,
    #                          n_jobs)) %>% st_drop_geometry()
    
  }
  
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
  

  sum(hex_total_sf$n_jobs)
  hex_total_sf <- left_join(hex_total, hex_empty, by = "id_hex") %>% st_as_sf()
  # mapview(hex_total_sf)
  #
  
  # teste <- hex_jobs %>% filter(h3_resolution > 0)
  # mapview(teste, zcol = 'n_jobs', alpha.regions = 0.6)
  
  
  #ainda faltando os dados socioeconomicos da microssimulaçao
  
  write_rds(hex_total_sf, sprintf("../data/dados_hex/muni_%s/dados_hex_%s.rds", sigla_muni, sigla_muni))
  

  # mapview(hex_total_sf, zcol = "n_jobs")

  
  
}