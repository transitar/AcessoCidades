#Este script agrega no nível de hexágono os dados de:
#Empregos, Escolas, Equipamentos de Saúde e de Lazer.

#Libera espaço da memória
rm(list = ls(all.names = TRUE)); gc()

#Carrega funções e pacotes necessários
source('./R/fun/setup.R')


#IMPORTANTE
#A fonte dos dados de cada tipo de oportunidade deve ser especificada
#antes de executada a função

#para empregos: source_emp = rais / aop
#para escolas: source_escolas = censo_escolar / aop / muni
#para equipamentos de lazer: source_lazer = osm / muni
#para equipamentos de saude: source_saude = cnes / aop / muni
#ano = ano dos empregos da rais

ano <- 2018;
source_saude <- 'aop';
source_lazer <- 'osm';
source_escolas <- "aop";
source_emp <- "aop"

#função de agregação dos dados de oportunidades no nível do hexágono
infos_to_hex <- function(sigla_muni, ano) {
  
  message(paste("Agregando dados de", munis_list$munis_df$name_muni[which(munis_list$munis_df$abrev_muni==sigla_muni)]))
  #dados da microssimulação
  
  create_diretorios <- function(sigla_muni){
    
    dir.create(sprintf("../data/dados_hex/muni_%s/", sigla_muni), recursive = TRUE)
    
  }
  
  create_diretorios(sigla_muni)
  
  # walk(munis_list$munis_df$abrev_muni, create_diretorios)
  
  save_hex_gpkg <- function(sigla_muni){
    
    hex <- read_rds(sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni))
    write_sf(hex,sprintf('../data/hex_municipio/hex_%s_%s_09.gpkg', ano, sigla_muni))
  }
  # walk(munis_list$munis_df$abrev_muni, save_hex_gpkg)
  save_hex_gpkg(sigla_muni)
  
  data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                 sigla_muni, sigla_muni))
  
  sigla_municipio <- sigla_muni
  decisao_muni <- planilha_municipios %>% filter(sigla_muni == abrev_muni)
  
  epsg <- decisao_muni$epsg
  
  
  
  #empregos
  file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', 2019, sigla_muni)
  hex <- read_rds(file_hex)

  #leitura do shape do município
  file_shape <- sprintf('../data-raw/municipios/%s/municipio_%s_%s.rds', 2019, sigla_muni, 2019)
  shape_muni <- read_rds(file_shape)

  #leitura dos dados de empregos
  if (source_emp == "aop"){
    
    empregos <- aopdata::read_landuse(city = sigla_muni, year = 2019, geometry = TRUE)

    hex3 <- empregos %>% select(n_jobs = T001, id_hex) %>% st_drop_geometry()
    
  } else {
    
  file <- sprintf('../data-raw/empregos/%s_empregos%s.gpkg', sigla_muni, ano)
  empregos <- read_sf(file)
  empregos <- empregos %>% rename(jobs = EMPREGOS)
  sum(empregos$jobs)
  ano <- 2019

  join_jobs_hex <- sf::st_join(st_transform(empregos, epsg), st_transform(hex, epsg))
  
  hex2 <- join_jobs_hex %>%
    st_drop_geometry() %>%
    group_by(id_hex) %>% 
    summarise(n_jobs = sum(jobs))
  
  hex3 <- left_join(hex, hex2, by = "id_hex") %>%
    mutate(n_jobs = ifelse(is.na(n_jobs)==T,
                           0,
                           n_jobs)) %>%
    st_drop_geometry()
  
  }
 
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
    # mapview(dados_saude)
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
  # mapview(dados_saude)
  
  
  if (muni == "dou"){
    
    saude_muni <- read_sf('../data-raw/dados_municipais_recebidos/muni_dou/muni_dou.gpkg',
                          layer = "saude") %>%
      select(S001, S002, S003, S004)
    dados_saude <- saude_muni
    # mapview(saude_muni) + mapview(dados_saude)
  }
  
  join_saude_hex <- sf::st_join(dados_saude %>% st_transform(decisao_muni$epsg),
                                st_transform(hex, decisao_muni$epsg))
  
  # join_saude_hex <- sf::st_join(dados_saude %>% left_join(hex %>% select(id_hex)) %>%
  #                                 st_as_sf() %>% st_transform(decisao_muni$epsg),
  #                               st_transform(hex, decisao_muni$epsg) %>% select(geometry)) %>% distinct(id_hex)
  
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
    
    source <- "aop"
    file_saude <- sprintf('../data/saude/%s/muni_%s_saude_%s/muni_%s.rds',source,  sigla_muni,
                          source,  sigla_muni)
    dados_saude <- read_rds(file_saude) %>% st_drop_geometry()
    
  } else if (source == "muni"){
    
    file_saude <- sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg', sigla_muni, sigla_muni)
    dados_saude <- read_sf(file_saude, layer = "saude") %>%
      mutate(S001 = 1) %>%
      select(S001,
             S002,
             S003,
             S004)
    
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
             E002 = ifelse(QT_MAT_B > 0 | QT_MAT_I > 0,1,0),
             E003 = ifelse(QT_MAT_F > 0,1,0),
             E004 = ifelse(QT_MAT_M > 0 | QT_MAT_P > 0,1,0),
             M001 = QT_MAT_B + QT_MAT_F + QT_MAT_I+QT_MAT_M+QT_MAT_P,
             M002 = QT_MAT_B+QT_MAT_I,
             M003 = QT_MAT_F,
             M004 = QT_MAT_M+QT_MAT_P) %>%
      #                            1, health_low)) %>%
      select(E001, E002, E003, E004, M001, M002,M003,M004)
    # mapview(dados_educacao)
    
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
    
    
    
  } else if(source_escolas == "muni"){
    
    ano = 2021
    file_educacao <- sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg', sigla_muni, sigla_muni)
    dados_educacao <- read_sf(file_educacao, layer = "educacao") %>%
    dados_educacao <- file_educacao %>%
      mutate(E001 = 1) %>%
      select(E001, E002, E003, E004)
    
    join_educacao_hex <- sf::st_join(dados_educacao, hex)
    
    hex2 <- join_educacao_hex %>%
      st_drop_geometry() %>%
      group_by(id_hex) %>% 
      summarise(E001 = sum(E001),
                E002 = sum(E002),
                E003 = sum(E003),
                E004 = sum(E004))
    
    hex_educacao <- left_join(hex, hex2, by = "id_hex") %>%
      mutate(E001 = ifelse(is.na(E001)==T, 0, E001),
             E002 = ifelse(is.na(E002)==T, 0, E002),
             E003 = ifelse(is.na(E003)==T, 0, E003),
             E004 = ifelse(is.na(E004)==T, 0, E004)) %>% st_drop_geometry() %>%
      select(-h3_resolution, -sigla_muni)
    dados_educacao <- hex_educacao
  
  } else {
    
  source <- "aop"
  file_educacao <- sprintf('../data/educacao/%s/muni_%s_educacao_%s/muni_%s.rds',source,  sigla_muni, source,  sigla_muni)
  dados_educacao <- read_rds(file_educacao) %>% st_drop_geometry()
  
  }

  hex5 <- left_join(hex4, dados_educacao, by = "id_hex")
  

  if (source_lazer == "osm") {
  
  file_lazer <- sprintf('../data-raw/lazer/%s/muni_%s_lazer_%s/muni_%s_lazer_%s.rds',source_lazer,
                        sigla_muni, source_lazer,  sigla_muni, source_lazer)
  
  dados_lazer <- read_rds(file_lazer) %>% st_transform(decisao_muni$epsg)
  
  if (sigla_muni == "dou"){
    
    dados_lazer_muni <- read_sf('../data-raw/dados_municipais_recebidos/muni_dou/muni_dou.gpkg',
                                layer = "lazer")
    # mapview( dados_lazer_muni) + mapview(dados_lazer)
    dados_lazer <- dados_lazer_muni
    
  }
  
  #contar lazer no hexagono
  
  ano <- 2019
  file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)
  hex_empty <- read_rds(file_hex) %>% select(id_hex)
  
  hex_lazer <- sf::st_join(hex_empty %>% st_transform(decisao_muni$epsg), dados_lazer)
  
  hex_lazer2 <- hex_lazer %>% mutate(count = ifelse(is.na(type)== TRUE,
                                         0,
                                         1)) %>%
    st_drop_geometry() %>%
    group_by(id_hex) %>% 
    summarise(lazer_tot = sum(count)) %>% distinct(id_hex, .keep_all = T)
  
  if (sigla_muni == "dou"){
    hex_lazer2 <- hex_lazer %>% mutate(count = ifelse(is.na(id)== TRUE,
                                                      0,
                                                      1)) %>%
      st_drop_geometry() %>%
      group_by(id_hex) %>% 
      summarise(lazer_tot = sum(count)) %>% distinct(id_hex, .keep_all = T)
  }
  
  
  hex_lazer3 <- left_join(hex_empty,hex_lazer2 , by = "id_hex") %>% select(id_hex, lazer_tot) %>%
    st_drop_geometry()
  
  
  hex_total <- left_join(hex5, hex_lazer3, by = "id_hex")
  
  
  } else {
    
    file_lazer <- ssprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg', sigla_muni, sigla_muni)
    dados_lazer <- read_sf(file_lazer, layer = "lazer") %>% st_transform(decisao_muni$epsg)

    no <- 2019
    file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)
    hex_empty <- read_rds(file_hex) %>% select(id_hex)
    
    hex_lazer <- sf::st_join(hex_empty %>% st_transform(decisao_muni$epsg), dados_lazer)
    
    hex_lazer2 <- hex_lazer %>% mutate(count = ifelse(is.na(type)== TRUE,
                                                      0,
                                                      1)) %>%
      st_drop_geometry() %>%
      group_by(id_hex) %>% 
      summarise(lazer_tot = sum(count)) %>% distinct(id_hex, .keep_all = T)
    hex_lazer3 <- left_join(hex_empty,hex_lazer2 , by = "id_hex") %>% select(id_hex, lazer_tot) %>%
      st_drop_geometry()
    
    
    hex_total <- left_join(hex5, hex_lazer3, by = "id_hex")
    
  }
  # mapview(hex_total_sf, zcol = 'lazer_tot')
  
  #paraciclos
  
  if (planilha_municipios$paraciclos[which(planilha_municipios$abrev_muni==sigla_muni)]== 1){
  
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
    
  } else if (planilha_municipios$fonte_paraciclos[which(planilha_municipios$abrev_muni==sigla_muni)]=="osm"){
    
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
    
  } else if (planilha_municipios$fonte_paraciclos[which(planilha_municipios$abrev_muni==sigla_muni)]=="muni"){
    
    paraciclos_hex <- read_rds(sprintf('../data/paraciclos/muni_%s/paraciclos_%s.rds', sigla_muni, sigla_muni))
    
  }
  
  # hex_total <- read_rds('../data/dados_hex/muni_poa/dados_hex_poa.rds') %>% st_drop_geometry()
  hex_total <- left_join(hex_total, paraciclos_hex, by = "id_hex")
  
  }
    
  
  
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
     
  dados_bikecomp <- dados_bikecomp %>% st_transform(decisao_muni$epsg)
  
  join_bikecomp_hex <- sf::st_join(dados_bikecomp,st_transform(hex_empty,decisao_muni$epsg ))
  
  hex2 <- join_bikecomp_hex %>%
    st_drop_geometry() %>%
    group_by(id_hex) %>% 
    summarise(n_bikes = n())
     
  hex_total <- left_join(hex_total, hex2, by = "id_hex") %>%
    mutate(n_bikes = ifelse(is.na(n_bikes)==T,
                           0,
                           n_bikes)) %>% st_drop_geometry()
    
   }
  
  
  

  # sum(hex_total_sf$n_jobs)
  hex_total_sf <- left_join(hex_total, hex_empty, by = "id_hex") %>% st_as_sf()
  
  # hex_total_sf <- hex_total
  
  teste <- hex_total_sf 
  # mapview(teste, zcol = "n_jobs")
  # mapview(teste, zcol = "lazer_tot")
  
  # teste <- hex_jobs %>% filter(h3_resolution > 0)
  # mapview(teste, zcol = 'n_jobs', alpha.regions = 0.6)
  
  
  #ainda faltando os dados socioeconomicos da microssimulaçao
  
  write_rds(hex_total_sf, sprintf("../data/dados_hex/muni_%s/dados_hex_%s.rds", sigla_muni, sigla_muni))
  
  # hex <- read_rds(sprintf("../data/dados_hex/muni_%s/dados_hex_%s.rds", sigla_muni, sigla_muni))
  write_sf(hex_total_sf, sprintf("../data/dados_hex/muni_%s/dados_hex_%s.gpkg", sigla_muni, sigla_muni))

  # mapview(hex_total_sf, zcol = "n_jobs")
  
  message(paste("Dados agregados por hexágono de", munis_list$munis_df$name_muni[which(munis_list$munis_df$abrev_muni==sigla_muni)],
                "salvos em", sprintf("../data/dados_hex/muni_%s/dados_hex_%s.gpkg", sigla_muni, sigla_muni)))
  
}

#aplica a função para o município
infos_to_hex(sigla_muni = "bel", ano = 2018)

