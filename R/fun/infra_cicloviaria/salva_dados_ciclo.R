rm(list = ls()); gc()
source('./R/fun/setup.R')

sigla_muni <- 'rma'
aprox_muni <- 0

salva_dados_ciclo <- function(sigla_muni, width = 16.5, height = 16.5){
  
  message(paste("Rodando",sigla_muni, "\n"))

  dados_hex <- read_rds(sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni))
  
  sigla_municipio <- sigla_muni
  
  decisao_muni <- read_excel('../planilha_municipios.xlsx',
                             sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
  
  if (decisao_muni$fonte_ciclo == "muni"){
                               
                               dados_ciclovias <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                                                  sigla_muni, sigla_muni),
                                                          layer = "infra_cicloviaria"
                               ) %>% st_as_sf() %>% st_zm(drop = T)
                             } else if (decisao_muni$fonte_ciclo == "osm") {
                               
                               dados_ciclovias <- read_sf(sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                                                                  sigla_muni, sigla_muni),
                                                          layer = "infra_cicloviaria"
                               ) %>% st_zm(drop = T)
                               
                               
                             }
  # mapview(dados_ciclovias)
  dados_ciclovias_length <- dados_ciclovias %>%
    mutate(length = as.numeric(st_length(.)))
  sum(dados_ciclovias_length$length)
  
  if (sigla_muni == "poa") {
    dados_ciclovias <- dados_ciclovias %>% st_transform(decisao_muni$epsg) %>%
      mutate(Tipo = ifelse(TIPO == "CICLOVIA",
                           "Ciclovia",
                           ifelse(TIPO == "CICLOFAIXA",
                                  "Ciclofaixa",
                                  "Compartilhado")))
  } else if (sigla_muni == 'pal'){
    
    dados_ciclovias <- dados_ciclovias %>% st_transform(decisao_muni$epsg) %>%
      mutate(Tipo = "Ciclovia/Ciclofaixa")
  } else {
    dados_ciclovias <- dados_ciclovias %>% st_transform(decisao_muni$epsg)
    
  }
  
  dados_ciclovias_buffer <- dados_ciclovias %>% st_transform(decisao_muni$epsg) %>%
    st_buffer(300) %>% st_union() %>% st_as_sf()
  dados_ciclovias_buffer2 <- dados_ciclovias_buffer %>% as.data.frame()
  
  dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                      sigla_muni, sigla_muni))
  # rm(dados_simulacao); gc()
  
  
  pop_counts <- dados_simulacao %>%
    group_by(hex) %>%
    summarise(pop_total = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
    st_as_sf() %>% mutate(quintil = as.factor(ntile(pop_total, 4)))
  
  dados_hex_intersect <- dados_hex %>%
    st_as_sf() %>%
    st_transform(decisao_muni$epsg) %>%
    st_intersection(dados_ciclovias_buffer)
  dados_hex_intersect <- dados_hex_intersect %>%
    mutate(area = st_area(.)) %>%
    mutate(area2 = as.numeric(area)) %>%
    filter(area2 > 60000.0)
  
  # mapview(dados_hex_intersect)
  
  id_hex_intersects <- dados_hex_intersect$id_hex %>% unique()
  
  data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                 sigla_muni, sigla_muni)) %>%  mutate(V0606 = case_when(V0606 == 1 ~ "Brancos",
                                                                                        V0606 == 2 ~ "Pretos",
                                                                                        V0606 == 3 ~ "Amarelos",
                                                                                        V0606 == 4 ~ "Pardos",
                                                                                        V0606 == 5 ~ "Indígenas"))
  data_micro2 <- data_micro %>% select(1:12, V0606, hex) %>%
    mutate(V0606 = as.factor(V0606))
  
  if (sigla_muni == "dou"){
    
    data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos"))
    
  } else {
    
    data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
    
  }
  
  if (sigla_muni == "dou"){
    
    recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(V0606 == "Pardos" ~ "Pretos",
                             V0606 == "Pretos" ~ "Pretos",
                             V0606 == "Brancos" ~ "Brancos",
                             V0606 == "Indígenas" ~ "Indígenas")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      # quintil_renda = case_when(Rend_pc < 7.1 ~ 1,
      #                           Rend_pc < 60 ~ 2,
      #                           Rend_pc < 398 ~ 3,
      #                           Rend_pc >399 ~ 4)) %>%
      group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
      mutate(class = paste(genero, cor))%>%
      mutate(id = paste(class, quintil_renda))
    
    para_completar_recorte_rr <- data.frame(cor = rep("Indígenas",3),
                                            quintil_renda = c(2,3,3),
                                            genero = c(rep("Mulheres",2), "Homens"),
                                            prop = rep(0,3),
                                            n = rep(0,3),
                                            class = c(rep("Mulheres Indígenas",2), "Homens Indígenas"),
                                            id = c("Mulheres Indígenas 2","Mulheres Indígenas 3", "Homens Indígenas 3"))
    recorte_rr <- rbind(recorte_rr, para_completar_recorte_rr)
    
  } else {
    
    recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      # quintil_renda = case_when(Rend_pc < 7.1 ~ 1,
      #                           Rend_pc < 60 ~ 2,
      #                           Rend_pc < 398 ~ 3,
      #                           Rend_pc >399 ~ 4)) %>%
      group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
      mutate(class = paste(genero, cor))%>%
      mutate(id = paste(class, quintil_renda))
    
  }
  #% da população total atendida
  
  resumo.infra_clico <- weighted.mean(recorte_rr$prop, w = recorte_rr$n)
  
  recorte_rr <- recorte_rr %>% select(-id) %>%
    mutate(tipo = "cobert_ciclo",
           city = sigla_muni)
  
  dir.create(sprintf('../data/map_plots_transports/muni_%s/dados/', sigla_muni), recursive = T)
  
  fwrite(recorte_rr, sprintf('../data/map_plots_transports/muni_%s/dados/cobertura_ciclo.csv', sigla_muni))
  
  rm(recorte_rr, data_micro2, data_micro, dados_simulacao); gc()
  
}

# plan(multisession)

tictoc::tic()
walk(.x = munis_list$munis_df$abrev_muni, .f = salva_dados_ciclo)
tictoc::toc()