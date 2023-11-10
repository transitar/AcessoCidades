# rm(list = ls());gc()

source('./R/fun/setup.R')
sf_use_s2(TRUE)

sigla_muni <- 'con'

salva_dados_tp <- function(sigla_muni, width = 16.5, height = 16.5){
  
  message(paste("Rodando",sigla_muni, "\n"))
  
  dados_hex <- read_rds(sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni))
  
  sigla_municipio <- sigla_muni
  decisao_muni <- read_excel('../planilha_municipios.xlsx',
                             sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
  
  if (decisao_muni$fonte_dados_tp == "muni_shape"){
    
    dados_peds <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                  sigla_muni, sigla_muni),
                          layer = "tp_peds")
    
    dados_linhas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                    sigla_muni, sigla_muni),
                            layer = "tp_linhas")
  }
  
  
  dados_peds <- dados_peds %>% st_transform(decisao_muni$epsg)
  dados_linhas <- dados_linhas %>% st_transform(decisao_muni$epsg)
  
  dados_peds_buffer_300 <- dados_peds %>% st_buffer(300) %>% st_union() %>% st_as_sf()

  dados_peds_buffer_500 <- dados_peds %>% st_buffer(500) %>% st_union() %>% st_as_sf()

  dados_linhas_buffer_300 <- dados_linhas %>% st_buffer(300) %>% st_union() %>% st_as_sf()

  dados_linhas_buffer_500 <- dados_linhas %>% st_buffer(500) %>% st_union() %>% st_as_sf()
  
  dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                      sigla_muni, sigla_muni))
  data_micro2 <- dados_simulacao %>%
    # filter(code_tract %in% lista_tract) %>%
    select(1:12, V0606, hex) %>%
    mutate(V0606 = as.factor(V0606))
  
  #remocão dos habitantes de cor amarela e indígena
  levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
  data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
  
  pop_counts <- dados_simulacao %>%
    group_by(hex) %>%
    summarise(pop_total = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
    st_as_sf() %>% mutate(quintil = as.factor(ntile(pop_total, 4)))
  
  dados_hex_intersect_300 <- dados_hex %>%
    st_as_sf() %>%
    st_transform(decisao_muni$epsg) %>%
    st_intersection(dados_peds_buffer_300)
  dados_hex_intersect_300 <- dados_hex_intersect_300 %>%
    mutate(area = st_area(.)) %>%
    mutate(area = as.numeric(area)) %>%
    filter(area >= 60000)
  
  id_hex_intersects_bus <- dados_hex_intersect_300$id_hex %>% unique()
  
  #dados da microssimulação
  
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
  
  
  data_micro_bus <- data_micro2 %>% filter(hex %in% id_hex_intersects_bus)
  
  if (sigla_muni == "dou"){
    
    
    recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(V0606 == "Pardos" ~ "Pretos",
                             V0606 == "Pretos" ~ "Pretos",
                             V0606 == "Brancos" ~ "Brancos",
                             V0606 == "Indígenas" ~ "Indígenas")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
      mutate(class = paste(genero, cor))%>%
      mutate(id = paste(class, quintil_renda))
    
    
  } else {
    
    
    recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
      mutate(class = paste(genero, cor))%>%
      mutate(id = paste(class, quintil_renda))
    
  }
  
  recorte_renda <- recorte_rr %>%
    ungroup() %>% group_by(quintil_renda) %>%
    summarise(prop = weighted.mean(prop, w = n))
  
  recorte_cor_renda <- recorte_rr %>%
    ungroup() %>% group_by(quintil_renda, cor) %>%
    summarise(prop = weighted.mean(prop, w = n))
  
  resumo.infra_tp300 <- weighted.mean(recorte_rr$prop, w = recorte_rr$n)
  
  recorte_cobertura_300 <- recorte_rr %>% select(-id) %>%
    mutate(tipo = "cobert_tp_300m",
           city = sigla_muni)
  
  dados_hex_intersect_500 <- dados_hex %>%
    st_as_sf() %>%
    st_transform(decisao_muni$epsg) %>%
    st_intersection(dados_peds_buffer_500)
  dados_hex_intersect_500 <- dados_hex_intersect_500 %>%
    mutate(area = st_area(.)) %>%
    mutate(area = as.numeric(area)) %>%
    filter(area >= 60000)
  id_hex_intersects_bus500 <- dados_hex_intersect_500$id_hex %>% unique()
  
  #dados da microssimulação
  
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
  
  data_micro_bus <- data_micro2 %>% filter(hex %in% id_hex_intersects_bus)
  
  recorte_rr500 <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus500), "OK","N"))  %>% 
    # mutate(total = "total") %>% 
    mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
           cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                           cor == "pretos" ~ "Pretos",
                           cor == "brancos" ~ "Brancos")) %>%
    mutate(
      quintil_renda = ntile(Rend_pc, 4)) %>%
    group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
    ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
    summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
    mutate(class = paste(genero, cor))%>%
    mutate(id = paste(class, quintil_renda))
  
  resumo.infra_tp500 <- weighted.mean(recorte_rr500$prop, recorte_rr500$n)
  
  recorte_cobertura_500 <- recorte_rr500 %>% select(-id) %>%
    mutate(tipo = "cobert_tp_500m",
           city = sigla_muni)
  
  dados_pacial <- rbind(recorte_cobertura_300, recorte_cobertura_500)
  
  
  frequencias <- read_rds(sprintf('../data/tp_frequencias/muni_%s/rotas_freq_%s.rds',
                                  sigla_muni, sigla_muni))
  
  frequencias2 <- frequencias %>% mutate(cond = case_when(headway_medio <= 15 ~ '< 15 minutos',
                                                          headway_medio <= 30 ~ '15-30 minutos',
                                                          headway_medio <= 60 ~ '30-60 minutos',
                                                          headway_medio > 60 ~ '>60 minutos'
  )) %>% arrange(headway_medio) %>%
    mutate(cond = factor(cond, levels = c('< 15 minutos',
                                          '15-30 minutos',
                                          '30-60 minutos',
                                          '>60 minutos')))
  viagens <- read_rds(sprintf('../data/tp_frequencias/muni_%s/rotas_viagens_%s.rds',
                              sigla_muni, sigla_muni)) %>%
    # mutate(headway_medio = 60/n_viagens_h) %>%
    mutate(headway_medio = ifelse(headway_medio >30 , 31, headway_medio)) %>%
    mutate(cond = case_when(headway_medio <= 5 ~ '< 5 minutos',
                            headway_medio <= 10 ~ '5-10 minutos',
                            headway_medio <= 15 ~ '10-15 minutos',
                            headway_medio > 25 ~ '>25 minutos'))
  
  # viagens <- read_rds(sprintf('../data/tp_frequencias/muni_%s/rotas_freq_%s.rds',
  #                             sigla_muni, sigla_muni)) %>%
  #   # mutate(headway_medio = 60/n_viagens_h) %>%
  #   mutate(headway_medio = ifelse(headway_medio >30 , 31, headway_medio)) %>%
  #   mutate(cond = case_when(headway_medio <= 5 ~ '< 5 minutos',
  #                           headway_medio <= 10 ~ '5-10 minutos',
  #                           headway_medio <= 15 ~ '10-15 minutos',
  #                           headway_medio > 25 ~ '>25 minutos'))
  
  
  if (sigla_muni == "dou"){
    
    recorte_rr_h <- data_micro2 %>%
      # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
      left_join(viagens %>% st_drop_geometry(), by = c("hex"="id_hex")) %>%
      drop_na(headway_medio) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4))%>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(V0606 == "Pardos" ~ "Pretos",
                             V0606 == "Pretos" ~ "Pretos",
                             V0606 == "Brancos" ~ "Brancos",
                             V0606 == "Indígenas" ~ "Indígenas")) %>%
      group_by(cor, quintil_renda, genero) %>%
      summarise(sd = sd(headway_medio),
                headway_medio = mean(headway_medio, na.rm =T),
                n = n()) %>%
      mutate(id = paste(genero, cor))
    
    
  } else {
    
    recorte_rr_h <- data_micro2 %>%
      # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
      left_join(viagens %>% st_drop_geometry(), by = c("hex"="id_hex")) %>%
      drop_na(headway_medio) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4))%>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      group_by(cor, quintil_renda, genero) %>%
      summarise(sd = sd(headway_medio),
                headway_medio = mean(headway_medio, na.rm =T),
                n = n()) %>%
      mutate(id = paste(genero, cor))

  }
  
  recorte_rr_h <- recorte_rr_h %>% select(-id, -sd, prop = headway_medio) %>%
    mutate(tipo = "headway_tp",
           city = sigla_muni)
  
  
  

# Renda -------------------------------------------------------------------

  tarifa <- as.numeric(decisao_muni$tarifa) * 60
  sm <- 1302
  
  if (sigla_muni == "dou"){
    
    recorte_tarifa <- data_micro2 %>%
      filter(Rend_pc >0) %>%
      mutate(tarifa_renda = (tarifa)/(Rend_pc*1302)) %>%
      # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(V0606 == "Pardos" ~ "Pretos",
                             V0606 == "Pretos" ~ "Pretos",
                             V0606 == "Brancos" ~ "Brancos",
                             V0606 == "Indígenas" ~ "Indígenas")) %>%
      group_by(cor, quintil_renda, genero) %>%
      summarise(tarifa_grupo = mean(tarifa_renda, na.rm = T),
                n = n()) %>%
      mutate(id = paste(genero, cor),
             ID_juntar = paste(genero, cor, quintil_renda))
    
  } else {
    
    recorte_tarifa <- data_micro2 %>%
      filter(Rend_pc >0) %>%
      mutate(tarifa_renda = (tarifa)/(Rend_pc*1302)) %>%
      # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      group_by(cor, quintil_renda, genero) %>%
      summarise(tarifa_grupo = mean(tarifa_renda, na.rm = T),
                n = n()) %>%
      mutate(id = paste(genero, cor),
             ID_juntar = paste(genero, cor, quintil_renda))
    
  }
  
  
  renda_pnad <- read.csv('../data/pnadc/rendas_medias_quartil_capitais_pnadc_2022_2.csv')
  
  if (munis_pnad$pnad[which(munis_pnad$muni==sigla_muni)] == 1){
  
  renda_pnad <- renda_pnad %>% filter(Capital == "Município de Manaus (AM)") %>%
    mutate(Quartil = case_when(Quartil == "q1" ~ 1,
                               Quartil == "q2" ~ 2,
                               Quartil == "q3" ~ 3,
                               Quartil == "q4" ~ 4),
           Raca = case_when(Raca == "Branca" ~ "Brancos",
                            Raca == "Negra" ~ "Pretos"),
           Genero = case_when(Genero == "Homem" ~ "Homens",
                              Genero == "Mulher" ~ "Mulheres")) %>%
    mutate(ID_juntar = paste(Genero, Raca, Quartil),
           tarifa_grupo = tarifa/mean) %>%
    select(genero = Genero, cor = Raca, quintil_renda = Quartil, tarifa_grupo, ID_juntar)
  
  recorte_tarifa <- data_micro2 %>%
    filter(Rend_pc >0) %>%
    mutate(tarifa_renda = (tarifa)/(Rend_pc*1302)) %>%
    # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
    mutate(
      quintil_renda = ntile(Rend_pc, 4)) %>%
    # mutate(total = "total") %>% 
    mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
           cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                           cor == "pretos" ~ "Pretos",
                           cor == "brancos" ~ "Brancos")) %>%
    group_by(cor, quintil_renda, genero) %>%
    summarise(tarifa_grupo = mean(tarifa_renda, na.rm = T),
              n = n()) %>%
    mutate(id = paste(genero, cor),
           ID_juntar = paste(genero, cor, quintil_renda)) %>%
    ungroup() %>%
    select(ID_juntar, id, n) %>% left_join(renda_pnad, by = "ID_juntar") %>%
    select(-ID_juntar)
  
  }
  
  recorte_tarifa <- recorte_tarifa %>% select(-id, prop = tarifa_grupo) %>%
    mutate(tipo = "tarifa_tp",
           city = sigla_muni)
  
  dados_total <- rbind(dados_pacial, recorte_rr_h, recorte_tarifa) %>% select(-class)
  
  fwrite(dados_total,sprintf('../data/map_plots_transports/muni_%s/dados/cobertura_tp.csv', sigla_muni))
    
  
}

tictoc::tic()
walk(munis_list$munis_df$abrev_muni[10:11], .f = salva_dados_tp)
tictoc::toc()


junta_tudo <- function(sigla_muni){
  
  ciclo <- fread(sprintf('../data/map_plots_transports/muni_%s/dados/cobertura_ciclo.csv', sigla_muni)) %>% select(-class)
  if (sigla_muni != "vic"){
    
  tp <- fread(sprintf('../data/map_plots_transports/muni_%s/dados/cobertura_tp.csv', sigla_muni))
  
  if (ncol(tp)==8){
  tp <- tp %>% select(-ID_juntar)
  }
  
  dados <- rbind(ciclo, tp)
  } else {
    dados <- ciclo
  }
  
  return(dados)
}

plan(multisession)

tictoc::tic()
dados_munis <- furrr::future_map(.x = munis_list$munis_df$abrev_muni, .f = junta_tudo)
tictoc::toc()

dados_munis_df <- rbindlist(dados_munis)

fwrite(dados_munis_df, '../data/tp_cobertura/dados_all.csv')


