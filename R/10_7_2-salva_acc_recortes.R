
source('./R/fun/setup.R')



# Escrita dos dados de acessibilidade por recorte -------------------------



rm(list = ls()); gc()
sigla_muni <- 'vic'

mode1 <- "transit"
oportunidade <- "lazer"
titulo_leg <- "Eq. de lazer"
sigla_op <- "LZ"
# time <- c(15,30,45,60)
time <- 30
type_acc <- "TMI"


#Aplica para todos os municípios

message(paste("Rodando",sigla_muni, "\n"))

dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))

data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                               sigla_muni, sigla_muni))

max_pop <- nrow(data_micro)

data_micro2 <- data_micro %>%
  # filter(code_tract %in% lista_tract) %>%
  select(1:12, V0606, hex) %>%
  mutate(V0606 = as.factor(V0606))


#remocão dos habitantes de cor amarela e indígena
levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")


if (sigla_muni == "dou"){
  
  data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos"))
  
} else {
  
  data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
  
}

dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))


salva_recortes_acc <- function(sigla_muni,
                           type_acc,
                           mode1,
                           oportunidade,
                           sigla_op,
                           titulo_leg,
                           time,
                           cols = 2,
                           width = 14,
                           height = 10){
  
  message(paste("Rodando Gráfixos de",sigla_muni, "\n"))
  
  if (type_acc == "CMA"){
    
    data_acess <- read_rds(sprintf('../r5r/accessibility/muni_%s/acc_%s.rds',
                                   sigla_muni, sigla_muni)) %>% filter(mode == mode1)
    
    
    dados_acc <- left_join(dados_hex, data_acess, by = c("id_hex"="origin")) %>% st_as_sf()
    
  } else if (type_acc == "TMI") {
    
    data_acess_tmi <- read_rds(sprintf('../r5r/accessibility/muni_%s/tmi/acc_tmi_%s.rds',
                                       sigla_muni, sigla_muni)) %>% filter(mode == mode1)
    
    
    dados_acc <- left_join(dados_hex, data_acess_tmi, by = c("id_hex"="origin")) %>% st_as_sf()
    DT <- dados_acc
    
    #inserir drop na em cada gráfico
    dados_acc_maps <- dados_acc %>% mutate_if(is.numeric, list(~na_if(., Inf)))
    dados_acc <- dados_acc_maps
    
    
    if (sigla_muni == "rma"){
      #shapes dos municipios
      arj <- geobr::read_municipality(code_muni = 2800308) %>% select(name_muni)
      nss <- geobr::read_municipality(code_muni = 2804805) %>% select(name_muni)
      bac <- geobr::read_municipality(code_muni = 2800605) %>% select(name_muni)
      sac <- geobr::read_municipality(code_muni = 2806701) %>% select(name_muni)
      
      # mapview(rma)
      
      sigla_municipio <- sigla_muni
      decisao_muni <- read_excel('../planilha_municipios.xlsx',
                                 sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
      # rma <- st_union(arj, nss) %>% st_union(bac) %>% st_union(sac)
      
      dados_acc_aju <- dados_acc %>%  st_transform(decisao_muni$epsg) %>%
        st_intersection(arj %>% st_transform(decisao_muni$epsg)) %>% mutate(sigla_muni = "aju") %>%
        mutate(area = as.numeric(st_area(.)))
      
      dados_acc_nss <- dados_acc %>%  st_transform(decisao_muni$epsg) %>%
        st_intersection(nss %>% st_transform(decisao_muni$epsg)) %>% mutate(sigla_muni = "nss") %>%
        mutate(area = as.numeric(st_area(.)))
      
      dados_acc_bac <- dados_acc %>%  st_transform(decisao_muni$epsg) %>%
        st_intersection(bac %>% st_transform(decisao_muni$epsg)) %>% mutate(sigla_muni = "bac") %>%
        mutate(area = as.numeric(st_area(.)))
      
      dados_acc_sac <- dados_acc %>%  st_transform(decisao_muni$epsg) %>%
        st_intersection(sac %>% st_transform(decisao_muni$epsg)) %>% mutate(sigla_muni = "sac") %>%
        mutate(area = as.numeric(st_area(.)))
      
      dados_acc <- rbind(dados_acc_aju,
                         dados_acc_nss,
                         dados_acc_bac,
                         dados_acc_sac)
      
      dados_acc <- dados_acc %>% group_by(id_hex) %>%
        arrange(-area) %>%
        slice(1) %>%
        ungroup()
      
      # mapview(dados_acc)
    }
    
  }
  
  if (type_acc == "CMA"){
    dados_hex <- readr::read_rds(sprintf("../data/dados_hex/muni_%s/dados_hex_%s.rds",
                                         sigla_muni, sigla_muni))
    
    empregos_tot <- sum(dados_hex$n_jobs, na.rm = T)
    saude_tot <- sum(dados_hex$S001, na.rm = T)
    saude_n1 <- sum(dados_hex$S002, na.rm = T)
    saude_n2 <- sum(dados_hex$S003, na.rm = T)
    saude_n3 <- sum(dados_hex$S004, na.rm = T)
    educacao_tot <- sum(dados_hex$E001, na.rm = T)
    educacao_n1 <- sum(dados_hex$E002, na.rm = T)
    educacao_n2 <- sum(dados_hex$E003, na.rm = T)
    educacao_n3 <- sum(dados_hex$E004, na.rm = T)
    matriculas_tot <- sum(dados_hex$M001, na.rm = T)
    matriculas_n1 <- sum(dados_hex$M002, na.rm = T)
    matriculas_n2 <- sum(dados_hex$M003, na.rm = T)
    matriculas_n3 <- sum(dados_hex$M004, na.rm = T)
    lazer_tot2 <- sum(dados_hex$lazer_tot, na.rm = T)
    paraciclos_tot <- sum(dados_hex$paraciclos, na.rm = T)
    bikes_comp_tot <- sum(dados_hex$n_bikes, na.rm = T)
    
    
    
    acess2 <- dados_acc %>%
      mutate(across(.cols = matches("CMATT"),
                    ~ .x/empregos_tot)) %>%
      mutate(across(.cols = matches("CMAST"),
                    ~ .x/saude_tot)) %>%
      mutate(across(.cols = matches("CMASB"),
                    ~ .x/saude_n1)) %>%
      mutate(across(.cols = matches("CMASM"),
                    ~ .x/saude_n2)) %>%
      mutate(across(.cols = matches("CMASA"),
                    ~ .x/saude_n3)) %>%
      mutate(across(.cols = matches("CMAET"),
                    ~ .x/educacao_tot)) %>%
      mutate(across(.cols = matches("CMAEI"),
                    ~ .x/educacao_n1)) %>%
      mutate(across(.cols = matches("CMAEF"),
                    ~ .x/educacao_n2)) %>%
      mutate(across(.cols = matches("CMAEM"),
                    ~ .x/educacao_n3)) %>%
      mutate(across(.cols = matches("CMAMT"),
                    ~ .x/matriculas_tot)) %>%
      mutate(across(.cols = matches("CMAMI"),
                    ~ .x/matriculas_n1)) %>%
      mutate(across(.cols = matches("CMAMF"),
                    ~ .x/matriculas_n2)) %>%
      mutate(across(.cols = matches("CMAMM"),
                    ~ .x/matriculas_n3)) %>%
      mutate(across(.cols = matches("CMALZ"),
                    ~ .x/lazer_tot2)) %>%
      mutate(across(.cols = matches("CMAPR"),
                    ~ .x/paraciclos_tot)) %>%
      mutate(across(.cols = matches("CMABK"),
                    ~ .x/bikes_comp_tot))
    
    
    acess <- acess2
  } else if (type_acc == "TMI"){
    acess <- dados_acc
  }
  
  if (sigla_muni == "rma"){
    rm(dados_acc_maps); gc()
    
  } else {
    rm(dados_acc, dados_acc_maps); gc()
  }
  
  if (type_acc == "CMA"){
    sigla_munii <- sigla_muni
    # abrir acess
    acess <- acess %>% filter(sigla_muni == sigla_munii)
    cols <- which(names(acess) %in% paste0(type_acc, sigla_op, time))
    acess <- acess %>% filter(mode == mode1) %>%
      select(id_hex, cols)
    acess2 <- acess %>% gather(ind, valor, which(names(acess) %in% paste0(type_acc,sigla_op, time)))
    acess <- acess2
    
    # ajustar levels
    #modificar aqui para compatibilizar com tmi
    acess <- acess %>%
      mutate(ind = factor(ind, 
                          levels = paste0(type_acc,sigla_op, time),
                          labels = paste0(time, " Minutos")))
    
  } else if (type_acc == "TMI"){
    
    if (sigla_muni == "rma"){
      
      sigla_munii <- sigla_muni
      # abrir acess
      # acess <- acess %>% filter(sigla_muni == sigla_munii)
      
      
      cols <- which(names(acess) %in% paste0(type_acc, sigla_op))
      acess <- acess %>% filter(mode == mode1) %>%
        select(id_hex, cols)
      acess2 <- acess %>% gather(ind, valor, which(names(acess) %in% paste0(type_acc,sigla_op)))
      acess <- acess2
      
      
    } else {
      
      sigla_munii <- sigla_muni
      # abrir acess
      acess <- acess %>% filter(sigla_muni == sigla_munii)
      cols <- which(names(acess) %in% paste0(type_acc, sigla_op))
      acess <- acess %>% filter(mode == mode1) %>%
        select(id_hex, cols)
      acess2 <- acess %>% gather(ind, valor, which(names(acess) %in% paste0(type_acc,sigla_op)))
      acess <- acess2
      
    }
    
  }
  #junção com os dados da microssimulação
  
  #Aqui, acess já foi filtrado pelo modo e data_micro já não contém amarelos e indigenas
  data_micro_acc <- data_micro2 %>% left_join(acess, by = c("hex"= "id_hex"))
  
  if (sigla_muni == "rma") {
    
    data_micro_acc <- data_micro_acc %>%
      left_join(dados_acc %>% select(id_hex, sigla_muni) %>% st_drop_geometry(),
                by = c("hex"="id_hex"))
    
  }
  
  rm(acess, acess2); gc()
  #precisa modificar os temas lá em cima para cada nível
  #por hora, funcionando apenas o tema para um único mapa
  # if (type_acc == "CMA") {
  #   
  #   if(length(time) == 1){
  #     # modificar pelo cleveland quando configurá-lo
  #     tema <- theme_bar_plots()
  #     dpi_mapa <- 400
  #   } else if (length(time) == 2){
  #     tema <- theme_for_CMA_2maps()
  #     dpi_mapa <- 500
  #   } else if (length(time) == 4){
  #     tema <- theme_for_CMA_4maps()
  #     dpi_mapa <- 600
  #   }
  #   
  # } else if (type_acc == "TMI") {
  #   tema <- theme_for_tmi
  # }
  
  #plot recorte 1
  if (type_acc == "CMA"){
    
    
    if (sigla_muni == "dou"){
      
      recorte_rr_acc <- data_micro_acc %>%
        # mutate(total = "total") %>% 
        mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
               cor = case_when(V0606 == "Pardos" ~ "Pretos",
                               V0606 == "Pretos" ~ "Pretos",
                               V0606 == "Brancos" ~ "Brancos",
                               V0606 == "Indígenas" ~ "Indígenas")) %>%
        mutate(
          quintil_renda = ntile(Rend_pc, 4)) %>%
        ungroup() %>%
        group_by(cor, quintil_renda, genero) %>% summarise(opp = mean(valor, na.rm = TRUE),
                                                           n =dplyr::n()) %>% 
        drop_na(quintil_renda)  %>%
        mutate(class = paste(genero, cor))
      
    } else {
      
      recorte_rr_acc <- data_micro_acc %>%
        # mutate(total = "total") %>% 
        mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
               cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                               cor == "pretos" ~ "Pretos",
                               cor == "brancos" ~ "Brancos")) %>%
        mutate(
          quintil_renda = ntile(Rend_pc, 4)) %>%
        ungroup() %>%
        group_by(cor, quintil_renda, genero) %>% summarise(opp = mean(valor, na.rm = TRUE),
                                                           n =dplyr::n()) %>% 
        drop_na(quintil_renda)  %>%
        mutate(class = paste(genero, cor))
      
    }
    
    # pop_acc_time <- data_micro_acc %>% filter(valor < time) %>% nrow()
    # pop_total <- nrow(data_micro_acc)
    # percent_acc <- (pop_acc_time/pop_total)*100
    # 
    # print(paste(percent_acc, "da população de", sigla_muni, "tem acesso à", titulo_leg, sigla_op,
    #             "por", mode1,"em até", time, "min"))
    
    
  } else if (type_acc == "TMI") {
    
    if (sigla_muni == "dou"){
      
      recorte_rr_acc <- data_micro_acc %>%
        # mutate(total = "total") %>% 
        mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
               cor = case_when(V0606 == "Pardos" ~ "Pretos",
                               V0606 == "Pretos" ~ "Pretos",
                               V0606 == "Brancos" ~ "Brancos",
                               V0606 == "Indígenas" ~ "Indígenas")) %>%
        mutate(
          quintil_renda = ntile(Rend_pc, 4)) %>%
        ungroup() %>%
        group_by(cor, quintil_renda, genero) %>% summarise(tempo = mean(valor, na.rm = TRUE),
                                                           n = dplyr::n()) %>% 
        drop_na(quintil_renda)  %>%
        mutate(class = paste(genero, cor))
      
      pop_acc_time <- data_micro_acc %>% filter(valor < time) %>% nrow()
      pop_total <- nrow(data_micro_acc)
      percent_acc <- (pop_acc_time/pop_total)*100
      
      print(paste(percent_acc, "da população de", sigla_muni, "tem acesso à", titulo_leg, sigla_op,
                  "por", mode1,"em até", time, "min"))
      
    } else {
      
      # if (sigla_muni == "rma"){
      #   
      #   recorte_rr_acc <- data_micro_acc %>%
      #     # mutate(total = "total") %>% 
      #     mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
      #            cor = case_when(cor == "pard_am_ing" ~ "Pretos",
      #                            cor == "pretos" ~ "Pretos",
      #                            cor == "brancos" ~ "Brancos")) %>%
      #     mutate(
      #       quintil_renda = ntile(Rend_pc, 4)) %>%
      #     ungroup() %>%
      #     group_by(cor, quintil_renda, genero, sigla_muni) %>% summarise(tempo = mean(valor, na.rm = TRUE),
      #                                                                    n = dplyr::n()) %>% 
      #     drop_na(quintil_renda)  %>%
      #     mutate(class = paste(genero, cor))
      #   
      #   pop_acc_time <- data_micro_acc %>% filter(valor < time) %>%
      #     group_by(sigla_muni) %>%
      #     summarise(n = n()) %>%
      #     pull(n)
      #   
      #   pop_total <- data_micro_acc %>%
      #     group_by(sigla_muni) %>%
      #     summarise(n = n() )%>%
      #     pull(n)
      #   
      #   percent_acc <- (pop_acc_time/pop_total)*100
      #   
      #   munis_rma <- c("Aracaju", "NSA Socorro", "Barra dos Coqueiros", "S. Cristóvão")
      #   
      #   print(paste(percent_acc, "da população de", munis_rma, "tem acesso à", titulo_leg, sigla_op,
      #               "por", mode1,"em até", time, "min"))
      #   
      #   
      #   
      # }
      
      recorte_rr_acc <- data_micro_acc %>%
        # mutate(total = "total") %>% 
        mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
               cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                               cor == "pretos" ~ "Pretos",
                               cor == "brancos" ~ "Brancos")) %>%
        mutate(
          quintil_renda = ntile(Rend_pc, 4)) %>%
        ungroup() %>%
        group_by(cor, quintil_renda, genero) %>% summarise(tempo = mean(valor, na.rm = TRUE),
                                                           n = dplyr::n()) %>% 
        drop_na(quintil_renda)  %>%
        mutate(class = paste(genero, cor))
      
      pop_acc_time <- data_micro_acc %>% filter(valor < time) %>% nrow()
      pop_total <- nrow(data_micro_acc)
      percent_acc <- (pop_acc_time/pop_total)*100
      
      print(paste(percent_acc, "da população de", sigla_muni, "tem acesso à", titulo_leg, sigla_op,
                  "por", mode1,"em até", time, "min"))
      
      
    }
  }
  # recorte_rr_acc
  rm(data_micro_acc); gc()
  # %>% group_by(cor, genero) %>% 
  recorte_rr_acc <- recorte_rr_acc %>% mutate(city = sigla_muni,
                                              indicador = sigla_op,
                                              tempo_considerado = time,
                                              modo = mode1)
  if (mode1 == "bike"){
    modo <- "bicicleta"
  } else if (mode1 == "transit"){
    modo <- "transporte_publico"
  } else if (mode1 == "walk"){
    modo <- "caminhada"
  }
  
  suppressWarnings(dir.create(sprintf('../data/ind_desigualdade/muni_%s/dados/',
                                      sigla_muni)))
  
  data.table::fwrite(recorte_rr_acc, sprintf("../data/ind_desigualdade/muni_%s/dados/%s_%s_%s_%s_%s.csv",
                                             sigla_muni, modo, type_acc , oportunidade, sigla_op, paste(time, collapse = '')))
  
  
}

#Aplica função para todos os municípios

# library("future")
# plan(multisession)
# 
# lista_modos <- c(rep("transit", 56), rep("walk", 56), rep("bike", 56))
# 
# lista_tempos <- rep(c(rep(15, 14), rep(30,14), rep(45, 14), rep(60,14)),3)
# 
# lista_oportunidade <- rep(rep(c("empregos",
#                                 rep("matriculas",4),
#                                 rep("escolas", 4),
#                                 rep("saude", 4),
#                                 "lazer"),4),3)
# 
# lista_siglaop <- rep(rep(c("TT",
#                            "MT", "MI", "MF", "MM",
#                            "ET", "EI", "EF", "EM",
#                            "ST", "SB", "SM", "SA",
#                            "LZ"), 4),3)
# 
# # lista_siglaop <- rep(rep(c("TT",
# #                            # "MT", "MI", "MF", "MM",
# #                            "ET", "EI", "EF", "EM",
# #                            "ST", "SB", "SM", "SA",
# #                            "LZ"), 1),3)
# 
# lista_titulo_leg <- rep(rep(c("Empregos",
#                               rep("Matrículas",4),
#                               rep("Escolas", 4),
#                               rep("Eq. de Saúde", 4),
#                               "Eq. de Lazer"), 4),3)
# 
# 
# 
# lista_args <- list(lista_modos, lista_oportunidade, lista_siglaop, lista_titulo_leg, lista_tempos)
# 
# furrr::future_pwalk(.l = lista_args, .f = salva_recortes_acc,
#                     sigla_muni = 'vic',
#                     type_acc = "TMI",
#                     cols = 1,
#                     width = 15,
#                     height = 10)


# Aplicação para TMI ------------------------------------------------------

lista_modos <- c(rep("transit", 9), rep("walk", 9), rep("bike", 9))

lista_oportunidade <- rep(c(
  
  rep("escolas", 4),
  rep("saude", 4),
  "lazer"),3)
# 

lista_siglaop <- rep(c(
  "ET", "EI", "EF", "EM",
  "ST", "SB", "SM", "SA",
  "LZ"), 3)

lista_titulo_leg <- rep(c(
  rep("Escolas", 4),
  rep("Eq. de Saúde", 4),
  "Eq. de Lazer"), 3)

lista_tempos <- c(rep(45, 9), rep(15,9), rep(30, 9))

lista_args <- list(lista_modos, lista_oportunidade, lista_siglaop, lista_titulo_leg, lista_tempos)

seed = TRUE

furrr::future_pwalk(.l = lista_args, .f = salva_recortes_acc,
                    sigla_muni = 'vic',
                    type_acc = "TMI",
                    cols = 1,
                    width = 15,
                    height = 10)




# Agrega resultados por municipios ----------------------------------------

sigla_muni <- "rma"
type_acc <- "CMA"

agrega_dados <- function(sigla_muni, type_acc) {
  
files <- list.files(sprintf('../data/ind_desigualdade/muni_%s/dados/', sigla_muni), pattern = type_acc, full.names = T)

dados_muni <- map_df(.x = files, .f = fread) %>% mutate(tipo_acc = type_acc)

data.table::fwrite(dados_muni, sprintf('../data/ind_desigualdade/muni_%s/recorte_%s_all.csv', sigla_muni, type_acc))

}

library(future)
plan(multisession)

munis <- rep(munis_list$munis_df$abrev_muni,2)
tipos <- c(rep("CMA",11), rep("TMI", 11))
furrr::future_walk2(.x = munis, .y = tipos, .f = agrega_dados)


# Agrega resultados completos ---------------------------------------------
ler_muni <- function(sigla_muni) {
all_cma <- fread(sprintf('../data/ind_desigualdade/muni_%s/recorte_cma_all.csv', sigla_muni))
}
dados_all_cma <- map_df(.x = munis_list$munis_df$abrev_muni, .f = ler_muni)
data.table::fwrite(dados_all_cma, '../data/ind_desigualdade/recorte_cma_all_munis.csv')

ler_muni_tmi <- function(sigla_muni) {
  all_cma <- fread(sprintf('../data/ind_desigualdade/muni_%s/recorte_TMI_all.csv', sigla_muni))
}
dados_all_ymi <- map_df(.x = munis_list$munis_df$abrev_muni, .f = ler_muni_tmi)
data.table::fwrite(dados_all_ymi, '../data/ind_desigualdade/recorte_tmi_all_munis.csv')



# Agrega resultados de recortes e gini ------------------------------------

dados_all_gini <- function(sigla_muni){
  
  all_gini <- read_xlsx(sprintf('../data/ind_desigualdade/muni_%s/muni_%s_desigualdade_cma_all.xlsx',
                                sigla_muni, sigla_muni)) %>% select(2:5) %>%
    mutate(city = sigla_muni)
  
}
dados_all_gini <- map_df(.x = munis_list$munis_df$abrev_muni, .f = dados_all_gini)

dados_all_razoes <- function(sigla_muni){
  
  all_razoes <- read_xlsx(sprintf('../data/ind_desigualdade/muni_%s/razoes_acc_recortes.xlsx',
                                sigla_muni)) %>% select(2:7)
  
}
dados_all_razoes <- map_df(.x = munis_list$munis_df$abrev_muni, .f = dados_all_razoes)


dados_all_gini <- dados_all_gini %>% rename(modo = mode) %>% mutate(acc_ind = "CMA")

times <- c(45,30,15)

dados_all_gini_2 <- dados_all_gini %>%
  
  filter(indicador %in% ifelse(modo == "transit", c(
    paste0("CMATT", times[1]),
    paste0("CMALZ", times[1]),
    # "CMAMT45",
    paste0("CMAEI", times[1]),
    paste0("CMAEF", times[1]),
    paste0("CMAEM", times[1]),
    paste0("CMASB", times[1]),
    paste0("CMASM", times[1]),
    paste0("CMASA", times[1])
    # "CMAST45",
    # "CMATT45"
  ), ifelse(modo == "bike", c(
    paste0("CMATT", times[2]),
    paste0("CMALZ", times[2]),
    paste0("CMAEI", times[2]),
    paste0("CMAEF", times[2]),
    paste0("CMAEM", times[2]),
    paste0("CMASB", times[2]),
    paste0("CMASM", times[2]),
    paste0("CMASA", times[2])
    # "CMAMT30",
    # "CMAST30",
    # "CMATT30"
  ),c(
    paste0("CMATT", times[3]),
    paste0("CMALZ", times[3]),
    paste0("CMAEI", times[3]),
    paste0("CMAEF", times[3]),
    paste0("CMAEM", times[3]),
    paste0("CMASB", times[3]),
    paste0("CMASM", times[3]),
    paste0("CMASA", times[3])
    
    # "CMAST15",
    # "CMATT15"
  )))) %>%
  
  mutate(indicador = substr(indicador, start = 1L, stop = 5L),
         largura = ifelse(indicador == "CMASA" | indicador == "CMASM", 1,
                          ifelse(indicador == "CMASB", 1,1)))

dados_all_gini_2$modo <- factor(dados_all_gini_2$modo,
                         levels = c("walk", "bike", "transit"),
                         labels = c(paste0("Caminhada (",times[3]," min)"),
                                    paste0("Bicicleta (",times[2]," min)"),
                                    paste0("Transporte Público (",times[1]," min)")))

dados_all_gini_2$indicador <- factor(dados_all_gini_2$indicador, levels = c("CMATT",
                                                              "CMAEI",
                                                              "CMAEF",
                                                              "CMAEM",
                                                              "CMASB",
                                                              "CMASM",
                                                              "CMASA",
                                                              "CMALZ"))
dados_all_gini_2$city <- factor(x = dados_all_gini_2$city, levels = c(
  "rma",
  "vic",
  "slz",
  "man",
  "pal",
  "bel", 
  "dou",
  "cit",
  "con",
  "poa",
  "noh"), labels = c(
    "R.M. Aracaju",
    "V. Conquista",
    "São Luís",
    "Manaus",
    "Palmas",
    "Belém",
    "Dourados",
    "C. Itapemirim",
    "Contagem",
    "Porto Alegre",
    "Novo Hamburgo"
  ))


write.xlsx(dados_all_gini_2, '../data/ind_desigualdade/dados_gini_palma_grafico.xlsx')

dados_razoes_spr <- dados_all_razoes %>% spread(recorte, media) %>% arrange(city, modo, indicador)

dados_razoes_spr <- dados_razoes_spr %>%

  filter(indicador %in% ifelse(modo == "transit", c(
    paste0("CMATT", times[1]),
    paste0("CMALZ", times[1]),
    # "CMAMT45",
    paste0("CMAEI", times[1]),
    paste0("CMAEF", times[1]),
    paste0("CMAEM", times[1]),
    paste0("CMASB", times[1]),
    paste0("CMASM", times[1]),
    paste0("CMASA", times[1])
    # "CMAST45",
    # "CMATT45"
  ), ifelse(modo == "bike", c(
    paste0("CMATT", times[2]),
    paste0("CMALZ", times[2]),
    paste0("CMAEI", times[2]),
    paste0("CMAEF", times[2]),
    paste0("CMAEM", times[2]),
    paste0("CMASB", times[2]),
    paste0("CMASM", times[2]),
    paste0("CMASA", times[2])
    # "CMAMT30",
    # "CMAST30",
    # "CMATT30"
  ),c(
    paste0("CMATT", times[3]),
    paste0("CMALZ", times[3]),
    paste0("CMAEI", times[3]),
    paste0("CMAEF", times[3]),
    paste0("CMAEM", times[3]),
    paste0("CMASB", times[3]),
    paste0("CMASM", times[3]),
    paste0("CMASA", times[3])
    
    # "CMAST15",
    # "CMATT15"
  )))) %>%
  mutate(indicador = substr(indicador, start = 1L, stop = 5L),
         largura = ifelse(indicador == "CMASA", 1,
                          ifelse(indicador == "CMASB", 1,1)))
dados_razoes_spr$modo <- factor(dados_razoes_spr$modo,
                                      levels = c("walk", "bike", "transit"),
                                      labels = c(paste0("Caminhada (",times[3]," min)"),
                                                 paste0("Bicicleta (",times[2]," min)"),
                                                 paste0("Transporte Público (",times[1]," min)")))
dados_razoes_spr$indicador <- factor(dados_razoes_spr$indicador, levels = c("CMATT","CMAEI", "CMAEF", "CMAEM", "CMASB", "CMASM", "CMASA", "CMALZ"))

dados_razoes_spr$city <- factor(x = dados_razoes_spr$city, levels = c(
  "rma",
  "vic",
  "slz",
  "man",
  "pal",
  "bel", 
  "dou",
  "cit",
  "con",
  "poa",
  "noh"), labels = c(
    "R.M. Aracaju",
    "V. Conquista",
    "São Luís",
    "Manaus",
    "Palmas",
    "Belém",
    "Dourados",
    "C. Itapemirim",
    "Contagem",
    "Porto Alegre",
    "Novo Hamburgo"
  ))


write.xlsx(dados_razoes_spr, '../data/ind_desigualdade/dados_recortes_grafico.xlsx')

dados_razoes_spr2 <- dados_razoes_spr %>% select(-largura, -acc_ind) %>% mutate(chave = paste0(city, modo, indicador)) %>%
  select(chave, cor, genero, responsavel)

dados_total_graficos <- dados_all_gini_2 %>%
  # select(-dados_all_gini_2) %>%
  mutate(chave = paste0(city, modo, indicador)) %>%
  left_join(dados_razoes_spr2, by = "chave") %>% select(-chave, -largura)

xlsx::write.xlsx(dados_total_graficos, '../data/ind_desigualdade/dados_total_grafico.xlsx')



dados_razoes_spr$city <- factor(x = dados_razoes_spr$city, levels = c(
  "R.M. Aracaju",
  "V. Conquista",
  "São Luís",
  "Manaus",
  "Palmas",
  "Belém",
  "Dourados",
  "C. Itapemirim",
  "Contagem",
  "Porto Alegre",
  "Novo Hamburgo"), labels = c(
    "R.M. Aracaju",
    "V. Conquista",
    "São Luís",
    "Manaus",
    "Palmas",
    "Belém",
    "Dourados",
    "C. Itapemirim",
    "Contagem",
    "Porto Alegre",
    "Novo Hamburgo"
  ))


