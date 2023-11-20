#~~~~~~~~~~~~~~~~~~~~~~~~~~

#Separa dados de estabelecimentos públicos de educação para um município e georreferencia

source('./R/fun/setup.R')

ano <- 2021

educacao_filter <- function(ano, munis = "all", download = FALSE) {
  
  
  matriculas <- fread(sprintf('../data-raw/censo_escolar/%s/dados/microdados_ed_basica_%s.csv',
                              ano, ano)) %>%
    select(c("CO_ENTIDADE", "TP_DEPENDENCIA", #"TP_ETAPA_ENSINO", 
             "IN_REGULAR", "IN_PROF", "QT_MAT_BAS", "QT_MAT_INF", "QT_MAT_FUND",
             "QT_MAT_MED", "QT_MAT_PROF")) %>%
    mutate(TP_ETAPA_ENSINO = case_when(QT_MAT_INF > 0 ~ 1,
                                       QT_MAT_FUND > 0 ~ 2,
                                       QT_MAT_MED > 0 ~ 3,
                                       QT_MAT_PROF > 0 ~ 3))
  
  # selecionar somente matriculas regulares
  matriculas <- matriculas[IN_REGULAR == 1 | IN_PROF == 1]
  # selecionar somente matriculas em escolas publicas
  matriculas <- matriculas[TP_DEPENDENCIA %in% c(1, 2, 3)]
  
  # colunas de interesse: 
  colunas <- c(c("CO_ENTIDADE", "NO_ENTIDADE","NO_MUNICIPIO", "NO_UF","CO_MUNICIPIO",
                 "DS_ENDERECO", "NU_ENDERECO", "DS_COMPLEMENTO",
                 "NO_BAIRRO", "CO_CEP",
                 "IN_LOCAL_FUNC_UNID_PRISIONAL", "IN_LOCAL_FUNC_PRISIONAL_SOCIO", # escolas prisionais
                 "IN_REGULAR", "IN_PROF", "IN_EJA",
                 "TP_DEPENDENCIA", "TP_SITUACAO_FUNCIONAMENTO"), 
               ifelse(ano == 2017, "NU_FUNCIONARIOS", "QT_FUNCIONARIOS"))
  
  escolas <- fread(sprintf("../data-raw/censo_escolar/%s/dados/microdados_ed_basica_%s.csv", ano, ano),
                   select = colunas)

  # format columns
  escolas <- janitor::clean_names(escolas)
  
  # filter municipalties
  muni_list <- munis_list$munis_metro$code_muni %>% unlist()
  escolas_munis <- escolas[co_municipio %in% muni_list]
  
  # only public
  escolas_munis <- escolas_munis[tp_dependencia %in% c(1, 2, 3)]
  
  # only active
  escolas_munis <- escolas_munis[tp_situacao_funcionamento == 1]
  
  # selecionar somente escola com ensino regular
  escolas_munis <- escolas_munis[in_regular == 1 | in_prof == 1]
  
  # Identifica codigo das escolas priosionais
  escolas_prisionais <- subset(escolas_munis, in_local_func_unid_prisional ==1 | in_local_func_prisional_socio ==1)$co_entidade
  
  # remove escolas prisionais
  escolas_fim <- subset(escolas_munis, co_entidade %nin% escolas_prisionais)
  escolas_fim$in_local_func_unid_prisional <- NULL
  escolas_fim$in_local_func_prisional_socio <- NULL
 
  # 3) trazer matriculas -------------------------------------------------------
  
  # usando inner_join para manter apenas escolas com matriculas que nao sejam EJA
  
  escolas_fim_matriculas <- left_join(escolas_fim, matriculas, by = c("co_entidade"="CO_ENTIDADE"))
  
  #coluna com o endereço completo
  
  escolas_fim_mat_end <- escolas_fim_matriculas %>% 
    mutate(adress = paste(ds_endereco, nu_endereco, ds_complemento, no_bairro, co_cep),
           city = no_municipio,
           state = no_uf,
           country = "Brasil")
  
  create_diretorios <- function(sigla_muni){
    
    dir.create(sprintf("../data-raw/educacao/censo_escolar/%s/muni_%s_educacao_%s/",ano, sigla_muni, ano), recursive = TRUE)
    
  }
  
  walk(munis_list$munis_df$abrev_muni, create_diretorios)
  
  
  library(ggmap)
  chave_google_maps <- readline(prompt = "API key google maps: ")
  
  if (chave_google_maps == ""){message("Erro: uma chave de API deve ser fornecida!")}
  
  stopifnot(!is.null(chave_google_maps), !missing(chave_google_maps), !chave_google_maps=="")
  
  register_google(key = chave_google_maps)
  
  escolas_censo_muni <- function(sigla_muni){
    
    if (sigla_muni == "rma"){
    escolas_muni <- escolas_fim_mat_end %>%
      # filter(co_municipio %in% munis_list$munis_metro$code_muni[which(munis_list$munis_metro$abrev_muni==sigla_muni)])
    filter(co_municipio %in% c(2800308,2804805,2800605,2806701)) %>%
      mutate(city = ifelse(co_municipio == 2806701, "São Cristóvão", city))
    
    
    } else {
      escolas_muni <- escolas_fim_mat_end %>%
        filter(co_municipio %in% munis_list$munis_metro$code_muni[which(munis_list$munis_metro$abrev_muni==sigla_muni)])
    }
    
    write.csv(escolas_muni, sprintf("../data-raw/educacao/censo_escolar/%s/muni_%s_educacao_%s/muni_%s_sem_geocode_%s.csv",
                                    ano,
                                    sigla_muni,
                                    ano,
                                    sigla_muni,
                                    ano))
    # Initialize the data frame
    escolas_to_geocode <- escolas_muni %>% mutate(addresses = paste(no_entidade ,adress, city, state, country)) %>%
      mutate(addresses = stringi::stri_enc_toutf8(addresses))
    escolas_to_geocode$addresses <- gsub('[^[:alnum:] ]','',escolas_to_geocode$addresses)
    
    escolas_to_geocode_lat_lon <- geocode(escolas_to_geocode$addresses)
    escolas_geocoded <- cbind(escolas_to_geocode, escolas_to_geocode_lat_lon)
    escolas_geocoded_sf <- st_as_sf(escolas_geocoded, coords = c("lon", "lat"), crs = 4326)
    # mapview(escolas_geocoded_sf)
    
    # Loop through the addresses to get the latitude and longitude of each address and add it to the
    # origAddress data frame in new columns lat and lon
    # Write a CSV file containing origAddress to the working directory
    write_sf(escolas_geocoded_sf, sprintf("../data-raw/educacao/censo_escolar/%s/muni_%s_educacao_%s/muni_%s_geocode_%s.shp",
                                           ano,
                                           sigla_muni,
                                           ano,
                                           sigla_muni,
                                           ano), append = F)
    
    #leitura dos dados ajustados manualmente
    
    escolas_geocoded_sf <- read_sf(sprintf("../data-raw/educacao/censo_escolar/%s/muni_%s_educacao_%s/muni_%s_geocode_%s.shp",
                                         ano,
                                         sigla_muni,
                                         ano,
                                         sigla_muni,
                                         ano))

    write_rds(escolas_geocoded_sf, sprintf("../data-raw/educacao/censo_escolar/%s/muni_%s_educacao_%s/muni_%s_geocode_%s.rds",
                                    ano,
                                    sigla_muni,
                                    ano,
                                    sigla_muni,
                                    ano))


  }
  
  lista_munis <- munis_list$munis_df$abrev_muni
  
  
  if (munis == "all"){
    
    walk(munis_list$munis_df$abrev_muni, escolas_censo_muni)
  }  else {
  escolas_censo_muni(ano = ano, munis = sigla_muni)
  
  }
  
}

educacao_filter(munis = "bel", ano = 2021)
