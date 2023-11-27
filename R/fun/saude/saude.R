#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Esse script trata da filtragem e georreferenciamento dos dados de 
#estabelecimentos de saúde da base de dados do CNES para um município
#especificado.


rm(list = ls())

source('./R/fun/setup.R')

#função ler dados de saúde e filtrá-los

library(read.dbc)
library(aopdata)

sigla_muni <- 'bel'
ano_cnes <- 2019
estado <- 'PA'


ano<- 2019
file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)
hex <- read_rds(file_hex)

if (sigla_muni %in% munis_list$munis_df_aop$abrev_muni) {
  
  
  create_diretorios_aop <- function(sigla_muni){
    
    dir.create(sprintf("../data/saude/aop/muni_%s_saude_aop/", sigla_muni), recursive = TRUE)
    
  }
  
  walk(munis_list$munis_df$abrev_muni, create_diretorios_aop)
  
  dados_aop_poa <- read_landuse(city = sigla_muni)
  hex <- dados_aop_poa %>% left_join(hex, by = "id_hex") %>% st_as_sf()
  hex <- hex %>% select(id_hex, S001, S002, S003, S004) #%>%

  readr::write_rds(hex, sprintf('../data/saude/aop/muni_%s_saude_aop/muni_%s.rds', sigla_muni, sigla_muni))
  
} else {
  
  create_diretorios_cnes <- function(sigla_muni){
    
    dir.create(sprintf("../data/saude/cnes/muni_%s_saude_cnes/", sigla_muni), recursive = TRUE)
    
  }
  
  walk(munis_list$munis_df$abrev_muni, create_diretorios_cnes)

  library(ggmap)
  chave_google_maps <- readline(prompt = "API key google maps: ")
  
  if (chave_google_maps == ""){message("Erro: uma chave de API deve ser fornecida!")}
  
  stopifnot(!is.null(chave_google_maps), !missing(chave_google_maps), !chave_google_maps=="")
  
  register_google(key = chave_google_maps)
  
  # dados_estabelecimentos <- read.dbc::read.dbc(sprintf('../data-raw/saude_estado/%s/estabelecimentos_%s_%s.dbc',
  #                                                      estado, tolower(estado), ano_cnes))
  
  dados_estabelecimentos <- fread(sprintf('../data-raw/saude_cnes_%s/tbEstabelecimento%s01.csv',
                                                       ano_cnes, ano_cnes))
  
  
  dados_estabelecimentos2 <- dados_estabelecimentos %>% select(CNES,
                                                               CPF_CNPJ,
                                                               CODUFMUN,
                                                               COD_CEP,
                                                               PF_PJ,
                                                               TP_UNID,
                                                               VINC_SUS,
                                                               GESPRG1E,
                                                               GESPRG1M,
                                                               GESPRG2E,
                                                               GESPRG2M,
                                                               GESPRG4E,
                                                               NIVATE_A,
                                                               GESPRG5E,
                                                               GESPRG4M,
                                                               GESPRG5M,
                                                               GESPRG6E,
                                                               GESPRG6M,
                                                               NIVATE_H)
  
  
  dados_estabelecimentos2 <- dados_estabelecimentos %>% select(CO_CNES,
                                                               NU_CNPJ_MANTENEDORA,
                                                               CO_ESTADO_GESTOR,
                                                               CO_MUNICIPIO_GESTOR,
                                                               CO_CEP,
                                                               TP_PFPJ,
                                                               NO_RAZAO_SOCIAL,
                                                               NO_FANTASIA,
                                                               NU_ENDERECO,
                                                               NO_COMPLEMENTO,
                                                               NO_BAIRRO,
                                                               
                                                               
                                                               TP_UNIDADE,
                                                               VINC_SUS,
                                                               GESPRG1E,
                                                               GESPRG1M,
                                                               GESPRG2E,
                                                               GESPRG2M,
                                                               GESPRG4E,
                                                               NIVATE_A,
                                                               GESPRG5E,
                                                               GESPRG4M,
                                                               GESPRG5M,
                                                               GESPRG6E,
                                                               GESPRG6M,
                                                               NIVATE_H)
  
  
  

  #' 2) Faz filtros selecionando os municipios de interesse, hospitais publicos, faz refactor dos niveis de atendimento,
  #' retira hospitais indesejados
  

    
    # 2) Limpar os dados do CNES ---------------------------------
    
    
    # Filter 0: healthcare nao aplica (pq nao tem servicos de alta/baixa complexidade, e.g. academias de saude, secretarias de saude etc)
    # cnes_filter0 <- dados_estabelecimentos[complex_nao_aplic_est == ""] 
    # cnes_filter0 <- cnes_filter0[complex_nao_aplic_mun == ""]
    # 
    # message("filter0: ", nrow(dados_estabelecimentos2))
    
    # Filter 1: healthcare facilities operating with the public health system
    cnes_filter1 <- setDT(dados_estabelecimentos2)[VINC_SUS == 1]
    
    message("filter1: ", nrow(cnes_filter1))
    
    # Filter 2: Pessoa juridica
    # cnes_filter1$PF_PJ <- iconv(cnes_filter1$pessoa_fisica_ou_pessoa_juridi,
    #                                                      from = "UTF-8", to = "ASCII//TRANSLIT")
    cnes_filter2 <- cnes_filter1[ PF_PJ == 3]
    
    message("filter2: ", nrow(cnes_filter2))
    
    # filter 3: Only municipalities in the project
    code_munis <- munis_list$munis_metro[ano_metro == ano]$code_muni %>% 
      unlist() %>% substring(., 1, 6)
    cnes_filter3 <- cnes_filter2[CODUFMUN %in% code_munis]
    
    message("filter3: ", nrow(cnes_filter3))
    
    # filter 4: Only atendimento hospitalar ou ambulatorial
    # install_ambu <- ifelse(ano %in% c(2017, 2018), "X", "SIM")
    cnes_filter4 <- cnes_filter3[ GESPRG1E==1 | GESPRG1M==1 | GESPRG2E == 1 | GESPRG2M==1 | GESPRG4E == 1 | GESPRG4M ==1 | NIVATE_A == 1 | GESPRG5E == 1 | GESPRG5M == 1| GESPRG6E == 1| GESPRG6M == 1 | NIVATE_H == 1]
    
    
    # filter 5. Remove special categories of facilities 
    
    # 5.1 Delete prison hospitals, research centers, police hospitals etc
    # to_remove1 <- 'ZOONOSES|VETERINARI|CENTRO DE ESTUDOS|PSIQUIAT|PRESIDIO|PENAL|JUDICIARIO|PENITENCIARIA|PENITENCIARIO|PRISIONAL|FUNDACAO CASA|CASA DE CUSTODIA|CASA DE CUST|SEDIT|DETENCAO|PROVISORIA|SANATORIO|POLICIA| PADI|DE REGULACAO|VIGILANCIA|SAMU |ACADEMIA|DEPEND QUIMICO|REEDUCACAO SOCIAL|CAPS|CENTRO DE ATENCAO PSICOSSOCIAL|DISTRIB DE ORGAOS|MILITAR|CADEIA PUBLICA|DOMICILIAR|ARTES MARCIAIS|UBS IPAT|UBS CDPM II'
    # PADI = Programa de Atenção Domiciliar ao Idoso
    # DE REGULACAO = gestora de servico
    # CAPS - CENTRO DE ATENCAO PSICOSSOCIAL - saude mental e drogas
    # UBS IPAT e UBS CDPM II - vinculatos a policia
    
    to_remove_3 <- "43|50|60|64|67|68|75|76|77|81|82|83|84|85"
    # dados_74_70 <- cnes_filter4 %>% filter(TP_UNID %like% "74|70")
    
    # 5.2 Delete Home care, tele saude, unidades moveis de saude
    # to_remove2 <- 'TELESSAUDE|UNIDADE MOVEL|DOMICILIAR|PSICOSSOCIAL|FARMACIA|DE ORGAOS|CENTRAL DE REGULACAO DO ACESSO'
    
    # apply filter 5
    cnes_filter5 <- cnes_filter4[!TP_UNID %like% to_remove_3 ]
    # cnes_filter5 <- cnes_filter5[ tipo_unidade %nlike% to_remove2 ]
    table(cnes_filter5$TP_UNID)
    
    
    
    
    
    ### Organiza Nivel de atencao criando dummy
    
    
    # convert health facilities Hierarchy into dummy variables
    cnes_filter5[, health_low := ifelse(GESPRG1E==1|
                                          GESPRG1M==1,
                                          1, 0)]
    
    cnes_filter5[, health_med := ifelse(GESPRG2E==1|
                                          GESPRG2M==1 |
                                          GESPRG5E==1 |
                                          GESPRG5M==1 , 1, 0)]
    
    cnes_filter5[, health_high := ifelse(GESPRG4E==1|
                                           GESPRG4M==1 |
                                           GESPRG6E==1 |
                                           GESPRG6M==1 , 1, 0)]
    
    cnes_filter6 <- cnes_filter5[ health_low == 1 | health_med == 1 | health_high == 1]
    # table(cnes_filter5$health_low, useNA = "always")  # 3593
    # table(cnes_filter5$health_med, useNA = "always")  # 4224
    # table(cnes_filter5$health_high, useNA = "always") # 858
    
    # nrow(cnes_filter5) # 4872 obs
    
    # colocar todos codigos de CNES com 7 digitos
    # cnes_filter6[, cnes := stringr::str_pad(CNES, width = 7, side = "left", pad = 0)]
    # dados_complementares <- read.dbc::read.dbc(sprintf('../data-raw/saude_estado/%s/dados_complementares_%s_%s.dbc',
    #                                                    estado, tolower(estado), ano_cnes))
    
    cnes_filter6 <- cnes_filter6 %>% filter(CODUFMUN == munis_list$munis_df$code_muni[which(munis_list$munis_df$abrev_muni==sigla_muni)] %>% 
                                               unlist() %>% substring(., 1, 6)) %>%
      mutate(nome_fantasia = NA,
             logradouro = NA,
             numero = NA,
             bairro = NA,
             city = munis_list$munis_df$name_muni[which(munis_list$munis_df$abrev_muni==sigla_muni)]
             )
      
    
    # 3) Salvar ---------
    write_rds(cnes_filter6, sprintf("../data/saude/cnes/muni_%s_saude_cnes/muni_%s_cnes_%s.rds",
                                    sigla_muni,
                                    sigla_muni,
                                    ano))
    write.xlsx(cnes_filter6, sprintf("../data/saude/cnes/muni_%s_saude_cnes/muni_%s_cnes_%s.xlsx",
                                    sigla_muni,
                                    sigla_muni,
                                    ano))
    
    nrow(cnes_filter6)
    

    # leitura dos dados com endereço
    
    cnes_filter6 <- read_xlsx(sprintf("../data/saude/cnes/muni_%s_saude_cnes/muni_%s_cnes_%s.xlsx",
                      sigla_muni,
                      sigla_muni,
                      ano)) %>%
      mutate(addresses = paste(nome_fantasia, logradouro, numero,bairro, city, "Brasil"))
    
    
    
    # Initialize the data frame
    cnes_to_geocode <- cnes_filter6 %>%
      # mutate(addresses = paste(COD_CEP, munis_names$muni[which(munis_names$cod_ibge == .$CODUFMUN)],
      #                          munis_names$estado_nome[which(munis_names$cod_ibge == .$CODUFMUN)],
      #                          "Brasil")) %>%
      mutate(addresses = stringi::stri_enc_toutf8(addresses))
    # write.xlsx(cnes_to_geocode, '../data/saude/cnes/muni_rma_saude_cnes/muni_rma_cnes_2019_bruto.xlsx')
    # escolas_to_geocode$addresses <- gsub('[^[:alnum:] ]','',escolas_to_geocode$addresses)
    
    # cnes_to_geocode <- cnes_to_geocode[1:10,]
    cnes_lat_lon <- geocode(cnes_to_geocode$addresses)
    cnes_geocoded <- cbind(cnes_to_geocode, cnes_lat_lon) %>% drop_na()
    cnes_geocoded_sf <- st_as_sf(cnes_geocoded, coords = c("lon", "lat"), crs = 4326)
    mapview(cnes_geocoded_sf)# + mapview(hospitais_muni)
    
    
    
    muni_shape <- read_rds(sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds',
                           sigla_muni))
    
    # hospitais_muni <- read_sf('../data-raw/dados_municipais_recebidos/muni_pal/muni_pal.gpkg',
    #                           layer = 'saude') %>% st_filter(muni_shape)
    
    # teste_lat_lon <- geocode(escolas_to_geocode$addresses)
    # escolas_geocoded <- cbind(escolas_to_geocode, escolas_to_geocode_lat_lon)
    # escolas_geocoded_sf <- st_as_sf(escolas_geocoded, coords = c("lon", "lat"), crs = 4326)
    # mapview(escolas_geocoded_sf)
    
    # Loop through the addresses to get the latitude and longitude of each address and add it to the
    # origAddress data frame in new columns lat and lon
    # Write a CSV file containing origAddress to the working directory
    # write_rds(cnes_geocoded_sf, sprintf("../data/saude/cnes/muni_%s_saude_cnes/muni_%s_cnes_geocoded_%s.rds",
    #                                    sigla_muni,
    #                                    sigla_muni,
    #                                    ano))
    
    write_sf(cnes_geocoded_sf, sprintf("../data/saude/cnes/muni_%s_saude_cnes/muni_%s_cnes_geocoded_%s.gpkg",
                                        sigla_muni,
                                        sigla_muni,
                                        ano))
    
    arq_saude_final <- read_sf(sprintf("../data/saude/cnes/muni_%s_saude_cnes/muni_%s_cnes_geocoded_%s.gpkg",
                                       sigla_muni,
                                       sigla_muni,
                                       ano))
    
    write_rds(arq_saude_final, sprintf("../data/saude/cnes/muni_%s_saude_cnes/muni_%s_cnes_geocoded_%s.rds",
                                        sigla_muni,
                                        sigla_muni,
                                        ano))
    
  }
  

#apenas depois de juntar com a base municipal

if (sigla_muni == "pal"){
  file_saude <- '../data-raw/dados_municipais_recebidos/muni_pal/muni_pal.gpkg'
  dados_saude <- read_sf(file_saude, layer = "saude") %>%
    mutate(health_low = ifelse(ATUACAO == "Atencao Basica",
                               1, health_low)
    )
  # dados_saude2 <- read_sf(file_saude, layer = "saude_cnes")
  # dados_saude2 <- dados_saude2 %>% filter(health_med == 1)
  
  
  
} 

  # saude_filter(2017)
  
  
  #' A funcao saude_geocode' faz o geocode de hospitais com coordenadas problematicas e tem como output
  #' a base final do ano ja com as coordenadas corrigidas e pronta
  #' Etapas:
  #' 1) Abre os hospitais do ano que foram filtrados na etapa anterior
  #' 2) Separa somente os hospitais para geocode que nao forem geocoded no ano anterior (isso so serve a partir de 2018)
  #' 3) Corrigi problemas de formatacao nas coordenadas
  #' 4) Identifica hospitais com lat/long problematicos a partir dos quatro criterios estabelecidos
  #' 5) Faz geocode dos hospitais problematicos usando google maps. Se a opcao 'run_gmaps = FALSE', vai fazer uso
  #' dos dados que ja foram rodados no gmaps
  #' 6) Recodifica hospitais que estao fora da cidade: alguns hospitais persistem em ficar fora da cidade, oq pode indicar
  #' que esses estabs estao com a localizacao correta, so que foram registrados numa cidade da RM
  #' 7) Traz as coordenadas corrigidas para a base do novo ano
  #' 8) Traz as coordenadas do ano anterior para a base do novo ano
  #' 9) Traz as coordenadas dos dados da PMAQ
  
  
  
  # ano <- 2019
  
  saude_geocode <- function(ano, run_gmaps = FALSE) {
    
    # 1) Abrir geocoded filter ------------
    cnes_geocoded_filter <- read_rds(sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded.rds", ano, ano))
    
    # cnes_geocoded_filter %>% filter(cnes %in% c("7698585",
    #                                               "9491473",
    #                                               "7698313",
    #                                               "5184932")) %>% View()
    
    
    # head(cnes_geocoded_filter)
    
    # pad 7 digits of estabe id
    cnes_geocoded_filter[, cnes := str_pad(cnes, width = 7, pad = 0)]
    
    # identify engine
    cnes_geocoded_filter[, geocode_engine := "streetmap"]
    
    
    # trazer entao as coordenadas do gmaps do ano anterior
    if (ano != 2017) {
      
      cnes_anterior <- read_rds(sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps.rds", ano-1, ano-1))
      # identificar estabs a serem atualizados
      estabs_to_update <- cnes_geocoded_filter[type_year_input %nin% ano]$cnes
      # filtra entao esses do ano anteior
      cnes_anterior <- cnes_anterior[cnes %in% estabs_to_update]
      # juncao
      # table(cnes_geocoded_filter$Addr_type, useNA = 'always')
      # table(cnes_geocoded_filter$geocode_engine, useNA = 'always')
      cnes_geocoded_filter[cnes_anterior, on = "cnes",
                           c("Addr_type", "matched_address", "lon", "lat", "geocode_engine", "gmaps") :=
                             list(i.Addr_type, i.matched_address, i.lon, i.lat, i.geocode_engine, i.gmaps)]
      
    }
    
    
    # table(rais_galileo_output$PrecisionDepth, useNA = 'always')
    # table(rais_galileo_output$type_input_galileo, useNA = 'always')
    
    # extrair somente o que eh novo para cada ano
    if (ano != 2017) {
      
      # identificar tipos
      tipos <- ano
      
      cnes_geocoded_filter_new <- cnes_geocoded_filter[type_year_input %in% tipos]
      
    } else cnes_geocoded_filter_new <- cnes_geocoded_filter 
    
    
    # 3) Rodar Google API p/ estabelecimentos q Galileo encontrou com baixa precisao ------
    # Manter todos resultados ‘point address’
    # ‘Street address (ext) e street name’, manter score  >= 0.90
    # Vai para google maps
    # Tied e Todas outras observações vão para Google maps
    # ‘Street address (ext) e street name’, manter score  < 0.90
    
    # 3.1) Selecionar estabs com baixa precisao
    cnes_geocoded_filter_new[, gmaps := fifelse(Status %in% c("T", "U"), TRUE,
                                                fifelse(Addr_type == "PointAddress", FALSE,
                                                        fifelse(cnes %like% "530010" & Addr_type %in% c("StreetAddress", "StreetAddressExt", "StreetName") & Score >= 75, FALSE,
                                                                fifelse(Addr_type %in% c("StreetAddress", "StreetAddressExt", "StreetName") & Score >= 90, FALSE, TRUE))))]
    
    estabs_problema <- cnes_geocoded_filter_new[gmaps == TRUE]
    
    # # 3.2) Selecionar muitos stabs com as mesmas coordenadas 
    
    
    message("Total of estabs to go to gmaps: ", unique(estabs_problema$cnes) %>% length())
    
    
    # 3.2) Listar esses enderecos com problema
    enderecos <- estabs_problema %>% mutate(fim = paste0(logradouro, " - ", bairro, " - ", municipio, ", ", uf, " - CEP ", cep)) %>% pull(fim)
    
    # 3.3) Registrar Google API Key
    my_api <- data.table::fread("../../data-raw/google_key.txt", header = TRUE)
    
    
    if (run_gmaps) {
      
      message("Running gmaps, this may take a while")
      
      register_google(key = my_api$key[2]) # kaue
      coordenadas_google <- lapply(X=enderecos, ggmap::geocode, output = "all")
      
      # identify list names as id_estab
      names(coordenadas_google) <- estabs_problema$cnes
      
      # save
      write_rds(coordenadas_google, 
                sprintf("../../data/acesso_oport/saude/%s/geocode/educacao_geocode_%s_output_google.rds", ano, ano))
    }  else coordenadas_google <- read_rds(sprintf("../../data/acesso_oport/saude/%s/geocode/educacao_geocode_%s_output_google.rds", ano, ano))
    
    # function to create data.frame from gmaps output
    create_dt <- function(x) {
      
      precision_depth0 <- ifelse(length(x[["results"]][[1]][["address_components"]]) > 0, 
                                 x[["results"]][[1]][["address_components"]], 
                                 NA)
      
      # check length from precision depth
      precision_depth <- ifelse(is.na(precision_depth0), NA,
                                ifelse(length(precision_depth0[[1]]$types) > 0,
                                       precision_depth0[[1]]$types[[1]], 
                                       NA))
      
      a <- data.table(
        matched_address = ifelse(!is.null(x[["results"]][[1]][["formatted_address"]]), x[["results"]][[1]][["formatted_address"]], NA),
        # PrecisionDepth = ifelse(!is.null(x[["results"]][[1]][["address_components"]][[1]]$types[[1]]), x[["results"]][[1]][["address_components"]][[1]]$types[[1]], NA),
        Addr_type = precision_depth,
        lon = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lng"]]), x[["results"]][[1]][["geometry"]][["location"]][["lng"]], NA),
        lat = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lat"]]), x[["results"]][[1]][["geometry"]][["location"]][["lat"]], NA)
      )
      
    }
    
    # 3.5) Rodar funcao que transforma todos os estabs georef em data.table
    estabs_problema_geocoded <- lapply(coordenadas_google, create_dt)
    
    # 3.6) Rbind as data.table
    estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "cnes",
                                             use.names = TRUE)
    
    
    # make sure we only using the correct subset
    estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[cnes %in% estabs_problema$cnes]
    
    # 3.9) Identificar o tipo de problema
    estabs_problema_geocoded_dt[, geocode_engine := 'gmaps']
    
    # 3.10) Identificar qualidade quando o endereco nao foi encontrado
    estabs_problema_geocoded_dt[is.na(lon), ':='(Addr_type = "address_not_found")]
    
    
    
    # 10) Substituir as coordenadas problematicas que estao na base do streetmap ----
    # pelas novas coordenadas que foram corrigidas pelo gmaps
    
    # table(cnes_geocoded_filter_new$Addr_type, useNA = 'always')
    # table(cnes_geocoded_filter_new$geocode_engine, useNA = 'always')
    # table(cnes_geocoded_filter_new$type_year_input, useNA = 'always')
    # table(cnes_geocoded_filter_new$gmaps, useNA = 'always')
    
    # primeiro trazer as coordenadas do gmaps para as novas coordenadas
    cnes_geocoded_filter_new[estabs_problema_geocoded_dt, on = "cnes",
                             c("Addr_type", "matched_address",  "lon", "lat", "geocode_engine") :=
                               list(i.Addr_type, i.matched_address, i.lon, i.lat, i.geocode_engine)]
    
    
    # depois, trazer essas para as coordendas completas do ano
    # 10.2) Fazer a substituicao
    cnes_geocoded_filter[cnes_geocoded_filter_new, on = "cnes",
                         c("Addr_type", "matched_address", "lon", "lat", "geocode_engine", "gmaps") :=
                           list(i.Addr_type, i.matched_address, i.lon, i.lat, i.geocode_engine, i.gmaps)]
    
    
    
    # table(cnes_geocoded_filter$Addr_type, useNA = 'always')
    # table(cnes_geocoded_filter$geocode_engine, useNA = 'always')
    # table(cnes_geocoded_filter$type_year_input, useNA = 'always')
    # table(cnes_geocoded_filter$gmaps, useNA = 'always')
    
    
    # # 10.3) Garantir que os enderecos sejam unicos
    # rais_galileo_output_fixed <- distinct(rais_galileo_output_fixed, id_estab, .keep_all = TRUE)
    
    # reorganize columns
    cnes_geocoded_filter <- cnes_geocoded_filter %>% select(cnes, everything()) %>% setDT()
    
    # 10.4) Salvar
    write_rds(cnes_geocoded_filter, 
              sprintf("../../data/acesso_oport/saude/%s/saude_%s_filter_geocoded_gmaps.rds", ano, ano), compress = 'gz')
    
    
    
    # # 9) Trazer dados da  PMAQ ---------------------------------
    # 
    # attr(cnes_filtered_fim$cnes, "sorted") <- NULL
    # 
    # 
    # # table(hospitais$geocode_engine, useNA = 'always')
    # 
    # ###### Usar dados de lat/lon quando eles existirem na PMAQ Ciclo 3 (estabelecimentos de baixa complexidade)
    # # Read PMAQ data
    # pmaq_df_coords_fixed <- fread('../../data-raw/hospitais/2019/PMAQ/pmaq_df_coords_fixed.csv', colClasses = 'character') %>%
    #   select(code_muni, cnes = CNES_FINAL, lon, lat) %>%
    #   mutate(cnes = str_pad(cnes, width = 7, side = "left", pad = 0),
    #          PrecisionDepth = "PMAQ",
    #          geocode_engine = "PMAQ") %>%
    #   mutate(lon = as.numeric(lon),
    #          lat = as.numeric(lat)) %>% setDT()
    # 
    # # lookup table (merge) to update coordinates
    # cnes_filtered_fim[pmaq_df_coords_fixed, on = "cnes",
    #                   c("PrecisionDepth", "lon", "lat", "geocode_engine") := 
    #                     list(i.PrecisionDepth, i.lon, i.lat, i.geocode_engine)]
    # 
    # 
    # 
    # 
    # # 10) Salvar  ------------------------------------------------------------------
    # 
    # cnes_filtered_fim %>%
    #   rename(code_muni = ibge) %>%
    #   # select(CNES, code_muni, health_low, health_med, health_high)
    #   readr::write_rds(sprintf("../../data/acesso_oport/saude/%s/saude_%s_geocoded.rds", ano, ano))
    
    
    
    
  }
  
  
  
  
  
}


#escrita dos dados de saúde






dados_aop_poa <- read_landuse(city = 'poa')
hex <- hex %>% left_join(dados_aop_poa, by = "id_hex") %>% st_as_sf()

teste <- hex %>% filter(S001 > 0)
mapview(teste, zcol = "S001")


dados_saude <- read.dbc::read.dbc(sprintf('../data-raw/saude_estado/%s/leitos_%s_2201.dbc',
                                          estado, tolower(estado)))
dados_estabelecimentos <- read.dbc::read.dbc(sprintf('../data-raw/saude_estado/%s/estabelecimentos_%s_2201.dbc',
                                                     estado, tolower(estado)))


