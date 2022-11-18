rm(list = ls())

source('./R/fun/setup.R')

#função ler dados de saúde e filtrá-los

library(read.dbc)

library(aopdata)

sigla_muni <- 'poa'
ano <- 2019
estado <- 'RS'

create_diretorios <- function(sigla_muni){
  
  dir.create(sprintf("../data/saude/aop/muni_%s_saude_aop/", sigla_muni), recursive = TRUE)
  
}

walk(munis_list$munis_df$abrev_muni, create_diretorios)

create_diretorios <- function(sigla_muni){
  
  dir.create(sprintf("../data/saude/cnes/muni_%s_saude_cnes/", sigla_muni), recursive = TRUE)
  
}

walk(munis_list$munis_df$abrev_muni, create_diretorios)



file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)
hex <- read_rds(file_hex)

if (sigla_muni %in% munis_list$munis_df_aop$abrev_muni) {
  dados_aop_poa <- read_landuse(city = sigla_muni)
  hex <- dados_aop_poa %>% left_join(hex, by = "id_hex") %>% st_as_sf()
  hex <- hex %>% select(id_hex, S001, S002, S003, S004) #%>%
    # drop_na(h3_resolution)
  
  readr::write_rds(hex, sprintf('../data/saude/aop/muni_%s_saude_aop/muni_%s.rds', sigla_muni, sigla_muni))
  
} else {
  
  # hex_scholl <- dados_aop_poa %>% left_join(hex, by = "id_hex") %>% st_as_sf()
  # 
  # mapview(hex_scholl, zcol = 'E001')
  #cod_uf faltando o ultimo digito nos dados do cnes
  
  dados_leitos <- read.dbc::read.dbc(sprintf('../data-raw/saude_estado/%s/leitos_%s_2201.dbc',
                                             estado, tolower(estado))) %>%
    filter(CODUFMUN == munis_list$munis_metro[abrev_muni == sigla_muni]$code_muni)
  
  dados_estabelecimentos <- read.dbc::read.dbc(sprintf('../data-raw/saude_estado/%s/estabelecimentos_%s_2201.dbc',
                                                       estado, tolower(estado)))
  
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


