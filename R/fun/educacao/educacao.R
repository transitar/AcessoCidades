# educação

source('./R/fun/setup.R')

#função ler dados de saúde e filtrá-los

library(read.dbc)

library(aopdata)

sigla_muni <- 'pal'
ano <- 2021
estado <- 'RS'

create_diretorios <- function(sigla_muni){
  
  dir.create(sprintf("../data/educacao/aop/muni_%s_educacao_aop/", sigla_muni), recursive = TRUE)
  
}

walk(munis_list$munis_df$abrev_muni, create_diretorios)

create_diretorios <- function(sigla_muni){
  
  dir.create(sprintf("../data/educacao/censo_escolar/muni_%s_educacao_censo_escolar/", sigla_muni), recursive = TRUE)
  
}

walk(munis_list$munis_df$abrev_muni, create_diretorios)



file_hex <- sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', ano, sigla_muni)
hex <- read_rds(file_hex)

if (sigla_muni %in% munis_list$munis_df_aop$abrev_muni) {
  dados_aop_poa <- read_landuse(city = sigla_muni)
  hex <- dados_aop_poa %>% left_join(hex, by = "id_hex") %>% st_as_sf()
  hex <- hex %>% select(id_hex, E001, E002, E003, E004, M001,
                        M002,M003,M004) #%>%
    # drop_na(h3_resolution)
  
  readr::write_rds(hex, sprintf('../data/educacao/aop/muni_%s_educacao_aop/muni_%s.rds', sigla_muni, sigla_muni))
  
} else {
  
#   
  # hex_scholl <- dados_aop_poa %>% left_join(hex, by = "id_hex") %>% st_as_sf()
  #
  # mapview(hex_scholl, zcol = 'E001')
  #cod_uf faltando o ultimo digito nos dados do cnes
  
  dados_escolas_censo <- fread(sprintf('../data-raw/censo_escolar/%s/dados/microdados_ed_basica_%s.csv',
                                       ano, ano))
  
  dados_escolas_muni <- dados_escolas_censo %>%
    filter(CO_MUNICIPIO == munis_list$munis_metro$code_muni[munis_list$munis_metro$abrev_muni == sigla_muni]) %>%
    filter(TP_DEPENDENCIA %in% c(1,2,3) | TP_CATEGORIA_ESCOLA_PRIVADA == 4) %>% #filtra escolas publicas e filantropicas
    filter(TP_SITUACAO_FUNCIONAMENTO %in% c(1,2)) %>%  #filtra escolas em atividade
    select(DS_ENDERECO, NU_ENDERECO, NO_BAIRRO, CO_CEP, #endereço
           QT_MAT_BAS,
           QT_MAT_INF,
           QT_MAT_INF_CRE,
           QT_MAT_INF_PRE,
           QT_MAT_FUND,
           )

  
  
  
  
  dados_leitos <- read.dbc::read.dbc(sprintf('../data-raw/saude_estado/%s/leitos_%s_2201.dbc',
                                             estado, tolower(estado))) %>%
    filter(CODUFMUN == munis_list$munis_metro[abrev_muni == sigla_muni]$code_muni)

  dados_estabelecimentos <- read.dbc::read.dbc(sprintf('../data-raw/saude_estado/%s/estabelecimentos_%s_2201.dbc',
                                                       estado, tolower(estado)))
#   
}
