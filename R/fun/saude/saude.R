rm(list = ls())

source('./R/fun/setup.R')

#função ler dados de saúde e filtrá-los

library(read.dbc)

library(aopdata)

sigla_muni <- 'poa'
ano <- 2019
file_hex <- sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)
hex <- read_rds(file_hex)


dados_aop_poa <- read_landuse(city = 'poa')
hex <- hex %>% left_join(dados_aop_poa, by = "id_hex") %>% st_as_sf()

mapview(hex, zcol = "S001.x")


dados_saude <- read.dbc::read.dbc('../data-raw/saude_estado/RS/leitos_rs_2201.dbc')

dados_estabelecimentos <- read.dbc::read.dbc('../data-raw/saude_estado/RS/estabelecimentos_rs_2201.dbc')
