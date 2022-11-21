#criação de graphs no r5r

options(java.parameters = '-Xmx7G')
library(r5r)



# FUNCAO PARA CONSTRUIR network -------------------------
# graph.obj é salvo na pasta './otp/graphs/ano/cidade
sigla_muni <- 'poa'
ano <- 2022

construir_graph_muni <- function(sigla_muni, ano) {
  
  
  path <- sprintf("../r5r/network/%s/muni_%s/", ano, sigla_muni)
  r5r::setup_r5(data_path = path, elevation = "TOBLER", overwrite = TRUE)
  
}


# aplicar funcao ------------------------------------------------------------------------------
# lapply(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, construir_graph_muni, ano = 2017)
# lapply(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, construir_graph_muni, ano = 2018)
lapply(munis_list$munis_metro$abrev_muni, construir_graph_muni, ano = 2022)