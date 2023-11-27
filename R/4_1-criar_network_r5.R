#criação da rede no r5r

#Libera memória
rm(list = ls(all.names = T)); gc(full = T)

#define a quantidade de memória ram a ser utilizada pelo r5r
options(java.parameters = '-Xmx5G')

#carrega o pacote r5r
library(r5r)

# FUNCAO PARA CONSTRUIR network -------------------------

construir_graph_muni <- function(sigla_muni, ano) {
  
  
  path <- sprintf("../r5r/network/%s/muni_%s/", ano, sigla_muni)
  r5r::setup_r5(data_path = path, elevation = "TOBLER", overwrite = TRUE,
                verbose = TRUE)
  
  message(paste("Rede r5r de", munis_list$munis_df$name_muni[which(munis_list$munis_df$abrev_muni==sigla_muni)] ,"criada com sucesso."))
  
}

#aplica a função para um município
construir_graph_muni(sigla_muni = 'bel',ano = 2022)

#Encerra o processo do r5r
r5r::stop_r5()
