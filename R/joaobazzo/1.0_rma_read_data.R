# Load ----

rm(list=ls())
gc(reset = TRUE)

# install.packages('easypackages')
# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
easypackages::packages('geobr'
                       , 'gtfs2gps'
                       , 'data.table'
                       , 'magrittr'
                       , 'ggplot2'
                       , 'raster'
                       , 'ggnewscale'
                       , 'progressr'
                       , 'tabulizer'
                       , 'aopdata')

od_file = "data-raw/bra_aracaju/Pesquisa de Origem e Destino - 2012.pdf"

# viagens/hab---------------------
dt_pico_mot_viagens <- tabulizer::extract_areas(file = od_file
                         ,pages = 5)
dt_pico_mot_viagens1 <- dt_pico_mot_viagens[[1]]
dt_pico_mot_viagens1 <- dt_pico_mot_viagens1 %>% as.data.frame()
names(dt_pico_mot_viagens1) <- c('n','name','pop','viagens','viagens_hab')
dt_pico_mot_viagens1$viagens_hab <- gsub("\\,",".",dt_pico_mot_viagens1$viagens_hab)
setDT(dt_pico_mot_viagens1)
dt_pico_mot_viagens1[,":="(
  "n" = NULL,
  "pop" = gsub("[.]","",pop),
  "viagens" = gsub("[.]","",viagens)
) ]

dt_pico_mot_viagens1[,":="(pop =   as.numeric(pop),
                          viagens = as.numeric(viagens),
                          viagens_hab = as.numeric(viagens_hab))]
dt_pico_mot_viagens1[,name:= janitor::make_clean_names(name)]

# geracao mot-----------
dt_pico_mot_geracao_n <- tabulizer::extract_areas(file = od_file
                                                ,pages = 6)
name_bairro <- c('Centro','Getulio Vargas','Cirurgia','Pereira Lobo',
                 'Suissa','Salgado Filho','Treze De Julho','Dezoito Do Forte',
                 'Palestina','Santo Antonio','Industrial','Santos Dummond','Jose Conrado De Araujo',
                 'Novo Paraiso','America','Siqueira Campos','Soledade','Lamarao','Cidade Nova',
                 'Japãozinho','Porto Dantas','Bugio','Jardim Centenario','Olaria','Capucho',
                 'Jabotiana','Ponto Novo','Luzia','Grageru','Jardins','Inacio Barbosa','Sao Conrado',
                 'Farolandia','Coroa Do Meio','Aeroporto','Atalaia','Santa Maria','Zona De Expansâo',
                 'Sao Jose','Nossa Sra Socorro I','Marcos Freire II','João Alves','Marcos Freire/Piabeta',
                 'Fernando Collor','Sao Cristovao','Barra Coqueiros','Municipios Norte','Municipios Sul')
dt_pico_mot_geracao_nn <- dt_pico_mot_geracao_n[[1]]
dt_pico_mot_geracao_nn <- as.data.frame(dt_pico_mot_geracao_nn)
setDT(dt_pico_mot_geracao_nn)
names(dt_pico_mot_geracao_nn)
dt_pico_mot_geracao_nn[,":="(
  "V1" = gsub("[.]","",V1),
  "V2" = gsub("[.]","",V2),
  "V3" = gsub("[.]","",V3),
  "V4" = gsub("[.]","",V4),
  "V5" = gsub("[.]","",V5),
  "V6" = gsub("[.]","",V6)
  ) ]

names(dt_pico_mot_geracao_nn) <- 
  c('ger_7_10','prod_7_10','atra_7_10'
    ,'ger_8h15_9h15','prod_8h15_9h15','atra_8h15_9h15')

dt_pico_mot_geracao_nn$name <- janitor::make_clean_names( c(name_bairro,"Totais"))

dt_pico_mot_geracao_nn

# divi_modal ----------------

div_raw <- tabulizer::extract_areas(file = od_file
                                                  ,pages = 18)
div_mod <- div_raw[[1]] %>% as.data.frame()
div_mod <- div_mod[-c(1:3),]
setDT(div_mod)
div_mod[,":="(
  V1 = NULL,
  V3 = NULL,
  V5 = NULL,
  V7 = NULL,
  V8 = NULL
)]
div_mod[,V6_p1 := stringr::str_split(V6," ",2,simplify = T)[,1],by = V2]
div_mod[,V6 := NULL]
div_mod[,":="(
  "V2" = janitor::make_clean_names(V2),
  "V4" = gsub("[.]","",V4) %>% as.numeric(),
  "V6_p1" = gsub("[.]","",V6_p1) %>% as.numeric()
) ]
names(div_mod) <- c("name","publico","privado")

div_mod
# end---------------

rma <- list("div_mod" = div_mod,
            "dt_pico_mot_geracao_nn" = dt_pico_mot_geracao_nn,
            "dt_pico_mot_viagens1" = dt_pico_mot_viagens1)
rma
readr::write_rds(x = rma,file = "data/bra_aracaju/od_dt.rds")
