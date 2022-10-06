Sys.setenv(TZ='UTC') # Fuso horario local

# carregar bibliotecas
suppressMessages(library(raster))
suppressMessages(library(ggplot2) )     # visualizacao de dados
suppressMessages(library(ggthemes)  )   # temas para visualizacao de dados
suppressMessages(library(sf)  )         # leitura e manipulacao de dados espaciais
suppressMessages(library(data.table)  ) # manipulacao de dados
# suppressMessages(library(read.dbc)  )   # leitura de bases relacionais em Microsoft Access
suppressMessages(library(geobr) )       # dados espaciais do brasil
suppressMessages(library(pbapply) )     # progress bar
suppressMessages(library(readr) )       # rapida leitura de dados 
suppressMessages(library(tidyr) )       # manipulacao de dados
suppressMessages(library(stringr) )     # operacoes em strings
suppressMessages(library(lubridate) )   # dados em data/horario
suppressMessages(library(fasttime)  )   # rapido processamento deddados em data/horario
suppressMessages(library(mapview) )     # visualizacao interativa dos dados
suppressMessages(library(RColorBrewer)) # paleta de cores
suppressMessages(library(extrafont) )   # fontes de texto
# suppressMessages(library(bit.64)  )     # lidar com numeros ee 64bits
suppressMessages(library(knitr))
suppressMessages(library(furrr))
suppressMessages(library(purrr))
suppressMessages(library(forcats))
suppressMessages(library(future.apply)) # Aplicar funcoes em paralelo
# suppressMessages(library(h3jsr)) # H3 grade hexagonal
suppressMessages(library(dplyr))
suppressMessages(library(hrbrthemes))
suppressMessages(library(beepr))
suppressMessages(library(patchwork))
suppressMessages(library(Hmisc)) # calcular quantis ponderados
suppressMessages(library(osmdata)) # Download de dados do OpenStreeteMaps (OSM)
suppressMessages(library(opentripplanner)) # Usar OTP de dentro do R: https://github.com/ITSLeeds/opentripplanner
suppressMessages(library(ggmap)) # geocoding
suppressMessages(library(h3jsr)) #h3 hex remotes::install_github("obrl-soil/h3jsr")
suppressMessages(library(bit64)) # viz large numbers
suppressMessages(library(quantreg))
suppressMessages(library(gtfstools))
suppressMessages(library(readxl))
suppressMessages(library(magrittr))
suppressMessages(library(ceramic))
suppressMessages(library(ggalt))
suppressMessages(library(hrbrthemes))
suppressMessages(library(ggnewscale))
suppressMessages(library(ggsn))
suppressMessages(library(ggthemes))
suppressMessages(library(rlist))
suppressMessages(library(pipeR))


munis_list <- list(
  
  # munis_df = tribble(
  #   ~code_muni, ~abrev_muni, ~name_muni,        ~abrev_estado,  ~map_plot_ratio_wh,
  #   2304400,    "for",       "Fortaleza",       "CE",           1.2,
  #   3550308,    "spo",       "Sao Paulo",       "SP",           0.65,
  #   3304557,    "rio",       "Rio de Janeiro",  "RJ",           1.91, 
  #   4106902,    "cur",       "Curitiba",        "PR",           0.62,
  #   4314902,    "poa",       "Porto Alegre",    "RS",           0.75,
  #   3106200,    "bho",       "Belo Horizonte",  "MG",           0.69,
  #   5300108,    "bsb",       "Brasilia",        "DF",           1.71,
  #   2927408,    "sal",       "Salvador",        "BA",           1.36,
  #   1302603,    "man",       "Manaus",          "AM",           1.27,           
  #   2611606,    "rec",       "Recife",          "PE",           0.68,
  #   5208707,    "goi",       "Goiania",         "GO",           0.93,
  #   1501402,    "bel",       "Belem",           "PA",           0.65,
  #   3518800,    "gua",       "Guarulhos",       "SP",           0.91,
  #   3509502,    "cam",       "Campinas",        "SP",           1.20,
  #   2111300,    "slz",       "Sao Luis",        "MA",           0.78,
  #   3304904,    "sgo",       "Sao Goncalo",     "RJ",           1.21,
  #   2704302,    "mac",       "Maceio",          "AL",           0.74,
  #   3301702,    "duq",       "Duque de Caxias", "RJ",           0.61,
  #   5002704,    "cgr",       "Campo Grande",    "MS",           0.87,
  #   2408102,    "nat",       "Natal",           "RN",           0.70
  # ) %>% setDT(),
  # 
  
  # munis_modo = tribble(
  #   ~abrev_muni, ~`2017`,  ~`2018`,  ~`2019`,  ~`2020`, 
  #   "for",       "todos",  "todos",  "todos",  "todos",     
  #   "spo",       "todos",  "todos",  "todos",  "todos",   
  #   "rio",       "ativo",  "todos",  "todos",  "todos",   
  #   "cur",       "todos",  "todos",  "todos",  "todos",   
  #   "poa",       "todos",  "todos",  "todos",  "todos",   
  #   "bho",       "todos",  "todos",  "todos",  "todos",   
  #   "bsb",       "ativo",  "ativo",  "ativo",  "ativo",   
  #   "sal",       "ativo",  "ativo",  "ativo",  "ativo",   
  #   "man",       "ativo",  "ativo",  "ativo",  "ativo",   
  #   "rec",       "ativo",  "ativo",  "todos",  "todos",   
  #   "goi",       "ativo",  "ativo",  "todos",  "todos",   
  #   "bel",       "ativo",  "ativo",  "ativo",  "ativo",   
  #   "gua",       "ativo",  "ativo",  "ativo",  "ativo",   
  #   "cam",       "todos",  "todos",  "todos",  "todos",   
  #   "slz",       "ativo",  "ativo",  "ativo",  "ativo",   
  #   "sgo",       "ativo",  "ativo",  "ativo",  "ativo",   
  #   "mac",       "ativo",  "ativo",  "ativo",  "ativo",   
  #   "duq",       "ativo",  "ativo",  "ativo",  "ativo",   
  #   "cgr",       "ativo",  "ativo",  "ativo",  "ativo",   
  #   "nat",       "ativo",  "ativo",  "ativo",  "ativo"   
  # ) %>% 
  #   pivot_longer(cols = `2017`:`2020`, names_to = "ano_modo", values_to = "modo") %>% 
  #   setDT(),
  
  # munis_metro = tribble(
  #   ~abrev_muni, ~ano_metro,  ~code_muni,
  #   "for",       2017,     2304400,
  #   "spo",       2017,     3550308,
  #   "rio",       2017,     3304557,
  #   "cur",       2017,     4106902,
  #   "poa",       2017,     4314902,
  #   "bho",       2017,     3106200,
  #   "bsb",       2017,     5300108,
  #   "sal",       2017,     2927408,
  #   "man",       2017,     1302603,
  #   "rec",       2017,     2611606,
  #   "goi",       2017,     5208707,
  #   "bel",       2017,     1501402,
  #   "gua",       2017,     3518800,
  #   "cam",       2017,     3509502,
  #   "slz",       2017,     2111300,
  #   "sgo",       2017,     3304904,
  #   "mac",       2017,     2704302,
  #   "duq",       2017,     3301702,
  #   "cgr",       2017,     5002704,
  #   "nat",       2017,     2408102,
  #   
  #   "for",       2018,     2304400,
  #   "spo",       2018,     3550308,
  #   "rio",       2018,     3304557,
  #   "cur",       2018,     4106902,
  #   "poa",       2018,     4314902,
  #   "bho",       2018,     3106200,
  #   "bsb",       2018,     5300108,
  #   "sal",       2018,     2927408,
  #   "man",       2018,     1302603,
  #   "rec",       2018,     2611606,
  #   "goi",       2018,     5208707,
  #   "bel",       2018,     1501402,
  #   "gua",       2018,     3518800,
  #   "cam",       2018,     3509502,
  #   "slz",       2018,     2111300,
  #   "sgo",       2018,     3304904,
  #   "mac",       2018,     2704302,
  #   "duq",       2018,     3301702,
  #   "cgr",       2018,     5002704,
  #   "nat",       2018,     2408102,
  #   
  # 
  # ) %>% setDT()
  
  munis_metro = tribble(
    ~abrev_muni, ~ano_metro,  ~code_muni,
    "poa",       2019,     4314902,
    "bel",       2019,     1501402,
    "man",       2019,     1302603,
    "slz",       2019,     2111300,
    "rma",       2019,     c(2800308,2804805,2800605,2806701),
    "noh",       2019,     4313409,
    "dou",       2019,     5003702,
    "con",       2019,     3118601,
    "vic",       2019,     2933307,
    "cit",       2019,     3201209,
    "pal",       2019,     1721000
    
    
  ) %>% setDT(),
  
  
  munis_df= tribble(
    ~abrev_muni, ~ano_metro,  ~code_muni, ~name_muni, ~abrev_estado, ~map_plot_ratio_wh,
    "poa",       2019,     4314902, "Porto Alegre" , "RS" , 1,
    "bel",       2019,     1501402, "Belém", "PA", 1,
    "man",       2019,     1302603, "Manaus", "AM", 1,
    "slz",       2019,     2111300, "São Luís", "MA", 1, 
    "rma",       2019,     c(2800308,2804805,2800605,2806701), "Região Metropolitana de Aracaju", "SE", 1, 
    "noh",       2019,     4313409, "Novo Hamburgo", "RS", 1, 
    "dou",       2019,     5003702, "Dourados", "MS", 1,
    "con",       2019,     3118601, "Contagem", "MG", 1,
    "vic",       2019,     2933307, "Vitória da Conquista", "BA", 1,
    "cit",       2019,     3201209, "Cachoeiro do Itapemirim", "ES", 1,
    "pal",       2019,     1721000, "Palmas", "TO", 1
    
    
  ) %>% setDT()
  
  
) 

censo <- list(
  
  
  variaveis_interesse = tribble(
    
    ~variavel ,~descricao ,~planilha_censo,~seq_variaveis,~legenda,
    "P001" ,"Pessoas brancas do sexo masculino" ,"Pessoa03",c("087","092","097",102,107,112,117,122,127,132,137,142,147,152,157,162),"Homens Brancos",
    "P002","Pessoas pretas do sexo masculino" ,"Pessoa03",c("088","093","098",103,108,113,118,123,128,133,138,143,148,153,158,163),"Homens Pretos",
    "P003","Pessoas amarelas do sexo masculino" ,"Pessoa03",c("089","094","099",104,109,114,119,124,129,134,139,144,149,154,159,164),"Homens Amarelos",
    "P004","Pessoas pardas do sexo masculino" ,"Pessoa03",c("090","095",100,105,110,115,120,125,130,135,140,145,150,155,160,165),"Homens Pardos",
    "P005","Pessoas indígenas do sexo masculino" ,"Pessoa03",c("091","096",101,106,111,116,121,126,131,136,141,146,151,156,161,166),"Homens Indígenas",
    "P006","Pessoas brancas do sexo feminino" ,"Pessoa03",c(167,172,177,182,187,192,197,202,207,212,217,222,227,232,237,242),"Mulheres Brancas",
    "P007","Pessoas pretas do sexo feminino" ,"Pessoa03",c(168,173,178,183,188,193,198,203,208,213,218,223,228,233,238,243),"Mulheres Pretas",
    "P008","Pessoas amarelas do sexo feminino" ,"Pessoa03",c(169,174,179,184,189,194,199,204,209,214,219,224,229,234,239,244),"Mulheres Amarelas",
    "P009","Pessoas pardas do sexo feminino" ,"Pessoa03",c(170,175,180,185,190,195,200,205,210,215,220,225,230,235,240,245),"Mulheres Pardas",
    "P010","Pessoas indígenas do sexo feminino" ,"Pessoa03",c(171,176,181,186,191,196,201,206,211,216,221,226,231,236,241,246),"Mulheres Indígenas"
    
    
  ) %>% setDT()
  
  
)




criate_folder <- function(dir) {
  
  if (!dir.exists(dir)){
    dir.create(dir,recursive = T)
    print("Dir created!")
  } else {
    print("Dir already exists!")
  }}


consolida_basico <- function(caminho) {
  
  ler_basico <- fread(caminho)
  
  # df <- ler_basico %>% 
  #   select(1:33) %>% 
  #   mutate_at(c(1:21), as.character) %>% 
  #   mutate_at(c(22:33), ~ str_replace(., ",", ".")) %>% 
  #   mutate_at(c(22:33), as.numeric)
  
  df <- ler_basico %>% 
    select(1:21) %>% 
    mutate_at(c(1:21), as.character)  
    # mutate_at(c(22:33), ~ str_replace(., ",", ".")) %>% 
    # mutate_at(c(22:33), as.numeric)
  
  
  return(df)
  
  
  coleta_variaveis <- function(var,arq_munis){
    
    
    message(paste("rodando", which(var_list %in% var), "de", length(var_list)))
    
    col_vec <- cols_gen_cor %>% filter(variavel == var) %>% pull(cols_vec) %>% unlist()
    
    x <- arq_munis %>% select(1,col_vec) %>% mutate_if(is.character, as.numeric) %>%
      rowwise() %>% mutate(!!var := sum(across(.cols = 2:ncol(.)))) %>%
      select(Cod_setor, var)
    
    return(x)
    
    
    # return(x_f)
    
  }
  
  
}

#labelling function
label_names <- list(
  "P001" = "Homens Brancos",
  "P002" = "Homens Pretos",
  "P003" = "Homens Amarelos",
  "P004" = "Homens Pardos",
  "P005" = "Homens Indígenas",
  "P006" = "Mulheres Brancas",
  "P007" = "Mulheres Pretas",
  "P008" = "Mulheres Amarelas",
  "P009" = "Mulheres Pardas",
  "P010" = "Mulheres Indígenas",
  "Ptot_mulheres" = "Mulheres (Total)",
  "Ptot_homens" = "Homens (Total)"
)

labeller_grupos <- function(variable, value){
  return(label_names[value])
}


tema_populacao <- function(base_size){
  theme_void() %+replace%
    theme(legend.position = "bottom",
          strip.text.x = element_text(size=rel(1.5)),
          strip.background = element_rect(
            color = NA,
            fill = "grey70"
          ),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.text = element_blank(),
          axis.ticks = element_blank(), 
          panel.grid = element_blank(),
          plot.margin=unit(c(2,0,0,0),"mm"),
          legend.key.width=unit(2,"line"),
          legend.key.height = unit(.5,"cm"),
          legend.text=element_text(size=rel(1)),
          legend.title=element_text(size=rel(1),                                   ),
          plot.title = element_text(hjust = 0, vjust = 4),
          strip.text = element_text(size = 10)
    )
}  






tabela_variaveis_censo <-  tribble(
  ~ original_name,
  ~ prefix_name,
  "Domicilio01","Dom01",
  "Domicilio02","Dom02",
  "DomicilioRenda","Dom_Renda",
  "Entorno01","Ent01",
  "Entorno02","Ent02",
  "Entorno03","Ent03",
  "Entorno04","Ent04",
  "Entorno05","Ent05",
  "Pessoa01","Pess01",
  "Pessoa02","Pess02",
  "Pessoa03","Pess03",
  "Pessoa04","Pess04",
  "Pessoa05","Pess05",
  "Pessoa06","Pess06" ,
  "Pessoa07","Pess07",
  "Pessoa08","Pess08",
  "Pessoa09","Pess09",
  "Pessoa10","Pess10",
  "Pessoa11","Pess11",
  "Pessoa12","Pess12",
  "Pessoa13","Pess13",
  "PessoaRenda","Pess_Renda",
  "Responsavel01","Resp01",
  "Responsavel02","Resp02",
  "ResponsavelRenda","Resp_Renda",
  
) %>% setDT()

#q

#oteste