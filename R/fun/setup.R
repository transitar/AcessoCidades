Sys.setenv(TZ='UTC') # Fuso horario local

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  
}

lpak<- function(pkg){sapply(pkg, require, character.only = TRUE)}


packages <- c('osmdata',
              'showtext',
              'nngeo',
              'tidyr',
              'raster',
              'ggplot2',
              'ggthemes',
              'sf',
              'data.table',
              'geobr',
              'pbapply',
              'readr',
              'stringr',
              'lubridate',
              'fasttime',
              'mapview',
              'RColorBrewer',
              'extrafont',
              'knitr',
              'furrr',
              'purrr',
              'forcats',
              'future.apply',
              'dplyr',
              'hrbrthemes',
              'beepr',
              'patchwork',
              'Hmisc',
              'opentripplanner',
              'ggmap',
              'h3jsr',
              'bit64',
              'quantreg',
              'gtfstools',
              'readxl',
              'magrittr',
              'ceramic',
              'ggalt',
              'hrbrthemes',
              'ggnewscale',
              'ggsn',
              'ggthemes',
              'rlist',
              'pipeR',
              'xlsx',
              'ggspatial')

suppressMessages(ipak(packages))
suppressMessages(lpak(packages))

rm(packages,lpak,ipak)

# carregar bibliotecas
# suppressMessages(library(osmdata))
# suppressMessages(library(raster))
# suppressMessages(library(ggplot2) )     # visualizacao de dados
# suppressMessages(library(ggthemes)  )   # temas para visualizacao de dados
# suppressMessages(library(sf)  )         # leitura e manipulacao de dados espaciais
# suppressMessages(library(data.table)  ) # manipulacao de dados
# # suppressMessages(library(read.dbc)  )   # leitura de bases relacionais em Microsoft Access
# suppressMessages(library(geobr) )       # dados espaciais do brasil
# suppressMessages(library(pbapply) )     # progress bar
# suppressMessages(library(readr) )       # rapida leitura de dados 
# suppressMessages(library(readr) )       # manipulacao de dados
# suppressMessages(library(stringr) )     # operacoes em strings
# suppressMessages(library(lubridate) )   # dados em data/horario
# suppressMessages(library(fasttime)  )   # rapido processamento deddados em data/horario
# suppressMessages(library(mapview) )     # visualizacao interativa dos dados
# suppressMessages(library(RColorBrewer)) # paleta de cores
# suppressMessages(library(extrafont) )   # fontes de texto
# # suppressMessages(library(bit.64)  )     # lidar com numeros ee 64bits
# suppressMessages(library(knitr))
# suppressMessages(library(furrr))
# suppressMessages(library(purrr))
# suppressMessages(library(forcats))
# suppressMessages(library(future.apply)) # Aplicar funcoes em paralelo
# # suppressMessages(library(h3jsr)) # H3 grade hexagonal
# suppressMessages(library(dplyr))
# suppressMessages(library(hrbrthemes))
# suppressMessages(library(beepr))
# suppressMessages(library(patchwork))
# suppressMessages(library(Hmisc)) # calcular quantis ponderados
# # suppressMessages(library(osmdata)) # Download de dados do OpenStreeteMaps (OSM)
# suppressMessages(library(opentripplanner)) # Usar OTP de dentro do R: https://github.com/ITSLeeds/opentripplanner
# suppressMessages(library(ggmap)) # geocoding
# suppressMessages(library(h3jsr)) #h3 hex remotes::install_github("obrl-soil/h3jsr")
# suppressMessages(library(bit64)) # viz large numbers
# suppressMessages(library(quantreg))
# suppressMessages(library(gtfstools))
# suppressMessages(library(readxl))
# suppressMessages(library(magrittr))
# suppressMessages(library(ceramic))
# suppressMessages(library(ggalt))
# suppressMessages(library(hrbrthemes))
# suppressMessages(library(ggnewscale))
# suppressMessages(library(ggsn))
# suppressMessages(library(ggthemes))
# suppressMessages(library(rlist))
# suppressMessages(library(pipeR))
# suppressMessages(library(xlsx))
# suppressMessages(library(ggspatial))

n_int_digits = function(x) {
  result = floor(log10(abs(x)))
  result[!is.finite(result)] = 0
  result
}

#cores

colors_purple <- c("#F1F2FE","#9FA4F9","#767DCE","#21367D","#1A295B")

colors_orange <- c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")

colors_green <- c("#dbede7", "#79b9a6", "#56a68e", "#0f805e", "#0b5e45", "#094d39", "#073b2c")

colors_escolas <- c("#7736a9",
                    "#e5665d",
                    "#FFE537",
                    "#c57422"
                    )
# #tesyte
# colors_escolas <- c("#EA4D2A",
#                     "#E3651B",
#                     "#FCA001",
#                     "#FFC80B"
# )


colors_viridis <- c("#450D54")

# colors_blue <- c("#eff1f6", "#8d97bd", "#6b79a9", "#344889", "#21367d", "#1d306e", "#19295f")

# colors_blue <- c("#DAE5EF", "#b5cadf", "#6a94be", "#3d73aa", "#25619f", "#054a91", "#05376d")
colors_blue <- c("#ecfaff", "#ade7ff", "#73d6ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#0989c0", "#066891")

colors_acc <- c("#090e20","#111B3F", "#21367D",
                "#37366E", "#4C355F", "#773340",
                "#A23222",
                "#BB4115", "#D34F07", "#E4753A",
                "#EB9432", "#F5C226", "#FAD920",
                "#FDE63A", "#FFF354")
viridis_magma_discrete <- c('#2B105F', '#C43C75', '#F3655C', '#FDE0A1')

aproxima_muni <- function(sigla_muni) {
  
  if (sigla_muni == "pal") {
    coord_sf(ylim = c(-1130754,-1166246), xlim = c(-5403046,-5358070), expand = FALSE)
    
  } else if (sigla_muni == "poa") {
    
    coord_sf(ylim = c(-3492606,-3539050), xlim = c(-5728164, -5676223), expand = FALSE)
    # (-3492606--3539050)/(-5676223--5728164)
  } else if (sigla_muni == "dou") {
    
    coord_sf(ylim = c(-2527577,-2552122), xlim = c(-6084234,-6119851), expand = FALSE)
    
  } else if (sigla_muni == "con") {
    
    
    coord_sf(ylim = c(-2245238,-2277744), xlim = c(-4895103, -4936148), expand = FALSE)
    
  } else if (sigla_muni == "rma") {
    

    coord_sf(ylim = c(-1198511,-1254623), xlim = c(-4161439,  -4091299), expand = FALSE)
    # coord_sf(ylim = c(-1198511,-1254623), xlim = c(-4161439,  -4075243), expand = FALSE)

  } else if (sigla_muni == "noh") {
  
  
  coord_sf(ylim = c(-3455221,-3485093), xlim = c(-5712702,  -5668809), expand = FALSE)

  
  } else if (sigla_muni == "slz") {
    
    coord_sf(ylim = c(-274487,-314218), xlim = c(-4959296,  -4914246), expand = FALSE)

  } else if (sigla_muni == "bel") {
    
    coord_sf(ylim = c(-112607,-172661), xlim = c(-5435689,  -5375866), expand = FALSE)
    # coord_sf(ylim = c(-136461,-165126), xlim = c(-5414704,  -5384011), expand = FALSE)
    
  } else if (sigla_muni == "cit") {
    #zoom
    coord_sf(ylim = c(-2339207,-2389118), xlim = c(-4625581,-4562226), expand = FALSE)
    # coord_sf(ylim = c(-2367514,-2381494), xlim = c(-4592071,  -4573587), expand = FALSE)
    
  } else if (sigla_muni == "vic") {
    #zoom
    coord_sf(ylim = c(-1664674,-1684450), xlim = c(-4568312,-4537114), expand = FALSE)
    
    #Centralizado TP
    # coord_sf(ylim = c(-1664674,-1684450), xlim = c(-4563278,-4531391), expand = FALSE)
    
    
    # coord_sf(ylim = c(-2367514,-2381494), xlim = c(-4592071,  -4573587), expand = FALSE)

    # -4574915,-1657510
    

    
  # } else if (sigla_muni == "man") {
  #   #zoom
  #   coord_sf(ylim = c(-319982,-357198), xlim = c(-6711334,-6657773), expand = FALSE)
  #   # coord_sf(ylim = c(-2367514,-2381494), xlim = c(-4592071,  -4573587), expand = FALSE)
  #   
  #   #   -6657773,-357198
  #   #-6711334,-319982
  #   
  # }
  
  
} else if (sigla_muni == "man") {
  #zoom
  coord_sf(ylim = c(-324276,-353319), xlim = c(-6705378,-6659389), expand = FALSE)
  # coord_sf(ylim = c(-2367514,-2381494), xlim = c(-4592071,  -4573587), expand = FALSE)
  

}
   

}

aproxima_muni_recortes <- function(sigla_muni) {
  
  if (sigla_muni == "pal") {
    
    coord_sf(ylim = c(-1163497,-1134516), xlim = c(-5392552,-5370110), expand = FALSE)
    #razão 1.291373
  } else if (sigla_muni == "poa") {
    
    coord_sf(ylim = c(1.291373*(-5677643--5716380)-3541424,-3541424), xlim = c(-5716380, -5677643), expand = FALSE)

  } else if (sigla_muni == "con") {
    
    coord_sf(ylim = c(1.291373*(-4894680--4917937)-2275309,-2275309), xlim = c(-4917937, -4894680), expand = FALSE)

  } else if (sigla_muni == "dou") {
    
    coord_sf(ylim = c(1.291373*(-6086358--6114267)-2557777,-2557777), xlim = c(-6114267, -6086358), expand = FALSE)

  } 

  else if (sigla_muni == "rma") {
    
    coord_sf(ylim = c(1.291373*(-4119220--4144039)-1244912,-1244912), xlim = c(-4144039, -4119220), expand = FALSE)
    # -4144039,-1233581
  } else if (sigla_muni == "noh") {
    
    coord_sf(ylim = c(1.291373*(-5684469--5699024)-3473777,-3473777), xlim = c(-5699024, -5684469), expand = FALSE)

  } else if (sigla_muni == "slz") {
    
    coord_sf(ylim = c(1.291373*(-4915495--4945559)-312553,-312553), xlim = c(-4945559, -4915495), expand = FALSE)

  } else if (sigla_muni == "bel") {
    
    coord_sf(ylim = c(1.291373*(-5383505--5405857)-165932,-165932), xlim = c(-5405857, -5383505), expand = FALSE)
   
  }  else if (sigla_muni == "cit") {
    coord_sf(ylim = c(1.291373*(-4573728--4586249)-2382024,-2382024), xlim = c(-4586249, -4573728), expand = FALSE)
    
  }  else if (sigla_muni == "vic") {
    
    coord_sf(ylim = c(1.291373*(-4537812--4556965)-1684824,-1684824), xlim = c(-4556965, -4537812), expand = FALSE)
  }  else if (sigla_muni == "man") {
    
    coord_sf(ylim = c(1.291373*(-6658419--6693973)-360661,-360661), xlim = c(-6693973, -6658419), expand = FALSE)
  }
  
  
  
  
  
}


munis_recorte_limites = tribble(
  ~abrev_muni, ~legenda,           ~rec_gen,  ~rec_dif_gen, ~rec_brancos, ~rec_pretos, ~rec_dif_cor, ~rec_amarelos, ~rec_indigenas, ~rec_resp_h, ~res_resp_m, ~rec_dif_resp,
  "poa", "Bairros",      3000,      200,         2000,         1000,       1000,          200,          100,             1500,          1500,          200,
  "bel",  "Bairros / Ilhas",       2000,         300,          1000,      3000,          2000,           40,           8,             600,          600,          200,
  "man",  "Bairros",               1600,         150,         800,      2400,         1500,           40,           20,             400,          400,          150,
  "slz",  "Centro histórico",                   1500,        200,       1000,         2000,       1200,           40,           12,            400,         400,         150,
  "rma",  "Bairros",                   1600,         250,      1000,         2000,        500,           30,           10,            500,         500,         150,
  "noh",  "Bairros",                    600,        100,        1000,          200,         800,           4,          4,             250,          250,          100,
  "dou",  "Setores",                    800,       100,          500,          500,        500,           40,          100,            400,         400,         200,
  "con",  "Unid. de planej.",  1500,      200,         1000,         1500,        200,           40,          20,            160,         160,          80,
  "vic",  "Bairros",         1000,        200,         800,         1400,       600,         10,           4,             300,          300,          150,
  "cit",  "Bairros",        1000,         100,       1200,         1200,       400,         10,           4,             400,          300,          200,
  "pal",  "Área de planej.",      800,       150,          600,         1000,        500,           60,            5,            400,         400,         150,
  
  
) %>% setDT()


munis_names = tribble(

~muni,   ~ uf, ~cod_muni_uf, ~pop, ~cod_ibge, ~muni_abrev, ~estado_nome, ~pop_2022,
"Porto Alegre",	"RS",	"Porto Alegre RS", 	1492530, 4314902, "poa", "Rio Grande do Sul", 1332570,
"Belém", "PA",	"Belém PA",	1506420	, 1501402, "bel", "Pará", 1303389,
"Manaus",	'AM',	"Manaus AM",2255903	, 1302603	, "man", "Amazonas", 2063547,
"São Luís",	"MA", "São Luís MA",	1115932,	2111300,	"slz", "Maranhão", 1037775,
"Aracaju",	"SE",	"Aracaju SE",	672614,	2800308,	"arj", "Sergipe", 602757,
"Nossa Senhora do Socorro",	"SE",	"Nossa Senhora do Socorro SE",	187733,	2804805,	"nss", "Sergipe", 192330,
"Barra dos Coqueiros",	"SE", "Barra dos Coqueiros SE",	31439,	2800605, 	"bac", "Sergipe", 41511,
"São Cristóvão",	"SE",	"São Cristóvão SE",	92090, 2806701,	"sac", "Sergipe", 95612,
"Novo Hamburgo",	"RS", "Novo Hamburgo RS",	247303,	4313409,	"noh", "Rio Grande do Sul", 227732,
"Contagem",	"MG",	"Contagem MG",	673849,	3118601,	"com", "Minas Gerais", 621865,
"Vitória da Conquista",	"BA",	"Vitória da Conquista BA",	343643,	2933307,	"vic", "Bahia", 370868,
"Cachoeiro de Itapemirim", 	"ES",	"Cachoeiro de Itapemirim ES",	212172,	3201209	, "cit", "Espírito Santo", 185784,
"Palmas",	"TO",	"Palmas TO",	313349,	1721000,	"pal", "Tocantins", 302692,
"Dourados", "MS", "Dourados MS", 243368, 5003702, "dou", "Mato Grosso do Sul", 243368

) %>% setDT()


munis_pnad = tribble(
  
  ~muni,   ~ pnad, ~nome_pnad,
  "poa",	1, "Município de Porto Alegre (RS)",
  "bel", 1, "Município de Belém (PA)",
  "man",	1, "Município de Manaus (AM)",
  "slz",	1, "Município de São Luís (MA)",
  "rma",	1, "Município de Aracaju (SE)",
  "noh",	0, NA,
  "dou",	0, NA,
  "con",	0, NA,
  "vic",	0, NA,
  "cit", 	0, NA,
  "pal",	1, "Município de Palmas (TO)"
  
) %>% setDT()

# munis_legend_position = tribble(
#   ~abrev_muni,  ~lisa_renda,           ~annotations,        ~lisa_cor,     ~lisa_resp, ~rec_pretos, ~rec_dif_cor, ~rec_amarelos, ~rec_indigenas, ~rec_resp_h, ~res_resp_m, ~rec_dif_resp,
#   "poa",              NA,                       NA,                 NA,         2000,         1000,       1000,          200,          100,             1500,          1500,          200,
#   "bel",              NA,                      NA ,                NA,         NA,           NA,           NA,             NA,          NA,          NA,
#   "man",              NA,                       NA,                NA,           NA,           NA,         NA,           NA,           NA,             NA,          NA,          NA,
#   "slz",              NA,                      NA,                 NA,           NA,           NA,         NA,           NA,           NA,             NA,          NA,          NA,
#   "rma",              list(0.77,0.26),    list("tl","bl"),       list(0.78,0.26),      1000,         2000,        500,           30,           10,            500,         500,         150,
#   "noh",              NA,                       NA,                 NA,           NA,           NA,         NA,           NA,           NA,             NA,          NA,          NA,
#   "dou",              NA,                       NA,                  NA,          500,          500,        500,           40,          100,            400,         400,         200,
#   "con",              NA,                       NA,                 NA,         1000,         1500,        200,           40,          20,            160,         160,          80,
#   "vic",              NA,                       NA,                   NA,           NA,           NA,         NA,           NA,           NA,             NA,          NA,          NA,
#   "cit",              NA,                       NA,                   NA,           NA,           NA,         NA,           NA,           NA,             NA,          NA,          NA,
#   "pal",              NA,                       NA,                  NA,          600,         1000,        500,           60,            5,            400,         400,         150,
#   
#   
# ) %>% setDT()



aproxima_muni_zoom <- function(sigla_muni) {
  
  if (sigla_muni == "pal") {
    coord_sf(ylim = c(-1130754,-1166246), xlim = c(-5398510,-5358070), expand = FALSE)
  } else if (sigla_muni == "dou"){
    
    coord_sf(ylim = c(-2511960,-2558183), xlim = c(-6057297,-6171192), expand = FALSE)
    
  } else if (sigla_muni == "cit"){
    
    coord_sf(ylim = c(-2367514,-2381494), xlim = c(-4592071,  -4573587), expand = FALSE)
    
    #sem zoom
    # coord_sf(ylim = c(-2339207,-2389118), xlim = c(-4625581,-4562226), expand = FALSE)

  } else if (sigla_muni == "vic"){
    
    coord_sf(ylim = c(-1637108,-1754082), xlim = c(-4652239,  -4494324), expand = FALSE)
    
    #sem zoom
    
    
  }
  
}

aproxima_muni_zoom_paraciclos <- function(sigla_muni) {
  

    if (sigla_muni == "bel"){
    
    coord_sf(ylim = c(-148051,-164339), xlim = c(-5406472,-5388222), expand = FALSE)

    } else if (sigla_muni == "rma"){
      
      coord_sf(ylim = c(-1216661,-1231094), xlim = c(-4132955,-4111730), expand = FALSE)
      
    }
  
}


munis_list <- list(
  
  munis_df_aop = tribble(
    ~code_muni, ~abrev_muni, ~name_muni,        ~abrev_estado,  ~map_plot_ratio_wh,
    2304400,    "for",       "Fortaleza",       "CE",           1.2,
    3550308,    "spo",       "Sao Paulo",       "SP",           0.65,
    3304557,    "rio",       "Rio de Janeiro",  "RJ",           1.91,
    4106902,    "cur",       "Curitiba",        "PR",           0.62,
    4314902,    "poa",       "Porto Alegre",    "RS",           0.75,
    3106200,    "bho",       "Belo Horizonte",  "MG",           0.69,
    5300108,    "bsb",       "Brasilia",        "DF",           1.71,
    2927408,    "sal",       "Salvador",        "BA",           1.36,
    1302603,    "man",       "Manaus",          "AM",           1.27,
    2611606,    "rec",       "Recife",          "PE",           0.68,
    5208707,    "goi",       "Goiania",         "GO",           0.93,
    1501402,    "bel",       "Belem",           "PA",           0.65,
    3518800,    "gua",       "Guarulhos",       "SP",           0.91,
    3509502,    "cam",       "Campinas",        "SP",           1.20,
    2111300,    "slz",       "Sao Luis",        "MA",           0.78,
    3304904,    "sgo",       "Sao Goncalo",     "RJ",           1.21,
    2704302,    "mac",       "Maceio",          "AL",           0.74,
    3301702,    "duq",       "Duque de Caxias", "RJ",           0.61,
    5002704,    "cgr",       "Campo Grande",    "MS",           0.87,
    2408102,    "nat",       "Natal",           "RN",           0.70
  ) %>% setDT(),


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
    "cit",       2019,     3201209, "Cachoeiro de Itapemirim", "ES", 1,
    "pal",       2019,     1721000, "Palmas", "TO", 1
    
    
  ) %>% setDT(),
  
  munis_modo = tribble(
    ~abrev_muni, ~`2017`,  ~`2018`,  ~`2019`,  ~`2022`, 
    "poa",       "todos",  "todos",  "todos",  "todos",     
    "bel",       "todos",  "todos",  "todos",  "ativo",   
    "man",       "ativo",  "todos",  "todos",  "ativo",   
    "slz",       "todos",  "todos",  "todos",  "todos",   
    "rma",       "todos",  "todos",  "todos",  "todos",   
    "noh",       "todos",  "todos",  "todos",  "todos",   
    "dou",       "ativo",  "ativo",  "ativo",  "todos",   
    "con",       "ativo",  "ativo",  "ativo",  "todos",   
    "vic",       "ativo",  "ativo",  "ativo",  "ativo",   
    "cit",       "ativo",  "ativo",  "todos",  "ativo",   
    "pal",       "ativo",  "ativo",  "todos",  "todos"   
      
  ) %>% 
    pivot_longer(cols = `2017`:`2022`, names_to = "ano_modo", values_to = "modo") %>% 
    setDT()
  
  
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
label_names2 <- tribble(~variavel, ~legenda,
  "P001" , "Homens Brancos",
  "P002","Homens Pretos",
  "P003" , "Homens Amarelos",
  "P004" , "Homens Pardos",
  "P005" , "Homens Indígenas",
  "P006" , "Mulheres Brancas",
  "P007" , "Mulheres Pretas",
  "P008" , "Mulheres Amarelas",
  "P009" , "Mulheres Pardas",
  "P010" , "Mulheres Indígenas",
  "Ptot_mulheres" , "Mulheres (Total)",
  "Ptot_homens" , "Homens (Total)"
)

label_names <- list(
                       "P001" = "Homens Brancos",
                       "P002"="Homens Pretos",
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