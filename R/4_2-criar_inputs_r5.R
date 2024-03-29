###### Criar inputs do OpenTripPlanner
# 1. pontos de origem e destino
# 2. Scripts em Python

# carregar bibliotecas
options(java.parameters = '-Xmx5G')
source('./R/fun/setup.R')

sigla_muni <- 'man'
ano <- 2022

### 1) Funcao para gerar pontos de origem e destino -----------------------------------
endereco_grade <- grades_muni <- paste0(
  "../data/hex_municipio/",
  list.files(
    "../data/hex_municipio/",
    pattern = paste0(sigla_muni,"_09.rds")
  )
)

# endereco_grade <- '../data-raw/censo_2021_info_muni_treated_v2/muni_pal.rds'

gerar_pontos_r5 <- function(sigla_muni, ano) {
  
  # sigla_muni <- "for"; ano <- 2017
  # sigla_muni <- "mac"; ano <- 2017
  # sigla_muni <- "cam"; ano <- 2017
  # sigla_muni <- "bsb"; ano <- 2017
  # ano <- 2019
  
  # status message
  message("Working on city ", sigla_muni, " at year ", ano, "\n")
  
  # lista grades relativas ao municipio - um arquivo para res. 8 e outro pra 9
  grades_muni <- paste0(
    "../data/hex_municipio/",
    list.files(
      "../data/hex_municipio/",
      pattern = paste0(sigla_muni,"_09.rds")
    )
  )
  
  # gera r5r_core usado pra fazer o snap dos pontos
  endereco_rede <- sprintf("../r5r/network/2022/muni_%s/", sigla_muni)
  r5r_core <- r5r::setup_r5(endereco_rede, verbose = FALSE)
  
  # gera os pontos a serem utilizados para cada resolucao
  gerar_por_resolucao <- function(endereco_grade) {
    # muni_res <- dir_muni[2]
    # endereco_grade <- grades_muni[2]
    
    # cria variavel dummy pra identificar se o hexagono tem populacao/oports
    
    #idealmente essa já deve ser a grade com todos os dados no hexagono
    
    grade <- data.table::setDT(readRDS(endereco_grade))
    # grade[
    #   , 
    #   tem_pop_oport := 
    #     pop_total > 0 | 
    #     renda_total > 0 |
    #     empregos_total > 0 | 
    #     saude_total > 0 | 
    #     edu_total > 0
    # ]
    
    # identifica resolucao utilizada
    res <- stringr::str_extract(endereco_grade, "\\d{2}(?=_)")
    
    # gera centroides e faz snap
    # suprime warnings de calculo de centroides com lat long
    suppressWarnings(
      centroides <- sf::st_centroid(sf::st_as_sf(grade)) %>% rename(id = id_hex)
    )
    snaps <- r5r::find_snap(r5r_core, centroides)
    
    # traz a informacao se o hexagono tem populacao/oportunidades e filtra os
    # pontos a serem utilizados com base nisso
    # mantem apenas os pontos cujo snap foi aceitavel (<= 452 metros, caiu no 
    # maximo em um dos seus vizinhos imediatos) ou cujo snap foi ruim ou
    # inexistente, mas que possuem alguma populacao/oportunidade
    data.table::setnames(snaps, "point_id", "id_hex")
    snaps[grade, on = "id_hex"] #, tem_pop_oport := i.tem_pop_oport] (usar quanto tiver os dados)
    snaps[, distance := as.numeric(distance)]
    antes <- nrow(snaps)
    
    #foram deixados apenas os com distância <452, pq?
    snaps <- snaps[distance <= 452] # | tem_pop_oport==TRUE]
    # snaps1 <- snaps[tem_pop_oport==TRUE]
    depois <- nrow(snaps)
    
    # mantem apenas as colunas de id, lat e lon, e renomeia lat e lon pra Y e X
    snaps <- snaps[, .(id_hex, X = lon, Y = lat)]
    
    # snaps_sf <- snaps %>% st_as_sf(coords = c("X", "Y"), crs = 4326)
    # mapview(snaps_sf, zcol = NULL, legend = NULL)
    
    # salva resultado
    
    create_diretorios <- function(sigla_muni){
      
      dir.create(sprintf("../r5r/points/%s/",ano), recursive = TRUE)
      
    }
    
    arquivo_resultado <- sprintf("../r5r/points/%s/points_muni_%s_%s_%s.csv", ano, sigla_muni, res,ano)
    # arquivo_resultado <- paste0(
    #   "../r5/points/", ano,
    #   "/points_", sigla_muni, "_", res, "_", ano, ".csv"
    # )
    data.table::fwrite(snaps, arquivo_resultado)
    
    resumo <- data.frame(sigla_muni = sigla_muni,
                         ano = ano,
                         res = res,
                         points_r5 = antes,
                         points_out = depois)
  }
  
  resumo <- lapply(grades_muni, gerar_por_resolucao) %>%
    rbindlist()
  
  # r5r::stop_r5(r5r_core)
  
  return(resumo)
  
}

gerar_pontos_r5r(sigla_muni = "rma", ano = 2022)
# write do gsheets
a <- lapply(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, gerar_pontos_OTP_muni, ano = 2017)
b <- lapply(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, gerar_pontos_OTP_muni, ano = 2018)
c <- lapply(munis_list$munis_metro[ano_metro == 2019]$abrev_muni, gerar_pontos_OTP_muni, ano = 2019)

go <- rbindlist(c(a, b, c))
go <- go[res == "09"]
go <- go %>%
  mutate(points_perc = points_out/points_r5) %>%
  rename(points_total = points_r5,
         points_r5 = points_out)
googlesheets4::write_sheet(data = go,
                           ss = "https://docs.google.com/spreadsheets/d/11pIp1Ioiua7NWDp41DKXMMWI_3JvmzonhNMoNpvR1aE/edit#gid=215944254",
                           sheet = "points_snap_new")



