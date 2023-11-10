
source('./R/fun/setup.R')

sigla_muni <- "rma"

areas_urb_nurb <- function(sigla_muni, width = 16.5, height = 16.5){

message(paste("Rodando",sigla_muni, "\n"))

path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)

data_contorno <- read_rds(path_contorno)

sigla_municipio <- sigla_muni
decisao_muni <- read_excel('../planilha_municipios.xlsx',
                           sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)


area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
                                   sigla_muni)) %>% filter(DN == 24) %>%
  st_make_valid() %>%
  st_union()


if (sigla_muni == "rma"){
  
  code_munis <- c(2800308, 2804805, 2800605, 2806701)
  siglas_munis <- c("arj", "nss", "bac", "sac")
  
  for (i in 1:length(code_munis)){
    
    mun <- geobr::read_municipality(code_muni = code_munis[i])
    area_urbanizada_mun <- mun %>% st_transform(decisao_muni$epsg) %>%
      st_intersection(st_make_valid(st_transform(area_urbanizada, decisao_muni$epsg)))
    
    area_muni <- as.numeric(st_area(mun %>% st_transform(decisao_muni$epsg)))/10^6
    area_urb_km2 <- as.numeric(st_area(st_make_valid(area_urbanizada_mun%>% st_transform(decisao_muni$epsg))))/10^6
    area_nurb_km2 <- as.numeric(st_area(st_difference(mun %>% st_transform(decisao_muni$epsg), st_make_valid(area_urbanizada_mun %>% st_transform(decisao_muni$epsg)))))/10^6
    
    print(paste("A área urbanizada de", siglas_munis[i], "é de", area_urb_km2, "km²"))
    print(paste("A área não urbanizada de", siglas_munis[i], "é de", area_nurb_km2, "km²"))
    print(paste("A área total de", siglas_munis[i], "é de", area_nurb_km2+area_urb_km2, "km²"))
    print(paste("A área total de", siglas_munis[i], "pelo shape ibge é de", area_muni, "km²"))
    print(paste("A diferença área total de", siglas_munis[i], "pelo shape ibge e soma de areas é de", area_muni-(area_nurb_km2+area_urb_km2), "km²"))
    
  }

} else {
  
area_muni <- as.numeric(st_area(data_contorno))/10^6
area_urb_km2 <- as.numeric(st_area(st_make_valid(area_urbanizada)))/10^6
area_nurb_km2 <- as.numeric(st_area(st_difference(data_contorno, st_make_valid(area_urbanizada))))/10^6

print(paste("A área urbanizada de", sigla_muni, "é de", area_urb_km2, "km²"))
print(paste("A área não urbanizada de", sigla_muni, "é de", area_nurb_km2, "km²"))
print(paste("A área total de", sigla_muni, "é de", area_nurb_km2+area_urb_km2, "km²"))
print(paste("A área total de", sigla_muni, "pelo shape ibge é de", area_muni, "km²"))
print(paste("A diferença área total de", sigla_muni, "pelo shape ibge e soma de areas é de", area_muni-(area_nurb_km2+area_urb_km2), "km²"))

}

}

walk(munis_list$munis_df$abrev_muni, .f = areas_urb_nurb)
