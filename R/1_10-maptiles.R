# devtools::install_github('kauebraga/ceramic',force = T)

rm(list = ls())

source('./R/fun/setup.R')
# source("R/fun/crop_ggmap.R")

# library(ceramic)
# library(geobr)
# library(sf)
# library(qdrmapbox)
# library(tidyverse)
# library(raster)
# library(ggplot2)
# library(geobr)
# library(readxl)

# register mapbox api key
my_api <- data.table::fread("R/api_mapbox.txt", header = F)
Sys.setenv(MAPBOX_API_KEY = my_api$V1)

Sys.getenv("MAPBOX_API_KEY")

sigla_muni = 'RMA'
ano = 2019

# 2.1) Baixar e salvar os maps tiles de todos os municipios ---------------------

baixar_map_tile_ceramic <- function(ano, sigla_muni) {
  
  
  # Criar pasta para salvar arquivos
  
  muni_folder_path <- sprintf("../../data/maptiles_crop/%s/mapbox_inicial_map", ano)
  criate_folder(muni_folder_path)
  
  # code_munis <- munis_list$munis_metro[abrev_muni == sigla_muni & ano_metro == ano]$code_muni %>% 
  #   unlist()
  
  # if(sigla_muni == "dou"){
  #   
  #   muni_folder_path_shape <- sprintf('../data-raw/municipios/%s/municipio_%s_%s_zoom.gpkg',ano,sigla_muni,ano)
  #   
  #   temp_sf <- read_sf(muni_folder_path_shape) 
  #   
  #   temp_sf2 <- temp_sf %>% st_transform(4326) %>% st_buffer(17000)
  #   
  # } else {
  
  muni_folder_path_shape <- sprintf('../data-raw/municipios/%s/municipio_%s_%s.rds',ano,sigla_muni,ano)
  
  temp_sf <- read_rds(muni_folder_path_shape) 
  
  temp_sf2 <- temp_sf %>% st_transform(4326) %>% st_buffer(35000)
  # }
  # mapview(temp_sf)
  
  # temp_sf <- read_municipality(code_muni = muni1,year = 2019) 
  
      # mapview::mapview(temp_sf)
    
    # read shape
    # temp_sf <- read_rds(sprintf("../../data-raw/muni1cipios/%s/muni1cipio_%s_%s.rds", ano, sigla_muni1, ano))
    
    # download tile based on custom template (style)
  
    tile_for <- cc_location(temp_sf2,
                            type = "styles/v1/jlucasao/clbdnab24000014qpo8d3yl6g/tiles"
                            # type = "styles/v1/jlucasao/clal64hz7000414qjw5rxkv7r/tiles",
                            # type = "styles/v1/jlucasao/cl8xeo1n4001515peioq6rk7f/tiles",
                            # mapbox://styles/jlucasao/cl8boo6f6002c14mdxm7dr32r
                            # mapbox://styles/v1/jlucasao/cl8boo6f6002c14mdxm7dr32r
                             # debug = TRUE
                            
                            #https://api.mapbox.com/styles/v1/jlucasao/cl8xeo1n4001515peioq6rk7f.html?title=view&access_token=pk.eyJ1Ijoiamx1Y2FzYW8iLCJhIjoiY2w4YWsyZXd3MDhqODN1cGl2aXhzYzgwbyJ9.FwJIPGPdR3i-hNfNa75J-A&zoomwheel=true&fresh=true#11.4/-2.5291/-44.2844
                            #https://api.mapbox.com/styles/v1/jlucasao/cl8xegzs900c415mvn8w9tkr5.html?title=view&access_token=pk.eyJ1Ijoiamx1Y2FzYW8iLCJhIjoiY2w4YWsyZXd3MDhqODN1cGl2aXhzYzgwbyJ9.FwJIPGPdR3i-hNfNa75J-A&zoomwheel=true&fresh=true#10.74/-3.7465/-38.4776
                            # https://api.mapbox.com/styles/v1/jlucasao/cl8vylwtv000v14oxyp33krfc.html?title=view&access_token=pk.eyJ1Ijoiamx1Y2FzYW8iLCJhIjoiY2w4YWsyZXd3MDhqODN1cGl2aXhzYzgwbyJ9.FwJIPGPdR3i-hNfNa75J-A&zoomwheel=true&fresh=true#10.78/-3.7796/-38.5001
                            #https://api.mapbox.com/styles/v1/jlucasao/cl8vylwtv000v14oxyp33krfc.html?title=view&access_token=pk.eyJ1Ijoiamx1Y2FzYW8iLCJhIjoiY2w4YWsyZXd3MDhqODN1cGl2aXhzYzgwbyJ9.FwJIPGPdR3i-hNfNa75J-A&zoomwheel=true&fresh=true#10.78/-3.7796/-38.5001
                            # https://api.mapbox.com/styles/v1/jlucasao/cl8vylwtv000v14oxyp33krfc.html?title=view&access_token=pk.eyJ1Ijoiamx1Y2FzYW8iLCJhIjoiY2w4YWsyZXd3MDhqODN1cGl2aXhzYzgwbyJ9.FwJIPGPdR3i-hNfNa75J-A&zoomwheel=true&fresh=true#10.43/-3.8313/-38.5157
    )
    
  
    # as rgb data.frame

    tab <- as.data.frame(tile_for, xy = TRUE)
    names(tab) <- c("x", "y", "red", "green", "blue")
    tab$hex <- rgb(tab$red, tab$green, tab$blue, maxColorValue = 255)
    
    ggplot() +
      geom_raster(data = tab, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      theme_map() +
      geom_sf(data = st_transform(temp_sf,3857), fill = NA)
    
    # save tile
    readr::write_rds(tab, sprintf("../data/maptiles_crop/%s/mapbox_2/maptile_crop_mapbox_%s_%s.rds", ano, sigla_muni, ano))
    
  }
  
  
  
  lista.muni <- munis_list$munis_metro$abrev_muni
  
  
  baixar_map_tile_ceramic(ano = 2019,sigla_muni = "slz")
  
  walk(.x = lista.muni,.f = baixar_map_tile_ceramic, ano = 2019)
  


