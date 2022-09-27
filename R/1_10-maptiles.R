# devtools::install_github('kauebraga/ceramic',force = T)

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

sigla_muni = 'man'
ano = 2019

# 2.1) Baixar e salvar os maps tiles de todos os municipios ---------------------

baixar_map_tile_ceramic <- function(ano, sigla_muni) {
  
  
  # Criar pasta para salvar arquivos
  
  muni_folder_path <- sprintf("../../data/maptiles_crop/%s/mapbox_inicial_map", ano)
  criate_folder(muni_folder_path)
  
  # code_munis <- munis_list$munis_metro[abrev_muni == sigla_muni & ano_metro == ano]$code_muni %>% 
  #   unlist()
  
  muni_folder_path_shape <- sprintf('../data-raw/municipios/%s/municipio_%s_%s.rds',ano,sigla_muni,ano)
  
  temp_sf <- read_rds(muni_folder_path_shape) 
  
  temp_sf2 <- temp_sf %>% st_transform(4326) %>% st_buffer(5000)
  
  mapview(temp_sf)
  
  # temp_sf <- read_municipality(code_muni = muni1,year = 2019) 
  
      # mapview::mapview(temp_sf)
    
    # read shape
    # temp_sf <- read_rds(sprintf("../../data-raw/muni1cipios/%s/muni1cipio_%s_%s.rds", ano, sigla_muni1, ano))
    
    # download tile based on custom template (style)
  
    tile_for <- cc_location(temp_sf2,
                            type = "styles/v1/jlucasao/cl8k841k5000214t9acn5n9zk/tiles",
                            # mapbox://styles/jlucasao/cl8boo6f6002c14mdxm7dr32r
                            # mapbox://styles/v1/jlucasao/cl8boo6f6002c14mdxm7dr32r
                             # debug = TRUE
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
    readr::write_rds(tab, sprintf("../../data/maptiles_crop/%s/mapbox/maptile_crop_mapbox_%s_%s.rds", ano, muni1, ano))
    
  }
  
  
  
  lista.muni <- as.numeric(munis_ref$cod_ibge) 
  
  
  baixar_map_tile_ceramic(ano = 2019,muni1 = 1501402)
  
  walk(.x = lista.muni,.f = baixar_map_tile_ceramic, ano = 2019)
  


