#01.8-topografia

###### Leitura e filtro de elevacao/topografia

#' info:
#' Os dados de topografia são provenientes da missão SRTM (_Shuttle Radar 
#' Topography Mission_), que é um esforço de pesquisa internacional que obtém 
#' dados de elevação numa precisão de 30 metros. Os dados de elevação do SRTM são 
#' divididos por quadrículo de 1 grau de latidude e 1 longitude, então é 
#' necessário:
#'   1 - identificar os quadrículos que cobrem a cidade escolhida;
#'   2 - baixar o raster correspondente a cada quadrículos em uma pasta temporária;
#'   3 - unir os grids em um grid único, usando a função raster::mosaic();
#'   4 - recortar do raster a área correspondente ao bounding box do município;
#'   5 - salvar o raster do município na pasta correspondente.
#'   
#' Para ter acesso a esses dados, é necessário criar um login no site
#' https://urs.earthdata.nasa.gov, e informar os dados de usuário e senha quando
#' for rodar esse script.


# carregar bibliotecas

source('./R/fun/setup.R')
library(httr)
library(raster)

#Credenciais para acessar os dados do SRTM
message("Informe usuário e senha para acessar https://urs.earthdata.nasa.gov ")
username <- readline("Usuário : ")
password <- readline("Senha : ")

#função de download dos dados do SRTM
download_srtm <- function(sigla_muni) {
  # read municipality boundary
  message(paste("rodando", sigla_muni))
  
  muni_sf <- readr::read_rds(sprintf( "../data-raw/municipios/2019/municipio_%s_2019.rds", sigla_muni)) %>%
    st_transform(31983)
  muni_sf <- muni_sf %>% st_buffer(15000) %>% st_transform(4326)
  # mapview(muni_sf)
  # muni_sf %>% mapview()
  
  # extract bounding box
  bbox <- st_bbox(muni_sf)
  # bbox <- as.integer(bbox)
  
  # identify which tiles are needed to cover the whole study area
  lons <- seq(floor(bbox[1]), ceiling(bbox[3]), by = 1)
  lats <- seq(floor(bbox[2]), ceiling(bbox[4]), by = 1)
  tiles <- expand.grid(lat = lats, lon = lons) %>%
    mutate(hx = if_else(lon < 0, "W", "E"),
           hy = if_else(lat < 0, "S", "N"))
  tile = sprintf("%s%02d%s%03d", tiles$hy, abs(tiles$lat), tiles$hx, abs(tiles$lon))
  
  # build the url's for each tile
  urls <- paste0("https://e4ftl01.cr.usgs.gov/MEASURES/SRTMGL1.003/2000.02.11/",
                 tile, ".SRTMGL1.hgt.zip")
  
  # download zip files and extract raster tiles
  outputdir <- tempdir()
  zipfiles <- paste0(outputdir, "\\", tile, ".hgt.zip")
  rstfiles <- paste0(outputdir, "\\", tile, ".hgt")
  
  walk2(urls, zipfiles, function(url, filename) {
    httr::GET(url = url, 
              authenticate(username, password),
              write_disk(path =filename, overwrite = TRUE),
              progress())
  })
  
  walk(zipfiles, unzip, exdir = outputdir)
  
  # read all raster tiles, merge them together, and then crop to the study area's bounding box
  rst <- map(rstfiles, raster)
  if (length(rst) == 1) {
    rst_layer <- rst[[1]]
  } else {
    rst_layer <- do.call(raster::mosaic, args = c(rst, fun = mean))
  }
  
  # para a rma acontece de ter uma quadricula completamente vazia, que não é baixada
  #é necessário baixar manualmente no qgis, fazer o mosaico e carrecar no R para cortar
  #ou colocar o buffer para 0 para evitar de baixar a quadricula
  
  # rst_layer <- raster::raster('../00 - Recebidos/C - RM Aracajú - SE/topografia/srtm_mosaic_rma.tif')
  
  rst_layer_crop <- raster::crop(rst_layer, st_bbox(muni_sf))
  
  # save processed raster to the municipality folder
  
  # save tile
  
  dir.create(sprintf("../data-raw/topodata/2020/muni_%s/", sigla_muni), recursive = TRUE)
  raster::writeRaster(rst_layer_crop, 
                      sprintf("../data-raw/topodata/2020/muni_%s/topografia_%s.tif", sigla_muni, sigla_muni),
                      overwrite = TRUE)
}

#Aplicação da função para o município
download_srtm(sigla_muni = "bel")



