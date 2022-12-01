#assentamentos precários
source('./R/fun/setup.R')


#criação dos diretórios

sigla_muni <- 'poa'

create_diretorios <- function(sigla_muni){
  
  dir.create(sprintf("../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/", sigla_muni), recursive = TRUE)
  
}

walk(munis_list$munis_df$abrev_muni, create_diretorios)

filtrar_assentamentos <- function(sigla_muni){

assentamentos <- read_sf('../data-raw/assentamentos_precarios/brasil/AGSN_2019/AGSN_2019.shp') %>%
  st_transform(4326)

assentamentos <- st_make_valid(assentamentos)

mapview(assentamentos)
muni_path <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
muni_shape <- read_rds(muni_path) %>% st_transform(4326)

# maptiles <- read_rds(sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni))
# muni_bbox <- raster::extent(maptiles)


assentamentos_muni <- st_join(assentamentos, muni_shape) %>% drop_na(code_state)
# mapview(assentamentos_muni)


# mapview(muni_shape)
#depois de baixar, filtrar peloshape e n'ao pqla bbox

# box <- st_bbox(muni_shape)

#save the data

write_rds(assentamentos_muni,
          sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                  sigla_muni, sigla_muni))



}

tictoc::tic()
walk(munis_list$munis_df$abrev_muni, filtrar_assentamentos)
tictoc::toc()