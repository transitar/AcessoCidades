#baixa parques, praças e bibliotecas da base do OSM

source('./R/fun/setup.R')
# 
# key <- 'leisure'
# value <- c()
sigla_muni <- 'poa'

create_diretorios <- function(sigla_muni){
  
  dir.create(sprintf("../data-raw/lazer/osm/muni_%s_lazer_osm/", sigla_muni), recursive = TRUE)
  
}

walk(munis_list$munis_df$abrev_muni, create_diretorios)



lazer_osm <- function(munis = 'all'){
  
  salva_lazer_osm <- function(sigla_muni, width = 16.5, height = 16.5){
    
    
    message(paste0('rodando',sigla_muni))
    
    muni_path <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    muni_shape <- read_rds(muni_path)
    
    # mapview(muni_shape)
    #depois de baixar, filtrar peloshape e n'ao pqla bbox
    
    box <- st_bbox(muni_shape)
    
    #Bibliotecas
    
    q_library <- opq(bbox = box) %>%
      add_osm_feature(key = 'amenity', value = 'library') %>% osmdata_sf()
    
    q_library_sf <- q_library$osm_points %>% st_as_sf()
    # mapview(q_library_sf)
    
    #Lazer tag leisure
    
    
    #park contem parques e algumas praças
    
    q_leisure_park <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'park') %>% osmdata_sf()
    
    q_leisure_park_sf <- q_leisure_park$osm_polygons %>% st_as_sf() %>%
      st_centroid()
    # mapview(q_leisure_park_sf)
    
    #golf_course: contém campos de golfe
    q_leisure_golf <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'golf_course') %>% osmdata_sf()
    
    q_leisure_golf_sf <- q_leisure_golf$osm_multipolygons %>% st_as_sf() %>%
      st_centroid()
    
    #dog_park
    q_leisure_dog <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'dog_park') %>% osmdata_sf()
    
    q_leisure_dog_sf <- q_leisure_dog$osm_polygons %>% st_as_sf() %>%
      st_centroid()
    # mapview(q_leisure_dog_sf)
    
    #garden: jardins
    
    q_leisure_garden <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'garden') %>% osmdata_sf()
    
    q_leisure_garden_sf <- q_leisure_garden$osm_polygons %>% st_as_sf() %>%
      st_centroid()
    # mapview(q_leisure_garden_sf)
    
    #nature_reserve reservas naturais
    q_leisure_nature <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'nature_reserve') %>% osmdata_sf()
    
    q_leisure_nature_sf <- q_leisure_nature$osm_polygons %>% st_as_sf() %>%
      st_centroid()
    # mapview(q_leisure_nature_sf)
  
    
    
    #pitch : campos de putebol outdoor
    
    q_leisure_pitch <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'pitch') %>% osmdata_sf()
    
    q_leisure_pitch_sf <- q_leisure_pitch$osm_polygons %>% st_as_sf() %>%
      st_centroid()
    # mapview(q_leisure_pitch_sf)
    
    #playground
    
    q_leisure_playground <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'playground') %>% osmdata_sf()
    
    q_leisure_playground_sf <- q_leisure_playground$osm_polygons %>% st_as_sf() %>%
      st_centroid()
    # mapview(q_leisure_playground_sf)
    
    #stadium 
    q_leisure_stadium <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'stadium') %>% osmdata_sf()
    
    q_leisure_stadium_sf <- q_leisure_stadium$osm_polygons %>% st_as_sf() %>%
      st_centroid()
    # mapview(q_leisure_stadium_sf)
    
    q_leisure_park_sf2 <- q_leisure_park_sf %>% select(osm_id,
                                                       name) %>% mutate(type = "park")
    q_leisure_golf_sf2 <- q_leisure_golf_sf %>% select(osm_id,
                                                       name) %>% mutate(type = "golf")
    q_leisure_garden_sf2 <- q_leisure_garden_sf %>% select(osm_id,
                                                       name) %>% mutate(type = "garden")
    q_leisure_nature_sf2 <- q_leisure_nature_sf %>% select(osm_id,
                                                       name) %>% mutate(type = "nature_reserve")
    q_leisure_pitch_sf2 <- q_leisure_pitch_sf %>% select(osm_id,
                                                       name) %>% mutate(type = "pitch")
    q_leisure_playground_sf2 <- q_leisure_playground_sf %>% select(osm_id,
                                                       name) %>% mutate(type = "playground")
    q_leisure_stadium_sf2 <- q_leisure_stadium_sf %>% select(osm_id,
                                                       name) %>% mutate(type = "stadium")
    q_leisure_dog_sf2 <- q_leisure_dog_sf %>% select(osm_id,
                                                          name) %>% mutate(type = "dog_park")
    
    lazer2 <- rbind(q_leisure_park_sf2,
                    q_leisure_golf_sf2,
                    q_leisure_garden_sf2,
                    q_leisure_nature_sf2,
                    q_leisure_pitch_sf2,
                    q_leisure_playground_sf2,
                    q_leisure_stadium_sf2,
                    q_leisure_dog_sf2)
    
    lazer_final <- lazer2 %>% st_filter(muni_shape)
    
    # mapview(lazer_final)
    
    # mapview(lazer2)
    
    readr::write_rds(lazer_final,
                     sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm.rds',
                             sigla_muni,
                             sigla_muni))

    # ciclo <- st_combine(q_high_sf,q_cycle_sf, q_cycle_right, q_cycle_left)


  }
  
  
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=salva_lazer_osm)
  
}


lazer_osm(munis = 'pal')



