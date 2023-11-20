#baixa parques, praças e bibliotecas da base do OSM


#Esse script baixa dados de lazer do OpenStreetMap

#São baixados os equipamentos catalogados como bibliotecas,
#parques, praças, petplace, jardins, reservas naturais,
#campos de futebol e quadras de esportes, playgrounds,
#estádios, praias, atrações turísticas, píeres,
#centros comunitários e espaços de pique-nique.


#Para utilizar esse scrip, forneça a sigla do seu município
#no argumento munis

rm(list =ls())
source('./R/fun/setup.R')

sigla_muni <- 'bel'

lazer_osm <- function(munis = 'all'){
  
  salva_lazer_osm <- function(sigla_muni, width = 16.5, height = 16.5){
    
    create_diretorios <- function(sigla_muni){
      
      dir.create(sprintf("../data-raw/lazer/osm/muni_%s_lazer_osm/", sigla_muni), recursive = TRUE)
      
    }
    
    create_diretorios(sigla_muni = sigla_muni)
    
    message(paste0('rodando ',sigla_muni))
    
    muni_path <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    muni_shape <- read_rds(muni_path)
    
    # mapview(muni_shape)

    box <- st_bbox(muni_shape)

    #Bibliotecas
    
    q_library <- opq(bbox = box) %>%
      add_osm_feature(key = 'amenity', value = 'library') %>% osmdata_sf()
    
    #mapview(q_library_sf)
    # q_library_sf <- q_library$osm_points %>% st_as_sf() %>%
    #   st_centroid()
    q_library_sf <- q_library$osm_polygons %>% st_as_sf() %>%
      st_centroid()
    
    q_library_sf_points <- q_library$osm_points %>% st_as_sf()
    #mapview(q_library_sf_points)
    
    st_write(q_library_sf_points, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                   sigla_muni,
                                   sigla_muni),
             layer = "bibliotecas",
             append = F)
    # mapview(q_library_sf)
    
    #Lazer tag leisure
    
    
    #park contem parques e algumas praças
    
    q_leisure_park <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'park') %>% osmdata_sf()
    
    q_leisure_park_sf <- q_leisure_park$osm_polygons %>% st_as_sf() %>%
      st_geometry() %>% 
      st_centroid(.)
    
    # q_leisure_park_sf_points <- q_leisure_park$osm_points %>%st_as_sf()
    # st_centroid(q_leisure_park_sf)
    # mapview(q_leisure_park_sf)
    # mapview(q_leisure_park_sf_points)
    
    st_write(q_leisure_park_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                   sigla_muni,
                                   sigla_muni),
             layer = "parques_pracas")
    
    #golf_course: contém campos de golfe
    # q_leisure_golf <- opq(bbox = box)%>%
    #   add_osm_feature(key = 'leisure', value = 'golf_course') %>% osmdata_sf()
    # 
    # q_leisure_golf_sf <- q_leisure_golf$osm_multipolygons %>% st_as_sf() %>%
    #   st_centroid()
    
    # dog_park
    q_leisure_dog <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'dog_park') %>% osmdata_sf()

    q_leisure_dog_sf <- q_leisure_dog$osm_polygons %>% st_as_sf() %>% st_geometry() %>% 
      st_centroid()
    # mapview(q_leisure_dog_sf)
    st_write(q_leisure_dog_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                        sigla_muni,
                                        sigla_muni),
             layer = "parques_cachorros")
    #garden: jardins
    
    q_leisure_garden <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'garden') %>% osmdata_sf()
    
    q_leisure_garden_sf <- q_leisure_garden$osm_polygons %>% st_as_sf() %>%st_geometry() %>% 
      st_centroid()
    # mapview(q_leisure_garden_sf)
    st_write(q_leisure_garden_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                        sigla_muni,
                                        sigla_muni),
             layer = "jardins")
    #nature_reserve reservas naturais
    q_leisure_nature <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'nature_reserve') %>% osmdata_sf()
    
    q_leisure_nature_sf <- q_leisure_nature$osm_polygons %>% st_as_sf() %>%st_geometry() %>% 
      st_centroid()
    # mapview(q_leisure_nature_sf)
    st_write(q_leisure_nature_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                        sigla_muni,
                                        sigla_muni),
             layer = "reservas_naturais")
    
    
    #pitch : campos de putebol outdoor
    
    q_leisure_pitch <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'pitch') %>% osmdata_sf()
    
    q_leisure_pitch_sf <- q_leisure_pitch$osm_polygons %>% st_as_sf() %>%st_geometry() %>% 
      st_centroid()
    # mapview(q_leisure_pitch_sf)
    st_write(q_leisure_pitch_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                        sigla_muni,
                                        sigla_muni),
             layer = "campos_futebol")
    #playground
    
    q_leisure_playground <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'playground') %>% osmdata_sf()
    
    q_leisure_playground_sf <- q_leisure_playground$osm_polygons %>% st_as_sf() %>%st_geometry() %>% 
      st_centroid()
    
    q_leisure_playground_sf_points <- q_leisure_playground$osm_points %>% st_as_sf() %>%st_geometry() %>% 
      st_centroid()
    # mapview(q_leisure_playground_sf)
    # mapview(q_leisure_playground_sf_points)
    
    st_write(q_leisure_playground_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                        sigla_muni,
                                        sigla_muni),
             layer = "playgrounds")
    st_write(q_leisure_playground_sf_points, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                              sigla_muni,
                                              sigla_muni),
             layer = "playgrounds",
             append = T)
    #stadium 
    q_leisure_stadium <- opq(bbox = box)%>%
      add_osm_feature(key = 'leisure', value = 'stadium') %>% osmdata_sf()
    
    q_leisure_stadium_sf <- q_leisure_stadium$osm_polygons %>% st_as_sf() %>%st_geometry() %>% 
      st_centroid()
    # mapview(q_leisure_stadium_sf)
    
    st_write(q_leisure_stadium_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                        sigla_muni,
                                        sigla_muni),
             layer = "estadios")
    
    
    #praia
    
    q_leisure_praia <- opq(bbox = box)%>%
      add_osm_feature(key = 'natural', value = 'beach') %>% osmdata_sf()
    
    q_leisure_praia_sf <- q_leisure_praia$osm_polygons %>% st_as_sf() %>%st_geometry() %>% 
      st_centroid()
    # mapview(q_leisure_praia_sf)
    
    st_write(q_leisure_praia_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                           sigla_muni,
                                           sigla_muni),
             layer = "praias")
    
    #atração turistica
    
    
    q_tourist_attraction <- opq(bbox = box)%>%
      add_osm_feature(key = 'tourism', value = 'attraction') %>% osmdata_sf()
    
    q_tourist_attraction_sf <- q_tourist_attraction$osm_polygons %>% st_as_sf() %>%st_geometry() %>% 
      st_centroid()
    # mapview(q_tourist_attraction_sf)
    
    st_write(q_tourist_attraction_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                         sigla_muni,
                                         sigla_muni),
             layer = "atracao_turistica")
    
    
    #pier
    
    
    q_tourist_pier <- opq(bbox = box)%>%
      add_osm_feature(key = 'man_made', value = 'pier') %>% osmdata_sf()
    
    q_tourist_pier_sf <- q_tourist_pier$osm_polygons %>% st_as_sf() %>%st_geometry() %>% 
      st_centroid()
    # mapview(q_tourist_attraction_sf)
    
    st_write(q_tourist_pier_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                              sigla_muni,
                                              sigla_muni),
             layer = "pier")
    # mapview(q_tourist_pier_sf)
    
    #community_centre
    
    
    q_tourist_centre <- opq(bbox = box)%>%
      add_osm_feature(key = 'amenity', value = 'community_centre') %>% osmdata_sf()
    
    q_tourist_centre_sf <- q_tourist_centre$osm_polygons %>% st_as_sf() %>%st_geometry() %>%
      st_centroid()
    # mapview(q_tourist_attraction_sf)
    
    st_write(q_tourist_centre_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                              sigla_muni,
                                              sigla_muni),
             layer = "centros_comunitarios")
    
    
    #picnic-site
    
    
    q_tourist_picnic <- opq(bbox = box)%>%
      add_osm_feature(key = 'tourism', value = 'picnic_site') %>% osmdata_sf()
    
    q_tourist_picnic_sf <- q_tourist_picnic$osm_polygons %>% st_as_sf() %>%st_geometry() %>%
      st_centroid()
    # mapview(q_tourist_attraction_sf)
    
    st_write(q_tourist_picnic_sf, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                              sigla_muni,
                                              sigla_muni),
             layer = "picnic")
    
  }
  
  
  if (munis == "all") {
    
    create_diretorios <- function(sigla_muni){
      
      dir.create(sprintf("../data-raw/lazer/osm/muni_%s_lazer_osm/", sigla_muni), recursive = TRUE)
      
    }
    
    walk(munis_list$munis_df$abrev_muni, create_diretorios)
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=salva_lazer_osm)
  
}

lazer_osm(munis = 'bel')


consolida_lazer <- function(munis = 'all'){
  
  salva_lazer_rds <- function(sigla_muni){
    
    
  
  
  #leitura do arquivo filtrado:
  
  # mapview(q_leisure_playground_sf)
  
  q_library_sf <- st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                  sigla_muni,
                                  sigla_muni),
                          layer = "bibliotecas")
  
  q_leisure_park_sf <- st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                       sigla_muni,
                                       sigla_muni),
                               layer = "parques_pracas")
  
  q_leisure_dog_sf <- st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                      sigla_muni,
                                      sigla_muni),
                              layer = "parques_cachorros")
  
  q_leisure_garden_sf <-  st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                          sigla_muni,
                                          sigla_muni),
                                  layer = "jardins") 
  
  q_leisure_nature_sf <-  st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                          sigla_muni,
                                          sigla_muni),
                                  layer = "reservas_naturais")
  
  q_leisure_pitch_sf <-  st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                         sigla_muni,
                                         sigla_muni),
                                 layer = "campos_futebol")
  
  q_leisure_playground_sf <-  st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                              sigla_muni,
                                              sigla_muni),
                                      layer = "playgrounds")
  
  q_leisure_stadium_sf <-  st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                           sigla_muni,
                                           sigla_muni),
                                   layer = "estadios")
  
  q_leisure_praia_sf <-  st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                         sigla_muni,
                                         sigla_muni),
                                 layer = "praias")
  
  q_tourist_attraction_sf <-  st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                              sigla_muni,
                                              sigla_muni),
                                      layer = "atracao_turistica")
  
  
  q_tourist_pier_sf <-  st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                        sigla_muni,
                                        sigla_muni),
                                layer = "pier")
  
  q_tourist_centre_sf <-  st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                          sigla_muni,
                                          sigla_muni),
                                  layer = "centros_comunitarios")
  
  q_tourist_picnic_sf <-  st_read(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_full.gpkg',
                                          sigla_muni,
                                          sigla_muni),
                                  layer = "picnic")
  
  
  q_library_sf2 <- q_library_sf %>% select(osm_id,
                                           name) %>% mutate(type = "library")
  
  q_leisure_park_sf2 <- q_leisure_park_sf %>%
    # select(osm_id,name) %>% 
    mutate(type = "park")
  # q_leisure_golf_sf2 <- q_leisure_golf_sf %>% select(osm_id,
  #                                                    name) %>% mutate(type = "golf")
  q_leisure_garden_sf2 <- q_leisure_garden_sf %>%
    # select(osm_id,name) %>%
    mutate(type = "garden")
  
  q_leisure_nature_sf2 <- q_leisure_nature_sf %>%
    # select(osm_id,name) %>%
    mutate(type = "nature_reserve")
  
  q_leisure_pitch_sf2 <- q_leisure_pitch_sf %>%
    # select(osm_id,name) %>%
    mutate(type = "pitch")
  
  q_leisure_playground_sf2 <- q_leisure_playground_sf %>%
    # select(osm_id,name) %>%
    mutate(type = "playground")
  
  q_leisure_stadium_sf2 <- q_leisure_stadium_sf %>%
    # select(osm_id,name) %>%
    mutate(type = "stadium")
  
  q_leisure_dog_sf2 <- q_leisure_dog_sf %>%
    # select(osm_id,name = leisure) %>%
    mutate(type = "dog_park")
  
  q_leisure_praia_sf2 <- q_leisure_praia_sf %>%
    # select(osm_id,name = natural) %>%
    mutate(type = "beach")
  
  q_tourist_attraction_sf2 <- q_tourist_attraction_sf %>%
    # select(osm_id,name = name) %>%
    mutate(type = "attraction")
  
  q_tourist_pier_sf2 <- q_tourist_pier_sf %>%
    # select(osm_id,name = name) %>%
    mutate(type = "pier")
  
  q_tourist_centre_sf2 <- q_tourist_centre_sf %>%
    # select(osm_id,name = name) %>%
    mutate(type = "community_centre")
  
  q_tourist_picnic_sf2 <- q_tourist_picnic_sf %>%
    # select(osm_id,name = name) %>%
    mutate(type = "picnic")
  
  
  lazer2 <- rbind(
    # q_library_sf2,
    q_leisure_park_sf2,
    # q_leisure_golf_sf2,
    q_leisure_garden_sf2,
    q_leisure_nature_sf2,
    q_leisure_pitch_sf2,
    q_leisure_playground_sf2,
    q_leisure_stadium_sf2,
    q_leisure_dog_sf2,
    q_leisure_praia_sf2,
    q_tourist_attraction_sf2,
    q_tourist_pier_sf2,
    q_tourist_centre_sf2,
    q_tourist_picnic_sf2
  )
  
  lazer_final <- lazer2 %>% st_filter(muni_shape)
  
  # mapview(lazer_final)
  
  # mapview(lazer2)
  
  readr::write_rds(lazer_final,
                   sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm.rds',
                           sigla_muni,
                           sigla_muni))
  
  write_sf(lazer_final,
           sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm_final.gpkg',
                   sigla_muni,
                   sigla_muni))
  
  }
  
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=salva_lazer_rds)
  
  
}

consolida_lazer(munis = 'bel')
