#download de dados cicloviários do osm
source('./R/fun/setup.R')

sigla_muni = "dou"

create_diretorios <- function(sigla_muni){
  
  dir.create(sprintf("../data-raw/dados_municipais_osm/muni_%s/", sigla_muni), recursive = TRUE)
  
}

walk(munis_list$munis_df$abrev_muni, create_diretorios)


#download dados de ciclovias do osm

download_osm <- function(munis = 'all'){
  
  salva_ciclo_osm <- function(sigla_muni, width = 16.5, height = 16.5){
    
    
    message(paste0('rodando ',sigla_muni))
    
    muni_path <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    muni_shape <- read_rds(muni_path)
    
    # mapview(muni_shape)
    #depois de baixar, filtrar peloshape e n'ao pqla bbox
    
    box <- st_bbox(muni_shape)
    
    #ciclovias
    key = "highway"
    value = "cycleway"
    type = "osm_lines"
    type_to_col <- "ciclovia"
    
    baixa_feature <- function(key, value = "nada", type, type_to_col) {
      
      if (value == "nada"){
        
        feature <- opq(bbox = box) %>%
          add_osm_feature(key = key) %>% osmdata_sf()
        
        feature_sf <- feature[type] %>% as.data.frame()
        
        if (nrow(feature_sf)==0){
          
          feature_sf <- NA
          
        } else {
          feature_sf <- st_as_sf(feature_sf)
        }
        
      } else {
      
      
      feature <- opq(bbox = box) %>%
        add_osm_feature(key = key, value = value) %>% osmdata_sf()
      
      feature_sf <- feature[type] %>% as.data.frame()
      
      if (nrow(feature_sf)==0){
        
        feature_sf <- NA
        
      } else {
        feature_sf <- st_as_sf(feature_sf)
        
      }

      
      }
      
      if (is.logical(feature_sf) == TRUE){
        
        feature_sf <- feature_sf
        
      } else if (type == "osm_lines"){
        
        feature_sf <- feature_sf %>% select(osm_id = osm_lines.osm_id,
                            name = osm_lines.name, osm_lines.geometry) %>% mutate(tipo = type_to_col)
        # feature_sf <- setNames(feature_sf, c("osm_id", "name", "geometry","tipo"))
      } 
      

      return(feature_sf)
    }
    # mapview(feature_sf)
#problema no sf
    highway_cycleway <- baixa_feature(key = "highway",
                                      value = "cycleway",
                                      type = "osm_lines",
                                      type_to_col = "ciclovia")
    st_geometry(highway_cycleway) <- "Tipo"
    
    cycleway <- baixa_feature(key = "cycleway",
                              type = "osm_lines",
                              type_to_col = "ciclovia")
    st_geometry(cycleway) <- "Tipo"
    cycleway_left <- baixa_feature(key = "cycleway:left",
                                    type = "osm_lines",
                                   type_to_col = "ciclofaixa")
    st_geometry(cycleway_left) <- "Tipo"
    cycleway_right <- baixa_feature(key = "cycleway:right",
                                   type = "osm_lines",
                                   type_to_col = "ciclofaixa")
    st_geometry(cycleway_right) <- "Tipo"

    cycleway_route <- baixa_feature(key = "route",
                                    value = "bycicle",
                                    type = "osm_lines",
                                    type_to_col = "ciclorrota")
    st_geometry(cycleway_route) <- "Tipo"
                                    
    # mapview(cycleway_left)
    
    # dfs_to_rbind <- is.na(cycleway_route)
    # como fazer aqui para que só rbind nos dfs não vazios
    
    ciclo <- rbind(highway_cycleway,
                   cycleway,
                   # cycleway_left,
                   cycleway_right)

    # mapview(ciclo) + mapview(muni_shape)
    
    ciclo_final <- ciclo %>% st_filter(muni_shape)
    
    # mapview(ciclo_final)
    
    # mapview(muni_shape) + mapview(ciclo_final)
    
    write_sf(ciclo_final,
                     sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                             sigla_muni,
                             sigla_muni),
                    layer = "infra_cicloviaria")
    
    #paraciclos 
    
    paraciclos <- opq(bbox = box) %>%
      add_osm_feature(key = "amenity", value = "bicycle_parking") %>% osmdata_sf()
    
    paraciclos_sf <- paraciclos$osm_points %>% as.data.frame() %>% st_as_sf() %>%
      select(osm_id, amenity) %>% mutate(Tipo = "Privado")
    paraciclos_final <- paraciclos_sf %>% st_filter(muni_shape)
    mapview(paraciclos_final)

    st_write(paraciclos_final,
                     sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                             sigla_muni,
                             sigla_muni),
             layer = "paraciclos", append = F)
    
    
  }
  
  
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=salva_lazer_osm)
  
}


lazer_osm(munis = 'pal')
