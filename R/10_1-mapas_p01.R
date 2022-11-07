
rm(list = ls())


source('./R/fun/setup.R')
width <- 16.5
height <- 16.5

sigla_muni <- 'poa'

graficos <- function(munis = "all"){
  
  
  
  faz_grafico_e_salva <- function(sigla_muni, width = 16.5, height = 16.5){
    
    message(paste("Rodando",sigla_muni, "\n"))
    
    path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    
    path_muni_data_censo <- sprintf('../data-raw/censo_2021_info_muni_treated_v2/muni_%s.rds',sigla_muni)
    path_muni_hex <- sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni)
    path_muni_setor <- sprintf('../data-raw/setores_censitarios/2019/setores_%s_2019.rds',sigla_muni)
    path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
    
    data_muni <- read_rds(path_muni_data_censo) %>%  mutate(Cod_setor = as.character(Cod_setor))
    data_msetor <- read_rds(path_muni_setor)
    data_mhex <- read_rds(path_muni_hex)
    data_contorno <- read_rds(path_contorno)
    
    maptiles <- read_rds(path_maptiles)
    
    data_complete <- data_mhex %>% left_join(data_muni, by = c("id_hex"="Cod_setor")) %>% 
      mutate(area = st_area(.)/10^6) %>%
      rowwise() %>% 
      mutate(Ptot_mulheres = sum(P006,P007,P008,P009,P010),
             Ptot_homens = sum(P001,P002,P003,P004,P005))
    

    # mulheres 
    
    teste_m <- data_complete %>% select(code_tract,P006,P007,P008,P009,P010,Ptot_mulheres) %>% 
      gather(key = dado,value = valor, 2:7) %>% 
      mutate(valor = as.numeric(valor))
     colors_blue <- c("#F1F2FE","#9FA4F9","#767DCE","#21367D","#1A295B")
     
     colors_orange <- c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
     

     map_m <- ggplot() +
       geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
       coord_equal() +
       scale_fill_identity()+
       # nova escala
       new_scale_fill() +
       # theme_map() +
       geom_sf(data = st_transform(teste_m,3857),aes(fill = valor),color = NA) +
       
       geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .7) +
       
       facet_wrap(~dado, labeller = labeller_grupos) +
       scale_fill_gradientn(
         name = "Nº de Habitantes",
         colors =colors_blue ,
         # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
         # values = NULL,
         space = "Lab",
         na.value = NA,
         # guide = "colourbar",
         aesthetics = "fill",
         # colors
       ) +
       # tema_populacao()
       theme(legend.position = "bottom",
             strip.text.x = element_text(size=rel(1.2)),
             strip.background = element_rect(
               color = NA,
               fill = "#eff0f0"
             ),
             panel.background = element_rect(fill = NA, colour = NA),
             axis.text = element_blank(),
             axis.title = element_blank(),
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
     
    
    
    
    # homens 
    
    teste_h <- data_complete %>% select(code_tract,P001,P002,P003,P004,P005,Ptot_homens) %>% 
      gather(key = dado,value = valor, 2:7) %>% 
      mutate(valor = as.numeric(valor))
    
    
    
    
    
    map_h <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(teste_h,3857),aes(fill = valor),color = NA) +

      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .7) +
      
      facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Habitantes",
        colors =colors_orange ,
        # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
        # values = NULL,
        space = "Lab",
        na.value = NA,
        # guide = "colourbar",
        aesthetics = "fill",
        # colors
      ) +
      # tema_populacao()
      theme(legend.position = "bottom",
            strip.text.x = element_text(size=rel(1.2)),
            strip.background = element_rect(
              color = NA,
              fill = "#eff0f0"
            ),
            panel.background = element_rect(fill = NA, colour = NA),
            axis.text = element_blank(),
            axis.title = element_blank(),
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
    
    #Escrita do png do mapa
    
    ggsave(map_m,
           device = "png",
           filename =  sprintf("../data/map_plots/muni_%s/1-populacao_mulheres_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    ggsave(map_h,
           device = "png",
           filename =  sprintf("../data/map_plots/muni_%s/2-populacao_homens_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )

  }
  
  # 2. Aplica funcao ------------------------------------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=faz_grafico_e_salva)
  
  
}

graficos(munis = "all")   
graficos(munis = 'con')

#funcao print mapas empregos
features <-as.data.frame( available_features ())


ciclovias_osm <- function(munis = 'all'){
  
  faz_grafico_e_salva_ciclo_osm <- function(sigla_muni, width = 16.5, height = 16.5){
    
    muni_path <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    muni_shape <- read_rds(muni_path)
    
    box <- st_bbox(muni_shape)
    q_high <- opq(bbox = box) %>%
      add_osm_feature(key = 'highway', value = 'cycleway') %>% osmdata_sf()
    
    q_high_sf <- q_high$osm_lines %>% st_as_sf()
    
    q_cycle <- opq(bbox = box)%>%
      add_osm_feature(key = 'cycleway')%>% osmdata_sf()
    
    q_cycle_sf <- q_cycle$osm_lines %>% st_as_sf()
    
    q_cycle_left <- opq(bbox = box)%>%
       add_osm_feature(key = 'cycleway:left') %>% osmdata_sf()
    
    q_cycle_left_sf <- q_cycle_left$osm_lines %>% st_as_sf()
    
    q_cycle_right <- opq(bbox = box)%>%
      add_osm_feature(key = 'cycleway:right') %>% osmdata_sf()
    
    q_cycle_right_sf <- q_cycle_right$osm_lines %>% st_as_sf()
    
    ciclo <- q_high_sf %>% st_union(st_transform(q_cycle_sf, 4326)) %>%
    st_union(st_transform(q_cycle_right_sf, 4326)) %>%
      st_union(st_transform(q_cycle_left_sf,4326))
    
    # ciclo <- st_combine(q_high_sf,q_cycle_sf, q_cycle_right, q_cycle_left)
    mapview(ciclo)
   
    
    mapview(q)
    
    download_osm_data
    
  
  
  }
}
  








