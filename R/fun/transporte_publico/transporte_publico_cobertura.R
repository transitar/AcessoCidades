#Acesso físico transporte público

#10_3-mapas cicloviários e de transporte público

#geraação de mapas

# rm(list = ls())


source('./R/fun/setup.R')
library(patchwork)

width <- 16.5
height <- 16.5

sigla_muni <- 'poa'

#gráficos de ciclovias

graficos <- function(munis = "all"){
  
  
  
  faz_grafico_e_salva <- function(sigla_muni, width = 16.5, height = 16.5){
    
    message(paste("Rodando",sigla_muni, "\n"))
    
    path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    
    dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
    
    
    # path_muni_data_censo <- sprintf('../data-raw/censo_2021_info_muni_treated_v2/muni_%s.rds',sigla_muni)
    # path_muni_hex <- sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni)
    # path_muni_setor <- sprintf('../data-raw/setores_censitarios/2019/setores_%s_2019.rds',sigla_muni)
    path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
    
    # data_muni <- read_rds(path_muni_data_censo) %>%  mutate(Cod_setor = as.character(Cod_setor))
    # data_msetor <- read_rds(path_muni_setor)
    # data_mhex <- read_rds(path_muni_hex)
    data_contorno <- read_rds(path_contorno)
    
    maptiles <- read_rds(path_maptiles)
    
    
    # shape de bairros
    
    # path_bairros <- sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',sigla_muni, sigla_muni)
    # 
    # bairros <- read_sf(path_bairros, layer = 'bairros')
    # mapview(bairros)
    # 
    # teste_m <- data_complete %>% select(code_tract,P006,P007,P008,P009,P010,Ptot_mulheres) %>% 
    #   gather(key = dado,value = valor, 2:7) %>% 
    #   mutate(valor = as.numeric(valor))
    
    
    #dados de bikes
    sigla_municipio <- sigla_muni
    decisao_muni <- read_excel('../planilha_municipios.xlsx',
                               sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
    
    
    #TP
    
    if (decisao_muni$fonte_dados_tp == "muni_shape"){
      
      dados_peds <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                         sigla_muni, sigla_muni),
                                 layer = "tp_peds")
      
      dados_linhas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                    sigla_muni, sigla_muni),
                            layer = "tp_linhas")
      
      
    }
    # mapview(dados_ciclovias)
    
    dados_peds <- dados_peds %>% st_transform(decisao_muni$epsg)
    dados_linhas <- dados_linhas %>% st_transform(decisao_muni$epsg)
    
    dados_peds_buffer_300 <- dados_peds %>% st_buffer(300) %>% st_union() %>% st_as_sf()
    dados_peds_buffer_500 <- dados_peds %>% st_buffer(500) %>% st_union() %>% st_as_sf()
    
    area_natend300 <- data_contorno %>% st_transform(decisao_muni$epsg) %>%
      st_difference(dados_peds_buffer_300)%>%
      st_union() %>% st_as_sf()
    
    area_natend500 <- data_contorno  %>% st_transform(decisao_muni$epsg) %>%
      st_difference(dados_peds_buffer_500)%>%
      st_union() %>% st_as_sf()
    
    
    
    
    # mapview(area_natend300)
    
    #mapa localização
    cores_ciclo <- c('#5766cc', '#33b099', '#d96e0a')
    
    
    
    #mapa localização
    
    map_linhas_tp <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_linhas,3857), alpha = 1, colour = '#d96e0a') +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +

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
            legend.text=element_text("Tipo", size=rel(1)),
            legend.title=element_text(size=rel(1),                                   ),
            plot.title = element_text(hjust = 0, vjust = 4),
            strip.text = element_text(size = 10)
      )
    # width = 16; height = 16
    # map_empregos
    ggsave(map_linhas_tp,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/5-linhas_tp_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
    
    
    # Mapa - Buffer PEDS -------------------------------------------------
    
    # dados_peds_buffer_300

    map_peds300_buffer <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_peds_buffer_300, 3857),fill = '#0f805e',
              color = NA,alpha = .7, size = 0.1) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
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
            legend.text=element_text("Tipo", size=rel(1)),
            legend.title=element_text(size=rel(1),                                   ),
            plot.title = element_text(hjust = 0, vjust = 4),
            strip.text = element_text(size = 10)
      )
    # width = 16; height = 16
    # map_empregos
    ggsave(map_peds300_buffer,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/6-peds_buffer_300m_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
    
    #buffer 500m
    
    map_peds500_buffer <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_peds_buffer_500, 3857),fill = '#0f805e',
              color = NA,alpha = .7, size = 0.1) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
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
            legend.text=element_text("Tipo", size=rel(1)),
            legend.title=element_text(size=rel(1),                                   ),
            plot.title = element_text(hjust = 0, vjust = 4),
            strip.text = element_text(size = 10)
      )
    # width = 16; height = 16
    # map_empregos
    ggsave(map_peds500_buffer,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/7-peds_buffer_500m_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
    #mapa diferença de 300m
    
    map_peds300_diff <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(area_natend300, 3857),fill = 'grey30',
              color = NA,alpha = .7, size = 0.1) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
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
            legend.text=element_text("Tipo", size=rel(1)),
            legend.title=element_text(size=rel(1),                                   ),
            plot.title = element_text(hjust = 0, vjust = 4),
            strip.text = element_text(size = 10)
      )
    # width = 16; height = 16
    # map_empregos
    ggsave(map_peds300_diff,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/8-peds_n_atendido_300m_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
    # área n atendida 500m
    map_peds500_diff <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(area_natend500, 3857),fill = 'grey30',
              color = NA,alpha = .7, size = 0.1) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
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
            legend.text=element_text("Tipo", size=rel(1)),
            legend.title=element_text(size=rel(1),                                   ),
            plot.title = element_text(hjust = 0, vjust = 4),
            strip.text = element_text(size = 10)
      )
    # width = 16; height = 16
    # map_empregos
    ggsave(map_peds500_diff,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/9-peds_n_atendido_500m_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
    
    #intersect com hex
    
    dados_hex_intersect_300 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(dados_peds_buffer_300)
    dados_hex_intersect <- dados_hex_intersect %>%
      mutate(area = st_area(.))
    
    
    
    dados_hex_intersect_500 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(dados_peds_buffer_500)
    dados_hex_intersect <- dados_hex_intersect %>%
      mutate(area = st_area(.))
    
    dados_hex_intersect_n300 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(area_natend300)
    dados_hex_intersect <- dados_hex_intersect %>%
      mutate(area = st_area(.))
    
    # mapview(dados_hex_intersect_n500)
    
    dados_hex_intersect_n500 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(area_natend500)
    dados_hex_intersect <- dados_hex_intersect %>%
      mutate(area = st_area(.))
    
    #bikes compartilhadas
    if (decisao_muni$bike_comp == 1){
    if (decisao_muni$fonte_bikecomp == "muni"){
      
      dados_bikecomp <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                        sigla_muni, sigla_muni),
                                layer = "bike_comp"
      )
    }
    }
    # mapview(dados_bikecomp)
    
    dados_bikecomp <- dados_bikecomp %>% st_transform(decisao_muni$epsg) %>%
      mutate(Tipo = "Bikepoa")
    
    
    #mapa
    cores_bikecomp <- c('#d96e0a')
    
    map_bikecomp <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_bikecomp,3857),aes(color = Tipo), alpha = 1, size = .8) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_manual(name = "Tipo",
                        values = cores_bikecomp
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
            legend.text=element_text("Tipo", size=rel(1)),
            legend.title=element_text(size=rel(1),                                   ),
            plot.title = element_text(hjust = 0, vjust = 4),
            strip.text = element_text(size = 10)
      )
    # width = 16; height = 16
    # map_empregos
    ggsave(map_bikecomp,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/2-bikes_compartilhadas_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    # Paraciclos --------------------------------------------------------------
    
    
    #map paraciclos
    
    
    if (decisao_muni$fonte_paraciclos == "muni_e_osm"){
      
      dados_paraciclos_muni <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                               sigla_muni, sigla_muni),
                                       layer = "paraciclos"
      ) %>% mutate(Tipo = "Público", id=1) %>% st_transform(decisao_muni$epsg) %>%
        select(name = Name, id, Tipo) %>% st_zm(drop = T)
      
      dados_paraciclos_osm <- read_sf(sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                                              sigla_muni, sigla_muni),
                                      layer = "paraciclos") %>% st_transform(decisao_muni$epsg) %>%
        select(name, id = osm_id, Tipo)
      paraciclos <- rbind(dados_paraciclos_muni, dados_paraciclos_osm) 
      
    }
    # mapview(dados_bikecomp)
    
    paraciclos <- paraciclos %>% st_transform(decisao_muni$epsg)
    
    
    #mapa
    cores_paraciclos <- c('#33b099', '#d96e0a')
    
    map_paraciclos <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(paraciclos,3857),aes(color = Tipo), alpha = 1, size = .8) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_manual(name = "Tipo",
                        values = cores_paraciclos
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
            legend.text=element_text("Tipo", size=rel(1)),
            legend.title=element_text(size=rel(1),                                   ),
            plot.title = element_text(hjust = 0, vjust = 4),
            strip.text = element_text(size = 10)
      )
    # width = 16; height = 16
    # map_empregos
    ggsave(map_paraciclos,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/3-paraciclos_%s.png", sigla_muni, sigla_muni),
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