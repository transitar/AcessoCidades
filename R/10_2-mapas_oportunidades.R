#geraação de mapas

# rm(list = ls())


source('./R/fun/setup.R')
library(patchwork)

width <- 16.5
height <- 16.5

sigla_muni <- 'poa'


graficos <- function(munis = "all"){
  
  
  
  faz_grafico_e_salva <- function(sigla_muni, width = 16.5, height = 16.5){
    
    message(paste("Rodando",sigla_muni, "\n"))
    
    path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    
    #path hexagonos com os dados
    
    #dados_hex <- 
    
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
    
    # data_complete <- data_mhex %>% left_join(data_muni, by = c("id_hex"="Cod_setor")) %>% 
      # mutate(area = st_area(.)/10^6) %>%
      # rowwise() %>% 
      # mutate(Ptot_mulheres = sum(P006,P007,P008,P009,P010),
      #        Ptot_homens = sum(P001,P002,P003,P004,P005))
      # 
    
    # shape de bairros
    
    # path_bairros <- sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',sigla_muni, sigla_muni)
    # 
    # bairros <- read_sf(path_bairros, layer = 'bairros')
    # mapview(bairros)
    # 
    # teste_m <- data_complete %>% select(code_tract,P006,P007,P008,P009,P010,Ptot_mulheres) %>% 
    #   gather(key = dado,value = valor, 2:7) %>% 
    #   mutate(valor = as.numeric(valor))
    

    dados_acc <- left_join(hex_empty, acess_cma, by = c("id_hex"="origin")) %>% st_as_sf() %>%
      filter(mode == "transit")
    mapview(dados_acc, zcol = "CMATT60")
    
    colors_purple <- c("#F1F2FE","#9FA4F9","#767DCE","#21367D","#1A295B")
    
    colors_orange <- c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
    
    colors_green <- c("#dbede7", "#79b9a6", "#56a68e", "#0f805e", "#0b5e45", "#094d39", "#073b2c")
    
    # colors_blue <- c("#eff1f6", "#8d97bd", "#6b79a9", "#344889", "#21367d", "#1d306e", "#19295f")
    
    # colors_blue <- c("#DAE5EF", "#b5cadf", "#6a94be", "#3d73aa", "#25619f", "#054a91", "#05376d")
    colors_blue <- c("#ecfaff", "#ade7ff", "#73d6ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#0989c0", "#066891")
    
    colors_acc <- c("#090e20","#111B3F", "#21367D",
                    "#37366E", "#4C355F", "#773340",
                    "#A23222",
                    "#BB4115", "#D34F07", "#E4753A",
                    "#EB9432", "#F5C226", "#FAD920",
                    "#FDE63A", "#FFF354")
    
    
    map_empregos <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_acc,3857),aes(fill = CMATT60),color = NA, alpha = .8) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      

      
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Empregos",
        colors =colors_acc ,
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
    # width = 16; height = 16
    # map_empregos
    ggsave(map_empregos,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/1-empregos_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    
    # Equipamentos de lazer:
    
    map_lazer <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_hex,3857),aes(fill = lazer_tot),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Equipamentos de Lazer",
        colors =colors_green ,
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
            legend.key.width=unit(1,"line"),
            legend.key.height = unit(.5,"cm"),
            legend.text=element_text(size=rel(1)),
            legend.title=element_text(size=rel(1), vjust = 1),
            plot.title = element_text(hjust = 0, vjust = 0),
            strip.text = element_text(size = 10)
      )
    
    # map_lazer
    ggsave(map_lazer,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/2-lazer_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    width = 16; height = 16
    

# Saúde -------------------------------------------------------------------

    
    #saude
    
    #saude total
    
    map_saude_total <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_hex,3857),aes(fill = S001),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Equipamentos de Saúde Totais")+
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Equipamentos de Saúde",
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
            legend.key.width=unit(1,"line"),
            legend.key.height = unit(.5,"cm"),
            legend.text= element_text(size=rel(1), vjust = 0),
            legend.title= element_text(size=rel(1), vjust = 1),
            plot.title = element_text(hjust = .2, vjust = -5),
            strip.text = element_text(size = 10)
      )

    # map_saude_total
    
    #saude nivel 1
    
    map_saude_nv1 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_hex,3857),aes(fill = S002),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Equipamentos de Saúde Nível 1")+
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Equipamentos de Saúde",
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
            legend.key.width=unit(1,"line"),
            legend.key.height = unit(.5,"cm"),
            legend.text= element_text(size=rel(1), vjust = 0),
            legend.title= element_text(size=rel(1), vjust = 1),
            plot.title = element_text(hjust = .2, vjust = -5),
            strip.text = element_text(size = 10)
      )
    
    # map_saude_nv1
    
    #saude nv2
    
    map_saude_nv2 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_hex,3857),aes(fill = S003),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Equipamentos de Saúde Nível 2")+
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Equipamentos de Saúde",
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
            legend.key.width=unit(1,"line"),
            legend.key.height = unit(.5,"cm"),
            legend.text= element_text(size=rel(1), vjust = 0),
            legend.title= element_text(size=rel(1), vjust = 1),
            plot.title = element_text(hjust = .2, vjust = -5),
            strip.text = element_text(size = 10)
      )
    
    # map_saude_nv2
    
    #saude nv3
    
    map_saude_nv3 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_hex,3857),aes(fill = S004),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Equipamentos de Saúde Nível 3")+
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Equipamentos de Saúde",
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
            legend.key.width=unit(1,"line"),
            legend.key.height = unit(.5,"cm"),
            legend.text= element_text(size=rel(1), vjust = 0),
            legend.title= element_text(size=rel(1), vjust = 1),
            plot.title = element_text(hjust = .2, vjust = -5),
            strip.text = element_text(size = 10)
      )
    
    map_saude_nv3
    
    map_saude <- map_saude_total + map_saude_nv1 + map_saude_nv2 + map_saude_nv3
    
    # map_saude
    ggsave(map_saude,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/3-saude_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    width = 16; height = 16
    
    

# Matriculas -----------------------------------------------------------------

    
    
    #matriculas
    
    #matriculas total
    
    map_matriculas_total <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_hex,3857),aes(fill = M001),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Matrículas Totais")+
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Matrículas",
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
            legend.key.width=unit(1,"line"),
            legend.key.height = unit(.5,"cm"),
            legend.text= element_text(size=rel(1), angle = 90 ,vjust = 0.5),
            legend.title= element_text(size=rel(1), vjust = 1),
            plot.title = element_text(hjust = 0.1, vjust = -5),
            strip.text = element_text(size = 10)
      )
    
    # map_matriculas_total
    
    #saude nivel 1
    
    map_matriculas_nv1 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_hex,3857),aes(fill = M002),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Matrículas Ensino Básico")+
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Matriculas",
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
            legend.key.width=unit(1,"line"),
            legend.key.height = unit(.5,"cm"),
            legend.text= element_text(size=rel(1), angle = 90 ,vjust = 0.5),
            legend.title= element_text(size=rel(1), vjust = 1),
            plot.title = element_text(hjust = .1, vjust = -5),
            strip.text = element_text(size = 10)
      )
    
    # map_matriculas_nv1
    
    #smatriculas
    
    map_matriculas_nv2 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_hex,3857),aes(fill = M003),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Matrículas Ensino Fundamental")+
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Matriculas",
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
            legend.key.width=unit(1,"line"),
            legend.key.height = unit(.5,"cm"),
            legend.text= element_text(size=rel(1), angle = 90 ,vjust = 0.5),
            legend.title= element_text(size=rel(1), vjust = 1),
            plot.title = element_text(hjust = .18, vjust = -5),
            strip.text = element_text(size = 10)
      )
    
    # map_matriculas_nv2
    
    #educacao nv3
    
    map_matriculas_nv3 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_hex,3857),aes(fill = M004),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Matrículas Ensino Médio")+
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_gradientn(
        name = "Nº de Matrículas",
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
            legend.key.width=unit(1,"line"),
            legend.key.height = unit(.5,"cm"),
            legend.text= element_text(size=rel(1), angle = 90 ,vjust = 0.5),
            legend.title= element_text(size=rel(1), vjust = 1),
            plot.title = element_text(hjust = .15, vjust = -5),
            strip.text = element_text(size = 10)
      )
    
    # map_educacao_nv3
    
    map_matriculas <- map_matriculas_total + map_matriculas_nv1 + map_matriculas_nv2 + map_matriculas_nv3
    
    # map_saude
    ggsave(map_matriculas,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/4-matriculas_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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