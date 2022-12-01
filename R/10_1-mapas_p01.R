
rm(list = ls())


source('./R/fun/setup.R')
width <- 16.5
height <- 16.5

sigla_muni <- 'poa'

tema <- function(base_size) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.key.width=unit(1.0,"line"),
      legend.key.height = unit(0.4,"cm"),
      legend.text=element_text(size=rel(0.5), angle = 0, vjust = 0),
      legend.title=element_text(size=rel(0.6), hjust = -0.5),
      strip.text = element_blank()
      # strip.text = element_text(size=rel(0.9))
      # plot.title = element_text(size = rel(1.5)),
      
      
      
    )
}

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
    
    dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                        sigla_muni, sigla_muni))
    #checar setores com todos os renda_class_pc == n_col
    
    lista_tract <- dados_simulacao %>% group_by(code_tract, renda_class_pc) %>%
      summarise(n = n()) %>% ungroup() %>%
      group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
                                         n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
      filter(n_classes > n_classes_col) %>% pull(code_tract)
    
    data_micro2 <- dados_simulacao %>% filter(code_tract %in% lista_tract) %>% select(1:10, V0606, hex) %>%
      mutate(V0606 = as.factor(V0606))
    
    
    #remocão dos habitantes de cor amarela e indígena
    levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
    data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
    
    data_micro2 <- data_micro2 %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos"))
    #cores
    
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
    
    
    
    #Mapa 1 - Densidade Populacional
    
    pop_counts <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(pop_total = n()) %>% left_join(data_mhex, by = c("hex" = "id_hex")) %>%
      st_as_sf()
    
    # st_write(pop_counts, 'pop_poa.shp')
    
    area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
                                       sigla_muni)) %>% filter(Classes == 24) %>%
      st_make_valid() %>%
      st_union()
    
    sigla_municipio <- sigla_muni
    decisao_muni <- read_excel('../planilha_municipios.xlsx',
                               sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
    
    simplepolys <- st_simplify(area_urbanizada, dTolerance = 300) %>%
      st_make_valid() %>%
      st_transform(decisao_muni$epsg) %>%
      st_buffer(2) %>%
      st_union() 
    
    
    # simplepolys <- rmapshaper::ms_simplify(input = as(area_urbanizada, 'Spatial')) %>%
    #   st_as_sf()
    # mapview(simplepolys)
    # 
    # contorno2 <- st_cont
    assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                                      sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
      mutate(title = "Assentamentos Precários")
    
    
    
    # mapview(area_urbanizada) +  mapview(pop_counts, zcol = "pop_total")
    
    
    # teste <- st_difference(data_contorno,area_urbanizada)
    # mapview(teste)
    
    
    map_pop_density <- ggplot()+
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      geom_sf(data = st_transform(pop_counts, 3857), aes(fill = pop_total), colour = NA, alpha=.6, size = 0)+
      geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
    geom_sf(data = simplepolys %>% st_transform(3857),
            # aes(size = 2),
            aes(color = "#D7ECFF"),
            fill = NA,
            # stroke = 2,
            # size = 2,
            linewidth = 1,
            alpha= 1)  +
      
      # geom_sf(data = assentamentos,
      #         aes(colour = "white"),
      #         fill = NA,
      #         size = 1.3)+
      
      scale_color_identity(labels = c("#D7ECFF" = "",
                                      blue = ""), guide = "legend") +
      labs(color = "Área urbanizada\n(Mapbiomas 2021)")+
      
    scale_fill_gradientn(
      name = "Nº de Habitantes",
      colors = colors_acc ,
      # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      # values = NULL,
      space = "Lab",
      na.value = NA,
      # guide = "colourbar",
      aesthetics = "fill",
      # colors
    ) +
    
    # scale_fill_continuous(palette = "Blues",
    #                   aesthetics = "fill")+
    # viridis::scale_fill_viridis(option = "cividis"
    # 
    # #                             labels = scales::label_percent(accuracy = .1, decimal.mark = ",")
    # # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
    # #                               scale = ifelse(sigla_op== "TT",1e-3,1))
    # #                      limits = c(0,500000))+
    # # , limits = c(0, 0.72)
    # # , breaks = c(0.001, 0.35, 0.7)
    # # , labels = c(0, "35", "70%")
    # ) +
      
      # scale_color_manual(values = 'transparent')+
      # facet_wrap(~ind, ncol = 2)+
      tema()+
      labs(fill = "Densidade Populacional") +
      theme(legend.title = element_text(size=rel(0.8), hjust = 1),
            axis.ticks.length = unit(0,"pt"),
            legend.margin = margin(t = -20)
            # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
            #       # plot.background = element_rect(fill = "#eff0f0",
            #       #                                 colour = NA)
            # legend.background = element_rect(fill = "white",
            #                                  colour = NA)
            #       
      )
    
    # map_pop_density
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    
    ggsave(map_pop_density,
           device = "png",
           filename =  sprintf('../data/map_plots_population/muni_%s/1-densidade_populacional_%s.png',
                               sigla_muni,
                               sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    
    #Mapa 2 - Vazios do censo (substituido por interpretacao no primeiro mapa)
    #Mapa 2 - Mapa de Renda
    
    # data_msetor
    # data_censo_setor <- read_rds(sprintf('../data-raw/censo_2021_info_muni_treated_v2/muni_%s.rds',
    #                                      sigla_muni))
    # data_msetor2 <- data_msetor %>% left_join(data_censo_setor %>%
    #                                            mutate(Cod_setor = as.character(Cod_setor)),
    #                                          by = c("code_tract"="Cod_setor")) %>%
    #   rowwise() %>%
    #   select(-P011) %>%
    #   # mutate(soma = across(.cols = starts_with("P0"), .fns = ~ sum(.x)))
    #   mutate(pop_total = rowSums(across(.cols = starts_with("P0"), .fns = ~ sum(.x, na.rm = T)))) %>%
    #   filter(pop_total > 0)
    # 
    # 
    #   # mutate(pop_total = sum(P001, P002, P003, P004, P005, P006, P007, P008, P009, P010))
    # 
    # map_pop_density <- ggplot()+
    #   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   coord_equal() +
    #   scale_fill_identity()+
    #   # nova escala
    #   new_scale_fill() +
    #   geom_sf(data = st_transform(data_msetor2, 3857), aes(fill = pop_total),
    #           colour = NA,
    #           alpha=.6,
    #           size = 0)+
    #   geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
    #   geom_sf(data = simplepolys %>% st_transform(3857),
    #           # aes(size = 2),
    #           aes(color = "#D7ECFF"),
    #           fill = NA,
    #           # stroke = 2,
    #           # size = 2,
    #           linewidth = 1,
    #           alpha= 1)  +
    #   
    #   # geom_sf(data = assentamentos,
    #   #         aes(colour = "white"),
    #   #         fill = NA,
    #   #         size = 1.3)+
    #   
    #   scale_color_identity(labels = c("#D7ECFF" = "",
    #                                   blue = ""), guide = "legend") +
    #   labs(color = "Área urbanizada\n(Mapbiomas 2021)")+
    #   
    #   scale_fill_gradientn(
    #     name = "Nº de Habitantes",
    #     colors = colors_acc ,
    #     # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    #     # values = NULL,
    #     space = "Lab",
    #     na.value = NA,
    #     # guide = "colourbar",
    #     aesthetics = "fill",
    #     # colors
    #   ) +
    #   
    #   # scale_fill_continuous(palette = "Blues",
    #   #                   aesthetics = "fill")+
    #   # viridis::scale_fill_viridis(option = "cividis"
    #   # 
    #   # #                             labels = scales::label_percent(accuracy = .1, decimal.mark = ",")
    #   # # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
    #   # #                               scale = ifelse(sigla_op== "TT",1e-3,1))
    #   # #                      limits = c(0,500000))+
    #   # # , limits = c(0, 0.72)
    #   # # , breaks = c(0.001, 0.35, 0.7)
    # # # , labels = c(0, "35", "70%")
    # # ) +
    # 
    # # scale_color_manual(values = 'transparent')+
    # # facet_wrap(~ind, ncol = 2)+
    # tema()+
    #   labs(fill = "Densidade Populacional") +
    #   theme(legend.title = element_text(size=rel(0.8), hjust = 1),
    #         axis.ticks.length = unit(0,"pt"),
    #         legend.margin = margin(t = -20)
    #   )
    # 
    # # map_pop_density
    # 
    # suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    # 
    # ggsave(map_pop_density,
    #        device = "png",
    #        filename =  sprintf('../data/map_plots_population/muni_%s/1-densidade_populacional_%s.png',
    #                            sigla_muni,
    #                            sigla_muni),
    #        dpi = 300,
    #        width = width, height = height, units = "cm" )
    
    #Mapa 2 - Renda
    
    renda <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(renda = mean(Rend_pc, na.rm =T)) %>% left_join(data_mhex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% drop_na(h3_resolution)
    
    grid_micro <- read_rds(sprintf("../data/microssimulacao/muni_%s/grid_muni_%s.rds",
                                   sigla_muni, sigla_muni))

    map_renda <- ggplot()+
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      geom_sf(data = st_transform(renda, 3857), aes(fill = renda), colour = NA, alpha=.6, size = 0)+
      geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "#D7ECFF"),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 1,
              alpha= 1)  +
      
      # geom_sf(data = assentamentos,
      #         aes(colour = "white"),
      #         fill = NA,
      #         size = 1.3)+
      
      scale_color_identity(labels = c("#D7ECFF" = "",
                                      blue = ""), guide = "legend") +
      labs(color = "Área urbanizada\n(Mapbiomas 2021)")+
      
      scale_fill_distiller("Renda per capita (R$)",
                           type = "div",
                           palette = "GnBu",
                           direction = 1,
                           breaks = seq(0,20,5),
                           labels = c("0","5", "10", "15", ">20"),
                           limits = c(0,20))

      
      # scale_fill_gradientn(
      #   name = "Renda Media",
      #   colors = colors_acc ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   # colors
      # ) +
      
      # scale_fill_continuous(palette = "Blues",
      #                   aesthetics = "fill")+
      # viridis::scale_fill_viridis(option = "cividis"
      # 
      # #                             labels = scales::label_percent(accuracy = .1, decimal.mark = ",")
      # # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
      # #                               scale = ifelse(sigla_op== "TT",1e-3,1))
      # #                      limits = c(0,500000))+
      # # , limits = c(0, 0.72)
      # # , breaks = c(0.001, 0.35, 0.7)
    # # , labels = c(0, "35", "70%")
    # ) +
    
    # scale_color_manual(values = 'transparent')+
    # facet_wrap(~ind, ncol = 2)+
    tema()+
      labs(fill = "Renda Media") +
      theme(legend.title = element_text(size=rel(0.8), hjust = 1),
            axis.ticks.length = unit(0,"pt"),
            legend.margin = margin(t = -20)
            # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
            #       # plot.background = element_rect(fill = "#eff0f0",
            #       #                                 colour = NA)
            # legend.background = element_rect(fill = "white",
            #                                  colour = NA)
            #       
      )
    
    # map_pop_density
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    
    ggsave(map_pop_density,
           device = "png",
           filename =  sprintf('../data/map_plots_population/muni_%s/1-densidade_populacional_%s.png',
                               sigla_muni,
                               sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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
  








