#10_3 mapas de acessibilidade

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
    
    # bus <- fread('../r5r/routing/2022/muni_poa/ttmatrix_transit_2022_poa_r5_pico.csv')
    # bike <- fread('../r5r/routing/2022/muni_poa/ttmatrix_bike_2022_poa_r5_pico.csv')
    # walk <- fread('../r5r/routing/2022/muni_poa/ttmatrix_walk_2022_poa_r5_pico.csv')
    # 
    # dados_gravar <- rbind(bus, bike, walk)
    # write_csv(dados_gravar, '../r5r/routing/2022/muni_poa/ttmatrix_all_2022_poa_r5.csv')
    
    
    data_acess <- read_rds(sprintf('../r5r/accessibility/muni_%s/acc_%s.rds',
                                   sigla_muni, sigla_muni))
    # data_acess <- acess_cma
    
    
    dados_acc <- left_join(dados_hex, data_acess, by = c("id_hex"="origin")) %>% st_as_sf()
    
    dados_acc_tp <- dados_acc %>%
      filter(mode == "transit")
    
    # mapview(dados_acc, zcol = "CMATT30")
    # mapview(dados_acc_tp, zcol = "CMATT30")
    
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
    modo <- 'transit'
    oportunidade <- 'Empregos'
    tempo <- 60
    
    plot_map_acc <- function(modo, oportunidade, tempo){
    
    dados_acc_map <- dados_acc %>% filter(mode == modo)
    }
    
    theme_for_CMA <- function(base_size) {
      
      # theme_void(base_family="Roboto Condensed") %+replace%
      theme_void() %+replace%
        
        theme(
          legend.position = "bottom",
          plot.margin=unit(c(2,0,0,0),"mm"),
          legend.key.width=unit(2,"line"),
          legend.key.height = unit(0.4,"cm"),
          legend.text=element_text(size=rel(0.6)),
          legend.title=element_text(size=rel(0.9)),
          # plot.title = element_text(hjust = 0, vjust = 4),
          
          
          
        )
    }
    


# CMA-Trabalho ------------------------------------------------------------

    library("future")
    plan(multisession)
    # sigla_munii <- 'poa'
    options(scipen = 100000000)
    # cols <- 2
    # width <- 14
    # height <- 10
    # mode1 <- "transit"
    # mapview(data_acess, zcol = "CMATT60")
    
    fazer_mapa_1530_t <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMATT15, CMATT30) %>%
        gather(ind, valor, CMATT15:CMATT30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMATT15", "CMATT30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
      suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
      
      
      ggsave(plot3, 
             file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_TT_1530.png",
                           sigla_munii, sigla_munii), 
             dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_TT_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_TT_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp1 %<-% fazer_mapa_1530_t('poa', mode1 = 'bike')
    temp1 %<-% fazer_mapa_1530_t('poa', mode1 = 'transit')
    temp1 %<-% fazer_mapa_1530_t('poa', mode1 = 'walk')
    

    fazer_mapa_4560_t <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMATT45, CMATT60) %>%
        gather(ind, valor, CMATT45:CMATT60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMATT45", "CMATT60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_TT_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_TT_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_TT_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
    }
    fazer_mapa_4560_t('poa', mode1 = 'bike')
    fazer_mapa_4560_t('poa', mode1 = 'transit')
    fazer_mapa_4560_t('poa', mode1 = 'walk')
    

# Bike_Saúde_Nível-1 ------------------------------------------------------

    

    #15 e 30 minutos
    
    fazer_mapa_1530_s1 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMASB15, CMASB30) %>%
        gather(ind, valor, CMASB15:CMASB30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMASB15", "CMASB30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      

      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_S1_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_S1_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_S1_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
    }
    fazer_mapa_1530_s1('poa', mode1 = 'bike')
    fazer_mapa_1530_s1('poa', mode1 = 'transit')
    fazer_mapa_1530_s1('poa', mode1 = 'walk')
    
    #45 e 60 minutos
    
    fazer_mapa_4560_s2 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMASB45, CMASB60) %>%
        gather(ind, valor, CMASB45:CMASB60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMASB45", "CMASB60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_S1_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_S1_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_S1_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
     
    }
    fazer_mapa_4560_s2('poa', mode1 = 'bike')
    fazer_mapa_4560_s2('poa', mode1 = 'transit')
    fazer_mapa_4560_s2('poa', mode1 = 'walk')

# Saude_Nivel-2 ------------------------------------------------------
    #15 e 30 minutos
    
    fazer_mapa_1530_s2 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMASM15, CMASM30) %>%
        gather(ind, valor, CMASM15:CMASM30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMASM15", "CMASM30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_S2_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_S2_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_S2_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
    
    }
    fazer_mapa_1530_s2('poa', mode1 = 'bike')
    fazer_mapa_1530_s2('poa', mode1 = 'transit')
    fazer_mapa_1530_s2('poa', mode1 = 'walk')
    
    #45 e 60 minutos
    
    fazer_mapa_4560_s2 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMASM45, CMASM60) %>%
        gather(ind, valor, CMASM45:CMASM60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMASM45", "CMASM60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_S2_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_S2_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_S2_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    
    }
    fazer_mapa_4560_s2('poa', mode1 = 'bike')
    fazer_mapa_4560_s2('poa', mode1 = 'transit')
    fazer_mapa_4560_s2('poa', mode1 = 'walk')
    

# Saude_Nivel-3 ------------------------------------------------------

    #15 e 30 minutos
    
    fazer_mapa_1530_s3 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMASA15, CMASA30) %>%
        gather(ind, valor, CMASA15:CMASA30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMASA15", "CMASA30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_S3_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_S3_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_S3_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
  
    }
    fazer_mapa_1530_s3('poa', mode1 = 'bike')
    fazer_mapa_1530_s3('poa', mode1 = 'transit')
    fazer_mapa_1530_s3('poa', mode1 = 'walk')
    
    #45 e 60 minutos
    
    fazer_mapa_4560_s3 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMASA45, CMASA60) %>%
        gather(ind, valor, CMASA45:CMASA60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMASA45", "CMASA60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_S3_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_S3_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_S3_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
    
    }
    fazer_mapa_4560_s3('poa', mode1 = 'bike')
    fazer_mapa_4560_s3('poa', mode1 = 'transit')
    fazer_mapa_4560_s3('poa', mode1 = 'walk')


# Saude_total -------------------------------------------------------------

    fazer_mapa_1530_st <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAST15, CMAST30) %>%
        gather(ind, valor, CMAST15:CMAST30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAST15", "CMAST30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_ST_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_ST_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_ST_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
    }
    fazer_mapa_1530_st('poa', mode1 = 'bike')
    fazer_mapa_1530_st('poa', mode1 = 'transit')
    fazer_mapa_1530_st('poa', mode1 = 'walk')
    
    #45 e 60 minutos
    
    fazer_mapa_4560_st <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAST45, CMAST60) %>%
        gather(ind, valor, CMAST45:CMAST60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAST45", "CMAST60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_ST_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_ST_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_ST_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_4560_st('poa', mode1 = 'bike')
    fazer_mapa_4560_st('poa', mode1 = 'transit')
    fazer_mapa_4560_st('poa', mode1 = 'walk')


# Matriculas_Nivel-1 -------------------------------------------------

    #15 e 30 minutos
    
    fazer_mapa_1530_m1 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAMI15, CMAMI30) %>%
        gather(ind, valor, CMAMI15:CMAMI30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAMI15", "CMAMI30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_M1_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_M1_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_M1_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
     
    }
    fazer_mapa_1530_m1('poa', mode1 = "bike")
    fazer_mapa_1530_m1('poa', mode1 = "transit")
    fazer_mapa_1530_m1('poa', mode1 = "walk")
    
    #45 e 60 minutos
    
    fazer_mapa_4560_m1 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAMI45, CMAMI60) %>%
        gather(ind, valor, CMAMI45:CMAMI60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAMI45", "CMAMI60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_M1_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_M1_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_M1_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_4560_m1('poa', mode1 = "bike")
    fazer_mapa_4560_m1('poa', mode1 = "transit")
    fazer_mapa_4560_m1('poa', mode1 = "walk")
    

# Bike_Matriculas_Nível-2 -------------------------------------------------

    fazer_mapa_1530_m2 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAMF15, CMAMF30) %>%
        gather(ind, valor, CMAMF15:CMAMF30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAMF15", "CMAMF30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_M2_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_M2_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_M2_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_1530_m2('poa', mode1 = "bike")
    fazer_mapa_1530_m2('poa', mode1 = "transit")
    fazer_mapa_1530_m2('poa', mode1 = "walk")
    
    #45 e 60 minutos
    
    fazer_mapa_4560_m2 <- function(sigla_munii, mode1,cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAMF45, CMAMF60) %>%
        gather(ind, valor, CMAMF45:CMAMF60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAMF45", "CMAMF60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_M2_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_M2_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_M2_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    
    }
    fazer_mapa_4560_m2('poa', mode1 = 'bike')
    fazer_mapa_4560_m2('poa', mode1 = 'transit')
    fazer_mapa_4560_m2('poa', mode1 = 'walk')
    
    

# Bikes_matriculas_nivel_3 ------------------------------------------------

    fazer_mapa_1530_m3 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAMM15, CMAMM30) %>%
        gather(ind, valor, CMAMM15:CMAMM30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAMM15", "CMAMM30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_M3_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_M3_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_M3_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
     
    }
    fazer_mapa_1530_m3('poa', mode1 = 'bike')
    fazer_mapa_1530_m3('poa', mode1 = 'transit')
    fazer_mapa_1530_m3('poa', mode1 = 'walk')
    
    #45 e 60 minutos
    
    fazer_mapa_4560_m3 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAMM45, CMAMM60) %>%
        gather(ind, valor, CMAMM45:CMAMM60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAMM45", "CMAMM60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_M3_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_M3_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_M3_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_4560_m3('poa', mode1 = 'bike')
    fazer_mapa_4560_m3('poa', mode1 = 'transit')
    fazer_mapa_4560_m3('poa', mode1 = 'walk')


# Matriculas_totais ------------------------------------------------
    
    #15 e 30 minutos
    fazer_mapa_1530_m4 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAMT15, CMAMT30) %>%
        gather(ind, valor, CMAMT15:CMAMT30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAMT15", "CMAMT30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_MT_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_MT_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_MT_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_1530_m4('poa', mode1 = "bike")
    fazer_mapa_1530_m4('poa', mode1 = "transit")
    fazer_mapa_1530_m4('poa', mode1 = "walk")
    
    #45 e 60 minutos
    
    fazer_mapa_4560_m4 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAMT45, CMAMT60) %>%
        gather(ind, valor, CMAMT45:CMAMT60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAMT45", "CMAMT60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_MT_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_MT_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_MT_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }

      
      
  
    }
    fazer_mapa_4560_m4('poa', mode1 = "bike")
    fazer_mapa_4560_m4('poa', mode1 = "transit")
    fazer_mapa_4560_m4('poa', mode1 = "walk")



# Lazer_total -------------------------------------------------------------

    #15 e 30 minutos
    fazer_mapa_1530_lz <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMALZ15, CMALZ30) %>%
        gather(ind, valor, CMALZ15:CMALZ30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMALZ15", "CMALZ30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_LZ_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_LZ_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_LZ_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_1530_lz('poa', mode1 = "bike")
    fazer_mapa_1530_lz('poa', mode1 = "transit")
    fazer_mapa_1530_lz('poa', mode1 = "walk")
    
    #45 e 60 minutos
    
    fazer_mapa_4560_lz <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMALZ45, CMALZ60) %>%
        gather(ind, valor, CMALZ45:CMALZ60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMALZ45", "CMALZ60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_LZ_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_LZ_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_LZ_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
      
      
    }
    fazer_mapa_4560_lz('poa', mode1 = "bike")
    fazer_mapa_4560_lz('poa', mode1 = "transit")
    fazer_mapa_4560_lz('poa', mode1 = "walk")
    

# Escolas_Nivel-1 ---------------------------------------------------------

    #15 e 30 minutos
    fazer_mapa_1530_e1 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAEI15, CMAEI30) %>%
        gather(ind, valor, CMAEI15:CMAEI30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAEI15", "CMAEI30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_E1_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_E1_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_E1_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_1530_e1('poa', mode1 = "bike")
    fazer_mapa_1530_e1('poa', mode1 = "transit")
    fazer_mapa_1530_e1('poa', mode1 = "walk")
    
    #45 e 60 minutos
    
    fazer_mapa_4560_e1 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAEI45, CMAEI60) %>%
        gather(ind, valor, CMAEI45:CMAEI60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAEI45", "CMAEI60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_E1_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_E1_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_E1_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
      
      
    }
    fazer_mapa_4560_e1('poa', mode1 = "bike")
    fazer_mapa_4560_e1('poa', mode1 = "transit")
    fazer_mapa_4560_e1('poa', mode1 = "walk")

    

# Escolas_Nivel-2 ---------------------------------------------------------

    #15 e 30 minutos
    fazer_mapa_1530_e2<- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAEF15, CMAEF30) %>%
        gather(ind, valor, CMAEF15:CMAEF30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAEF15", "CMAEF30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_E2_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_E2_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_E2_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_1530_e2('poa', mode1 = "bike")
    fazer_mapa_1530_e2('poa', mode1 = "transit")
    fazer_mapa_1530_e2('poa', mode1 = "walk")
    
    #45 e 60 minutos
    
    fazer_mapa_4560_e2 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAEF45, CMAEF60) %>%
        gather(ind, valor, CMAEF45:CMAEF60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAEF45", "CMAEF60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_E2_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_E2_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_E2_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
      
      
    }
    fazer_mapa_4560_e2('poa', mode1 = "bike")
    fazer_mapa_4560_e2('poa', mode1 = "transit")
    fazer_mapa_4560_e2('poa', mode1 = "walk")    



# Escolas_nivel-3 ---------------------------------------------------------

    #15 e 30 minutos
    fazer_mapa_1530_e3 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAEM15, CMAEM30) %>%
        gather(ind, valor, CMAEM15:CMAEM30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAEM15", "CMAEM30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_E3_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_E3_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_E3_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_1530_e3('poa', mode1 = "bike")
    fazer_mapa_1530_e3('poa', mode1 = "transit")
    fazer_mapa_1530_e3('poa', mode1 = "walk")
    
    #45 e 60 minutos
    
    fazer_mapa_4560_e3 <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAEM45, CMAEM60) %>%
        gather(ind, valor, CMAEM45:CMAEM60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAEM45", "CMAEM60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_E3_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_E3_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_E3_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
      
      
    }
    fazer_mapa_4560_e3('poa', mode1 = "bike")
    fazer_mapa_4560_e3('poa', mode1 = "transit")
    fazer_mapa_4560_e3('poa', mode1 = "walk")    



# Escolas_total -----------------------------------------------------------

    #15 e 30 minutos
    fazer_mapa_1530_et <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAET15, CMAET30) %>%
        gather(ind, valor, CMAET15:CMAET30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAET15", "CMAET30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_ET_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_ET_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_ET_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_1530_et('poa', mode1 = "bike")
    fazer_mapa_1530_et('poa', mode1 = "transit")
    fazer_mapa_1530_et('poa', mode1 = "walk")
    
    #45 e 60 minutos
    
    fazer_mapa_4560_et <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAET45, CMAET60) %>%
        gather(ind, valor, CMAET45:CMAET60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAET45", "CMAET60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), color = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Oportunidades\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_ET_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_ET_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_ET_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
      
      
    }
    fazer_mapa_4560_et('poa', mode1 = "bike")
    fazer_mapa_4560_et('poa', mode1 = "transit")
    fazer_mapa_4560_et('poa', mode1 = "walk")
    

# Paraciclos --------------------------------------------------------------

    #15 e 30 minutos
    fazer_mapa_1530_pr <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAPR15, CMAPR30) %>%
        gather(ind, valor, CMAPR15:CMAPR30)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAPR15", "CMAPR30"), 
                            labels = c("15 Minutos", "30 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Paraciclos\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_PR_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_PR_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_PR_1530.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    fazer_mapa_1530_pr('poa', mode1 = "bike")
    fazer_mapa_1530_pr('poa', mode1 = "transit")
    fazer_mapa_1530_pr('poa', mode1 = "walk")
    
    #45 e 60 minutos
    
    fazer_mapa_4560_pr <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, CMAPR45, CMAPR60) %>%
        gather(ind, valor, CMAPR45:CMAPR60)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(ind = factor(ind, 
                            levels = c("CMAPR45", "CMAPR60"), 
                            labels = c("45 Minutos", "60 Minutos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.7)+
        viridis::scale_fill_viridis(option = "B"
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        facet_wrap(~ind, ncol = 2)+
        theme_for_CMA()+
        labs(fill = "Paraciclos\n acessíveis",
             title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_PR_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_PR_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_PR_4560.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
      
      
    }
    fazer_mapa_4560_pr('poa', mode1 = "bike")
    fazer_mapa_4560_pr('poa', mode1 = "transit")
    fazer_mapa_4560_pr('poa', mode1 = "walk")


# Bikes Compartilhadas ----------------------------------------------------

    if (decisao_muni$bike_comp == 1) {
      #15 e 30 minutos
      fazer_mapa_1530_bk <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
        
        # abrir acess
        acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
          filter(mode == mode1) %>%
          select(sigla_muni, CMABK15, CMABK30) %>%
          gather(ind, valor, CMABK15:CMABK30)
        
        # abrir tiles
        path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
        
        map_tiles <- read_rds(path_maptiles)
        
        # ajustar levels
        acess <- acess %>%
          mutate(ind = factor(ind, 
                              levels = c("CMABK15", "CMABK30"), 
                              labels = c("15 Minutos", "30 Minutos")))
        
        
        # fazer plots
        plot3 <- ggplot()+
          geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
          coord_equal() +
          scale_fill_identity()+
          # nova escala
          new_scale_fill() +
          geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.7)+
          viridis::scale_fill_viridis(option = "B"
                                      # , limits = c(0, 0.72)
                                      # , breaks = c(0.001, 0.35, 0.7)
                                      # , labels = c(0, "35", "70%")
          ) +
          facet_wrap(~ind, ncol = 2)+
          theme_for_CMA()+
          labs(fill = "Estações de\n Bicletas Compartilhadas \n acessíveis",
               title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
          theme(plot.title = element_text(hjust = 0.5))
        
        # create dir
        if (mode1 == "bike"){
          suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
          
          
          ggsave(plot3, 
                 file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_BK_1530.png",
                               sigla_munii, sigla_munii), 
                 dpi = 300, width = width, height = height, units = "cm")
        } else if (mode1 == "transit") {
          
          suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
          
          
          ggsave(plot3, 
                 file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_BK_1530.png",
                               sigla_munii, sigla_munii), 
                 dpi = 300, width = width, height = height, units = "cm")
          
        } else if (mode1 == "walk") {
          suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
          
          
          ggsave(plot3, 
                 file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_BK_1530.png",
                               sigla_munii, sigla_munii), 
                 dpi = 300, width = width, height = height, units = "cm")
          
        }
        
        
      }
      temp1 %<-% fazer_mapa_1530_bk('poa', mode1 = "bike")
      temp2 %<-% fazer_mapa_1530_bk('poa', mode1 = "transit")
      temp3 %<-% fazer_mapa_1530_bk('poa', mode1 = "walk")
      rm(list = ls(pattern = "temp"))
      #45 e 60 minutos
      
      fazer_mapa_4560_bk <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
        
        # abrir acess
        acess <- dados_acc %>% filter(sigla_muni == sigla_munii) %>%
          filter(mode == mode1) %>%
          select(sigla_muni, CMABK45, CMABK60) %>%
          gather(ind, valor, CMABK45:CMABK60)
        
        # abrir tiles
        path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
        
        map_tiles <- read_rds(path_maptiles)
        
        # ajustar levels
        acess <- acess %>%
          mutate(ind = factor(ind, 
                              levels = c("CMABK45", "CMABK60"), 
                              labels = c("45 Minutos", "60 Minutos")))
        
        
        # fazer plots
        plot3 <- ggplot()+
          geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
          coord_equal() +
          scale_fill_identity()+
          # nova escala
          new_scale_fill() +
          geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.7)+
          viridis::scale_fill_viridis(option = "B"
                                      # , limits = c(0, 0.72)
                                      # , breaks = c(0.001, 0.35, 0.7)
                                      # , labels = c(0, "35", "70%")
          ) +
          facet_wrap(~ind, ncol = 2)+
          theme_for_CMA()+
          labs(fill = "Estações de\n Bicletas Compartilhadas \n acessíveis",
               title = subset(munis_list$munis_df, abrev_muni == sigla_munii)$name_muni) +
          theme(plot.title = element_text(hjust = 0.5))
        
        # create dir
        if (mode1 == "bike"){
          suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/', sigla_munii)))
          
          
          ggsave(plot3, 
                 file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/%s_CMA_BK_4560.png",
                               sigla_munii, sigla_munii), 
                 dpi = 300, width = width, height = height, units = "cm")
        } else if (mode1 == "transit") {
          
          suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/', sigla_munii)))
          
          
          ggsave(plot3, 
                 file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/%s_CMA_BK_4560.png",
                               sigla_munii, sigla_munii), 
                 dpi = 300, width = width, height = height, units = "cm")
          
        } else if (mode1 == "walk") {
          suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/', sigla_munii)))
          
          
          ggsave(plot3, 
                 file= sprintf("../data/map_plots_acc/muni_%s/caminhada/%s_CMA_BK_4560.png",
                               sigla_munii, sigla_munii), 
                 dpi = 300, width = width, height = height, units = "cm")
          
        }
        
        
        
        
      }
      temp1 %<-% fazer_mapa_4560_bk('poa', mode1 = "bike")
      temp2 %<-% fazer_mapa_4560_bk('poa', mode1 = "transit")
      temp3 %<-% fazer_mapa_4560_bk('poa', mode1 = "walk")
      rm(list = ls(pattern = "temp"))
    }


# Modelo com escala personalizada -----------------------------------------

# 
#     map_acc <- ggplot() +
#       geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
#       coord_equal() +
#       scale_fill_identity()+
#       # nova escala
#       new_scale_fill() +
#       # theme_map() +
#       geom_sf(data = st_transform(dados_acc_tp,3857),aes(fill = CMATT60),color = NA, size = 0, alpha = .8) +
#       
#       # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
#       
#       geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
#       
#       
#       
#       # facet_wrap(~dado, labeller = labeller_grupos) +
#       scale_fill_gradientn(
#         name = "Nº de Empregos",
#         colors =colors_acc ,
#         # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
#         # values = NULL,
#         space = "Lab",
#         na.value = NA,
#         # guide = "colourbar",
#         aesthetics = "fill",
#         # colors
#       ) +
#       # tema_populacao()
#       theme(legend.position = "bottom",
#             strip.text.x = element_text(size=rel(1.2)),
#             strip.background = element_rect(
#               color = NA,
#               fill = "#eff0f0"
#             ),
#             panel.background = element_rect(fill = NA, colour = NA),
#             axis.text = element_blank(),
#             axis.title = element_blank(),
#             axis.ticks = element_blank(), 
#             panel.grid = element_blank(),
#             plot.margin=unit(c(2,0,0,0),"mm"),
#             legend.key.width=unit(2,"line"),
#             legend.key.height = unit(.5,"cm"),
#             legend.text=element_text(size=rel(1), angle = 90, vjust = 0.5),
#             legend.title=element_text(size=rel(1), vjust = 1),
#             plot.title = element_text(hjust = 0, vjust = 4),
#             strip.text = element_text(size = 10)
#       )
#     # width = 16; height = 16
#     # map_empregos
#     ggsave(map_acc,
#            device = "png",
#            filename =  sprintf("../data/map_plots_acc/muni_%s/1_acc_empregos_60min_%s.png", sigla_muni, sigla_muni),
#            dpi = 300,
#            width = width, height = height, units = "cm")
#     
    
    
    
 
    
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