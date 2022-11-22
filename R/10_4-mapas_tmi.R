#10_4-mapas_tmi

source('./R/fun/setup.R')
library(patchwork)

width <- 16.5
height <- 16.5

sigla_muni <- 'poa'

#não rodar tudo de uma vez/ consome muita memória

graficos_tmi <- function(munis = "all"){
  
  
  
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
    
    
    data_acess_tmi <- read_rds(sprintf('../r5r/accessibility/muni_%s/tmi/acc_tmi_%s.rds',
                                   sigla_muni, sigla_muni))
    
    
    dados_acc <- left_join(dados_hex, data_acess_tmi, by = c("id_hex"="origin")) %>% st_as_sf()
    DT <- dados_acc
    
    #inserir drop na em cada gráfico
    dados_acc_maps <- dados_acc %>% mutate_if(is.numeric, list(~na_if(., Inf)))
    
    dados_acc_tp <- dados_acc_maps %>%
      filter(mode == "transit")
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
    
    # plot_map_acc <- function(modo, oportunidade, tempo){
    #   
    #   dados_acc_map <- dados_acc %>% filter(mode == modo)
    # }
    
    theme_for_TMI <- function(base_size) {
      
      # theme_void(base_family="Roboto Condensed") %+replace%
      theme_void() %+replace%
        
        theme(
          legend.position = "bottom",
          plot.margin=unit(c(2,0,0,0),"mm"),
          legend.key.width=unit(1,"line"),
          legend.key.height = unit(0.4,"cm"),
          legend.text=element_text(size=rel(0.4)),
          legend.title=element_text(size=rel(0.6)),
          plot.title = element_text(hjust = 0, vjust = 4, size = rel(0.6)),
          strip.text = element_text(size = rel(0.6))
          # legend.key.width=unit(0.5,"cm")
          
        )
    }
    
    sigla_munii <- 'poa'
    options(scipen = 100000000)
    cols <- 2
    width <- 14
    height <- 16
    mode1 <- "transit"
    
    library("future")
    plan(multisession)
    
    # TMI-Saúde ------------------------------------------------------------
    
    fazer_mapa_180_s <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIST, TMISB, TMISM, TMISA) %>%
        gather(ind, valor, TMIST:TMISA)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >180, 180, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIST", "TMISB", "TMISM", "TMISA"), 
                            labels = c("Eq. de saúde totais", "Eq. de saúde nível 1",
                                       "Eq. de saúde nível 2", "Eq. de saúde nível 3")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 2)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/saude/5_%s_tmi_saude_max180.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/saude/5_%s_tmi_saude_max180.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/saude/5_%s_tmi_saude_max180.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp1 %<-% fazer_mapa_180_s('poa', mode1 = 'bike')
    temp2 %<-% fazer_mapa_180_s('poa', mode1 = 'transit')
    temp3 %<-% fazer_mapa_180_s('poa', mode1 = 'walk')
    rm(list = ls(pattern = "temp"))
    
    fazer_mapa_120_s <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIST, TMISB, TMISM, TMISA) %>%
        gather(ind, valor, TMIST:TMISA)
      

      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor <- ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >120, 120, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIST", "TMISB", "TMISM", "TMISA"), 
                            labels = c("Eq. de saúde totais", "Eq. de saúde nível 1",
                                       "Eq. de saúde nível 2", "Eq. de saúde nível 3")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 2)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/saude/4_%s_tmi_saude_max120.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/saude/4_%s_tmi_saude_max120.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/saude/4_%s_tmi_saude_max120.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp4 %<-% fazer_mapa_120_s('poa', mode1 = 'bike')
    temp5 %<-% fazer_mapa_120_s('poa', mode1 = 'transit')
    temp6 %<-% fazer_mapa_120_s('poa', mode1 = 'walk')
    rm(list = ls(pattern = "temp"))
    
    fazer_mapa_90_s <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIST, TMISB, TMISM, TMISA) %>%
        gather(ind, valor, TMIST:TMISA)
      

        
        # abrir tiles
        path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor <- ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >90, 90, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIST", "TMISB", "TMISM", "TMISA"), 
                            labels = c("Eq. de saúde totais", "Eq. de saúde nível 1",
                                       "Eq. de saúde nível 2", "Eq. de saúde nível 3")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 2)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/saude/3_%s_tmi_saude_max90.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/saude/3_%s_tmi_saude_max90.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/saude/3_%s_tmi_saude_max90.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp7 %<-% fazer_mapa_90_s('poa', mode1 = 'bike')
    temp8 %<-% fazer_mapa_90_s('poa', mode1 = 'transit')
    temp9 %<-% fazer_mapa_90_s('poa', mode1 = 'walk')
    
    rm(list = ls(pattern = "temp"))
    
    fazer_mapa_60_s <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIST, TMISB, TMISM, TMISA) %>%
        gather(ind, valor, TMIST:TMISA)
      
      
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor <- ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >60, 60, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIST", "TMISB", "TMISM", "TMISA"), 
                            labels = c("Eq. de saúde totais", "Eq. de saúde nível 1",
                                       "Eq. de saúde nível 2", "Eq. de saúde nível 3")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 2)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/saude', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/saude/2_%s_tmi_saude_max60.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/saude', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/saude/2_%s_tmi_saude_max60.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/saude/2_%s_tmi_saude_max60.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp10 %<-% fazer_mapa_60_s('poa', mode1 = 'bike')
    temp11 %<-% fazer_mapa_60_s('poa', mode1 = 'transit')
    temp12 %<-% fazer_mapa_60_s('poa', mode1 = 'walk')
    
    rm(list = ls(pattern = "temp"))
    
    
    fazer_mapa_30_s <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIST, TMISB, TMISM, TMISA) %>%
        gather(ind, valor, TMIST:TMISA)
      
      
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >30, 30, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIST", "TMISB", "TMISM", "TMISA"), 
                            labels = c("Eq. de saúde totais", "Eq. de saúde nível 1",
                                       "Eq. de saúde nível 2", "Eq. de saúde nível 3")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 2)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/saude/1_%s_tmi_saude_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/saude/1_%s_tmi_saude_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/saude/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/saude/1_%s_tmi_saude_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp13 %<-% fazer_mapa_30_s('poa', mode1 = 'bike')
    temp14 %<-% fazer_mapa_30_s('poa', mode1 = 'transit')
    temp15 %<-% fazer_mapa_30_s('poa', mode1 = 'walk')
    
    rm(list = ls(pattern = "temp"))
    
    
    # Escolas ------------------------------------------------------
    
    fazer_mapa_180_se <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIET, TMIEI, TMIEF, TMIEM) %>%
        gather(ind, valor, TMIET:TMIEM)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >180, 180, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIET", "TMIEI", "TMIEF", "TMIEM"), 
                            labels = c("Eq. de educacao totais", "Eq. de educacao nível 1",
                                       "Eq. de educacao nível 2", "Eq. de educacao nível 3")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 2)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/educacao/5_%s_tmi_educacao_max180.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/educacao/5_%s_tmi_educacao_max180.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/educacao/5_%s_tmi_educacao_max180.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp1 %<-% fazer_mapa_180_se('poa', mode1 = 'bike')
    temp2 %<-% fazer_mapa_180_se('poa', mode1 = 'transit')
    temp3 %<-% fazer_mapa_180_se('poa', mode1 = 'walk')
    rm(list = ls(pattern = "temp"))
    
   
    fazer_mapa_120_se <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIET, TMIEI, TMIEF, TMIEM) %>%
        gather(ind, valor, TMIET:TMIEM)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >120, 120, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIET", "TMIEI", "TMIEF", "TMIEM"), 
                            labels = c("Eq. de educacao totais", "Eq. de educacao nível 1",
                                       "Eq. de educacao nível 2", "Eq. de educacao nível 3")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 2)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/educacao/4_%s_tmi_educacao_max120.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/educacao/4_%s_tmi_educacao_max120.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/educacao/4_%s_tmi_educacao_max120.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp1 %<-% fazer_mapa_120_se('poa', mode1 = 'bike')
    temp2 %<-% fazer_mapa_120_se('poa', mode1 = 'transit')
    temp3 %<-% fazer_mapa_120_se('poa', mode1 = 'walk')
    rm(list = ls(pattern = "temp"))
    
    
    fazer_mapa_90_se <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIET, TMIEI, TMIEF, TMIEM) %>%
        gather(ind, valor, TMIET:TMIEM)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >90, 90, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIET", "TMIEI", "TMIEF", "TMIEM"), 
                            labels = c("Eq. de educacao totais", "Eq. de educacao nível 1",
                                       "Eq. de educacao nível 2", "Eq. de educacao nível 3")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 2)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/educacao/3_%s_tmi_educacao_max90.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/educacao/3_%s_tmi_educacao_max90.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/educacao/3_%s_tmi_educacao_max90.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp1 %<-% fazer_mapa_90_se('poa', mode1 = 'bike')
    temp2 %<-% fazer_mapa_90_se('poa', mode1 = 'transit')
    temp3 %<-% fazer_mapa_90_se('poa', mode1 = 'walk')
    rm(list = ls(pattern = "temp"))
    
    
    fazer_mapa_60_se <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIET, TMIEI, TMIEF, TMIEM) %>%
        gather(ind, valor, TMIET:TMIEM)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >60, 60, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIET", "TMIEI", "TMIEF", "TMIEM"), 
                            labels = c("Eq. de educacao totais", "Eq. de educacao nível 1",
                                       "Eq. de educacao nível 2", "Eq. de educacao nível 3")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 2)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/educacao/2_%s_tmi_educacao_max60.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/educacao/2_%s_tmi_educacao_max60.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/educacao/2_%s_tmi_educacao_max60.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp1 %<-% fazer_mapa_60_se('poa', mode1 = 'bike')
    temp2 %<-% fazer_mapa_60_se('poa', mode1 = 'transit')
    temp3 %<-% fazer_mapa_60_se('poa', mode1 = 'walk')
    rm(list = ls(pattern = "temp"))
    
    
    fazer_mapa_30_se <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIET, TMIEI, TMIEF, TMIEM) %>%
        gather(ind, valor, TMIET:TMIEM)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >30, 30, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIET", "TMIEI", "TMIEF", "TMIEM"), 
                            labels = c("Eq. de educacao totais", "Eq. de educacao nível 1",
                                       "Eq. de educacao nível 2", "Eq. de educacao nível 3")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 2)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/educacao/1_%s_tmi_educacao_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/educacao/1_%s_tmi_educacao_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/educacao/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/educacao/1_%s_tmi_educacao_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp1 %<-% fazer_mapa_30_se('poa', mode1 = 'bike')
    temp2 %<-% fazer_mapa_30_se('poa', mode1 = 'transit')
    temp3 %<-% fazer_mapa_30_se('poa', mode1 = 'walk')
    rm(list = ls(pattern = "temp"))
    

# Lazer -------------------------------------------------------------------

    fazer_mapa_30_lz <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMILZ) %>%
        gather(ind, valor, TMILZ)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >30, 30, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMILZ"), 
                            labels = c("Eq. de lazer")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 1)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/lazer/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/lazer/1_%s_tmi_lazer_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/lazer/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/lazer/1_%s_tmi_lazer_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/lazer/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/lazer/1_%s_tmi_lazer_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp1 %<-% fazer_mapa_30_lz('poa', mode1 = 'bike')
    temp2 %<-% fazer_mapa_30_lz('poa', mode1 = 'transit')
    temp3 %<-% fazer_mapa_30_lz('poa', mode1 = 'walk')
    rm(list = ls(pattern = "temp"))
    

# Paraciclos --------------------------------------------------------------

    fazer_mapa_30_pr <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIPR) %>%
        gather(ind, valor, TMIPR)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >30, 30, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIPR"), 
                            labels = c("Paraciclos")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 1)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/paraciclos/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/paraciclos/1_%s_tmi_paraciclos_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/paraciclos/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/paraciclos/1_%s_tmi_paraciclos_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/paraciclos/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/paraciclos/1_%s_tmi_paraciclos_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp1 %<-% fazer_mapa_30_pr('poa', mode1 = 'bike')
    temp2 %<-% fazer_mapa_30_pr('poa', mode1 = 'transit')
    temp3 %<-% fazer_mapa_30_pr('poa', mode1 = 'walk')
    rm(list = ls(pattern = "temp"))
    
    

# Bikes Compartilhadas ----------------------------------------------------

    if (decisao_muni$bike_comp == 1) {
    
    fazer_mapa_30_bk <- function(sigla_munii, mode1, cols = 2, width = 14, height = 10) {
      
      # abrir acess
      acess <- dados_acc_maps %>% filter(sigla_muni == sigla_munii) %>%
        filter(mode == mode1) %>%
        select(sigla_muni, TMIBK) %>%
        gather(ind, valor, TMIBK)
      
      # abrir tiles
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
      
      map_tiles <- read_rds(path_maptiles)
      
      # ajustar levels
      acess <- acess %>%
        mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
        mutate(valor = ifelse(valor >30, 30, valor)) %>%
        mutate(ind = factor(ind, 
                            levels = c("TMIBK"), 
                            labels = c("Bicicletas Compartilhadas")))
      
      
      # fazer plots
      plot3 <- ggplot()+
        geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        # nova escala
        new_scale_fill() +
        geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6)+
        viridis::scale_fill_viridis(option = "D",
                                    direction = -1
                                    # , limits = c(0, 0.72)
                                    # , breaks = c(0.001, 0.35, 0.7)
                                    # , labels = c(0, "35", "70%")
        ) +
        scale_colour_manual(values = 'transparent') +
        facet_wrap(~ind, ncol = 1)+
        theme_for_TMI()+
        labs(fill = "Tempo Mínimo\n de Acesso")
      
      # create dir
      if (mode1 == "bike"){
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/bicicleta/tmi/bikes_compartilhadas/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/bicicleta/tmi/bikes_compartilhadas/1_%s_tmi_bikes_compartilhadas_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 600, width = width, height = height, units = "cm")
      } else if (mode1 == "transit") {
        
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/transporte_publico/tmi/bikes_compartilhadas/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/transporte_publico/tmi/bikes_compartilhadas/1_%s_tmi_bikes_compartilhadas_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      } else if (mode1 == "walk") {
        suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/caminhada/tmi/bikes_compartilhadas/', sigla_munii)))
        
        
        ggsave(plot3, 
               file= sprintf("../data/map_plots_acc/muni_%s/caminhada/tmi/paraciclos/1_%s_tmi_bikes_compartilhadas_max30.png",
                             sigla_munii, sigla_munii), 
               dpi = 300, width = width, height = height, units = "cm")
        
      }
      
      
    }
    temp1 %<-% fazer_mapa_30_bk('poa', mode1 = 'bike')
    temp2 %<-% fazer_mapa_30_bk('poa', mode1 = 'transit')
    temp3 %<-% fazer_mapa_30_bk('poa', mode1 = 'walk')
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