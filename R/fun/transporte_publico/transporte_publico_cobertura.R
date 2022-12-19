#Acesso físico transporte público

#10_3-mapas cicloviários e de transporte público

#geraação de mapas

# rm(list = ls())


source('./R/fun/setup.R')

library(patchwork)
library(showtext)
# library(ggmap)
library(ggspatial)
showtext_auto()
width <- 16.5
height <- 16.5
# font_add_google("Encode Sans Light 300")
font_add("encode_sans", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-VariableFont_wdth,wght.ttf')
font_add("encode_sans_regular", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Regular.ttf')
font_add("encode_sans_bold", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Bold.ttf')
font_add("encode_sans_light", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Light.ttf')
sigla_muni <- 'con'


#gráficos de ciclovias

graficos <- function(munis = "all"){
  
  
  
  faz_grafico_e_salva <- function(sigla_muni, width = 16.5, height = 16.5){
    
    message(paste("Rodando",sigla_muni, "\n"))
    

# LEITURA DOS DADOS -------------------------------------------------------

    
    
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
    # mapview(dados_linhas)
    
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
    
    area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
                                       sigla_muni)) %>% filter(DN == 24) %>%
      st_make_valid() %>%
      st_union()
    # mapview(simplepolys)
    
    sigla_municipio <- sigla_muni
    decisao_muni <- read_excel('../planilha_municipios.xlsx',
                               sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
    
    simplepolys <- st_simplify(area_urbanizada, dTolerance = 300) %>%
      st_make_valid() %>%
      st_transform(decisao_muni$epsg) %>%
      st_buffer(2) %>%
      st_union() 
    
    assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                                      sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
      mutate(title = "Assentamentos Precários")
    
    
    dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                        sigla_muni, sigla_muni))
    
    pop_counts <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(pop_total = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% mutate(quintil = as.factor(ntile(pop_total, 4)))
    
    
    # mapview(area_natend300)
    
    #mapa localização
    cores_ciclo <- c('#5766cc', '#33b099', '#d96e0a')


# MAPA DAS LINHAS TP ------------------------------------------------------

    
    
    
    
    map_linhas_tp <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(fill = "#5766cc"),
              
              # fill = "#d96e0a",
              size = 1.3,
              color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      geom_sf(data = st_transform(dados_linhas, 3857),
              aes(color = '#0f805e'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              linewidth = 0.8) +
      
      scale_color_manual(name = "Infraestrutura de Transporte Público",
                         values = c("#0f805e" = "#0f805e"),
                         label = c("#0f805e" = "Linhas de Transporte Público")
      )+
      
      
      ggnewscale::new_scale_color() +
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "grey45"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.6,
              alpha= 0.7)  +
      scale_color_manual(name = "Área Urbanizada",
                         values = c("grey45" = "grey45"),
                         label = c("grey45" = "Palmas")
      )+
      
      
      scale_fill_manual(values = c("1" = "#FEF8ED",
                                   
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542",
                                   
                                   # "#33b099" = "#33b099",
                                   "#5766cc" = "#5766cc"),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos",
                                  # "#33b099" = "Cobertura de 300m",
                                  "#5766cc" = "Aglomerados subnormais")) +
      labs(fill = "População") +
      # ggnewscale::new_scale_color() +
      
      
      
      
      
      # 
      # scale_color_manual(values = c("grey45" = "grey45",
      #                               '#0f805e' = '#0f805e'),
      #                    label = c("grey45" = "Área Urbanizada\n(Mapa Biomas (2021))",
      #                              '#0f805e' = "Ciclovias")) +
    # ggsn::scalebar(dados_ciclovias_buffer, dist = 5, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84')
    
    # scale_fill_manual(values = '#d96e0a',
    #                   label = "Aglomerados\nSubnormais") +
    # labs(fill = '') +
    # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = .5) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tr") +
      # geom_sf(data = assentamentos,
      #         aes(colour = "white"),
      #         fill = NA,
      #         size = 1.3)+
      # scale_fill_manual(values = c('#33b099'='#33b099',"#d96e0a" ="#d96e0a", '#CFF0FF' = "#CFF0FF"),
      #                   labels = c('#33b099'="Cobertura de 300m","#d96e0a"="Aglomerados\nSubnormais",
      #                              '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)")
      # ) +
      # 
      # scale_color_identity(labels = c("#21367d" = "",
      #                                 blue = ""), guide = "legend") +
    # labs(color = "Área urbanizada\n(Mapbiomas 2021)")+
    # tema_populacao()
    theme(
      strip.text.x = element_text(size=rel(1.2)),
      strip.background = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.margin = margin(unit(c(10,10,5,10),"mm")),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(1,"line"),
      legend.key = element_blank(),
      legend.text=element_text(size=25, family = "encode_sans_light"),
      legend.title=element_text(size=30, family = "encode_sans_bold"),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.22, 0.30),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
      #                                      colour = "#E0DFE3"),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/5-linhas_tp_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = 1.62*13, height = 13, units = "cm" )
    
    
    if (sigla_muni == "pal"){
    
    map_linhas_tp_sem_zoom <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(fill = "#5766cc"),
              
              # fill = "#d96e0a",
              size = 1.3,
              color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      geom_sf(data = st_transform(dados_linhas, 3857),
              aes(color = '#0f805e'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              linewidth = 1.0) +
      
      scale_color_manual(name = "Infraestrutura de Transporte Público",
                         values = c("#0f805e" = "#0f805e"),
                         label = c("#0f805e" = "Linhas de Transporte Público")
      )+
      
      
      ggnewscale::new_scale_color() +
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "grey45"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.8,
              alpha= 0.7)  +
      scale_color_manual(name = "Área Urbanizada",
                         values = c("grey45" = "grey45"),
                         label = c("grey45" = "Palmas")
      )+
      
      
      scale_fill_manual(values = c("1" = "#FEF8ED",
                                   
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542",
                                   
                                   # "#33b099" = "#33b099",
                                   "#5766cc" = "#5766cc"),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos",
                                  # "#33b099" = "Cobertura de 300m",
                                  "#5766cc" = "Aglomerados subnormais")) +
      labs(fill = "População") +
      # ggnewscale::new_scale_color() +
      
      
      
      
      
      # 
      # scale_color_manual(values = c("grey45" = "grey45",
      #                               '#0f805e' = '#0f805e'),
      #                    label = c("grey45" = "Área Urbanizada\n(Mapa Biomas (2021))",
      #                              '#0f805e' = "Ciclovias")) +
    # ggsn::scalebar(dados_ciclovias_buffer, dist = 5, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84')
    
    # scale_fill_manual(values = '#d96e0a',
    #                   label = "Aglomerados\nSubnormais") +
    # labs(fill = '') +
    # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tr") +
      # geom_sf(data = assentamentos,
      #         aes(colour = "white"),
      #         fill = NA,
      #         size = 1.3)+
      # scale_fill_manual(values = c('#33b099'='#33b099',"#d96e0a" ="#d96e0a", '#CFF0FF' = "#CFF0FF"),
      #                   labels = c('#33b099'="Cobertura de 300m","#d96e0a"="Aglomerados\nSubnormais",
      #                              '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)")
      # ) +
      # 
      # scale_color_identity(labels = c("#21367d" = "",
      #                                 blue = ""), guide = "legend") +
    # labs(color = "Área urbanizada\n(Mapbiomas 2021)")+
    # tema_populacao()
    theme(
      strip.text.x = element_text(size=rel(1.2)),
      strip.background = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.margin = margin(unit(c(10,10,5,10),"mm")),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(1,"line"),
      legend.key = element_blank(),
      legend.text=element_text(size=25, family = "encode_sans_light"),
      legend.title=element_text(size=30, family = "encode_sans_bold"),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.22, 0.30),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
      #                                      colour = "#E0DFE3"),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      guides(fill = guide_legend(byrow = TRUE)) +
      coord_sf(ylim = c(-1097645,-1179329), xlim = c(-5466340,-5327631), expand = FALSE)
    
    ggsave(map_linhas_tp_sem_zoom,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/5-linhas_tp_sem_zoom_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = 1.62*13, height = 13, units = "cm" )
    
    }
    
    
    # #mapa localização linhastp zoom
    # 
    # map_linhas_tp_zoom <- ggplot() +
    #   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   coord_equal() +
    #   scale_fill_identity()+
    #   # nova escala
    #   new_scale_fill() +
    #   # theme_map() +
    #   geom_sf(data = st_transform(dados_linhas,3857), aes(color = '#d96e0a'),
    #           alpha = 1) +
    #   scale_color_manual(values = c("#d96e0a" ="#d96e0a"),
    #                      labels = c("#d96e0a"="Linhas de\nTransporte Coletivo")
    #   ) +
    #   labs(color = "")+
    #   
    #   # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    #   
    #   geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
    #   
    #   # tema_populacao()
    #   theme(
    #     strip.text.x = element_text(size=rel(1.2)),
    #     strip.background = element_blank(),
    #     panel.background = element_rect(fill = NA, colour = NA),
    #     legend.background = element_blank(),
    #     axis.text = element_blank(),
    #     axis.title = element_blank(),
    #     axis.ticks = element_blank(), 
    #     panel.grid = element_blank(),
    #     plot.margin=unit(c(2,0,0,0),"mm"),
    #     legend.key.width=unit(2,"line"),
    #     legend.key.height = unit(.5,"cm"),
    #     legend.text=element_text("Tipo", size=rel(1)),
    #     legend.title=element_text(size=rel(1),                                   ),
    #     plot.title = element_text(hjust = 0, vjust = 4),
    #     strip.text = element_text(size = 10),
    #     legend.position = c(0.25, 0.12),
    #     legend.spacing.y = unit(0.2, 'cm')
    #     # legend.margin = margin(t = -80)
    #   ) +
    #   guides(fill = guide_legend(byrow = TRUE)) +
    #   aproxima_muni_zoom(sigla_muni = sigla_muni)
    # 
    # 
    # 
    # 
    # # width = 16; height = 16
    # # map_linhas_tp
    # ggsave(map_linhas_tp_zoom,
    #        device = "png",
    #        filename =  sprintf("../data/map_plots_transports/muni_%s/5-linhas_tp_zoom_%s.png", sigla_muni, sigla_muni),
    #        dpi = 400,
    #        width = width, height = height, units = "cm" )
    # 
    
    
    
# Mapa - Buffer PEDS 300 M-------------------------------------------------
    

    map_linhas_tp_buffer <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      new_scale_fill() +
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      geom_sf(data = assentamentos,
              aes(fill = "#5766cc"),
              size = 1.3,
              color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      geom_sf(data = st_transform(dados_peds_buffer_300, 3857),
              aes(color = '#0f805e'),
              # color = '#0f805e',
              # color = NA,
              fill = "grey70",
              alpha = 0.7,
              linewidth = 1.0) +
      
      scale_color_manual(name = "Infraestrutura de Transporte Público",
                         values = c("#0f805e" = "#0f805e"),
                         label = c("#0f805e" = "Cobertura de 300m")
      )+
      ggnewscale::new_scale_color() +
      geom_sf(data = simplepolys %>% st_transform(3857),
              aes(color = "grey45"),
              fill = NA,
              linewidth = 0.8,
              alpha= 0.7)  +
      scale_color_manual(name = "Área Urbanizada",
                         values = c("grey45" = "grey45"),
                         label = c("grey45" = "Palmas")
      )+
      
      
      scale_fill_manual(values = c("1" = "#FEF8ED",
                                   
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542",
                                   
                                   # "#33b099" = "#33b099",
                                   "#5766cc" = "#5766cc"),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos",
                                  # "#33b099" = "Cobertura de 300m",
                                  "#5766cc" = "Aglomerados subnormais")) +
      labs(fill = "População") +
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tr") +
    theme(
      strip.text.x = element_text(size=rel(1.2)),
      strip.background = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.margin = margin(unit(c(10,10,5,10),"mm")),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(1,"line"),
      legend.key = element_blank(),
      legend.text=element_text(size=25, family = "encode_sans_light"),
      legend.title=element_text(size=30, family = "encode_sans_bold"),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.22, 0.30),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.box.just = "left"
    ) +
      guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp_buffer,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/6-linhas_tp_buffer_300m_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = 1.62*13, height = 13, units = "cm" )
    

# MAPA PEDS BUFFER 500M ----------------------------------------------------------

    map_linhas_tp_buffer_500 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      new_scale_fill() +
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      geom_sf(data = assentamentos,
              aes(fill = "#5766cc"),
              size = 1.3,
              color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      geom_sf(data = st_transform(dados_peds_buffer_500, 3857),
              aes(color = '#0f805e'),
              # color = '#0f805e',
              # color = NA,
              fill = "grey70",
              alpha = 0.7,
              linewidth = 1.0) +
      
      scale_color_manual(name = "Infraestrutura de Transporte Público",
                         values = c("#0f805e" = "#0f805e"),
                         label = c("#0f805e" = "Cobertura de 500m")
      )+
      ggnewscale::new_scale_color() +
      geom_sf(data = simplepolys %>% st_transform(3857),
              aes(color = "grey45"),
              fill = NA,
              linewidth = 0.8,
              alpha= 0.7)  +
      scale_color_manual(name = "Área Urbanizada",
                         values = c("grey45" = "grey45"),
                         label = c("grey45" = "Palmas")
      )+
      
      
      scale_fill_manual(values = c("1" = "#FEF8ED",
                                   
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542",
                                   
                                   # "#33b099" = "#33b099",
                                   "#5766cc" = "#5766cc"),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos",
                                  # "#33b099" = "Cobertura de 300m",
                                  "#5766cc" = "Aglomerados subnormais")) +
      labs(fill = "População") +
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tr") +
      theme(
        strip.text.x = element_text(size=rel(1.2)),
        strip.background = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        legend.margin = margin(unit(c(10,10,5,10),"mm")),
        legend.key.width=unit(2,"line"),
        legend.key.height = unit(1,"line"),
        legend.key = element_blank(),
        legend.text=element_text(size=25, family = "encode_sans_light"),
        legend.title=element_text(size=30, family = "encode_sans_bold"),
        plot.title = element_text(hjust = 0, vjust = 4),
        strip.text = element_text(size = 10),
        legend.position = c(0.22, 0.30),
        legend.box.background = element_rect(fill=alpha('white', 0.7),
                                             colour = "#A09C9C",
                                             linewidth = 0.8,
                                             linetype = "solid"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.box.just = "left"
      ) +
      guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp_buffer_500,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/7-linhas_tp_buffer_500m_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = 1.62*13, height = 13, units = "cm" )
    
    
    
    

# DIFERENCA DE 300M -------------------------------------------------------

    
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

# Hex atendidos -----------------------------------------------------------

    
    
    #aqui esta usando os dados os dados civlovirarios. verificar
    # mapview(dados_hex_intersect_300)
    dados_hex_intersect_300 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(dados_peds_buffer_300)
    dados_hex_intersect_300 <- dados_hex_intersect_300 %>%
      mutate(area = st_area(.)) %>%
      mutate(area = as.numeric(area)) %>%
      filter(area >= 60000)
    
    
    # mapview(dados_hex_intersect_300)
    id_hex_intersects_bus <- dados_hex_intersect_300$id_hex %>% unique()
    
    #dados da microssimulação
    
    data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                   sigla_muni, sigla_muni)) %>%  mutate(V0606 = case_when(V0606 == 1 ~ "Brancos",
                                                                                          V0606 == 2 ~ "Pretos",
                                                                                          V0606 == 3 ~ "Amarelos",
                                                                                          V0606 == 4 ~ "Pardos",
                                                                                          V0606 == 5 ~ "Indígenas"))
    #ajeitar o formato
    grid_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/grid_muni_%s.RDS',
                                   sigla_muni, sigla_muni))
    # aaaa <- data_micro %>% st_drop_geometry() %>%
    #   group_by(hex) %>%
    #   summarise(n = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
    #   st_as_sf()
    # 
    # mapview(aaaa)
      
      
    #checar setores com todos os renda_class_pc == n_col
    
    # lista_tract <- data_micro %>% group_by(code_tract, renda_class_pc) %>%
    #   summarise(n = n()) %>% ungroup() %>%
    #   group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
    #                                      n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
    #   filter(n_classes > n_classes_col) %>% pull(code_tract)
    
    
    
    # data_micro2 <- data_micro %>% filter(code_tract %in% lista_tract) %>% select(1:12, V0606, hex) %>%
    #   mutate(V0606 = as.factor(V0606))
    
    data_micro2 <- data_micro %>% select(1:12, V0606, hex) %>%
      mutate(V0606 = as.factor(V0606))
    
    
    
    # aaaa <- data_micro2 %>% st_drop_geometry() %>%
    #   group_by(hex) %>%
    #   summarise(n = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
    #   st_as_sf()
    # 
    # mapview(aaaa)
    
    
    # levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
    data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
    
    data_micro_bus <- data_micro2 %>% filter(hex %in% id_hex_intersects_bus)
    # mapview(data_micro_bus)
    #recorte nos dados da microssimulacao
    
    recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
      mutate(class = paste(genero, cor))%>%
      mutate(id = paste(class, quintil_renda))
    
    # teste_recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus), "OK","N")) %>%
    #   filter(teste == "OK") %>%
    #   group_by(hex) %>%
    #   summarise(n = n()) %>%
    #   left_join(dados_hex, by = c("hex" = "id_hex")) %>% st_as_sf()
    # mapview(teste_recorte_rr)
# Cleveland Plot Atendidos 300m -------------------------------------------

    
    
    plot_cleveland_bus300 <- ggplot(recorte_rr, aes(prop, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#ADBEF0", "#174DE8", "#EBB814", "#B39229"),
      )+
      scale_size_continuous( range = c(0,11),
                             limits = c(1,50000),
                             breaks = c(0,10000,25000,50000),
                             name = "Habitantes",
                             guide = "legend")
    p_b300_bus <- plot_cleveland_bus300 + scale_color_manual(name = "Gênero e Cor",
                                                           values = c("Homens Brancos"="#ADBEF0",
                                                                      "Homens Pretos"="#174DE8",
                                                                      "Mulheres Brancos" = "#EBB814",
                                                                      "Mulheres Pretos"="#B39229"),
                                                           labels = c("Homens Brancos",
                                                                      "Homens Pretos",
                                                                      "Mulheres Brancas",
                                                                      "Mulheres Pretas"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "População com acesso à infraestrutura de transporte público")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      # theme_minimal() +
      xlab("% de habitantes do recorte") +
      ylab("Quartil de renda per capita") +
      scale_x_continuous(labels = scales::percent,
                         limits = c(0.90,0.95),
                         breaks = seq(0.85,0.95, 0.025)) +
      scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
      theme(#axis.title = element_blank(),
        panel.grid.minor = element_line(),
        legend.background = element_blank(),
        panel.background = element_blank(),
        legend.direction = "vertical",
        legend.position = "bottom",
        text = element_text(family = "sans",
                            # face = "bold",
                            size = 20),
        
        plot.title = element_text(size = 35, margin = margin(b=10), family = "encode_sans_bold"),
        plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 30, margin = margin(t=10), color = "grey70", hjust = 0),
        legend.title = element_text(size = 35, family = "encode_sans_bold"),
        legend.text = element_text(size = 30, family = "encode_sans_light"),
        axis.text = element_text(size = 30, family = "encode_sans_light"),
        axis.title = element_text(size = 35, family = "encode_sans_bold"))
    
    
    
    #escrita do gráfico
    
    ggsave(p_b300_bus,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/11-linhasbus_300_cleveland_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 15, height = 10, units = "cm" )
    
    
    

# Pessoas não atendidas 300 m----------------------------------------------------

    dados_hex_intersect_ntad300 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_difference(dados_peds_buffer_300)
    dados_hex_intersect_ntad300 <- dados_hex_intersect_ntad300 %>%
      mutate(area = st_area(.)) %>%
      mutate(area2 = as.numeric(area)) %>%
      filter(area2 > 60000.0)
    
    
    # mapview(dados_hex_intersect_ntad300)
    id_hex_intersects_bus_ntad300 <- dados_hex_intersect_ntad300$id_hex %>% unique()
    
    #ESPACIAL
    
    # recorte_rr_ntad <- data_micro2 %>% mutate(quintil_renda = ntile(Rend_pc, 4)) %>%
    #   filter(quintil_renda %in% c(1,2,3)) %>%
    #   filter(hex %in% id_hex_intersects_bus_ntad300)%>%
    #   st_drop_geometry() %>%
    #   group_by(hex) %>% summarise(n = n()) %>%
    #   left_join(dados_hex, by = c("hex" = "id_hex")) %>% st_as_sf()
    
    # mapview(recorte_rr_ntad)
    # pop_counts <- dados_simulacao %>% 
    #   group_by(hex) %>%
    #   summarise(pop_total = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
    #   st_as_sf() %>% mutate(quintil = as.factor(ntile(pop_total, 4)))
    
    
    # mapview(pop_counts)
    # dados_simulacao %>% filter(class)
    
    
    recorte_rr_ntad <- data_micro2 %>%
      mutate(quintil_renda = ntile(Rend_pc, 4)) %>%
      filter(quintil_renda %in% c(1,2,3)) %>%
      filter(hex %in% id_hex_intersects_bus_ntad300) %>% st_drop_geometry() %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      # mutate(
      #   quintil_renda = ntile(Rend_pc, 4)) %>%
      group_by(hex) %>% summarise(n = n()) %>%
      left_join(dados_hex, by = c("hex" = "id_hex")) %>% st_as_sf() %>%
      mutate(n = as.numeric(n))
    # mapview(recorte_rr_ntad)
    # mapview(dados_hex_intersect_ntad300)
    
    #NÃO ESPACIAL
    recorte_rr_ntad2 <- data_micro2 %>% group_by(hex) %>%
      filter(hex %in% id_hex_intersects_bus_ntad300) %>% st_drop_geometry() %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      group_by(cor, quintil_renda, genero) %>% summarise(n = n()) %>%
      mutate(class = paste(genero, cor))%>%
      mutate(id = paste(class, quintil_renda))
    
    
    
    plot_cleveland_bus300_ntad <- ggplot(recorte_rr_ntad2, aes(n, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("grey70", "#FFB578", "black", "#cc3003"),
      )+
      scale_size_continuous( range = c(0,10),
                             limits = c(1,3000),
                             breaks = c(0,500,1000,3000),
                             name = "Habitantes",
                             guide = "legend")
    p_b300_bus_ntad <- plot_cleveland_bus300_ntad + scale_color_manual(name = "Gênero e Cor",
                                                             values = c("Homens Brancos"="grey70",
                                                                        "Homens Pretos"="#FFB578",
                                                                        "Mulheres Brancos" = "black",
                                                                        "Mulheres Pretos"="#cc3003"),
                                                             labels = c("Homens Brancos",
                                                                        "Homens Pretos",
                                                                        "Mulheres Brancas",
                                                                        "Mulheres Pretas"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "População sem acesso à infraestrutura de Transporte público")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      # theme_minimal() +
      xlab("Habitantes do recorte") +
      ylab("Quartil de renda per capita") +
      scale_x_continuous(labels = scales::number,
                         limits = c(1000,3500),
                         breaks = seq(1000,3500, 500)) +
      scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
      theme(#axis.title = element_blank(),
        panel.grid.minor = element_line(),
        legend.background = element_blank(),
        panel.background = element_blank(),
        legend.direction = "vertical",
        legend.position = "bottom",
        text = element_text(family = "sans",
                            # face = "bold",
                            size = 20),
        
        plot.title = element_text(size = 35, margin = margin(b=10), family = "encode_sans_bold"),
        plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 30, margin = margin(t=10), color = "grey70", hjust = 0),
        legend.title = element_text(size = 35, family = "encode_sans_bold"),
        legend.text = element_text(size = 30, family = "encode_sans_light"),
        axis.text = element_text(size = 30, family = "encode_sans_light"),
        axis.title = element_text(size = 35, family = "encode_sans_bold"))
    
    ggsave(p_b300_bus_ntad,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/12-linhasbus_300_cleveland_nao_atendidos%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 15, height = 10, units = "cm" )
    
    # %>% 
    #   ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
    #   
    #   summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
    #   mutate(class = paste(genero, cor))%>%
    #   mutate(id = paste(class, quintil_renda))
    
    # recorte_ntad <- data_micro %>% filter(hex %in% id_hex_intersects_bus_ntad300) %>%
    #   left_join(grid_micro, by = c("hex" = "h3_address")) %>% st_as_sf()
    # 
    # 
    # 
    # recorte_ntad <- data_micro2 %>% group_by(hex) %>%
    #   filter(hex %in% id_hex_intersects_bus_ntad300) %>% st_drop_geometry() %>% summarise(n = n()) %>%
    #   left_join(dados_hex, by = c("hex" = "id_hex")) %>% st_as_sf()
    # 
    #   
    #   mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus_ntad300), "N","OK")) %>%
    #   filter(teste == "N") %>% left_join(grid_micro, by = c("hex" = "h3_address")) %>% st_as_sf()
    #   sum(recorte_ntad$n)
    # mapview(recorte_ntad, zcol = "n")
    # mapview(dados_hex_intersect_ntad300)
    # 
    # vamo <- st_join(dados_hex %>% st_transform(decisao_muni$epsg),
    #                 dados_peds_buffer_300 %>% st_transform(decisao_muni$epsg) %>% 
    #                   mutate(teste = 'OK')) %>% na.omit() %>% mapview()
    
    
    


# Aglomerados Subnormais nao atendidos ------------------------------------

    
    
    aglomerados_natend300 <- assentamentos %>% st_transform(decisao_muni$epsg) %>%
      st_difference(dados_peds_buffer_300) %>% st_as_sf() %>%
      mutate(area_ntad = st_area(.))
    
    assentamentos_area <- assentamentos %>%
      mutate(area_total = st_area(.)) %>%
      select(NM_AGSN, area_total)
    
    aglomerados_natend300 <- aglomerados_natend300 %>%
      left_join(assentamentos_area %>% st_drop_geometry(), by = "NM_AGSN") %>%
      mutate(area_prop = area_ntad/area_total,
             pop_ntad = as.numeric(SUM_EDOC*area_prop))
    # mapview(aglomerados_natend500, zcol = "pop_ntad")
    
    write_sf(aglomerados_natend300,
             sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s_assentamentos_ntad.gpkg',
                     sigla_muni, sigla_muni))
    write.xlsx(aglomerados_natend300 %>% st_drop_geometry(),
             sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s_assentamentos_ntad.xlsx',
                     sigla_muni, sigla_muni))
    
    # aglomerados_natend500 <- assentamentos %>% st_transform(decisao_muni$epsg) %>%
    #   st_join(dados_peds_buffer_500 %>% mutate(teste = 1)) %>% st_as_sf() %>%
    #   filter(is.na(teste)==T)
    
    

# Mapa dos Aglomerados Nao Atendidos --------------------------------------

# library(units)
#     
#     get_image_dimensions(path_maptiles)
#     
#     require(jpeg)
#     img <- read_rds(path_maptiles) 
#     
#     ratio <- (max(img$y)-min(img$y))/(max(img$x)-min(img$x))
    
    dados_areas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                   sigla_muni, sigla_muni), layer= "areas")
    # mapview(dados_areas)
    
    
    
    map_aglomerados_ntad <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "grey45"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.8,
              alpha= 0.7)  +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "grey70"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      

      # ggnewscale::new_scale_color() +
      
      
     
      
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(recorte_rr_ntad, 3857),
              aes(fill = n),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(aglomerados_natend300, 3857),
              aes(color = '#0f805e'),
              # color = '#0f805e',
              # color = NA,
              # fill = "#0f805e",
              fill = NA,
              alpha = 0.6,
              linewidth = 1.0) +
      scale_color_manual(name = "Uso do solo",
                         values = c("grey45" = "grey45",
                                    "grey70" = "grey70",
                                    "#0f805e" = "#0f805e"),
                         label = c("grey45" = "Área urbanizada",
                                   "grey70" = "Áreas de planejamento",
                                   "#0f805e" = "Aglomerados subnormais")
      )+
      
      # 
      # scale_color_manual(name = "Aglomerados subnormais",
      #                    values = c("#0f805e" = "#0f805e"),
      #                    label = c("#0f805e" = "Palmas")
      # )+
      
      # scale_fill_viridis_b() +

      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      # geom_sf(data = assentamentos,
      #         # aes(fill = "#d96e0a"),
      #         aes(fill = "#5766cc"),
      #         
      #         # fill = "#d96e0a",
      #         size = 1.3,
      #         color = NA,
      #         show.legend = "polygon",
      #         alpha = 0.9)+
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +

      
      
    viridis::scale_fill_viridis(option = "B", direction = -1, name = "População (hab)"
                                
                                ) +
      
      
      # scale_fill_viridis_c(direction = -1,option = "inferno", name = "População não atendida") +
      # scale_fill_manual(values = c("1" = "#FEF8ED",
      #                              
      #                              "2" = "#FED49A",
      #                              "3" = "#FDA065",
      #                              "4" = "#D96542",
      #                              
      #                              # "#33b099" = "#33b099",
      #                              "#5766cc" = "#5766cc"),
      #                   label = c("1" = "25% menos populosos",
      #                             
      #                             "2" = "25% a 50% menos populosos",
      #                             "3" = "25% a 50% mais populosos",
      #                             "4" = "25% mais populosos",
      #                             # "#33b099" = "Cobertura de 300m",
      #                             "#5766cc" = "Aglomerados subnormais")) +
      labs(fill = "População") +
      # ggnewscale::new_scale_color() +
      
      
      
      
      
      # 
      # scale_color_manual(values = c("grey45" = "grey45",
      #                               '#0f805e' = '#0f805e'),
      #                    label = c("grey45" = "Área Urbanizada\n(Mapa Biomas (2021))",
      #                              '#0f805e' = "Ciclovias")) +
    # ggsn::scalebar(dados_ciclovias_buffer, dist = 5, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84')
    
    # scale_fill_manual(values = '#d96e0a',
    #                   label = "Aglomerados\nSubnormais") +
    # labs(fill = '') +
    # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tr") +
      # geom_sf(data = assentamentos,
      #         aes(colour = "white"),
      #         fill = NA,
      #         size = 1.3)+
      # scale_fill_manual(values = c('#33b099'='#33b099',"#d96e0a" ="#d96e0a", '#CFF0FF' = "#CFF0FF"),
      #                   labels = c('#33b099'="Cobertura de 300m","#d96e0a"="Aglomerados\nSubnormais",
      #                              '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)")
      # ) +
      # 
      # scale_color_identity(labels = c("#21367d" = "",
      #                                 blue = ""), guide = "legend") +
    # labs(color = "Área urbanizada\n(Mapbiomas 2021)")+
    # tema_populacao()
    theme(
      strip.text.x = element_text(size=rel(1.2)),
      strip.background = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.margin = margin(unit(c(10,10,5,10),"mm")),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(1,"line"),
      legend.key = element_blank(),
      legend.text=element_text(size=25, family = "encode_sans_light"),
      legend.title=element_text(size=30, family = "encode_sans_bold"),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.22, 0.30),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
      #                                      colour = "#E0DFE3"),
      legend.spacing.y = unit(0.4, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_aglomerados_ntad,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/14-linhas_tp_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = 1.62*13, height = 13, units = "cm" )
    
    
    
    
    
    
    
    # map_aglomerados_ntad <- ggplot() +
    #   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   coord_equal() +
    #   scale_fill_identity()+
    #   # nova escala
    #   new_scale_fill() +
    #   
    #   # geom_sf(data = simplepolys %>% st_transform(3857),
    #   #         # aes(size = 2),
    #   #         # aes(color = "#21367d"),
    #   #         aes(color = 'black'),
    #   #         # fill = NA,
    #   #         # stroke = 2,
    #   #         # size = 2,
    #   #         linewidth = 0.3,
    #   #         alpha= 0.8)  +
    #   # # theme_map() +
    #   geom_sf(data = st_transform(aglomerados_natend500, 3857),
    #           aes(fill = pop_ntad),
    #           # fill = '#33b099',
    #           # fill = NA,
    #           color = "black",
    #           alpha = .5,
    #           linewidth = 0.5) +
    #   
    #   # geom_sf(data = assentamentos,
    #   #         aes(fill = "#d96e0a"),
    #   #         # fill = "#d96e0a",
    #   #         size = 1.3,
    #   #         color = NA,
    #   #         show.legend = "polygon",
    #   #         alpha = 0.8)+
    #   
    #   # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    #   
    #   geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
    #   
    #   labs(fill = '') +
    #   labs(color = "") +
    #   
    #   # scale_fill_manual(values = c('#33b099'='#33b099',
    #   #                              "#d96e0a" ="#d96e0a"
    #   #                              # '#CFF0FF' = "#CFF0FF"
    #   # ),
    #   # labels = c('#33b099'="Cobertura de 300m",
    #   #            "#d96e0a"="Aglomerados\nSubnormais"
    #   #            # '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)"
    #   # )
    #   # ) +
    #   # scale_color_identity(labels = c("black" = "Área urbanizada\n(Mapbiomas 2021)",
    #   #                                 blue = ""), guide = "legend") +
    # # scale_fill_gradientn(
    # #   name = "População (hab)",
    # #   colors = colors_blue,
    # #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    # #   # values = NULL,
    # #   space = "Lab",
    # #   na.value = NA,
    # #   # guide = "colourbar",
    # #   aesthetics = "fill",
    # #   # colors
    # # ) +
    #   # geom_sf_label(data = st_transform(aglomerados_natend500, 3857),
    #   #               aes(label = NM_AGSN)
    #   #               )+
    # 
    #   
    #   
    #   # tema_populacao()
    #   theme(
    #     strip.text.x = element_text(size=rel(1.2)),
    #     strip.background = element_blank(),
    #     panel.background = element_rect(fill = NA, colour = NA),
    #     legend.background = element_blank(),
    #     axis.text = element_blank(),
    #     axis.title = element_blank(),
    #     axis.ticks = element_blank(), 
    #     panel.grid = element_blank(),
    #     plot.margin=unit(c(2,0,0,0),"mm"),
    #     legend.key.width=unit(2,"line"),
    #     legend.key.height = unit(.5,"cm"),
    #     legend.text=element_text("Tipo", size=rel(1)),
    #     legend.title=element_text(size=rel(1),                                   ),
    #     plot.title = element_text(hjust = 0, vjust = 4),
    #     strip.text = element_text(size = 10),
    #     legend.position = c(0.18, 0.15),
    #     legend.spacing.y = unit(0.05, 'cm'),
    #     legend.margin = margin(-0.45,0,0,0, unit="cm")
    #     # legend.margin = margin(t = -80)
    #   ) +
    #   guides(fill = guide_legend(byrow = TRUE)) +
    #   aproxima_muni(sigla_muni = sigla_muni)
    # # width = 16; height = 16
    # # map_empregos
    # ggsave(map_aglomerados_ntad,
    #        device = "png",
    #        filename =  sprintf("../data/map_plots_transports/muni_%s/12-aglomerados_ntad_500m_tp_%s.png", sigla_muni, sigla_muni),
    #        dpi = 400,
    #        width = width, height = height, units = "cm" )
        
    
    
    
    
    
    # mapview(aglomerados_natend500)
    
    
    # dados_hex_intersect_500 <- dados_hex %>%
    #   st_as_sf() %>%
    #   st_transform(decisao_muni$epsg) %>%
    #   st_intersection(dados_peds_buffer_500)
    # dados_hex_intersect_500 <- dados_hex_intersect_500 %>%
    #   mutate(area = st_area(.))
    # 
    # dados_hex_intersect_n300 <- dados_hex %>%
    #   st_as_sf() %>%
    #   st_transform(decisao_muni$epsg) %>%
    #   st_intersection(area_natend300)
    # dados_hex_intersect_n300 <- dados_hex_intersect_n300 %>%
    #   mutate(area = st_area(.))
    # 
    # # mapview(dados_hex_intersect_n500)
    # 
    # dados_hex_intersect_n500 <- dados_hex %>%
    #   st_as_sf() %>%
    #   st_transform(decisao_muni$epsg) %>%
    #   st_intersection(area_natend500)
    # dados_hex_intersect_n300 <- dados_hex_intersect_n300 %>%
    #   mutate(area = st_area(.))
    # 
    # 
    # 
    # 
    # 
    
    

# Mapa de Frequencias -----------------------------------------------------

frequencias <- read_rds(sprintf('../data/tp_frequencias/muni_%s/rotas_freq_%s.rds',
                                sigla_muni, sigla_muni))
    
frequencias2 <- frequencias %>% mutate(cond = case_when(headway_medio <= 15 ~ '< 15 minutos',
                                                     headway_medio <= 30 ~ '15-30 minutos',
                                                     headway_medio <= 60 ~ '30-60 minutos',
                                                     headway_medio > 60 ~ '>60 minutos'
    )) %>% arrange(headway_medio) %>%
  mutate(cond = factor(cond, levels = c('< 15 minutos',
                                        '15-30 minutos',
                                        '30-60 minutos',
                                        '>60 minutos')))
    

    # map_frequencias <- ggplot() +
    #   # geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   # coord_equal() +
    #   # scale_fill_identity()+
    #   # # nova escala
    #   # new_scale_fill() +
    #   
    #   # geom_sf(data = simplepolys %>% st_transform(3857),
    #   #         # aes(size = 2),
    #   #         # aes(color = "#21367d"),
    #   #         aes(color = 'black'),
    #   #         # fill = NA,
    #   #         # stroke = 2,
    #   #         # size = 2,
    #   #         linewidth = 0.3,
    #   #         alpha= 0.8)  +
    #   # theme_map() +
    #   geom_sf(data = st_transform(frequencias2, 3857),
    #           # aes(fill = cond),
    #           fill = '#21367d',
    #           # fill = NA,
    #           color = NA,
    #           alpha = .5,
    #           size = 0.7) +
    #   
    #   # geom_sf(data = assentamentos,
    #   #         aes(fill = "#d96e0a"),
    #   #         # fill = "#d96e0a",
    #   #         size = 1.3,
    #   #         color = NA,
    #   #         show.legend = "polygon",
    #   #         alpha = 0.8)+
    #   
    #   # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    #   
    #   geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
    #   
    #   labs(fill = '') +
    #   labs(color = "") +
    #   
    #   scale_fill_manual(values = c('#21367d'='#21367d'
    #                                # "#d96e0a" ="#d96e0a"
    #                                # '#CFF0FF' = "#CFF0FF"
    #   ),
    #   labels = c('#21367d'=""
    #              # "#d96e0a"="Aglomerados\nSubnormais"
    #              # '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)"
    #   ))+
    #   
    #   scale_color_identity(labels = c("black" = "Área urbanizada\n(Mapbiomas 2021)",
    #                                   blue = ""), guide = "legend") +
    #   
    #   facet_wrap(~ cond, ncol = 2) +
    # 
    #   theme(
    #     # strip.text.x = element_text(size=rel(1.2)),
    #     # strip.background = element_blank(),
    #     # panel.background = element_rect(fill = NA, colour = NA),
    #     # legend.background = element_blank(),
    #     axis.text = element_blank(),
    #     axis.title = element_blank(),
    #     axis.ticks = element_blank(),
    #     
    #     panel.grid = element_blank()
    #     # plot.margin=unit(c(2,0,0,0),"mm"),
    #     # legend.key.width=unit(2,"line"),
    #     # legend.key.height = unit(.5,"cm"),
    #     # legend.text=element_text("Tipo", size=rel(1)),
    #     # legend.title=element_text(size=rel(1)),
    #     # plot.title = element_text(hjust = 0, vjust = 4),
    #     # strip.text = element_text(size = 10),
    #     # legend.position = c(0.25, 0.15),
    #     # legend.spacing.y = unit(0.1, 'cm'),
    #     # legend.margin = margin(-0.45,0,0,0, unit="cm")
    #     # legend.margin = margin(t = -80)
    #   ) +
    #   aproxima_muni(sigla_muni = sigla_muni)
    #   # guides(fill = guide_legend(byrow = TRUE))
    # # width = 16; height = 16
    # # map_empregos
    # # map_frequencias
    # 
    # ggsave(map_frequencias,
    #        device = "png",
    #        filename =  sprintf("../data/map_plots_transports/muni_%s/13-frequencias_%s.png", sigla_muni, sigla_muni),
    #        dpi = 400,
    #        width = width, height = height, units = "cm" )
    
    BrBG <- c("#003c30","#01665e" , "#35978f" , "#80cdc1", "#c7eae5", "#f5f5f5","#f6e8c3",
              "#dfc27d", "#bf812d","#8c510a", "#543005")
    
    
    
    
    map_frequencias2 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      geom_sf(data = st_transform(recorte_rr_ntad, 3857),
              aes(#fill = "ntad",
                  color = "ntad"),
              # color = "ntad",
              fill = "grey40",
              alpha=1,
              size = 0)+
      scale_fill_manual(name = "",
                        values = c("ntad" = "grey40"),
                        label = c("ntad" = "População não atendidada")
                        
      ) +
      
      
      ggnewscale::new_scale_fill() +
      geom_sf(data = st_transform(frequencias2, 3857),
              aes(fill = headway_medio),
              # fill = '#21367d',
              # fill = NA,
              color = NA,
              alpha = 1,
              size = 1) +
      viridis::scale_fill_viridis(option = "rocket", direction = -1, name = "Headway médio (min)"
                                  
      ) +
      
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "grey45"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.8,
              alpha= 0.7)  +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "grey70"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      
      # ggnewscale::new_scale_color() +
      
      
      
      
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")

      
      geom_sf(data = st_transform(aglomerados_natend300, 3857),
              aes(color = '#0f805e'),
              # color = '#0f805e',
              # color = NA,
              # fill = "#0f805e",
              fill = NA,
              alpha = 0.6,
              linewidth = 1.0) +
      scale_color_manual(name = "Uso do solo",
                         values = c("grey45" = "grey45",
                                    "grey70" = "grey70",
                                    "#0f805e" = "#0f805e",
                                    "#FBFEA2" = "#FBFEA2",
                                    "ntad" = "grey40"),
                         label = c("grey45" = "Área urbanizada",
                                   "grey70" = "Áreas de planejamento",
                                   "#0f805e" = "Aglomerados subnormais",
                                   "#FBFEA2" = "População não atendidada",
                                   "ntad" = "População não atendido")
      )+
      

      # 
      # scale_color_manual(name = "Aglomerados subnormais",
      #                    values = c("#0f805e" = "#0f805e"),
      #                    label = c("#0f805e" = "Palmas")
      # )+
      
      # scale_fill_viridis_b() +
      
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
    
    # geom_sf(data = assentamentos,
    #         # aes(fill = "#d96e0a"),
    #         aes(fill = "#5766cc"),
    #         
    #         # fill = "#d96e0a",
    #         size = 1.3,
    #         color = NA,
    #         show.legend = "polygon",
    #         alpha = 0.9)+
    
    # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
    #         color = NA,alpha = .7, linewidth = 1) +
    
    
    

      
      
      # scale_fill_viridis_c(direction = -1,option = "inferno", name = "População não atendida") +
      # scale_fill_manual(values = c("1" = "#FEF8ED",
      #                              
      #                              "2" = "#FED49A",
      #                              "3" = "#FDA065",
      #                              "4" = "#D96542",
      #                              
      #                              # "#33b099" = "#33b099",
      #                              "#5766cc" = "#5766cc"),
    #                   label = c("1" = "25% menos populosos",
    #                             
    #                             "2" = "25% a 50% menos populosos",
    #                             "3" = "25% a 50% mais populosos",
    #                             "4" = "25% mais populosos",
    #                             # "#33b099" = "Cobertura de 300m",
    #                             "#5766cc" = "Aglomerados subnormais")) +
    # labs(fill = "População") +
      # ggnewscale::new_scale_color() +
      
      
      
      
      
      # 
      # scale_color_manual(values = c("grey45" = "grey45",
      #                               '#0f805e' = '#0f805e'),
      #                    label = c("grey45" = "Área Urbanizada\n(Mapa Biomas (2021))",
      #                              '#0f805e' = "Ciclovias")) +
    # ggsn::scalebar(dados_ciclovias_buffer, dist = 5, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84')
    
    # scale_fill_manual(values = '#d96e0a',
    #                   label = "Aglomerados\nSubnormais") +
    # labs(fill = '') +
    # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tr") +
      # geom_sf(data = assentamentos,
      #         aes(colour = "white"),
      #         fill = NA,
      #         size = 1.3)+
      # scale_fill_manual(values = c('#33b099'='#33b099',"#d96e0a" ="#d96e0a", '#CFF0FF' = "#CFF0FF"),
      #                   labels = c('#33b099'="Cobertura de 300m","#d96e0a"="Aglomerados\nSubnormais",
      #                              '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)")
      # ) +
      # 
      # scale_color_identity(labels = c("#21367d" = "",
      #                                 blue = ""), guide = "legend") +
    # labs(color = "Área urbanizada\n(Mapbiomas 2021)")+
    # tema_populacao()
    theme(
      strip.text.x = element_text(size=rel(1.2)),
      strip.background = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.margin = margin(unit(c(10,10,5,10),"mm")),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(1,"line"),
      legend.key = element_blank(),
      legend.text=element_text(size=25, family = "encode_sans_light"),
      legend.title=element_text(size=30, family = "encode_sans_bold"),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.22, 0.30),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
      #                                      colour = "#E0DFE3"),
      legend.spacing.y = unit(0.4, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("white", "white", "white", "grey40"))))
    
    
    ggsave(map_frequencias2,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/15-frequencias_buffer_%s.png", sigla_muni, sigla_muni),
           dpi = 250,
           width = width, height = height, units = "cm" )
    
    
    
    
    

# Mapa das regioes da cidade ----------------------------------------------

    
    map_areas <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      # geom_sf(data = st_transform(recorte_rr_ntad, 3857),
      #         aes(#fill = "ntad",
      #           color = "ntad"),
      #         # color = "ntad",
      #         fill = "grey40",
      #         alpha=1,
      #         size = 0)+
      # scale_fill_manual(name = "",
      #                   values = c("ntad" = "grey40"),
      #                   label = c("ntad" = "População não atendidada")
      #                   
      # ) +
      
      
      # ggnewscale::new_scale_fill() +
      # geom_sf(data = st_transform(frequencias2, 3857),
      #         aes(fill = headway_medio),
      #         # fill = '#21367d',
      #         # fill = NA,
      #         color = NA,
      #         alpha = 1,
      #         size = 1) +
      # viridis::scale_fill_viridis(option = "rocket", direction = -1, name = "Headway médio (min)"
      #                             
      # ) +
      
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "grey45"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.8,
              alpha= 0.7)  +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "grey70"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 1,
              alpha= 0.7) +
      geom_sf_label(data = dados_areas %>% st_transform(3857), aes(label = REGIAO))+
      
      
      # ggnewscale::new_scale_color() +
      
      
      
      
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      
    
    geom_sf(data = st_transform(assentamentos, 3857),
            aes(color = '#0f805e'),
            # color = '#0f805e',
            # color = NA,
            # fill = "#0f805e",
            fill ='#0f805e',
            alpha = 0.6,
            linewidth = 1.0) +
      scale_color_manual(name = "Uso do solo",
                         values = c("grey45" = "grey45",
                                    "grey70" = "grey70",
                                    "#0f805e" = "#0f805e",
                                    "#FBFEA2" = "#FBFEA2"),
                         label = c("grey45" = "Área urbanizada",
                                   "grey70" = "Áreas de planejamento",
                                   "#0f805e" = "Aglomerados subnormais")
      )+
      
      
      # 
      # scale_color_manual(name = "Aglomerados subnormais",
      #                    values = c("#0f805e" = "#0f805e"),
      #                    label = c("#0f805e" = "Palmas")
      # )+
      
      # scale_fill_viridis_b() +
      
      # labs(color = 'Infraestrutura Cicloviária',
    #      fill = 'População') +
    
    # geom_sf(data = assentamentos,
    #         # aes(fill = "#d96e0a"),
    #         aes(fill = "#5766cc"),
    #         
    #         # fill = "#d96e0a",
    #         size = 1.3,
    #         color = NA,
    #         show.legend = "polygon",
    #         alpha = 0.9)+
    
    # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
    #         color = NA,alpha = .7, linewidth = 1) +
    
    
    
    
    
    
    # scale_fill_viridis_c(direction = -1,option = "inferno", name = "População não atendida") +
    # scale_fill_manual(values = c("1" = "#FEF8ED",
    #                              
    #                              "2" = "#FED49A",
    #                              "3" = "#FDA065",
    #                              "4" = "#D96542",
    #                              
    #                              # "#33b099" = "#33b099",
    #                              "#5766cc" = "#5766cc"),
    #                   label = c("1" = "25% menos populosos",
    #                             
    #                             "2" = "25% a 50% menos populosos",
    #                             "3" = "25% a 50% mais populosos",
    #                             "4" = "25% mais populosos",
    #                             # "#33b099" = "Cobertura de 300m",
    #                             "#5766cc" = "Aglomerados subnormais")) +
    # labs(fill = "População") +
    # ggnewscale::new_scale_color() +
    
    
    
    
    
    # 
    # scale_color_manual(values = c("grey45" = "grey45",
    #                               '#0f805e' = '#0f805e'),
    #                    label = c("grey45" = "Área Urbanizada\n(Mapa Biomas (2021))",
    #                              '#0f805e' = "Ciclovias")) +
    # ggsn::scalebar(dados_ciclovias_buffer, dist = 5, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84')
    
    # scale_fill_manual(values = '#d96e0a',
    #                   label = "Aglomerados\nSubnormais") +
    # labs(fill = '') +
    # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tr") +
      # geom_sf(data = assentamentos,
      #         aes(colour = "white"),
      #         fill = NA,
      #         size = 1.3)+
      # scale_fill_manual(values = c('#33b099'='#33b099',"#d96e0a" ="#d96e0a", '#CFF0FF' = "#CFF0FF"),
      #                   labels = c('#33b099'="Cobertura de 300m","#d96e0a"="Aglomerados\nSubnormais",
      #                              '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)")
      # ) +
      # 
      # scale_color_identity(labels = c("#21367d" = "",
      #                                 blue = ""), guide = "legend") +
    # labs(color = "Área urbanizada\n(Mapbiomas 2021)")+
    # tema_populacao()
    theme(
      strip.text.x = element_text(size=rel(1.2)),
      strip.background = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.margin = margin(unit(c(10,10,5,10),"mm")),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(1,"line"),
      legend.key = element_blank(),
      legend.text=element_text(size=25, family = "encode_sans_light"),
      legend.title=element_text(size=30, family = "encode_sans_bold"),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.22, 0.30),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
      #                                      colour = "#E0DFE3"),
      legend.spacing.y = unit(0.4, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#0f805e", "white", "white"))))
    
    
    ggsave(map_areas,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/16-areas_%s.png", sigla_muni, sigla_muni),
           dpi = 250,
           width = width, height = height, units = "cm" )
    
    
    
    
    
    
    
    
    # map_frequencias2 <- ggplot() +
    #   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   coord_equal() +
    #   scale_fill_identity()+
    #   # nova escala
    #   new_scale_fill() +
    #   
    #   # geom_sf(data = simplepolys %>% st_transform(3857),
    #   #         # aes(size = 2),
    #   #         # aes(color = "#21367d"),
    #   #         aes(color = 'black'),
    #   #         # fill = NA,
    #   #         # stroke = 2,
    #   #         # size = 2,
    #   #         linewidth = 0.3,
    #   #         alpha= 0.8)  +
    #   # theme_map() +
    # geom_sf(data = st_transform(frequencias2, 3857),
    #         aes(fill = headway_medio),
    #         # fill = '#21367d',
    #         # fill = NA,
    #         color = NA,
    #         alpha = 1,
    #         size = 1) +
    #   
    #   # geom_sf(data = assentamentos,
    #   #         aes(fill = "#d96e0a"),
    #   #         # fill = "#d96e0a",
    #   #         size = 1.3,
    #   #         color = NA,
    #   #         show.legend = "polygon",
    #   #         alpha = 0.8)+
    #   
    #   # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    #   
    # geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
    #   
    #   labs(fill = 'Headway\nMédio (min)') +
    #   labs(color = "") +
    #   
    #   # scale_fill_manual(values = c('#21367d'='#21367d'
    #   #                              # "#d96e0a" ="#d96e0a"
    #   #                              # '#CFF0FF' = "#CFF0FF"
    #   # ),
    #   # labels = c('#21367d'=""
    #   #            # "#d96e0a"="Aglomerados\nSubnormais"
    #   #            # '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)"
    #   # ))+
    #   # scale_fill_gradientn(
    #   #   name = "Nº de\nEscolas",
    #   #   colors = BrBG ,
    #   #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    #   #   # values = NULL,
    #   #   space = "Lab",
    #   #   na.value = NA,
    #   #   # guide = "colourbar",
    #   #   aesthetics = "fill",
    #   #   # labels = scales::label_number(accuracy = 1)
    #   #   # colors
    #   # )
    #   # scale_fill_distiller(pallete = "BrBG") +
    # viridis::scale_fill_viridis(option = "rocket",
    #                             direction = -1
    #                             # , limits = c(0, 0.72)
    #                             # , breaks = c(0.001, 0.35, 0.7)
    #                             # , labels = c(0, "35", "70%")
    # ) +
    # 
    #   # scale_color_identity(labels = c("black" = "Área urbanizada\n(Mapbiomas 2021)",
    #                                   # blue = ""), guide = "legend") +
    #   
    #   # facet_wrap(~ cond, ncol = 2) +
    #   
    #   theme(
    #     strip.text.x = element_text(size=rel(1)),
    #     legend.direction = "horizontal",
    #     strip.background = element_blank(),
    #     panel.background = element_rect(fill = NA, colour = NA),
    #     legend.background = element_blank(),
    #     axis.text = element_blank(),
    #     axis.title = element_blank(),
    #     axis.ticks = element_blank(),
    #     
    #     panel.grid = element_blank(),
    #     plot.margin=unit(c(2,0,0,0),"mm"),
    #     legend.key.width=unit(1,"line"),
    #     legend.key.height = unit(.3,"cm"),
    #     legend.text=element_text("Tipo", size=rel(1)),
    #     legend.title=element_text(size=rel(1)),
    #     plot.title = element_text(hjust = 0, vjust = 4),
    #     strip.text = element_text(size = 10),
    #     legend.position = c(0.25, 0.1),
    #     # legend.spacing.y = unit(0.1, 'cm'),
    #     # legend.margin = margin(-0.45,0,0,0, unit="cm"),
    #     # legend.margin = margin(t = -80)
    #   ) +
    #   aproxima_muni(sigla_muni = sigla_muni)
    # # map_frequencias2
    # # guides(fill = guide_legend(byrow = TRUE))
    # # width = 16; height = 16
    # # map_empregos
    # # map_frequencias
    # 
    # ggsave(map_frequencias2,
    #        device = "png",
    #        filename =  sprintf("../data/map_plots_transports/muni_%s/14-frequencias_buffer_%s.png", sigla_muni, sigla_muni),
    #        dpi = 400,
    #        width = width, height = height, units = "cm" )
    
    

# Cleveland de Frequencia -------------------------------------------------

    # dados_hex_intersect_freq <- dados_hex %>%
    #   st_as_sf() %>%
    #   st_transform(decisao_muni$epsg) %>%
    #   st_intersection(frequencias2)
    # dados_hex_intersect_freq <- dados_hex_intersect_freq %>%
    #   mutate(area = st_area(.))
    # 
    # # mapview(dados_hex_intersect_freq)
    # dados_hex_intersect_freq <- dados_hex_intersect_freq$id_hex %>% unique()
    
    recorte_rr <- data_micro2 %>%
      # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
      left_join(frequencias2 %>% st_drop_geometry(), by = c("hex"="id_hex")) %>%
      drop_na(headway_medio) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4))%>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      group_by(cor, quintil_renda, genero) %>%
      summarise(sd = sd(headway_medio),
                headway_medio = mean(headway_medio, na.rm =T),
                n = n()) %>%
      mutate(id = paste(genero, cor))
    
    
    
    # sd(teste$headway_medio)
    # recorte_rr <- data_micro2 %>%
    #   # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
    #   left_join(frequencias2 %>% st_drop_geometry(), by = c("hex"="id_hex")) %>%
    #   # mutate(total = "total") %>% 
    #   mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
    #          cor = case_when(cor == "pard_am_ing" ~ "Pretos",
    #                          cor == "pretos" ~ "Pretos",
    #                          cor == "brancos" ~ "Brancos")) %>%
    #   drop_na(headway_medio) %>%
    #   mutate(
    #     quintil_renda = ntile(Rend_pc, 4)) %>%
    #   group_by(cor, quintil_renda, genero) %>%
    #   summarise(n = n(),
    #             headway_medio = mean(headway_medio, na.rm =T),
    #             headway_sd = sd(headway_medio)) %>% 
    #   # ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
    #   # summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"],
    #   #           headway_medio = mean(headway_medio, na.rm = T)) %>%
    #   # mutate(class = paste(genero, cor))%>%
    #   mutate(id = paste(class, quintil_renda))
    
    
    
    plot_cleveland_headway <- ggplot(recorte_rr, aes(headway_medio, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = id, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003"),
      )+
      scale_size_continuous( range = c(0,10),
                             limits = c(1,20000),
                             breaks = c(0,1000,5000,20000),
                             name = "Habitantes",
                             guide = "legend")
    p_head_bus <- plot_cleveland_headway + scale_color_manual(name = "Gênero e Cor",
                                                                values = c("Homens Brancos"="grey70",
                                                                           "Homens Pretos"="#FFB578",
                                                                           "Mulheres Brancos" = "black",
                                                                           "Mulheres Pretos"="#cc3003"),
                                                                labels = c("Homens Brancos",
                                                                           "Homens Pretos",
                                                                           "Mulheres Brancas",
                                                                           "Mulheres Pretas"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "Headway médio por recortes")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      # theme_minimal() +
      xlab("Headway médio (min)") +
      ylab("Quartil de renda per capita") +
      scale_x_continuous(labels = scales::number,
                         limits = c(20,30),
                         breaks = seq(0,30, 2.5)) +
      scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
      theme(#axis.title = element_blank(),
        panel.grid.minor = element_line(),
        legend.background = element_blank(),
        panel.background = element_blank(),
        legend.direction = "vertical",
        legend.position = "bottom",
        text = element_text(family = "sans",
                            # face = "bold",
                            size = 20),
        
        plot.title = element_text(size = 20, margin = margin(b=10), family = "encode_sans_bold"),
        plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 18, margin = margin(t=10), color = "grey70", hjust = 0),
        legend.title = element_text(size = 20, family = "encode_sans_bold"),
        legend.text = element_text(size = 16, family = "encode_sans_light"),
        axis.text = element_text(size = 16, family = "encode_sans_light"),
        axis.title = element_text(size = 18, family = "encode_sans_bold"))
    
    
    
    #escrita do gráfico
    
    ggsave(p_head_bus,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/16-linhasbus_freq_cleveland_%s.png", sigla_muni, sigla_muni),
           dpi = 200,
           width = 15, height = 10, units = "cm" )
    
    
    

    
    
    
  }
  
  # 2. Aplica funcao ------------------------------------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=faz_grafico_e_salva)
  
  
}