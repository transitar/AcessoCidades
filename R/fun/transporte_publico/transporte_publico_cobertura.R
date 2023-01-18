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
sigla_muni <- 'pal'


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
    # mapview(dados_peds)
    
    dados_peds <- dados_peds %>% st_transform(decisao_muni$epsg)
    dados_linhas <- dados_linhas %>% st_transform(decisao_muni$epsg)
    
    dados_peds_buffer_300 <- dados_peds %>% st_buffer(300) %>% st_union() %>% st_as_sf()
    dados_peds_buffer_500 <- dados_peds %>% st_buffer(500) %>% st_union() %>% st_as_sf()
    
    # mapview(dados_peds_buffer_300)
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
    
    dados_areas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                   sigla_muni, sigla_muni), layer= "areas") %>% st_transform(decisao_muni$epsg)
    
    assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                                      sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
      mutate(title = "Assentamentos Precários")
    # mapview(assentamentos)
    
    dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                        sigla_muni, sigla_muni))
    data_micro2 <- data_micro %>%
      # filter(code_tract %in% lista_tract) %>%
      select(1:12, V0606, hex) %>%
      mutate(V0606 = as.factor(V0606))
    
    
    #remocão dos habitantes de cor amarela e indígena
    levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
    data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
    
    pop_counts <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(pop_total = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% mutate(quintil = as.factor(ntile(pop_total, 4)))
    
    
    
    # mapview(area_natend300)
    
    #mapa localização
    cores_ciclo <- c('#5766cc', '#33b099', '#d96e0a')
    
    #área não urbana
    
    n_urb <- st_difference(st_transform(data_contorno, decisao_muni$epsg),
                           st_transform(simplepolys, decisao_muni$epsg))
    # mapview(n_urb)


# MAPA DAS LINHAS TP ------------------------------------------------------

    
    
    #colocar os bairros
    
    map_linhas_tp <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              fill = "#d8faf0",
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.1,
              alpha= 0.8)  +
      
      new_scale_fill() +

      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
                                                color = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      # new_scale_color() +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "aglomerados"),
              
              fill = "#0f805e",
              size = 1.3,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      # geom_sf(data = st_transform(dados_linhas, 3857),
      #         aes(color = 'linhas'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         alpha = 1,
      #         linewidth = 0.4) +
      # 
      # scale_color_manual(name = "Infraestrutura de Transporte Público",
      #                    values = c("linhas" = "#2B6CB0"),
      #                    label = c("linhas" = "Linhas de Transporte Público")
      # )+
      
      
      
    #0f805e verde linhas
      
      # ggnewscale::new_scale_color() +
      
      
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              linetype = "solid",
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7) +
      
      # geom_sf(data = simplepolys %>% st_transform(3857),
      #         # aes(size = 2),
      #         aes(color = "urb"),
      #         # color = "grey45",
      #         # aes(fill = '#CFF0FF'),
      #         linetype = "solid",
      #         fill = NA,
      #         # stroke = 2,
      #         # size = 2,
      #         linewidth = 0.4,
      #         alpha= 0.7)  +
      

      
      # scale_color_manual(name = "Uso do solo",
      #                    values = c("urb" = "grey45",
      #                               "areas" = "grey25"),
      #                    label = c("urb" = "Área Urbanizada",
      #                              "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)])
      # )+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c("urb" = "#d8faf0",
                                    "areas" = "grey25",
                                    "aglomerados" = "#0f805e"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      #8F040E vermelho bordas
      # geom_sf(data = dados_areas %>% st_transform(3857),
      #         # aes(size = 2),
      #         color = "white",
      #         # color = "grey45",
      #         # aes(fill = '#CFF0FF'),
      #         fill = NA,
      #         linetype = "solid",
      #         # stroke = 2,
      #         # size = 2,
      #         linewidth = 0.5,
      #         alpha= 0.7) +
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e")
                                                 # colour = c("grey25", "white", "white")
                                                 ))
        # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
        #                     order = 1)
        ) +
      # ggnewscale::new_scale_color() +
      ggnewscale::new_scale_color() +
      # ggnewscale::new_scale_fill() +
      
      geom_sf(data = st_transform(dados_linhas, 3857),
              aes(color = 'linhas'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              linewidth = 0.4) +
      
      scale_color_manual(name = "Infraestrutura de Transporte Público",
                         values = c("linhas" = "#2B6CB0"),
                         label = c("linhas" = "Linhas de Transporte Público")
      )+
      
      
      
      
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
      legend.position = c(0.25, 0.30),
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
      guides(#fill = guide_legend(byrow = TRUE),
             color = guide_legend(override.aes = list(fill = c("white")))) +
      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/5-linhas_tp_%s_new3.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )
    
    
    
    
    
    if (sigla_muni == "pal" | sigla_muni == "dou"){
    
    map_linhas_tp_sem_zoom <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              fill = "#d8faf0",
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.1,
              alpha= 0.8)  +
      
      new_scale_fill() +
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
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
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
                                                color = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "aglomerados"),
              
              fill = "#0f805e",
              size = 1.3,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              linetype = "solid",
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7) +
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c("urb" = "#d8faf0",
                                    "areas" = "grey25",
                                    "aglomerados" = "#0f805e"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e")
                                                  # colour = c("grey25", "white", "white")
        )))+
      
      ggnewscale::new_scale_color() +
      
      geom_sf(data = st_transform(dados_linhas, 3857),
              aes(color = 'linhas'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              linewidth = 0.4) +
      
      scale_color_manual(name = "Infraestrutura de Transporte Público",
                         values = c("linhas" = "#2B6CB0"),
                         label = c("linhas" = "Linhas de Transporte Público")
      )+
      
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
      legend.position = c(0.26, 0.44),
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
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white")))) +
      aproxima_muni_zoom(sigla_muni = sigla_muni)
    
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
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              fill = "#d8faf0",
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.1,
              alpha= 0.8)  +
      
      new_scale_fill() +
      
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
                                                color = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "aglomerados"),
              
              fill = "#0f805e",
              size = 1.3,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              linetype = "solid",
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7) +
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c("urb" = "#d8faf0",
                                    "areas" = "grey25",
                                    "aglomerados" = "#0f805e"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e")
                                                  # colour = c("grey25", "white", "white")
        ),
        order = 2)) +
      
      ggnewscale::new_scale_color() +
      
      geom_sf(data = st_transform(dados_peds_buffer_300, 3857),
              aes(color = 'buffer300'),
              # color = '#0f805e',
              # color = NA,
              fill = "grey70",
              alpha = 0.7,
              linewidth = 0.4) +
      
      scale_color_manual(name = "Infraestrutura de Transporte Público",
                         values = c("buffer300" = "#2B6CB0"),
                         label = c("buffer300" = "Cobertura de 300m")
      )+
      
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
      legend.position = c(0.25, 0.30),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.box.just = "left"
    ) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("grey70")),
                             order = 3)) +
      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp_buffer,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/6-linhas_tp_buffer_300m_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )
    
    

# Mapa de Buffer 300 sem zoom -------------------------------------------------

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
              linewidth = 0.5) +
      
      scale_color_manual(name = "Infraestrutura de Transporte Público",
                         values = c("#0f805e" = "#0f805e"),
                         label = c("#0f805e" = "Cobertura de 300m")
      )+
      ggnewscale::new_scale_color() +
      geom_sf(data = simplepolys %>% st_transform(3857),
              aes(color = "grey45"),
              fill = NA,
              linewidth = 0.5,
              alpha= 0.7)  +
      scale_color_manual(name = "Área Urbanizada",
                         values = c("grey45" = "grey45"),
                         label = c("grey45" = "Dourados")
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
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey60", linewidth = .5) +
      
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
        legend.position = c(0.26, 0.44),
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
      )+
      guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni_zoom(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp_buffer,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/6-linhas_tp_buffer_300m_%s_sem_zoom.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )
    
    
    
    

# MAPA PEDS BUFFER 500M ----------------------------------------------------------

    map_linhas_tp_buffer_500 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              fill = "#d8faf0",
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.1,
              alpha= 0.8)  +
      
      new_scale_fill() +
      
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
                                                color = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "aglomerados"),
              
              fill = "#0f805e",
              size = 1.3,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              linetype = "solid",
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7) +
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c("urb" = "#d8faf0",
                                    "areas" = "grey25",
                                    "aglomerados" = "#0f805e"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e")
                                                  # colour = c("grey25", "white", "white")
        ),
        order = 2)) +
      
      ggnewscale::new_scale_color() +
      
      geom_sf(data = st_transform(dados_peds_buffer_500, 3857),
              aes(color = 'buffer500'),
              # color = '#0f805e',
              # color = NA,
              fill = "grey70",
              alpha = 0.7,
              linewidth = 0.4) +
      
      scale_color_manual(name = "Infraestrutura de Transporte Público",
                         values = c("buffer500" = "#2B6CB0"),
                         label = c("buffer500" = "Cobertura de 500m")
      )+
      
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
        legend.position = c(0.25, 0.30),
        legend.box.background = element_rect(fill=alpha('white', 0.7),
                                             colour = "#A09C9C",
                                             linewidth = 0.8,
                                             linetype = "solid"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.box.just = "left"
      ) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("grey70")),
                             order = 3)) +
      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp_buffer_500,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/7-linhas_tp_buffer_500m_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )

# Mapa buffer 500 sem zoom ------------------------------------------------
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
              linewidth = 0.5) +
      
      scale_color_manual(name = "Infraestrutura de Transporte Público",
                         values = c("#0f805e" = "#0f805e"),
                         label = c("#0f805e" = "Cobertura de 500m")
      )+
      ggnewscale::new_scale_color() +
      geom_sf(data = simplepolys %>% st_transform(3857),
              aes(color = "grey45"),
              fill = NA,
              linewidth = 0.5,
              alpha= 0.7)  +
      scale_color_manual(name = "Área Urbanizada",
                         values = c("grey45" = "grey45"),
                         label = c("grey45" = "Dourados")
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
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.5) +
      
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
        legend.position = c(0.26, 0.44),
        legend.box.background = element_rect(fill=alpha('white', 0.7),
                                             colour = "#A09C9C",
                                             linewidth = 0.8,
                                             linetype = "solid"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.box.just = "left"
      ) +
      guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni_zoom(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp_buffer_500,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/7-linhas_tp_buffer_500m_%s_sem_zoom.png", sigla_muni, sigla_muni),
           dpi = 300,
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
    options(scipen = 10000000)
    pop_max <- pop_max <- plyr::round_any(max(recorte_rr$n), 50000, f = ceiling)
    break_max <- pop_max
    break_leap <- break_max/4
    escala <- ifelse(pop_max > 150000, 12, ifelse( pop_max > 100000, 12, ifelse(pop_max > 50000, 12, 12)))
    
    range_tot <- max(recorte_rr$prop)-min(recorte_rr$prop)
    extend <- 0.01
    
    if (range_tot <= 0.01){
      passo <- 0.005
    } else if (range_tot <= 0.05) {
      passo <- 0.01
    } else if (range_tot <= 0.10){
      passo <- 0.025
    } else if (range_tot <= 25){
      passo <- 0.05
    } else {
      passo <- 0.5
    }
    
    range1 <- plyr::round_any(ifelse( (min(recorte_rr$prop) - extend)<0, 0, min(recorte_rr$prop) - extend),
                              passo,
                              f=floor)
    range2 <- plyr::round_any(ifelse( (max(recorte_rr$prop) + extend)>1, 1, max(recorte_rr$prop) + extend),
                              passo,
                              f=ceiling)
    
    plot_cleveland_bus300 <- ggplot(recorte_rr, aes(prop, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#ADBEF0", "#174DE8", "#EBB814", "#B39229"),
      )+
      scale_size_continuous( range = c(0,escala),
                             limits = c(0,break_max),
                             breaks = c(break_leap,break_leap*2,break_leap*3),
                             name = "Habitantes",
                             guide = "legend")
    p_b300_bus <- plot_cleveland_bus300 + scale_color_manual(name = "Gênero e Cor",
                                                           values = c("Homens Brancos"="#ADBEF0",
                                                                      "Homens Pretos"="#174DE8",
                                                                      "Mulheres Brancos" = "#EBB814",
                                                                      "Mulheres Pretos"="#B39229"),
                                                           labels = c("Homens Brancos"="Homens Brancos",
                                                                      "Homens Pretos"= "Homens Negros",
                                                                      "Mulheres Brancos"="Mulheres Brancas",
                                                                      "Mulheres Pretos"="Mulheres Negras"))+
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
                         limits = c(range1,range2),
                         breaks = seq(range1,range2, passo)) +
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
           filename =  sprintf("../data/map_plots_transports/muni_%s/11-linhasbus_300_cleveland_%s_new2.png", sigla_muni, sigla_muni),
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
      filter(quintil_renda %in% c(1,2,3,4)) %>%
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
    
    # ceiling(max(recorte_rr_ntad$n))
    pop_max_ntad <- plyr::round_any(max(recorte_rr_ntad2$n), 10^(n_int_digits(max(recorte_rr_ntad2$n))), f = ceiling)
    break_max_ntad <- pop_max_ntad
    break_leap_ntad <- break_max_ntad/4
    escala_ntad <- ifelse(pop_max_ntad > 150000, 12, ifelse( pop_max_ntad > 100000, 12, ifelse(pop_max_ntad > 50000, 12, 12)))
    
    range_tot <- max(recorte_rr_ntad2$n)-min(recorte_rr_ntad2$n)
    extend_ntad <- 100
    
    if (range_tot <= 100){
      passo_ntad <- 50
    } else if (range_tot <= 500) {
      passo_ntad <- 100
    } else if (range_tot <= 1000){
      passo_ntad <- 250
    } else if (range_tot <= 5000){
      passo_ntad <- 500
    } else {
      passo_ntad <- 1000
    }
    
    range1_ntad <- plyr::round_any(ifelse( (min(recorte_rr_ntad2$n) - extend_ntad)<0, 0, min(recorte_rr_ntad2$n) - extend_ntad),
                                   passo_ntad,
                                   f = floor)
    
    range2_ntad <- plyr::round_any(ifelse( (max(recorte_rr_ntad2$n) + extend_ntad)>break_max_ntad, 1, max(recorte_rr_ntad2$n) + extend_ntad),
                                   passo_ntad,
                                   f = ceiling)
    
    plot_cleveland_bus300_ntad <- ggplot(recorte_rr_ntad2, aes(n, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("grey70", "#FFB578", "black", "#cc3003"),
      )+
      scale_size_continuous( range = c(0,escala_ntad),
                             limits = c(0,break_max_ntad),
                             breaks = c(break_leap_ntad,break_leap_ntad*2,break_leap_ntad*3),
                             name = "Habitantes",
                             guide = "legend")
    p_b300_bus_ntad <- plot_cleveland_bus300_ntad + scale_color_manual(name = "Gênero e Cor",
                                                                       values = c("Homens Brancos"="grey70",
                                                                                  "Homens Pretos"="#FFB578",
                                                                                  "Mulheres Brancos" = "black",
                                                                                  "Mulheres Pretos"="#cc3003"),
                                                                       labels = c("Homens Brancos"="Homens Brancos",
                                                                                  "Homens Pretos"= "Homens Negros",
                                                                                  "Mulheres Brancos"="Mulheres Brancas",
                                                                                  "Mulheres Pretos"="Mulheres Negras"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "População sem acesso à infraestrutura de transporte público")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      # theme_minimal() +
      xlab("Habitantes do recorte") +
      ylab("Quartil de renda per capita") +
      scale_x_continuous(labels = scales::number,
                         limits = c(range1_ntad,range2_ntad),
                         breaks = seq(range1_ntad,range2_ntad, passo_ntad)) +
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
           filename =  sprintf("../data/map_plots_transports/muni_%s/12-linhasbus_300_cleveland_nao_atendidos%s_new.png", sigla_muni, sigla_muni),
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
    
    
    recorte_rr_ntad_map <- recorte_rr_ntad %>%
      st_drop_geometry() %>%
      left_join(pop_counts, by = "hex") %>% st_as_sf()
    
    map_aglomerados_ntad <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = st_transform(recorte_rr_ntad_map, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
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
                                    "#0f805e" = "#0f805e"),
                         label = c("grey45" = "Área urbanizada",
                                   "grey70" = "Bairros",
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

      
      
    # viridis::scale_fill_viridis(option = "B",
    #                             direction = -1,
    #                             name = "População (hab)",
    #                             breaks = seq(1,3,1),
    #                             labels = paste(seq(1,3,1), "quartil"),
    #                             limits = c(1,3)
    #                             
    #                             ) +
      # scale_fill_gradientn(
      #   name = "Quintil de população",
      #   colors = viridis_magma_discrete,
      #   breaks = seq(1,3,1),
      #   labels = seq(1,3,1),
      #   limits = c(1,3),
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   # colors
      # ) +
      
    scale_fill_manual(name = "População",
                      values = c("1" = "#FEF8ED",
                                 
                                 "2" = "#FED49A",
                                 "3" = "#FDA065",
                                 "4" = "#D96542"),
                      label = c("1" = "25% menos populosos",
                                
                                "2" = "25% a 50% menos populosos",
                                "3" = "25% a 50% mais populosos",
                                "4" = "25% mais populosos")) +
      
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
      legend.spacing.y = unit(0.1, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      guides(fill = guide_legend(byrow = TRUE,
                                 order = 2)) +
      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_aglomerados_ntad,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/14-aglomerados_nao_atendidos_tp_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
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
    # mapview(frequencias2)

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
    
    
    cor_ntad <- "#957A03"
    
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
              fill = cor_ntad,
              alpha=1,
              size = 0)+

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
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.6,
              alpha= 0.7) +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.8,
              alpha= 0.7)  +
      
      
      
      # ggnewscale::new_scale_color() +
      
      
      
      
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")

      
      geom_sf(data = st_transform(aglomerados_natend300, 3857),
              aes(color = 'assentamentos'),
              # color = '#0f805e',
              # color = NA,
              # fill = "#0f805e",
              fill = NA,
              alpha = 0.6,
              linewidth = 1.0) +
      scale_color_manual(name = "Uso do solo",
                         breaks = c('urb', "areas", "assentamentos", "ntad"),
                         values = c("urb" = "grey50",
                                    "areas" = "#fefedf",
                                    "assentamentos" = "#0F805E",
                                    "ntad" = cor_ntad),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = "Áreas de planejamento",
                                   "assentamentos" = "Aglomerados subnormais",
                                   "ntad" = "População não atendida")
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
      guides(color = guide_legend(override.aes = list(fill = c("white", "white", "white", cor_ntad),
                                                      color = c("grey50", "#fdfc99", "#0F805E", cor_ntad))))
    #511277 roxo
    #142577 azul
    #957A03 amarelo queimado
    ggsave(map_frequencias2,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/15-frequencias_buffer_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
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
      geom_sf_label(data = dados_areas %>% st_transform(3857), aes(label = area))+
      
      
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
    showtext_auto()
    
    recorte_rr_h <- data_micro2 %>%
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
    
    pop_max_headway <-plyr::round_any(max(recorte_rr_h$n),10^(n_int_digits(max(recorte_rr_h$n))), f = ceiling)
    break_max_h <- pop_max_headway
    break_leap_h <- break_max_h/4
    escala_h <- ifelse(pop_max_headway > 150000, 12, ifelse( pop_max_headway > 100000, 12, ifelse(pop_max_headway > 50000, 12, 12)))
    
    range_tot <- max(recorte_rr_h$headway_medio)-min(recorte_rr_h$headway_medio)
    extend_h <- 0.01
    
    if (range_tot <= 0.5){
      passo_h <- 0.1
    } else if (range_tot <= 1) {
      passo_h <- 0.25
    } else if (range_tot <= 2.5){
      passo_h <- 0.5
    } else if (range_tot <= 5){
      passo_h <- 1
    } else if (range_tot <= 10) {
      passo_h <- 2.5
    } else {
      passo_h <- 5
    }
    
    # range1_h <- floor(ifelse( (min(recorte_rr_h$headway_medio) - extend_h)<0, 0, min(recorte_rr_h$headway_medio) - extend_h))
    
    range1_h <- plyr::round_any(ifelse( (min(recorte_rr_h$headway_medio) - extend_h)<0, 0, min(recorte_rr_h$headway_medio) - extend_h),
                                passo_h,
                                f = floor)
    
    # range2_h <- ceiling(ifelse( (max(recorte_rr_h$headway_medio) + extend_h)>break_max_h, 1, max(recorte_rr_h$headway_medio) + extend_h))
    
    range2_h <- plyr::round_any(ifelse( (max(recorte_rr_h$headway_medio) + extend_h)>break_max_h, 1, max(recorte_rr_h$headway_medio) + extend_h),
                                passo_h,
                                f = ceiling)
    
    plot_cleveland_headway <- ggplot(recorte_rr_h, aes(headway_medio, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = id, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003")
      )+
      scale_size_continuous( range = c(0,escala_h),
                             limits = c(0,break_max_h),
                             breaks = c(break_leap_h,break_leap_h*2,break_leap_h*3),
                             name = "Habitantes",
                             guide = "legend")
    p_head_bus <- plot_cleveland_headway + scale_color_manual(name = "Gênero e Cor",
                                                                values = c("Homens Brancos"="grey70",
                                                                           "Homens Pretos"="#FFB578",
                                                                           "Mulheres Brancos" = "black",
                                                                           "Mulheres Pretos"="#cc3003"),
                                                                labels = c("Homens Brancos"="Homens Brancos",
                                                                           "Homens Pretos"="Homens Negros",
                                                                           "Mulheres Brancos"="Mulheres Brancas",
                                                                           "Mulheres Pretos"="Mulheres Negras"))+
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
                         limits = c(range1_h,range2_h),
                         breaks = seq(range1_h,range2_h, passo_h)) +
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
    
    ggsave(p_head_bus,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/16-linhasbus_freq_cleveland_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 15, height = 10, units = "cm" )
    
    

# Tarifa - Espacial -------------------------------------------------------

   tarifa <- as.numeric(decisao_muni$tarifa) * 60
    sm <- 1302
    
    renda <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(renda = mean(Rend_pc*sm, na.rm =T)) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% drop_na(h3_resolution) %>%
      # mutate(renda = ifelse(renda>10, 10, renda))
      mutate(tarifa_renda = ifelse(renda == 0, 1, tarifa/renda)) %>%
      mutate(tarifa_renda2 = case_when(tarifa_renda <= 0.05 ~ "Até 5%",
                                       tarifa_renda <= 0.10 & tarifa_renda > 0.05 ~ "De 5% a 10%",
                                       tarifa_renda <= 0.15 & tarifa_renda > 0.10 ~ "De 10% a 15%",
                                       tarifa_renda <= 0.20 & tarifa_renda > 0.15 ~ "De 15% a 20%",
                                       tarifa_renda <= 0.25 & tarifa_renda > 0.20 ~ "De 20% a 25%",
                                       tarifa_renda <= 0.30 & tarifa_renda > 0.25 ~ "Entre 25% e 30%",
                                       tarifa_renda > 0.30  ~ "Acima de 30%"))
    
    renda2 <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(renda = mean(Rend_pc*sm, na.rm =T)) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% drop_na(h3_resolution) %>%
      # mutate(renda = ifelse(renda>10, 10, renda))
      mutate(tarifa_renda = ifelse(renda == 0, 1, tarifa/renda)) %>%
      mutate(tarifa_renda2 = case_when(tarifa_renda <= 0.05 ~ "Até 5%",
                                       tarifa_renda <= 0.15 & tarifa_renda > 0.05 ~ "De 5% a 15%",
                                       tarifa_renda <= 0.25 & tarifa_renda > 0.15 ~ "De 15% a 25%",
                                       tarifa_renda > 0.25  ~ "Acima de 25%"))

    # mapview(renda, zcol = "tarifa_renda")
    # verde 0E7F5D
    # azul #2B6CB0
    # cor_ag <- "#2B6CB0"
    # map_tarifa <- ggplot() +
    #   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   coord_equal() +
    #   scale_fill_identity()+
    #   # nova escala
    #   new_scale_fill() +
    # 
    #   geom_sf(data = st_transform(renda, 3857),
    #           aes(fill = tarifa_renda2),
    #           colour = NA,
    #           alpha=.8,
    #           size = 0)+
    # 
    #   geom_sf(data = dados_areas %>% st_transform(3857),
    #           aes(color = "areas"),
    #           fill = NA,
    #           linewidth = 0.7,
    #           alpha= 0.7) +
    # 
    # geom_sf(data = simplepolys %>% st_transform(3857),
    #         aes(color = "urb"),
    #         fill = NA,
    #         linewidth = 0.5,
    #         alpha= 0.7)  +
    #   
    #   geom_sf(data = assentamentos,
    #           aes(color = "ag"),
    #           linewidth = 0.3,
    #           fill = cor_ag,
    #           # fill = NA,
    #           show.legend = "polygon",
    #           alpha = 0.5)+
    #   
    #   scale_color_manual(name = "Uso do solo",
    #                      breaks = c("ag", "urb", "areas"),
    #                      values = c("urb" = "black",
    #                                 "areas" = "grey45",
    #                                 "ag" = cor_ag),
    #                      label = c("urb" = "Área urbanizada",
    #                                "areas" =  munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
    #                                "ag" = "Aglomerados subnormais")
    #   )+
    #   # "urb" = "#8F040E"
    #   
    #   # viridis::scale_fill_viridis(option = "A",
    #   #                             name = "Renda per capita (SM)",
    #   #                             breaks = seq(0,10,2),
    #   #                             labels = c("0","2", "4", "6","8", ">10"),
    #   #                             limits = c(0,10)) +
    #   # scale_fill_viridis_d(option = "plasma",
    #   #                      name = "% da renda",
    #   #                      direction = 1,
    #   #                      breaks = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #   #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"),
    #   #                      labels = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #   #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"))+
    # # colors_blue <- c("#ecfaff", "#ade7ff", "#73d6ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#0989c0", "#066891")
    # # cores escala paired
    # # c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#602F94", "#FFFF99", "#B15928")
    # # cores PuOr
    # # c("#B45709", "#E2810C", "#FEB863", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    # #cores BrBG com PuOr
    # # c("#02665E", "#369692", "#80CEC1", "#DEEBF7", "#B3ABD4", "#8171AF", "#54288A")
    # #cores RdYlBu com PuOr
    # # c("#F76D3F", "#FDAE61", "#FFE090", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    # #cores PuOu com mudan;a nos laranjas
    # # c("#DA8535", "#EDAF71", "#F5CDAA", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    # 
    # #escala azul e roxo da ivys
    # # c("#f0e1ee", "#e3c9df", "#873286", "#67127c", "#3c1a7d", "#1e1975", "#001459")
    # 
    # #escala ylOrRd 
    # # c("#FFEB8F", "#FDD263", "#FA792F", "#F83521", "#D90017", "#AD001D", "#521A08")
    # #escala carol oranges
    # # c("#FFDE59", "#FFBD59", "#FF914B", "#FF5C00", "#C10000", "#B0006A", "#6200B0")
    # #spectral
    # # c("#8A0033", "#EE5634", "#FB9E4F", "#FDDA79", "#E1F686", "#2974AF", "#4B3991")
    # 
    # #escala carola pastel
    # # c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080")
    # 
    # scale_fill_manual(name = "% da renda",
    #                   breaks = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                              "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"),
    #                   values = c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080"),
    #                   labels = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                              "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%")
    #                   ) +
    #   
    # # scale_fill_gradientn(
    # #     name = "% da renda",
    # #     colors = colors_purple ,
    # #     # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    # #     # values = NULL,
    # #     space = "Lab",
    # #     na.value = NA,
    # #     # guide = "colourbar",
    # #     aesthetics = "fill",
    # #     # colors
    # #   ) +
    # 
    # geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
    #   
    #   ggspatial::annotation_scale(style = "ticks",
    #                               location = "br",
    #                               text_family = "encode_sans_bold",
    #                               text_cex = 3,
    #                               line_width = 1,
    #                               width_hint = 0.10,
    #                               pad_x = unit(0.35, "cm"),
    #                               pad_y = unit(0.35, "cm")
    #   ) +
    #   ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tr") +
    # 
    # theme(
    #   strip.text.x = element_text(size=rel(1.2)),
    #   strip.background = element_blank(),
    #   panel.background = element_rect(fill = NA, colour = NA),
    #   axis.text = element_blank(),
    #   axis.title = element_blank(),
    #   axis.ticks = element_blank(), 
    #   panel.grid = element_blank(),
    #   plot.margin=unit(c(0,0,0,0),"mm"),
    #   legend.margin = margin(unit(c(10,10,5,10),"mm")),
    #   legend.key.width=unit(2,"line"),
    #   legend.key.height = unit(1,"line"),
    #   legend.key = element_blank(),
    #   legend.text=element_text(size=25, family = "encode_sans_light"),
    #   legend.title=element_text(size=30, family = "encode_sans_bold"),
    #   plot.title = element_text(hjust = 0, vjust = 4),
    #   strip.text = element_text(size = 10),
    #   legend.position = c(0.20, 0.33),
    #   legend.box.background = element_rect(fill=alpha('white', 0.7),
    #                                        colour = "#A09C9C",
    #                                        linewidth = 0.8,
    #                                        linetype = "solid"),
    #   legend.background = element_blank(),
    #   # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
    #   #                                      colour = "#E0DFE3"),
    #   legend.spacing.y = unit(0.4, 'cm'),
    #   legend.box.just = "left"
    #   # legend.margin = margin(t = -80)
    # ) +
    #   # guides(fill = guide_legend(byrow = TRUE)) +
    #   aproxima_muni(sigla_muni = sigla_muni)  +
    #   guides(color = guide_legend(override.aes = list(fill = c(cor_ag, "white", "white"),
    #                                                   alpha = c(0.5, rep(0.1,2))),
    #                               order = 1),
    #          fill = guide_legend(override.aes = list(fill = c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080"),
    #                                                  color = c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080")),
    #                              order = 2)
    #          )
    # 
    # 
    # 
    # 
    # suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    # 
    # ggsave(map_tarifa,
    #        device = "png",
    #        filename =  sprintf('../data/map_plots_transports/muni_%s/17-tarifa_renda_new8carol_pastel_%s.png',
    #                            sigla_muni,
    #                            sigla_muni),
    #        dpi = 300,
    #        width = width, height = height, units = "cm" )
    

# Mapa tarifa versal com menos classes ------------------------------------

    cor_ag <- "#2B6CB0"
    map_tarifa <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = st_transform(renda2, 3857),
              aes(fill = tarifa_renda2),
              colour = NA,
              alpha=.8,
              size = 0)+
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              aes(color = "areas"),
              fill = NA,
              linewidth = 0.7,
              alpha= 0.7) +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              aes(color = "urb"),
              fill = NA,
              linewidth = 0.5,
              alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              aes(color = "ag"),
              linewidth = 0.3,
              fill = cor_ag,
              # fill = NA,
              show.legend = "polygon",
              alpha = 0.5)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("ag", "urb", "areas"),
                         values = c("urb" = "grey25",
                                    "areas" = "grey45",
                                    "ag" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" =  munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      # "urb" = "#8F040E"
      
      # viridis::scale_fill_viridis(option = "A",
      #                             name = "Renda per capita (SM)",
      #                             breaks = seq(0,10,2),
      #                             labels = c("0","2", "4", "6","8", ">10"),
      #                             limits = c(0,10)) +
      # scale_fill_viridis_d(option = "plasma",
      #                      name = "% da renda",
      #                      direction = 1,
      #                      breaks = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"),
    #                      labels = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"))+
    # colors_blue <- c("#ecfaff", "#ade7ff", "#73d6ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#0989c0", "#066891")
    # cores escala paired
    # c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#602F94", "#FFFF99", "#B15928")
    # cores PuOr
    # c("#B45709", "#E2810C", "#FEB863", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    #cores BrBG com PuOr
    # c("#02665E", "#369692", "#80CEC1", "#DEEBF7", "#B3ABD4", "#8171AF", "#54288A")
    #cores RdYlBu com PuOr
    # c("#F76D3F", "#FDAE61", "#FFE090", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    #cores PuOu com mudan;a nos laranjas
    # c("#DA8535", "#EDAF71", "#F5CDAA", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    
    #escala azul e roxo da ivys
    # c("#f0e1ee", "#e3c9df", "#873286", "#67127c", "#3c1a7d", "#1e1975", "#001459")
    
    #escala ylOrRd 
    # c("#FFEB8F", "#FDD263", "#FA792F", "#F83521", "#D90017", "#AD001D", "#521A08")
    #escala carol oranges
    # c("#FFDE59", "#FFBD59", "#FF914B", "#FF5C00", "#C10000", "#B0006A", "#6200B0")
    #spectral
    # c("#8A0033", "#EE5634", "#FB9E4F", "#FDDA79", "#E1F686", "#2974AF", "#4B3991")
    
    #escala carola pastel
    # c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080")
    
    scale_fill_manual(name = "% da renda",
                      breaks = c("Até 5%", "De 5% a 15%", "De 15% a 25%", "Acima de 25%"),
                      values = c("#FFE8A4", "#E36C27", "#960C5F", "#470080"),
                      labels = c("Até 5%", "De 5% a 15%", "De 15% a 25%", "Acima de 25%")
    ) +
      
      # scale_fill_gradientn(
      #     name = "% da renda",
      #     colors = colors_purple ,
      #     # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #     # values = NULL,
      #     space = "Lab",
      #     na.value = NA,
      #     # guide = "colourbar",
      #     aesthetics = "fill",
      #     # colors
    #   ) +
    
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
        legend.position = c(0.20, 0.33),
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
      aproxima_muni(sigla_muni = sigla_muni)  +
      guides(color = guide_legend(override.aes = list(fill = c(cor_ag, "white", "white"),
                                                      alpha = c(0.5, rep(0.1,2))),
                                  order = 1),
             fill = guide_legend(override.aes = list(fill = c("#FFE8A4", "#E36C27", "#960C5F", "#470080"),
                                                     color = c("#FFE8A4", "#E36C27", "#960C5F", "#470080")),
                                 order = 2)
      )
    
    
    
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    
    ggsave(map_tarifa,
           device = "png",
           filename =  sprintf('../data/map_plots_transports/muni_%s/17-tarifa_renda_new_%s.png',
                               sigla_muni,
                               sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )    
    
    

# Cleveland de peso da tarifa ---------------------------------------------

    recorte_tarifa <- data_micro2 %>%
      filter(Rend_pc >0) %>%
      mutate(tarifa_renda = (tarifa)/(Rend_pc*1302)) %>%
      # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
        mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      group_by(cor, quintil_renda, genero) %>%
      summarise(tarifa_grupo = mean(tarifa_renda, na.rm = T),
                n = n()) %>%
      mutate(id = paste(genero, cor))
    
    
    pop_max_tarifa <-plyr::round_any(max(recorte_tarifa$n),10^(n_int_digits(max(recorte_tarifa$n))), f = ceiling)
    break_max_tarifa <- pop_max_tarifa
    break_leap_tarifa <- pop_max_tarifa/4
    escala_tarifa <- ifelse(pop_max_tarifa > 150000, 10, ifelse( pop_max_tarifa > 100000, 10, ifelse(pop_max_tarifa > 50000, 10, 10)))
    
    range_tot_tarifa <- max(recorte_tarifa$tarifa_grupo)-min(recorte_tarifa$tarifa_grupo)
    extend_tarifa <- 0.01
    
    if (range_tot_tarifa <= 0.005){
      passo_tarifa <- 0.001
    } else if (range_tot_tarifa <= 0.01) {
      passo_tarifa <- 0.0025
    } else if (range_tot_tarifa <= 0.025){
      passo_tarifa <- 0.005
    } else if (range_tot_tarifa <= 0.05) {
      passo_tarifa <- 0.01
    } else if (range_tot_tarifa <= 0.10) {
      passo_tarifa <- 0.025
    } else {
      passo_tarifa <- 0.10
    }
    
    range1_tarifa <- plyr::round_any(ifelse( (min(recorte_tarifa$tarifa_grupo) - extend_tarifa)<0, 0, min(recorte_tarifa$tarifa_grupo) - extend_tarifa),
                                passo_tarifa,
                                f = floor)
    
    range2_tarifa <- plyr::round_any(ifelse( (max(recorte_tarifa$tarifa_grupo) + extend_tarifa)>1, 1, max(recorte_tarifa$tarifa_grupo) + extend_tarifa),
                                passo_tarifa,
                                f = ceiling)
    
    plot_cleveland_tarifa <- ggplot(recorte_tarifa, aes(tarifa_grupo, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = id, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003")
      )+
      scale_size_continuous( range = c(0,escala_tarifa),
                             limits = c(0,break_max_tarifa),
                             breaks = c(break_leap_tarifa,break_leap_tarifa*2,break_leap_tarifa*3),
                             name = "Habitantes",
                             guide = "legend")
    p_tarifa <- plot_cleveland_tarifa + scale_color_manual(name = "Gênero e Cor",
                                                              values = c("Homens Brancos"="grey70",
                                                                         "Homens Pretos"="#FFB578",
                                                                         "Mulheres Brancos" = "black",
                                                                         "Mulheres Pretos"="#cc3003"),
                                                              labels = c("Homens Brancos"="Homens Brancos",
                                                                         "Homens Pretos"="Homens Negros",
                                                                         "Mulheres Brancos"="Mulheres Brancas",
                                                                         "Mulheres Pretos"="Mulheres Negras"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "% da renda mensal gasta para realizar 60 viagens*")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      labs(#subtitle = "por mês no transporte público",
           caption = paste0("*Considerando a tarifa do transporte público em ",
                            munis_list$munis_df$name_muni[which(munis_list$munis_df$abrev_muni==sigla_muni)]," de R$ ",
                                                          decisao_muni$tarifa))+
      # theme_minimal() +
      xlab("% da renda") +
      ylab("Quartil de renda per capita") +
      scale_x_continuous(labels = scales::percent,
                         limits = c(range1_tarifa,range2_tarifa),
                         breaks = seq(range1_tarifa,range2_tarifa, passo_tarifa)) +
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
        
        plot.title = element_text(size = 33, margin = margin(b=10), family = "encode_sans_bold"),
        plot.subtitle = element_text(size=20, color = "darkslategrey"
                                     # margin = margin(b = 25)
                                     ),
        plot.caption = element_text(size = 20,
                                    # margin = margin(t=10),
                                    color = "darkslategrey",
                                    hjust = 0,
                                    family = "encode_sans_light"),
        legend.title = element_text(size = 33, family = "encode_sans_bold"),
        legend.text = element_text(size = 27, family = "encode_sans_light"),
        axis.text = element_text(size = 27, family = "encode_sans_light"),
        axis.title = element_text(size = 33, family = "encode_sans_bold"))
    
    ggsave(p_tarifa,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/18-linhasbus_freq_cleveland_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 15, height = 10, units = "cm" )
    
    # mapview(renda, zcol = "tarifa_renda2")
    
  }
  
  # 2. Aplica funcao ------------------------------------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=faz_grafico_e_salva)
  
  
}