#geraação de mapas

# rm(list = ls())


source('./R/fun/setup.R')

# library(patchwork)
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
sigla_muni <- 'rma'


graficos <- function(munis = "all"){
  
  
  
  faz_grafico_e_salva <- function(sigla_muni, width = 16.5, height = 16.5){
    
    message(paste("Rodando",sigla_muni, "\n"))
    
    path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    
    #path hexagonos com os dados
    
    #dados_hex <- 
    
    dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni)) %>%
    st_as_sf()
    
    
    # path_muni_data_censo <- sprintf('../data-raw/censo_2021_info_muni_treated_v2/muni_%s.rds',sigla_muni)
    # path_muni_hex <- sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni)
    # path_muni_setor <- sprintf('../data-raw/setores_censitarios/2019/setores_%s_2019.rds',sigla_muni)
    path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni) 
    
    # data_muni <- read_rds(path_muni_data_censo) %>%  mutate(Cod_setor = as.character(Cod_setor))
    # data_msetor <- read_rds(path_muni_setor)
    # data_mhex <- read_rds(path_muni_hex)
    data_contorno <- read_rds(path_contorno)
    # mapview(data_contorno)
    maptiles <- read_rds(path_maptiles)
    
    
    sigla_municipio <- sigla_muni
    decisao_muni <- read_excel('../planilha_municipios.xlsx',
                               sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
    
    
    area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
                                       sigla_muni)) %>% filter(DN == 24) %>%
      st_make_valid() %>%
      st_union()
    
    simplepolys <- st_make_valid(area_urbanizada) %>% st_simplify(area_urbanizada, dTolerance = 300) %>%
      st_make_valid() %>%
      st_transform(decisao_muni$epsg) %>%
      st_buffer(2) %>%
      st_union() 
    
    assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                                      sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
      mutate(title = "Assentamentos Precários")
    # mapview(assentamentos)
    
    dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                        sigla_muni, sigla_muni))
    
    pop_counts <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(pop_total = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% mutate(quintil = as.factor(ntile(pop_total, 4)))
    
    dados_areas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                   sigla_muni, sigla_muni), layer= "areas") %>%
      st_make_valid()
    # mapview(dados_areas)
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
    hex_empty <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds',
                                  sigla_muni, sigla_muni)) %>% select(id_hex, geometry)
    

    # dados_acc <- left_join(hex_empty, acess_cma, by = c("id_hex"="origin")) %>% st_as_sf() %>%
    #   filter(mode == "transit")
    # mapview(dados_acc, zcol = "CMATT60")
    
    colors_purple <- c("#F1F2FE",
                       "#9FA4F9","#767DCE","#21367D","#1A295B")
    
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
    
    dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds',
                                  sigla_muni, sigla_muni)) %>% st_as_sf()
    

# Mapa de empregos anterior -----------------------------------------------

    
    # map_empregos <- ggplot() +
    #   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   coord_equal() +
    #   scale_fill_identity()+
    #   # nova escala
    #   new_scale_fill() +
    #   # theme_map() +
    #   geom_sf(data = st_transform(dados_hex,3857),aes(fill = n_jobs),color = NA, alpha = .8) +
    #   
    #   # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    #   
    #   geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
    #   
    # 
    #   
    #   # facet_wrap(~dado, labeller = labeller_grupos) +
    #   scale_fill_gradientn(
    #     name = "Nº de Empregos",
    #     colors =colors_purple ,
    #     # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    #     # values = NULL,
    #     space = "Lab",
    #     na.value = NA,
    #     # guide = "colourbar",
    #     aesthetics = "fill",
    #     # colors
    #   ) +
    #   # tema_populacao()
    #   theme(legend.position = "bottom",
    #         strip.text.x = element_text(size=rel(1.2)),
    #         strip.background = element_rect(
    #           color = NA,
    #           fill = "#eff0f0"
    #         ),
    #         panel.background = element_rect(fill = NA, colour = NA),
    #         axis.text = element_blank(),
    #         axis.title = element_blank(),
    #         axis.ticks = element_blank(), 
    #         panel.grid = element_blank(),
    #         plot.margin=unit(c(2,0,0,0),"mm"),
    #         legend.key.width=unit(2,"line"),
    #         legend.key.height = unit(.5,"cm"),
    #         legend.text=element_text(size=rel(1)),
    #         legend.title=element_text(size=rel(1),                                   ),
    #         plot.title = element_text(hjust = 0, vjust = 4),
    #         strip.text = element_text(size = 10)
    #   )
    

# Mapa de empregos --------------------------------------------------------

    hist(dados_hex$n_jobs)
    max_jobs <- 1600
    dados_empregos <- dados_hex %>% filter(n_jobs >0) %>%
      mutate(n_jobs = ifelse(n_jobs > max_jobs, max_jobs, n_jobs))
    
    limits <- c(0, max_jobs)
    breaks <- seq(0,max_jobs,max_jobs/4)
    labels <- c(seq(0,max_jobs-max_jobs/4,max_jobs/4), paste0("> ", max_jobs))
    # hist(dados_empregos$n_jobs)
    
    map_empregos <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(dados_empregos, 3857),
              aes(fill = n_jobs),
              colour = NA,
              alpha=.8,
              size = 0)+
      
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      # geom_sf(data = st_transform(dados_linhas, 3857),
      #         aes(color = '#0f805e'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         alpha = 1,
      #         linewidth = 1.0) +
      # 
      # scale_color_manual(name = "Infraestrutura de Transporte Público",
      #                    values = c("#0f805e" = "#0f805e"),
      #                    label = c("#0f805e" = "Linhas de Transporte Público")
      # )+
      
      
      # ggnewscale::new_scale_color() +
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7)  +
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.3,
              fill = "#0F805E",
              show.legend = "polygon",
              alpha = 0.5)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("ag", "urb", "areas"),
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#0F805E"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      

      # viridis::scale_fill_viridis(option = 'A',
      #                    direction = 1,
      #                      name = "Nº de Empregos",
      #                      breaks = breaks,
      # 
      #                      labels = labels,
      #                    limits = limits)+
      
      
      scale_fill_gradientn(
        name = "Nº de Empregos",
        colors =colors_orange ,
        # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
        # values = NULL,
        space = "Lab",
        na.value = NA,
        # guide = "colourbar",
        aesthetics = "fill",
        breaks = breaks,
        limits = limits,
        labels = labels
        # colors
      ) +
      
      
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
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey85", size = .4) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
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
      legend.position = c(0.82, 0.26),
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
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#0F805E", "white", "white"),
                                                      linewidth = c(1,1,1),
                                                      alpha = c(0.5,0.5,0.5)),
                                  order = 1))
    
    
    
    # width = 16; height = 16
    # map_empregos
    ggsave(map_empregos,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/1-empregos_%s_new2.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 16.5, height = 16.5, units = "cm" )
    

# Mapa de Eq. de lazer Antigo ----------------------------------------------------

    
    
    
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
 
    

# Mapa de Eq. de lazer novo -----------------------------------------------

    dados_lazer <- dados_hex %>% filter(lazer_tot >0)
    map_lazer <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(dados_lazer, 3857),
              aes(fill = lazer_tot),
              colour = NA,
              alpha=.8,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey85", linewidth = 0.4) +
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      # geom_sf(data = dados_areas %>% st_transform(3857),
      #         # aes(size = 2),
      #         aes(color = "grey70"),
      #         # color = "grey45",
      #         # aes(fill = '#CFF0FF'),
      #         fill = NA,
      #         # stroke = 2,
      #         # size = 2,
      #         linewidth = 0.5,
      #         alpha= 0.7) +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      # geom_sf(data = st_transform(dados_linhas, 3857),
      #         aes(color = '#0f805e'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         alpha = 1,
      #         linewidth = 1.0) +
      # 
      # scale_color_manual(name = "Infraestrutura de Transporte Público",
    #                    values = c("#0f805e" = "#0f805e"),
    #                    label = c("#0f805e" = "Linhas de Transporte Público")
    # )+
    
    
    # ggnewscale::new_scale_color() +
    geom_sf(data = simplepolys %>% st_transform(3857),
            # aes(size = 2),
            aes(color = "urb"),
            # color = "grey45",
            # aes(fill = '#CFF0FF'),
            fill = NA,
            # stroke = 2,
            # size = 2,
            linewidth = 0.4,
            alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.3,
              fill = "#d96e0a",
              show.legend = "polygon",
              alpha = 0.7)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("ag", "urb", "areas"),
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#d96e0a"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      

      
      scale_fill_gradientn(
        name = "Nº de Eq. de lazer",
        colors =colors_green ,
        labels = c(0,3,6,9),
        breaks = c(0, 3, 6, 9),
        limits = c(0,9),
        # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
        # values = NULL,
        space = "Lab",
        na.value = NA,
        # guide = "colourbar",
        aesthetics = "fill",
        # colors
      ) +
      
      
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
    

      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
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
      legend.position = c(0.82, 0.26),
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
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni)+
      guides(color = guide_legend(override.aes = list(fill = c("#d96e0a", "white", "white"),
                                                      linewidth = c(1,1,1),
                                                      alpha = c(0.5,0.5,0.5)),
                                  order = 1#,
                                  # byrow = T
                                  ))
    
    
    
    
    
    
    
    
    
   
    # map_lazer
    ggsave(map_lazer,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/2-lazer_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 16.5, height = 16.5, units = "cm" )
    # width = 16; height = 16
    

# Saúde Antigo-------------------------------------------------------------------

    saude_st <- dados_hex %>% filter(S001 > 0) %>% select(id_hex,S001,S002, S003, S004) %>% drop_na()
    #saude
    
    #saude total
    
    map_saude_total <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(saude_st,3857),aes(fill = factor(S001)),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Eq. de Saúde Nv.1")+
      scale_fill_manual(values = c("#ade7ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#066891"),
                        labels = c(1,2,3,4,5)) +
      labs(fill = "Eq. de\nSaúde Totais") +
      # facet_wrap(~dado, labeller = labeller_grupos) +
      # scale_fill_gradientn(
      #   name = "Nº de\nEscolas",
      #   colors =colors_orange ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   labels = scales::label_number(accuracy = 1)
    #   # colors
    # ) +
    # tema_populacao()
    theme(legend.position = c(0.25,0.1),
          legend.direction = "horizontal",
          strip.text.x = element_text(size=rel(1)),
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
          legend.text= element_text(size=rel(.6), vjust = 0, angle = 0),
          legend.title= element_text(size=rel(.9), vjust = 1),
          legend.background = element_blank(),
          # plot.title = element_text(hjust = 0.1, vjust = -5),
          # strip.text = element_text(size = 9)
    )
    
    # map_educacao_nv3
    
    # map_matriculas <- map_matriculas_total + map_matriculas_nv1 + map_matriculas_nv2 + map_matriculas_nv3
    # map_escolas <- wrap_plots(map_escolas_total, map_escolas_nv1,
    #                           map_escolas_nv2, map_escolas_nv3, ncol = 2) &
    #   # plot_layout(#ncol = 5,
    #   #             # widths = c(4, -0.5 , 4 , -0.5, 4 ),
    #   #             # heights = c(20,2),
    #   #             guides = "collect") &
    #   theme(legend.position = c(0.50,0.12),
    #         legend.direction = "horizontal",
    #         legend.key.width = unit(0.8, "line"),
    #         legend.key.height = unit(0.3, "cm"),
    #         # panel.spacing = ,
    #         # legend.margin = margin(t=-70),
    #         # strip.background = element_blank(),
    #         # legend.box.margin = margin(t=-100),
    #         legend.box.background = element_blank(),
    #         legend.background = element_blank(),
    #         legend.title= element_text(size=rel(.8), vjust = 1),
    #         legend.text= element_text(size=rel(.6), vjust = 0.6, angle = 0),
    #         strip.text = element_text(size = rel(1), hjust = 0.2),
    #         plot.title = element_text(hjust = 0.2, vjust = -4, size = rel(1)),
    #         # legend.box.background = element_rect(fill = "white", colour = "black")
    #         
    #         # = element_rect(fill = "white", colour = "black",) #,  margin(t=-10, unit = "mm"))
    #   )

    # map_saude_total
    
    #saude nivel 1
    
    map_saude_nv1 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(saude_st,3857),aes(fill = factor(S002)),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Eq. de Saúde Nv.1")+
      scale_fill_manual(values = c("#ade7ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#066891"),
                        labels = c(1,2,3,4,5)) +
      labs(fill = "Eq. de\nSaúde Nv.1") +
      # facet_wrap(~dado, labeller = labeller_grupos) +
      # scale_fill_gradientn(
      #   name = "Nº de\nEscolas",
      #   colors =colors_orange ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   labels = scales::label_number(accuracy = 1)
    #   # colors
    # ) +
    # tema_populacao()
    theme(legend.position = c(0.25,0.1),
          legend.direction = "horizontal",
          strip.text.x = element_text(size=rel(1)),
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
          legend.text= element_text(size=rel(.6), vjust = 0, angle = 0),
          legend.title= element_text(size=rel(.9), vjust = 1),
          legend.background = element_blank(),
          # plot.title = element_text(hjust = 0.1, vjust = -5),
          # strip.text = element_text(size = 9)
    )
    
    # map_saude_nv1
    
    #saude nv2
    saude_s2 <- saude_st %>% drop_na(S003)
    
    map_saude_nv2 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(saude_st,3857),aes(fill = factor(S003)),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Eq. de Saúde Nv. 2")+
      scale_fill_manual(values = c("#ade7ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#0989c0", "#066891"),
                        labels = c(1,2,3,4,5,6)) +
      labs(fill = "Eq. de\nSaúde Nv. 2") +
      # facet_wrap(~dado, labeller = labeller_grupos) +
      # scale_fill_gradientn(
      #   name = "Nº de\nEscolas",
      #   colors =colors_orange ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   labels = scales::label_number(accuracy = 1)
    #   # colors
    # ) +
    # tema_populacao()
    theme(legend.position = c(0.25,0.1),
          legend.direction = "horizontal",
          strip.text.x = element_text(size=rel(1)),
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
          legend.text= element_text(size=rel(.6), vjust = 0, angle = 0),
          legend.title= element_text(size=rel(.9), vjust = 1),
          legend.background = element_blank(),
          # plot.title = element_text(hjust = 0.1, vjust = -5),
          # strip.text = element_text(size = 9)
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
      geom_sf(data = st_transform(saude_st,3857),aes(fill = factor(S004)),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Eq. de Saúde Nv.3")+
      scale_fill_manual(values = c("#ade7ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#066891"),
                        labels = c(1,2,3,4,5)) +
      labs(fill = "Eq. de\nSaúde Nv.3") +
      # facet_wrap(~dado, labeller = labeller_grupos) +
      # scale_fill_gradientn(
      #   name = "Nº de\nEscolas",
      #   colors =colors_orange ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   labels = scales::label_number(accuracy = 1)
    #   # colors
    # ) +
    # tema_populacao()
    theme(legend.position = c(0.25,0.1),
          legend.direction = "horizontal",
          strip.text.x = element_text(size=rel(1)),
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
          legend.text= element_text(size=rel(.6), vjust = 0, angle = 0),
          legend.title= element_text(size=rel(.9), vjust = 1),
          legend.background = element_blank(),
          # plot.title = element_text(hjust = 0.1, vjust = -5),
          # strip.text = element_text(size = 9)
    )
    
    # map_saude_nv3
    
    # map_saude <- map_saude_total + map_saude_nv1 + map_saude_nv2 + map_saude_nv3
    
    map_saude <- wrap_plots(map_saude_total, map_saude_nv1, map_saude_nv2, map_saude_nv3, ncol = 2) &
      # plot_layout(#ncol = 5,
      #             # widths = c(4, -0.5 , 4 , -0.5, 4 ),
      #             # heights = c(20,2),
      #             guides = "collect") &
      theme(legend.position = c(0.5,0.12),
            legend.direction = "horizontal",
            legend.key.width = unit(0.8, "line"),
            legend.key.height = unit(0.3, "cm"),
            # panel.spacing = ,
            # legend.margin = margin(t=-70),
            # strip.background = element_blank(),
            # legend.box.margin = margin(t=-100),
            legend.box.background = element_blank()
            # legend.box.background = element_rect(fill = "white", colour = "black")
            
            # = element_rect(fill = "white", colour = "black",) #,  margin(t=-10, unit = "mm"))
      )
    
    # map_saude
    ggsave(map_saude,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/3-saude_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 16, height = 20, units = "cm" )
    width = 16; height = 20

# Sa[ude novo apenas totais -----------------------------------------------

    
    dados_saude_total <- dados_hex %>% filter(S001 >0) %>%
      mutate(S001 = ifelse(S001 > 5, 5, S001))
    
    map_saude_total <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +

      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
    # ggnewscale::new_scale_color() +
    geom_sf(data = simplepolys %>% st_transform(3857),
            # aes(size = 2),
            aes(color = "urb"),
            # color = "grey45",
            # aes(fill = '#CFF0FF'),
            fill = NA,
            # stroke = 2,
            # size = 2,
            linewidth = 0.5,
            alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.5,
              fill = "#d96e0a",
              show.legend = "polygon",
              alpha = 0.7)+
      
      geom_sf(data = st_transform(dados_saude_total, 3857),
              aes(fill = as.factor(S001)),
              colour = "grey70",
              alpha=.8,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#d96e0a"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      scale_fill_viridis_d(option = "viridis",
                           name = "Eq. de saúde totais",
                           direction = 1,
                           breaks = seq(1,5,1),
                           labels = seq(1,5,1)) +
      
    # scale_fill_manual(name = "Eq. de saúde totais",
    #                   values = c("1" = "#c4c9ed",
    # 
    #                              "2" = "#96a0df",
    #                              "3" = "#5766cc",
    #                              "4" = "#3b458a",
    #                              "5" = "#1f2447"),
    #                   label = c("1" = "1",
    # 
    #                             "2" = "2",
    #                             "3" = "3",
    #                             "4" = "4",
    #                             # "#33b099" = "Cobertura de 300m",
    #                             "5" = "5")) +

    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.3) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +

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
      legend.position = c(0.80, 0.27),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
      #                                      colour = "#E0DFE3"),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.spacing.x = unit(0.5, "cm"),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#d96e0a", rep("white",2))
                                                      # color = c("#d96e0a", rep("#A09C9C", 2))
                                                      ),
                                  order = 1)
             # fill = guide_legend(override.aes = list(fill = c("#c4c9ed",
             #                                                  
             #                                                  "#96a0df",
             #                                                  "#5766cc",
             #                                                  # "4" = "#3b458a",
             #                                                  "#1f2447"),
             #                                         color = rep("#A09C9C", 4)),
                                 # order = 2)
    )
    
    ggsave(map_saude_total,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/3-saude_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 16, height = 20, units = "cm" )

# saude básico novo  ------------------------------------------------------

    dados_saude_basico <- dados_hex %>% filter(S002 >0)# %>%
      # mutate(S002 = ifelse(S002 > 5, 5, S002))
    total_saude_basico <- sum(dados_saude_basico$E002)
    
    map_saude_basico <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey30", linewidth = 0.4) +
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +

    # ggnewscale::new_scale_color() +
    geom_sf(data = simplepolys %>% st_transform(3857),
            # aes(size = 2),
            aes(color = "urb"),
            # color = "grey45",
            # aes(fill = '#CFF0FF'),
            fill = NA,
            # stroke = 2,
            # size = 2,
            linewidth = 0.4,
            alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.4,
              fill = "#d96e0a",
              show.legend = "polygon",
              alpha = 0.7)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#d96e0a"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      guides(color = guide_legend(override.aes = list(fill = c("#d96e0a", rep("white",2))
                                                      # color = c("#d96e0a", rep("#A09C9C", 2))
      ),
      order = 1)
      # fill = guide_legend(override.aes = list(fill = c("#c4c9ed",
      #                                                  
      #                                                  "#96a0df",
      #                                                  "#5766cc",
      #                                                  # "4" = "#3b458a",
      #                                                  "#1f2447"),
      #                                         color = rep("#A09C9C", 4)),
      # order = 2)
      ) +
      
      new_scale_color() +
      
      geom_sf(data = st_transform(dados_saude_basico, 3857),
              aes(fill = as.factor(S002)),
              colour = "grey70",
              alpha=.8,
              size = 0)+
      
      scale_fill_viridis_d(option = "viridis",
                           name = "Eq. de saúde básica",
                           direction = 1,
                           breaks = seq(1,5,1),
                           labels = seq(1,5,1)) +
      

    # scale_fill_manual(name = "Eq. de saúde baixa complexidade",
    #                   values = c(
    #                     # "1" = "#b5b9fb",
    #                              
    #                              "1" = "#969cf8",
    #                              # "3" = "#767DCE",
    #                              # "4" = "#2b47a4",
    #                              "2" = "#1A295B"
    #                     ),
    #                   label = c("1" = "1",
    #                             
    #                             "2" = "2"
    #                             # "3" = "3",
    #                             # "4" = "4",
    #                             # "#33b099" = "Cobertura de 300m",
    #                             # "5" = "5"
    #                             )) +


      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +

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
      legend.position = c(0.82, 0.24),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
      #                                      colour = "#E0DFE3"),
      legend.spacing.y = unit(0.3, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) 
      
      # guides(color = guide_legend(override.aes = list(fill = c("#d96e0a", rep("white",2))
      #                                                 # color = c("#d96e0a", rep("#A09C9C", 2))
      # ),
      # order = 1)
      # # fill = guide_legend(override.aes = list(fill = c("#c4c9ed",
      # #                                                  
      # #                                                  "#96a0df",
      # #                                                  "#5766cc",
      # #                                                  # "4" = "#3b458a",
      # #                                                  "#1f2447"),
      # #                                         color = rep("#A09C9C", 4)),
      # # order = 2)
      # )

    ggsave(map_saude_basico,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/4-saude-baixa_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 16.5, height = 16.5, units = "cm" )

# Saude médio novo --------------------------------------------------------

    dados_saude_medio <- dados_hex %>% filter(S003 >0) %>%
      mutate(S003 = ifelse(S003 > 5, 5, S003))
    
    saude_medio_total <- sum(dados_hex$S003)
    
    map_saude_medio <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      # ggnewscale::new_scale_color() +
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.4,
              fill = "#d96e0a",
              show.legend = "polygon",
              alpha = 0.7)+
      
      geom_sf(data = st_transform(dados_saude_medio, 3857),
              aes(fill = as.factor(S002)),
              colour = "grey70",
              alpha=.8,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#d96e0a"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      scale_fill_viridis_d(option = "viridis",
                           name = "Eq. de saúde de média complexidade",
                           direction = 1,
                           breaks = seq(1,5,1),
                           labels = seq(1,5,1)) +
      
      
      # scale_fill_manual(name = "Eq. de saúde baixa complexidade",
      #                   values = c(
      #                     # "1" = "#b5b9fb",
      #                              
      #                              "1" = "#969cf8",
      #                              # "3" = "#767DCE",
      #                              # "4" = "#2b47a4",
      #                              "2" = "#1A295B"
      #                     ),
    #                   label = c("1" = "1",
    #                             
    #                             "2" = "2"
    #                             # "3" = "3",
    #                             # "4" = "4",
    #                             # "#33b099" = "Cobertura de 300m",
    #                             # "5" = "5"
    #                             )) +
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey30", linewidth = 0.4) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
      
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
        legend.position = c(0.78, 0.24),
        legend.box.background = element_rect(fill=alpha('white', 0.7),
                                             colour = "#A09C9C",
                                             linewidth = 0.8,
                                             linetype = "solid"),
        legend.background = element_blank(),
        # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
        #                                      colour = "#E0DFE3"),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.box.just = "left"
        # legend.margin = margin(t = -80)
      ) +
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      
      guides(color = guide_legend(override.aes = list(fill = c("#d96e0a", rep("white",2))
                                                      # color = c("#d96e0a", rep("#A09C9C", 2))
      ),
      order = 1)
      # fill = guide_legend(override.aes = list(fill = c("#c4c9ed",
      #                                                  
      #                                                  "#96a0df",
      #                                                  "#5766cc",
      #                                                  # "4" = "#3b458a",
      #                                                  "#1f2447"),
      #                                         color = rep("#A09C9C", 4)),
      # order = 2)
      )
    
    ggsave(map_saude_medio,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/5-saude-mediaa_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 16.5, height = 16.5, units = "cm" )

# alta complexidade -------------------------------------------------------

    dados_saude_alta <- dados_hex %>% filter(S004 >0) #%>%
      # mutate(S004 = ifelse(S004 > 2, 2, S004))
    sum(dados_hex$S004)
    # hist(dados_saude_alta$S004)
    map_saude_alta <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +

      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
    geom_sf(data = simplepolys %>% st_transform(3857),
            # aes(size = 2),
            aes(color = "urb"),
            # color = "grey45",
            # aes(fill = '#CFF0FF'),
            fill = NA,
            # stroke = 2,
            # size = 2,
            linewidth = 0.4,
            alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.4,
              fill = "#d96e0a",
              show.legend = "polygon",
              alpha = 0.3)+
      
      geom_sf(data = st_transform(dados_saude_alta, 3857),
              aes(fill = as.factor(S004)),
              colour = "grey70",
              alpha=.8,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#d96e0a"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      scale_fill_viridis_d(option = "viridis",
                           name = "Eq. de saúde de alta complexidade",
                           direction = 1,
                           breaks = seq(1,5,1),
                           labels = seq(1,5,1)) +
      
    # scale_fill_manual(name = "Eq. de saúde alta complexidade",
    #                   values = c(
    #                     # "1" = "#b5b9fb",
    #                     
    #                     "1" = "#969cf8",
    #                     # "3" = "#767DCE",
    #                     # "4" = "#2b47a4",
    #                     "2" = "#1A295B"),
    #                   label = c("1" = "1",
    #                             
    #                             "2" = "2"
    #                             # "3" = "3",
    #                             # "4" = "4",
    #                             # "#33b099" = "Cobertura de 300m",
    #                             # "5" = "5"
    #                             )) +

    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey30", linewidth = 0.4) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +

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
      legend.position = c(0.79, 0.24),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
      #                                      colour = "#E0DFE3"),
      legend.spacing.y = unit(0.3, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      
      guides(color = guide_legend(override.aes = list(fill = c("#d96e0a", rep("white",2))
                                                      # color = c("#d96e0a", rep("#A09C9C", 2))
      ),
      order = 1)
      # fill = guide_legend(override.aes = list(fill = c("#c4c9ed",
      #                                                  
      #                                                  "#96a0df",
      #                                                  "#5766cc",
      #                                                  # "4" = "#3b458a",
      #                                                  "#1f2447"),
      #                                         color = rep("#A09C9C", 4)),
      # order = 2)
      )

    
    
    ggsave(map_saude_alta,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/6-saude-alta_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 16.5, height = 16.5, units = "cm" )
    

  # Matriculas Antigo-----------------------------------------------------------------

    
    
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
        name = "Nº de\nMatrículas",
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
      theme(legend.position = c(0.25,0.1),
            legend.direction = "horizontal",
            strip.text.x = element_text(size=rel(1)),
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
            legend.text= element_text(size=rel(.6), vjust = 0, angle = 90),
            legend.title= element_text(size=rel(.9), vjust = 1),
            legend.background = element_blank(),
            # plot.title = element_text(hjust = 0.1, vjust = -5),
            # strip.text = element_text(size = 9)
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
        name = "Nº de\nMatriculas",
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
      theme(legend.position = c(0.25,0.1),
            legend.direction = "horizontal",
            strip.text.x = element_text(size=rel(1)),
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
            legend.text= element_text(size=rel(.6), vjust = 0, angle = 90),
            legend.title= element_text(size=rel(.9), vjust = 1),
            legend.background = element_blank(),
            # plot.title = element_text(hjust = 0.1, vjust = -5),
            # strip.text = element_text(size = 9)
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
        name = "Nº de\nMatriculas",
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
      theme(legend.position = c(0.25,0.1),
            legend.direction = "horizontal",
            strip.text.x = element_text(size=rel(1)),
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
            legend.text= element_text(size=rel(.6), vjust = 0, angle = 90),
            legend.title= element_text(size=rel(.9), vjust = 1),
            legend.background = element_blank(),
            # plot.title = element_text(hjust = 0.1, vjust = -5),
            # strip.text = element_text(size = 9)
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
        name = "Nº de\nMatrículas",
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
      theme(legend.position = c(0.25,0.1),
            legend.direction = "horizontal",
            strip.text.x = element_text(size=rel(1)),
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
            legend.text= element_text(size=rel(.6), vjust = -2, angle = 90),
            legend.title= element_text(size=rel(.9), vjust = 1),
            legend.background = element_blank(),
            # plot.title = element_text(hjust = 0.1, vjust = 0),
            # strip.text = element_text(size = 9)
      )
    
    # map_educacao_nv3
    
    # map_matriculas <- map_matriculas_total + map_matriculas_nv1 + map_matriculas_nv2 + map_matriculas_nv3
    map_matriculas <- wrap_plots(map_matriculas_total, map_matriculas_nv1,
                                 map_matriculas_nv2, map_matriculas_nv3, ncol = 2) &
      # plot_layout(#ncol = 5,
      #             # widths = c(4, -0.5 , 4 , -0.5, 4 ),
      #             # heights = c(20,2),
      #             guides = "collect") &
      theme(legend.position = c(0.35,0.12),
            legend.direction = "horizontal",
            legend.key.width = unit(0.8, "line"),
            legend.key.height = unit(0.3, "cm"),
            # panel.spacing = ,
            # legend.margin = margin(t=-70),
            # strip.background = element_blank(),
            # legend.box.margin = margin(t=-100),
            legend.box.background = element_blank(),
            legend.background = element_blank(),
            legend.title= element_text(size=rel(.8), vjust = 1),
            legend.text= element_text(size=rel(.6), vjust = 0.6, angle = 90),
            strip.text = element_text(size = rel(1), hjust = 0.2),
            plot.title = element_text(hjust = 0.2, vjust = -4, size = rel(1)),
            # legend.box.background = element_rect(fill = "white", colour = "black")
            
            # = element_rect(fill = "white", colour = "black",) #,  margin(t=-10, unit = "mm"))
      )
    
    # map_saude
    ggsave(map_matriculas,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/4-matriculas_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    

# Matriculas novo (totais) ------------------------------------------------

    dados_matriculas <- dados_hex %>% filter(M001 >0)
      # mutate(S001 = ifelse(M001 > 5, 5, S001))
    map_matriculas <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +

      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +

    # ggnewscale::new_scale_color() +
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
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.9,
              fill = "#0F805E",
              show.legend = "polygon",
              alpha = 0.5)+
      
      geom_sf(data = st_transform(dados_matriculas, 3857),
              aes(fill = M001),
              colour = "grey70",
              alpha=.8,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#0F805E"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" =  munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+

    scale_fill_gradientn(
      name = "Nº de matrículas",
      colors =colors_orange ,
      # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      # values = NULL,
      space = "Lab",
      na.value = NA,
      # guide = "colourbar",
      aesthetics = "fill",
      # colors
    ) +

    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.3) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +

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
      legend.position = c(0.82, 0.25),
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
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#0F805E", "white", "white"),
                                                      alpha = c(0.5, rep(0.1,2))),
                                  order = 1))

    ggsave(map_matriculas,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/7-matriculas_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    

# Matrículas ensino infantil ----------------------------------------------

    dados_matriculas_infantil <- dados_hex %>% filter(M002 >0)
    # mutate(S001 = ifelse(M001 > 5, 5, S001))
    map_matriculas_infantil <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      # ggnewscale::new_scale_color() +
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
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.9,
              fill = "#0F805E",
              show.legend = "polygon",
              alpha = 0.5)+
      
      geom_sf(data = st_transform(dados_matriculas_infantil, 3857),
              aes(fill = M002),
              colour = "grey70",
              alpha=.8,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#0F805E"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" =  munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      scale_fill_gradientn(
        name = "Nº de matrículas de E. infantil",
        colors =colors_orange ,
        # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
        # values = NULL,
        space = "Lab",
        na.value = NA,
        # guide = "colourbar",
        aesthetics = "fill",
        # colors
      ) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.3) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
      
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
        legend.position = c(0.81, 0.25),
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
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#0F805E", "white", "white"),
                                                      alpha = c(0.5, rep(0.1,2))),
                                  order = 1))
    
    ggsave(map_matriculas_infantil,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/8-matriculas_infantil_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )

# Matrículas de ensino fundamental ----------------------------------------

    dados_matriculas_fundamental <- dados_hex %>% filter(M003 >0)
    # mutate(S001 = ifelse(M001 > 5, 5, S001))
    map_matriculas_fundamental <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      # ggnewscale::new_scale_color() +
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
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.9,
              fill = "#0F805E",
              show.legend = "polygon",
              alpha = 0.5)+
      
      geom_sf(data = st_transform(dados_matriculas_fundamental, 3857),
              aes(fill = M002),
              colour = "grey70",
              alpha=.8,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#0F805E"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" =  munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      scale_fill_gradientn(
        name = "Nº de matrículas de E. fundamental",
        colors =colors_orange ,
        # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
        # values = NULL,
        space = "Lab",
        na.value = NA,
        # guide = "colourbar",
        aesthetics = "fill",
        # colors
      ) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.3) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
      
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
        legend.position = c(0.80, 0.25),
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
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#0F805E", "white", "white"),
                                                      alpha = c(0.5, rep(0.1,2))),
                                  order = 1))
    
    ggsave(map_matriculas_fundamental,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/9-matriculas_fundamental_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )    

# Matriculas ensino  médio -----------------------------------------------
    dados_matriculas_medio <- dados_hex %>% filter(M004 >0)
    # mutate(S001 = ifelse(M001 > 5, 5, S001))
    map_matriculas_medio <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      # ggnewscale::new_scale_color() +
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
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.9,
              fill = "#0F805E",
              show.legend = "polygon",
              alpha = 0.5)+
      
      geom_sf(data = st_transform(dados_matriculas_medio, 3857),
              aes(fill = M002),
              colour = "grey70",
              alpha=.8,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#0F805E"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" =  munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      scale_fill_gradientn(
        name = "Nº de matrículas de E. médio",
        colors =colors_orange ,
        # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
        # values = NULL,
        space = "Lab",
        na.value = NA,
        # guide = "colourbar",
        aesthetics = "fill",
        # colors
      ) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.3) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
      
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
        legend.position = c(0.80, 0.25),
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
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#0F805E", "white", "white"),
                                                      alpha = c(0.5, rep(0.1,2))),
                                  order = 1))
    
    ggsave(map_matriculas_medio,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/10-matriculas_medio_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )

# Escolas Novo Totais------------------------------------------------------------

    dados_escolas <- dados_hex %>% filter(E001 >0)
    # mutate(S001 = ifelse(M001 > 5, 5, S001))
    map_escolas <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
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
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.9,
              fill = "#2B6CB0",
              show.legend = "polygon",
              alpha = 0.5)+
      
      geom_sf(data = st_transform(dados_escolas, 3857),
              aes(fill = factor(E001)),
              colour = "grey70",
              alpha= 1,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#2B6CB0"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+

      scale_fill_manual(
        name = "Nº de Escolas",
        # colors =colors_orange[2:length(colors_orange)],
        values =colors_escolas,
        breaks = seq(1,max(dados_escolas %>% distinct(E001)),1),
        labels = seq(1,max(dados_escolas %>% distinct(E001)),1)#,
        # limits = c(1,max(dados_escolas %>% distinct(E001))),

      ) +
      
      # scale_fill_gradientn(
      #   name = "Nº de Escolas",
      #   # colors =colors_orange[2:length(colors_orange)],
      #   colors =colors_escolas,
      #   breaks = seq(1,max(dados_escolas %>% distinct(E001)),1),
      #   labels = seq(1,max(dados_escolas %>% distinct(E001)),1),
      #   limits = c(1,max(dados_escolas %>% distinct(E001))),
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   # colors
      # ) +

    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey75", linewidth = 0.4) +
      
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
      legend.position = c(0.20, 0.24),
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
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#2B6CB0", "white", "white")),
                                  order = 1),
             fill = guide_legend(order = 2)
             )
    
    ggsave(map_escolas,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/11-escolas_%s_new3.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )

# Escolas Infantil --------------------------------------------------------

    dados_escolas_infantil <- dados_hex %>% filter(E002 >0)
    total_escolas_infantil <- sum(dados_hex$E002)
    # mutate(S001 = ifelse(M001 > 5, 5, S001))
    map_escolas_infantil <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey75", linewidth = 0.4) +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.4,
              fill = "#2B6CB0",
              show.legend = "polygon",
              alpha = 0.5)+
      
      geom_sf(data = st_transform(dados_escolas_infantil, 3857),
              aes(fill = factor(E002)),
              colour = "grey70",
              alpha= 1,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#2B6CB0"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      
      scale_fill_manual(
        name = "Nº de escolas infantis",
        # colors =colors_orange[2:length(colors_orange)],
        values =colors_escolas,
        breaks = seq(1,max(dados_escolas_infantil %>% distinct(E001)),1),
        labels = seq(1,max(dados_escolas_infantil %>% distinct(E001)),1)#,
        # limits = c(1,max(dados_escolas %>% distinct(E001))),
        
      ) +
      
      # scale_fill_gradientn(
      #   name = "Nº de escolas infantis",
      #   # colors =colors_purple[3:length(colors_purple)],
      #   colors =colors_escolas,
      #   breaks = seq(1,max(dados_escolas_infantil %>% distinct(E002)),1),
      #   labels = seq(1,max(dados_escolas_infantil %>% distinct(E002)),1),
      #   limits = c(1,max(dados_escolas_infantil %>% distinct(E002))),
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   # colors
      # ) +
      

      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
      
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
        legend.position = c(0.82, 0.22),
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
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#2B6CB0", "white", "white")),
                                  order = 1),
             fill = guide_legend(order = 2)
      )
    
    ggsave(map_escolas_infantil,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/12-escolas_infantis_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )

# Escolasde ensino fundamental --------------------------------------------

    dados_escolas_fundamental <- dados_hex %>% filter(E003 >0)
    total_escolas_fundamental <- sum(dados_hex$E003)
    # mutate(S001 = ifelse(M001 > 5, 5, S001))
    map_escolas_fundamental <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey75", linewidth = 0.4) +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.4,
              fill = "#2B6CB0",
              show.legend = "polygon",
              alpha = 0.5)+
      
      geom_sf(data = st_transform(dados_escolas_fundamental, 3857),
              aes(fill = factor(E003)),
              colour = "grey70",
              alpha=.8,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#2B6CB0"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      scale_fill_manual(
        name = "Nº de escolas de ensino fundamental",
        # colors =colors_orange[2:length(colors_orange)],
        values =colors_escolas,
        breaks = seq(1,max(dados_escolas_fundamental %>% distinct(E001)),1),
        labels = seq(1,max(dados_escolas_fundamental %>% distinct(E001)),1)#,
        # limits = c(1,max(dados_escolas %>% distinct(E001))),
        
      ) +
      
      # scale_fill_gradientn(
      #   name = "Nº de escolas de ensino fundamental",
      #   colors =colors_orange[2:length(colors_orange)],
      #   breaks = seq(1,max(dados_escolas_fundamental %>% distinct(E003)),1),
      #   labels = seq(1,max(dados_escolas_fundamental %>% distinct(E003)),1),
      #   limits = c(1,max(dados_escolas_fundamental %>% distinct(E003))),
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   # colors
      # ) +
      

      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
      
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
        legend.position = c(0.78, 0.20),
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
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#2B6CB0", "white", "white")),
                                  order = 1),
             fill = guide_legend(order = 2)
      )
    
    ggsave(map_escolas_fundamental,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/13-escolas_fundamental_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    

# Escolas ensino médio novo -----------------------------------------------

    dados_escolas_medio <- dados_hex %>% filter(E004 >0)
    total_escolas_medio <- sum(dados_hex$E004)
    # mutate(S001 = ifelse(M001 > 5, 5, S001))
    map_escolas_medio <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey75", linewidth = 0.4) +
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.4,
              fill = "#2B6CB0",
              show.legend = "polygon",
              alpha = 0.5)+
      
      geom_sf(data = st_transform(dados_escolas_medio, 3857),
              aes(fill = factor(E004)),
              colour = "grey70",
              alpha=.8,
              size = 0)+
      
      scale_color_manual(name = "Uso do solo",
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#2B6CB0"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      scale_fill_manual(
        name = "Nº de escolas de ensino médio",
        # colors =colors_orange[2:length(colors_orange)],
        values =colors_escolas,
        breaks = seq(1,max(dados_escolas_medio %>% distinct(E001)),1),
        labels = seq(1,max(dados_escolas_medio %>% distinct(E001)),1)#,
        # limits = c(1,max(dados_escolas %>% distinct(E001))),
        
      ) +
      
      # scale_fill_gradientn(
      #   name = "Nº de escolas de ensino médio",
      #   colors =colors_orange[2:length(colors_orange)],
      #   breaks = seq(1,max(dados_escolas_medio %>% distinct(E004)),1),
      #   labels = seq(1,max(dados_escolas_medio %>% distinct(E004)),1),
      #   limits = c(1,max(dados_escolas_medio %>% distinct(E004))),
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   # colors
      # ) +
      

      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
      
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
        legend.position = c(0.81, 0.20),
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
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni) +
      guides(color = guide_legend(override.aes = list(fill = c("#2B6CB0", "white", "white")),
                                  order = 1),
             fill = guide_legend(order = 2)
      )
    
    ggsave(map_escolas_medio,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/14-escolas_medio_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )    

# Escolas Antigo -----------------------------------------------------------------

    #matriculas
    
    #matriculas total
    escolas_et <- dados_hex %>% filter(E001 > 0)
    
    map_escolas_total <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(escolas_et,3857),aes(fill = factor(E001)),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Escolas totais")+
      scale_fill_manual(values = c("#F5AF72","#d96e0a","#EF581B"),
                        labels = c(1,2,3)) +
      labs(fill = "Quantidade de\nEscolas") +
      # facet_wrap(~dado, labeller = labeller_grupos) +
      # scale_fill_gradientn(
      #   name = "Nº de\nEscolas",
      #   colors =colors_orange ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   labels = scales::label_number(accuracy = 1)
      #   # colors
      # ) +
      # tema_populacao()
      theme(legend.position = c(0.25,0.1),
            legend.direction = "vertical",
            strip.text.x = element_text(size=rel(1)),
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
            legend.text= element_text(size=rel(.6), vjust = 0, angle = 0),
            legend.title= element_text(size=rel(.9), vjust = 1),
            legend.background = element_blank(),
            # plot.title = element_text(hjust = 0.1, vjust = -5),
            # strip.text = element_text(size = 9)
      )
    
    # map_matriculas_total
    
    #saude nivel 1
    
    map_escolas_nv1 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(escolas_et,3857),aes(fill = factor(E002)),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Escolas de Ensino Básico")+
      scale_fill_manual(values = c("#F5AF72","#d96e0a","#EF581B"),
                        labels = c(1,2,3)) +
      labs(fill = "Quantidade de\nEscolas") +
      # facet_wrap(~dado, labeller = labeller_grupos) +
      # scale_fill_gradientn(
      #   name = "Nº de\nEscolas",
      #   colors =colors_orange ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   labels = scales::label_number(accuracy = 1)
    #   # colors
    # ) +
    # tema_populacao()
    theme(legend.position = c(0.25,0.1),
          legend.direction = "vertical",
          strip.text.x = element_text(size=rel(1)),
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
          legend.text= element_text(size=rel(.6), vjust = 0, angle = 0),
          legend.title= element_text(size=rel(.9), vjust = 1),
          legend.background = element_blank(),
          # plot.title = element_text(hjust = 0.1, vjust = -5),
          # strip.text = element_text(size = 9)
    )
    
    # map_matriculas_nv1
    
    #smatriculas
    
    map_escolas_nv2 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(escolas_et,3857),aes(fill = factor(E003)),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Escolas de Ensino Fundamental")+
      scale_fill_manual(values = c("#F5AF72","#d96e0a","#EF581B"),
                        labels = c(1,2,3)) +
      labs(fill = "Quantidade de\nEscolas") +
      # facet_wrap(~dado, labeller = labeller_grupos) +
      # scale_fill_gradientn(
      #   name = "Nº de\nEscolas",
      #   colors =colors_orange ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   labels = scales::label_number(accuracy = 1)
    #   # colors
    # ) +
    # tema_populacao()
    theme(legend.position = c(0.25,0.1),
          legend.direction = "vertical",
          strip.text.x = element_text(size=rel(1)),
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
          legend.text= element_text(size=rel(.6), vjust = 0, angle = 0),
          legend.title= element_text(size=rel(.9), vjust = 1),
          legend.background = element_blank(),
          # plot.title = element_text(hjust = 0.1, vjust = -5),
          # strip.text = element_text(size = 9)
    )
    
    # map_matriculas_nv2
    
    #educacao nv3
    
    map_escolas_nv3 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(escolas_et,3857),aes(fill = factor(E004)),color = NA, alpha = .7) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      ggtitle("Escolas de Ensino Médio")+
      scale_fill_manual(values = c("#F5AF72","#d96e0a","#EF581B"),
                        labels = c(1,2,3)) +
      labs(fill = "Quantidade de\nEscolas") +
      # facet_wrap(~dado, labeller = labeller_grupos) +
      # scale_fill_gradientn(
      #   name = "Nº de\nEscolas",
      #   colors =colors_orange ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   labels = scales::label_number(accuracy = 1)
    #   # colors
    # ) +
    # tema_populacao()
    theme(legend.position = c(0.25,0.1),
          legend.direction = "vertical",
          strip.text.x = element_text(size=rel(1)),
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
          legend.text= element_text(size=rel(.6), vjust = 0, angle = 0),
          legend.title= element_text(size=rel(.9), vjust = 1),
          legend.background = element_blank(),
          # plot.title = element_text(hjust = 0.1, vjust = -5),
          # strip.text = element_text(size = 9)
    )
    
    # map_educacao_nv3
    
    # map_matriculas <- map_matriculas_total + map_matriculas_nv1 + map_matriculas_nv2 + map_matriculas_nv3
    map_escolas <- wrap_plots(map_escolas_total, map_escolas_nv1,
                                 map_escolas_nv2, map_escolas_nv3, ncol = 2) &
      # plot_layout(#ncol = 5,
      #             # widths = c(4, -0.5 , 4 , -0.5, 4 ),
      #             # heights = c(20,2),
      #             guides = "collect") &
      theme(legend.position = c(0.50,0.12),
            legend.direction = "horizontal",
            legend.key.width = unit(0.8, "line"),
            legend.key.height = unit(0.3, "cm"),
            # panel.spacing = ,
            # legend.margin = margin(t=-70),
            # strip.background = element_blank(),
            # legend.box.margin = margin(t=-100),
            legend.box.background = element_blank(),
            legend.background = element_blank(),
            legend.title= element_text(size=rel(.8), vjust = 1),
            legend.text= element_text(size=rel(.6), vjust = 0.6, angle = 0),
            strip.text = element_text(size = rel(1), hjust = 0.2),
            plot.title = element_text(hjust = 0.2, vjust = -4, size = rel(1)),
            # legend.box.background = element_rect(fill = "white", colour = "black")
            
            # = element_rect(fill = "white", colour = "black",) #,  margin(t=-10, unit = "mm"))
      )
    
    # map_saude
    ggsave(map_escolas,
           device = "png",
           filename =  sprintf("../data/map_plots_amenities/muni_%s/5-escolas_%s.png", sigla_muni, sigla_muni),
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