
# rm(list = ls())


source('./R/fun/setup.R')
sf_use_s2(use_s2 = TRUE)

library(showtext)
# library(ggmap)
library(ggspatial)

showtext_auto()
width <- 16.5
height <- 16.5
# font_add_google("Encode Sans Light 300")
font_add("encode_sans", '../data/fontes/EncodeSans-VariableFont_wdth,wght.ttf')
font_add("encode_sans_regular", '../data/fontes/EncodeSans-Regular.ttf')
font_add("encode_sans_bold", '../data/fontes/EncodeSans-Bold.ttf')
font_add("encode_sans_light", '../data/fontes/EncodeSans-Light.ttf')
sigla_muni <- 'pal'
ano <- 2019

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
    
    #path hexagonos com os dados
    
    #dados_hex <- 
    ano <- 2019
    dados_hex <- read_rds(sprintf('../data/hex_municipio/hex_%s_%s_09.rds', ano, sigla_muni)) %>%
      st_as_sf()
    
    # mapview(dados_hex)
    # path_muni_data_censo <- sprintf('../data-raw/censo_2021_info_muni_treated_v2/muni_%s.rds',sigla_muni)
    # path_muni_hex <- sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni)
    # path_muni_setor <- sprintf('../data-raw/setores_censitarios/2019/setores_%s_2019.rds',sigla_muni)
    path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni) 
    
    # data_muni <- read_rds(path_muni_data_censo) %>%  mutate(Cod_setor = as.character(Cod_setor))
    # data_msetor <- read_rds(path_muni_setor)
    # data_mhex <- read_rds(path_muni_hex)
    data_contorno <- read_rds(path_contorno)
    
    maptiles <- read_rds(path_maptiles)
    

    
    
    sigla_municipio <- sigla_muni
    decisao_muni <- read_excel('../planilha_municipios.xlsx',
                               sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
    
    
    dados_areas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                   sigla_muni, sigla_muni), layer= "areas") %>% st_transform(decisao_muni$epsg)
    # mapview(dados_areas)
    area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
                                       sigla_muni)) %>% filter(DN == 24) %>%
      st_make_valid() %>%
      st_union() #%>% st_transform(3857)
    
    sigla_municipio <- sigla_muni
    decisao_muni <- read_excel('../planilha_municipios.xlsx',
                               sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
    
    simplepolys <- st_simplify(area_urbanizada, dTolerance = 300) %>%
      st_make_valid() %>%
      st_transform(decisao_muni$epsg) %>%
      st_buffer(2) %>%
      st_union() 
    
    # mapview(simplepolys)
    
    assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                                      sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
      mutate(title = "Assentamentos Precários")
    
    
    dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                        sigla_muni, sigla_muni))
    #checar setores com todos os renda_class_pc == n_col
    
    # lista_tract <- dados_simulacao %>% group_by(code_tract, renda_class_pc) %>%
    #   summarise(n = n()) %>% ungroup() %>%
    #   group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
    #                                      n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
    #   filter(n_classes > n_classes_col) %>% pull(code_tract)
    
    # data_micro2 <- dados_simulacao %>% filter(code_tract %in% lista_tract) %>% select(1:10, V0606, hex) %>%
    #   mutate(V0606 = as.factor(V0606))
    
    data_micro2 <- dados_simulacao  %>% select(1:12, V0606, hex) %>%
      mutate(V0606 = as.factor(V0606))
    
    #remocão dos habitantes de cor amarela e indígena
    levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
    # data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
    
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
    
    
    
    
    
    #Dados de Densidade Populacional
    
    pop_counts <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(pop_total = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf()
    
    st_write(pop_counts,sprintf("../data/microssimulacao/muni_%s/population_counts_%s.gpkg",
                                sigla_muni, sigla_muni), append = F)
    
    # st_write(pop_counts, 'pop_poa.shp')
    
    # area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
    #                                    sigla_muni)) %>% filter(Classes == 24) %>%
    #   st_make_valid() %>%
    #   st_union()
    # 
    # sigla_municipio <- sigla_muni
    # decisao_muni <- read_excel('../planilha_municipios.xlsx',
    #                            sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
    # 
    # simplepolys <- st_simplify(area_urbanizada, dTolerance = 300) %>%
    #   st_make_valid() %>%
    #   st_transform(decisao_muni$epsg) %>%
    #   st_buffer(2) %>%
    #   st_union() 
    
    
    # simplepolys <- rmapshaper::ms_simplify(input = as(area_urbanizada, 'Spatial')) %>%
    #   st_as_sf()
    # mapview(simplepolys)
    # 
    # contorno2 <- st_cont
    # assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
    #                                   sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
    #   mutate(title = "Assentamentos Precários")
    
    
    
    # mapview(area_urbanizada) +  mapview(pop_counts, zcol = "pop_total")
    
    
    # teste <- st_difference(data_contorno,area_urbanizada)
    # mapview(teste)
    

# Mapa de densidade populacional antigo -----------------------------------

    
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
      theme(legend.title = element_text(size=rel(0.9), hjust = 0),
            axis.ticks.length = unit(0,"pt"),
            legend.margin = margin(t = 0),
            legend.position = c(0.18, 0.2),
            legend.direction = "vertical",
            legend.box = "vertical",
            legend.text =element_text(size=rel(0.9), hjust = 0, vjust = 1),
            legend.spacing.y = unit(0.4, 'cm')
            # legend.title=element_text(size=rel(0.6), hjust = 0)
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
    

# Mapa de densidade populacional novo -------------------------------------

    # cor_ag_densidade <- "#d7b377" #bullywood
    cor_ag_densidade <- "#e9eb9e" #crayola
    # cor_ag_densidade <- "#d8f793" #mindaro green
    
    map_pop_density <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = pop_total),
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
              linewidth = 0.7,
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
            linewidth = 0.5,
            alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.3,
              fill = cor_ag_densidade,
              show.legend = "polygon",
              alpha = 0.5)+
      
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("ag", "urb", "areas"),
                         values = c("urb" = "#9dd1f1",
                                    "areas" = "grey45",
                                    "ag" = cor_ag_densidade),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = "Bairros",
                                   "ag" = "Aglomerados subnormais")
      )+
      
      
      scale_fill_gradientn(
        name = "Habitantes",
        colors =colors_acc ,
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
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = .3) +
      
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
      guides(color = guide_legend(override.aes = list(fill = c(cor_ag_densidade, "white", "white"),
                                                      alpha = c(0.5, rep(0.1,2))),
                                  order = 1))
    
    
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    
    ggsave(map_pop_density,
           device = "png",
           filename =  sprintf('../data/map_plots_population/muni_%s/1-densidade_populacional_%s_new.png',
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
    
    

# Renda - Tratamentod os dados --------------------------------------------


    
    renda_escrita <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(renda = mean(Rend_pc, na.rm =T)) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% drop_na(h3_resolution)
      # mutate(renda = ifelse(renda>10, 10, renda))
    
    st_write(renda_escrita,sprintf("../data/microssimulacao/muni_%s/renda_%s.gpkg",
                                sigla_muni, sigla_muni), append = F)
    
    renda <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(renda = mean(Rend_pc, na.rm =T)) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% drop_na(h3_resolution) %>%
      mutate(renda = ifelse(renda>10, 10, renda))
    # mean(renda$renda)
    
    # mapview(renda, zcol = "renda")
    
    grid_micro <- read_rds(sprintf("../data/microssimulacao/muni_%s/grid_muni_%s.rds",
                                   sigla_muni, sigla_muni))
    

# Mapa de Renda Antigo ----------------------------------------------------



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
              aes(color = "#8E8E8E"),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = .7,
              alpha= .1)  +
      
      # geom_sf(data = assentamentos,
      #         aes(colour = "white"),
      #         fill = NA,
      #         size = 1.3)+
      
      scale_color_identity(labels = c("#8E8E8E" = "",
                                      blue = ""), guide = "legend") +
      labs(color = "Área urbanizada\n(Mapbiomas 2021)")+
      
      scale_fill_distiller("Renda per capita\n(Salarios Minimos)",
                           type = "div",
                           palette = "GnBu",
                           direction = 1,
                           breaks = seq(0,10,2),
                           labels = c("0","2", "4", "6","8", ">10"),
                           limits = c(0,10)) +

      
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
      theme(legend.title = element_text(size=rel(0.9), hjust = 0),
            axis.ticks.length = unit(0,"pt"),
            legend.margin = margin(t = 0),
            legend.position = c(0.18, 0.2),
            legend.direction = "vertical",
            legend.box = "vertical",
            legend.text =element_text(size=rel(0.9), hjust = 0, vjust = 1)
            # legend.background = element_rect(fill = "white", color = NA)
            # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
            #       # plot.background = element_rect(fill = "#eff0f0",
            #       #                                 colour = NA)
            # legend.background = element_rect(fill = "white",
            #                                  colour = NA)
            #       
      )
    # map_renda
    # map_pop_density
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    
    ggsave(map_renda,
           device = "png",
           filename =  sprintf('../data/map_plots_population/muni_%s/2-renda_per_capita_%s.png',
                               sigla_muni,
                               sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    
    
    
    #Mapa 3 - Recortes

# Maa de RendaNovo --------------------------------------------------------
# hist(renda$renda)
    
    cor_ag <- "#dbad6a"
    map_renda <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(renda, 3857),
              aes(fill = renda),
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
              linewidth = 0.7,
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
            linewidth = 0.5,
            alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.3,
              fill = cor_ag,
              show.legend = "polygon",
              alpha = 0.5)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("ag", "urb", "areas"),
                         values = c("urb" = "#9dd1f1",
                                    "areas" = "grey45",
                                    "ag" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = "Bairros",
                                   "ag" = "Aglomerados subnormais")
      )+
      
      viridis::scale_fill_viridis(option = "A",
                                  name = "Renda per capita (SM)",
                                  breaks = seq(0,10,2),
                                  labels = c("0","2", "4", "6","8", ">10"),
                                  limits = c(0,10)) +
      
      
      # scale_fill_gradientn(
      #   name = "Renda per capita (SM)",
      #   colors =colors_purple ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   breaks = seq(0,10,2),
      #   labels = c("0","2", "4", "6","8", ">10"),
      #   limits = c(0,10)
      #   # colors
      # ) +
      
    # scale_fill_distiller("Renda per capita (SM)",
    #                      type = "div",
    #                      palette = "GnBu",
    #                      direction = 1,
    #                      breaks = seq(0,10,2),
    #                      labels = c("0","2", "4", "6","8", ">10"),
    #                      limits = c(0,10)) +
      
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
      aproxima_muni(sigla_muni = sigla_muni)  +
      guides(color = guide_legend(override.aes = list(fill = c(cor_ag, "white", "white"),
                                                      alpha = c(0.5, rep(0.1,2))),
                                  order = 1))
    
    
    
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    
    ggsave(map_renda,
           device = "png",
           filename =  sprintf('../data/map_plots_population/muni_%s/2-renda_per_capita_new_%s.png',
                               sigla_muni,
                               sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    
    
    
    

    

# dados de recorte de genero e recorte de cor --------------------------------------------------

data_micro <- dados_simulacao  %>%
      mutate(V0606 = case_when(V0606 == 1 ~ "Brancos",
                               V0606 == 2 ~ "Pretos",
                               V0606 == 3 ~ "Amarelos",
                               V0606 == 4 ~ "Pardos",
                               V0606 == 5 ~ "Indígenas"),
            genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"))
    
    
data_recorte <- data_micro %>%
      group_by(hex, V0606, genero) %>%
      summarise(n = n()) %>%

      left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf()

data_recorte_gen <- data_micro %>%
  group_by(hex, genero) %>%
  summarise(n = n()) %>%
  left_join(dados_hex, by = c("hex" = "id_hex")) %>%
  st_as_sf()

data_recorte_cor <- data_micro %>%
  group_by(hex, V0606) %>%
  summarise(n = n()) %>%
  left_join(dados_hex, by = c("hex" = "id_hex")) %>%
  st_as_sf()

data_recorte_cor2 <- data_micro %>%
  mutate(cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                         cor == "pretos" ~ "Pretos",
                         cor == "brancos" ~ "Brancos")) %>%
  group_by(hex, cor) %>%
  summarise(n = n()) %>%
  left_join(dados_hex, by = c("hex" = "id_hex")) %>%
  st_as_sf()


data_recorte_cor2_hex <- data_recorte_cor2 %>%
  
  group_by(hex) %>%
  
  mutate(dif_bp = ifelse(is_empty(n[cor == "Brancos"]) == F &
                           is_empty(n[cor == "Pretos"]) == F,
                         n[cor == "Brancos"] - n[cor == "Pretos"],
                         
                         ifelse(is_empty(n[cor == "Brancos"]) == F &
                                  is_empty(n[cor == "Pretos"]) == T,
                                n[cor == "Brancos"] - 0,
                                ifelse(is_empty(n[cor == "Pretos"]) == F &
                                         is_empty(n[cor == "Brancos"]) == T,
                                       -n[cor == "Pretos"], NA))),
         dif_pb = ifelse(is_empty(n[cor == "Pretos"]) == F &
                           is_empty(n[cor == "Brancos"]) == F,
                         n[cor == "Pretos"] - n[cor == "Brancos"],
                         
                         ifelse(is_empty(n[cor == "Pretos"]) == F &
                                  is_empty(n[cor == "Brancos"]) == T,
                                n[cor == "Pretos"] - 0,
                                ifelse(is_empty(n[cor == "Brancos"]) == F &
                                         is_empty(n[cor == "Pretos"]) == T,
                                       -n[cor == "Brancos"], NA))),
         
         brancos  = ifelse(is_empty(n[cor == "Brancos"]) == F,
                            n[cor == "Brancos"], 0),
         pretos  = ifelse(is_empty(n[cor == "Pretos"]) == F,
                          n[cor == "Pretos"], 0)
         
         
         
  ) %>%
  distinct(hex, .keep_all = T) %>%
  select(hex, dif_bp, dif_pb, brancos, pretos)


# dados para os mapas - recorte de genero

# data_recorte_gen

data_recorte_gen_hex <- data_recorte_gen %>%
  group_by(hex) %>%
  
  mutate(dif_hm = ifelse(is_empty(n[genero == "Homens"]) == F &
                                   is_empty(n[genero == "Mulheres"]) == F,
                                 n[genero == "Homens"] - n[genero == "Mulheres"],
                                 
                                 ifelse(is_empty(n[genero == "Homens"]) == F &
                                          is_empty(n[genero == "Mulheres"]) == T,
                                        n[genero == "Homens"] - 0,
                                        ifelse(is_empty(n[genero == "Mulheres"]) == F &
                                                 is_empty(n[genero == "Homens"]) == T,
                                               -n[genero == "Mulheres"], NA))),
         mulheres  = ifelse(is_empty(n[genero == "Mulheres"]) == F,
                               n[genero == "Mulheres"], 0),
         homens  = ifelse(is_empty(n[genero == "Homens"]) == F,
                            n[genero == "Homens"], 0)
         
                               
                              
  ) %>%
  distinct(hex, .keep_all = T) %>%
  select(hex, dif_hm, mulheres, homens)


data_recorte_gen_resp <- data_micro %>%
  group_by(hex, resp_home) %>%
  summarise(n = n()) %>%
  filter(resp_home != "Resp_dep") %>%
  left_join(dados_hex, by = c("hex" = "id_hex")) %>%
  st_as_sf()
  
data_recorte_gen_resp_hex <- data_recorte_gen_resp %>%
    group_by(hex) %>%
    
    mutate(dif_resp_hm = ifelse(is_empty(n[resp_home == "Resp_masc"]) == F &
                                                   is_empty(n[resp_home == "Resp_fem"]) == F ,
                                                 n[resp_home == "Resp_masc"] - n[resp_home == "Resp_fem"],
                                                 
                                                 ifelse(is_empty(n[resp_home == "Resp_masc"]) == F &
                                                          is_empty(n[resp_home == "Resp_fem"]) == T,
                                                        n[resp_home == "Resp_masc"] - 0,
                                                        ifelse(is_empty(n[resp_home == "Resp_fem"]) == F &
                                                                 is_empty(n[resp_home == "Resp_masc"]) == T,
                                                               -n[resp_home == "Resp_fem"], NA))),
           resp_mulheres  = ifelse(is_empty(n[resp_home == "Resp_fem"]) == F,
                              n[resp_home == "Resp_fem"], 0),
           resp_homens  = ifelse(is_empty(n[resp_home == "Resp_masc"]) == F,
                            n[resp_home == "Resp_masc"], 0)
           
           
           
    ) %>%
    distinct(hex, .keep_all = T) %>%
    select(hex, dif_resp_hm, resp_mulheres, resp_homens) %>% st_as_sf()
  
  

# mapview(data_recorte_gen_hex, zcol = "dif_hm")



# Mapa de gênero - Homens -------------------------------------------------

#contagem
# data_recorte_gen_hex2 <- data_recorte_gen_hex %>%
#   mutate(homens = ifelse(homens > 1500, 1500, homens),
#          mulheres = ifelse(mulheres > 1500, 1500, mulheres),
#          dif_hm = ifelse(dif_hm > 200,200, ifelse(dif_hm < (-200), -200, dif_hm)))


#dourados
# data_recorte_gen_hex2 <- data_recorte_gen_hex %>%
#   mutate(homens = ifelse(homens > 800, 800, homens),
#          mulheres = ifelse(mulheres > 800, 800, mulheres),
#          dif_hm = ifelse(dif_hm > 100,100, ifelse(dif_hm < (-100), -100, dif_hm)))


data_recorte_gen_hex2 <- data_recorte_gen_hex %>%
  mutate(homens = ifelse(homens > munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         homens),
         mulheres = ifelse(mulheres > munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                           munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                           mulheres),
         dif_hm = ifelse(dif_hm > munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         ifelse(dif_hm < (-munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]),
                                -munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                                dif_hm)))


# boxplot(data_recorte_gen_hex$homens)
# boxplot(data_recorte_gen_hex$mulheres)
# boxplot(data_recorte_gen_hex$dif_hm)

#homens
map_pop_homens <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  geom_sf(data = st_transform(data_recorte_gen_hex2, 3857),
          aes(fill = homens),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  
  geom_sf(data = assentamentos,
          aes(color = "#2B6CB0"),
          size = 1.3,
          fill = '#2B6CB0',
          show.legend = "polygon",
          alpha = 0.3)+
  
  # ggnewscale::new_scale_color() +
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "#8f040e"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#2B6CB0", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#2B6CB0" = "#2B6CB0"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#2B6CB0" = "Ag. subnormais")
  )+
  
  
  scale_fill_gradientn(
    name = "Habitantes",
    colors = colors_orange ,
    
    space = "Lab",
    na.value = NA,
    # guide = "colourbar",
    aesthetics = "fill",
    breaks = seq(0,munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                 munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4),
    labels =c(as.character(seq(0,
                               munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]-munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4,
                               munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4)),
              paste0(">",munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)])),
    limits = c(0,munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)])
    # colors
  ) +
  ggtitle("Homens") +
  
  
  # ggnewscale::new_scale_color() +
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
  
  ggspatial::annotation_scale(style = "ticks",
                              location = "br",
                              text_family = "encode_sans_bold",
                              text_cex = 2,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.25, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  
  theme(
    strip.text.x = element_text(size=30, family = "encode_sans_bold"),
    strip.background = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.margin=unit(c(0,0,0,0),"mm"),
    legend.margin = margin(unit(c(2,2,1,2),"mm")),
    legend.key.width=unit(1,"line"),
    legend.key.height = unit(0.75,"line"),
    legend.key = element_blank(),
    legend.text=element_text(size=22, family = "encode_sans_light"),
    legend.title= element_text(size=24, family = "encode_sans_bold"),
    plot.title = element_text(size=24, family = "encode_sans_bold", vjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = c(0.50, -0.17),
    legend.box.background = element_rect(fill=alpha('white', 0.7),
                                         colour = "#A09C9C",
                                         linewidth = 0.2,
                                         linetype = "solid"),
    legend.background = element_blank(),
    # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
    #                                      colour = "#E0DFE3"),
    legend.spacing.y = unit(0.05, 'cm'),
    legend.box.just = "left",
    legend.direction = "horizontal"
    # legend.margin = margin(t = -80)
  ) +
  guides(colour = guide_legend(nrow = 3, order = 1,
                               override.aes = list(fill = c("#2B6CB0","white", "white"),
                                                   alpha = c(0.2,0.7,0.7)))) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)


# Mapa gênero - Mulehres ----------------------------------------------------

map_pop_mulheres <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  
  # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
  geom_sf(data = st_transform(data_recorte_gen_hex2, 3857),
          aes(fill = mulheres),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  geom_sf(data = assentamentos,
          aes(color = "#f6dc8d"),
          size = 1.3,
          fill = '#f6dc8d',
          show.legend = "polygon",
          alpha = 0.5)+
  
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "#8f040e"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#f6dc8d", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#f6dc8d" = "#f6dc8d"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#f6dc8d" = "Ag. subnormais")
  )+
  
  
  
  scale_fill_gradientn(
    name = "Habitantes",
    colors = colors_purple ,
    # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    # values = NULL,
    space = "Lab",
    na.value = NA,
    # guide = "colourbar",
    aesthetics = "fill",
    
    
    breaks = seq(0,munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                 munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4),
    labels =c(as.character(seq(0,
                               munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]-munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4,
                               munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4)),
              paste0(">",munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)])),
    limits = c(0,munis_recorte_limites$rec_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)])

    
  ) +
  ggtitle("Mulheres")+
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
  
  ggspatial::annotation_scale(style = "ticks",
                              location = "br",
                              text_family = "encode_sans_bold",
                              text_cex = 2,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.25, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  
  theme(
    strip.text.x = element_text(size=30, family = "encode_sans_bold"),
    strip.background = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.margin=unit(c(0,0,0,0),"mm"),
    legend.margin = margin(unit(c(2,2,1,2),"mm")),
    legend.key.width=unit(1,"line"),
    legend.key.height = unit(0.75,"line"),
    legend.key = element_blank(),
    legend.text=element_text(size=22, family = "encode_sans_light"),
    legend.title= element_text(size=24, family = "encode_sans_bold"),
    plot.title = element_text(size=24, family = "encode_sans_bold", vjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = c(0.50, -0.17),
    legend.box.background = element_rect(fill=alpha('white', 0.7),
                                         colour = "#A09C9C",
                                         linewidth = 0.2,
                                         linetype = "solid"),
    legend.background = element_blank(),
    
    legend.spacing.y = unit(0.05, 'cm'),
    legend.box.just = "left",
    legend.direction = "horizontal"
    # legend.margin = margin(t = -80)
  ) +
  guides(colour = guide_legend(nrow = 3, order = 1,
                               override.aes = list(fill = c("#f6dc8d","white", "white"),
                                                   alpha = c(0.2,0.7,0.7)))) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)






# Mapa diferença de homens e mulheres -------------------------------------



#necessário definir os limites e breaks

#mudar limite para cada cidade

# data_recorte_gen_hex2 <- data_recorte_gen_hex %>%
#   mutate(dif_hm = ifelse(dif_hm > 100, 100, ifelse(dif_hm < (-100), -100, dif_hm)))

map_pop_dif_gen <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  
  new_scale_fill() +
  
  geom_sf(data = st_transform(data_recorte_gen_hex2, 3857),
          aes(fill = dif_hm),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  
  geom_sf(data = assentamentos,
          aes(color = "#0f805e"),
          size = 1.3,
          fill = '#0f805e',
          show.legend = "polygon",
          alpha = 0.3)+
  
  
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "#8f040e"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.4)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#0f805e", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#0f805e" = "#0f805e"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#0f805e" = "Ag. subnormais")
  )+
  
  scale_fill_distiller("Habitantes",
                       type = "div",
                       palette = "RdBu",
                       breaks = seq(-munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                                    munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                                    munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]/2),
                       labels = c(paste0("<-",
                                       munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]),
                                       -munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]/2,
                                       "0",
                                       munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)]/2,
                                       paste0(">",
                                       munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)])),

                       limits = c(-munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                                  munis_recorte_limites$rec_dif_gen[which(munis_recorte_limites$abrev_muni == sigla_muni)])) +
  
  ggtitle("Diferença entre homens e mulheres")+
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
  
  ggspatial::annotation_scale(style = "ticks",
                              location = "br",
                              text_family = "encode_sans_bold",
                              text_cex = 2,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.25, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  theme(
    strip.text.x = element_text(size=30, family = "encode_sans_bold"),
    strip.background = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.margin=unit(c(0,0,0,0),"mm"),
    legend.margin = margin(unit(c(2,2,1,2),"mm")),
    legend.key.width=unit(1,"line"),
    legend.key.height = unit(0.75,"line"),
    legend.key = element_blank(),
    legend.text=element_text(size=22, family = "encode_sans_light"),
    legend.title= element_text(size=24, family = "encode_sans_bold"),
    plot.title = element_text(size=24, family = "encode_sans_bold", vjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = c(0.50, -0.17),
    legend.box.background = element_rect(fill=alpha('white', 0.7),
                                         colour = "#A09C9C",
                                         linewidth = 0.2,
                                         linetype = "solid"),
    legend.background = element_blank(),
    # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
    #                                      colour = "#E0DFE3"),
    legend.spacing.y = unit(0.05, 'cm'),
    legend.box.just = "left",
    legend.direction = "horizontal"
    # legend.margin = margin(t = -80)
  ) +
  guides(colour = guide_legend(nrow = 3, order = 1,
                               override.aes = list(fill = c("#0f805e","white", "white"),
                                                   alpha = c(0.2,0.7,0.7)))) +
  
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)








# Composição dos mapas em separado de genero ------------------------------

#ajustar legend.margin para a caixa da legenda ficar do mesmo tamanho da caixa do mapa
#margin(cima,direita, baixo, esquerda)

map_genero <- wrap_plots(map_pop_homens, map_pop_mulheres, map_pop_dif_gen, ncol = 3) &
  theme(plot.margin = unit(c(0,0,0,0),"mm"),
        legend.margin = margin(unit(c(2,4,1,3),"mm")) )


suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))


ggsave(map_genero,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/8-recorte_genero_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 15, height = 12, units = "cm" )



# Mapas de recorte de cor - Brancos ------------------------------------------------


# boxplot(data_recorte_cor2_hex$brancos)
# boxplot(data_recorte_cor2_hex$pretos)
# boxplot(data_recorte_cor2_hex$dif_bp)

#Contagem
# data_recorte_cor2_hex_map <- data_recorte_cor2_hex %>%
#   mutate(brancos = ifelse(brancos > 1000, 1000, brancos),
#          pretos = ifelse(pretos > 1500, 1500, pretos))

#Dourados
# data_recorte_cor2_hex_map <- data_recorte_cor2_hex %>%
#   mutate(brancos = ifelse(brancos > 500, 500, brancos),
#          pretos = ifelse(pretos > 500, 500, pretos))

data_recorte_cor2_hex_map <- data_recorte_cor2_hex %>%
  mutate(brancos = ifelse(brancos > munis_recorte_limites$rec_brancos[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         munis_recorte_limites$rec_brancos[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         brancos),
         pretos = ifelse(pretos > munis_recorte_limites$rec_pretos[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                           munis_recorte_limites$rec_pretos[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                           pretos),
         dif_bp = ifelse(dif_bp > munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         ifelse(dif_bp < (-munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)]),
                                -munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                                dif_bp)))

#brancos
map_pop_brancos <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  geom_sf(data = st_transform(data_recorte_cor2_hex_map, 3857),
          aes(fill = brancos),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  
  geom_sf(data = assentamentos,
          aes(color = "#2B6CB0"),
          size = 1.3,
          fill = '#2B6CB0',
          show.legend = "polygon",
          alpha = 0.3)+
  
  # ggnewscale::new_scale_color() +
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "#8f040e"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#2B6CB0", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#2B6CB0" = "#2B6CB0"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#2B6CB0" = "Ag. subnormais")
  )+
  
  
  scale_fill_gradientn(
    name = "Habitantes",
    colors = colors_orange ,

    space = "Lab",
    na.value = NA,
    # guide = "colourbar",
    aesthetics = "fill",
    
    
    breaks = seq(0,munis_recorte_limites$rec_brancos[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                 munis_recorte_limites$rec_brancos[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4),
    labels =c(as.character(seq(0,
                               munis_recorte_limites$rec_brancos[which(munis_recorte_limites$abrev_muni == sigla_muni)]-munis_recorte_limites$rec_brancos[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4,
                               munis_recorte_limites$rec_brancos[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4)),
              paste0(">",munis_recorte_limites$rec_brancos[which(munis_recorte_limites$abrev_muni == sigla_muni)])),
    limits = c(0,munis_recorte_limites$rec_brancos[which(munis_recorte_limites$abrev_muni == sigla_muni)])
    # 
    # breaks = seq(0,500,125),
    # labels =c(as.character(seq(0,375,125)), ">500"),
    # limits = c(0,500)
    # colors
  ) +
  ggtitle("Brancos") +
  
  
  # ggnewscale::new_scale_color() +
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
  
  ggspatial::annotation_scale(style = "ticks",
                              location = "br",
                              text_family = "encode_sans_bold",
                              text_cex = 2,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.35, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +

theme(
  strip.text.x = element_text(size=30, family = "encode_sans_bold"),
  strip.background = element_blank(),
  panel.background = element_rect(fill = NA, colour = NA),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(), 
  panel.grid = element_blank(),
  plot.margin=unit(c(0,0,0,0),"mm"),
  legend.margin = margin(unit(c(2,2,1,2),"mm")),
  legend.key.width=unit(1,"line"),
  legend.key.height = unit(0.75,"line"),
  legend.key = element_blank(),
  legend.text=element_text(size=22, family = "encode_sans_light"),
  legend.title= element_text(size=24, family = "encode_sans_bold"),
  plot.title = element_text(size=24, family = "encode_sans_bold", vjust = 0.5),
  strip.text = element_text(size = 10),
  legend.position = c(0.50, -0.17),
  legend.box.background = element_rect(fill=alpha('white', 0.7),
                                       colour = "#A09C9C",
                                       linewidth = 0.2,
                                       linetype = "solid"),
  legend.background = element_blank(),
  # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
  #                                      colour = "#E0DFE3"),
  legend.spacing.y = unit(0.05, 'cm'),
  legend.box.just = "left",
  legend.direction = "horizontal"
  # legend.margin = margin(t = -80)
) +
  guides(colour = guide_legend(nrow = 3, order = 1,
                               override.aes = list(fill = c("#2B6CB0","white", "white"),
                                                   alpha = c(0.2,0.7,0.7)))) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)

#negros

# Mapa Recorte de cor - Pretos --------------------------------------------

map_pop_pretos <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  
  # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
  geom_sf(data = st_transform(data_recorte_cor2_hex_map, 3857),
          aes(fill = pretos),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  geom_sf(data = assentamentos,
          aes(color = "#f6dc8d"),
          size = 1.3,
          fill = '#f6dc8d',
          show.legend = "polygon",
          alpha = 0.5)+
  
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "#8f040e"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#f6dc8d", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#f6dc8d" = "#f6dc8d"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#f6dc8d" = "Ag. subnormais")
  )+
  
  
  
  scale_fill_gradientn(
    name = "Habitantes",
    colors = colors_purple ,
    # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    # values = NULL,
    space = "Lab",
    na.value = NA,
    # guide = "colourbar",
    aesthetics = "fill",
    
    
    breaks = seq(0,munis_recorte_limites$rec_pretos[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                 munis_recorte_limites$rec_pretos[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4),
    labels =c(as.character(seq(0,
                               munis_recorte_limites$rec_pretos[which(munis_recorte_limites$abrev_muni == sigla_muni)]-munis_recorte_limites$rec_pretos[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4,
                               munis_recorte_limites$rec_pretos[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4)),
              paste0(">",munis_recorte_limites$rec_pretos[which(munis_recorte_limites$abrev_muni == sigla_muni)])),
    limits = c(0,munis_recorte_limites$rec_pretos[which(munis_recorte_limites$abrev_muni == sigla_muni)])
    # 
    # breaks = seq(0,500,125),
    # labels =c(as.character(seq(0,375,125)), ">500"),
    # limits = c(0,500)
    
  ) +
  ggtitle("Negros")+
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
  
  ggspatial::annotation_scale(style = "ticks",
                              location = "br",
                              text_family = "encode_sans_bold",
                              text_cex = 2,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.35, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  
  theme(
    strip.text.x = element_text(size=30, family = "encode_sans_bold"),
    strip.background = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.margin=unit(c(0,0,0,0),"mm"),
    legend.margin = margin(unit(c(2,2,1,2),"mm")),
    legend.key.width=unit(1,"line"),
    legend.key.height = unit(0.75,"line"),
    legend.key = element_blank(),
    legend.text=element_text(size=22, family = "encode_sans_light"),
    legend.title= element_text(size=24, family = "encode_sans_bold"),
    plot.title = element_text(size=24, family = "encode_sans_bold", vjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = c(0.50, -0.17),
    legend.box.background = element_rect(fill=alpha('white', 0.7),
                                         colour = "#A09C9C",
                                         linewidth = 0.2,
                                         linetype = "solid"),
    legend.background = element_blank(),

    legend.spacing.y = unit(0.05, 'cm'),
    legend.box.just = "left",
    legend.direction = "horizontal"
    # legend.margin = margin(t = -80)
  ) +
  guides(colour = guide_legend(nrow = 3, order = 1,
                               override.aes = list(fill = c("#f6dc8d","white", "white"),
                                                   alpha = c(0.2,0.7,0.7)))) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)





# Mapa de diferença entre brancos e pretos --------------------------------


#diferença entre brancos e pretos

#necessário definir os limites e breaks

#mudar limite para cada cidade

#Contagem
# data_recorte_cor2_hex2 <- data_recorte_cor2_hex %>%
#   mutate(dif_bp = ifelse(dif_bp < (-500), -500, ifelse(dif_bp > 500, 500, dif_bp)))

#dourados
# data_recorte_cor2_hex2 <- data_recorte_cor2_hex %>%
#   mutate(dif_bp = ifelse(dif_bp < (-200), -200, ifelse(dif_bp > 200, 200, dif_bp)))

map_pop_dif_cor <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+

  new_scale_fill() +

  geom_sf(data = st_transform(data_recorte_cor2_hex_map, 3857),
          aes(fill = dif_bp),
          colour = NA,
          alpha=.8,
          size = 0)+

  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  
  geom_sf(data = assentamentos,
          aes(color = "#0f805e"),
          size = 1.3,
          fill = '#0f805e',
          show.legend = "polygon",
          alpha = 0.3)+
  

geom_sf(data = simplepolys %>% st_transform(3857),
        # aes(size = 2),
        aes(color = "#8f040e"),
        # color = "grey45",
        # aes(fill = '#CFF0FF'),
        fill = NA,
        # stroke = 2,
        # size = 2,
        linewidth = 0.3,
        alpha= 0.4)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#0f805e", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#0f805e" = "#0f805e"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#0f805e" = "Ag. subnormais")
  )+

  scale_fill_distiller("Habitantes",
                       type = "div",
                       palette = "RdBu",
                       
                       breaks = seq(-munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                                    munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                                    munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)]/2),
                       labels = c(paste0("<-",
                                         munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)]),
                                  -munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)]/2,
                                  "0",
                                  munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)]/2,
                                  paste0(">",
                                         munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)])),
                       
                       limits = c(as.numeric(-munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)]),
                                  as.numeric(munis_recorte_limites$rec_dif_cor[which(munis_recorte_limites$abrev_muni == sigla_muni)]))
                       
                     # breaks = seq(-200,200,100),labels = c("<-500","-250","0","250",">500"),
                     # limits = c(-200,200)
                     ) +
  ggtitle("Diferença entre brancos e negros")+
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +

ggspatial::annotation_scale(style = "ticks",
                            location = "br",
                            text_family = "encode_sans_bold",
                            text_cex = 2,
                            line_width = 1,
                            width_hint = 0.10,
                            pad_x = unit(0.35, "cm"),
                            pad_y = unit(0.35, "cm")
) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
theme(
  strip.text.x = element_text(size=30, family = "encode_sans_bold"),
  strip.background = element_blank(),
  panel.background = element_rect(fill = NA, colour = NA),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(), 
  panel.grid = element_blank(),
  plot.margin=unit(c(0,0,0,0),"mm"),
  legend.margin = margin(unit(c(2,2,1,2),"mm")),
  legend.key.width=unit(1,"line"),
  legend.key.height = unit(0.75,"line"),
  legend.key = element_blank(),
  legend.text=element_text(size=22, family = "encode_sans_light"),
  legend.title= element_text(size=24, family = "encode_sans_bold"),
  plot.title = element_text(size=24, family = "encode_sans_bold", vjust = 0.5),
  strip.text = element_text(size = 10),
  legend.position = c(0.50, -0.17),
  legend.box.background = element_rect(fill=alpha('white', 0.7),
                                       colour = "#A09C9C",
                                       linewidth = 0.2,
                                       linetype = "solid"),
  legend.background = element_blank(),
  # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
  #                                      colour = "#E0DFE3"),
  legend.spacing.y = unit(0.05, 'cm'),
  legend.box.just = "left",
  legend.direction = "horizontal"
  # legend.margin = margin(t = -80)
) +
  guides(colour = guide_legend(nrow = 3, order = 1,
         override.aes = list(fill = c("#0f805e","white", "white"),
                             alpha = c(0.2,0.7,0.7)))) +

  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)



# Composição dos mapas de recorte de cor ----------------------------------


# map_genero <- map_pop_mulheres + map_pop_homens+ map_pop_dif_gen

#ajustar legend.margin para a caixa da legenda ficar do mesmo tamanho da caixa do mapa
#margin(cima,direita, baixo, esquerda)

map_cor <- wrap_plots(map_pop_brancos, map_pop_pretos, map_pop_dif_cor, ncol = 3) &
  theme(plot.margin = unit(c(0,0,0,0),"mm"),
        legend.margin = margin(unit(c(2,4,1,3),"mm")) )

suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))

ggsave(map_cor,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/9-recorte_cor_brancos-pretos_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 15, height = 12, units = "cm" )



# Mapa de recorte de cor - amarelos  ---------------------------

#amarelos

amarelos <- data_recorte_cor %>% filter(V0606 == "Amarelos")
amarelos2 <- amarelos %>% 
  mutate(n = ifelse(n > munis_recorte_limites$rec_amarelos[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                    munis_recorte_limites$rec_amarelos[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                    n))
# boxplot(amarelos$n)


map_pop_amarelos <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  geom_sf(data = st_transform(amarelos, 3857),
          aes(fill = n),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  
  geom_sf(data = assentamentos,
          aes(color = "#2B6CB0"),
          size = 1.3,
          fill = '#2B6CB0',
          show.legend = "polygon",
          alpha = 0.3)+
  
  # ggnewscale::new_scale_color() +
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "#8f040e"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#2B6CB0", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#2B6CB0" = "#2B6CB0"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#2B6CB0" = "Ag. subnormais")
  )+
  
  
  scale_fill_gradientn(
    name = "Habitantes",
    colors = colors_orange ,
    
    space = "Lab",
    na.value = NA,
    # guide = "colourbar",
    aesthetics = "fill",
    
    
    breaks = seq(0,munis_recorte_limites$rec_amarelos[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                 munis_recorte_limites$rec_amarelos[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4),
    labels =c(as.character(seq(0,
                               munis_recorte_limites$rec_amarelos[which(munis_recorte_limites$abrev_muni == sigla_muni)]-munis_recorte_limites$rec_amarelos[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4,
                               munis_recorte_limites$rec_amarelos[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4)),
              paste0(">",munis_recorte_limites$rec_amarelos[which(munis_recorte_limites$abrev_muni == sigla_muni)])),
    limits = c(0,munis_recorte_limites$rec_amarelos[which(munis_recorte_limites$abrev_muni == sigla_muni)])
    
    # breaks = seq(0,40,10),
    # labels =c(as.character(seq(0,30,10)), ">40"),
    # limits = c(0,40)
    # colors
  ) +
  ggtitle("Amarelos") +
  
  
  # ggnewscale::new_scale_color() +
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
  
  ggspatial::annotation_scale(style = "ticks",
                              location = "br",
                              text_family = "encode_sans_bold",
                              text_cex = 2,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.35, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  
  theme(
    strip.text.x = element_text(size=30, family = "encode_sans_bold"),
    strip.background = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.margin=unit(c(0,0,0,0),"mm"),
    legend.margin = margin(unit(c(2,2,1,2),"mm")),
    legend.key.width=unit(1,"line"),
    legend.key.height = unit(0.75,"line"),
    legend.key = element_blank(),
    legend.text=element_text(size=22, family = "encode_sans_light"),
    legend.title= element_text(size=24, family = "encode_sans_bold"),
    plot.title = element_text(size=24, family = "encode_sans_bold", vjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = c(0.50, -0.17),
    legend.box.background = element_rect(fill=alpha('white', 0.7),
                                         colour = "#A09C9C",
                                         linewidth = 0.2,
                                         linetype = "solid"),
    legend.background = element_blank(),
    # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
    #                                      colour = "#E0DFE3"),
    legend.spacing.y = unit(0.05, 'cm'),
    legend.box.just = "left",
    legend.direction = "horizontal"
    # legend.margin = margin(t = -80)
  ) +
  guides(colour = guide_legend(nrow = 3, order = 1,
                               override.aes = list(fill = c("#2B6CB0","white", "white"),
                                                   alpha = c(0.2,0.7,0.7)))) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)



# Mapa Indígenas ----------------------------------------------------------


indigenas <- data_recorte_cor %>% filter(V0606 == "Indígenas")
# boxplot(indigenas$n)
indigenas2 <- indigenas %>%
  mutate(n = ifelse(n > munis_recorte_limites$rec_indigenas[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                    munis_recorte_limites$rec_indigenas[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                    n))

map_pop_indigenas <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  
  # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
  geom_sf(data = st_transform(indigenas, 3857),
          aes(fill = n),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  geom_sf(data = assentamentos,
          aes(color = "#f6dc8d"),
          size = 1.3,
          fill = '#f6dc8d',
          show.legend = "polygon",
          alpha = 0.5)+
  
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "#8f040e"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#f6dc8d", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#f6dc8d" = "#f6dc8d"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#f6dc8d" = "Ag. subnormais")
  )+
  
  
  
  scale_fill_gradientn(
    name = "Habitantes",
    colors = colors_purple ,
    # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    # values = NULL,
    space = "Lab",
    na.value = NA,
    # guide = "colourbar",
    aesthetics = "fill",
    
    
    breaks = seq(0,munis_recorte_limites$rec_indigenas[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                 munis_recorte_limites$rec_indigenas[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4),
    labels =c(as.character(seq(0,
                               munis_recorte_limites$rec_indigenas[which(munis_recorte_limites$abrev_muni == sigla_muni)]-munis_recorte_limites$rec_indigenas[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4,
                               munis_recorte_limites$rec_indigenas[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4)),
              paste0(">",munis_recorte_limites$rec_indigenas[which(munis_recorte_limites$abrev_muni == sigla_muni)])),
    limits = c(0,munis_recorte_limites$rec_indigenas[which(munis_recorte_limites$abrev_muni == sigla_muni)])
    
    # breaks = seq(0,100,25),
    # labels =c(as.character(seq(0,75,25)), ">100"),
    # limits = c(0,100)

    
  ) +
  ggtitle("Indígenas")+
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
  
  ggspatial::annotation_scale(style = "ticks",
                              location = "br",
                              text_family = "encode_sans_bold",
                              text_cex = 2,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.35, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  
  theme(
    strip.text.x = element_text(size=30, family = "encode_sans_bold"),
    strip.background = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.margin=unit(c(0,0,0,0),"mm"),
    legend.margin = margin(unit(c(2,2,1,2),"mm")),
    legend.key.width=unit(1,"line"),
    legend.key.height = unit(0.75,"line"),
    legend.key = element_blank(),
    legend.text=element_text(size=22, family = "encode_sans_light"),
    legend.title= element_text(size=24, family = "encode_sans_bold"),
    plot.title = element_text(size=24, family = "encode_sans_bold", vjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = c(0.50, -0.17),
    legend.box.background = element_rect(fill=alpha('white', 0.7),
                                         colour = "#A09C9C",
                                         linewidth = 0.2,
                                         linetype = "solid"),
    legend.background = element_blank(),
    
    legend.spacing.y = unit(0.05, 'cm'),
    legend.box.just = "left",
    legend.direction = "horizontal"
    # legend.margin = margin(t = -80)
  ) +
  guides(colour = guide_legend(nrow = 3, order = 1,
                               override.aes = list(fill = c("#f6dc8d","white", "white"),
                                                   alpha = c(0.2,0.7,0.7)))) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)






# Composição dos mapas de recorte de cor amarelos e indigenas----------------------------------


# map_genero <- map_pop_mulheres + map_pop_homens+ map_pop_dif_gen

#ajustar legend.margin para a caixa da legenda ficar do mesmo tamanho da caixa do mapa
#margin(cima,direita, baixo, esquerda)

map_am_in <- wrap_plots(map_pop_amarelos, map_pop_indigenas, ncol = 2) &
  theme(plot.margin = unit(c(0,0,0,0),"mm"),
        legend.margin = margin(unit(c(2,4,1,3),"mm")) )

suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))

ggsave(map_am_in,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/10-recorte_cor_amarelos-indigenas_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 10, height = 12, units = "cm" )




# recorte de genero do responsável homens ----------------------------------------

# boxplot(data_recorte_gen_resp_hex$resp_homens)
# boxplot(data_recorte_gen_resp_hex$resp_mulheres)
# boxplot(data_recorte_gen_resp_hex$dif_resp_hm)
# mapview(data_recorte_gen_resp_hex)

# data_recorte_gen_resp_hex2 <- data_recorte_gen_resp_hex %>%
#   mutate(resp_homens = ifelse(resp_homens > 160, 160, resp_homens),
#          resp_mulheres = ifelse(resp_mulheres > 160, 160, resp_mulheres),
#          dif_resp_hm = ifelse(dif_resp_hm > 80, 80, ifelse(dif_resp_hm < (-80), -80, dif_resp_hm)))


data_recorte_gen_resp_hex2 <- data_recorte_gen_resp_hex %>%
  mutate(resp_homens = ifelse(resp_homens > munis_recorte_limites$rec_resp_h[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         munis_recorte_limites$rec_resp_h[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         resp_homens),
         resp_mulheres = ifelse(resp_mulheres > munis_recorte_limites$res_resp_m[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                           munis_recorte_limites$res_resp_m[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                           resp_mulheres),
         dif_resp_hm = ifelse(dif_resp_hm > munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                         ifelse(dif_resp_hm < (-munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)]),
                                -munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                                dif_resp_hm)))


#Resp homens
map_pop_resp_homens <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  geom_sf(data = st_transform(data_recorte_gen_resp_hex2, 3857),
          aes(fill = resp_homens),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  
  geom_sf(data = assentamentos,
          aes(color = "#2B6CB0"),
          size = 1.3,
          fill = '#2B6CB0',
          show.legend = "polygon",
          alpha = 0.3)+
  
  # ggnewscale::new_scale_color() +
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "#8f040e"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#2B6CB0", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#2B6CB0" = "#2B6CB0"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#2B6CB0" = "Ag. subnormais")
  )+
  
  
  scale_fill_gradientn(
    name = "Habitantes",
    colors = colors_orange ,
    # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    # values = NULL,
    space = "Lab",
    na.value = NA,
    # guide = "colourbar",
    aesthetics = "fill",
    
    
    breaks = seq(0,munis_recorte_limites$rec_resp_h[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                 munis_recorte_limites$rec_resp_h[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4),
    labels =c(as.character(seq(0,
                               munis_recorte_limites$rec_resp_h[which(munis_recorte_limites$abrev_muni == sigla_muni)]-munis_recorte_limites$rec_resp_h[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4,
                               munis_recorte_limites$rec_resp_h[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4)),
              paste0(">",munis_recorte_limites$rec_resp_h[which(munis_recorte_limites$abrev_muni == sigla_muni)])),
    limits = c(0,munis_recorte_limites$rec_resp_h[which(munis_recorte_limites$abrev_muni == sigla_muni)])
    
    # breaks = seq(0,160,40),
    # labels =c(as.character(seq(0,120,40)), ">160"),
    # limits = c(0,160)
    
  ) +
  ggtitle("Responsáveis homens") +
  
  
  # ggnewscale::new_scale_color() +
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
  
  ggspatial::annotation_scale(style = "ticks",
                              location = "br",
                              text_family = "encode_sans_bold",
                              text_cex = 2,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.25, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  
  theme(
    strip.text.x = element_text(size=30, family = "encode_sans_bold"),
    strip.background = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.margin=unit(c(0,0,0,0),"mm"),
    legend.margin = margin(unit(c(2,2,1,2),"mm")),
    legend.key.width=unit(1,"line"),
    legend.key.height = unit(0.75,"line"),
    legend.key = element_blank(),
    legend.text=element_text(size=22, family = "encode_sans_light"),
    legend.title= element_text(size=24, family = "encode_sans_bold"),
    plot.title = element_text(size=24, family = "encode_sans_bold", vjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = c(0.50, -0.17),
    legend.box.background = element_rect(fill=alpha('white', 0.7),
                                         colour = "#A09C9C",
                                         linewidth = 0.2,
                                         linetype = "solid"),
    legend.background = element_blank(),
    # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
    #                                      colour = "#E0DFE3"),
    legend.spacing.y = unit(0.05, 'cm'),
    legend.box.just = "left",
    legend.direction = "horizontal"
    # legend.margin = margin(t = -80)
  ) +
  guides(colour = guide_legend(nrow = 3, order = 1,
                               override.aes = list(fill = c("#2B6CB0","white", "white"),
                                                   alpha = c(0.2,0.7,0.7)))) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)


# Mapa gênero resp - mulheres ----------------------------------------------------


map_pop_resp_mulheres <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  
  # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
  geom_sf(data = st_transform(data_recorte_gen_resp_hex2, 3857),
          aes(fill = resp_mulheres),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  geom_sf(data = assentamentos,
          aes(color = "#f6dc8d"),
          size = 1.3,
          fill = '#f6dc8d',
          show.legend = "polygon",
          alpha = 0.5)+
  
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "#8f040e"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#f6dc8d", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#f6dc8d" = "#f6dc8d"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#f6dc8d" = "Ag. subnormais")
  )+
  
  
  
  scale_fill_gradientn(
    name = "Habitantes",
    colors = colors_purple ,
    # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    # values = NULL,
    space = "Lab",
    na.value = NA,
    # guide = "colourbar",
    aesthetics = "fill",
    
    
    breaks = seq(0,munis_recorte_limites$res_resp_m[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                 munis_recorte_limites$res_resp_m[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4),
    labels =c(as.character(seq(0,
                               munis_recorte_limites$res_resp_m[which(munis_recorte_limites$abrev_muni == sigla_muni)]-munis_recorte_limites$res_resp_m[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4,
                               munis_recorte_limites$res_resp_m[which(munis_recorte_limites$abrev_muni == sigla_muni)]/4)),
              paste0(">",munis_recorte_limites$res_resp_m[which(munis_recorte_limites$abrev_muni == sigla_muni)])),
    limits = c(0,munis_recorte_limites$res_resp_m[which(munis_recorte_limites$abrev_muni == sigla_muni)])
    
    # breaks = seq(0,160,40),
    # labels =c(as.character(seq(0,120,40)), ">160"),
    # limits = c(0,160)
    
  ) +
  ggtitle("Responsáveis mulheres")+
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
  
  ggspatial::annotation_scale(style = "ticks",
                              location = "br",
                              text_family = "encode_sans_bold",
                              text_cex = 2,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.25, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  
  theme(
    strip.text.x = element_text(size=30, family = "encode_sans_bold"),
    strip.background = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.margin=unit(c(0,0,0,0),"mm"),
    legend.margin = margin(unit(c(2,2,1,2),"mm")),
    legend.key.width=unit(1,"line"),
    legend.key.height = unit(0.75,"line"),
    legend.key = element_blank(),
    legend.text=element_text(size=22, family = "encode_sans_light"),
    legend.title= element_text(size=24, family = "encode_sans_bold"),
    plot.title = element_text(size=24, family = "encode_sans_bold", vjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = c(0.50, -0.17),
    legend.box.background = element_rect(fill=alpha('white', 0.7),
                                         colour = "#A09C9C",
                                         linewidth = 0.2,
                                         linetype = "solid"),
    legend.background = element_blank(),
    
    legend.spacing.y = unit(0.05, 'cm'),
    legend.box.just = "left",
    legend.direction = "horizontal"
    # legend.margin = margin(t = -80)
  ) +
  guides(colour = guide_legend(nrow = 3, order = 1,
                               override.aes = list(fill = c("#f6dc8d","white", "white"),
                                                   alpha = c(0.2,0.7,0.7)))) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)



# Mapa diferença de resp homens e mulheres -------------------------------------



#necessário definir os limites e breaks

#mudar limite para cada cidade

# data_recorte_gen_resp_hex2 <- data_recorte_gen_resp_hex %>%
#   mutate(dif_resp_hm = ifelse(dif_resp_hm < (-200), -200, ifelse(dif_resp_hm > 200, 200, dif_resp_hm)))


map_pop_dif_resp_gen <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  
  new_scale_fill() +
  
  geom_sf(data = st_transform(data_recorte_gen_resp_hex2, 3857),
          aes(fill = dif_resp_hm),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  
  geom_sf(data = assentamentos,
          aes(color = "#0f805e"),
          size = 1.3,
          fill = '#0f805e',
          show.legend = "polygon",
          alpha = 0.3)+
  
  
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "#8f040e"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.4)  +
  scale_color_manual(name = "Uso do solo",
                     breaks = c("#0f805e", "#8f040e", "grey60"),
                     values = c("#8f040e" = "#8f040e",
                                "grey60" = "grey60",
                                "#0f805e" = "#0f805e"),
                     label = c("#8f040e" = "Área urbanizada",
                               "grey60" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                               "#0f805e" = "Ag. subnormais")
  )+
  
  scale_fill_distiller("Habitantes",
                       type = "div",
                       palette = "RdBu",
                       
                       breaks = seq(-munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                                    munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)],
                                    munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)]/2),
                       labels = c(paste0("<-",
                                         munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)]),
                                  -munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)]/2,
                                  "0",
                                  munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)]/2,
                                  paste0(">",
                                         munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)])),
                       
                       limits = c(as.numeric(-munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)]),
                                  as.numeric(munis_recorte_limites$rec_dif_resp[which(munis_recorte_limites$abrev_muni == sigla_muni)]))
                       
                       # breaks = seq(-80,80,40),labels = c("<-80","-40","0","40",">80"),
                       # limits = c(-80,80)
                       ) +
  
  ggtitle("Diferença de resp. homens e mulheres")+
  
  geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
  
  ggspatial::annotation_scale(style = "ticks",
                              location = "br",
                              text_family = "encode_sans_bold",
                              text_cex = 2,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.25, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
  theme(
    strip.text.x = element_text(size=30, family = "encode_sans_bold"),
    strip.background = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.margin=unit(c(0,0,0,0),"mm"),
    legend.margin = margin(unit(c(2,2,1,2),"mm")),
    legend.key.width=unit(1,"line"),
    legend.key.height = unit(0.75,"line"),
    legend.key = element_blank(),
    legend.text=element_text(size=22, family = "encode_sans_light"),
    legend.title= element_text(size=24, family = "encode_sans_bold"),
    plot.title = element_text(size=23, family = "encode_sans_bold", vjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = c(0.50, -0.17),
    legend.box.background = element_rect(fill=alpha('white', 0.7),
                                         colour = "#A09C9C",
                                         linewidth = 0.2,
                                         linetype = "solid"),
    legend.background = element_blank(),
    # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
    #                                      colour = "#E0DFE3"),
    legend.spacing.y = unit(0.05, 'cm'),
    legend.box.just = "left",
    legend.direction = "horizontal"
    # legend.margin = margin(t = -80)
  ) +
  guides(colour = guide_legend(nrow = 3, order = 1,
                               override.aes = list(fill = c("#0f805e","white", "white"),
                                                   alpha = c(0.2,0.7,0.7)))) +
  
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)



# Composição dos mapas de recorte de cor ----------------------------------

#ajustar legend.margin para a caixa da legenda ficar do mesmo tamanho da caixa do mapa
#margin(cima,direita, baixo, esquerda)

map_resp_genero <- wrap_plots(map_pop_resp_homens, map_pop_resp_mulheres, map_pop_dif_resp_gen, ncol = 3) &
  theme(plot.margin = unit(c(0,0,0,0),"mm"),
        legend.margin = margin(unit(c(2,4,1,3),"mm")) )


# Composição dos mapas em separado de genero ------------------------------


suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))


ggsave(map_resp_genero,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/11-recorte_genero_responsavel_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 15, height = 12, units = "cm" )



# Mapas de recorte.Antigo. Isolar -----------------------------------------
# 
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     data_complete <- data_mhex %>% left_join(data_muni, by = c("id_hex"="Cod_setor")) %>% 
#       mutate(area = st_area(.)/10^6) %>%
#       rowwise() %>% 
#       mutate(Ptot_mulheres = sum(P006,P007,P008,P009,P010),
#              Ptot_homens = sum(P001,P002,P003,P004,P005))
#     
# 
#     # mulheres 
#     
#     teste_m <- data_complete %>% select(code_tract,P006,P007,P008,P009,P010,Ptot_mulheres) %>% 
#       gather(key = dado,value = valor, 2:7) %>% 
#       mutate(valor = as.numeric(valor))
#      colors_blue <- c("#F1F2FE","#9FA4F9","#767DCE","#21367D","#1A295B")
#      
#      colors_orange <- c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
#      
# 
#      map_m <- ggplot() +
#        geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
#        coord_equal() +
#        scale_fill_identity()+
#        # nova escala
#        new_scale_fill() +
#        # theme_map() +
#        geom_sf(data = st_transform(teste_m,3857),aes(fill = valor),color = NA) +
#        
#        geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .7) +
#        
#        facet_wrap(~dado, labeller = labeller_grupos) +
#        scale_fill_gradientn(
#          name = "Nº de Habitantes",
#          colors =colors_blue ,
#          # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
#          # values = NULL,
#          space = "Lab",
#          na.value = NA,
#          # guide = "colourbar",
#          aesthetics = "fill",
#          # colors
#        ) +
#        # tema_populacao()
#        theme(legend.position = "bottom",
#              strip.text.x = element_text(size=rel(1.2)),
#              strip.background = element_rect(
#                color = NA,
#                fill = "#eff0f0"
#              ),
#              panel.background = element_rect(fill = NA, colour = NA),
#              axis.text = element_blank(),
#              axis.title = element_blank(),
#              axis.ticks = element_blank(), 
#              panel.grid = element_blank(),
#              plot.margin=unit(c(2,0,0,0),"mm"),
#              legend.key.width=unit(2,"line"),
#              legend.key.height = unit(.5,"cm"),
#              legend.text=element_text(size=rel(1)),
#              legend.title=element_text(size=rel(1),                                   ),
#              plot.title = element_text(hjust = 0, vjust = 4),
#              strip.text = element_text(size = 10)
#        )
#      
#     
#     
#     
#     # homens 
#     
#     teste_h <- data_complete %>% select(code_tract,P001,P002,P003,P004,P005,Ptot_homens) %>% 
#       gather(key = dado,value = valor, 2:7) %>% 
#       mutate(valor = as.numeric(valor))
#     
#     
#     
#     
#     
#     map_h <- ggplot() +
#       geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
#       coord_equal() +
#       scale_fill_identity()+
#       # nova escala
#       new_scale_fill() +
#       # theme_map() +
#       geom_sf(data = st_transform(teste_h,3857),aes(fill = valor),color = NA) +
# 
#       geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .7) +
#       
#       facet_wrap(~dado, labeller = labeller_grupos) +
#       scale_fill_gradientn(
#         name = "Nº de Habitantes",
#         colors =colors_orange ,
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
#             legend.text=element_text(size=rel(1)),
#             legend.title=element_text(size=rel(1),                                   ),
#             plot.title = element_text(hjust = 0, vjust = 4),
#             strip.text = element_text(size = 10)
#       )
#     
#     #Escrita do png do mapa
#     
#     ggsave(map_m,
#            device = "png",
#            filename =  sprintf("../data/map_plots/muni_%s/1-populacao_mulheres_%s.png", sigla_muni, sigla_muni),
#            dpi = 300,
#            width = width, height = height, units = "cm" )
#     
#     ggsave(map_h,
#            device = "png",
#            filename =  sprintf("../data/map_plots/muni_%s/2-populacao_homens_%s.png", sigla_muni, sigla_muni),
#            dpi = 300,
#            width = width, height = height, units = "cm" )

# Aplica a função ---------------------------------------------------------


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
# features <-as.data.frame( available_features ())


  








