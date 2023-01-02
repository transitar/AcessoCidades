
# rm(list = ls())


source('./R/fun/setup.R')
sf_use_s2(use_s2 = TRUE)

library(showtext)
# library(ggmap)
library(ggspatial)
library(patchwork)
showtext_auto()
width <- 16.5
height <- 16.5
# font_add_google("Encode Sans Light 300")
font_add("encode_sans", '../data/fontes/EncodeSans-VariableFont_wdth,wght.ttf')
font_add("encode_sans_regular", '../data/fontes/EncodeSans-Regular.ttf')
font_add("encode_sans_bold", '../data/fontes/EncodeSans-Bold.ttf')
font_add("encode_sans_light", '../data/fontes/EncodeSans-Light.ttf')
sigla_muni <- 'pal'

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
              aes(color = "grey60"),
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
            aes(color = "#e1ffce"),
            # color = "grey45",
            # aes(fill = '#CFF0FF'),
            fill = NA,
            # stroke = 2,
            # size = 2,
            linewidth = 0.8,
            alpha= 0.7)  +
      scale_color_manual(name = "Uso do solo",
                         values = c("#e1ffce" = "#e1ffce",
                                    "grey60" = "grey60"),
                         label = c("#e1ffce" = "Área urbanizada",
                                   "grey60" = "Bairros")
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
              aes(color = "grey60"),
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
            aes(color = "#bfa5e7"),
            # color = "grey45",
            # aes(fill = '#CFF0FF'),
            fill = NA,
            # stroke = 2,
            # size = 2,
            linewidth = 0.8,
            alpha= 0.7)  +
      scale_color_manual(name = "Uso do solo",
                         values = c("#bfa5e7" = "#bfa5e7",
                                    "grey60" = "grey60"),
                         label = c("#bfa5e7" = "Área urbanizada",
                                   "grey60" = "Bairros")
      )+
      
      
      # scale_fill_gradientn(
      #   name = "Habitantes",
      #   colors =colors_acc ,
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   # colors
      # ) +
    scale_fill_distiller("Renda per capita (SM)",
                         type = "div",
                         palette = "GnBu",
                         direction = 1,
                         breaks = seq(0,10,2),
                         labels = c("0","2", "4", "6","8", ">10"),
                         limits = c(0,10)) +
      
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
      aproxima_muni(sigla_muni = sigla_muni)    
    
    
    
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    
    ggsave(map_renda,
           device = "png",
           filename =  sprintf('../data/map_plots_population/muni_%s/2-renda_per_capita_%s.png',
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
# mapview(data_recorte_gen_hex, zcol = "dif_hm")


# Mapas de recorte de gênero ----------------------------------------------

#homens
map_pop_homens <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  geom_sf(data = st_transform(data_recorte_gen_hex, 3857),
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
          linewidth = 0.7,
          alpha= 0.7) +
  
# ggnewscale::new_scale_color() +
geom_sf(data = simplepolys %>% st_transform(3857),
        # aes(size = 2),
        aes(color = "#e1ffce"),
        # color = "grey45",
        # aes(fill = '#CFF0FF'),
        fill = NA,
        # stroke = 2,
        # size = 2,
        linewidth = 0.5,
        alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     values = c("#e1ffce" = "#e1ffce",
                                "grey60" = "grey60"),
                     label = c("#e1ffce" = "Área urbanizada",
                               "grey60" = "Bairros")
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
                              pad_y = unit(0.35, "cm")
  ) +
  ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0),
                                    location = "tr",
                                    height = unit(0.5, "cm"),
                                    width = unit(0.5, "cm")) +
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
  plot.title = element_text(size=30, family = "encode_sans_bold", vjust = 0.5),
  strip.text = element_text(size = 10),
  legend.position = c(0.50, -0.15),
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
  guides(colour = guide_legend(nrow = 2)) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)

#mulheres

map_pop_mulheres <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  
  # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
  geom_sf(data = st_transform(data_recorte_gen_hex, 3857),
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
          linewidth = 0.7,
          alpha= 0.7) +
  
geom_sf(data = simplepolys %>% st_transform(3857),
        # aes(size = 2),
        aes(color = "#e1ffce"),
        # color = "grey45",
        # aes(fill = '#CFF0FF'),
        fill = NA,
        # stroke = 2,
        # size = 2,
        linewidth = 0.5,
        alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     values = c("#e1ffce" = "#e1ffce",
                                "grey60" = "grey60"),
                     label = c("#e1ffce" = "Área urbanizada",
                               "grey60" = "Bairros")
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
    plot.title = element_text(size=30, family = "encode_sans_bold", vjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = c(0.50, -0.15),
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
  guides(colour = guide_legend(nrow = 2)) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)





#diferença de homens -  mulheres



map_pop_dif_gen <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  
  # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
  geom_sf(data = st_transform(data_recorte_gen_hex, 3857),
          aes(fill = dif_hm),
          colour = NA,
          alpha=.8,
          size = 0)+
  
  # labs(color = 'Infraestrutura Cicloviária',
  #      fill = 'População') +
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey60"),
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
        aes(color = "#e1ffce"),
        # color = "grey45",
        # aes(fill = '#CFF0FF'),
        fill = NA,
        # stroke = 2,
        # size = 2,
        linewidth = 0.5,
        alpha= 0.7)  +
  scale_color_manual(name = "Uso do solo",
                     values = c("#e1ffce" = "#e1ffce",
                                "grey60" = "grey60"),
                     label = c("#e1ffce" = "Área urbanizada",
                               "grey60" = "Bairros")
  )+
  
  
  # scale_fill_gradientn(
  #   name = "Habitantes",
  #   colors = colors_orange ,
  #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
  #   # values = NULL,
  #   space = "Lab",
  #   na.value = NA,
  #   # guide = "colourbar",
  #   aesthetics = "fill",
  #   # colors
  # ) +
  
scale_fill_distiller("Habitantes",type = "div",palette = "RdBu",
                     breaks = seq(-100,100,50),labels = c("<-100","-50","0","50",">100"),
                     limits = c(-100,100)) +
  ggtitle("Diferença entre homens e mulheres")+
  
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
  plot.title = element_text(size=30, family = "encode_sans_bold", vjust = 0.5),
  strip.text = element_text(size = 10),
  legend.position = c(0.50, -0.15),
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
  guides(colour = guide_legend(nrow = 2)) +
  # guides(fill = guide_legend(byrow = TRUE)) +
  aproxima_muni_recortes(sigla_muni = sigla_muni)


# Composição dos mapas em separado de genero ------------------------------


# map_genero <- map_pop_mulheres + map_pop_homens+ map_pop_dif_gen

suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))




map_genero <- wrap_plots(map_pop_mulheres, map_pop_homens, map_pop_dif_gen, ncol = 3)# &
  # plot_layout(#ncol = 5,
  #             # widths = c(4, -0.5 , 4 , -0.5, 4 ),
  #             # heights = c(20,2),
  #             guides = "collect") &
  # theme(legend.position = c(0.5,0.1),
  #       legend.direction = "horizontal",
  #       legend.key.width = unit(0.2, "line"),
  #       legend.key.height = unit(0.1, "cm"),
  #       # panel.spacing = ,
  #       # legend.margin = margin(t=-70),
  #       # strip.background = element_blank(),
  #       # legend.box.margin = margin(t=-100),
  #       legend.box.background = element_blank()
  #       # legend.box.background = element_rect(fill = "white", colour = "black")
  # 
  #         # = element_rect(fill = "white", colour = "black",) #,  margin(t=-10, unit = "mm"))
  #       ) #&
  # plot_annotation(theme = theme(plot.background = element_rect_round(color  = '#5766cc',
  #                                                                    size = 1.2,
  #                                                                    linetype = "solid",
  #                                                                    radius = unit(0.10, "snpc"))))
ggsave(map_genero,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/8-recorte_genero_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 15, height = 10, units = "cm" )
# Mapas de recorte.Antigo. Isolar -----------------------------------------

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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
  








