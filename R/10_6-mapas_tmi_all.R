#10_6-mapas_tmi_all
# rm(list = ls(all.names = T))

source('./R/fun/setup.R')
library(patchwork)
library(showtext)
# library(elementalist)
options(scipen = 100000000)
width <- 14
height <- 10

sigla_muni <- 'poa'
mode1 <- "transit"
oportunidade <- "escolas"
titulo_leg <- "Escolas"
sigla_op <- "EF"
time <- 30
# time <- c(15,30,45,60)
type_acc <- "TMI"


#oportunidade = nome da pasta para salvar

# theme_for_TMI <- function(base_size) {
#   
#   # theme_void(base_family="Roboto Condensed") %+replace%
#   theme_void() %+replace%
#     
#     theme(
#       legend.position = "bottom",
#       plot.margin=unit(c(2,0,0,0),"mm"),
#       legend.key.width=unit(1,"line"),
#       legend.key.height = unit(0.4,"cm"),
#       legend.text=element_text(size=rel(0.4)),
#       legend.title=element_text(size=rel(0.5)),
#       plot.title = element_text(hjust = 0, vjust = 4, size = rel(0.6)),
#       strip.text = element_text(size = rel(0.6))
#       # legend.key.width=unit(0.5,"cm")
#       
#     )
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
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4, size = rel(0.6)),
      strip.text = element_text(size = rel(0.6))
      # legend.key.width=unit(0.5,"cm")

    )
}

theme_for_CMA_1map <- function(base_size) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.key.width=unit(1,"line"),
      legend.key.height = unit(0.4,"cm"),
      legend.text=element_text(size=rel(0.5), angle = 0, vjust = 0),
      legend.title=element_text(size=rel(0.6)),
      strip.text = element_blank()
      # strip.text = element_text(size=rel(0.9))
      # plot.title = element_text(size = rel(1.5)),
      
      
      
    )
}

theme_for_CMA_2maps <- function(base_size) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(0.4,"cm"),
      legend.text=element_text(size=rel(0.6)),
      legend.title=element_text(size=rel(0.9)),
      strip.text = element_text(size=rel(0.7))
      # plot.title = element_text(hjust = 0, vjust = 4),
      
      
      
    )
}

theme_for_CMA_4maps <- function(base_size) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1,"line"),
      legend.key.height = unit(0.25,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.7)),
      strip.text = element_text(size=rel(0.9))
      # plot.title = element_text(hjust = 0, vjust = 4),
      
      
      
    )
}

tema_TMI <- function(base_size) {
  
  
  theme(
    strip.text.x = element_text(size=rel(1.2)),
    strip.background = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    plot.margin=unit(c(0,0,0,0),"mm"),
    legend.margin = margin(unit(c(10,10,10,10),"mm")),
    legend.key.width=unit(2,"line"),
    legend.key.height = unit(1,"line"),
    legend.key = element_blank(),
    legend.text=element_text(size=30, family = "encode_sans_light"),
    legend.title= ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
    plot.title = element_text(hjust = 0, vjust = 4),
    strip.text = element_text(size = 10),
    legend.position = c(0.19, 0.25),
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
  )
  
  
  
}

#retirar type_acc; oportunidade, titulo_leg

#sigla_muni: sigla do municipio
#type_acc: CMA ou TMI (TMI ainda nao funcionando)
#oportunidade: nome da pasta para ser salva em /CMA/
#sigla_op: sigla do tipo de oportunidade: TT, ST, SB, SM, SA, ET, EI, EF, EM, MT, MI, MF, MM, LZ, PR, BK
#titulo_leg: palavra a ser colocada na legenda %s\nAcessiveis
#time: tempo maximo: 15, 30, 45, 60, ou 75 ou combinacao de tempos com c(15,30,45,60)
#funciona apenas para 1 ou 4 tempos
#cols: numero de colunas do facet



mapas_tmi <- function(sigla_muni,
                      type_acc,
                      mode1,
                      oportunidade,
                      sigla_op,
                      titulo_leg,
                      time = 45,
                      cols = 1,
                      width = 16.5,
                      height = 16.5){
  
  showtext_auto()
  font_add("encode_sans", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-VariableFont_wdth,wght.ttf')
  font_add("encode_sans_regular", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Regular.ttf')
  font_add("encode_sans_bold", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Bold.ttf')
  font_add("encode_sans_light", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Light.ttf')
  
  message(paste("Rodando",sigla_muni, "\n"))
  
  path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
  
  dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
  
  path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
  
  data_contorno <- read_rds(path_contorno)
  
  map_tiles <- read_rds(path_maptiles)
  
  sigla_municipio <- sigla_muni
  decisao_muni <- read_excel('../planilha_municipios.xlsx',
                             sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
  area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
                                     sigla_muni)) %>% filter(DN == 24) %>%
    st_make_valid() %>%
    st_union()
  # area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
  #                                    sigla_muni)) %>% filter(DN == 24) %>%
  #   st_make_valid() %>%
  #   st_union()
  # mapview(simplepolys)
  
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
  
  rm(dados_simulacao)
  gc()
  
  dados_areas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                 sigla_muni, sigla_muni), layer= "areas") %>% st_transform(decisao_muni$epsg)
  
  if (type_acc == "CMA"){
    
    data_acess <- read_rds(sprintf('../r5r/accessibility/muni_%s/acc_%s.rds',
                                   sigla_muni, sigla_muni))
    
    
    dados_acc <- left_join(dados_hex, data_acess, by = c("id_hex"="origin")) %>% st_as_sf()
    
  } else if(type_acc == "TMI") {
    
    data_acess_tmi <- read_rds(sprintf('../r5r/accessibility/muni_%s/tmi/acc_tmi_%s.rds',
                                       sigla_muni, sigla_muni))
    
    
    dados_acc <- left_join(dados_hex, data_acess_tmi, by = c("id_hex"="origin")) %>% st_as_sf()
    DT <- dados_acc
    
    #inserir drop na em cada gráfico
    dados_acc_maps <- dados_acc %>% mutate_if(is.numeric, list(~na_if(., Inf)))
    dados_acc_maps <- dados_acc_maps %>% mutate_if(is.numeric, list(~na_if(., -Inf)))
    dados_acc <- dados_acc_maps
  }
  
  #totais de oportunidades
  # dados_acc %>% distinct_all() %>% nrow()
  # abrir acess
  acess <- dados_acc #%>% filter(sigla_muni == sigla_munii)
  
  # dados_hex <- readr::read_rds(sprintf("../data/dados_hex/muni_%s/dados_hex_%s.rds",
  #                                      sigla_muni, sigla_muni))
  # 
  # empregos_tot <- sum(dados_hex$n_jobs, na.rm = T)
  # saude_tot <- sum(dados_hex$S001, na.rm = T)
  # saude_n1 <- sum(dados_hex$S002, na.rm = T)
  # saude_n2 <- sum(dados_hex$S003, na.rm = T)
  # saude_n3 <- sum(dados_hex$S004, na.rm = T)
  # educacao_tot <- sum(dados_hex$E001, na.rm = T)
  # educacao_n1 <- sum(dados_hex$E002, na.rm = T)
  # educacao_n2 <- sum(dados_hex$E003, na.rm = T)
  # educacao_n3 <- sum(dados_hex$E004, na.rm = T)
  # matriculas_tot <- sum(dados_hex$M001, na.rm = T)
  # matriculas_n1 <- sum(dados_hex$M002, na.rm = T)
  # matriculas_n2 <- sum(dados_hex$M003, na.rm = T)
  # matriculas_n3 <- sum(dados_hex$M004, na.rm = T)
  # lazer_tot2 <- sum(dados_hex$lazer_tot, na.rm = T)
  # paraciclos_tot <- sum(dados_hex$paraciclos, na.rm = T)
  # bikes_comp_tot <- sum(dados_hex$n_bikes, na.rm = T)
  
  
  
  # acess2 <- acess %>%
  #   mutate(across(.cols = matches("CMATT"),
  #                 ~ .x/empregos_tot)) %>%
  #   mutate(across(.cols = matches("CMAST"),
  #                 ~ .x/saude_tot)) %>%
  #   mutate(across(.cols = matches("CMASB"),
  #                 ~ .x/saude_n1)) %>%
  #   mutate(across(.cols = matches("CMASM"),
  #                 ~ .x/saude_n2)) %>%
  #   mutate(across(.cols = matches("CMASA"),
  #                 ~ .x/saude_n3)) %>%
  #   mutate(across(.cols = matches("CMAET"),
  #                 ~ .x/educacao_tot)) %>%
  #   mutate(across(.cols = matches("CMAEI"),
  #                 ~ .x/educacao_n1)) %>%
  #   mutate(across(.cols = matches("CMAEF"),
  #                 ~ .x/educacao_n2)) %>%
  #   mutate(across(.cols = matches("CMAEM"),
  #                 ~ .x/educacao_n3)) %>%
  #   mutate(across(.cols = matches("CMAMT"),
  #                 ~ .x/matriculas_tot)) %>%
  #   mutate(across(.cols = matches("CMAMI"),
  #                 ~ .x/matriculas_n1)) %>%
  #   mutate(across(.cols = matches("CMAMF"),
  #                 ~ .x/matriculas_n2)) %>%
  #   mutate(across(.cols = matches("CMAMM"),
  #                 ~ .x/matriculas_n3)) %>%
  #   mutate(across(.cols = matches("CMALZ"),
  #                 ~ .x/lazer_tot2)) %>%
  #   mutate(across(.cols = matches("CMAPR"),
  #                 ~ .x/paraciclos_tot)) %>%
  #   mutate(across(.cols = matches("CMABK"),
  #                 ~ .x/bikes_comp_tot))
  # 
  # # DT <- acess
  # 
  # #inserir drop na em cada gráfico
  # # dados_acc_maps <- acess %>% mutate_if(is.numeric, list(~na_if(., Inf)))
  # # dados_acc_maps <- dados_acc_maps %>% mutate_if(is.numeric, list(~na_if(., NaN)))
  # data_acess <- acess2
  # acess <- acess2
  
  
  
  # empregos_tot <- sum(dados_hex$n_jobs)
  
  cols <- which(names(acess) %in% paste0(type_acc, sigla_op))
  acess <- acess %>% filter(mode == mode1) %>%
    select(cols)
  acess2 <- acess %>% gather(ind, valor, which(names(acess) %in% paste0(type_acc,sigla_op)))
  acess <- acess2
  
  rm(acess2)
  #Ajustar o tempo de acesso maximo
  acess <- acess %>%
    mutate(valor = ifelse(is.na(valor)==T, 180, valor)) %>%
    mutate(valor = ifelse(valor >time, time, valor))
  
  # acess <- acess %>%
  #   mutate(across(.cols = matches("CMATT"),
  #                 ~ .x/empregos_tot))
  
  
  
  # ajustar levels
  #modificar aqui para compatibilizar com tmi
  # acess <- acess %>%
  #   mutate(ind = factor(ind, 
  #                       levels = paste0(type_acc,sigla_op, time),
  #                       labels = paste0(time, " Minutos")))
  
  #definição do tema
  if (type_acc == "CMA") {
    
    if(length(time) == 1){
      tema <- theme_for_CMA_1map()
      dpi_mapa <- 300
    } else if (length(time) == 2){
      tema <- theme_for_CMA_2maps()
      dpi_mapa <- 500
    } else if (length(time) == 4){
      tema <- theme_for_CMA_4maps()
      dpi_mapa <- 600
    }
    
  } else if (type_acc == "TMI") {
    tema <- theme_for_TMI()
    dpi_mapa <- 300
  }
  
  # assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
  #                                   sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
  #   mutate(title = "Assentamentos Precários")
  # 
  # area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
  #                                    sigla_muni)) %>% filter(Classes == 24)
  # mapview(area_urbanizada)
  
  # st_write(acess, "../teste_poa_acc.gpkg")
  # options(scipen = 1000000000)
  # fazer plots
  #adicionar if com escala viridis para o tmi
  
  limits <- c(0, time)
  if (time == 15 | time == 45){
    breaks <- seq(0,time,time/3)
    labels <- c(seq(0,time-time/3,time/3), paste0("> ", time))
  } else {
    breaks <- seq(0,time,time/4)
    labels <- c(seq(0,time-time/4,time/4), paste0("> ", time))
  }
  
  if (mode1 == "bike"){
    modo <- "bicicleta"
    legenda_text <- "por bicicleta"
  } else if (mode1 == "transit"){
    modo <- "transporte_publico"
    legenda_text <- "por transporte público"
  } else if (mode1 == "walk"){
    modo <- "caminhada"
    legenda_text <- "por caminhada"
  }
  
  
  
  plot3 <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() +
    
    # geom_sf(data = area_urbanizada %>% st_transform(3857),
    #         aes(colour = "black"),
    #         fill = "grey50",
    #         size = 1.3,
    #         colour = NA)  +
    
    geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6, size = 0)+
    
    viridis::scale_fill_viridis(option = "D",
                                direction = -1,
                                limits = limits,
                                breaks = breaks,
                                labels = labels
    ) +
    # scale_colour_manual(values = 'transparent') +
    # facet_wrap(~ind, ncol = 2)+

    labs(fill = sprintf("TMI a %s", titulo_leg)) +
    labs(fill = sprintf("<span style = 'color :#000000;'>Tempo mínimo de acesso<br>a %s %s</span>",
                        titulo_leg,
                        legenda_text)) +
    
    ggnewscale::new_scale_fill() +
    
    geom_sf(data = dados_areas %>% st_transform(3857),
            # aes(size = 2),
            aes(color = "bairros"),
            # color = "grey45",
            # aes(fill = '#CFF0FF'),
            fill = NA,
            # stroke = 2,
            # size = 2,
            linewidth = 0.7,
            alpha= 0.7) +

    geom_sf(data = assentamentos,
            # aes(fill = "#d96e0a"),
            aes(color = "assentamentos"),
            
            # fill = "#d96e0a",
            linewidth = 0.5,
            fill = "#f1886e",
            show.legend = "polygon",
            alpha = 0.7)+
    
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
    
    scale_color_manual(name = "Uso do solo",
                       breaks = c("assentamentos", "bairros", "urb"),
                       values = c("urb" = "#fdfdc1",
                                  "assentamentos" = "#f1886e",
                                  "bairros" = "grey60"),
                       label = c("urb" = "Área urbanizada",
                                 "assentamentos" = "Aglomerados subnormais",
                                 "bairros" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)])
    )+
    
    geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
    # geom_sf(data = assentamentos,
    #         aes(colour = "white"),
    #         fill = NA,
    #         size = 1.3)+
    
    # scale_color_identity(labels = c(white = "",
    #                                 blue = ""), guide = "legend") +
    # labs(colour = "Assentamentos\nPrecários")+
    
    # scale_fill_gradientn(
    #   name = "Nº de Empregos",
  #   colors = colors_purple ,
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
  
  
  # scale_color_manual(values = 'transparent')+
  # facet_wrap(~ind, ncol = 2)+
  # tema+
  
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
    
    
    
    
    
    tema_TMI() +
    
    # theme(strip.text = element_blank(),
    #       legend.title = element_text(size=rel(0.6),
    #                                   vjust = 1
    #                                   ),
    #       axis.ticks.length = unit(0,"pt")
    # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
    #       # plot.background = element_rect(fill = "#eff0f0",
    #       #                                 colour = NA)
    # legend.background = element_rect(fill = "white",
    #                                  colour = NA)
  # #       
  #                                          ) +
  aproxima_muni(sigla_muni = sigla_muni) +
    guides(color = guide_legend(override.aes = list(fill = c("#f1886e", "white", "white"),
                                                    color = c("#f1886e", "grey60", "#fcfb76"),
                                                    linewidth = c(1,1,1)),
                                order = 1))
    
    # rm(acess)
    rm(acess, dados_acc, dados_areas, pop_counts, assentamentos,
       simplepolys, area_urbanizada, map_tiles, dados_hex, data_contorno)
    gc()
    # theme(strip.text = element_blank(),
    #       legend.title = element_text(
    #         # size=rel(0.6),
    #                                   vjust = 1
    #       ),
    #       axis.ticks.length = unit(0,"pt")
    #       # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
    #       #       # plot.background = element_rect(fill = "#eff0f0",
    #       #       #                                 colour = NA)
    #       # legend.background = element_rect(fill = "white",
    #       #                                  colour = NA)
    #       #       
    # )
  # labs(fill = "Tempo Mínimo\n de Acesso")
  
  # plot3 <- ggplot()+
  #   geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
  #   coord_equal() +
  #   scale_fill_identity()+
  #   # nova escala
  #   new_scale_fill() +
  #   
  #   # geom_sf(data = area_urbanizada %>% st_transform(3857),
  #   #         aes(colour = "black"),
  #   #         fill = "grey50",
  #   #         size = 1.3,
  #   #         colour = NA)  +
  #   
  #   geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.6, size = 0)+
  #   geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
  #   # geom_sf(data = assentamentos,
  #   #         aes(colour = "white"),
  #   #         fill = NA,
  #   #         size = 1.3)+
  #   
  #   # scale_color_identity(labels = c(white = "",
  #   #                                 blue = ""), guide = "legend") +
  #   # labs(colour = "Assentamentos\nPrecários")+
  #   
  #   # scale_fill_gradientn(
  #   #   name = "Nº de Empregos",
  # #   colors = colors_purple ,
  # #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
  # #   # values = NULL,
  # #   space = "Lab",
  # #   na.value = NA,
  # #   # guide = "colourbar",
  # #   aesthetics = "fill",
  # #   # colors
  # # ) +
  # 
  # # scale_fill_continuous(palette = "Blues",
  # #                   aesthetics = "fill")+
  # viridis::scale_fill_viridis(option = "B",
  #                             
  #                             labels = scales::label_percent(accuracy = 1, decimal.mark = ",")
  #                             # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
  #                             #                               scale = ifelse(sigla_op== "TT",1e-3,1))
  #                             #                      limits = c(0,500000))+
  #                             # , limits = c(0, 0.72)
  #                             # , breaks = c(0.001, 0.35, 0.7)
  #                             # , labels = c(0, "35", "70%")
  # ) +
  #   
  #   # scale_color_manual(values = 'transparent')+
  #   # facet_wrap(~ind, ncol = 2)+
  #   tema+
  #   labs(fill = sprintf("%s\nacessíveis", titulo_leg)) +
  #   theme(strip.text = element_blank(),
  #         legend.title = element_text(size=rel(0.6),
  #                                     vjust = 1
  #         ),
  #         axis.ticks.length = unit(0,"pt")
  #         # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
  #         #       # plot.background = element_rect(fill = "#eff0f0",
  #         #       #                                 colour = NA)
  #         # legend.background = element_rect(fill = "white",
  #         #                                  colour = NA)
  #         #       
  #   )
  
  # map_urbanizado <- ggplot()+
  #   geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
  #   coord_equal() +
  #   scale_fill_identity()+
  #   # nova escala
  #   new_scale_fill() +
  #   
  #   geom_sf(data = area_urbanizada %>% st_transform(3857),
  #           aes(fill = "#5766cc"),
  #           # fill = "#5766cc",
  #           size = 1.3,
  #           colour = NA) +
  #   geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
  #   scale_fill_identity(labels = c("#5766cc" = ""), guide = "legend") +
  #   labs(fill = "Área Urbanizada")+
  #   tema
  # # labs(fill = sprintf("%s\nacessíveis", titulo_leg)) +
  # # theme(plot.title = element_text(hjust = 0.5, size = rel(1))
  # #       # plot.background = element_rect(fill = "#eff0f0",
  # #       #                                 colour = NA)
  # #       # legend.background = element_rect(fill = "white",
  # #       #                                  colour = NA)
  # #       
  # # )
  # 
  # 
  # map_precarios <- ggplot()+
  #   geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
  #   coord_equal() +
  #   scale_fill_identity()+
  #   # nova escala
  #   new_scale_fill() +
  #   
  #   geom_sf(data = assentamentos %>% st_transform(3857),
  #           aes(fill = "#d96e0a"),
  #           # fill = "#5766cc",
  #           size = 1.3,
  #           colour = NA) +
  #   geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
  #   scale_fill_identity(labels = c("#d96e0a" = ""), guide = "legend") +
  #   labs(fill = "Assentamentos Precários")+
  #   tema +
  #   # labs(fill = sprintf("%s\nacessíveis", titulo_leg)) +
  #   theme(plot.title = element_blank(),
  #         strip.text = element_blank())
  # # plot.background = element_rect(fill = "#eff0f0",
  # #                                 colour = NA)
  # # legend.background = element_rect(fill = "white",
  # #                                  colour = NA))
  # 
  # 
  # 
  # h <- wrap_plots(plot3, map_urbanizado, map_precarios, ncol = 3) &
  #   # plot_layout(#ncol = 5,
  #   #             # widths = c(4, -0.5 , 4 , -0.5, 4 ),
  #   #             # heights = c(20,2),
  #   #             guides = "collect") &
  #   theme(legend.position = c(0.5,0.1),
  #         legend.direction = "horizontal",
  #         legend.key.width = unit(1, "line"),
  #         legend.key.height = unit(0.3, "cm"),
  #         # panel.spacing = ,
  #         # legend.margin = margin(t=-70),
  #         # strip.background = element_blank(),
  #         # legend.box.margin = margin(t=-100),
  #         legend.box.background = element_blank()
  #         # legend.box.background = element_rect(fill = "white", colour = "black")
  #         
  #         # = element_rect(fill = "white", colour = "black",) #,  margin(t=-10, unit = "mm"))
  #   ) #&
  # plot_annotation(theme = theme(plot.background = element_rect_round(color  = '#5766cc',
  #                                                                    size = 1.2,
  #                                                                    linetype = "solid",
  #                                                                    radius = unit(0.10, "snpc"))))
  # png("mtcars.png",res = 300)
  # print(h)
  # dev.off()
  # h <- plot3 +
  # plot_spacer()+
  # map_urbanizado +
  # plot_spacer() +
  # map_precarios +
  # plot_layout(ncol = 5,
  #             widths = c(4, -0.5 , 4 , -0.5, 4 ),
  #             heights = c(20,2),
  #             guides = "collect") & theme(legend.position = "bottom",
  #                                         legend.margin = margin(t=-70),
  #                                         strip.background = element_blank(),
  #                                         # legend.key.width = 0.6,
  #                                         # legend.title = element_text(size = rel(0.4)),
  #                                         # plot.margin = grid::unit(c(0,0,0,0), "mm"),
  #                                         # plot.title = element_text(margin = margin(c(0,0,-100,0))),
  #                                         # scale_x_continuous(expand(c(0,0))),
  #                                         # scale_y_continuous(expand(c(0,0))),
  #                                         # legend.title = element_text(size=rel(0.6))
  #                                         ) &
  #   plot_annotation(theme = theme(plot.background = element_rect_round(color  = 'blue',
  #                                                                      size = 2,
  #                                                                      linetype = "dashed")))
  #   # plot_annotation(theme = theme(plot.margin = grid::unit(c(0,0,10,0), "mm")))
  
  
  # h1 <- h & theme(
  #   panel.background = element_rect_round(fill = NA,
  #                                  color = "black",
  #                                  linetype = "dashed")
  # )
  
  # plot3 +
  #   # plot_spacer()+
  #   map_urbanizado +
  #   # plot_spacer() +
  #   map_precarios +
  #   plot_layout(ncol = 3,
  #               widths = c(4, 4 , 4 ))
  
  

  
  suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/%s/%s/%s/', sigla_muni, modo, type_acc ,oportunidade)))
  
  
  ggsave(plot3, 
         file= sprintf("../data/map_plots_acc/muni_%s/%s/%s/%s/%s_%s_%s_%s.png",
                       sigla_muni, modo, type_acc , oportunidade, sigla_muni, type_acc , sigla_op, paste(time, collapse = '')), 
         dpi = 350, width = 16.5, height = 16.5, units = "cm")
  
  rm(plot3)
  gc()
  # width <- 18
  # height <- 10
  
}


mapas_tmi(sigla_muni = "poa",
          type_acc = "TMI",
          sigla_op = "BK",
          mode1 = "walk",
          oportunidade = "bikes_compartilhadas",
          titulo_leg = "Bicicletas compartilhadas<br>",
          time = 15)
# mapas_tmi(sigla_muni = "poa",
#           type_acc = "TMI",
#           sigla_op = "EI",
#           mode1 = "bike",
#           oportunidade = "educacao",
#           titulo_leg = "E.q de\nEducação",
#           time = 15)
# mapas_tmi(sigla_muni = "poa",
#           type_acc = "TMI",
#           sigla_op = "EF",
#           mode1 = "bike",
#           oportunidade = "educacao",
#           titulo_leg = "E.q de\nEducação",
#           time = 15)
# mapas_tmi(sigla_muni = "poa",
#           type_acc = "TMI",
#           sigla_op = "EM",
#           mode1 = "bike",
#           oportunidade = "educacao",
#           titulo_leg = "E.q de\nEducação",
#           time = 15)



# mapas_tmi(sigla_muni = "poa",
#           type_acc = "TMI",
#           sigla_op = "ET",
#           mode1 = "transit",
#           oportunidade = "educacao",
#           titulo_leg = "E.q de\nEducação",
#           time = 15)
# mapas_tmi(sigla_muni = "poa",
#           type_acc = "TMI",
#           sigla_op = "EI",
#           mode1 = "transit",
#           oportunidade = "educacao",
#           titulo_leg = "E.q de\nEducação",
#           time = 15)
# mapas_tmi(sigla_muni = "poa",
#           type_acc = "TMI",
#           sigla_op = "EF",
#           mode1 = "transit",
#           oportunidade = "educacao",
#           titulo_leg = "E.q de\nEducação",
#           time = 15)
# 
# mapas_tmi(sigla_muni = "poa",
#           type_acc = "TMI",
#           sigla_op = "EM",
#           mode1 = "transit",
#           oportunidade = "educacao",
#           titulo_leg = "E. de\nEducação",
#           time = 15)

# library("future")
# plan(multisession)
# 
# lista_modos <- c(rep("walk", 1), rep("bike", 1))
# 
# lista_oportunidade <- rep(c(
#                             "bikes_compartilhadas"),2)
# 
# lista_siglaop <- rep(c("BK"), 2)
# 
# lista_titulo_leg <- rep(c(
#                           "B. Compart.")
#                           ,2)
# lista_tempos <- c(rep(30,1), rep(30, 1))
# 
# lista_args <- list(lista_modos, lista_oportunidade, lista_siglaop, lista_titulo_leg, lista_tempos)
# 
# furrr::future_pwalk(.l = lista_args, .f = mapas_tmi,
#                     sigla_muni = 'poa',
#                     type_acc = "TMI",
#                     cols = 1,
#                     width = 14,
#                     height = 10)
# 



# temp1 %<-% mapas_cma(sigla_muni = 'poa',
#                      type_acc = "CMA",
#                      mode1 = "transit",
#                      oportunidade = "empregos",
#                      sigla_op = "TT",
#                      titulo_leg = "Empregos",
#                      time = c(60),
#                      cols = 1,
#                      width = 14,
#                      height = 10
#                      )
# 
# 
# temp2 %<-% mapas_cma(sigla_muni = 'poa',
#                      type_acc = "CMA",
#                      mode1 = "bike",
#                      oportunidade = "matriculas",
#                      sigla_op = "MT",
#                      titulo_leg = "Matriculas",
#                      time = c(60),
#                      cols = 1,
#                      width = 14,
#                      height = 10
# )
# 
# temp3 %<-% mapas_cma(sigla_muni = 'poa',
#                      type_acc = "CMA",
#                      mode1 = "transit",
#                      oportunidade = "empregos",
#                      sigla_op = "TT",
#                      titulo_leg = "Empregos",
#                      time = c(60),
#                      cols = 1,
#                      width = 14,
#                      height = 10
# )
lista_modos <- c(rep("walk", 9), rep("bike", 9))
# lista_modos <- c(rep("transit", 9), rep("walk", 9), rep("bike", 9))
# lista_modos <- c(rep("transit", 11), rep("walk", 11), rep("bike", 11))
lista_oportunidade <- rep(c(
  
  rep("escolas", 4),
  rep("saude", 4),
  "lazer"),2)
# lista_oportunidade <- rep(c(
#   
#   rep("escolas", 4),
#   rep("saude", 4),
#   "lazer"),3)
# lista_oportunidade <- rep(c(
# 
# rep("escolas", 4),
# rep("saude", 4),
# "lazer",
# "bikes_compartilhadas",
# "paraciclos"),3)

lista_siglaop <- rep(c(
  "ET", "EI", "EF", "EM",
  "ST", "SB", "SM", "SA",
  "LZ"), 2)
# lista_siglaop <- rep(c(
#   "ET", "EI", "EF", "EM",
#   "ST", "SB", "SM", "SA",
#   "LZ"), 3)
# lista_siglaop <- rep(c(
#   "ET", "EI", "EF", "EM",
#   "ST", "SB", "SM", "SA",
#   "LZ", "BK", "PR"), 3)

lista_titulo_leg <- rep(c(
  rep("Escolas", 4),
  rep("Eq. de Saúde", 4),
  "Eq. de Lazer"), 2)
# lista_titulo_leg <- rep(c(
#   rep("Escolas", 4),
#   rep("Eq. de Saúde", 4),
#   "Eq. de Lazer"), 3)
# lista_titulo_leg <- rep(c(
#   rep("Escolas", 4),
#   rep("Eq. de Saúde", 4),
#   "Eq. de Lazer",
#   "Est. de B. Comp.",
#   "Paraciclos"), 3)
lista_tempos <- c(rep(15,9), rep(30, 9))
# lista_tempos <- c(rep(45, 9), rep(15,9), rep(30, 9))
# lista_tempos <- c(rep(30, 11), rep(15,11), rep(15, 11))

lista_args <- list(lista_modos, lista_oportunidade, lista_siglaop, lista_titulo_leg, lista_tempos)


library("future")
seed = TRUE
plan(multisession)

furrr::future_pwalk(.l = lista_args, .f = mapas_tmi,
                    sigla_muni = 'poa',
                    type_acc = "TMI",
                    cols = 1,
                    width = 16.5,
                    height = 16.5)



# Aplicação para todos os tempos ------------------------------------------

lista_modos <- c(rep(rep("transit", 9),4), rep(rep("walk", 9),4), rep(rep("bike", 9),4))

lista_tempos <- rep(c(rep(15, 9), rep(30,9), rep(45, 9), rep(60, 9)),3)

lista_oportunidade <- rep(rep(c(
  rep("escolas", 4),
  rep("saude", 4),
  "lazer"),4),3)

lista_siglaop <- rep(rep(c(
  "ET", "EI", "EF", "EM",
  "ST", "SB", "SM", "SA",
  "LZ"), 4),3)

lista_titulo_leg <- rep(rep(c(
  rep("escolas", 4),
  rep("Eq. de saúde", 4),
  "Eq. de lazer"), 4),3)

lista_args <- list(lista_modos, lista_oportunidade, lista_siglaop, lista_titulo_leg, lista_tempos)

library("future")
seed = TRUE
plan(multisession)

furrr::future_pwalk(.l = lista_args, .f = mapas_tmi,
                    sigla_muni = 'dou',
                    type_acc = "TMI",
                    cols = 1,
                    width = 16.5,
                    height = 16.5)



# Aplicação para todos os tempos com bikes e paraciclos -------------------


lista_modos <- c(rep(rep("transit", 11),4), rep(rep("walk", 11),4), rep(rep("bike", 11),4))

lista_tempos <- rep(c(rep(15, 11), rep(30,11), rep(45, 11), rep(60, 11)),3)

lista_oportunidade <- rep(rep(c(
  rep("escolas", 4),
  rep("saude", 4),
  "lazer",
  "paraciclos",
  "bikes_compartilhadas"),4),3)

lista_siglaop <- rep(rep(c(
  "ET", "EI", "EF", "EM",
  "ST", "SB", "SM", "SA",
  "LZ", "PR", "BK"), 4),3)

lista_titulo_leg <- rep(rep(c(
  rep("escolas", 4),
  rep("Eq. de saúde", 4),
  "Eq. de lazer",
  "Paraciclos",
  "Bicicletas compartilhadas"), 4),3)

lista_args <- list(lista_modos, lista_oportunidade, lista_siglaop, lista_titulo_leg, lista_tempos)

library("future")
seed = TRUE
plan(multisession)

furrr::future_pwalk(.l = lista_args, .f = mapas_tmi,
                    sigla_muni = 'poa',
                    type_acc = "TMI",
                    cols = 1,
                    width = 16.5,
                    height = 16.5)


# Aplica;'ao poa ----------------------------------------------------------


lista_modos <- c(rep("transit", 11), rep("walk", 11), rep("bike", 11))

lista_tempos <- c(rep(45, 11), rep(45,11), rep(45, 11))

lista_oportunidade <- rep(c(
  rep("escolas", 4),
  rep("saude", 4),
  "lazer",
  "paraciclos",
  "bikes_compartilhadas"),3)

lista_siglaop <- rep(c(
  "ET", "EI", "EF", "EM",
  "ST", "SB", "SM", "SA",
  "LZ", "PR", "BK"), 3)

lista_titulo_leg <- rep(c(
  rep("escolas", 4),
  rep("Eq. de saúde", 4),
  "Eq. de lazer",
  "Paraciclos",
  "Bicicletas compartilhadas"), 3)

lista_args <- list(lista_modos, lista_oportunidade, lista_siglaop, lista_titulo_leg, lista_tempos)

library("future")
seed = TRUE
plan(multisession)

furrr::future_pwalk(.l = lista_args, .f = mapas_tmi,
                    sigla_muni = 'poa',
                    type_acc = "TMI",
                    cols = 1,
                    width = 16.5,
                    height = 16.5)
