source('./R/fun/setup.R')
library(patchwork)
# library(elementalist)

# width <- 14
# height <- 10
# 
# sigla_muni <- 'poa'
# mode1 <- "walk"
# oportunidade <- "matriculas"
# titulo_leg <- "Matrículas"
# sigla_op <- "MT"
# time <- 60
# # time <- c(15,30,45,60)
# type_acc <- "CMA"

#não funciona ainda para tmi
#oportunidade = nome da pasta para salvar

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

#retirar type_acc; oportunidade, titulo_leg

#sigla_muni: sigla do municipio
#type_acc: CMA ou TMI (TMI ainda nao funcionando)
#oportunidade: nome da pasta para ser salva em /CMA/
#sigla_op: sigla do tipo de oportunidade: TT, ST, SB, SM, SA, ET, EI, EF, EM, MT, MI, MF, MM, LZ, PR, BK
#titulo_leg: palavra a ser colocada na legenda %s\nAcessiveis
#time: tempo maximo: 15, 30, 45, 60, ou 75 ou combinacao de tempos com c(15,30,45,60)
#funciona apenas para 1 ou 4 tempos
#cols: numero de colunas do facet

mapas_cma <- function(sigla_muni,
                     type_acc,
                     mode1,
                     oportunidade,
                     sigla_op,
                     titulo_leg,
                     time = 60,
                     cols = 1,
                     width = 15,
                     height = 5){
  
  message(paste("Rodando",sigla_muni, "\n"))
  
  path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
  
  dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))

  path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
  
  data_contorno <- read_rds(path_contorno)
  
  map_tiles <- read_rds(path_maptiles)
  
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
    data_acess <- dados_acc_maps
  }
  
  #totais de oportunidades
  # dados_acc %>% distinct_all() %>% nrow()
    # abrir acess
    acess <- dados_acc #%>% filter(sigla_muni == sigla_munii)
    
    dados_hex <- readr::read_rds(sprintf("../data/dados_hex/muni_%s/dados_hex_%s.rds",
                                         sigla_muni, sigla_muni))
    
    empregos_tot <- sum(dados_hex$n_jobs, na.rm = T)
    saude_tot <- sum(dados_hex$S001, na.rm = T)
    saude_n1 <- sum(dados_hex$S002, na.rm = T)
    saude_n2 <- sum(dados_hex$S003, na.rm = T)
    saude_n3 <- sum(dados_hex$S004, na.rm = T)
    educacao_tot <- sum(dados_hex$E001, na.rm = T)
    educacao_n1 <- sum(dados_hex$E002, na.rm = T)
    educacao_n2 <- sum(dados_hex$E003, na.rm = T)
    educacao_n3 <- sum(dados_hex$E004, na.rm = T)
    matriculas_tot <- sum(dados_hex$M001, na.rm = T)
    matriculas_n1 <- sum(dados_hex$M002, na.rm = T)
    matriculas_n2 <- sum(dados_hex$M003, na.rm = T)
    matriculas_n3 <- sum(dados_hex$M004, na.rm = T)
    lazer_tot2 <- sum(dados_hex$lazer_tot, na.rm = T)
    paraciclos_tot <- sum(dados_hex$paraciclos, na.rm = T)
    bikes_comp_tot <- sum(dados_hex$n_bikes, na.rm = T)
    
    
    
    acess2 <- acess %>%
      mutate(across(.cols = matches("CMATT"),
                    ~ .x/empregos_tot)) %>%
      mutate(across(.cols = matches("CMAST"),
                    ~ .x/saude_tot)) %>%
      mutate(across(.cols = matches("CMASB"),
                    ~ .x/saude_n1)) %>%
      mutate(across(.cols = matches("CMASM"),
                    ~ .x/saude_n2)) %>%
      mutate(across(.cols = matches("CMASA"),
                    ~ .x/saude_n3)) %>%
      mutate(across(.cols = matches("CMAET"),
                    ~ .x/educacao_tot)) %>%
      mutate(across(.cols = matches("CMAEI"),
                    ~ .x/educacao_n1)) %>%
      mutate(across(.cols = matches("CMAEF"),
                    ~ .x/educacao_n2)) %>%
      mutate(across(.cols = matches("CMAEM"),
                    ~ .x/educacao_n3)) %>%
      mutate(across(.cols = matches("CMAMT"),
                    ~ .x/matriculas_tot)) %>%
      mutate(across(.cols = matches("CMAMI"),
                    ~ .x/matriculas_n1)) %>%
      mutate(across(.cols = matches("CMAMF"),
                    ~ .x/matriculas_n2)) %>%
      mutate(across(.cols = matches("CMAMM"),
                    ~ .x/matriculas_n3)) %>%
      mutate(across(.cols = matches("CMALZ"),
                    ~ .x/lazer_tot2)) %>%
      mutate(across(.cols = matches("CMAPR"),
                    ~ .x/paraciclos_tot)) %>%
      mutate(across(.cols = matches("CMABK"),
                    ~ .x/bikes_comp_tot))
    
    # DT <- acess
    
    #inserir drop na em cada gráfico
    # dados_acc_maps <- acess %>% mutate_if(is.numeric, list(~na_if(., Inf)))
    # dados_acc_maps <- dados_acc_maps %>% mutate_if(is.numeric, list(~na_if(., NaN)))
    data_acess <- acess2
    acess <- acess2
    
    

    # empregos_tot <- sum(dados_hex$n_jobs)
    
    cols <- which(names(acess) %in% paste0(type_acc, sigla_op, time))
    acess <- acess %>% filter(mode == mode1) %>%
      select(sigla_muni, cols)
    acess2 <- acess %>% gather(ind, valor, which(names(acess) %in% paste0(type_acc,sigla_op, time)))
    acess <- acess2
    

    
    # acess <- acess %>%
    #   mutate(across(.cols = matches("CMATT"),
    #                 ~ .x/empregos_tot))
    
    

    # ajustar levels
    #modificar aqui para compatibilizar com tmi
    acess <- acess %>%
      mutate(ind = factor(ind, 
                          levels = paste0(type_acc,sigla_op, time),
                          labels = paste0(time, " Minutos")))
    
    #definição do tema
    if (type_acc == "CMA") {
      
    if(length(time) == 1){
      tema <- theme_for_CMA_1map()
      dpi_mapa <- 400
    } else if (length(time) == 2){
      tema <- theme_for_CMA_2maps()
      dpi_mapa <- 500
    } else if (length(time) == 4){
      tema <- theme_for_CMA_4maps()
      dpi_mapa <- 600
    }
      
    } else if (type_acc == "TMI") {
      tema <- theme_for_tmi
    }
    
    assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                                      sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
      mutate(title = "Assentamentos Precários")
    
    area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
                                       sigla_muni)) %>% filter(Classes == 24)
    # mapview(area_urbanizada)
    
    # st_write(acess, "../teste_poa_acc.gpkg")
    # options(scipen = 1000000000)
    # fazer plots
    #adicionar if com escala viridis para o tmi
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
      viridis::scale_fill_viridis(option = "B",
                                  
                                  labels = scales::label_percent(accuracy = 1, decimal.mark = ",")
                                  # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
                                  #                               scale = ifelse(sigla_op== "TT",1e-3,1))
      #                      limits = c(0,500000))+
      # , limits = c(0, 0.72)
      # , breaks = c(0.001, 0.35, 0.7)
                                  # , labels = c(0, "35", "70%")
      ) +
      
      # scale_color_manual(values = 'transparent')+
      # facet_wrap(~ind, ncol = 2)+
      tema+
      labs(fill = sprintf("%s\nacessíveis", titulo_leg)) +
      theme(strip.text = element_blank(),
            legend.title = element_text(size=rel(0.6),
                                        vjust = 1
                                        ),
            axis.ticks.length = unit(0,"pt")
      # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
      #       # plot.background = element_rect(fill = "#eff0f0",
      #       #                                 colour = NA)
    # legend.background = element_rect(fill = "white",
    #                                  colour = NA)
    #       
                                             )
    
    map_urbanizado <- ggplot()+
      geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = area_urbanizada %>% st_transform(3857),
              aes(fill = "#5766cc"),
              # fill = "#5766cc",
              size = 1.3,
              colour = NA) +
      geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
      scale_fill_identity(labels = c("#5766cc" = ""), guide = "legend") +
                                      labs(fill = "Área Urbanizada")+
      tema
      # labs(fill = sprintf("%s\nacessíveis", titulo_leg)) +
      # theme(plot.title = element_text(hjust = 0.5, size = rel(1))
      #       # plot.background = element_rect(fill = "#eff0f0",
      #       #                                 colour = NA)
      #       # legend.background = element_rect(fill = "white",
      #       #                                  colour = NA)
      #       
      # )
    
    
    map_precarios <- ggplot()+
      geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = assentamentos %>% st_transform(3857),
              aes(fill = "#d96e0a"),
              # fill = "#5766cc",
              size = 1.3,
              colour = NA) +
      geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
      scale_fill_identity(labels = c("#d96e0a" = ""), guide = "legend") +
      labs(fill = "Assentamentos Precários")+
      tema +
      # labs(fill = sprintf("%s\nacessíveis", titulo_leg)) +
      theme(plot.title = element_blank(),
            strip.text = element_blank())
            # plot.background = element_rect(fill = "#eff0f0",
            #                                 colour = NA)
            # legend.background = element_rect(fill = "white",
            #                                  colour = NA))
            
    
    
    h <- wrap_plots(plot3, map_urbanizado, map_precarios, ncol = 3) &
      # plot_layout(#ncol = 5,
      #             # widths = c(4, -0.5 , 4 , -0.5, 4 ),
      #             # heights = c(20,2),
      #             guides = "collect") &
      theme(legend.position = c(0.5,0.1),
            legend.direction = "horizontal",
            legend.key.width = unit(1, "line"),
            legend.key.height = unit(0.3, "cm"),
            # panel.spacing = ,
            # legend.margin = margin(t=-70),
            # strip.background = element_blank(),
            # legend.box.margin = margin(t=-100),
            legend.box.background = element_blank()
            # legend.box.background = element_rect(fill = "white", colour = "black")
              
              # = element_rect(fill = "white", colour = "black",) #,  margin(t=-10, unit = "mm"))
            ) #&
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
    
    
    if (mode1 == "bike"){
      modo <- "bicicleta"
    } else if (mode1 == "transit"){
      modo <- "transporte_publico"
    } else if (mode1 == "walk"){
      modo <- "caminhada"
    }
    
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/%s/%s/%s/', sigla_muni, modo, type_acc ,oportunidade)))
    
    
    ggsave(h, 
           file= sprintf("../data/map_plots_acc/muni_%s/%s/%s/%s/%s_%s_%s_%s.png",
                         sigla_muni, modo, type_acc , oportunidade, sigla_muni, type_acc , sigla_op, paste(time, collapse = '')), 
           dpi = dpi_mapa, width = width, height = height, units = "cm")
    
  
  # width <- 18
  # height <- 10
  
}
  
  


library("future")
plan(multisession)

lista_modos <- c(rep("transit", 16), rep("walk", 16), rep("bike", 16))

lista_oportunidade <- rep(c("empregos",
                        rep("matriculas",4),
                        rep("escolas", 4),
                        rep("saude", 4),
                        "lazer",
                        "bikes_compartilhadas",
                        "paraciclos"),3)

lista_siglaop <- rep(c("TT",
                   "MT", "MI", "MF", "MM",
                   "ET", "EI", "EF", "EM",
                   "ST", "SB", "SM", "SA",
                   "LZ", "BK", "PR"), 3)

lista_titulo_leg <- rep(c("Empregos",
                      rep("Matrículas",4),
                      rep("Escolas", 4),
                      rep("Eq. de Saúde", 4),
                      "Eq. de Lazer",
                      "Est. de B.\n Compartilhadas",
                      "Paraciclos"), 3)
lista_tempos <- c(rep(60, 16), rep(15,16), rep(15, 16))

lista_args <- list(lista_modos, lista_oportunidade, lista_siglaop, lista_titulo_leg, lista_tempos)

furrr::future_pwalk(.l = lista_args, .f = mapas_cma,
                    sigla_muni = 'poa',
                    type_acc = "CMA",
                    cols = 1,
                    width = 14,
                    height = 10)




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

