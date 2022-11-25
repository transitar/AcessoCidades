source('./R/fun/setup.R')
library(patchwork)

width <- 14
height <- 10

sigla_muni <- 'poa'
mode1 <- "bike"
oportunidade <- "bikes_compartilhadas"
titulo_leg <- "Bicicletas Compartilhadas"
sigla_op <- "BK"
time <- c(15,30,45,60)
type_acc <- "CMA"

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
      legend.title=element_text(size=rel(0.6)),
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
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1,"line"),
      legend.key.height = unit(0.4,"cm"),
      legend.text=element_text(size=rel(0.6)),
      legend.title=element_text(size=rel(0.9)),
      strip.text = element_text(size=rel(1.3))
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
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(0.5,"cm"),
      legend.text=element_text(size=rel(0.9)),
      legend.title=element_text(size=rel(1.2)),
      strip.text = element_text(size=rel(1.4))
      # plot.title = element_text(hjust = 0, vjust = 4),
      
      
      
    )
}

#retirar type_acc; oportunidade, titulo_leg

mapas_cma <- function(sigla_muni,
                     type_acc,
                     mode1,
                     oportunidade,
                     sigla_op,
                     titulo_leg,
                     time,
                     cols = 2,
                     width = 14,
                     height = 10){
  
  message(paste("Rodando",sigla_muni, "\n"))
  
  path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
  
  dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))

  path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
  
  data_contorno <- read_rds(path_contorno)
  
  maptiles <- read_rds(path_maptiles)
  
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
  
    
    # abrir acess
    acess <- dados_acc %>% filter(sigla_muni == sigla_munii)
    cols <- which(names(acess) %in% paste0(type_acc, sigla_op, time))
    acess <- acess %>% filter(mode == mode1) %>%
      select(sigla_muni, cols)
    acess2 <- acess %>% gather(ind, valor, which(names(acess) %in% paste0(type_acc,sigla_op, time)))
    acess <- acess2

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
    # fazer plots
    #adicionar if com escala viridis para o tmi
    plot3 <- ggplot()+
      geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      geom_sf(data = st_transform(acess, 3857), aes(fill = valor), colour = NA, alpha=.7, size = 0)+
      viridis::scale_fill_viridis(option = "B"
                                  # , limits = c(0, 0.72)
                                  # , breaks = c(0.001, 0.35, 0.7)
                                  # , labels = c(0, "35", "70%")
      ) +
      scale_color_manual(values = 'transparent')+
      facet_wrap(~ind, ncol = 2)+
      tema+
      labs(fill = sprintf("%s\nacessíveis", titulo_leg)) +
      theme(plot.title = element_text(hjust = 0.5, size = rel(1)))
    
    if (mode1 == "bike"){
      modo <- "bicicleta"
    } else if (mode1 == "transit"){
      modo <- "transporte_publico"
    } else if (mode == "walk"){
      modo <- "caminhada"
    }
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/%s/%s/%s/', sigla_munii, modo, type_acc ,oportunidade)))
    
    
    ggsave(plot3, 
           file= sprintf("../data/map_plots_acc/muni_%s/%s/%s/%s/%s_%s_%s_%s.png",
                         sigla_munii, modo, type_acc , oportunidade, sigla_munii, type_acc , sigla_op, paste(time, collapse = '')), 
           dpi = dpi_mapa, width = width, height = height, units = "cm")
    
  
  
  
}
  
  


library("future")
plan(multisession)

temp1 %<-% mapas_cma(sigla_muni = 'poa',
                     type_acc = "CMA",
                     mode1 = "bike",
                     oportunidade = "bikes_compartilhadas",
                     sigla_op = "BK",
                     titulo_leg = "Bicicletas Compartilhadas",
                     time = c(15,60),
                     cols = 2,
                     width = 14,
                     height = 10
                     )
