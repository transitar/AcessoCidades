# Load ----

rm(list=ls())
gc(reset = TRUE)

# install.packages('easypackages')
easypackages::packages('geobr'
                       , 'gtfs2gps'
                       , 'openxlsx'
                       , 'data.table'
                       , 'magrittr'
                       , 'ggplot2'
                       , 'patchwork'
                       , 'mapview'
                       , 'raster'
                       , 'ggnewscale'
                       , 'rayshader'
                       , 'progressr'
                       , 'pbapply'
                       , 'aopdata')

# Arquivo 04--------
od1 <- openxlsx::read.xlsx("data-raw/bra_palmas/OneDrive_2022-12-11/5 - OD/(Palmas) 04 Vetor Origem e Destino de Viagens (Modos Coletivo e Individual).xlsx")
setDT(od1)

vec_names <- c('zona', 'coletivo_origem_hpm', 'coletivo_destino_hpm',
               'coletivo_origem_hpt', 'coletivo_destino_hpt', 'individual_origem_hpm', 'individual_destino_hpm')
od1 <- od1[-1,]
names(od1) <- vec_names
od1[1:4,]
unique(od1$zona) %>% sort()

# geometria quadras----------
sf_zona <- sf::read_sf("data-raw/bra_palmas/OneDrive_2022-12-11/5 - OD/(Palmas) 02 Zoneamento de Tráfego.shp")
sf_zona %>% names()
sf_zona$Zona %>% unique() %>% sort()

# merge od to sf

sf_zona1 <- merge(x = sf_zona
                  ,y = od1
                  ,by.y = "zona"
                  ,by.x = "Zona")

sf_zona1 %>% names()
sf_zona1 %>% head(1)

sf_zona2 <- data.table::melt.data.table(
  data = setDT(sf_zona1)
  ,id.vars = c('Zona','Regiao','Populacao','geometry')
  ,measure.vars = c('coletivo_origem_hpm', 'coletivo_destino_hpm',
                    'individual_origem_hpm', 'individual_destino_hpm')
  #,measure.vars = list(
  #  "coletivo" = c('coletivo_origem_hpm', 'coletivo_destino_hpm'),
  #  "individual" = c('individual_origem_hpm', 'individual_destino_hpm')
  #),
  #,variable.name = c("coletivo")
)
sf_zona2[,type := fcase(variable %like% "origem","origem",
                        variable %like% "destino","destino"
)]
sf_zona2[,veh := fcase(variable %like% "individual","individual",
                       variable %like% "coletivo","coletivo"
)]
sf_zona2[,value := as.numeric(value)]
zona_exclude <- sf_zona2[,sum(value),by = Zona][V1==0,Zona]
sf_zona2 <- sf_zona2[!(Zona %in% zona_exclude),]
sf_zona2 <- sf::st_as_sf(sf_zona2)
#  Map -----

my_tile <- readr::read_rds("data-raw/bra_palmas/maptile_crop_mapbox_pal_2019.rds")
my_bound <- sf::st_transform(x = sf_zona2,3857)
bound_bbox <- sf::st_bbox(sf::st_buffer(my_bound,2000)) 

bound_bbox

my_tile_crop <- my_tile[my_tile$x > bound_bbox[[1]] & 
                          my_tile$x < bound_bbox[[3]] &
                          my_tile$y > bound_bbox[[2]] & 
                          my_tile$y < bound_bbox[[4]],]

plot_f <- function(type,veh,pal,cap = TRUE,title){
  
  tmp_input <- my_bound[my_bound$value > 0 & 
                          my_bound$veh == veh & 
                          my_bound$type == type,]
  
  if(cap){ mycap <- "*Horário de pico da manhã (06h - 08h)" }else{
    mycap <- NULL }
  
  a <- ggplot()+
    geom_raster(data = my_tile_crop, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    #  add hex
    ggnewscale::new_scale_fill() +
    geom_sf(data = tmp_input
            ,mapping = aes(fill = value)
            , colour = "transparent") +
    viridis::scale_fill_viridis(option = pal
                                ,direction = -1)+
    # add boundary
    ggnewscale::new_scale_color() +
    geom_sf(data = my_bound,color = "grey15"
            ,linetype = "solid",alpha = 0.05, size = 0.015,fill = NA) +
    # map itens
    ggsn::north(data = my_bound,
                location = "topright",symbol = 12) +
    ggsn::scalebar(x.min = as.numeric(sf::st_bbox(sf::st_transform(my_bound,3857))[1]),
                   x.max = as.numeric(sf::st_bbox(sf::st_transform(my_bound,3857))[3])-7500,
                   y.min = as.numeric(sf::st_bbox(sf::st_transform(my_bound,3857))[2]),
                   y.max = as.numeric(sf::st_bbox(sf::st_transform(my_bound,3857))[4]),
                   dist = 5,
                   #text = element_text(family = "LM Roman 10"),
                   family = "Encode Sans",
                   fontface = "plain",
                   dist_unit = "km",st.size = 3,
                   st.bottom = FALSE, st.color = "black",
                   transform = FALSE, model = "WGS84") +
    # labels and theme
    labs(title = title
      ,fill = "Número de\n viagens"
      ,caption = mycap
      #, fill =  "Número de \nparadas"
      , color = NULL
      , x = NULL
      , y = NULL) +
    theme(legend.position = c(0.80,0.3),
          plot.margin=unit(c(0,2,0,1),"mm"),
          legend.key.width=unit(1,"line"),
          legend.key.height = unit(0.4,"cm"),
          legend.text =element_text(size=rel(0.8)),
          axis.ticks = element_blank(), 
          axis.text = element_blank(),
          legend.background = element_blank(),
          panel.background = element_rect(fill = "white",colour = NA),
          legend.title=element_text(size=rel(0.9)),
          strip.text = element_text(size=rel(1.3)) ,
          panel.grid.major.y = element_line(colour = "grey92"),
          text = element_text(family = "Encode Sans"))
  return(a)
}

ind_org <- plot_f("origem","individual","E",FALSE,"Individual (Origem)")
ind_des <- plot_f("destino","individual","E",FALSE,"Individual (Destino)")
col_org <- plot_f("origem","coletivo","D",FALSE,"Coletivo (Origem)")
col_des <- plot_f("destino","coletivo","D",TRUE,"Coletivo (Destino)") 


pf <- (ind_org + ind_des) / (col_org + col_des) + 
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")

# save
ggplot2::ggsave(pf,filename = "figures/bra_palmas/maps_OD.png",
                scale = 1.0,width = 18,height = 18,
                bg = "white",
                units = "cm",dpi = 300)
dir.create("figures/bra_palmas/")

# outros ----
od2 <- openxlsx::read.xlsx("data-raw/bra_palmas/OneDrive_2022-12-11/5 - OD/(Palmas) 05 Matriz de Origem e Destino de Viagens da Hora Pico da Manhã Modo Coletivo.xlsx")
setDT(od2)

od2[,sum(HPM),by = .(Origem)][order(V1),]

od3 <- openxlsx::read.xlsx("data-raw/bra_palmas/OneDrive_2022-12-11/5 - OD/(Palmas) 06 Matriz de Origem e Destino de Viagens da Hora Pico da Manhã Modo Individual.xlsx")
setDT(od3)

od3[,sum(VEÍCULO.HPM),by = Origem][order(V1),]
# End -----