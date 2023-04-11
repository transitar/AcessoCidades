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
                       , 'mapview','ggspatial'
                       , 'raster'
                       , 'ggnewscale'
                       , 'rayshader'
                       , 'progressr'
                       , 'pbapply'
                       , 'extrafont'
                       , 'extrafontdb'
                       , 'aopdata')


# arquivo --------
rm(list=ls())
linhas <- sf::read_sf("data-raw/bra_belem/linhas_hidroviario.gpkg") %>% 
  sf::st_as_sfc() %>% 
  sf::st_transform(crs = 3857) 

pontos <- sf::read_sf("data-raw/bra_belem/pontos_hidroviario.gpkg") %>% 
  sf::st_as_sfc() %>% 
  sf::st_transform(crs = 3857)

my_tile <- readr::read_rds("data-raw/bra_belem/maptile_crop_mapbox_bel_2019.rds")

my_bound <- geobr::read_municipality(code_muni = 1501402,simplified = FALSE)
#my_bound <- sf::st_buffer(my_bound,2000)
bbox <- sf::st_transform(x = linhas,crs = 3857) %>% 
  sf::st_buffer(3000) %>% sf::st_bbox() 
bbox

my_tile_crop <- my_tile[my_tile$x >   bbox[[1]] & 
                          my_tile$x < bbox[[3]] &
                          my_tile$y > bbox[[2]] & 
                          my_tile$y < bbox[[4]],]

ggplot()+
  geom_raster(data = my_tile_crop, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = linhas
          ,colour = "darkblue"
          ,linetype = "dotted"
          ,alpha = 1, linewidth = 0.55,fill = NA) +
  #  add hex
  geom_sf(data = pontos 
          ,color = "red",size = 2) +
  # annotation
  ggspatial::annotation_scale(style = "ticks",
                              location = "bl",
                              text_family = "Encode Sans",
                              text_face = "bold",
                              text_cex = 0.75,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.35, "cm")) +
  ggspatial::annotation_north_arrow(
    style = north_arrow_minimal(text_size = 0)
    , location = "tr") +
  # labels and theme
  labs(color = "Rotas hidroviárias"
       , x = NULL
       , y = NULL) +
  theme(legend.position = c(0.05,0.415),
        legend.key.width=unit(2,"line"),
        legend.text = element_text(size = rel(0.8)
                                   , family = "Encode Sans"
                                   , face = "plain"),
        legend.title = element_text(size = rel(0.95)
                                    , family = "Encode Sans"
                                    , face = "bold"),
        title = element_text(size = 10
                             , family = "Encode Sans"
                             , face = "plain"),
        plot.margin=unit(c(0,2,0,1),"mm"),
        axis.ticks = element_blank(), 
        axis.text = element_blank(),
        strip.text.x = element_text(size=rel(1.2)),
        panel.grid.major.y = element_line(colour = "grey92"),
        
        panel.background = element_rect(fill = "white",colour = NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(fill=alpha('white', 0.7),
                                             colour = "#A09C9C",
                                             linewidth = 0.8,
                                             linetype = "solid"))

#dir.create("figures/bra_belem")
# save
ggplot2::ggsave(filename = "figures/bra_belem/rotas.png",
                scale = 0.45,width = 18,height = 30,
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