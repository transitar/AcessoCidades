# Load ----

rm(list=ls())
gc(reset = TRUE)

# install.packages('easypackages')
easypackages::packages('geobr'
                       , 'data.table'
                       , 'magrittr'
                       , 'ggplot2'
                       , 'patchwork'
                       , 'mapview','ggspatial'
                       , 'raster'
                       , 'ggnewscale'
                       , 'rayshader'
                       , 'progressr'
                       , 'extrafont'
                       , 'extrafontdb'
                       , 'aopdata')


# input OD data--------
od1 <- readr::read_rds("data/bra_aracaju/od_dt.rds")
od1$div_mod[,total := publico + privado]
od1$div_mod[,r_publico := 100 * (publico / total)]
od1$div_mod[,r_privado := 100 * (privado / total)]

# geometria quadras----------
sf_zona <- sf::read_sf("data-raw/bra_aracaju/POLIGONAIS_2/POLIGONAIS_2.shp")
sf_zona$name <- janitor::make_clean_names(sf_zona$bairro)

sf_zona$name[sf_zona$name == "x13_de_julho"] <- "treze_de_julho"
sf_zona$name[sf_zona$name == "x18_do_forte"] <- "dezoito_do_forte"
sf_zona$name[sf_zona$name == "santos_dumont"] <- "santos_dummond"
sf_zona$name[sf_zona$name == "centenario"] <- "jardim_centenario"
sf_zona$name[sf_zona$name == "joao_conrado_de_araujo"] <- "jose_conrado_de_araujo"

od1$dt_pico_mot_viagens1$name[od1$dt_pico_mot_viagens1$name == "santos_dumont"] <- "santos_dummond"
od1$dt_pico_mot_viagens1$name[od1$dt_pico_mot_viagens1$name == "santos_dumont"] <- "santos_dummond"
od1$dt_pico_mot_viagens1$name[od1$dt_pico_mot_viagens1$name == "suica"] <- "suissa"

# merge od to sf

sf_zona1 <- data.table::merge.data.table(  x = as.data.table(sf_zona)
                                           ,y = od1$div_mod
                                           ,by = "name"
                                           ,all = TRUE) %>% 
  data.table::merge.data.table(x = .
                               ,y = od1$dt_pico_mot_geracao_nn
                               ,by = "name",all = TRUE) %>% 
  data.table::merge.data.table(x = .
                               ,y = od1$dt_pico_mot_viagens1
                               ,by = "name",all = TRUE)
sf_zona1 <- sf_zona1[!is.na(geometry),]

sf_zona2 <- data.table::melt.data.table(
  data = setDT(sf_zona1)
  ,id.vars = c('name','bairro','pop','geometry')
  ,measure.vars = c('publico', 'privado', 
                    'r_publico', 'r_privado', 'ger_7_10',
                    'prod_7_10', 'atra_7_10',
                    'ger_8h15_9h15','prod_8h15_9h15','atra_8h15_9h15',
                    'viagens','viagens_hab')
)
sf_zona2[,value := as.numeric(value)]
sf_zona2 <- sf_zona2[!is.na(bairro),]
sf_zona2 <- sf::st_as_sf(sf_zona2)
#  Map -----

my_tile <- readr::read_rds("data-raw/bra_aracaju/maptile_crop_mapbox_rma_2019.rds")
my_bound <- sf::st_transform(x = sf_zona2,crs = 3857)
#my_bound <- my_bound[my_bound$Zona != 164,]

bound_bbox <- sf::st_bbox(sf::st_buffer(my_bound,2000)) 

bound_bbox

my_tile_crop <- my_tile[my_tile$x > bound_bbox[[1]] & 
                          my_tile$x < bound_bbox[[3]] &
                          my_tile$y > bound_bbox[[2]] & 
                          my_tile$y < bound_bbox[[4]],]



plot_f <- function(var,pal,title,mycap = NULL,myfill = "Número de\n viagens"){ 
  
  # var = "publico"; pal = "D"
  # rm_zona = FALSE;cap = TRUE;title = "Publico"
  
  tmp_input <- my_bound[my_bound$value > 0 & 
                          my_bound$variable == var,]
  
  # if(is.null(cap)){ mycap <- "*Horário de pico da manhã (06h - 08h)" }else{
  #   mycap <- NULL }
  
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
    geom_sf(data = my_bound
            ,color = "grey5"
              ,linetype = "solid"
            ,alpha = 0.035, linewidth = 0.055,fill = NA) +
    ggspatial::annotation_scale(style = "ticks",
                                location = "bl",
                                text_family = "Encode Sans",
                                text_face = "bold",
                                text_cex = 0.75,
                                line_width = 1,
                                width_hint = 0.10,
                                pad_x = unit(0.35, "cm"),
                                pad_y = unit(0.35, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      style = north_arrow_minimal(text_size = 0)
      , location = "tl") +
    
    # labels and theme
    labs(title = title
         ,fill = myfill
         ,caption = mycap
         #, fill =  "Número de \nparadas"
         , color = NULL
         , x = NULL
         , y = NULL) +
    theme(legend.position = c(0.80,0.175),
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
          plot.margin=unit(c(0.1,2,0,1),"mm"),
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
  
  
  
  return(a)
}


# p1) public x privado -----
od1$div_mod[1,]

t_publico <- plot_f(var = "publico",title = "Transporte Público",pal = "D"
                    ,mycap = "*Período de 08h15 a 09h14")
t_publico

t_individ <- plot_f(var = "privado",title = "Transporte Individual",pal = "A")

pf <- (t_individ + t_publico) + 
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")

pf

# save
ggplot2::ggsave(pf,filename = "figures/bra_aracaju/maps_ind_publico.png",
                scale = 1.0,width = 18,height = 15,
                bg = "white",
                units = "cm",dpi = 300)
# p1.1) public x privado(PROP) -----

tmp_input <- my_bound[my_bound$variable %in% c("r_privado"
                                                 ,"r_publico"),]
tmp_bound <- my_bound[
  my_bound$variable %in%  c("r_publico","r_privado"),]

pal = "D"
myfill = "Proporção (%)"
mycap = "*Período de 08h15 a 09h14"

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
  # facet
  facet_wrap(~variable
             ,labeller = as_labeller(c(
               `r_publico` = "Transporte Público",
               `r_privado` = "Transporte Individual"
             )))+  
  # add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = tmp_bound
          ,color = "grey5"
            ,linetype = "solid"
          ,alpha = 0.035, linewidth = 0.055,fill = NA) +
  ggspatial::annotation_scale(style = "ticks",
                              location = "bl",
                              text_family = "Encode Sans",
                              text_face = "bold",
                              text_cex = 0.75,
                              line_width = 1,
                              width_hint = 0.10,
                              pad_x = unit(0.35, "cm"),
                              pad_y = unit(0.35, "cm")
  ) +
  ggspatial::annotation_north_arrow(
    style = north_arrow_minimal(text_size = 0)
    , location = "tl") +
  
  # labels and theme
  labs(title = NULL
       ,fill = myfill
       ,caption = mycap
       #, fill =  "Número de \nparadas"
       , color = NULL
       , x = NULL
       , y = NULL) +
  theme(legend.position = c(0.85,0.175),
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
        plot.margin=unit(c(0.1,2,0,1),"mm"),
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


# save
ggplot2::ggsave(a,filename = "figures/bra_aracaju/maps_ind_publico_facet.png",
                scale = 1.0,width = 18,height = 15,
                bg = "white",
                units = "cm",dpi = 300)

# p2) producao atracao -----
od1$dt_pico_mot_geracao_nn[1,]

t_produ <- plot_f(var = "prod_8h15_9h15",title = "Produção",pal = "D")
t_attra <- plot_f(var = "atra_8h15_9h15"
                  ,mycap = "*Período de 08h15 a 09h14"
                  ,title = "Atração",pal = "C")

pf <- (t_produ + t_attra) + 
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")
pf

# save
ggplot2::ggsave(pf,filename = "figures/bra_aracaju/maps_prod_atr.png",
                scale = 1.0,width = 18,height = 15,
                bg = "white",
                units = "cm",dpi = 300)

# p3) taxa viagem ----
od1$dt_pico_mot_viagens1[viagens_hab > 0,]

t_taxa_trip <- plot_f(var = "viagens_hab"
                      ,title = "Taxa de viagens"
                      ,mycap = "*Período de 08h15 - 09h14"
                      ,myfill = "Viagens per\n capita"
                      ,pal = "C")
t_taxa_trip
# save
ggplot2::ggsave(t_taxa_trip
                ,filename = "figures/bra_aracaju/rma_taxa_trip.png"
                ,scale = 1.0,width = 9,height = 15,
                bg = "white",
                units = "cm",dpi = 300)

# CROP

ind_org <- plot_f("origem","individual","E",FALSE,"Individual (Origem)",TRUE)
ind_des <- plot_f("destino","individual","E",FALSE,"Individual (Destino)",TRUE)
col_org <- plot_f("origem","coletivo","D",FALSE,"Coletivo (Origem)",TRUE)
col_des <- plot_f("destino","coletivo","D",TRUE,"Coletivo (Destino)",TRUE) 
col_des
ind_org

pf <- (ind_org + ind_des) / (col_org + col_des) + 
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")

# save
ggplot2::ggsave(pf,filename = "figures/bra_palmas/maps_OD_rm.png",
                scale = 1.0,width = 18,height = 30,
                bg = "white",
                units = "cm",dpi = 300)
dir.create("figures/bra_palmas/")

# outros ----
od1$div_mod[order(r_publico,decreasing = T),]







# End -----