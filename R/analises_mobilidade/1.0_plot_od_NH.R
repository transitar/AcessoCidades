# Load ----

rm(list=ls())
gc(reset = TRUE)

# install.packages('easypackages')
easypackages::packages('geobr'
                       , 'gtfs2gps'
                       , 'openxlsx'
                       , 'ggrepel'
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

# Bairros----------------


nh_bair <- geobr::read_neighborhood()
nh_bair <- nh_bair[nh_bair$code_muni == 4313409,]
setDT(nh_bair)
nh_bair$name_neighborhood

z1 <- c('Lomba Grande')
z2 <- c('Primavera', 'Rincão', 'Boa Saúde','Petrópolis')
z3 <- c('Alpes Do Vale', 'São José', 'Diehl', 'Roselândia', 'São Jorge')
z4 <- c('Vila Nova', 'Hamburgo Velho', 'Vila Rosa', 'Guarani','Operário')
z5 <- c('Pátria Nova', 'Rio Branco', 'Boa Vista', 'Centro'
        , 'Ideal', 'Mauá','Ouro Branco')
z6 <- c('Santo Afonso', 'Rondônia', 'Industrial','Liberdade')
z7 <- c('Canudos')

nh_bair[,zona := 
          fcase(name_neighborhood %in% z1,"z1",
                name_neighborhood %in% z2,"z2",
                name_neighborhood %in% z3,"z3",
                name_neighborhood %in% z4,"z4",
                name_neighborhood %in% z5,"z5",
                name_neighborhood %in% z6,"z6",
                name_neighborhood %in% z7,"z7"
          )]

# add info OD (tipo 1)
nh_bair[zona == "z1",":="(mob_ativ = 5.00,mod_priv = 72.0,mod_publ = 23.0)]
nh_bair[zona == "z2",":="(mob_ativ = 18.0,mod_priv = 52.9,mod_publ = 29.1)]
nh_bair[zona == "z3",":="(mob_ativ = 24.9,mod_priv = 51.7,mod_publ = 20.5)]
nh_bair[zona == "z4",":="(mob_ativ = 21.0,mod_priv = 58.3,mod_publ = 20.7)]
nh_bair[zona == "z5",":="(mob_ativ = 16.6,mod_priv = 65.6,mod_publ = 17.8)]
nh_bair[zona == "z6",":="(mob_ativ = 19.1,mod_priv = 48.3,mod_publ = 30.3)]
nh_bair[zona == "z7",":="(mob_ativ = 17.7,mod_priv = 60.6,mod_publ = 21.6)]
# add info OD (tipo 2)
nh_bair[zona == "z1",":="(walk = 5.00,bike = 0.0,truck = 0.0,car = 72.0,moto = 0.0,bus_inter = 0.0,bus_urb = 21.3,taxi = 0.0,trem = 0.0,uber = 1.7)]
nh_bair[zona == "z2",":="(walk = 18.0,bike = 0.0,truck = 0.0,car = 52.9,moto = 0.0,bus_inter = 0.0,bus_urb = 27.9,taxi = 0.6,trem = 0.0,uber = 0.6)]
nh_bair[zona == "z3",":="(walk = 24.9,bike = 0.0,truck = 2.9,car = 51.7,moto = 0.0,bus_inter = 0.0,bus_urb = 17.2,taxi = 1.2,trem = 1.1,uber = 1.0)]
nh_bair[zona == "z4",":="(walk = 21.0,bike = 0.0,truck = 0.0,car = 53.6,moto = 4.7,bus_inter = 1.1,bus_urb = 16.5,taxi = 1.1,trem = 0.0,uber = 2.0)]
nh_bair[zona == "z5",":="(walk = 15.6,bike = 1.0,truck = 0.0,car = 62.3,moto = 3.3,bus_inter = 0.9,bus_urb = 12.9,taxi = 0.9,trem = 1.8,uber = 1.4)]
nh_bair[zona == "z6",":="(walk = 13.8,bike = 5.3,truck = 2.3,car = 45.7,moto = 2.6,bus_inter = 1.2,bus_urb = 19.4,taxi = 0.0,trem = 6.9,uber = 2.9)]
nh_bair[zona == "z7",":="(walk = 15.8,bike = 1.9,truck = 0.0,car = 55.3,moto = 5.3,bus_inter = 0.0,bus_urb = 18.7,taxi = 0.0,trem = 0.5,uber = 2.4)]

rm(z1);rm(z2);rm(z3);rm(z4);rm(z5);rm(z6);rm(z7)

nh_bair <- nh_bair %>% 
  melt.data.table(id.vars = c("name_muni","name_neighborhood","code_neighborhood"
                              ,"zona","geom")
                  ,measure.vars = c('mob_ativ','mod_priv','mod_publ'
                                    ,'walk','bike','truck','car','moto'
                                    ,'bus_inter','bus_urb','taxi'
                                    ,'trem','uber'))

nh_bair[,type := fifelse(variable %in% c('mob_ativ','mod_priv','mod_publ')
                       ,"agregado","individual")]

#  Read Tile -----

my_tile <- readr::read_rds("data-raw/mapbox/maptile_crop_mapbox_noh_2019.rds")

# bbox
nh_bair_sf <- sf::st_as_sf(nh_bair)
nh_bair_sf <- sf::st_transform(x = nh_bair_sf,3857)
bound_bbox <- sf::st_bbox(sf::st_buffer(nh_bair_sf,2000)) 

nh_bair_sf_zona <- lapply(paste0("z",1:7),function(i){ # i = "z1"
  message(i)
  tmp <- nh_bair_sf[nh_bair_sf$zona == i,]
  df = data.frame("zona" = i)
  df$geom <-  sf::st_union(tmp)
  df <- sf::st_as_sf(df)
  df <- sf::st_buffer(df,15)
  df <- sf::st_cast(df,"MULTIPOLYGON")
  return(df)
}) %>% data.table::rbindlist() %>% sf::st_as_sf()

# crop
my_tile_crop <- my_tile[my_tile$x > bound_bbox[[1]] & 
                          my_tile$x < bound_bbox[[3]] &
                          my_tile$y > bound_bbox[[2]] & 
                          my_tile$y < bound_bbox[[4]],]


plot_f <- function(dt,nh_bair_sf_zona,var,type,pal,pal_dir = -1,title,myfill,mycap = NULL){ 
  # 
  # dt = nh_bair_sf
  # nh_bair_sf_zona
  # var = "car"
  # type = "individual"
  # pal = "D"
  # pal_dir = 1
  # title = "Automóvel"
  # myfill = "Repartição\nmodal (%)"
  # mycap = NULL

  tmp_input <- dt[dt$variable == var & 
                    dt$type == type,]
  tmp_input_dt <- as.data.table(copy(tmp_input))
  
  wp <- sf::st_centroid(nh_bair_sf_zona) %>% 
    sf::st_coordinates() %>% as.data.table() %>% 
    .[,zona := nh_bair_sf_zona$zona] %>% 
    .[tmp_input,on = "zona",label := i.value] %>% 
    .[,label := paste0(label,"%")]
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
            ,color = "grey5"
            ,linewidth = 0.055) +
    viridis::scale_fill_viridis(option = pal
                                ,direction = pal_dir)+
    # add boundary
    ggnewscale::new_scale_color() +
    geom_sf(data = nh_bair_sf_zona
            ,color = "grey5"
            ,linetype = "solid"
            ,alpha = 1, linewidth = 0.555,fill = NA) +
    ggrepel::geom_label_repel(data= wp,aes(x = X,y = Y,label = label),
              color = "black",fontface = "bold", point.size = NA,size = 3.5) +
    ggspatial::annotation_scale(style = "ticks",
                                location = "br",
                                text_family = "Encode Sans",
                                text_face = "bold",
                                text_cex = 0.75,
                                line_width = 1,
                                width_hint = 0.10,
                                pad_x = unit(0.35, "cm"),
                                pad_y = unit(0.35, "cm"))+ 
    ggspatial::annotation_north_arrow(
      style = north_arrow_minimal(text_size = 0)
      , location = "tr") +
    
    # labels and theme
    labs(title = title
         ,fill = myfill
         ,caption = mycap
         #, fill =  "Número de \nparadas"
         , color = NULL
         , x = NULL
         , y = NULL) +
    theme(#legend.position = "bottom"
          #,legend.direction = "horizontal",
          legend.position = c(0.1250,0.225)
          ,plot.caption = element_text(size = rel(1.0)
                                       , family = "Encode Sans"
                                       , face = "plain"),
          legend.key.width=unit(2,"line"),
          legend.text = element_text(size = rel(0.8)
                                     , family = "Encode Sans"
                                     , face = "plain"),
          legend.title = element_text(size = rel(0.75)
                                      , family = "Encode Sans"
                                      , face = "bold"),
          title = element_text(size = 13
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



# ORIGINAL
nh_bair_sf$variable %>% unique() %>% as.character()
nh_bair_sf$type %>% unique() %>% as.character()


# MOdos agregados
mod_priv_plot <- plot_f(dt = nh_bair_sf,nh_bair_sf_zona = nh_bair_sf_zona
                   ,var = "mod_priv",type = "agregado"
                   ,pal = "D",pal_dir = -1,title = "Transporte individual motorizado"
                   ,myfill = "Repartição\nmodal (%)"
                   ,mycap = NULL)
mod_publ_plot <- plot_f(dt = nh_bair_sf,nh_bair_sf_zona = nh_bair_sf_zona
                        ,var = "mod_publ",type = "agregado"
                        ,pal = "B",pal_dir = -1,title = "Transporte coletivo"
                        ,myfill = "Repartição\nmodal (%)"
                        ,mycap = NULL)
mod_ativ_plot <- plot_f(dt = nh_bair_sf,nh_bair_sf_zona = nh_bair_sf_zona
                        ,var = "mob_ativ",type = "agregado"
                        ,pal = "E",pal_dir = -1,title = "Transporte ativo"
                        ,myfill = "Repartição\nmodal (%)"
                        ,mycap = "Fonte: Plano Diretor de Mobilidade Urbana de \nNovo Hamburgo (2019).")

pf <- (mod_priv_plot + mod_publ_plot + mod_ativ_plot) + 
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")") + 
  plot_layout(nrow = 2, byrow = TRUE)

ggplot2::ggsave(pf,filename = "figures/bra_nh/modos_agregados1.png",
                scale = 0.85,width = 30,height = 30,
                bg = "white",
                units = "cm",dpi = 300)
# save
#dir.create("figures/bra_nh/")
ggplot2::ggsave(pf,filename = "figures/bra_nh/modos_agregados.png",
                scale = 1.1,width = 30,height = 10,
                bg = "white",
                units = "cm",dpi = 300)


# End -----