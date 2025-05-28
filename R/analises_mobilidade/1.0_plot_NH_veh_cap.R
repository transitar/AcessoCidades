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

# Bairros----------------


nh_bair <- geobr::read_neighborhood()
nh_bair <- nh_bair[nh_bair$code_muni == 4313409,]
setDT(nh_bair)
nh_bair$name_neighborhood

z1 <- c(  1.44  ,"Alpes Do Vale" 
         , 0.78 ,"Boa Saúde"     
         , 0.81 ,"Boa Vista"     
         , 0.89 , "Canudos"       
         , 0.63 , "Centro"        
         , 0.66 , "Diehl"         
         , 0.77 , "Guarani"       
         , 0.94 , "Hamburgo Velho"
         , 1.09 ,"Ideal"         
         , 0.86 , "Industrial"    
         , 0.83 , "Liberdade"     
         ,    1 , "Lomba Grande"  
         , 0.89 , "Mauá"          
         , 0.55 , "Operário"      
         , 0.66 , "Ouro Branco"   
         , 1.03 , "Pátria Nova"   
         , 0.68 , "Petrópolis"    
         , 0.89 , "Primavera"     
         , 0.63 , "Rincão"        
         , 0.60 , "Rio Branco"    
         , 1.18 , "Rondônia"      
         , 1.23 , "Roselândia"    
         , 0.71 , "Santo Afonso"  
         , 0.89 , "São Jorge"     
         , 1    , "São José"      
         , 1    , "Vila Nova"     
         , 1.02 , "Vila Rosa")
z_names <- z1[seq(2,54,2)]
z_values <-z1[seq(1,54,2)]
z1 <- data.table(
  "names" = z_names,
  "values" = z_values
)
nh_bair[z1,on = c("name_neighborhood" = "names"),taxa_mot := i.values]
nb_bair[,taxa_mot := as.numeric(taxa_mot)]

#  Read Tile -----

my_tile <- readr::read_rds("data-raw/mapbox/maptile_crop_mapbox_noh_2019.rds")

# bbox
nh_bair_sf <- sf::st_as_sf(nh_bair)
nh_bair_sf <- sf::st_transform(x = nh_bair_sf,3857)
nh_bair_sf$taxa_mot <- as.numeric(nh_bair_sf$taxa_mot <-)
bound_bbox <- sf::st_bbox(sf::st_buffer(nh_bair_sf,2000)) 

# crop
my_tile_crop <- my_tile[my_tile$x > bound_bbox[[1]] & 
                          my_tile$x < bound_bbox[[3]] &
                          my_tile$y > bound_bbox[[2]] & 
                          my_tile$y < bound_bbox[[4]],]
  
  
  # if(is.null(cap)){ mycap <- "*Horário de pico da manhã (06h - 08h)" }else{
  #   mycap <- NULL }
  
nh_bair_sf$taxa_cut_label <- cut(as.numeric(nh_bair_sf$taxa_mot)
                           , breaks = seq(1.5, 0.5
                                          , by = -0.2))
nh_bair_sf$taxa_cut_label <-  nh_bair_sf$taxa_cut_label %>% 
  gsub("[,]","*",x = .) %>% 
  gsub("[.]",",",x = .) %>% 
  gsub("[*]"," – ",x = .)
  
 ggplot()+
    geom_raster(data = my_tile_crop, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    #  add hex
    ggnewscale::new_scale_fill() +
    geom_sf(data = nh_bair_sf 
            ,mapping = aes(fill = taxa_cut_label)
            ,color = "grey5"
            ,linewidth = 0.155) +
    ggplot2::scale_fill_viridis_d(option = "D"
                                ,direction = 1)+
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
    labs(title = NULL
         ,fill = "Taxa de motorização\n(automóveis/habitante)"
         ,caption = "Fonte: Plano Diretor de Mobilidade Urbana de Novo Hamburgo (2019)."
         #, fill =  "Número de \nparadas"
         , color = NULL
         , x = NULL
         , y = NULL) +
    theme(legend.position = "right"
      #legend.direction = "horizontal",
      #legend.position = c(0.1250,0.225)
      ,plot.caption = element_text(size = rel(0.5)
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
  

# save
#dir.create("figures/bra_nh/")
ggplot2::ggsave(filename = "figures/bra_nh/taxa_motorizacao.png",
                scale = 1.1,width = 15,height = 10,
                bg = "white",
                units = "cm",dpi = 300)


# End -----