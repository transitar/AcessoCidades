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
zt <- readr::read_rds("data/bra_cit/zona_trafego.rds")
setDT(zt)
zt[,r_ti := 100 * round(o_ti/(o_tp + o_ti),3)]
zt[,r_tp := 100 * round(o_tp/(o_tp + o_ti),3)]

zt <- zt %>% 
  melt.data.table(id.vars = c("zt","geometry")
                  ,measure.vars = c('r_ti', 'r_tp'))

#  Read Tile -----
my_tile <- readr::read_rds("data-raw/mapbox/maptile_crop_mapbox_cit_2019.rds")

# bbox
zt_sf <- sf::st_as_sf(zt)
zt_sf <- sf::st_transform(x = zt_sf,3857)
bound_bbox <- sf::st_bbox(sf::st_buffer(zt_sf[zt_sf$zt != "zt_NA",],1000)) 

# crop
my_tile_crop <- my_tile[my_tile$x > bound_bbox[[1]] & 
                          my_tile$x < bound_bbox[[3]] &
                          my_tile$y > bound_bbox[[2]] & 
                          my_tile$y < bound_bbox[[4]],]


plot_f <- function(dt,var,pal,title = NULL,subtitle,myfill,mycap = NULL){ 
  tmp_input <- copy(dt)
  tmp_input <- tmp_input[tmp_input$variable %like% var,]
  tmp_input <- tmp_input[tmp_input$zt != "zt_NA",]
  tmp_input <- tmp_input[order(tmp_input$value,decreasing = TRUE),]
  # label cut
  tmp_vec <- as.numeric(tmp_input$value)
  tmp_max <- max(tmp_vec)
  tmp_min <- min(tmp_vec)-1
  my_breaks <- seq(tmp_max, tmp_min, length.out = 6)
  
  tmp_input$taxa_cut_label <- cut(tmp_vec
                                  , breaks = my_breaks)
  tmp_input$taxa_cut_label <- tmp_input$taxa_cut_label %>% 
    gsub("[,]","*",x = .) %>% 
    gsub("[.]",",",x = .) %>% 
    gsub("[*]"," – ",x = .) %>% 
    gsub("\\(-1","[0",x = .) 
  tmp_input$taxa_cut_label <- factor(tmp_input$taxa_cut_label
                                     ,levels = unique(tmp_input$taxa_cut_label))
  
  wp <- tmp_input %>% sf::st_centroid() %>% 
    sf::st_coordinates() %>% as.data.frame() %>% as.data.table() %>% 
    .[,zt := tmp_input$zt] %>% 
    .[as.data.table(tmp_input),on = "zt",label := i.value] 
  # if(is.null(cap)){ mycap <- "*Horário de pico da manhã (06h - 08h)" }else{
  #   mycap <- NULL }
  # facet
  if(!is.null(title)) tmp_input$title <- title
  
  a <- ggplot()+
    geom_raster(data = my_tile_crop, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    #  add hex
    ggnewscale::new_scale_fill() +
    geom_sf(data = tmp_input 
            ,mapping = aes(fill = taxa_cut_label)
            ,color = "grey5"
            ,linewidth = 0.155, alpha=.7) +
    ggplot2::scale_fill_viridis_d(option = pal
                                  ,direction = 1)+
    facet_grid(rows = if(!is.null(title)){vars(title)},
               ,switch = "y")+
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
         ,subtitle = subtitle
         ,fill = myfill
         ,caption = mycap
         #, fill =  "Número de \nparadas"
         , color = NULL
         , x = NULL
         , y = NULL) +
    theme(#legend.position = "bottom"
      #,legend.direction = "horizontal",
      legend.position = c(0.1750,0.225)
      ,strip.background.y = element_rect(fill = "white",colour = "grey9"
                                         ,linewidth = unit(0.2,"line"))
      ,strip.text.y = element_text(size = rel(1.5)
                                   , family = "Encode Sans"
                                   , face = "plain"
                                   , margin = margin(0,0.3,0,0.3, "cm"))
      ,plot.caption = element_text(size = rel(1.0)
                                   , family = "Encode Sans"
                                   , face = "plain"),
      legend.key.width=unit(2,"line"),
      legend.text = element_text(size = rel(0.8)
                                 , family = "Encode Sans"
                                 , face = "plain"),
      legend.title = element_text(size = rel(0.75)
                                  , family = "Encode Sans"
                                  , face = "bold")
      ,plot.subtitle =  element_text(size = rel(0.85)
                                     , family = "Encode Sans"
                                     , face = "plain")
      ,title = element_text(size = 13
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



# MOdos agregados
r_ti_plot <- plot_f(dt = zt_sf
                    ,var = "r_ti"
                    ,pal = "D"
                    ,subtitle = NULL
                    ,title = "Transporte Individual"
                    ,myfill = "Percentual de\n viagens (%)"
                    ,mycap = NULL)
r_tp_plot <- plot_f(dt = zt_sf
                    ,var = "r_tp"
                    ,pal = "A"
                    ,subtitle = NULL
                    ,title = "Transporte Público"
                    ,myfill = "Percentual de\n viagens (%)"
                    ,mycap = "*Informações sobre transporte ativo não foram incluídas devido à indisponibilidade de dados.\nFonte: Adaptado do Plano de Mobilidade Urbana (LOGIT, 2013).")
r_tp_plot

pf <- (r_ti_plot | r_tp_plot ) + 
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")") + 
  plot_layout(nrow = 1, byrow = TRUE)
pf
break()
ggplot2::ggsave(pf,filename = "figures/bra_cit/ti_tp_od_perc.png",
                scale = 0.85,width = 40,height = 15.5,
                bg = "white",
                units = "cm",dpi = 300)
# save
#dir.create("figures/bra_nh/")
ggplot2::ggsave(pf,filename = "figures/bra_nh/modos_agregados.png",
                scale = 1.1,width = 30,height = 10,
                bg = "white",
                units = "cm",dpi = 300)


# End -----