#Acesso físico e financeiro transporte público

#Mapas e gráficos de transporte público

#Este código produz os mapas de cobertura de transporte público,
#gráfico de desigualdade por recortes na cobvertura, mapa de frequências,
#e gráfico de desigualdade por recorte nas frequências e mapa de acesso
#financeiro, juntamnete com o cleveland de desigualdade no acesso financeiro.

# rm(list = ls());gc()

source('./R/fun/setup.R')
sf_use_s2(TRUE)

#função de gráficos de transporte público

graficos_tp <- function(munis = "all"){

  faz_grafico_e_salva <- function(sigla_muni, width = 16.5, height = 16.5){
    
    message(paste("Executando gráficos de", munis_names$muni[which(munis_names$muni_abrev==sigla_muni)], "\n"))

# LEITURA DOS DADOS -------------------------------------------------------

    
    path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    
    dados_hex <- read_rds(sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni))
    
    
    if (munis_parameters$aprox_maptiles[which(munis_parameters$abrev_muni ==sigla_muni)]==1){
      
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019_aprox.rds',sigla_muni)
      
    } else {
      
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni) 
      
    }
    
    data_contorno <- read_rds(path_contorno)
    maptiles <- read_rds(path_maptiles)

    sigla_municipio <- sigla_muni
    decisao_muni <- planilha_municipios %>% filter(sigla_muni == abrev_muni)
    
    
    #TP
    
    if (decisao_muni$fonte_dados_tp == "muni_shape"){
      
      dados_peds <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                         sigla_muni, sigla_muni),
                                 layer = "tp_peds")
      
      dados_linhas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                    sigla_muni, sigla_muni),
                            layer = "tp_linhas")
      
      
    }

    if (sigla_muni %in% c("cit","man")){
      aguas <- st_read(sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                               sigla_muni,
                               sigla_muni),
                       layer = 'aguas')
    }
    
    dados_peds <- dados_peds %>% st_transform(decisao_muni$epsg)
    dados_linhas <- dados_linhas %>% st_transform(decisao_muni$epsg)
    
    dir.create(sprintf('../data/tp_cobertura/muni_%s/', sigla_muni))
    
    dados_peds_buffer_300 <- dados_peds %>% st_buffer(300) %>% st_union() %>% st_as_sf()
    st_write(dados_peds_buffer_300, sprintf('../data/tp_cobertura/muni_%s/buffer_300_%s.gpkg', sigla_muni, sigla_muni),
             append = F)
    dados_peds_buffer_500 <- dados_peds %>% st_buffer(500) %>% st_union() %>% st_as_sf()
    st_write(dados_peds_buffer_500, sprintf('../data/tp_cobertura/muni_%s/buffer_500_%s.gpkg', sigla_muni, sigla_muni),
             append = F)
    
    dados_linhas_buffer_300 <- dados_linhas %>% st_buffer(300) %>% st_union() %>% st_as_sf()
    st_write(dados_linhas_buffer_300, sprintf('../data/tp_cobertura/muni_%s/buffer_linhas_300_%s.gpkg', sigla_muni, sigla_muni),
             append = F)
    dados_linhas_buffer_500 <- dados_linhas %>% st_buffer(500) %>% st_union() %>% st_as_sf()
    st_write(dados_linhas_buffer_500, sprintf('../data/tp_cobertura/muni_%s/buffer_linhas_500_%s.gpkg', sigla_muni, sigla_muni),
             append = F)
    
    if (sigla_muni == "rma"){
    area_natend300 <- data_contorno %>% st_transform(decisao_muni$epsg) %>%
      st_difference(dados_linhas_buffer_300)%>%
      st_union() %>% st_as_sf()
    
    area_natend500 <- data_contorno  %>% st_transform(decisao_muni$epsg) %>%
      st_difference(dados_linhas_buffer_500)%>%
      st_union() %>% st_as_sf()
    
    } else {
    
    #Com base na área de peds
    
    area_natend300 <- data_contorno %>% st_transform(decisao_muni$epsg) %>%
      st_difference(dados_peds_buffer_300)%>%
      st_union() %>% st_as_sf()
    
    area_natend500 <- data_contorno  %>% st_transform(decisao_muni$epsg) %>%
      st_difference(dados_peds_buffer_500)%>%
      st_union() %>% st_as_sf()
    # mapview(area_natend300)
    }
    
    
    
    area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
                                       sigla_muni)) %>% filter(DN == 24) %>%
      st_make_valid() %>%
      st_union()
    # mapview(area_urbanizada)
    
      simplepolys <-  st_make_valid(area_urbanizada) %>% st_simplify(area_urbanizada, dTolerance = 300) %>%
      st_make_valid() %>%
      st_transform(decisao_muni$epsg) %>%
      st_buffer(150) %>%
      st_union()
    
    # mapview(simplepolys)
    
    if (sigla_muni == "slz"){
      
      dados_areas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                 sigla_muni, sigla_muni), layer= "centro") %>% st_transform(decisao_muni$epsg) %>%
        st_make_valid()

    } else {
    dados_areas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                   sigla_muni, sigla_muni), layer= "areas") %>% st_transform(decisao_muni$epsg)
    
    }
    
    assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                                      sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
      mutate(title = "Assentamentos Precários") %>% st_make_valid() %>%
      st_union() %>%
      st_make_valid() %>%
      st_simplify(dTolerance = 300)
    

    
    assentamentos2 <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                                      sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
      mutate(title = "Assentamentos Precários") %>% st_make_valid()
    # mapview(assentamentos)
    
    dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                        sigla_muni, sigla_muni))
    data_micro2 <- dados_simulacao %>%
      # filter(code_tract %in% lista_tract) %>%
      select(1:12, V0606, hex) %>%
      mutate(V0606 = as.factor(V0606))
    
    
    #remocão dos habitantes de cor amarela e indígena
    levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
    data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
    
    pop_counts <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(pop_total = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% mutate(quintil = as.factor(ntile(pop_total, 4)))
    
    # mapview(pop_counts, zcol = "quintil")
    
    if (decisao_muni$terminais == 1){
      terminais <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                   sigla_muni, sigla_muni), layer= "terminais") %>% st_transform(decisao_muni$epsg)
    } else {
      terminais <- F
    }
    # mapview(terminais)
    
    #mapa localização
    cores_ciclo <- c('#5766cc', '#33b099', '#d96e0a')
    
    #área não urbana
    
    n_urb <- st_difference(st_transform(data_contorno, decisao_muni$epsg),
                           st_transform(simplepolys, decisao_muni$epsg))
    # mapview(n_urb)
    cor_aguas <- "#92c1e3"
    cor_ag <- "#2B6CB0"


# MAPA DAS LINHAS TP ------------------------------------------------------

    #colocar os bairros
    
    map_linhas_tp <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      

      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey40", linewidth = 0.4) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"

                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"

                        )) +
      
      guides(
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              fill = NA,
              aes(color = "urb"),
              linetype = "solid",
              linewidth = 0.8,
              alpha= 0.8)  +
      
      new_scale_fill() +

      geom_sf(data = dados_areas %>% st_transform(3857),
              aes(color = "areas"),
              fill = NA,
              linetype = "solid",
              linewidth = 0.4,
              alpha= 0.7) +
      
      geom_sf(data = assentamentos,
              aes(color = "aglomerados"),
              fill = cor_ag,
              linewidth = 0.8,
              show.legend = "polygon",
              alpha = 0.5)+
      

      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c(
                           "urb" = "grey40",
                           "areas" = "grey60",
                           "aglomerados" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(
        colour = guide_legend(override.aes = list(fill = c("white",
                                                           "white",
                                                           cor_ag
                                                           )
        ))
      ) +
      
      ggnewscale::new_scale_color() +

      geom_sf(data = st_transform(dados_linhas, 3857),
              aes(color = 'linhas'),
              alpha = 1,
              linewidth = 0.3) +
      
      #TERMINAIS
      

      # geom_sf(data = st_transform(terminais, 3857),
      #         aes(color = 'terminais'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         alpha = 1,
      #         fill = "grey20",
      #         size  = 2.5,
      #         shape = 15,
      #         linewidth = 0.3) +
      
      scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                         # values = c("linhas" = "#4b0082",
                         values = c("linhas" = "#8F040E",
                                    "terminais" = "grey20"),
                         label = c("linhas" = "Linhas de Transporte Público",
                                   "terminais" = "Terminais de Integração")
      )+

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
      # legend.title=element_text(size=30, family = "encode_sans_bold"),
      legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.20, 0.32),
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
    ) +
      guides(#fill = guide_legend(byrow = TRUE),
             color = guide_legend(override.aes = list(fill = c("white"#,
                                                               # "white"
                                                               ),
                                                      shape = c(NA#,
                                                                # 15
                                                                ),
                                                      # colour = c("#2B6CB0","black"),
                                                      size = c(1#,
                                                               # 2
                                                               ),
                                                      linetype = c(1#,
                                                                   # 0
                                                                   )
                                                      ),
                                  order = 3)) +

      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/5-linhas_tp_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )
    

    
    if (sigla_muni == "pal" | sigla_muni == "dou" | sigla_muni == "cit"){
    
    map_linhas_tp_sem_zoom <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey30", linewidth = 0.6) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                            order = 1,
                            byrow = TRUE)
      ) +
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              # fill = "#d8faf0",
              fill = NA,
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.8)  +
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      new_scale_fill() +
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      # geom_sf(data = dados_areas %>% st_transform(3857),
      #         # aes(size = 2),
      #         aes(color = "areas"),
      #         # color = "grey45",
      #         # aes(fill = '#CFF0FF'),
      #         fill = NA,
      #         linetype = "solid",
      #         # stroke = 2,
    #         # size = 2,
    #         linewidth = 0.4,
    #         alpha= 0.7) +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "aglomerados"),
              # "#0f805e"
              fill = cor_ag,
              linewidth = 0.5,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.5)+
      
      

      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c(#"urb" = "#d8faf0",
                           "urb" = "grey40",
                           "areas" = "grey60",
                           "aglomerados" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white",
                                                           # "#d8faf0",
                                                           # "white",
                                                           cor_ag)
                                                  # colour = c("grey25", "white", "white")
        ))
        # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
        #                     order = 1)
      ) +
      
      ggnewscale::new_scale_color() +
      
      geom_sf(data = st_transform(dados_linhas, 3857),
              aes(color = 'linhas'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              linewidth = 0.2) +
      
      # geom_sf(data = st_transform(terminais, 3857),
      #         aes(color = 'terminais'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         alpha = 1,
      #         fill = "grey20",
      #         size  = 2.5,
      #         shape = 15,
      #         linewidth = 0.3) +
      
    scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                       values = c("linhas" = "#8F040E",
                                  "terminais" = "grey20"),
                       label = c("linhas" = "Linhas de Transporte Público",
                                 "terminais" = "Terminais de Integração")
    )+
      
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
      # legend.title=element_text(size=30, family = "encode_sans_bold"),
      legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.17, 0.30),
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
    ) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white"#,
                                                          # "white"
        ),
        shape = c(NA#,
                  # 15
        ),
        # colour = c("#2B6CB0","black"),
        size = c(1#,
                 # 2
        ),
        linetype = c(1#,
                     # 0
        )
        ),
        order = 3)) +
      aproxima_muni_zoom(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp_sem_zoom,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/5-linhas_tp_sem_zoom_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = 1.62*13, height = 13, units = "cm" )
    
    }
    
    
# Mapa de linhas TP SVG ---------------------------------------------------

    map_linhas_tp_svg <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey40", linewidth = 0.4) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                        )) +
      
      guides(
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              fill = NA,
              aes(color = "urb"),
              linetype = "solid",
              linewidth = 0.8,
              alpha= 0.8)  +
      
      new_scale_fill() +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              aes(color = "areas"),
              fill = NA,
              linetype = "solid",
              linewidth = 0.4,
              alpha= 0.7) +
      
      geom_sf(data = assentamentos,
              aes(color = "aglomerados"),
              fill = cor_ag,
              linewidth = 0.8,
              show.legend = "polygon",
              alpha = 0.5)+
      
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c(#"urb" = "#d8faf0",
                           "urb" = "grey40",
                           "areas" = "grey60",
                           "aglomerados" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(
        colour = guide_legend(override.aes = list(fill = c("white",
                                                           # "#d8faf0",
                                                           "white",
                                                           cor_ag
        )
        ))
      ) +
      ggnewscale::new_scale_color() +

      geom_sf(data = st_transform(dados_linhas, 3857),
              aes(color = 'linhas'),
              alpha = 1,
              linewidth = 0.3) +
      
      #TERMINAIS
      
      
      # geom_sf(data = st_transform(terminais, 3857),
      #         aes(color = 'terminais'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         alpha = 1,
      #         fill = "grey20",
      #         size  = 2.5,
    #         shape = 15,
    #         linewidth = 0.3) +
    
    scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                       # values = c("linhas" = "#4b0082",
                       values = c("linhas" = "#8F040E",
                                  "terminais" = "grey20"),
                       label = c("linhas" = "Linhas de Transporte Público",
                                 "terminais" = "Terminais de Integração")
    )+
      
    ggspatial::annotation_scale(style = "ticks",
                                location = "br",
                                text_family = "encode_sans_bold",
                                text_cex = 1,
                                line_width = 1,
                                width_hint = 0.10,
                                pad_x = unit(0.35, "cm"),
                                pad_y = unit(0.35, "cm")
    ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tr") +
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
      legend.text=element_text(size=8, family = "encode_sans_light"),
      # legend.title=element_text(size=30, family = "encode_sans_bold"),
      legend.title=ggtext::element_markdown(size=10, family = "encode_sans_bold", lineheight = 0.15),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.20, 0.32),
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
    ) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white"#,
                                                          # "white"
        ),
        shape = c(NA#,
                  # 15
        ),
        # colour = c("#2B6CB0","black"),
        size = c(1#,
                 # 2
        ),
        linetype = c(1#,
                     # 0
        )
        ),
        order = 3)) +
      
      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp_svg,
           device = "svg",
           filename =  sprintf("../data/map_plots_transports/muni_%s/5-linhas_tp_%s.svg", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )    
    
    
# Mapa - Buffer PEDS 300 M-------------------------------------------------
    
    cor_ag <- "#2B6CB0"
    
    
    if (sigla_muni == "rma"){
      
      
      map_linhas_tp_buffer <- ggplot() +
        geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        new_scale_fill() +
        
        geom_sf(data = st_transform(pop_counts, 3857),
                aes(fill = quintil),
                colour = NA,
                alpha=1,
                size = 0)+
        
        
        
        geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey40", linewidth = 0.6) +
        
        scale_fill_manual(name = "População",
                          breaks = c("1", "2", "3", "4"),
                          values = c("1" = "#FEF8ED",
                                     "2" = "#fee6c3",
                                     "3" = "#fac690",
                                     "4" = "#e9a250"
                                     
                                     # "#33b099" = "#33b099",
                                     # "aglomerados" = "#0f805e"
                                     # 'n_urb' = '#d8faf0'
                          ),
                          label = c("1" = "25% menos populosos",
                                    
                                    "2" = "25% a 50% menos populosos",
                                    "3" = "25% a 50% mais populosos",
                                    "4" = "25% mais populosos"
                                    # "#33b099" = "Cobertura de 300m",
                                    # "aglomerados" = "Aglomerados subnormais"
                                    # "n_urb" = "Área urbanizada"
                          )) +
        
        guides(#fill = guide_legend(byrow = TRUE),
          # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
          fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                  color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                              order = 1,
                              byrow = TRUE)
        ) +
        
        new_scale_fill() +
        
        geom_sf(data = simplepolys %>% st_transform(3857),
                # aes(size = 2),
                # fill = "#d8faf0",
                fill = NA,
                aes(color = "urb"),
                # color = "grey45",
                # aes(fill = '#CFF0FF'),
                linetype = "solid",
                # fill = NA,
                # stroke = 2,
                # size = 2,
                linewidth = 0.8,
                alpha= 0.8)  +
        
        new_scale_fill() +
        geom_sf(data = dados_areas %>% st_transform(3857),
                # aes(size = 2),
                aes(color = "areas"),
                # color = "grey45",
                # aes(fill = '#CFF0FF'),
                fill = NA,
                linetype = "solid",
                # stroke = 2,
                # size = 2,
                linewidth = 0.4,
                alpha= 0.7) +
        
        geom_sf(data = assentamentos,
                # aes(fill = "#d96e0a"),
                aes(color = "aglomerados"),
                # "#0f805e"
                fill = cor_ag,
                linewidth = 0.8,
                # color = NA,
                show.legend = "polygon",
                alpha = 0.5)+
        
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c(#"urb" = "#d8faf0",
                           "urb" = "#8F040E",
                           "areas" = "grey60",
                           "aglomerados" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
        
        guides(#fill = guide_legend(byrow = TRUE),
          colour = guide_legend(override.aes = list(fill = c("white",
                                                             # "#d8faf0",
                                                             "white",
                                                             cor_ag
          )
          # colour = c("grey25", "white", "white")
          ))
          # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
          #                     order = 1)
        ) +
        
        
        
        ggnewscale::new_scale_color() +
        
        geom_sf(data = st_transform(dados_linhas_buffer_300, 3857),
                aes(color = 'buffer300'),
                # color = '#0f805e',
                # color = NA,
                fill = "grey70",
                alpha = 0.5,
                linewidth = 0.5) +
        
        # TERMINAIS
        geom_sf(data = st_transform(terminais, 3857),
                aes(color = 'terminais'),
                # color = '#0f805e',
                # color = NA,
                alpha = 1,
                fill = "grey20",
                size  = 2.5,
                shape = 15,
                linewidth = 0.3) +
      
      
      scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                         values = c("buffer300" = "#4b0082",
                                    "terminais" = "grey20"),
                         label = c("buffer300" = "Cobertura de 300m",
                                   "terminais" = "Terminais de Integração")
      )+
        
        # geom_sf(data = st_transform(aguas,3857), fill = cor_aguas, colour = NA) +
        
        ggspatial::annotation_scale(style = "ticks",
                                    location = "bl",
                                    text_family = "encode_sans_bold",
                                    text_cex = 3,
                                    line_width = 1,
                                    width_hint = 0.10,
                                    pad_x = unit(0.35, "cm"),
                                    pad_y = unit(0.35, "cm")
        ) +
        ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
        
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
          # legend.title=element_text(size=30, family = "encode_sans_bold"),
          legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
          plot.title = element_text(hjust = 0, vjust = 4),
          strip.text = element_text(size = 10),
          legend.position = c(0.80, 0.36),
          legend.box.background = element_rect(fill=alpha('white', 0.7),
                                               colour = "#A09C9C",
                                               linewidth = 0.8,
                                               linetype = "solid"),
          legend.background = element_blank(),
          legend.spacing.y = unit(0.1, 'cm'),
          legend.box.just = "left"
        ) +
        # guides(#fill = guide_legend(byrow = TRUE),
        #   color = guide_legend(override.aes = list(fill = c("grey70")),
        #                        order = 3)) +
        guides(#fill = guide_legend(byrow = TRUE),
          color = guide_legend(override.aes = list(fill = c("white",
                                                            "white"
          ),
          shape = c(NA,
                    15
          ),
          # colour = c("#2B6CB0","black"),
          size = c(1,
                   2
          ),
          linetype = c(1,
                       0
          )
          ),
          order = 3)) +
        
        # coord_sf(ylim = c(-1664674,-1684450), xlim = c(-4563278,-4531391), expand = FALSE)
        aproxima_muni(sigla_muni = sigla_muni)
      
      
    } else {
    
    
    map_linhas_tp_buffer <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      new_scale_fill() +
      
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey40", linewidth = 0.6) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              # fill = "#d8faf0",
              fill = NA,
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.8,
              alpha= 0.8)  +
      
      new_scale_fill() +
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              linetype = "solid",
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7) +
      
      # geom_sf(data = assentamentos,
      #         # aes(fill = "#d96e0a"),
      #         aes(color = "aglomerados"),
      #         # "#0f805e"
      #         fill = cor_ag,
      #         linewidth = 0.8,
      #         # color = NA,
      #         show.legend = "polygon",
      #         alpha = 0.5)+
      
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c(#"urb" = "#d8faf0",
                           "urb" = "#8F040E",
                           "areas" = "grey60",
                           "aglomerados" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white",
                                                           # "#d8faf0",
                                                           "white"#,
                                                           # cor_ag
                                                           )
                                                  # colour = c("grey25", "white", "white")
        ))
        # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
        #                     order = 1)
      ) +
      
      
      
      ggnewscale::new_scale_color() +
      
      geom_sf(data = st_transform(dados_peds_buffer_300, 3857),
              aes(color = 'buffer300'),
              # color = '#0f805e',
              # color = NA,
              fill = "grey70",
              alpha = 0.5,
              linewidth = 0.8) +
      
      #TERMINAIS
      # geom_sf(data = st_transform(terminais, 3857),
      #         aes(color = 'terminais'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         alpha = 1,
      #         fill = "grey20",
      #         size  = 2.5,
      #         shape = 15,
      #         linewidth = 0.3) +
      
      
      scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                         values = c("buffer300" = "#4b0082",
                                    "terminais" = "grey20"),
                         label = c("buffer300" = "Cobertura de 300m",
                                   "terminais" = "Terminais de Integração")
      )+
      
      # geom_sf(data = st_transform(aguas,3857), fill = cor_aguas, colour = NA) +
      
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
      # legend.title=element_text(size=30, family = "encode_sans_bold"),
      legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.16, 0.30),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.box.just = "left"
    ) +
      # guides(#fill = guide_legend(byrow = TRUE),
      #   color = guide_legend(override.aes = list(fill = c("grey70")),
      #                        order = 3)) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white"#,
                                                          # "white"
        ),
        shape = c(NA#,
                  # 15
        ),
        # colour = c("#2B6CB0","black"),
        size = c(1#,
                 # 2
        ),
        linetype = c(1#,
                     # 0
        )
        ),
        order = 3)) +
      
      # coord_sf(ylim = c(-1664674,-1684450), xlim = c(-4563278,-4531391), expand = FALSE)
      aproxima_muni(sigla_muni = sigla_muni)
    
    }
    
    
    ggsave(map_linhas_tp_buffer,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/6-linhas_tp_buffer_300m_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )
    
    

# Mapa de Buffer 300 sem zoom -------------------------------------------------

    map_linhas_tp_buffer <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      new_scale_fill() +
      
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey40", linewidth = 0.8) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              # fill = "#d8faf0",
              fill = NA,
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.8,
              alpha= 0.8)  +
      
      new_scale_fill() +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "aglomerados"),
              # "#0f805e"
              fill = cor_ag,
              linewidth = 0.8,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.5)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c(#"urb" = "#d8faf0",
                           "urb" = "#8F040E",
                           "areas" = "grey60",
                           "aglomerados" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white",
                                                           # "#d8faf0",
                                                           # "white",
                                                           cor_ag)
                                                  # colour = c("grey25", "white", "white")
        ))
        # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
        #                     order = 1)
      ) +
      
      ggnewscale::new_scale_color() +
      
      geom_sf(data = st_transform(dados_peds_buffer_300, 3857),
              aes(color = 'buffer300'),
              # color = '#0f805e',
              # color = NA,
              fill = "grey70",
              alpha = 0.5,
              linewidth = 0.8) +
      
      #TERMINAIS
      # geom_sf(data = st_transform(terminais, 3857),
      #         aes(color = 'terminais'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         alpha = 1,
      #         fill = "grey20",
      #         size  = 2.5,
      #         shape = 15,
      #         linewidth = 0.3) +
    
    
    scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                       values = c("buffer300" = "#4b0082",
                                  "terminais" = "grey20"),
                       label = c("buffer300" = "Cobertura de 300m",
                                 "terminais" = "Terminais de Integração")
    )+
      
      
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
        # legend.title=element_text(size=30, family = "encode_sans_bold"),
        legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
        plot.title = element_text(hjust = 0, vjust = 4),
        strip.text = element_text(size = 10),
        legend.position = c(0.22, 0.30),
        legend.box.background = element_rect(fill=alpha('white', 0.7),
                                             colour = "#A09C9C",
                                             linewidth = 0.8,
                                             linetype = "solid"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.box.just = "left"
      ) +
      # guides(#fill = guide_legend(byrow = TRUE),
      #   color = guide_legend(override.aes = list(fill = c("grey70")),
      #                        order = 3)) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white"#,
                                                          # "white"
        ),
        shape = c(NA#,
                  # 15
        ),
        # colour = c("#2B6CB0","black"),
        size = c(1#,
                 # 2
        ),
        linetype = c(1#,
                     # 0
        )
        ),
        order = 3)) +
      aproxima_muni_zoom(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp_buffer,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/6-linhas_tp_buffer_300m_%s_sem_zoom.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )
    
    
    
    

# MAPA PEDS BUFFER 500M ----------------------------------------------------------
    
    if (sigla_muni == "rma"){
     
      map_linhas_tp_buffer_500 <- ggplot() +
        geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
        coord_equal() +
        scale_fill_identity()+
        new_scale_fill() +
        
        geom_sf(data = st_transform(pop_counts, 3857),
                aes(fill = quintil),
                colour = NA,
                alpha=1,
                size = 0)+
        
        
        
        geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey40", linewidth = 0.6) +
        
        scale_fill_manual(name = "População",
                          breaks = c("1", "2", "3", "4"),
                          values = c("1" = "#FEF8ED",
                                     "2" = "#fee6c3",
                                     "3" = "#fac690",
                                     "4" = "#e9a250"
                                     
                                     # "#33b099" = "#33b099",
                                     # "aglomerados" = "#0f805e"
                                     # 'n_urb' = '#d8faf0'
                          ),
                          label = c("1" = "25% menos populosos",
                                    
                                    "2" = "25% a 50% menos populosos",
                                    "3" = "25% a 50% mais populosos",
                                    "4" = "25% mais populosos"
                                    # "#33b099" = "Cobertura de 300m",
                                    # "aglomerados" = "Aglomerados subnormais"
                                    # "n_urb" = "Área urbanizada"
                          )) +
        
        guides(#fill = guide_legend(byrow = TRUE),
          # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
          fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                  color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                              order = 1,
                              byrow = TRUE)
        ) +
        
        new_scale_fill() +
        
        geom_sf(data = simplepolys %>% st_transform(3857),
                # aes(size = 2),
                # fill = "#d8faf0",
                fill = NA,
                aes(color = "urb"),
                # color = "grey45",
                # aes(fill = '#CFF0FF'),
                linetype = "solid",
                # fill = NA,
                # stroke = 2,
                # size = 2,
                linewidth = 0.8,
                alpha= 0.8)  +
        
        new_scale_fill() +
        geom_sf(data = dados_areas %>% st_transform(3857),
                # aes(size = 2),
                aes(color = "areas"),
                # color = "grey45",
                # aes(fill = '#CFF0FF'),
                fill = NA,
                linetype = "solid",
                # stroke = 2,
                # size = 2,
                linewidth = 0.4,
                alpha= 0.7) +
        
        geom_sf(data = assentamentos,
                # aes(fill = "#d96e0a"),
                aes(color = "aglomerados"),
                # "#0f805e"
                fill = cor_ag,
                linewidth = 0.8,
                # color = NA,
                show.legend = "polygon",
                alpha = 0.5)+
        
        
        scale_color_manual(name = "Uso do solo",
                           breaks = c("areas", "urb", "aglomerados"),
                           values = c(#"urb" = "#d8faf0",
                             "urb" = "#8F040E",
                             "areas" = "grey60",
                             "aglomerados" = cor_ag),
                           label = c("urb" = "Área urbanizada",
                                     "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                     "aglomerados" = "Aglomerados Subnormais")
        )+
        
        guides(#fill = guide_legend(byrow = TRUE),
          colour = guide_legend(override.aes = list(fill = c("white",
                                                             # "#d8faf0",
                                                             "white",
                                                             cor_ag
          )
          # colour = c("grey25", "white", "white")
          ))
          # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
          #                     order = 1)
        ) +
        
        
        
        ggnewscale::new_scale_color() +
        
        geom_sf(data = st_transform(dados_linhas_buffer_500, 3857),
                aes(color = 'buffer300'),
                # color = '#0f805e',
                # color = NA,
                fill = "grey70",
                alpha = 0.5,
                linewidth = 0.5) +
        
        # TERMINAIS
        geom_sf(data = st_transform(terminais, 3857),
                aes(color = 'terminais'),
                # color = '#0f805e',
                # color = NA,
                alpha = 1,
                fill = "grey20",
                size  = 2.5,
                shape = 15,
                linewidth = 0.3) +
        
        
        scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                           values = c("buffer300" = "#4b0082",
                                      "terminais" = "grey20"),
                           label = c("buffer300" = "Cobertura de 300m",
                                     "terminais" = "Terminais de Integração")
        )+
        
        # geom_sf(data = st_transform(aguas,3857), fill = cor_aguas, colour = NA) +
        
        ggspatial::annotation_scale(style = "ticks",
                                    location = "bl",
                                    text_family = "encode_sans_bold",
                                    text_cex = 3,
                                    line_width = 1,
                                    width_hint = 0.10,
                                    pad_x = unit(0.35, "cm"),
                                    pad_y = unit(0.35, "cm")
        ) +
        ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
        
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
          # legend.title=element_text(size=30, family = "encode_sans_bold"),
          legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
          plot.title = element_text(hjust = 0, vjust = 4),
          strip.text = element_text(size = 10),
          legend.position = c(0.80, 0.36),
          legend.box.background = element_rect(fill=alpha('white', 0.7),
                                               colour = "#A09C9C",
                                               linewidth = 0.8,
                                               linetype = "solid"),
          legend.background = element_blank(),
          legend.spacing.y = unit(0.1, 'cm'),
          legend.box.just = "left"
        ) +
        # guides(#fill = guide_legend(byrow = TRUE),
        #   color = guide_legend(override.aes = list(fill = c("grey70")),
        #                        order = 3)) +
        guides(#fill = guide_legend(byrow = TRUE),
          color = guide_legend(override.aes = list(fill = c("white",
                                                            "white"
          ),
          shape = c(NA,
                    15
          ),
          # colour = c("#2B6CB0","black"),
          size = c(1,
                   2
          ),
          linetype = c(1,
                       0
          )
          ),
          order = 3)) +
        
        # coord_sf(ylim = c(-1664674,-1684450), xlim = c(-4563278,-4531391), expand = FALSE)
        aproxima_muni(sigla_muni = sigla_muni)
      
    } else {
    
    
    map_linhas_tp_buffer_500 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      new_scale_fill() +
      
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey40", linewidth = 0.6) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              # fill = "#d8faf0",
              fill = NA,
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.8,
              alpha= 0.8)  +
      
      new_scale_fill() +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              linetype = "solid",
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7) +
    
    # geom_sf(data = assentamentos,
    #         # aes(fill = "#d96e0a"),
    #         aes(color = "aglomerados"),
    #         # "#0f805e"
    #         fill = cor_ag,
    #         linewidth = 0.8,
    #         # color = NA,
    #         show.legend = "polygon",
    #         alpha = 0.5)+
    #   
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c(#"urb" = "#d8faf0",
                           "urb" = "#8F040E",
                           "areas" = "grey60",
                           "aglomerados" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white",
                                                           # "#d8faf0",
                                                           "white"#,
                                                           # cor_ag
                                                           )
                                                  # colour = c("grey25", "white", "white")
        ))
        # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
        #                     order = 1)
      ) +
      
      ggnewscale::new_scale_color() +
      
      geom_sf(data = st_transform(dados_peds_buffer_500, 3857),
              aes(color = 'buffer500'),
              # color = '#0f805e',
              # color = NA,
              fill = "grey70",
              alpha = 0.5,
              linewidth = 0.8) +
      
      #TERMINAIS
      # geom_sf(data = st_transform(terminais, 3857),
      #         aes(color = 'terminais'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         alpha = 1,
      #         fill = "grey20",
      #         size  = 2.5,
      #         shape = 15,
      #         linewidth = 0.3) +
    
    
    scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                       values = c("buffer500" = "#4b0082",
                                  "terminais" = "grey20"),
                       label = c("buffer500" = "Cobertura de 500m",
                                 "terminais" = "Terminais de Integração")
    )+
      
      # geom_sf(data = st_transform(aguas,3857), fill = cor_aguas, colour = NA) +
      
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
        # legend.title=element_text(size=30, family = "encode_sans_bold"),
        legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
        plot.title = element_text(hjust = 0, vjust = 4),
        strip.text = element_text(size = 10),
        legend.position = c(0.20, 0.32),
        legend.box.background = element_rect(fill=alpha('white', 0.7),
                                             colour = "#A09C9C",
                                             linewidth = 0.8,
                                             linetype = "solid"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.box.just = "left"
      ) +
      # guides(#fill = guide_legend(byrow = TRUE),
      #   color = guide_legend(override.aes = list(fill = c("grey70")),
      #                        order = 3)) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white"#,
                                                          # "white"
        ),
        shape = c(NA#,
                  # 15
        ),
        # colour = c("#2B6CB0","black"),
        size = c(1#,
                 # 2
        ),
        linetype = c(1#,
                     # 0
        )
        ),
        order = 3)) +
      
      # coord_sf(ylim = c(-1664674,-1684450), xlim = c(-4563278,-4531391), expand = FALSE)
      aproxima_muni(sigla_muni = sigla_muni)
    
    }
    
    ggsave(map_linhas_tp_buffer_500,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/7-linhas_tp_buffer_500m_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )

# Mapa buffer 500 sem zoom ------------------------------------------------
    map_linhas_tp_buffer_500 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      new_scale_fill() +
      
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey40", linewidth = 0.8) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              # fill = "#d8faf0",
              fill = NA,
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.8,
              alpha= 0.8)  +
      
      new_scale_fill() +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "aglomerados"),
              # "#0f805e"
              fill = cor_ag,
              linewidth = 0.8,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.5)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c(#"urb" = "#d8faf0",
                           "urb" = "#8F040E",
                           "areas" = "grey60",
                           "aglomerados" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white",
                                                           # "#d8faf0",
                                                           # "white",
                                                           cor_ag)
                                                  # colour = c("grey25", "white", "white")
        ))
        # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
        #                     order = 1)
      ) +
      
      ggnewscale::new_scale_color() +
      
      geom_sf(data = st_transform(dados_peds_buffer_500, 3857),
              aes(color = 'buffer500'),
              # color = '#0f805e',
              # color = NA,
              fill = "grey70",
              alpha = 0.5,
              linewidth = 0.8) +
      
      #TERMINAIS
      # geom_sf(data = st_transform(terminais, 3857),
      #         aes(color = 'terminais'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         alpha = 1,
      #         fill = "grey20",
      #         size  = 2.5,
      #         shape = 15,
      #         linewidth = 0.3) +
    
    
    scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                       values = c("buffer500" = "#4b0082",
                                  "terminais" = "grey20"),
                       label = c("buffer500" = "Cobertura de 500m",
                                 "terminais" = "Terminais de Integração")
    )+
      
      
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
        # legend.title=element_text(size=30, family = "encode_sans_bold"),
        legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
        plot.title = element_text(hjust = 0, vjust = 4),
        strip.text = element_text(size = 10),
        legend.position = c(0.22, 0.30),
        legend.box.background = element_rect(fill=alpha('white', 0.7),
                                             colour = "#A09C9C",
                                             linewidth = 0.8,
                                             linetype = "solid"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.box.just = "left"
      ) +
      # guides(#fill = guide_legend(byrow = TRUE),
      #   color = guide_legend(override.aes = list(fill = c("grey70")),
      #                        order = 3)) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white"#,
                                                          # "white"
        ),
        shape = c(NA#,
                  # 15
        ),
        # colour = c("#2B6CB0","black"),
        size = c(1#,
                 # 2
        ),
        linetype = c(1#,
                     # 0
        )
        ),
        order = 3)) +
      aproxima_muni_zoom(sigla_muni = sigla_muni)
    
    ggsave(map_linhas_tp_buffer_500,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/7-linhas_tp_buffer_500m_%s_sem_zoom.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )
    
    
    
    

# DIFERENCA DE 300M -------------------------------------------------------

    
      #mapa diferença de 300m
    
    map_peds300_diff <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(area_natend300, 3857),fill = 'grey30',
              color = NA,alpha = .7, size = 0.1) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
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
            legend.text=element_text("Tipo", size=rel(1)),
            legend.title=element_text(size=rel(1),                                   ),
            plot.title = element_text(hjust = 0, vjust = 4),
            strip.text = element_text(size = 10)
      )
    # width = 16; height = 16
    # map_empregos
    ggsave(map_peds300_diff,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/8-peds_n_atendido_300m_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
    # área n atendida 500m
    map_peds500_diff <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(area_natend500, 3857),fill = 'grey30',
              color = NA,alpha = .7, size = 0.1) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
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
            legend.text=element_text("Tipo", size=rel(1)),
            legend.title=element_text(size=rel(1),                                   ),
            plot.title = element_text(hjust = 0, vjust = 4),
            strip.text = element_text(size = 10)
      )
    # width = 16; height = 16
    # map_empregos
    ggsave(map_peds500_diff,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/9-peds_n_atendido_500m_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
    
    #intersect com hex

# Hex atendidos 300-----------------------------------------------------------

    
    
    #aqui esta usando os dados os dados civlovirarios. verificar
    # mapview(dados_hex_intersect_300)
    
    if (sigla_muni == "rma"){
      
      dados_hex_intersect_300 <- dados_hex %>%
        st_as_sf() %>%
        st_transform(decisao_muni$epsg) %>%
        st_intersection(dados_linhas_buffer_300)
      dados_hex_intersect_300 <- dados_hex_intersect_300 %>%
        mutate(area = st_area(.)) %>%
        mutate(area = as.numeric(area)) %>%
        filter(area >= 60000)
      
      
    } else {
    
      dados_hex_intersect_300 <- dados_hex %>%
        st_as_sf() %>%
        st_transform(decisao_muni$epsg) %>%
        st_intersection(dados_peds_buffer_300)
      dados_hex_intersect_300 <- dados_hex_intersect_300 %>%
        mutate(area = st_area(.)) %>%
        mutate(area = as.numeric(area)) %>%
        filter(area >= 60000)
    }
    
    if (sigla_muni == "rma"){
      
      arj <- geobr::read_municipality(code_muni = 2800308) %>% select(name_muni)
      nss <- geobr::read_municipality(code_muni = 2804805) %>% select(name_muni)
      bac <- geobr::read_municipality(code_muni = 2800605) %>% select(name_muni)
      sac <- geobr::read_municipality(code_muni = 2806701) %>% select(name_muni)
      
      hex_aju <- dados_hex %>%  st_transform(decisao_muni$epsg) %>%
        st_intersection(arj %>% st_transform(decisao_muni$epsg)) %>% mutate(sigla_muni = "aju") %>%
        mutate(area = as.numeric(st_area(.)))
      
      hex_nss <- dados_hex %>%  st_transform(decisao_muni$epsg) %>%
        st_intersection(nss %>% st_transform(decisao_muni$epsg)) %>% mutate(sigla_muni = "nss") %>%
        mutate(area = as.numeric(st_area(.)))
      
      hex_bac <- dados_hex %>%  st_transform(decisao_muni$epsg) %>%
        st_intersection(bac %>% st_transform(decisao_muni$epsg)) %>% mutate(sigla_muni = "bac") %>%
        mutate(area = as.numeric(st_area(.)))
      
      hex_sac <- dados_hex %>%  st_transform(decisao_muni$epsg) %>%
        st_intersection(sac %>% st_transform(decisao_muni$epsg)) %>% mutate(sigla_muni = "sac") %>%
        mutate(area = as.numeric(st_area(.)))
      
      dados_hex_rma <- rbind(hex_aju,
                             hex_nss,
                             hex_bac,
                             hex_sac)
      # mapview(dados_hex_rma)
      dados_hex_rma <- dados_hex_rma %>% group_by(id_hex) %>%
        arrange(-area) %>%
        slice(1) %>%
        ungroup()
      
      
      
    }
    # mapview(dados_hex_intersect_300)
    id_hex_intersects_bus <- dados_hex_intersect_300$id_hex %>% unique()
    
    #dados da microssimulação
    
    data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                   sigla_muni, sigla_muni)) %>%  mutate(V0606 = case_when(V0606 == 1 ~ "Brancos",
                                                                                          V0606 == 2 ~ "Pretos",
                                                                                          V0606 == 3 ~ "Amarelos",
                                                                                          V0606 == 4 ~ "Pardos",
                                                                                          V0606 == 5 ~ "Indígenas"))
    #ajeitar o formato
    # grid_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/grid_muni_%s.RDS',
    #                                sigla_muni, sigla_muni))
    # aaaa <- data_micro %>% st_drop_geometry() %>%
    #   group_by(hex) %>%
    #   summarise(n = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
    #   st_as_sf()
    # 
    # mapview(aaaa)
      
      
    #checar setores com todos os renda_class_pc == n_col
    
    # lista_tract <- data_micro %>% group_by(code_tract, renda_class_pc) %>%
    #   summarise(n = n()) %>% ungroup() %>%
    #   group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
    #                                      n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
    #   filter(n_classes > n_classes_col) %>% pull(code_tract)
    
    
    
    # data_micro2 <- data_micro %>% filter(code_tract %in% lista_tract) %>% select(1:12, V0606, hex) %>%
    #   mutate(V0606 = as.factor(V0606))
    
    data_micro2 <- data_micro %>% select(1:12, V0606, hex) %>%
      mutate(V0606 = as.factor(V0606))
    
    
    
    # aaaa <- data_micro2 %>% st_drop_geometry() %>%
    #   group_by(hex) %>%
    #   summarise(n = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
    #   st_as_sf()
    # 
    # mapview(aaaa)
    
    
    # levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
    if (sigla_muni == "dou"){
      
      data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos"))
      
    } else {
      
      data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
      
    }
    
    
    data_micro_bus <- data_micro2 %>% filter(hex %in% id_hex_intersects_bus)
    # mapview(data_micro_bus)
    #recorte nos dados da microssimulacao
    
    if (sigla_muni == "dou"){
      
      
      recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus), "OK","N"))  %>% 
        # mutate(total = "total") %>% 
        mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
               cor = case_when(V0606 == "Pardos" ~ "Pretos",
                               V0606 == "Pretos" ~ "Pretos",
                               V0606 == "Brancos" ~ "Brancos",
                               V0606 == "Indígenas" ~ "Indígenas")) %>%
        mutate(
          quintil_renda = ntile(Rend_pc, 4)) %>%
        group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
        ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
        summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
        mutate(class = paste(genero, cor))%>%
        mutate(id = paste(class, quintil_renda))
     
       
    } else {
      
      
      if (sigla_muni == "rma"){
        
        recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus), "OK","N"))  %>% 
          # mutate(total = "total") %>% 
          mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
                 cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                                 cor == "pretos" ~ "Pretos",
                                 cor == "brancos" ~ "Brancos")) %>%
          mutate(
            quintil_renda = ntile(Rend_pc, 4)) %>%
          
          left_join(dados_hex_rma %>% select(id_hex, sigla_muni),
                    by = c('hex' = 'id_hex')) %>%
          # quintil_renda = case_when(Rend_pc < 7.1 ~ 1,
          #                           Rend_pc < 60 ~ 2,
          #                           Rend_pc < 398 ~ 3,
          #                           Rend_pc >399 ~ 4)) %>%
          group_by(cor, quintil_renda, genero, teste, sigla_muni) %>% summarise(n = n()) %>% 
          ungroup() %>% group_by(cor, quintil_renda, genero, sigla_muni) %>% 
          summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
          mutate(class = paste(genero, cor))%>%
          mutate(id = paste(class, quintil_renda))
        
        resumo.infra_tp300 <- weighted.mean(recorte_rr$prop, w = recorte_rr$n)
        
        resumo.infra_tp300_rma <- recorte_rr %>% group_by(sigla_muni) %>%
          summarise(media_muni = sum(prop * n)/sum(n))
        
      }
    
    recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
      mutate(class = paste(genero, cor))%>%
      mutate(id = paste(class, quintil_renda))
    
    }
    
    recorte_renda <- recorte_rr %>%
      ungroup() %>% group_by(quintil_renda) %>%
      summarise(prop = weighted.mean(prop, w = n))
    
    recorte_cor_renda <- recorte_rr %>%
      ungroup() %>% group_by(quintil_renda, cor) %>%
      summarise(prop = weighted.mean(prop, w = n))
    
    resumo.infra_tp300 <- weighted.mean(recorte_rr$prop, w = recorte_rr$n)
    
    # teste_recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus), "OK","N")) %>%
    #   filter(teste == "OK") %>%
    #   group_by(hex) %>%
    #   summarise(n = n()) %>%
    #   left_join(dados_hex, by = c("hex" = "id_hex")) %>% st_as_sf()
    # mapview(teste_recorte_rr)
# Cleveland Plot Atendidos 300m -------------------------------------------
    options(scipen = 10000000)
    pop_max <- pop_max <- plyr::round_any(max(recorte_rr$n), 50000, f = ceiling)
    break_max <- pop_max
    break_leap <- break_max/4
    escala <- ifelse(pop_max > 150000, 12, ifelse( pop_max > 100000, 12, ifelse(pop_max > 50000, 12, 12)))
    
    range_tot <- max(recorte_rr$prop)-min(recorte_rr$prop)
    extend <- 0.01
    
    if (range_tot <= 0.01){
      passo <- 0.005
    } else if (range_tot <= 0.05) {
      passo <- 0.01
    } else if (range_tot <= 0.10){
      passo <- 0.025
    } else if (range_tot <= 0.25){
      passo <- 0.05
    } else {
      passo <- 0.10
    }
    
    range1 <- plyr::round_any(ifelse( (min(recorte_rr$prop) - extend)<0, 0, min(recorte_rr$prop) - extend),
                              passo,
                              f=floor)
    range2 <- plyr::round_any(ifelse( (max(recorte_rr$prop) + extend)>1, 1, max(recorte_rr$prop) + extend),
                              passo,
                              f=ceiling)
    
    if (sigla_muni == "dou"){
      
      plot_cleveland_bus300 <- ggplot(recorte_rr, aes(prop, as.character(quintil_renda))) +
        geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
        geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
        guides(fill=guide_legend(title="Gênero e cor")) +
        scale_fill_manual(values = c("#ADBEF0", "#174DE8", "#EBB814", "#B39229"),
        )+
        scale_size_continuous( range = c(0,escala),
                               limits = c(0,break_max),
                               breaks = c(break_leap,break_leap*2,break_leap*3),
                               name = "Habitantes",
                               guide = "legend")
      p_b300_bus <- plot_cleveland_bus300 + scale_color_manual(name = "Gênero e Cor",
                                                               values = c("Homens Brancos"="#ADBEF0",
                                                                          "Homens Pretos"="#174DE8",
                                                                          "Homens Indígenas"="black",
                                                                          "Mulheres Brancos" = "#EBB814",
                                                                          "Mulheres Pretos"="#B39229",
                                                                          "Mulheres Indígenas"="#cc3003"),
                                                               labels = c("Homens Brancos"="Homens Brancos",
                                                                          "Homens Pretos"="Homens Negros",
                                                                          "Mulheres Brancos"="Mulheres Brancas",
                                                                          "Mulheres Pretos"="Mulheres Negras",
                                                                          "Homens Indígenas"="Homens Indígenas",
                                                                          "Mulheres Indígenas"="Mulheres Indígenas"))+
        # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
        #                    limits = c(0,5), # baixa complexidade car                   
        #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
        scale_y_discrete(expand = c(0.4,0.4)) +
        labs(title = "População com acesso à infraestrutura de transporte público")+  
        #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
        # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
        # theme_minimal() +
        xlab("% de habitantes do recorte") +
        ylab("Quartil de renda per capita") +
        scale_x_continuous(labels = scales::percent,
                           limits = c(range1,range2),
                           breaks = seq(range1,range2, passo)) +
        scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
        theme(#axis.title = element_blank(),
          panel.grid.minor = element_line(),
          legend.background = element_blank(),
          panel.background = element_blank(),
          legend.direction = "vertical",
          legend.position = "bottom",
          text = element_text(family = "sans",
                              # face = "bold",
                              size = 20),
          
          plot.title = element_text(size = 35, margin = margin(b=10), family = "encode_sans_bold"),
          plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
          plot.caption = element_text(size = 30, margin = margin(t=10), color = "grey70", hjust = 0),
          legend.title = element_text(size = 35, family = "encode_sans_bold"),
          legend.text = element_text(size = 30, family = "encode_sans_light"),
          axis.text = element_text(size = 30, family = "encode_sans_light"),
          axis.title = element_text(size = 35, family = "encode_sans_bold"))
      
      
    } else {
    
    plot_cleveland_bus300 <- ggplot(recorte_rr, aes(prop, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#ADBEF0", "#174DE8", "#EBB814", "#B39229"),
      )+
      scale_size_continuous( range = c(0,escala),
                             limits = c(0,break_max),
                             breaks = c(break_leap,break_leap*2,break_leap*3),
                             name = "Habitantes",
                             guide = "legend")
    p_b300_bus <- plot_cleveland_bus300 + scale_color_manual(name = "Gênero e Cor",
                                                           values = c("Homens Brancos"="#ADBEF0",
                                                                      "Homens Pretos"="#174DE8",
                                                                      "Mulheres Brancos" = "#EBB814",
                                                                      "Mulheres Pretos"="#B39229"),
                                                           labels = c("Homens Brancos"="Homens Brancos",
                                                                      "Homens Pretos"= "Homens Negros",
                                                                      "Mulheres Brancos"="Mulheres Brancas",
                                                                      "Mulheres Pretos"="Mulheres Negras"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "População com acesso à infraestrutura de transporte público")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      # theme_minimal() +
      xlab("% de habitantes do recorte") +
      ylab("Quartil de renda per capita") +
      scale_x_continuous(labels = scales::percent,
                         limits = c(range1,range2),
                         breaks = seq(range1,range2, passo)) +
      scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
      theme(#axis.title = element_blank(),
        panel.grid.minor = element_line(),
        legend.background = element_blank(),
        panel.background = element_blank(),
        legend.direction = "vertical",
        legend.position = "bottom",
        text = element_text(family = "sans",
                            # face = "bold",
                            size = 20),
        
        plot.title = element_text(size = 35, margin = margin(b=10), family = "encode_sans_bold"),
        plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 30, margin = margin(t=10), color = "grey70", hjust = 0),
        legend.title = element_text(size = 35, family = "encode_sans_bold"),
        legend.text = element_text(size = 30, family = "encode_sans_light"),
        axis.text = element_text(size = 30, family = "encode_sans_light"),
        axis.title = element_text(size = 35, family = "encode_sans_bold"))
    
    }
    
    #escrita do gráfico
    
    
    # plot_cleveland_bus300 <- ggplot(recorte_rr, aes(prop, as.character(quintil_renda))) +
    #   geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
    #   geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
    #   guides(fill=guide_legend(title="Gênero e cor")) +
    #   scale_fill_manual(values = c("#ADBEF0", "#174DE8", "#EBB814", "#B39229"),
    #   )+
    #   scale_size_continuous( range = c(0,escala),
    #                          limits = c(0,break_max),
    #                          breaks = c(break_leap,break_leap*2,break_leap*3),
    #                          name = "Habitantes",
    #                          guide = "legend")
    # p_b300_bus <- plot_cleveland_bus300 + scale_color_manual(name = "Gênero e Cor",
    #                                                          values = c("Homens Brancos"="#ADBEF0",
    #                                                                     "Homens Pretos"="#5766cc",
    #                                                                     "Mulheres Brancos" = "#33b099",
    #                                                                     "Mulheres Pretos"="#d96e0a"),
    #                                                          labels = c("Homens Brancos"="Homens Brancos",
    #                                                                     "Homens Pretos"="Homens Negros",
    #                                                                     "Mulheres Brancos"="Mulheres Brancas",
    #                                                                     "Mulheres Pretos"="Mulheres Negras"))+
    #   # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
    #   #                    limits = c(0,5), # baixa complexidade car                   
    #   #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
    #   scale_y_discrete(expand = c(0.4,0.4)) +
    #   labs(title = "População com acesso à infraestrutura de transporte público")+  
    #   #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
    #   # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
    #   # theme_minimal() +
    #   xlab("% de habitantes do recorte") +
    #   ylab("Quartil de renda per capita") +
    #   scale_x_continuous(labels = scales::percent,
    #                      limits = c(range1,range2),
    #                      breaks = seq(range1,range2, passo)) +
    #   scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
    #   theme_bw() +
    #   theme(#axis.title = element_blank(),
    #     panel.grid.minor = element_blank(),
    #     panel.border = element_blank(),
    #     panel.grid = element_blank(),
    #     legend.background = element_blank(),
    #     panel.background = element_blank(),
    #     legend.direction = "vertical",
    #     legend.position = "bottom",
    #     legend.box = NULL,
    #     plot.background = element_rect(colour = "white"),
    #     text = element_text(family = "sans",
    #                         # face = "bold",
    #                         size = 10),
    #     
    #     plot.title = element_text(size = 10, margin = margin(b=10), family = "encode_sans_bold"),
    #     plot.subtitle = element_text(size=7, color = "darkslategrey", margin = margin(b = 25)),
    #     plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    #     legend.title = element_text(size = 10, family = "encode_sans_bold"),
    #     legend.text = element_text(size = 8, family = "encode_sans_light"),
    #     axis.text = element_text(size = 8, family = "encode_sans_light"),
    #     axis.title = element_text(size = 10, family = "encode_sans_bold"),
    #     legend.box.background = element_blank()) +
    #   guides(colour = guide_legend(order = 1,
    #                                override.aes = list(fill = rep("white", 4))),
    #          size = guide_legend(order = 2,
    #                              override.aes = list(fill = rep("white", 3))))
    
    
    
    if (sigla_muni == "dou"){
    
      ggsave(p_b300_bus,
             device = "png",
             filename =  sprintf("../data/map_plots_transports/muni_%s/11-linhasbus_300_cleveland_%s_new.png", sigla_muni, sigla_muni),
             dpi = 300,
             width = 15, height = 11, units = "cm" )
      
      
      } else {

    ggsave(p_b300_bus,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/11-linhasbus_300_cleveland_%s_insta2x.png", sigla_muni, sigla_muni),
           dpi = 600,
           width = 15, height = 10, units = "cm" )
        
        # ggsave(p_b300_bus,
        #        device = "svg",
        #        filename =  sprintf("../data/map_plots_transports/muni_%s/11-linhasbus_300_cleveland_%s_insta.svg", sigla_muni, sigla_muni),
        #        dpi = 300,
        #        width = 15, height = 10, units = "cm" )
    
      }
    

    

# Pessoas atendidas 500m --------------------------------------------------

    
    if (sigla_muni == "rma"){
      
      dados_hex_intersect_500 <- dados_hex %>%
        st_as_sf() %>%
        st_transform(decisao_muni$epsg) %>%
        st_intersection(dados_linhas_buffer_500)
      dados_hex_intersect_500 <- dados_hex_intersect_500 %>%
        mutate(area = st_area(.)) %>%
        mutate(area = as.numeric(area)) %>%
        filter(area >= 60000)
      
    } else {
    
    dados_hex_intersect_500 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(dados_peds_buffer_500)
    dados_hex_intersect_500 <- dados_hex_intersect_500 %>%
      mutate(area = st_area(.)) %>%
      mutate(area = as.numeric(area)) %>%
      filter(area >= 60000)
    
    }
    
    # mapview(dados_hex_intersect_300)
    id_hex_intersects_bus500 <- dados_hex_intersect_500$id_hex %>% unique()
    
    #dados da microssimulação
    
    data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                   sigla_muni, sigla_muni)) %>%  mutate(V0606 = case_when(V0606 == 1 ~ "Brancos",
                                                                                          V0606 == 2 ~ "Pretos",
                                                                                          V0606 == 3 ~ "Amarelos",
                                                                                          V0606 == 4 ~ "Pardos",
                                                                                          V0606 == 5 ~ "Indígenas"))
    #ajeitar o formato
    # grid_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/grid_muni_%s.RDS',
    #                                sigla_muni, sigla_muni))
    # aaaa <- data_micro %>% st_drop_geometry() %>%
    #   group_by(hex) %>%
    #   summarise(n = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
    #   st_as_sf()
    # 
    # mapview(aaaa)
    
    
    #checar setores com todos os renda_class_pc == n_col
    
    # lista_tract <- data_micro %>% group_by(code_tract, renda_class_pc) %>%
    #   summarise(n = n()) %>% ungroup() %>%
    #   group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
    #                                      n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
    #   filter(n_classes > n_classes_col) %>% pull(code_tract)
    
    
    
    # data_micro2 <- data_micro %>% filter(code_tract %in% lista_tract) %>% select(1:12, V0606, hex) %>%
    #   mutate(V0606 = as.factor(V0606))
    
    data_micro2 <- data_micro %>% select(1:12, V0606, hex) %>%
      mutate(V0606 = as.factor(V0606))
    
    
    
    # aaaa <- data_micro2 %>% st_drop_geometry() %>%
    #   group_by(hex) %>%
    #   summarise(n = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
    #   st_as_sf()
    # 
    # mapview(aaaa)
    
    
    # levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
    if (sigla_muni == "dou"){
      
      data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos"))
      
    } else {
      
      data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
      
    }
    
    data_micro_bus <- data_micro2 %>% filter(hex %in% id_hex_intersects_bus)
    # mapview(data_micro_bus)
    #recorte nos dados da microssimulacao
    
    
    
    if (sigla_muni == "rma"){
      
      recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus500), "OK","N"))  %>% 
        # mutate(total = "total") %>% 
        mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
               cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                               cor == "pretos" ~ "Pretos",
                               cor == "brancos" ~ "Brancos")) %>%
        mutate(
          quintil_renda = ntile(Rend_pc, 4)) %>%
        
        left_join(dados_hex_rma %>% select(id_hex, sigla_muni),
                  by = c('hex' = 'id_hex')) %>%
        # quintil_renda = case_when(Rend_pc < 7.1 ~ 1,
        #                           Rend_pc < 60 ~ 2,
        #                           Rend_pc < 398 ~ 3,
        #                           Rend_pc >399 ~ 4)) %>%
        group_by(cor, quintil_renda, genero, teste, sigla_muni) %>% summarise(n = n()) %>% 
        ungroup() %>% group_by(cor, quintil_renda, genero, sigla_muni) %>% 
        summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
        mutate(class = paste(genero, cor))%>%
        mutate(id = paste(class, quintil_renda))
      
      resumo.infra_tp500 <- weighted.mean(recorte_rr$prop, w = recorte_rr$n)
      
      resumo.infra_tp500_rma <- recorte_rr %>% group_by(sigla_muni) %>%
        summarise(media_muni = sum(prop * n)/sum(n))
      
    }
    
    
    
    recorte_rr500 <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus500), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
      mutate(class = paste(genero, cor))%>%
      mutate(id = paste(class, quintil_renda))
    
    resumo.infra_tp500 <- weighted.mean(recorte_rr500$prop, recorte_rr500$n)    
    
    
    
# Pessoas não atendidas 300 m----------------------------------------------------

    
    if (sigla_muni == "rma"){
      
      dados_hex_intersect_ntad300 <- dados_hex %>%
        st_as_sf() %>%
        st_transform(decisao_muni$epsg) %>%
        st_difference(dados_linhas_buffer_300)
      dados_hex_intersect_ntad300 <- dados_hex_intersect_ntad300 %>%
        mutate(area = st_area(.)) %>%
        mutate(area2 = as.numeric(area)) %>%
        filter(area2 > 60000.0)
      
    } else {
    
    dados_hex_intersect_ntad300 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_difference(dados_peds_buffer_300)
    dados_hex_intersect_ntad300 <- dados_hex_intersect_ntad300 %>%
      mutate(area = st_area(.)) %>%
      mutate(area2 = as.numeric(area)) %>%
      filter(area2 > 60000.0)
    
    }
    
    # mapview(dados_hex_intersect_ntad300)
    id_hex_intersects_bus_ntad300 <- dados_hex_intersect_ntad300$id_hex %>% unique()
    
    #ESPACIAL
    
    # recorte_rr_ntad <- data_micro2 %>% mutate(quintil_renda = ntile(Rend_pc, 4)) %>%
    #   filter(quintil_renda %in% c(1,2,3)) %>%
    #   filter(hex %in% id_hex_intersects_bus_ntad300)%>%
    #   st_drop_geometry() %>%
    #   group_by(hex) %>% summarise(n = n()) %>%
    #   left_join(dados_hex, by = c("hex" = "id_hex")) %>% st_as_sf()
    
    # mapview(recorte_rr_ntad)
    # pop_counts <- dados_simulacao %>% 
    #   group_by(hex) %>%
    #   summarise(pop_total = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
    #   st_as_sf() %>% mutate(quintil = as.factor(ntile(pop_total, 4)))
    
    
    # mapview(pop_counts)
    # dados_simulacao %>% filter(class)
    
    
    recorte_rr_ntad <- data_micro2 %>%
      mutate(quintil_renda = ntile(Rend_pc, 4)) %>%
      filter(quintil_renda %in% c(1,2,3,4)) %>%
      filter(hex %in% id_hex_intersects_bus_ntad300) %>% st_drop_geometry() %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      # mutate(
      #   quintil_renda = ntile(Rend_pc, 4)) %>%
      group_by(hex) %>% summarise(n = n()) %>%
      left_join(dados_hex, by = c("hex" = "id_hex")) %>% st_as_sf() %>%
      mutate(n = as.numeric(n))
    # mapview(recorte_rr_ntad)
    # mapview(dados_hex_intersect_ntad300)
    
    #NÃO ESPACIAL
    recorte_rr_ntad2 <- data_micro2 %>% group_by(hex) %>%
      filter(hex %in% id_hex_intersects_bus_ntad300) %>% st_drop_geometry() %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      group_by(cor, quintil_renda, genero) %>% summarise(n = n()) %>%
      mutate(class = paste(genero, cor))%>%
      mutate(id = paste(class, quintil_renda))
    
    # ceiling(max(recorte_rr_ntad$n))
    pop_max_ntad <- plyr::round_any(max(recorte_rr_ntad2$n), 10^(n_int_digits(max(recorte_rr_ntad2$n))), f = ceiling)
    break_max_ntad <- pop_max_ntad
    break_leap_ntad <- break_max_ntad/4
    escala_ntad <- ifelse(pop_max_ntad > 150000, 12, ifelse( pop_max_ntad > 100000, 12, ifelse(pop_max_ntad > 50000, 12, 12)))
    
    range_tot <- max(recorte_rr_ntad2$n)-min(recorte_rr_ntad2$n)
    extend_ntad <- 100
    
    if (range_tot <= 100){
      passo_ntad <- 50
    } else if (range_tot <= 500) {
      passo_ntad <- 100
    } else if (range_tot <= 1000){
      passo_ntad <- 250
    } else if (range_tot <= 5000){
      passo_ntad <- 500
    } else {
      passo_ntad <- 1000
    }
    
    range1_ntad <- plyr::round_any(ifelse( (min(recorte_rr_ntad2$n) - extend_ntad)<0, 0, min(recorte_rr_ntad2$n) - extend_ntad),
                                   passo_ntad,
                                   f = floor)
    
    range2_ntad <- plyr::round_any(ifelse( (max(recorte_rr_ntad2$n) + extend_ntad)>break_max_ntad, 1, max(recorte_rr_ntad2$n) + extend_ntad),
                                   passo_ntad,
                                   f = ceiling)
    
    plot_cleveland_bus300_ntad <- ggplot(recorte_rr_ntad2, aes(n, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("grey70", "#FFB578", "black", "#cc3003"),
      )+
      scale_size_continuous( range = c(0,escala_ntad),
                             limits = c(0,break_max_ntad),
                             breaks = c(break_leap_ntad,break_leap_ntad*2,break_leap_ntad*3),
                             name = "Habitantes",
                             guide = "legend")
    p_b300_bus_ntad <- plot_cleveland_bus300_ntad + scale_color_manual(name = "Gênero e Cor",
                                                                       values = c("Homens Brancos"="grey70",
                                                                                  "Homens Pretos"="#FFB578",
                                                                                  "Mulheres Brancos" = "black",
                                                                                  "Mulheres Pretos"="#cc3003"),
                                                                       labels = c("Homens Brancos"="Homens Brancos",
                                                                                  "Homens Pretos"= "Homens Negros",
                                                                                  "Mulheres Brancos"="Mulheres Brancas",
                                                                                  "Mulheres Pretos"="Mulheres Negras"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "População sem acesso à infraestrutura de transporte público")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      # theme_minimal() +
      xlab("Habitantes do recorte") +
      ylab("Quartil de renda per capita") +
      scale_x_continuous(labels = scales::number,
                         limits = c(range1_ntad,range2_ntad),
                         breaks = seq(range1_ntad,range2_ntad, passo_ntad)) +
      scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
      theme(#axis.title = element_blank(),
        panel.grid.minor = element_line(),
        legend.background = element_blank(),
        panel.background = element_blank(),
        legend.direction = "vertical",
        legend.position = "bottom",
        text = element_text(family = "sans",
                            # face = "bold",
                            size = 20),
        
        plot.title = element_text(size = 35, margin = margin(b=10), family = "encode_sans_bold"),
        plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 30, margin = margin(t=10), color = "grey70", hjust = 0),
        legend.title = element_text(size = 35, family = "encode_sans_bold"),
        legend.text = element_text(size = 30, family = "encode_sans_light"),
        axis.text = element_text(size = 30, family = "encode_sans_light"),
        axis.title = element_text(size = 35, family = "encode_sans_bold"))
    
    ggsave(p_b300_bus_ntad,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/12-linhasbus_300_cleveland_nao_atendidos%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 15, height = 10, units = "cm" )
    
    # %>% 
    #   ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
    #   
    #   summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
    #   mutate(class = paste(genero, cor))%>%
    #   mutate(id = paste(class, quintil_renda))
    
    # recorte_ntad <- data_micro %>% filter(hex %in% id_hex_intersects_bus_ntad300) %>%
    #   left_join(grid_micro, by = c("hex" = "h3_address")) %>% st_as_sf()
    # 
    # 
    # 
    # recorte_ntad <- data_micro2 %>% group_by(hex) %>%
    #   filter(hex %in% id_hex_intersects_bus_ntad300) %>% st_drop_geometry() %>% summarise(n = n()) %>%
    #   left_join(dados_hex, by = c("hex" = "id_hex")) %>% st_as_sf()
    # 
    #   
    #   mutate(teste = ifelse(is.element(hex,id_hex_intersects_bus_ntad300), "N","OK")) %>%
    #   filter(teste == "N") %>% left_join(grid_micro, by = c("hex" = "h3_address")) %>% st_as_sf()
    #   sum(recorte_ntad$n)
    # mapview(recorte_ntad, zcol = "n")
    # mapview(dados_hex_intersect_ntad300)
    # 
    # vamo <- st_join(dados_hex %>% st_transform(decisao_muni$epsg),
    #                 dados_peds_buffer_300 %>% st_transform(decisao_muni$epsg) %>% 
    #                   mutate(teste = 'OK')) %>% na.omit() %>% mapview()
    
    
    


# Aglomerados Subnormais nao atendidos ------------------------------------

    if (sigla_muni == "rma"){
      
      aglomerados_natend300 <- assentamentos2 %>% st_transform(decisao_muni$epsg) %>%
        st_difference(dados_linhas_buffer_300) %>% st_as_sf() %>%
        mutate(area_ntad = st_area(.))
      
      assentamentos_area <- assentamentos2 %>%
        mutate(area_total = st_area(.)) %>%
        select(NM_AGSN, area_total)
      
      aglomerados_natend300 <- aglomerados_natend300 %>%
        left_join(assentamentos_area %>% st_drop_geometry(), by = "NM_AGSN") %>%
        mutate(area_prop = area_ntad/area_total,
               pop_ntad = as.numeric(SUM_EDOC*area_prop))
      
    } else {
      
    aglomerados_natend300 <- assentamentos2 %>% st_transform(decisao_muni$epsg) %>%
      st_difference(dados_peds_buffer_300) %>% st_as_sf() %>%
      mutate(area_ntad = st_area(.))
    
    assentamentos_area <- assentamentos2 %>%
      mutate(area_total = st_area(.)) %>%
      select(NM_AGSN, area_total)
    
    aglomerados_natend300 <- aglomerados_natend300 %>%
      left_join(assentamentos_area %>% st_drop_geometry(), by = "NM_AGSN") %>%
      mutate(area_prop = area_ntad/area_total,
             pop_ntad = as.numeric(SUM_EDOC*area_prop))
    
    }
    # mapview(aglomerados_natend500, zcol = "pop_ntad")
    
    write_sf(aglomerados_natend300,
             sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s_assentamentos_ntad.gpkg',
                     sigla_muni, sigla_muni))
    write.xlsx(aglomerados_natend300 %>% st_drop_geometry(),
             sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s_assentamentos_ntad.xlsx',
                     sigla_muni, sigla_muni))
    
    # aglomerados_natend500 <- assentamentos %>% st_transform(decisao_muni$epsg) %>%
    #   st_join(dados_peds_buffer_500 %>% mutate(teste = 1)) %>% st_as_sf() %>%
    #   filter(is.na(teste)==T)
    
    

# Mapa dos Aglomerados Nao Atendidos --------------------------------------


    recorte_rr_ntad_map <- recorte_rr_ntad %>%
      st_drop_geometry() %>%
      left_join(pop_counts, by = "hex") %>% st_as_sf()
    
    map_aglomerados_ntad <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = st_transform(recorte_rr_ntad_map, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      

      
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              # fill = "#d8faf0",
              fill = NA,
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.8)  +
      
      new_scale_fill() +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              linetype = "solid",
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7) +
      

      # ggnewscale::new_scale_color() +
      
      
     
      
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")

      
      # geom_sf(data = st_transform(aglomerados_natend300, 3857),
      #         aes(color = '#0f805e'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         # fill = "#0f805e",
      #         fill = NA,
      #         alpha = 0.6,
      #         linewidth = 1.0) +
    
    geom_sf(data = aglomerados_natend300 %>% st_union(),
            # aes(fill = "#d96e0a"),
            aes(color = "aglomerados"),
            # "#0f805e"
            fill = cor_ag,
            linewidth = 0.8,
            # color = NA,
            show.legend = "polygon",
            alpha = 0.5)+

    
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c(#"urb" = "#d8faf0",
                           "urb" = "#8F040E",
                           "areas" = "grey60",
                           "aglomerados" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white",
                                                           # "#d8faf0",
                                                           "white",
                                                           cor_ag
                                                           )
                                                  # colour = c("grey25", "white", "white")
        ))
        # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
        #                     order = 1)
      ) +
      # 
      # scale_color_manual(name = "Aglomerados subnormais",
      #                    values = c("#0f805e" = "#0f805e"),
      #                    label = c("#0f805e" = "Palmas")
      # )+
      
      # scale_fill_viridis_b() +
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey40", linewidth = 0.5) +
      
      # geom_sf(data = st_transform(aguas,3857), fill = cor_aguas, colour = NA) +
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      # geom_sf(data = assentamentos,
      #         # aes(fill = "#d96e0a"),
      #         aes(fill = "#5766cc"),
      #         
      #         # fill = "#d96e0a",
      #         size = 1.3,
      #         color = NA,
      #         show.legend = "polygon",
      #         alpha = 0.9)+
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +

      
      
    # viridis::scale_fill_viridis(option = "B",
    #                             direction = -1,
    #                             name = "População (hab)",
    #                             breaks = seq(1,3,1),
    #                             labels = paste(seq(1,3,1), "quartil"),
    #                             limits = c(1,3)
    #                             
    #                             ) +
      # scale_fill_gradientn(
      #   name = "Quintil de população",
      #   colors = viridis_magma_discrete,
      #   breaks = seq(1,3,1),
      #   labels = seq(1,3,1),
      #   limits = c(1,3),
      #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #   # values = NULL,
      #   space = "Lab",
      #   na.value = NA,
      #   # guide = "colourbar",
      #   aesthetics = "fill",
      #   # colors
      # ) +
      
      # scale_fill_viridis_c(direction = -1,option = "inferno", name = "População não atendida") +
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
    ggnewscale::new_scale_color() +
      
      geom_sf(data = st_transform(terminais, 3857),
              aes(color = 'terminais'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              fill = "grey20",
              size  = 2.5,
              shape = 15,
              linewidth = 0.3) +
      
      scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                         values = c(
                                    "terminais" = "grey20"),
                         label = c(
                                   "terminais" = "Terminais de Integração")
      )+
      
    

      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
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
      legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.80, 0.32),
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
    ) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white"
        ),
        shape = c(
                  15
        ),
        # colour = c("#2B6CB0","black"),
        size = c(
                 2
        ),
        linetype = c(
                     0
        )
        ),
        order = 3)) +
      # coord_sf(ylim = c(-1664674,-1684450), xlim = c(-4563278,-4531391), expand = FALSE)
      aproxima_muni(sigla_muni = sigla_muni)
    
    ggsave(map_aglomerados_ntad,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/14-aglomerados_nao_atendidos_tp_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )
    

# Mapa de Ag subnormais sem zoom ------------------------------------------

    map_aglomerados_ntad_sz <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+

      new_scale_fill() +
      
      geom_sf(data = st_transform(recorte_rr_ntad_map, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+

      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"
                        ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      
      guides(
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250"),
                                                color = c("#FEF8ED", "#fee6c3", "#fac690", "#e9a250")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              fill = NA,
              aes(color = "urb"),
              linetype = "solid",
              linewidth = 0.4,
              alpha= 0.8)  +
      
      new_scale_fill() +
      
      # geom_sf(data = dados_areas %>% st_transform(3857),
      #         # aes(size = 2),
      #         aes(color = "areas"),
      #         # color = "grey45",
      #         # aes(fill = '#CFF0FF'),
      #         fill = NA,
      #         linetype = "solid",
      #         # stroke = 2,
      #         # size = 2,
      #         linewidth = 0.4,
    #         alpha= 0.7) +
    
    
    # ggnewscale::new_scale_color() +
    
    geom_sf(data = aglomerados_natend300 %>% st_union(),
            # aes(fill = "#d96e0a"),
            aes(color = "aglomerados"),
            # "#0f805e"
            fill = cor_ag,
            linewidth = 0.5,
            # color = NA,
            show.legend = "polygon",
            alpha = 0.5)+
      
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c(#"urb" = "#d8faf0",
                           "urb" = "#8F040E",
                           "areas" = "grey60",
                           "aglomerados" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white",
                                                           # "#d8faf0",
                                                           # "white",
                                                           cor_ag)
                                                  # colour = c("grey25", "white", "white")
        ))
      ) +
      # 
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey40", linewidth = 0.5) +

    ggnewscale::new_scale_color() +

    scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                       values = c(
                         "terminais" = "grey20"),
                       label = c(
                         "terminais" = "Terminais de Integração")
    )+

      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +

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
      legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.24, 0.28),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.box.just = "left"
    ) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white"
        ),
        shape = c(
          15
        ),
        # colour = c("#2B6CB0","black"),
        size = c(
          2
        ),
        linetype = c(
          0
        )
        ),
        order = 3)) +
      aproxima_muni_zoom(sigla_muni = sigla_muni)
    
    ggsave(map_aglomerados_ntad_sz,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/14-aglomerados_nao_atendidos_tp_%s__sem_zoom_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )    
    
    
    
    
    
    # map_aglomerados_ntad <- ggplot() +
    #   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   coord_equal() +
    #   scale_fill_identity()+
    #   # nova escala
    #   new_scale_fill() +
    #   
    #   # geom_sf(data = simplepolys %>% st_transform(3857),
    #   #         # aes(size = 2),
    #   #         # aes(color = "#21367d"),
    #   #         aes(color = 'black'),
    #   #         # fill = NA,
    #   #         # stroke = 2,
    #   #         # size = 2,
    #   #         linewidth = 0.3,
    #   #         alpha= 0.8)  +
    #   # # theme_map() +
    #   geom_sf(data = st_transform(aglomerados_natend500, 3857),
    #           aes(fill = pop_ntad),
    #           # fill = '#33b099',
    #           # fill = NA,
    #           color = "black",
    #           alpha = .5,
    #           linewidth = 0.5) +
    #   
    #   # geom_sf(data = assentamentos,
    #   #         aes(fill = "#d96e0a"),
    #   #         # fill = "#d96e0a",
    #   #         size = 1.3,
    #   #         color = NA,
    #   #         show.legend = "polygon",
    #   #         alpha = 0.8)+
    #   
    #   # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    #   
    #   geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
    #   
    #   labs(fill = '') +
    #   labs(color = "") +
    #   
    #   # scale_fill_manual(values = c('#33b099'='#33b099',
    #   #                              "#d96e0a" ="#d96e0a"
    #   #                              # '#CFF0FF' = "#CFF0FF"
    #   # ),
    #   # labels = c('#33b099'="Cobertura de 300m",
    #   #            "#d96e0a"="Aglomerados\nSubnormais"
    #   #            # '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)"
    #   # )
    #   # ) +
    #   # scale_color_identity(labels = c("black" = "Área urbanizada\n(Mapbiomas 2021)",
    #   #                                 blue = ""), guide = "legend") +
    # # scale_fill_gradientn(
    # #   name = "População (hab)",
    # #   colors = colors_blue,
    # #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    # #   # values = NULL,
    # #   space = "Lab",
    # #   na.value = NA,
    # #   # guide = "colourbar",
    # #   aesthetics = "fill",
    # #   # colors
    # # ) +
    #   # geom_sf_label(data = st_transform(aglomerados_natend500, 3857),
    #   #               aes(label = NM_AGSN)
    #   #               )+
    # 
    #   
    #   
    #   # tema_populacao()
    #   theme(
    #     strip.text.x = element_text(size=rel(1.2)),
    #     strip.background = element_blank(),
    #     panel.background = element_rect(fill = NA, colour = NA),
    #     legend.background = element_blank(),
    #     axis.text = element_blank(),
    #     axis.title = element_blank(),
    #     axis.ticks = element_blank(), 
    #     panel.grid = element_blank(),
    #     plot.margin=unit(c(2,0,0,0),"mm"),
    #     legend.key.width=unit(2,"line"),
    #     legend.key.height = unit(.5,"cm"),
    #     legend.text=element_text("Tipo", size=rel(1)),
    #     legend.title=element_text(size=rel(1),                                   ),
    #     plot.title = element_text(hjust = 0, vjust = 4),
    #     strip.text = element_text(size = 10),
    #     legend.position = c(0.18, 0.15),
    #     legend.spacing.y = unit(0.05, 'cm'),
    #     legend.margin = margin(-0.45,0,0,0, unit="cm")
    #     # legend.margin = margin(t = -80)
    #   ) +
    #   guides(fill = guide_legend(byrow = TRUE)) +
    #   aproxima_muni(sigla_muni = sigla_muni)
    # # width = 16; height = 16
    # # map_empregos
    # ggsave(map_aglomerados_ntad,
    #        device = "png",
    #        filename =  sprintf("../data/map_plots_transports/muni_%s/12-aglomerados_ntad_500m_tp_%s.png", sigla_muni, sigla_muni),
    #        dpi = 400,
    #        width = width, height = height, units = "cm" )
        
    
    
    
    
    
    # mapview(aglomerados_natend500)
    
    
    # dados_hex_intersect_500 <- dados_hex %>%
    #   st_as_sf() %>%
    #   st_transform(decisao_muni$epsg) %>%
    #   st_intersection(dados_peds_buffer_500)
    # dados_hex_intersect_500 <- dados_hex_intersect_500 %>%
    #   mutate(area = st_area(.))
    # 
    # dados_hex_intersect_n300 <- dados_hex %>%
    #   st_as_sf() %>%
    #   st_transform(decisao_muni$epsg) %>%
    #   st_intersection(area_natend300)
    # dados_hex_intersect_n300 <- dados_hex_intersect_n300 %>%
    #   mutate(area = st_area(.))
    # 
    # # mapview(dados_hex_intersect_n500)
    # 
    # dados_hex_intersect_n500 <- dados_hex %>%
    #   st_as_sf() %>%
    #   st_transform(decisao_muni$epsg) %>%
    #   st_intersection(area_natend500)
    # dados_hex_intersect_n300 <- dados_hex_intersect_n300 %>%
    #   mutate(area = st_area(.))
    # 
    # 
    # 
    # 
    # 
    
    

# Mapa de Frequencias -----------------------------------------------------

    if (munis_parameters$frequencias[which(munis_parameters$abrev_muni ==  sigla_muni)]==1){
frequencias <- read_rds(sprintf('../data/tp_frequencias/muni_%s/rotas_freq_%s.rds',
                                sigla_muni, sigla_muni))
    
frequencias2 <- frequencias %>% mutate(cond = case_when(headway_medio <= 15 ~ '< 15 minutos',
                                                     headway_medio <= 30 ~ '15-30 minutos',
                                                     headway_medio <= 60 ~ '30-60 minutos',
                                                     headway_medio > 60 ~ '>60 minutos'
    )) %>% arrange(headway_medio) %>%
  mutate(cond = factor(cond, levels = c('< 15 minutos',
                                        '15-30 minutos',
                                        '30-60 minutos',
                                        '>60 minutos')))
viagens <- read_rds(sprintf('../data/tp_frequencias/muni_%s/rotas_viagens_%s.rds',
                            sigla_muni, sigla_muni)) %>%
  # mutate(headway_medio = 60/n_viagens_h) %>%
  mutate(headway_medio = ifelse(headway_medio >30 , 31, headway_medio)) %>%
  mutate(cond = case_when(headway_medio <= 5 ~ '< 5 minutos',
                        headway_medio <= 10 ~ '5-10 minutos',
                        headway_medio <= 15 ~ '10-15 minutos',
                        headway_medio > 25 ~ '>25 minutos'))


labels_freq <- c(seq(0,25,5),">30")
breaks_freq <- seq(0,30,5)
limits_freq <- c(0,31)


    BrBG <- c("#003c30","#01665e" , "#35978f" , "#80cdc1", "#c7eae5", "#f5f5f5","#f6e8c3",
              "#dfc27d", "#bf812d","#8c510a", "#543005")
    
    
    # cor_ntad <- "#957A03" #coco de bosta
    cor_ntad <- "#daa520" #amarelo
    
    cor_ag_freq <- "#2B6CB0"
    
    map_frequencias2 <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      geom_sf(data = st_transform(recorte_rr_ntad, 3857),
              aes(#fill = "ntad",
                  color = "ntad"),
              # color = "ntad",
              fill = cor_ntad,
              alpha=1,
              size = 0)+

      ggnewscale::new_scale_fill() +
      geom_sf(data = st_transform(viagens, 3857),
              aes(fill = headway_medio),
              # fill = '#21367d',
              # fill = NA,
              color = NA,
              alpha = 1,
              size = 1) +
      
      viridis::scale_fill_viridis(option = "rocket", direction = -1, name = "Intervalo médio (min)",
                                  labels = labels_freq,
                                  breaks = breaks_freq,
                                  limits = limits_freq
                                  
      ) +
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.5,
              alpha= 0.7) +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.4,
              alpha= 0.7)  +
      
      
      
      # ggnewscale::new_scale_color() +
      
      
      
      
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")

      
      # geom_sf(data = st_transform(aglomerados_natend300, 3857),
      #         aes(color = 'assentamentos'),
      #         # color = '#0f805e',
      #         # color = NA,
      #         # fill = "#0f805e",
      #         fill = NA,
      #         alpha = 0.6,
      #         linewidth = 1.0) +
      
      geom_sf(data = st_transform(assentamentos, 3857),
              aes(color = 'assentamentos'),
              # color = '#0f805e',
              # color = NA,
              # fill = "#0f805e",
              # fill = NA,
              fill = cor_ag_freq,
              alpha = 0.5,
              linewidth = 0.5) +
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c('urb', "areas", "assentamentos", "ntad"),
                         values = c("urb" = "grey50",
                                    "areas" = "#fefedf",
                                    "assentamentos" = cor_ag_freq,
                                    "ntad" = cor_ntad),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "assentamentos" = "Aglomerados subnormais",
                                   "ntad" = "População não atendida")
      )+
      
      guides(color = guide_legend(override.aes = list(fill = c("white",
                                                               "white",
                                                               cor_ag_freq,
                                                               cor_ntad),
                                                      color = c("grey50",
                                                                "#fdfc99",
                                                                cor_ag_freq,
                                                                cor_ntad)),
                                  order = 1)) +

      ggnewscale::new_scale_color() +
      # scale_color_manual(name = "Aglomerados subnormais",
      #                    values = c("#0f805e" = "#0f805e"),
      #                    label = c("#0f805e" = "Palmas")
      # )+
      geom_sf(data = st_transform(terminais, 3857),
              aes(color = 'terminais'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              fill = "#ead136",
              size  = 2.5,
              shape = 15,
              linewidth = 0.3) +

      
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      scale_color_manual(name = "Infraestrutura de<br>Transporte Público",
                         values = c(
                                    "terminais" = "#ead136"),
                         label = c(
                                   "terminais" = "Terminais de Integração")
      )+
    # geom_sf(data = assentamentos,
    #         # aes(fill = "#d96e0a"),
    #         aes(fill = "#5766cc"),
    #         
    #         # fill = "#d96e0a",
    #         size = 1.3,
    #         color = NA,
    #         show.legend = "polygon",
    #         alpha = 0.9)+
    
    # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
    #         color = NA,alpha = .7, linewidth = 1) +
    
    
    

      
      
      # scale_fill_viridis_c(direction = -1,option = "inferno", name = "População não atendida") +
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
      
      
      # geom_sf(data = st_transform(aguas,3857), fill = cor_aguas, colour = NA) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "bl",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
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
      legend.title=ggtext::element_markdown(size=30, family = "encode_sans_bold", lineheight = 0.15),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.82, 0.36),
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
    ) +
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white"
        ),
        shape = c(
                  15
        ),
        # colour = c("#2B6CB0","black"),
        size = c(
                 2
        ),
        linetype = c(
                     0
        )
        ),
        order = 3)) +
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni)

    #511277 roxo
    #142577 azul
    #957A03 amarelo queimado
    ggsave(map_frequencias2,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/15-frequencias_buffer_zoom%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    
    
    

# Cleveland de Frequencia -------------------------------------------------

    # dados_hex_intersect_freq <- dados_hex %>%
    #   st_as_sf() %>%
    #   st_transform(decisao_muni$epsg) %>%
    #   st_intersection(frequencias2)
    # dados_hex_intersect_freq <- dados_hex_intersect_freq %>%
    #   mutate(area = st_area(.))
    # 
    # # mapview(dados_hex_intersect_freq)
    # dados_hex_intersect_freq <- dados_hex_intersect_freq$id_hex %>% unique()
    showtext_auto()
    
    if (sigla_muni == "dou"){
      
      recorte_rr_h <- data_micro2 %>%
        # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
        left_join(viagens %>% st_drop_geometry(), by = c("hex"="id_hex")) %>%
        drop_na(headway_medio) %>%
        mutate(
          quintil_renda = ntile(Rend_pc, 4))%>%
        # mutate(total = "total") %>% 
        mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
               cor = case_when(V0606 == "Pardos" ~ "Pretos",
                               V0606 == "Pretos" ~ "Pretos",
                               V0606 == "Brancos" ~ "Brancos",
                               V0606 == "Indígenas" ~ "Indígenas")) %>%
        group_by(cor, quintil_renda, genero) %>%
        summarise(sd = sd(headway_medio),
                  headway_medio = mean(headway_medio, na.rm =T),
                  n = n()) %>%
        mutate(id = paste(genero, cor))
      
      
    } else {
    
    recorte_rr_h <- data_micro2 %>%
      # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
      left_join(viagens %>% st_drop_geometry(), by = c("hex"="id_hex")) %>%
      drop_na(headway_medio) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4))%>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      group_by(cor, quintil_renda, genero) %>%
      summarise(sd = sd(headway_medio),
                headway_medio = mean(headway_medio, na.rm =T),
                n = n()) %>%
      mutate(id = paste(genero, cor))
    
    }
    
    # sd(teste$headway_medio)
    # recorte_rr <- data_micro2 %>%
    #   # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
    #   left_join(frequencias2 %>% st_drop_geometry(), by = c("hex"="id_hex")) %>%
    #   # mutate(total = "total") %>% 
    #   mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
    #          cor = case_when(cor == "pard_am_ing" ~ "Pretos",
    #                          cor == "pretos" ~ "Pretos",
    #                          cor == "brancos" ~ "Brancos")) %>%
    #   drop_na(headway_medio) %>%
    #   mutate(
    #     quintil_renda = ntile(Rend_pc, 4)) %>%
    #   group_by(cor, quintil_renda, genero) %>%
    #   summarise(n = n(),
    #             headway_medio = mean(headway_medio, na.rm =T),
    #             headway_sd = sd(headway_medio)) %>% 
    #   # ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
    #   # summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"],
    #   #           headway_medio = mean(headway_medio, na.rm = T)) %>%
    #   # mutate(class = paste(genero, cor))%>%
    #   mutate(id = paste(class, quintil_renda))
    
    pop_max_headway <-plyr::round_any(max(recorte_rr_h$n),10^(n_int_digits(max(recorte_rr_h$n))), f = ceiling)
    break_max_h <- pop_max_headway
    break_leap_h <- break_max_h/4
    escala_h <- ifelse(pop_max_headway > 150000, 12, ifelse( pop_max_headway > 100000, 12, ifelse(pop_max_headway > 50000, 12, 12)))
    
    range_tot <- max(recorte_rr_h$headway_medio)-min(recorte_rr_h$headway_medio)
    extend_h <- 0.01
    
    if (range_tot <= 0.5){
      passo_h <- 0.1
    } else if (range_tot <= 1) {
      passo_h <- 0.25
    } else if (range_tot <= 2.5){
      passo_h <- 0.5
    } else if (range_tot <= 5){
      passo_h <- 1
    } else if (range_tot <= 10) {
      passo_h <- 2.5
    } else {
      passo_h <- 5
    }
    
    # range1_h <- floor(ifelse( (min(recorte_rr_h$headway_medio) - extend_h)<0, 0, min(recorte_rr_h$headway_medio) - extend_h))
    
    range1_h <- plyr::round_any(ifelse( (min(recorte_rr_h$headway_medio) - extend_h)<0, 0, min(recorte_rr_h$headway_medio) - extend_h),
                                passo_h,
                                f = floor)
    
    # range2_h <- ceiling(ifelse( (max(recorte_rr_h$headway_medio) + extend_h)>break_max_h, 1, max(recorte_rr_h$headway_medio) + extend_h))
    
    range2_h <- plyr::round_any(ifelse( (max(recorte_rr_h$headway_medio) + extend_h)>break_max_h, 1, max(recorte_rr_h$headway_medio) + extend_h),
                                passo_h,
                                f = ceiling)
    
    
    if (sigla_muni == "dou"){
      
      plot_cleveland_headway <- ggplot(recorte_rr_h, aes(headway_medio, as.character(quintil_renda))) +
        geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
        geom_point(aes(color = id, size= n), shape = 1, stroke = 1.5) +
        guides(fill=guide_legend(title="Gênero e cor")) +
        scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003")
        )+
        scale_size_continuous( range = c(0,escala_h),
                               limits = c(0,break_max_h),
                               breaks = c(break_leap_h,break_leap_h*2,break_leap_h*3),
                               name = "Habitantes",
                               guide = "legend")
      p_head_bus <- plot_cleveland_headway + scale_color_manual(name = "Gênero e Cor",
                                                                values = c("Homens Brancos"="#ADBEF0",
                                                                           "Homens Pretos"="#174DE8",
                                                                           "Homens Indígenas"="grey70",
                                                                           "Mulheres Brancos" = "#EBB814",
                                                                           "Mulheres Pretos"="#B39229",
                                                                           "Mulheres Indígenas"="#cc3003"),
                                                                labels = c("Homens Brancos"="Homens Brancos",
                                                                           "Homens Pretos"="Homens Negros",
                                                                           "Mulheres Brancos"="Mulheres Brancas",
                                                                           "Mulheres Pretos"="Mulheres Negras",
                                                                           "Homens Indígenas"="Homens Indígenas",
                                                                           "Mulheres Indígenas"="Mulheres Indígenas"))+
        # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
        #                    limits = c(0,5), # baixa complexidade car                   
        #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
        scale_y_discrete(expand = c(0.4,0.4)) +
        labs(title = "Intervalo médio por recortes")+  
        #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
        # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
        # theme_minimal() +
        xlab("Intervalo médio (min)") +
        ylab("Quartil de renda per capita") +
        scale_x_continuous(labels = scales::number,
                           limits = c(range1_h,range2_h),
                           breaks = seq(range1_h,range2_h, passo_h)) +
        scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
        theme(#axis.title = element_blank(),
          panel.grid.minor = element_line(),
          legend.background = element_blank(),
          panel.background = element_blank(),
          legend.direction = "vertical",
          legend.position = "bottom",
          text = element_text(family = "sans",
                              # face = "bold",
                              size = 20),
          
          plot.title = element_text(size = 35, margin = margin(b=10), family = "encode_sans_bold"),
          plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
          plot.caption = element_text(size = 30, margin = margin(t=10), color = "grey70", hjust = 0),
          legend.title = element_text(size = 35, family = "encode_sans_bold"),
          legend.text = element_text(size = 30, family = "encode_sans_light"),
          axis.text = element_text(size = 30, family = "encode_sans_light"),
          axis.title = element_text(size = 35, family = "encode_sans_bold"))
      
      
    } else {
    
    plot_cleveland_headway <- ggplot(recorte_rr_h, aes(headway_medio, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = id, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003")
      )+
      scale_size_continuous( range = c(0,escala_h),
                             limits = c(0,break_max_h),
                             breaks = c(break_leap_h,break_leap_h*2,break_leap_h*3),
                             name = "Habitantes",
                             guide = "legend")
    p_head_bus <- plot_cleveland_headway + scale_color_manual(name = "Gênero e Cor",
                                                                values = c("Homens Brancos"="grey70",
                                                                           "Homens Pretos"="#FFB578",
                                                                           "Mulheres Brancos" = "black",
                                                                           "Mulheres Pretos"="#cc3003"),
                                                                labels = c("Homens Brancos"="Homens Brancos",
                                                                           "Homens Pretos"="Homens Negros",
                                                                           "Mulheres Brancos"="Mulheres Brancas",
                                                                           "Mulheres Pretos"="Mulheres Negras"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "Intervalo médio por recortes")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      # theme_minimal() +
      xlab("Intervalo médio (min)") +
      ylab("Quartil de renda per capita") +
      scale_x_continuous(labels = scales::number,
                         limits = c(range1_h,range2_h),
                         breaks = seq(range1_h,range2_h, passo_h)) +
      scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
      theme(#axis.title = element_blank(),
        panel.grid.minor = element_line(),
        legend.background = element_blank(),
        panel.background = element_blank(),
        legend.direction = "vertical",
        legend.position = "bottom",
        text = element_text(family = "sans",
                            # face = "bold",
                            size = 20),
        
        plot.title = element_text(size = 35, margin = margin(b=10), family = "encode_sans_bold"),
        plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 30, margin = margin(t=10), color = "grey70", hjust = 0),
        legend.title = element_text(size = 35, family = "encode_sans_bold"),
        legend.text = element_text(size = 30, family = "encode_sans_light"),
        axis.text = element_text(size = 30, family = "encode_sans_light"),
        axis.title = element_text(size = 35, family = "encode_sans_bold"))
    
    }
    
    #escrita do gráfico
    if (sigla_muni == "dou"){
      
      ggsave(p_head_bus,
             device = "png",
             filename =  sprintf("../data/map_plots_transports/muni_%s/16-linhasbus_freq_cleveland_%s_new2.png", sigla_muni, sigla_muni),
             dpi = 300,
             width = 15, height = 11, units = "cm" )
      
    } else {
      
      
    ggsave(p_head_bus,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/16-linhasbus_freq_cleveland_%s_new_medio.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 15, height = 10, units = "cm" )
    
    }
}
# Tarifa - Espacial -------------------------------------------------------

    if (munis_parameters$ac_finan[which(munis_parameters$abrev_muni==sigla_muni)]==1){
    
    
   tarifa <- as.numeric(decisao_muni$tarifa) * 60
    sm <- 1302
    
    renda <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(renda = mean(Rend_pc*sm, na.rm =T)) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% drop_na(h3_resolution) %>%
      # mutate(renda = ifelse(renda>10, 10, renda))
      mutate(tarifa_renda = ifelse(renda == 0, 1, tarifa/renda)) %>%
      mutate(tarifa_renda2 = case_when(tarifa_renda <= 0.05 ~ "Até 5%",
                                       tarifa_renda <= 0.10 & tarifa_renda > 0.05 ~ "De 5% a 10%",
                                       tarifa_renda <= 0.15 & tarifa_renda > 0.10 ~ "De 10% a 15%",
                                       tarifa_renda <= 0.20 & tarifa_renda > 0.15 ~ "De 15% a 20%",
                                       tarifa_renda <= 0.25 & tarifa_renda > 0.20 ~ "De 20% a 25%",
                                       tarifa_renda <= 0.30 & tarifa_renda > 0.25 ~ "Entre 25% e 30%",
                                       tarifa_renda > 0.30  ~ "Acima de 30%"))
    
    renda2 <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(renda = mean(Rend_pc*sm, na.rm =T)) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% drop_na(h3_resolution) %>%
      # mutate(renda = ifelse(renda>10, 10, renda))
      mutate(tarifa_renda = ifelse(renda == 0, 1, tarifa/renda)) %>%
      mutate(tarifa_renda2 = case_when(tarifa_renda <= 0.05 ~ "Até 5%",
                                       tarifa_renda <= 0.15 & tarifa_renda > 0.05 ~ "De 5% a 15%",
                                       tarifa_renda <= 0.25 & tarifa_renda > 0.15 ~ "De 15% a 25%",
                                       tarifa_renda > 0.25  ~ "Acima de 25%"))
    

    # mapview(renda, zcol = "tarifa_renda")
    # verde 0E7F5D
    # azul #2B6CB0
    # cor_ag <- "#2B6CB0"
    # map_tarifa <- ggplot() +
    #   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   coord_equal() +
    #   scale_fill_identity()+
    #   # nova escala
    #   new_scale_fill() +
    # 
    #   geom_sf(data = st_transform(renda, 3857),
    #           aes(fill = tarifa_renda2),
    #           colour = NA,
    #           alpha=.8,
    #           size = 0)+
    # 
    #   geom_sf(data = dados_areas %>% st_transform(3857),
    #           aes(color = "areas"),
    #           fill = NA,
    #           linewidth = 0.7,
    #           alpha= 0.7) +
    # 
    # geom_sf(data = simplepolys %>% st_transform(3857),
    #         aes(color = "urb"),
    #         fill = NA,
    #         linewidth = 0.5,
    #         alpha= 0.7)  +
    #   
    #   geom_sf(data = assentamentos,
    #           aes(color = "ag"),
    #           linewidth = 0.3,
    #           fill = cor_ag,
    #           # fill = NA,
    #           show.legend = "polygon",
    #           alpha = 0.5)+
    #   
    #   scale_color_manual(name = "Uso do solo",
    #                      breaks = c("ag", "urb", "areas"),
    #                      values = c("urb" = "black",
    #                                 "areas" = "grey45",
    #                                 "ag" = cor_ag),
    #                      label = c("urb" = "Área urbanizada",
    #                                "areas" =  munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
    #                                "ag" = "Aglomerados subnormais")
    #   )+
    #   # "urb" = "#8F040E"
    #   
    #   # viridis::scale_fill_viridis(option = "A",
    #   #                             name = "Renda per capita (SM)",
    #   #                             breaks = seq(0,10,2),
    #   #                             labels = c("0","2", "4", "6","8", ">10"),
    #   #                             limits = c(0,10)) +
    #   # scale_fill_viridis_d(option = "plasma",
    #   #                      name = "% da renda",
    #   #                      direction = 1,
    #   #                      breaks = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #   #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"),
    #   #                      labels = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #   #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"))+
    # # colors_blue <- c("#ecfaff", "#ade7ff", "#73d6ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#0989c0", "#066891")
    # # cores escala paired
    # # c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#602F94", "#FFFF99", "#B15928")
    # # cores PuOr
    # # c("#B45709", "#E2810C", "#FEB863", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    # #cores BrBG com PuOr
    # # c("#02665E", "#369692", "#80CEC1", "#DEEBF7", "#B3ABD4", "#8171AF", "#54288A")
    # #cores RdYlBu com PuOr
    # # c("#F76D3F", "#FDAE61", "#FFE090", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    # #cores PuOu com mudan;a nos laranjas
    # # c("#DA8535", "#EDAF71", "#F5CDAA", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    # 
    # #escala azul e roxo da ivys
    # # c("#f0e1ee", "#e3c9df", "#873286", "#67127c", "#3c1a7d", "#1e1975", "#001459")
    # 
    # #escala ylOrRd 
    # # c("#FFEB8F", "#FDD263", "#FA792F", "#F83521", "#D90017", "#AD001D", "#521A08")
    # #escala carol oranges
    # # c("#FFDE59", "#FFBD59", "#FF914B", "#FF5C00", "#C10000", "#B0006A", "#6200B0")
    # #spectral
    # # c("#8A0033", "#EE5634", "#FB9E4F", "#FDDA79", "#E1F686", "#2974AF", "#4B3991")
    # 
    # #escala carola pastel
    # # c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080")
    # 
    # scale_fill_manual(name = "% da renda",
    #                   breaks = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                              "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"),
    #                   values = c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080"),
    #                   labels = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                              "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%")
    #                   ) +
    #   
    # # scale_fill_gradientn(
    # #     name = "% da renda",
    # #     colors = colors_purple ,
    # #     # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
    # #     # values = NULL,
    # #     space = "Lab",
    # #     na.value = NA,
    # #     # guide = "colourbar",
    # #     aesthetics = "fill",
    # #     # colors
    # #   ) +
    # 
    # geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
    #   
    #   ggspatial::annotation_scale(style = "ticks",
    #                               location = "br",
    #                               text_family = "encode_sans_bold",
    #                               text_cex = 3,
    #                               line_width = 1,
    #                               width_hint = 0.10,
    #                               pad_x = unit(0.35, "cm"),
    #                               pad_y = unit(0.35, "cm")
    #   ) +
    #   ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tr") +
    # 
    # theme(
    #   strip.text.x = element_text(size=rel(1.2)),
    #   strip.background = element_blank(),
    #   panel.background = element_rect(fill = NA, colour = NA),
    #   axis.text = element_blank(),
    #   axis.title = element_blank(),
    #   axis.ticks = element_blank(), 
    #   panel.grid = element_blank(),
    #   plot.margin=unit(c(0,0,0,0),"mm"),
    #   legend.margin = margin(unit(c(10,10,5,10),"mm")),
    #   legend.key.width=unit(2,"line"),
    #   legend.key.height = unit(1,"line"),
    #   legend.key = element_blank(),
    #   legend.text=element_text(size=25, family = "encode_sans_light"),
    #   legend.title=element_text(size=30, family = "encode_sans_bold"),
    #   plot.title = element_text(hjust = 0, vjust = 4),
    #   strip.text = element_text(size = 10),
    #   legend.position = c(0.20, 0.33),
    #   legend.box.background = element_rect(fill=alpha('white', 0.7),
    #                                        colour = "#A09C9C",
    #                                        linewidth = 0.8,
    #                                        linetype = "solid"),
    #   legend.background = element_blank(),
    #   # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
    #   #                                      colour = "#E0DFE3"),
    #   legend.spacing.y = unit(0.4, 'cm'),
    #   legend.box.just = "left"
    #   # legend.margin = margin(t = -80)
    # ) +
    #   # guides(fill = guide_legend(byrow = TRUE)) +
    #   aproxima_muni(sigla_muni = sigla_muni)  +
    #   guides(color = guide_legend(override.aes = list(fill = c(cor_ag, "white", "white"),
    #                                                   alpha = c(0.5, rep(0.1,2))),
    #                               order = 1),
    #          fill = guide_legend(override.aes = list(fill = c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080"),
    #                                                  color = c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080")),
    #                              order = 2)
    #          )
    # 
    # 
    # 
    # 
    # suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    # 
    # ggsave(map_tarifa,
    #        device = "png",
    #        filename =  sprintf('../data/map_plots_transports/muni_%s/17-tarifa_renda_new8carol_pastel_%s.png',
    #                            sigla_muni,
    #                            sigla_muni),
    #        dpi = 300,
    #        width = width, height = height, units = "cm" )
    

# Mapa tarifa versao com menos classes ------------------------------------

    # cor_ag <- "#2B6CB0" #azul
    cor_ag <- "#96D6C2" #verde
    map_tarifa <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = st_transform(renda2, 3857),
              aes(fill = tarifa_renda2),
              colour = NA,
              alpha=.8,
              size = 0)+
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              aes(color = "areas"),
              fill = NA,
              linewidth = 0.5,
              alpha= 0.7) +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              aes(color = "urb"),
              fill = NA,
              linewidth = 0.8,
              alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              aes(color = "ag"),
              linewidth = 0.8,
              fill = cor_ag,
              # fill = NA,
              show.legend = "polygon",
              alpha = 0.5)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("ag", "urb", "areas"),
                         values = c("urb" = "grey25",
                                    "areas" = "grey45",
                                    "ag" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" =  munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      # "urb" = "#8F040E"
      
      # viridis::scale_fill_viridis(option = "A",
      #                             name = "Renda per capita (SM)",
      #                             breaks = seq(0,10,2),
      #                             labels = c("0","2", "4", "6","8", ">10"),
      #                             limits = c(0,10)) +
      # scale_fill_viridis_d(option = "plasma",
      #                      name = "% da renda",
      #                      direction = 1,
      #                      breaks = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"),
    #                      labels = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"))+
    # colors_blue <- c("#ecfaff", "#ade7ff", "#73d6ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#0989c0", "#066891")
    # cores escala paired
    # c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#602F94", "#FFFF99", "#B15928")
    # cores PuOr
    # c("#B45709", "#E2810C", "#FEB863", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    #cores BrBG com PuOr
    # c("#02665E", "#369692", "#80CEC1", "#DEEBF7", "#B3ABD4", "#8171AF", "#54288A")
    #cores RdYlBu com PuOr
    # c("#F76D3F", "#FDAE61", "#FFE090", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    #cores PuOu com mudan;a nos laranjas
    # c("#DA8535", "#EDAF71", "#F5CDAA", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    
    #escala azul e roxo da ivys
    # c("#f0e1ee", "#e3c9df", "#873286", "#67127c", "#3c1a7d", "#1e1975", "#001459")
    
    #escala ylOrRd 
    # c("#FFEB8F", "#FDD263", "#FA792F", "#F83521", "#D90017", "#AD001D", "#521A08")
    #escala carol oranges
    # c("#FFDE59", "#FFBD59", "#FF914B", "#FF5C00", "#C10000", "#B0006A", "#6200B0")
    #spectral
    # c("#8A0033", "#EE5634", "#FB9E4F", "#FDDA79", "#E1F686", "#2974AF", "#4B3991")
    
    #escala carola pastel
    # c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080")
    
    scale_fill_manual(name = "% da renda",
                      breaks = c("Até 5%", "De 5% a 15%", "De 15% a 25%", "Acima de 25%"),
                      values = c("#FFE8A4", "#E36C27", "#960C5F", "#470080"),
                      labels = c("Até 5%", "De 5% a 15%", "De 15% a 25%", "Acima de 25%")
    ) +
      
      # scale_fill_gradientn(
      #     name = "% da renda",
      #     colors = colors_purple ,
      #     # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #     # values = NULL,
      #     space = "Lab",
      #     na.value = NA,
      #     # guide = "colourbar",
      #     aesthetics = "fill",
      #     # colors
    #   ) +
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
      
      # geom_sf(data = st_transform(aguas,3857), fill = cor_aguas, colour = NA) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
      
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
        legend.position = c(0.19, 0.27),
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
      # coord_sf(ylim = c(-1664674,-1684450), xlim = c(-4563278,-4531391), expand = FALSE) +
      aproxima_muni(sigla_muni = sigla_muni)  +
      guides(color = guide_legend(override.aes = list(fill = c(cor_ag,
                                                               "white",
                                                               "white"),
                                                      alpha = c(0.5,
                                                                rep(0.1,2))),
                                  order = 1),
             fill = guide_legend(override.aes = list(fill = c("#FFE8A4", "#E36C27", "#960C5F", "#470080"),
                                                     color = c("#FFE8A4", "#E36C27", "#960C5F", "#470080")),
                                 order = 2)
      )
    
    
    
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    
    ggsave(map_tarifa,
           device = "png",
           filename =  sprintf('../data/map_plots_transports/muni_%s/17-tarifa_renda_new_insta_%s.png',
                               sigla_muni,
                               sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )    


# Mapa de Tarifa em SVG ---------------------------------------------------

    # cor_ag <- "#2B6CB0" #azul
    cor_ag <- "#96D6C2" #verde
    map_tarifa_svg <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = st_transform(renda2, 3857),
              aes(fill = tarifa_renda2),
              colour = NA,
              alpha=.8,
              size = 0)+
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              aes(color = "areas"),
              fill = NA,
              linewidth = 0.5,
              alpha= 0.7) +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              aes(color = "urb"),
              fill = NA,
              linewidth = 0.8,
              alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              aes(color = "ag"),
              linewidth = 0.8,
              fill = cor_ag,
              # fill = NA,
              show.legend = "polygon",
              alpha = 0.5)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("ag", "urb", "areas"),
                         values = c("urb" = "grey25",
                                    "areas" = "grey45",
                                    "ag" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" =  munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      # "urb" = "#8F040E"
      
      # viridis::scale_fill_viridis(option = "A",
      #                             name = "Renda per capita (SM)",
      #                             breaks = seq(0,10,2),
      #                             labels = c("0","2", "4", "6","8", ">10"),
      #                             limits = c(0,10)) +
      # scale_fill_viridis_d(option = "plasma",
      #                      name = "% da renda",
      #                      direction = 1,
      #                      breaks = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"),
    #                      labels = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"))+
    # colors_blue <- c("#ecfaff", "#ade7ff", "#73d6ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#0989c0", "#066891")
    # cores escala paired
    # c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#602F94", "#FFFF99", "#B15928")
    # cores PuOr
    # c("#B45709", "#E2810C", "#FEB863", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    #cores BrBG com PuOr
    # c("#02665E", "#369692", "#80CEC1", "#DEEBF7", "#B3ABD4", "#8171AF", "#54288A")
    #cores RdYlBu com PuOr
    # c("#F76D3F", "#FDAE61", "#FFE090", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    #cores PuOu com mudan;a nos laranjas
    # c("#DA8535", "#EDAF71", "#F5CDAA", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    
    #escala azul e roxo da ivys
    # c("#f0e1ee", "#e3c9df", "#873286", "#67127c", "#3c1a7d", "#1e1975", "#001459")
    
    #escala ylOrRd 
    # c("#FFEB8F", "#FDD263", "#FA792F", "#F83521", "#D90017", "#AD001D", "#521A08")
    #escala carol oranges
    # c("#FFDE59", "#FFBD59", "#FF914B", "#FF5C00", "#C10000", "#B0006A", "#6200B0")
    #spectral
    # c("#8A0033", "#EE5634", "#FB9E4F", "#FDDA79", "#E1F686", "#2974AF", "#4B3991")
    
    #escala carola pastel
    # c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080")
    
    scale_fill_manual(name = "% da renda",
                      breaks = c("Até 5%", "De 5% a 15%", "De 15% a 25%", "Acima de 25%"),
                      values = c("#FFE8A4", "#E36C27", "#960C5F", "#470080"),
                      labels = c("Até 5%", "De 5% a 15%", "De 15% a 25%", "Acima de 25%")
    ) +
      
      # scale_fill_gradientn(
      #     name = "% da renda",
      #     colors = colors_purple ,
      #     # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #     # values = NULL,
      #     space = "Lab",
      #     na.value = NA,
      #     # guide = "colourbar",
      #     aesthetics = "fill",
      #     # colors
    #   ) +
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
      
      # geom_sf(data = st_transform(aguas,3857), fill = cor_aguas, colour = NA) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 1,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
      
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
        legend.text=element_text(size=8, family = "encode_sans_light"),
        legend.title=element_text(size=10, family = "encode_sans_bold"),
        plot.title = element_text(hjust = 0, vjust = 4),
        strip.text = element_text(size = 10),
        legend.position = c(0.17, 0.27),
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
      # coord_sf(ylim = c(-1664674,-1684450), xlim = c(-4563278,-4531391), expand = FALSE) +
      aproxima_muni(sigla_muni = sigla_muni)  +
      guides(color = guide_legend(override.aes = list(fill = c(cor_ag,
                                                               "white",
                                                               "white"),
                                                      alpha = c(0.5,
                                                                rep(0.1,2))),
                                  order = 1),
             fill = guide_legend(override.aes = list(fill = c("#FFE8A4", "#E36C27", "#960C5F", "#470080"),
                                                     color = c("#FFE8A4", "#E36C27", "#960C5F", "#470080")),
                                 order = 2)
      )
    
    
    
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    
    ggsave(map_tarifa_svg,
           device = "svg",
           filename =  sprintf('../data/map_plots_transports/muni_%s/17-tarifa_renda_new_insta_%s.svg',
                               sigla_muni,
                               sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )    
    
    
    
# Mapa de tarifa sem zoom -------------------------------------------------

    # cor_ag <- "#2B6CB0" #azul
    cor_ag <- "#96D6C2" #verde
    
    map_tarifa_sz <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = st_transform(renda2, 3857),
              aes(fill = tarifa_renda2),
              colour = NA,
              alpha=.8,
              size = 0)+
      
      # geom_sf(data = dados_areas %>% st_transform(3857),
      #         aes(color = "areas"),
      #         fill = NA,
      #         linewidth = 0.5,
      #         alpha= 0.7) +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              aes(color = "urb"),
              fill = NA,
              linewidth = 0.6,
              alpha= 0.7)  +
      
      geom_sf(data = assentamentos,
              aes(color = "ag"),
              linewidth = 0.8,
              fill = cor_ag,
              # fill = NA,
              show.legend = "polygon",
              alpha = 0.5)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("ag", "urb", "areas"),
                         values = c("urb" = "#FEFEDF",
                                    "areas" = "grey45",
                                    "ag" = cor_ag),
                         label = c("urb" = "Área urbanizada",
                                   "areas" =  munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      # "urb" = "#8F040E"
      
      # viridis::scale_fill_viridis(option = "A",
      #                             name = "Renda per capita (SM)",
      #                             breaks = seq(0,10,2),
      #                             labels = c("0","2", "4", "6","8", ">10"),
      #                             limits = c(0,10)) +
      # scale_fill_viridis_d(option = "plasma",
      #                      name = "% da renda",
      #                      direction = 1,
      #                      breaks = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"),
    #                      labels = c("Até 5%", "De 5% a 10%", "De 10% a 15%", "De 15% a 20%",
    #                                 "De 20% a 25%", "Entre 25% e 30%", "Acima de 30%"))+
    # colors_blue <- c("#ecfaff", "#ade7ff", "#73d6ff", "#38c4ff", "#0cb7ff", "#0ba0e0", "#0989c0", "#066891")
    # cores escala paired
    # c("#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#602F94", "#FFFF99", "#B15928")
    # cores PuOr
    # c("#B45709", "#E2810C", "#FEB863", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    #cores BrBG com PuOr
    # c("#02665E", "#369692", "#80CEC1", "#DEEBF7", "#B3ABD4", "#8171AF", "#54288A")
    #cores RdYlBu com PuOr
    # c("#F76D3F", "#FDAE61", "#FFE090", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    #cores PuOu com mudan;a nos laranjas
    # c("#DA8535", "#EDAF71", "#F5CDAA", "#FFFFBF", "#B3ABD4", "#8171AF", "#54288A")
    
    #escala azul e roxo da ivys
    # c("#f0e1ee", "#e3c9df", "#873286", "#67127c", "#3c1a7d", "#1e1975", "#001459")
    
    #escala ylOrRd 
    # c("#FFEB8F", "#FDD263", "#FA792F", "#F83521", "#D90017", "#AD001D", "#521A08")
    #escala carol oranges
    # c("#FFDE59", "#FFBD59", "#FF914B", "#FF5C00", "#C10000", "#B0006A", "#6200B0")
    #spectral
    # c("#8A0033", "#EE5634", "#FB9E4F", "#FDDA79", "#E1F686", "#2974AF", "#4B3991")
    
    #escala carola pastel
    # c("#FFE8A4", "#FFBD59", "#F1A26F", "#E36C27", "#DB2C2E", "#960C5F", "#470080")
    
    scale_fill_manual(name = "% da renda",
                      breaks = c("Até 5%", "De 5% a 15%", "De 15% a 25%", "Acima de 25%"),
                      values = c("#FFE8A4", "#E36C27", "#960C5F", "#470080"),
                      labels = c("Até 5%", "De 5% a 15%", "De 15% a 25%", "Acima de 25%")
    ) +
      
      # scale_fill_gradientn(
      #     name = "% da renda",
      #     colors = colors_purple ,
      #     # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      #     # values = NULL,
      #     space = "Lab",
      #     na.value = NA,
      #     # guide = "colourbar",
      #     aesthetics = "fill",
      #     # colors
    #   ) +
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey30", linewidth = 0.8) +
      
      ggspatial::annotation_scale(style = "ticks",
                                  location = "br",
                                  text_family = "encode_sans_bold",
                                  text_cex = 3,
                                  line_width = 1,
                                  width_hint = 0.10,
                                  pad_x = unit(0.35, "cm"),
                                  pad_y = unit(0.35, "cm")
      ) +
      ggspatial::annotation_north_arrow(style = north_arrow_minimal(text_size = 0), location = "tl") +
      
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
        legend.position = c(0.20, 0.28),
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
      aproxima_muni(sigla_muni = sigla_muni)  +
      guides(color = guide_legend(override.aes = list(fill = c(cor_ag,
                                                               # "white",
                                                               "white"),
                                                      alpha = c(0.5,
                                                                rep(0.1,1))),
                                  order = 1),
             fill = guide_legend(override.aes = list(fill = c("#FFE8A4", "#E36C27", "#960C5F", "#470080"),
                                                     color = c("#FFE8A4", "#E36C27", "#960C5F", "#470080")),
                                 order = 2)
      )
    
    
    
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
    
    ggsave(map_tarifa_sz,
           device = "png",
           filename =  sprintf('../data/map_plots_transports/muni_%s/17-tarifa_renda_sem_zoom_new_%s.png',
                               sigla_muni,
                               sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )      
    

# Cleveland de peso da tarifa ---------------------------------------------

    
    if (sigla_muni == "dou"){
      
      recorte_tarifa <- data_micro2 %>%
        filter(Rend_pc >0) %>%
        mutate(tarifa_renda = (tarifa)/(Rend_pc*1302)) %>%
        # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
        mutate(
          quintil_renda = ntile(Rend_pc, 4)) %>%
        # mutate(total = "total") %>% 
        mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
               cor = case_when(V0606 == "Pardos" ~ "Pretos",
                               V0606 == "Pretos" ~ "Pretos",
                               V0606 == "Brancos" ~ "Brancos",
                               V0606 == "Indígenas" ~ "Indígenas")) %>%
        group_by(cor, quintil_renda, genero) %>%
        summarise(tarifa_grupo = mean(tarifa_renda, na.rm = T),
                  n = n()) %>%
        mutate(id = paste(genero, cor),
               ID_juntar = paste(genero, cor, quintil_renda))
      
    } else {
    
    recorte_tarifa <- data_micro2 %>%
      filter(Rend_pc >0) %>%
      mutate(tarifa_renda = (tarifa)/(Rend_pc*1302)) %>%
      # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
        mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      group_by(cor, quintil_renda, genero) %>%
      summarise(tarifa_grupo = mean(tarifa_renda, na.rm = T),
                n = n()) %>%
      mutate(id = paste(genero, cor),
             ID_juntar = paste(genero, cor, quintil_renda))
    
    }
    
    
    # dados da PNADC
    if (munis_pnad$pnad[which(munis_pnad$muni==sigla_muni)]==1){
      
    renda_pnad <- read.csv('../data/pnadc/rendas_medias_quartil_capitais_pnadc_2022_2.csv')
    
    renda_pnad <- renda_pnad %>% filter(Capital == munis_pnad$nome_pnad[which(munis_pnad$muni==sigla_muni)]) %>%
      mutate(Quartil = case_when(Quartil == "q1" ~ 1,
                                 Quartil == "q2" ~ 2,
                                 Quartil == "q3" ~ 3,
                                 Quartil == "q4" ~ 4),
             Raca = case_when(Raca == "Branca" ~ "Brancos",
                              Raca == "Negra" ~ "Pretos"),
             Genero = case_when(Genero == "Homem" ~ "Homens",
                              Genero == "Mulher" ~ "Mulheres")) %>%
      mutate(ID_juntar = paste(Genero, Raca, Quartil),
             tarifa_grupo = tarifa/mean) %>%
      select(genero = Genero, cor = Raca, quintil_renda = Quartil, tarifa_grupo, ID_juntar)
      
    #completar com renda da pnadc
    
    recorte_tarifa <- data_micro2 %>%
      filter(Rend_pc >0) %>%
      mutate(tarifa_renda = (tarifa)/(Rend_pc*1302)) %>%
      # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      group_by(cor, quintil_renda, genero) %>%
      summarise(tarifa_grupo = mean(tarifa_renda, na.rm = T),
                n = n()) %>%
      mutate(id = paste(genero, cor),
             ID_juntar = paste(genero, cor, quintil_renda)) %>%
      ungroup() %>%
      select(ID_juntar, id, n) %>% left_join(renda_pnad, by = "ID_juntar")
    
    }
    #######
    
    #recorte apenas gênero e renda
    
    recorte_tarifa_genero <- recorte_tarifa %>%
      ungroup() %>%
      group_by(genero, quintil_renda) %>%
      summarise(tarifa_grupo = weighted.mean(tarifa_grupo, n))
    
    #recorte apenas raça/cor e renda
    
    recorte_tarifa_cor <- recorte_tarifa %>%
      ungroup() %>%
      group_by(cor, quintil_renda) %>%
      summarise(tarifa_grupo = weighted.mean(tarifa_grupo, n))
    
    #recorte apenas raça/cor
    
    recorte_tarifa_cor2 <- recorte_tarifa %>%
      ungroup() %>%
      group_by(cor) %>%
      summarise(tarifa_grupo = weighted.mean(tarifa_grupo, n))
    
    #recorte apenas genero
    
    recorte_tarifa_gen2 <- recorte_tarifa %>%
      ungroup() %>%
      group_by(genero) %>%
      summarise(tarifa_grupo = weighted.mean(tarifa_grupo, n))
    
    #recorte apenas renda
    
    recorte_tarifa_renda <- recorte_tarifa %>%
      ungroup() %>%
      group_by(quintil_renda) %>%
      summarise(tarifa_grupo = weighted.mean(tarifa_grupo, n))
    
    recorte_tarifa_geral <- recorte_tarifa %>%
      ungroup() %>%
      summarise(tarifa_grupo = weighted.mean(tarifa_grupo, n))
    # medias_quartis <- recorte_tarifa %>% group_by(quintil_renda) %>%
    #   summarise(media_quartil = weighted.mean(tarifa_grupo, w = n))
    # 
    # recorte_tarifa_quartis <- data_micro2 %>%
    #   # filter(Rend_pc >0) %>%
    #   # mutate(tarifa_renda = (tarifa)/(Rend_pc*1302)) %>%
    #   # mutate(teste = ifelse(is.element(hex,dados_hex_intersect_freq), "OK","N"))  %>% 
    #   mutate(
    #     quintil_renda = ntile(Rend_pc, 4)) %>%
    #   # mutate(total = "total") %>% 
    #   mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
    #          cor = case_when(cor == "pard_am_ing" ~ "Pretos",
    #                          cor == "pretos" ~ "Pretos",
    #                          cor == "brancos" ~ "Brancos")) %>%
    #   group_by(quintil_renda) %>%
    #   summarise(min = min(Rend_pc, na.rm = T),
    #             mediana = median(Rend_pc, na.rm = T),
    #             max = max(Rend_pc, na.rm = T))
    
    
    pop_max_tarifa <-plyr::round_any(max(recorte_tarifa$n),10^(n_int_digits(max(recorte_tarifa$n))), f = ceiling)
    break_max_tarifa <- pop_max_tarifa
    break_leap_tarifa <- pop_max_tarifa/4
    escala_tarifa <- ifelse(pop_max_tarifa > 150000, 10, ifelse( pop_max_tarifa > 100000, 10, ifelse(pop_max_tarifa > 50000, 10, 10)))
    
    range_tot_tarifa <- max(recorte_tarifa$tarifa_grupo)-min(recorte_tarifa$tarifa_grupo)
    extend_tarifa <- 0.01
    
    if (range_tot_tarifa <= 0.005){
      passo_tarifa <- 0.001
    } else if (range_tot_tarifa <= 0.01) {
      passo_tarifa <- 0.0025
    } else if (range_tot_tarifa <= 0.025){
      passo_tarifa <- 0.005
    } else if (range_tot_tarifa <= 0.05) {
      passo_tarifa <- 0.01
    } else if (range_tot_tarifa <= 0.10) {
      passo_tarifa <- 0.025
    } else {
      passo_tarifa <- 0.10
    }
    
    range1_tarifa <- plyr::round_any(ifelse( (min(recorte_tarifa$tarifa_grupo) - extend_tarifa)<0, 0, min(recorte_tarifa$tarifa_grupo) - extend_tarifa),
                                passo_tarifa,
                                f = floor)
    
    range2_tarifa <- plyr::round_any(ifelse( (max(recorte_tarifa$tarifa_grupo) + extend_tarifa)>1, 1, max(recorte_tarifa$tarifa_grupo) + extend_tarifa),
                                passo_tarifa,
                                f = ceiling)
    
    if (sigla_muni == "dou"){
      
      plot_cleveland_tarifa <- ggplot(recorte_tarifa, aes(tarifa_grupo, as.character(quintil_renda))) +
        geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
        geom_point(aes(color = id, size= n), shape = 1, stroke = 1.5) +
        guides(fill=guide_legend(title="Gênero e cor")) +
        scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003")
        )+
        scale_size_continuous( range = c(0,escala_tarifa),
                               limits = c(0,break_max_tarifa),
                               breaks = c(break_leap_tarifa,break_leap_tarifa*2,break_leap_tarifa*3),
                               name = "Habitantes",
                               guide = "legend")
      p_tarifa <- plot_cleveland_tarifa + scale_color_manual(name = "Gênero e Cor",
                                                             values = c("Homens Brancos"="#ADBEF0",
                                                                        "Homens Pretos"="#174DE8",
                                                                        "Homens Indígenas"="grey70",
                                                                        "Mulheres Brancos" = "#EBB814",
                                                                        "Mulheres Pretos"="#B39229",
                                                                        "Mulheres Indígenas"="#cc3003"),
                                                             labels = c("Homens Brancos"="Homens Brancos",
                                                                        "Homens Pretos"="Homens Negros",
                                                                        "Mulheres Brancos"="Mulheres Brancas",
                                                                        "Mulheres Pretos"="Mulheres Negras",
                                                                        "Homens Indígenas"="Homens Indígenas",
                                                                        "Mulheres Indígenas"="Mulheres Indígenas"))+
        # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
        #                    limits = c(0,5), # baixa complexidade car                   
        #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
        scale_y_discrete(expand = c(0.4,0.4)) +
        labs(title = "% da renda mensal gasta para realizar 60 viagens*")+  
        #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
        labs(#subtitle = "por mês no transporte público",
          caption = paste0("*Considerando a tarifa do transporte público em ",
                           munis_list$munis_df$name_muni[which(munis_list$munis_df$abrev_muni==sigla_muni)]," de R$ ",
                           decisao_muni$tarifa))+
        # theme_minimal() +
        xlab("% da renda") +
        ylab("Quartil de renda per capita") +
        scale_x_continuous(labels = scales::percent,
                           limits = c(range1_tarifa,range2_tarifa),
                           breaks = seq(range1_tarifa,range2_tarifa, passo_tarifa)) +
        scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
        theme(#axis.title = element_blank(),
          panel.grid.minor = element_line(),
          legend.background = element_blank(),
          panel.background = element_blank(),
          legend.direction = "vertical",
          legend.position = "bottom",
          text = element_text(family = "sans",
                              # face = "bold",
                              size = 20),
          
          plot.title = element_text(size = 33, margin = margin(b=10), family = "encode_sans_bold"),
          plot.subtitle = element_text(size=20, color = "darkslategrey"
                                       # margin = margin(b = 25)
          ),
          plot.caption = element_text(size = 20,
                                      # margin = margin(t=10),
                                      color = "darkslategrey",
                                      hjust = 0,
                                      family = "encode_sans_light"),
          legend.title = element_text(size = 33, family = "encode_sans_bold"),
          legend.text = element_text(size = 27, family = "encode_sans_light"),
          axis.text = element_text(size = 27, family = "encode_sans_light"),
          axis.title = element_text(size = 33, family = "encode_sans_bold"))
      
      ggsave(p_tarifa,
             device = "png",
             filename =  sprintf("../data/map_plots_transports/muni_%s/19-custo_tarifa_cleveland_%s_new_pnadc.png", sigla_muni, sigla_muni),
             dpi = 300,
             width = 15, height = 11, units = "cm" )
      
      
      
    } else {
    
    plot_cleveland_tarifa <- ggplot(recorte_tarifa, aes(tarifa_grupo, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = id, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003")
      )+
      scale_size_continuous( range = c(0,escala_tarifa),
                             limits = c(0,break_max_tarifa),
                             breaks = c(break_leap_tarifa,break_leap_tarifa*2,break_leap_tarifa*3),
                             name = "Habitantes",
                             guide = "legend")
    p_tarifa <- plot_cleveland_tarifa + scale_color_manual(name = "Gênero e Cor",
                                                              values = c("Homens Brancos"="grey70",
                                                                         "Homens Pretos"="#FFB578",
                                                                         "Mulheres Brancos" = "black",
                                                                         "Mulheres Pretos"="#cc3003"),
                                                              labels = c("Homens Brancos"="Homens Brancos",
                                                                         "Homens Pretos"="Homens Negros",
                                                                         "Mulheres Brancos"="Mulheres Brancas",
                                                                         "Mulheres Pretos"="Mulheres Negras"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "% da renda mensal gasta para realizar 60 viagens*")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      labs(#subtitle = "por mês no transporte público",
           caption = paste0("*Considerando a tarifa do transporte público em ",
                            munis_list$munis_df$name_muni[which(munis_list$munis_df$abrev_muni==sigla_muni)]," de R$ ",
                                                          decisao_muni$tarifa))+
      # theme_minimal() +
      xlab("% da renda") +
      ylab("Quartil de renda per capita") +
      scale_x_continuous(labels = scales::percent,
                         limits = c(range1_tarifa,range2_tarifa),
                         breaks = seq(range1_tarifa,range2_tarifa, passo_tarifa)) +
      
      # scale_x_continuous(labels = scales::percent,
      #                    limits = c(0,1.5),
      #                    breaks = seq(0,1.5, 0.25)) +
      
      scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
      theme(#axis.title = element_blank(),
        panel.grid.minor = element_line(),
        legend.background = element_blank(),
        panel.background = element_blank(),
        legend.direction = "vertical",
        legend.position = "bottom",
        text = element_text(family = "sans",
                            # face = "bold",
                            size = 20),
        
        plot.title = element_text(size = 33, margin = margin(b=10), family = "encode_sans_bold"),
        plot.subtitle = element_text(size=20, color = "darkslategrey"
                                     # margin = margin(b = 25)
                                     ),
        plot.caption = element_text(size = 20,
                                    # margin = margin(t=10),
                                    color = "darkslategrey",
                                    hjust = 0,
                                    family = "encode_sans_light"),
        legend.title = element_text(size = 33, family = "encode_sans_bold"),
        legend.text = element_text(size = 27, family = "encode_sans_light"),
        axis.text = element_text(size = 27, family = "encode_sans_light"),
        axis.title = element_text(size = 33, family = "encode_sans_bold"))
    
    
    
    # plot_cleveland_tarifa <- ggplot(recorte_tarifa, aes(tarifa_grupo, as.character(quintil_renda))) +
    #   geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
    #   geom_point(aes(color = id, size= n), shape = 1, stroke = 1.5) +
    #   guides(fill=guide_legend(title="Gênero e cor")) +
    #   scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003")
    #   )+
    #   scale_size_continuous( range = c(0,escala_tarifa),
    #                          limits = c(0,break_max_tarifa),
    #                          breaks = c(break_leap_tarifa,break_leap_tarifa*2,break_leap_tarifa*3),
    #                          name = "Habitantes",
    #                          guide = "legend")
    # p_tarifa <- plot_cleveland_tarifa + scale_color_manual(name = "Gênero e Cor",
    #                                                        values = c("Homens Brancos"="#ADBEF0",
    #                                                                   "Homens Pretos"="#5766cc",
    #                                                                   "Mulheres Brancos" = "#33b099",
    #                                                                   "Mulheres Pretos"="#d96e0a"),
    #                                                        labels = c("Homens Brancos"="Homens Brancos",
    #                                                                   "Homens Pretos"="Homens Negros",
    #                                                                   "Mulheres Brancos"="Mulheres Brancas",
    #                                                                   "Mulheres Pretos"="Mulheres Negras"))+
    #   # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
    #   #                    limits = c(0,5), # baixa complexidade car                   
    #   #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
    #   scale_y_discrete(expand = c(0.4,0.4)) +
    #   labs(title = "% da renda mensal gasta para realizar 60 viagens*")+  
    #   #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
    #   labs(#subtitle = "por mês no transporte público",
    #     caption = paste0("*Considerando a tarifa do transporte público em ",
    #                      munis_list$munis_df$name_muni[which(munis_list$munis_df$abrev_muni==sigla_muni)]," de R$ ",
    #                      decisao_muni$tarifa))+
    #   # theme_minimal() +
    #   xlab("% da renda") +
    #   ylab("Quartil de renda per capita") +
    #   scale_x_continuous(labels = scales::percent,
    #                      limits = c(range1_tarifa,range2_tarifa),
    #                      breaks = seq(range1_tarifa,range2_tarifa, passo_tarifa)) +
    #   
    #   # scale_x_continuous(labels = scales::percent,
    #   #                    limits = c(0,1.5),
    #   #                    breaks = seq(0,1.5, 0.25)) +
    #   
    #   scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
    #   theme_bw() +
    #   theme(#axis.title = element_blank(),
    #     panel.grid.minor = element_blank(),
    #     panel.border = element_blank(),
    #     panel.grid = element_blank(),
    #     legend.background = element_blank(),
    #     panel.background = element_blank(),
    #     legend.direction = "vertical",
    #     legend.position = "bottom",
    #     legend.box = NULL,
    #     plot.background = element_rect(colour = "white"),
    #     text = element_text(family = "sans",
    #                         # face = "bold",
    #                         size = 10),
    #     
    #     plot.title = element_text(size = 10, margin = margin(b=10), family = "encode_sans_bold"),
    #     plot.subtitle = element_text(size=7, color = "darkslategrey", margin = margin(b = 25)),
    #     plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    #     legend.title = element_text(size = 10, family = "encode_sans_bold"),
    #     legend.text = element_text(size = 8, family = "encode_sans_light"),
    #     axis.text = element_text(size = 8, family = "encode_sans_light"),
    #     axis.title = element_text(size = 10, family = "encode_sans_bold"),
    #     legend.box.background = element_blank()) +
    #   guides(colour = guide_legend(order = 1,
    #                                override.aes = list(fill = rep("white", 4))),
    #          size = guide_legend(order = 2,
    #                              override.aes = list(fill = rep("white", 3))))
    
    
    
    
    ggsave(p_tarifa,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/19-custo_tarifa_cleveland_%s_new_insta.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 15, height = 11, units = "cm" )
    
    
    # ggsave(p_tarifa,
    #        device = "svg",
    #        filename =  sprintf("../data/map_plots_transports/muni_%s/19-custo_tarifa_cleveland_%s_new_insta.svg", sigla_muni, sigla_muni),
    #        dpi = 300,
    #        width = 15, height = 11, units = "cm" )
    }
    
    
    
    }
    # mapview(renda, zcol = "tarifa_renda2")
    
    
    
  }
  
  # 2. Aplica funcao ------------------------------------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=faz_grafico_e_salva)
  
  
}

graficos_tp(munis = "bel")