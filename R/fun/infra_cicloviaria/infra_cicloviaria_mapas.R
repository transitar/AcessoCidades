#10_3-mapas cicloviários e de transporte público

#geraação de mapas

# rm(list = ls())


source('./R/fun/setup.R')
library(patchwork)
library(showtext)
# library(ggmap)
library(ggspatial)
showtext_auto()
width <- 16.5
height <- 16.5
# font_add_google("Encode Sans Light 300")
font_add("encode_sans", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-VariableFont_wdth,wght.ttf')
font_add("encode_sans_regular", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Regular.ttf')
font_add("encode_sans_bold", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Bold.ttf')
font_add("encode_sans_light", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Light.ttf')
sigla_muni <- 'pal'
aprox_muni <- 0

#gráficos de ciclovias

graficos <- function(munis = "all"){
  
  
  
  faz_grafico_e_salva <- function(sigla_muni, width = 16.5, height = 16.5){
    
    message(paste("Rodando",sigla_muni, "\n"))
    
    path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    
    dados_hex <- read_rds(sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni))
    
    
    # path_muni_data_censo <- sprintf('../data-raw/censo_2021_info_muni_treated_v2/muni_%s.rds',sigla_muni)
    # path_muni_hex <- sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni)
    # path_muni_setor <- sprintf('../data-raw/setores_censitarios/2019/setores_%s_2019.rds',sigla_muni)
    if (aprox_muni == 1){
      
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019_aprox.rds',sigla_muni)
      
    } else {
      
      path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni) 
      
    }
    
    # data_muni <- read_rds(path_muni_data_censo) %>%  mutate(Cod_setor = as.character(Cod_setor))
    # data_msetor <- read_rds(path_muni_setor)
    # data_mhex <- read_rds(path_muni_hex)
    data_contorno <- read_rds(path_contorno)
    
    maptiles <- read_rds(path_maptiles)

    
    # shape de bairros
    
    # path_bairros <- sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',sigla_muni, sigla_muni)
    # 
    # bairros <- read_sf(path_bairros, layer = 'bairros')
    # mapview(bairros)
    # 
    # teste_m <- data_complete %>% select(code_tract,P006,P007,P008,P009,P010,Ptot_mulheres) %>% 
    #   gather(key = dado,value = valor, 2:7) %>% 
    #   mutate(valor = as.numeric(valor))
    sigla_municipio <- sigla_muni
    decisao_muni <- read_excel('../planilha_municipios.xlsx',
                               sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
    
    #Dados de Bairros / Áreas
    
    if(sigla_muni == "pal"){
      
      bairros <- read_sf('../data-raw/dados_municipais_recebidos/muni_pal/muni_pal.gpkg',
                         layer = "areas")
      bairros2 <- bairros %>% group_by(REGIAO) %>% st_make_valid()
        
      
    } else if (sigla_muni == "slz"){
      
      bairros <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                 sigla_muni, sigla_muni), layer= "centro") %>% st_transform(decisao_muni$epsg)
      
      bairros2 <- bairros %>% st_make_valid()
    } else {
      
      bairros <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                 sigla_muni, sigla_muni), layer= "areas") %>% st_transform(decisao_muni$epsg)
      bairros2 <- bairros %>% st_make_valid()
    }
    # mapview(bairros2)
    
    if (sigla_muni %in% c("cit", "man")){
      aguas <- st_read(sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                               sigla_muni,
                               sigla_muni),
                       layer = 'aguas')
    }
    
    #dados de bikes

    
    
    #ciclovias
    
    if (decisao_muni$fonte_ciclo == "muni"){
      
      dados_ciclovias <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                         sigla_muni, sigla_muni),
                                 layer = "infra_cicloviaria"
                                ) %>% st_as_sf() %>% st_zm(drop = T)
    } else if (decisao_muni$fonte_ciclo == "osm") {
      
      dados_ciclovias <- read_sf(sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                                         sigla_muni, sigla_muni),
                                 layer = "infra_cicloviaria"
      ) %>% st_zm(drop = T)
      
      
    }
    # mapview(dados_ciclovias)
    dados_ciclovias_length <- dados_ciclovias %>%
      mutate(length = as.numeric(st_length(.)))
    sum(dados_ciclovias_length$length)
    
    if (sigla_muni == "poa") {
    dados_ciclovias <- dados_ciclovias %>% st_transform(decisao_muni$epsg) %>%
      mutate(Tipo = ifelse(TIPO == "CICLOVIA",
                                         "Ciclovia",
                                         ifelse(TIPO == "CICLOFAIXA",
                                                "Ciclofaixa",
                                                "Compartilhado")))
    } else if (sigla_muni == 'pal'){
      
      dados_ciclovias <- dados_ciclovias %>% st_transform(decisao_muni$epsg) %>%
        mutate(Tipo = "Ciclovia/Ciclofaixa")
    } else {
      dados_ciclovias <- dados_ciclovias %>% st_transform(decisao_muni$epsg)
      
    }
      
    
    #mapa localização
    cores_ciclo <- c('#5766cc', '#33b099', '#d96e0a')
    
    
    
    
    
    
    
    #mapa localização
    
    # map_ciclovias <- ggplot() +
    #   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   coord_equal() +
    #   scale_fill_identity()+
    #   # nova escala
    #   new_scale_fill() +
    #   # theme_map() +
    #   geom_sf(data = st_transform(dados_ciclovias,3857),aes(color = Tipo), alpha = 1) +
    #   
    #   # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    #   
    #   geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
    #   
    #   
    #   
    #   # facet_wrap(~dado, labeller = labeller_grupos) +
    #   # scale_color_manual(name = "Tipo",
    #   #                   values = c("Ciclovia" = '#33b099', 'Ciclofaixa' = '#5766cc',
    #   #                              'Compartilhado' = '#d96e0a'),
    #   #                   breaks = c('Ciclovia', 'Ciclofaixa', 'Compartilhado'),
    #   #                   labels = c('Ciclovia', 'Ciclofaixa', 'Compartilhado')
    #   #                   ) +
    # scale_color_manual(name = "Tipo",
    #                   values = c("Ciclovia/Ciclofaixa" = '#33b099'),
    #                   breaks = c('Ciclovia/Ciclofaixa'),
    #                   labels = c('Ciclovia/Ciclofaixa')
    #                   ) +
    # 
    # 
    #   # tema_populacao()
    #   theme(legend.position = c(0.25,0.12),
    #         legend.background = element_blank(),
    #         # strip.text.x = element_text(size=rel(1.2)),
    #         # strip.background = element_rect(
    #         #   color = NA,
    #         #   fill = "#eff0f0"
    #         # ),
    #         panel.background = element_rect(fill = NA, colour = NA),
    #         axis.text = element_blank(),
    #         axis.title = element_blank(),
    #         axis.ticks = element_blank(), 
    #         panel.grid = element_blank(),
    #         plot.margin=unit(c(2,0,0,0),"mm"),
    #         legend.key.width=unit(2,"line"),
    #         legend.key.height = unit(.5,"cm"),
    #         legend.text=element_text("Tipo", size=rel(1)),
    #         legend.title=element_text(size=rel(1),                                   ),
    #         plot.title = element_text(hjust = 0, vjust = 4),
    #         strip.text = element_text(size = 10)
    #   ) +
    #   aproxima_muni(sigla_muni = sigla_muni)
    #     
    #     
    #   
    #   
    #   
    # # width = 16; height = 16
    # # map_ciclovias
    # ggsave(map_ciclovias,
    #        device = "png",
    #        filename =  sprintf("../data/map_plots_transports/muni_%s/1-ciclovias_%s.png", sigla_muni, sigla_muni),
    #        dpi = 300,
    #        width = width, height = height, units = "cm" )
    
    
    #NÃO USADO
    # map_ciclovias_zoom <- ggplot() +
    #   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    #   coord_equal() +
    #   scale_fill_identity()+
    #   # nova escala
    #   new_scale_fill() +
    #   # theme_map() +
    #   geom_sf(data = st_transform(dados_ciclovias,3857),aes(color = Tipo), alpha = 1) +
    #   
    #   # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
    #   
    #   geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
    #   
    #   scale_color_manual(name = "Tipo",
    #                      values = c("Ciclovia/Ciclofaixa" = '#33b099'),
    #                      breaks = c('Ciclovia/Ciclofaixa'),
    #                      labels = c('Ciclovia/Ciclofaixa')
    #   ) +
    #   
    #   
    #   # facet_wrap(~dado, labeller = labeller_grupos) +
    #   # scale_color_manual(name = "Tipo",
    #   #                    values = c("Ciclovia" = '#33b099', 'Ciclofaixa' = '#5766cc',
    #   #                               'Compartilhado' = '#d96e0a'),
    #   #                    breaks = c('Ciclovia', 'Ciclofaixa', 'Compartilhado'),
    #   #                    labels = c('Ciclovia', 'Ciclofaixa', 'Compartilhado')
    #   # ) +
    #   # tema_populacao()
    #   theme(legend.position = c(0.25,0.08),
    #         legend.background = element_blank(),
    #         # strip.text.x = element_text(size=rel(1.2)),
    #         # strip.background = element_rect(
    #         #   color = NA,
    #         #   fill = "#eff0f0"
    #         # ),
    #         panel.background = element_rect(fill = NA, colour = NA),
    #         axis.text = element_blank(),
    #         axis.title = element_blank(),
    #         axis.ticks = element_blank(), 
    #         panel.grid = element_blank(),
    #         plot.margin=unit(c(2,0,0,0),"mm"),
    #         legend.key.width=unit(2,"line"),
    #         legend.key.height = unit(.5,"cm"),
    #         legend.text=element_text("Tipo", size=rel(1)),
    #         legend.title=element_text(size=rel(1),                                   ),
    #         plot.title = element_text(hjust = 0, vjust = 4),
    #         strip.text = element_text(size = 10)
    #         ) +
    #   aproxima_muni_zoom(sigla_muni = sigla_muni)
    # 
    # ggsave(map_ciclovias_zoom,
    #        device = "png",
    #        filename =  sprintf("../data/map_plots_transports/muni_%s/1-ciclovias_zoom_%s.png", sigla_muni, sigla_muni),
    #        dpi = 300,
    #        width = width, height = height, units = "cm" )

# Buffer Ciclovias -------------------------------------------------

    dados_ciclovias_buffer <- dados_ciclovias %>% st_transform(decisao_muni$epsg) %>%
      st_buffer(300) %>% st_union() %>% st_as_sf()
    dados_ciclovias_buffer2 <- dados_ciclovias_buffer %>% as.data.frame()
    
    suppressWarnings(dir.create(sprintf('../data/ciclovias/%s/', sigla_muni), recursive = T))
    # mapview(dados_ciclovias_buffer)
    write_sf(dados_ciclovias_buffer2, sprintf('../data/ciclovias/%s/buffer_300_%s.gpkg', sigla_muni, sigla_muni))
    

# Dados de layers do mapa -------------------------------------------------

    
    
    area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
                                       sigla_muni)) %>% filter(DN == 24) %>%
      st_make_valid() %>%
      st_union()
    # mapview(simplepolys)
    
    sigla_municipio <- sigla_muni
    decisao_muni <- read_excel('../planilha_municipios.xlsx',
                               sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
    
    simplepolys <- st_make_valid(area_urbanizada) %>% st_simplify(area_urbanizada, dTolerance = 300) %>%
      st_make_valid() %>%
      st_transform(decisao_muni$epsg) %>%
      st_buffer(2) %>%
      st_union() 
    
    assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                                      sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
      mutate(title = "Assentamentos Precários") %>% st_make_valid() %>%
      st_union() %>%
      st_make_valid() %>%
      st_simplify(dTolerance = 150)
    
    
    dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                sigla_muni, sigla_muni))
    
    pop_counts <- dados_simulacao %>%
      group_by(hex) %>%
      summarise(pop_total = n()) %>% left_join(dados_hex, by = c("hex" = "id_hex")) %>%
      st_as_sf() %>% mutate(quintil = as.factor(ntile(pop_total, 4)))
    
    dados_areas <- bairros2
    

# Mapa de ciclovias -------------------------------------------------------

    cor_ag <- "#2B6CB0"
    cor_aguas <- "#92c1e3"
    
    map_ciclovias <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+

      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        #laranjas mais claros
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        
                        #laranjas forte
                        # values = c("1" = "#FEF8ED",
                        #            "2" = "#FED49A",
                        #            "3" = "#FDA065",
                        #            "4" = "#D96542"
                        #            
                        #            # "#33b099" = "#33b099",
                        #            # "aglomerados" = "#0f805e"
                        #            # 'n_urb' = '#d8faf0'
                        # ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      #laranjas mais fortes
      # guides(#fill = guide_legend(byrow = TRUE),
      #   # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
      #   fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
      #                                           color = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542")),
      #                       order = 1,
      #                       byrow = TRUE)
      # ) +
      #laranjas mais fracos
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
              linewidth = 0.4,
              alpha= 0.8)  +
      
      new_scale_fill() +
      # new_scale_color() +
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +

      
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
      # 
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
      
      geom_sf(data = st_transform(dados_ciclovias, 3857),
              aes(color = 'ciclo'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              linewidth = 1.0) +
      
      scale_color_manual(name = "Infraestrutura Cicloviária",
                         # values = c("ciclo" = "#8F040E"),
                         values = c("ciclo" = "#4b0082"),
                         label = c("ciclo" = "Ciclovias/Ciclofaixas")
      )+
      
      #Camada de águas
      
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
            legend.text=element_text(size=23, family = "encode_sans_light"),
            legend.title=element_text(size=28, family = "encode_sans_bold"),
            plot.title = element_text(hjust = 0, vjust = 4),
            strip.text = element_text(size = 10),
            legend.position = c(0.18, 0.31),
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
        color = guide_legend(override.aes = list(fill = c("white")))) +
      aproxima_muni(sigla_muni = sigla_muni)
    # width = 16; height = 16
    # map_empregos
    ggsave(map_ciclovias,
           # scale = 0.61,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/1-ciclovias_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )

# Mapa ciclovias SVG ------------------------------------------------------

    cor_ag <- "#2B6CB0"
    cor_aguas <- "#92c1e3"
    
    map_ciclovias_svg <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        #laranjas mais claros
                        values = c("1" = "#FEF8ED",
                                   "2" = "#fee6c3",
                                   "3" = "#fac690",
                                   "4" = "#e9a250"
                                   
                                   # "#33b099" = "#33b099",
                                   # "aglomerados" = "#0f805e"
                                   # 'n_urb' = '#d8faf0'
                        ),
                        
                        #laranjas forte
                        # values = c("1" = "#FEF8ED",
                        #            "2" = "#FED49A",
                        #            "3" = "#FDA065",
                        #            "4" = "#D96542"
                        #            
                        #            # "#33b099" = "#33b099",
                        #            # "aglomerados" = "#0f805e"
                        #            # 'n_urb' = '#d8faf0'
                        # ),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos"
                                  # "#33b099" = "Cobertura de 300m",
                                  # "aglomerados" = "Aglomerados subnormais"
                                  # "n_urb" = "Área urbanizada"
                        )) +
      #laranjas mais fortes
      # guides(#fill = guide_legend(byrow = TRUE),
      #   # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
      #   fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
      #                                           color = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542")),
      #                       order = 1,
      #                       byrow = TRUE)
      # ) +
      #laranjas mais fracos
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
              linewidth = 0.4,
              alpha= 0.8)  +
      
      new_scale_fill() +
      # new_scale_color() +
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      
      
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
      # 
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
      
      geom_sf(data = st_transform(dados_ciclovias, 3857),
              aes(color = 'ciclo'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              linewidth = 1.0) +
      
      scale_color_manual(name = "Infraestrutura Cicloviária",
                         # values = c("ciclo" = "#8F040E"),
                         values = c("ciclo" = "#4b0082"),
                         label = c("ciclo" = "Ciclovias/Ciclofaixas")
      )+
      
      #Camada de águas
      
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
      legend.text=element_text(size=8, family = "encode_sans_light"),
      legend.title=element_text(size=10, family = "encode_sans_bold"),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.19, 0.31),
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
        color = guide_legend(override.aes = list(fill = c("white")))) +
      aproxima_muni(sigla_muni = sigla_muni)
    # width = 16; height = 16
    # map_empregos
    ggsave(map_ciclovias_svg,
           # scale = 0.61,
           device = "svg",
           filename =  sprintf("../data/map_plots_transports/muni_%s/1-ciclovias_%s_new.svg", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )    

# Ciclovias Buffer --------------------------------------------------------
    cor_ag <- "#2B6CB0"

    map_ciclovias_buffer <- ggplot() +
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
      
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
      
              # aes(size = 2),
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      
      # geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
      
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
              linewidth = 0.4,
              alpha= 0.8)  +
      
      new_scale_fill() +
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      
      

      geom_sf(data = dados_areas %>% st_transform(3857),

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
              linewidth = 0.5,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.5)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c("urb" = "#8F040E",
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
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),
              aes(color = 'buffer'),
              # color = '#0f805e',
              # color = NA,
              fill = "grey70",
              alpha = 0.7,
              linewidth = 1.0) +
      
      scale_color_manual(name = "Infraestrutura Cicloviária",
                         values = c("buffer" = "#4b0082"),
                         label = c("buffer" = "Cobertura de 300m")
      )+
      
      
      #Camada de águas
      
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
      legend.text=element_text(size=23, family = "encode_sans_light"),
      legend.title=element_text(size=28, family = "encode_sans_bold"),
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
        color = guide_legend(override.aes = list(fill = c("grey70")),
                             order = 3)) +
      aproxima_muni(sigla_muni = sigla_muni)
    # width = 16; height = 16
    # map_empregos
    ggsave(map_ciclovias_buffer,
           # scale = 0.61,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/4-ciclovias_buffer_%s_new_insta.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )
    
    
    
    
    # setor <- read_rds('../data-raw/setores_censitarios/2019/setores_pal_2019.rds')
    # mapview(setor)

# Mapa buffer SVG ---------------------------------------------------------

    cor_ag <- "#2B6CB0"
    
    map_ciclovias_buffer_svg <- ggplot() +
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
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", size = .1) +
      
      # aes(size = 2),
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      
      # geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
      
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
              linewidth = 0.4,
              alpha= 0.8)  +
      
      new_scale_fill() +
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      
      
      
      geom_sf(data = dados_areas %>% st_transform(3857),
              
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
              linewidth = 0.5,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.5)+
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c("urb" = "#8F040E",
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
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),
              aes(color = 'buffer'),
              # color = '#0f805e',
              # color = NA,
              fill = "grey70",
              alpha = 0.7,
              linewidth = 1.0) +
      
      scale_color_manual(name = "Infraestrutura Cicloviária",
                         values = c("buffer" = "#4b0082"),
                         label = c("buffer" = "Cobertura de 300m")
      )+
      
      
      #Camada de águas
      
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
        legend.title=element_text(size=10, family = "encode_sans_bold"),
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
        color = guide_legend(override.aes = list(fill = c("grey70")),
                             order = 3)) +
      aproxima_muni(sigla_muni = sigla_muni)
    # width = 16; height = 16
    # map_empregos
    ggsave(map_ciclovias_buffer_svg,
           # scale = 0.61,
           device = "svg",
           filename =  sprintf("../data/map_plots_transports/muni_%s/4-ciclovias_buffer_%s_new_insta.svg", sigla_muni, sigla_muni),
           dpi = 300,
           width = 1.62*13, height = 13, units = "cm" )
    
    
        

# INTERSECT HEX ATENDIDOS -------------------------------------------------

    
    
    
    #intersect com hex
    
    dados_hex_intersect <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(dados_ciclovias_buffer)
    dados_hex_intersect <- dados_hex_intersect %>%
      mutate(area = st_area(.)) %>%
      mutate(area2 = as.numeric(area)) %>%
      filter(area2 > 60000.0)
    
    # mapview(dados_hex_intersect)
    
    id_hex_intersects <- dados_hex_intersect$id_hex %>% unique()
    
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
    # mapview(dados_hex_in)
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
    
    
    
    #tema dos mapas de barras
    
    theme_bar_plots <- function(base_size) {
      
      # theme_void(base_family="Roboto Condensed") %+replace%
      theme_minimal() %+replace%
        
        theme(
          # legend.position = "bottom",
          # plot.margin=unit(c(2,0,0,0),"mm"),
          # legend.key.width=unit(1,"line"),
          # legend.key.height = unit(0.4,"cm"),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          panel.background = element_blank(),
          panel.border = element_blank(),
          # strip.text = element_text(size=rel(1.3))
          # plot.title = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = 12, angle = 90),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.position = c(0.9,0.85),
          legend.background = element_rect(size = 0.5,
                                           linetype = "solid",
                                           colour = "grey70")
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank()
          
        )
    }
    
    
    #remocão dos habitantes de cor amarela e indígena
    # levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
    
    # data_micro2 <- data_micro2 %>% mutate(V0606 = case_when(V0606 == 1 ~ "Brancos",
    #                                                         V0606 == 2 ~ "Pretos",
    #                                                         V0606 == 3 ~ "Amarelos",
    #                                                         V0606 == 4 ~ "Pardos",
    #                                                         V0606 == 5 ~ "Pardos"))
    
    
    if (sigla_muni == "dou"){
      
      data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos"))
      
    } else {
    
    data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
    
    }
    
    #TESTE COM CLASSES DE RENDA
    # teste <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
    #   # mutate(total = "total") %>% 
    #   mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
    #          cor = case_when(cor == "pard_am_ing" ~ "Pardos",
    #                          cor == "pretos" ~ "Pretos",
    #                          cor == "brancos" ~ "Brancos")) %>%
    #   mutate(
    #     quintil_renda = case_when(Rend_pc < 1254/1200 ~ "Classe E",
    #                               Rend_pc < 2004/1200 ~ "Classe D",
    #                               Rend_pc < 8640/1200 ~ "Classe C",
    #                               Rend_pc < 11261/1200 ~ "Classe B",
    #                               Rend_pc > 11262/1200 ~ "Classe A"))
    
    # teste2 <- teste %>% filter(genero == "Mulheres" & quintil_renda == "Classe A" & cor == "Brancos")
    # teste3 <- teste2 %>% group_by(hex) %>% summarise(n = n()) %>%
    #   left_join(grid_micro, by = c("hex"="h3_address")) %>% st_as_sf()
    # mapview(teste3, zcol = "n")
    # 
    # testep <- teste %>% filter(genero == "Mulheres" & quintil_renda == "Classe A" & cor == "Pretos")
    # testep <- testep %>% group_by(hex) %>% summarise(n = n()) %>%
    #   left_join(grid_micro, by = c("hex"="h3_address")) %>% st_as_sf()
    # mapview(testep, zcol = "n") + mapview(teste3, zcol = "n")
    # 
    # data_micro_ciclo <- data_micro2 %>% filter(hex %in% id_hex_intersects)
    # 
    # mapview(hex) + mapview(grid_micro)
    
    

    
    

    

# Acesso à infraestrutura Cicloviária 1 - Recorte de cor e gênero --------------------------------------

    # recorte_cg <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>%
    #   # mutate(total = "total") %>%
    #   mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
    #                                      cor = case_when(cor == "pard_am_ing" ~ "Pretos",
    #                                                      cor == "pretos" ~ "Pretos",
    #                                                      cor == "brancos" ~ "Brancos")) %>%
    #   filter(teste == "OK")
    #   # group_by(V0606, genero, teste) %>% summarise(n = n()) %>%
    #   # ungroup() %>% group_by(V0606, genero) %>%
    #   # summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"])
    # 
    # 
    # 
    # 
    # 
    # 
    # ggplot(recorte_cg,
    #        aes(y = prop, x = genero, fill = V0606)) +
    #   geom_col(position = position_dodge(),
    #            width = .6)
    # 
    # ggplot(recorte_cg)+
    #   geom_boxplot(aes(Rend_pc)) +
    #   facet_wrap(~V0606)
    # 
    # 
    # 
    # +
    #   scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "blue", "grey70")) +
    #   geom_text(aes(label = scales::percent(prop), group = cor),position = position_dodge(width = .6),
    #             vjust = -0.5, size = 4.5) +
    # 
    #   geom_text(aes(label = scales::label_number(suffix = "K\n(hab)",
    #                                              decimal.mark = "," ,
    #                                              scale = 1e-3)(n),
    #                 group = cor),
    #             position = position_dodge(width = .6),
    #             vjust = 0.5,
    #             hjust = 1.2,
    #             size = 4.5,
    #             colour = "white",
    #             fontface = "bold",
    #             angle = 90) +
    #   ylab("Proporção de\nHabitantes (%)")+
    #   scale_y_continuous(labels = scales::percent,
    #                      limits = c(0,1))+
    #   labs(fill = "Cor") +
    #   theme_bar_plots()
      
    

# Acesso à infraestrutura Cicloviária 2 - Recorte de Raça e Cor  e Cleveland-----------

    
    
    
    #warning: momes das colunas da micro de poa e palmas sao diferentes
    
    
    if (sigla_muni == "dou"){
      
      recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
        # mutate(total = "total") %>% 
        mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
               cor = case_when(V0606 == "Pardos" ~ "Pretos",
                               V0606 == "Pretos" ~ "Pretos",
                               V0606 == "Brancos" ~ "Brancos",
                               V0606 == "Indígenas" ~ "Indígenas")) %>%
        mutate(
          quintil_renda = ntile(Rend_pc, 4)) %>%
        # quintil_renda = case_when(Rend_pc < 7.1 ~ 1,
        #                           Rend_pc < 60 ~ 2,
        #                           Rend_pc < 398 ~ 3,
        #                           Rend_pc >399 ~ 4)) %>%
        group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
        ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
        summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
        mutate(class = paste(genero, cor))%>%
        mutate(id = paste(class, quintil_renda))
      
      para_completar_recorte_rr <- data.frame(cor = rep("Indígenas",3),
                                              quintil_renda = c(2,3,3),
                                              genero = c(rep("Mulheres",2), "Homens"),
                                              prop = rep(0,3),
                                              n = rep(0,3),
                                              class = c(rep("Mulheres Indígenas",2), "Homens Indígenas"),
                                              id = c("Mulheres Indígenas 2","Mulheres Indígenas 3", "Homens Indígenas 3"))
      recorte_rr <- rbind(recorte_rr, para_completar_recorte_rr)
      
    } else {
      
      if (sigla_muni == "rma"){
        
        recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
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
        
        resumo.infra_clico <- weighted.mean(recorte_rr$prop, w = recorte_rr$n)
        
        medias_munis_rma <- recorte_rr %>% group_by(sigla_muni) %>%
          summarise(media_muni = sum(prop * n)/sum(n))
        
      }
    
    recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
             quintil_renda = ntile(Rend_pc, 4)) %>%
        # quintil_renda = case_when(Rend_pc < 7.1 ~ 1,
        #                           Rend_pc < 60 ~ 2,
        #                           Rend_pc < 398 ~ 3,
        #                           Rend_pc >399 ~ 4)) %>%
      group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(cor, quintil_renda, genero) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
      mutate(class = paste(genero, cor))%>%
      mutate(id = paste(class, quintil_renda))
    
    }
    #% da população total atendida
    
    resumo.infra_clico <- weighted.mean(recorte_rr$prop, w = recorte_rr$n)
    
    # library(BAMMtools)
    # aaa <- BAMMtools::getJenksBreaks(data_micro2$Rend_pc, 4, subset = NULL)
    #teswte na renda per capita
    
    # recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>%
    #   # mutate(total = "total") %>%
    #   mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
    #          cor = case_when(cor == "pard_am_ing" ~ "Pardos",
    #                          cor == "pretos" ~ "Pretos",
    #                          cor == "brancos" ~ "Brancos")) %>%
    #   mutate(
    #     quintil_renda = case_when(Rend_pc < 1254/1200 ~ "Classe E",
    #                               Rend_pc < 2004/1200 ~ "Classe D",
    #                               Rend_pc < 8640/1200 ~ "Classe C",
    #                               Rend_pc < 11261/1200 ~ "Classe B",
    #                               Rend_pc > 11262/1200 ~ "Classe A")) %>%
    #   group_by(cor, quintil_renda, genero, teste) %>% summarise(n = n()) %>%
    #   ungroup() %>% group_by(cor, quintil_renda, genero) %>%
    #   summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
    #   mutate(class = paste(genero, cor))%>%
    #   mutate(id = paste(class, quintil_renda))
    
    
    
    

    
    # recorte_rr_t <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
    #   # mutate(total = "total") %>% 
    #   mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
    #          cor = case_when(cor == "pard_am_ing" ~ "Pardos",
    #                          cor == "pretos" ~ "Pretos",
    #                          cor == "brancos" ~ "Brancos")) %>%
    #   mutate(
    #     quintil_renda = ntile(Rend_pc, 4)) %>%
    #   group_by(cor, genero, teste) %>% summarise(n = n()) %>% 
    #   ungroup() %>% group_by(cor, genero) %>% 
    #   summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"]) %>%
    #   mutate(class = paste(genero, cor))%>%
    #   mutate(id = paste(class))
    # 
    #   
    # ggplot(recorte_rr_t) +
    #   geom_histogram(aes(Rend_pc), breaks = seq(0,10,1)) +
    #   facet_wrap(~cor+~quintil_renda, ncol = 4)
    # 
    # ggplot(data_micro2) +
    #   geom_histogram(aes(Rend_pc), breaks = seq(0,5,0.5))
      # facet_wrap(~cor+~quintil_renda, ncol = 4)
      
      # mutate(class = case_when(class == "Homens Brancos" ~ "Homens Brancos",
      #                          class == "Homens Brancos" ~ "Homens Brancos",
      #                          class == "Homens Brancos" ~ "Homens Brancos",
      #                          class == "Homens Brancos" ~ "Homens Brancos",
      #                          class == "Homens Brancos" ~ "Homens Brancos",
      #                          class == "Homens Brancos" ~ "Homens Brancos"))
    
    # bora <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
    #   # mutate(total = "total") %>% 
    #   mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
    #          cor = case_when(cor == "pard_am_ing" ~ "Pretos",
    #                          cor == "pretos" ~ "Pretos",
    #                          cor == "brancos" ~ "Brancos")) %>%
    #   mutate(
    #     quintil_renda = ntile(renda_class_pc, 5)) %>%
    #   mutate(class = paste(genero, cor)) %>%
    #   
    #   # filter(class == "Homens Brancos", quintil_renda == 1 ) %>%
    #   group_by(class, quintil_renda) %>%
    #   summarise(n = n())
    #   # mutate(id = paste(class, quintil_renda))
    # 
    # bora2 <- bora %>% group_by(quintil_renda) %>%
    #   mutate(prop = n/sum(n),
    #          total_quintil = sum(n),
    #          id = paste(class, quintil_renda))
    # 
    # vamo <- left_join(recorte_rr, bora2, by = "id") %>%
    #   mutate(prop_teste = n.x / total_quintil)
    
    # ggplot(recorte_rr, aes(prop, quintil_renda)) +
    #   geom_point(aes(color = class, size = n))
    # aa <- data_micro %>% group_by(V0606) %>%
    #   summarise(prop = (n()/ nrow(data_micro))*100)
    

# Cleveland Plot de acesso a infra cicloviaria ----------------------------

    
    options(scipen = 10000000)
    
    pop_max <- plyr::round_any(max(recorte_rr$n), 10^(n_int_digits(max(recorte_rr$n))), f = ceiling)
    # pop_max <- round(max(recorte_rr$n),digits = -n_int_digits(max(recorte_rr$n)))
    break_max <- pop_max
    # break_max <- round(max(recorte_rr$n),digits = -n_int_digits(max(recorte_rr$n)))
    break_leap <- break_max/4
    escala <- ifelse(pop_max > 150000, 12, ifelse( pop_max > 100000, 12, ifelse(pop_max > 50000, 12, 12)))
    
    range_tot <- max(recorte_rr$prop)-min(recorte_rr$prop)
    
    if (range_tot <= 0.01){
      passo <- 0.0025
      extend <- 0.005
    } else if (range_tot <= 0.05) {
      passo <- 0.01
      extend <- 0.005
    } else if (range_tot <= 0.10){
      passo <- 0.025
      extend <- 0.01
    } else if (range_tot <= 25){
      passo <- 0.05
      extend <- 0.01
    } else {
      passo <- 0.05
      extend <- 0.01
    }
    
    range1 <- plyr::round_any(ifelse( (min(recorte_rr$prop) - extend)<0, 0, min(recorte_rr$prop) - extend),
                              passo, f = floor)
    # range1 <- round(ifelse( (min(recorte_rr$prop) - extend)<0, 0, min(recorte_rr$prop) - extend), digits = 2)
    # range2 <- round(ifelse( (max(recorte_rr$prop) + extend)>1, 1, max(recorte_rr$prop) + extend), digits = 2)
    range2 <- plyr::round_any(ifelse( (max(recorte_rr$prop) + extend)>1, 1, max(recorte_rr$prop) + extend),
                              passo, f=ceiling)
    
    
    if (sigla_muni == "dou"){
      
      
      plot_cleveland_ciclo <- ggplot(recorte_rr, aes(prop, as.character(quintil_renda))) +
        geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
        geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
        guides(fill=guide_legend(title="Gênero e cor")) +
        scale_fill_manual(values = c("#ADBEF0", "#174DE8", "#EBB814", "#B39229"),
        )+
        scale_size_continuous( range = c(0,escala),
                               limits = c(0,pop_max),
                               breaks = c(break_leap,break_leap*2, break_leap*3),
                               name = "Habitantes",
                               guide = "legend")
      
      
      p_c_ciclo <- plot_cleveland_ciclo + scale_color_manual(name = "Gênero e Cor",
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

        labs(title = "Proporção da população com acesso à infraestrutura cicloviária")+  

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
          axis.title = element_text(size = 35, family = "encode_sans_bold"),
          legend.box.background = element_blank()) +
        guides(colour = guide_legend(order = 1,
                                     override.aes = list(fill = rep("white", 6))),
               size = guide_legend(order = 2,
                                   override.aes = list(fill = rep("white", 3))))
      
      
      
    } else {
    
    
    plot_cleveland_ciclo <- ggplot(recorte_rr, aes(prop, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#ADBEF0", "#174DE8", "#EBB814", "#B39229"),
      )+
      # scale_size(range=c(0,10),
      #            # breaks=c(0,5,10),
      #            # labels=c("5000","10000","15000"),
      #            name = "Habitantes",
      #            guide="legend") +
      scale_size_continuous( range = c(0,escala),
                             limits = c(0,pop_max),
                             breaks = c(break_leap,break_leap*2, break_leap*3),
                             name = "Habitantes",
                             guide = "legend")
    
    
    p_c_ciclo <- plot_cleveland_ciclo + scale_color_manual(name = "Gênero e Cor",
                                                           values = c("Homens Brancos"="#ADBEF0",
                                                                      "Homens Pretos"="#174DE8",
                                                                      "Mulheres Brancos" = "#EBB814",
                                                                      "Mulheres Pretos"="#B39229"),
                                                           labels = c("Homens Brancos"="Homens Brancos",
                                                                      "Homens Pretos"="Homens Negros",
                                                                      "Mulheres Brancos"="Mulheres Brancas",
                                                                      "Mulheres Pretos"="Mulheres Negras"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      # scale_y_discrete(expand = c(1,1)) +
      labs(title = "Proporção da população com acesso à infraestrutura cicloviária")+  
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
        axis.title = element_text(size = 35, family = "encode_sans_bold"),
        legend.box.background = element_blank()) +
      guides(colour = guide_legend(order = 1,
                                   override.aes = list(fill = rep("white", 4))),
             size = guide_legend(order = 2,
                                 override.aes = list(fill = rep("white", 3))))
    }
    
    #insta
    
    # plot_cleveland_ciclo <- ggplot(recorte_rr, aes(prop, as.character(quintil_renda))) +
    #   geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
    #   geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
    #   guides(fill=guide_legend(title="Gênero e cor")) +
    #   scale_fill_manual(values = c("#ADBEF0", "#174DE8", "#EBB814", "#B39229"),
    #   )+
    #   # scale_size(range=c(0,10),
    #   #            # breaks=c(0,5,10),
    #   #            # labels=c("5000","10000","15000"),
    #   #            name = "Habitantes",
    #   #            guide="legend") +
    #   scale_size_continuous( range = c(0,escala),
    #                          limits = c(0,pop_max),
    #                          breaks = c(break_leap,break_leap*2, break_leap*3),
    #                          name = "Habitantes",
    #                          guide = "legend")
    # 
    # 
    # p_c_ciclo <- plot_cleveland_ciclo + scale_color_manual(name = "Gênero e Cor",
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
    #   # scale_y_discrete(expand = c(1,1)) +
    #   labs(title = "Proporção da população com acesso à infraestrutura cicloviária")+  
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
    
    
    
    
    #escrita do gráfico
    
    if (sigla_muni == "dou"){
      
      ggsave(p_c_ciclo,
             device = "png",
             filename =  sprintf("../data/map_plots_transports/muni_%s/10-ciclovias_cleveland_%s_new.png", sigla_muni, sigla_muni),
             dpi = 300,
             width = 15, height = 11, units = "cm" )
      
    } else {
      
    ggsave(p_c_ciclo,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/10-ciclovias_cleveland_%s_new_insta.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 15, height = 10, units = "cm" )
    }
    # ggsave(p_c_ciclo,
    #        device = "svg",
    #        filename =  sprintf("../data/map_plots_transports/muni_%s/10-ciclovias_insta_svg_%s.svg", sigla_muni, sigla_muni),
    #        dpi = 300,
    #        width = 15, height = 10, units = "cm" )
    
    
    
    

# POPULAÇÃO NÃO ATENDIDA Não automatizado--------------------------------------------------

    
    dados_hex_intersect_ntad300 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_difference(dados_ciclovias_buffer)
    dados_hex_intersect_ntad300 <- dados_hex_intersect_ntad300 %>%
      mutate(area = st_area(.)) %>%
      mutate(area2 = as.numeric(area)) %>%
      filter(area2 > 60000.0)
    
    #mapview(dados_hex)
    # mapview(dados_hex_intersect_ntad300)
    id_hex_intersects_bus_ntad300 <- dados_hex_intersect_ntad300$id_hex %>% unique()
    
    
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
    
    options(scipen = 1000000000)
    
    plot_cleveland_bike300_ntad <- ggplot(recorte_rr_ntad2, aes(n, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("grey70", "#FFB578", "black", "#cc3003"),
      )+
      scale_size_continuous( range = c(0,11),
                             limits = c(1,16000),
                             breaks = c(0,5000,10000,15000),
                             name = "Habitantes",
                             guide = "legend")
    p_b300_bike_ntad <- plot_cleveland_bike300_ntad + scale_color_manual(name = "Gênero e Cor",
                                                                       values = c("Homens Brancos"="grey70",
                                                                                  "Homens Pretos"="#FFB578",
                                                                                  "Mulheres Brancos" = "black",
                                                                                  "Mulheres Pretos"="#cc3003"),
                                                                       labels = c("Homens Brancos",
                                                                                  "Homens Pretos",
                                                                                  "Mulheres Brancas",
                                                                                  "Mulheres Pretas"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "Proporção da população sem acesso à infraestrutura Cicloviária")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      # theme_minimal() +
      xlab("Habitantes do recorte") +
      ylab("Quartil de renda per capita") +
      scale_x_continuous(labels = scales::number,
                         limits = c(0,16000),
                         # breaks = seq(1000,140000, 20000),
                         breaks = c(5000, 10000,15000)) +
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
    
    ggsave(p_b300_bike_ntad,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/11-ciclo_300_cleveland_nao_atendidos%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 15, height = 10, units = "cm" )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     ggplot(recorte_rr,
           aes(y = prop, x = quintil_renda, fill = cor)) + 
      geom_col(position = position_dodge(),
               width = .8) + 
      scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
      geom_text(aes(label = scales::percent(prop),
                    group = cor),
                position = position_dodge(width = .8),
                vjust = -0.5,
                size = 4.5) +
      
      geom_text(aes(label = scales::label_number(suffix = "K (hab)",
                                                 scale = 1e-3,
                                                 accuracy = 0.1)(n),
                    group = cor),
                position = position_dodge(width = .8),
                vjust = 0.5,
                hjust = 1.1,
                size = 4.5,
                colour = "white",
                fontface = "bold",
                angle = 90) +
      
      ylab("Proporção de\nHabitantes (%)")+
      scale_y_continuous(labels = scales::percent,
                         limits = c(0,1))+
      labs(fill = "Cor") +
      theme_bar_plots()
    
    

# Acesso à infraestrutura Cicloviária 3  - Recorte de Gênero e Ren --------

    
#     recorte_gr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
#       # mutate(total = "total") %>% 
#       mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
#              cor = case_when(cor == "pard_am_ing" ~ "Pardos",
#                              cor == "pretos" ~ "Pretos",
#                              cor == "brancos" ~ "Brancos"))%>%
#       
#       mutate(
#         quintil_renda = ntile(Rend_pc, 5)) %>%
#       
#       group_by(genero, quintil_renda, teste) %>% summarise(n = n()) %>% 
#       ungroup() %>% group_by(genero, quintil_renda) %>% 
#       summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"])
#     
#     
#     ggplot(recorte_gr,
#            aes(y = prop, x = quintil_renda, fill = genero)) + 
#       geom_col(position = position_dodge(),
#                width = .8) + 
#       scale_fill_manual(values = c("#33b099", "#5766cc")) +
#       geom_text(aes(label = scales::percent(prop), group = genero),
#                 position = position_dodge(width = .8),
#                 vjust = -0.5,
#                 size = 4.5) +
#       
#       geom_text(aes(label = scales::label_number(suffix = "K\n(hab)",
#                                                  decimal.mark = "," ,
#                                                  scale = 1e-3)(n),
#                     group = genero),
#                 position = position_dodge(width = .8),
#                 vjust = 0.5,
#                 hjust = 1.2,
#                 size = 4.5,
#                 colour = "white",
#                 fontface = "bold",
#                 angle = 90) +
#       ylab("Proporção de\nHabitantes (%)")+
#       scale_y_continuous(labels = scales::percent,
#                          limits = c(0,1))+
#       labs(fill = "Gênero") +
#       theme_bar_plots()
#     
#     
# 
# # Gráficos de desigualdade na acessibilidade ------------------------------
# 
#     
#     
#     
#     #Acessibilidade a empregos por TP
#     dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
#     data_acess <- read_rds(sprintf('../r5r/accessibility/muni_%s/acc_%s.rds',
#                                    sigla_muni, sigla_muni))
#     
#     
#     dados_acc <- left_join(dados_hex, data_acess, by = c("id_hex"="origin")) %>% st_as_sf()
#     
#     
#     data_micro_acc <- data_micro2 %>% left_join(dados_acc %>% filter(mode == "transit"), by = c("hex"= "id_hex"))
#     
#     #recorte de renda e cor
#     
#     recorte_rr_acc <- data_micro_acc %>%
#       # mutate(total = "total") %>% 
#       mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
#              cor = case_when(cor == "pard_am_ing" ~ "Pretos",
#                              cor == "pretos" ~ "Pretos",
#                              cor == "brancos" ~ "Brancos")) %>%
#       filter(!renda_class == "Total_u10") %>%
#       group_by(V0606) %>%
#       mutate(
#         quintil_acc = ntile(CMATT60, 5)) %>%
#       ungroup() %>%
#       group_by(V0606, quintil_acc) %>% summarise(opp = mean(CMATT60)) %>% 
#       drop_na(quintil_acc)
#     
#     
# 
#     
#     ggplot(recorte_rr_acc,
#            aes(y = opp, x = quintil_acc, fill = V0606)) + 
#       geom_col(position = position_dodge(),
#                width = .7) + 
#       scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "blue", "grey70")) +
#       geom_text(aes(label = scales::label_number(suffix = "K", scale = 1e-3)(opp), group = V0606),
#                 position = position_dodge(width = .7),
#                 vjust = -0.5, size = 3.5) +
#       ylab("Ooportunidades\nAcessíveis")+
#       scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3))+
#       labs(fill = "Cor") +
#       theme_bar_plots() +
#       theme(
#         legend.position = c(0.1,0.85)
#       )
#     
#     #recorte de genero e cor
#     
#     
#     recorte_cg_acc <- data_micro_acc %>%
#       # mutate(total = "total") %>% 
#       mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
#              cor = case_when(cor == "pard_am_ing" ~ "Pretos",
#                              cor == "pretos" ~ "Pretos",
#                              cor == "brancos" ~ "Brancos")) %>%
#       group_by(V0606, genero) %>% summarise(opp = mean(CMATT60, na.rm = T)) 
#     
#     
#     ggplot(recorte_cg_acc,
#            aes(y = opp, x = genero, fill = V0606)) + 
#       geom_col(position = position_dodge(),
#                width = .7) + 
#       scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "blue", "grey70")) +
#       geom_text(aes(label = scales::label_number(suffix = "K", scale = 1e-3)(opp), group = V0606),
#                 position = position_dodge(width = .7),
#                 vjust = -0.5, size = 3.5) +
#       ylab("Oportunidades\n Acessíveis")+
#       scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3),
#                          limits = c(0,500000))+
#       labs(fill = "Cor") +
#       theme_bar_plots() +
#       theme(legend.position = c(0.1,0.9))
#     
#     #teste 
#   #quintil de renda
#    
#     
#     recorte_rr_acc_renda <- data_micro_acc %>%
#       # mutate(total = "total") %>% 
#       mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
#              cor = case_when(cor == "pard_am_ing" ~ "outros",
#                              cor == "pretos" ~ "Pretos",
#                              cor == "brancos" ~ "Brancos")) %>%
#       mutate(
#         quintil_renda = ntile(Rend_pc, 5)) %>%
#       group_by(V0606, quintil_renda) %>% summarise(opp = mean(CMATT15, na.rm = T),
#                                                  pop = n())
#       # drop_na(quintil_acc)
#     
#     teste <- data_micro_acc %>%
#       # mutate(total = "total") %>% 
#       mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
#              cor = case_when(cor == "pard_am_ing" ~ "outros",
#                              cor == "pretos" ~ "Pretos",
#                              cor == "brancos" ~ "Brancos")) %>%
#       # filter(V0606 %in% "Amarela") 
#       group_by(V0606) %>%
#       mutate(quintil_renda = ntile(Rend_pc, 5)) %>% select(1:10, V0606, CMATT60, quintil_renda)
#     
#     teste2 <- teste %>% group_by(quintil_renda) %>%
#       summarise(media = mean(Rend_pc, na.rm = T),
#                 max = max(Rend_pc))
#     
#     
#     
#     
#     ggplot(teste,
#            aes(y = Rend_pc, group = quintil_renda)) + 
#       geom_boxplot(position = position_dodge(),
#                width = .7) + 
#       scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "#d96e0a", "grey70")) +
#       geom_text(aes(label = opp, group = V0606),position = position_dodge(width = .7),
#                 vjust = -0.5, size = 3.5) +
#       ylab("Oportunidades\nAcessíveis")+
#       scale_y_continuous(labels = scales::label_number(suffix = "M", scale = 1e-6))+
#       labs(fill = "Cor") +
#       theme_bar_plots() +
#       theme(
#         legend.position = c(0.1,0.9)
#       )
#     
#     
#     
# 
#     ggplot(recorte_rr_acc_renda,
#            aes(y = opp, x = quintil_renda, fill = V0606)) + 
#       geom_col(position = position_dodge(),
#                width = .7) + 
#       scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "#d96e0a", "grey70")) +
#       geom_text(aes(label = opp, group = V0606),position = position_dodge(width = .7),
#                 vjust = -0.5, size = 3.5) +
#       ylab("Proporção de\nHabitantes (%)")+
#       scale_y_continuous(labels = scales::label_number(suffix = "M\n(Oportunidades)", scale = 1e-6))+
#       labs(fill = "Cor") +
#       theme_bar_plots() +
#       theme(
#         legend.position = c(0.1,0.9)
#       )
#     
#     
#     #recorte de renda e gênero
#     
#     recorte_rg_acc <- data_micro_acc %>%
#       # mutate(total = "total") %>% 
#       mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
#              cor = case_when(cor == "pard_am_ing" ~ "Pretos",
#                              cor == "pretos" ~ "Pretos",
#                              cor == "brancos" ~ "Brancos")) %>%
#       group_by(genero) %>%
#       mutate(
#         quintil_acc = ntile(CMATT60, 5)) %>%
#       ungroup() %>%
#       group_by(genero, quintil_acc) %>% summarise(opp = mean(CMATT60)) %>% 
#       drop_na(quintil_acc)
#     
#     
#     
#     
#     ggplot(recorte_rg_acc,
#            aes(y = opp, x = quintil_acc, fill = genero)) + 
#       geom_col(position = position_dodge(),
#                width = .7) + 
#       scale_fill_manual(values = c("#33b099", "#5766cc")) +
#       geom_text(aes(label = scales::label_number(suffix = "K", scale = 1e-3)(opp), group = genero),
#                 position = position_dodge(width = .7),
#                 vjust = -0.5, size = 3.5) +
#       ylab("oportunidades\nAcessíveis")+
#       scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3))+
#       labs(fill = "Cor") +
#       theme_bar_plots() +
#       theme(
#         legend.position = c(0.1,0.85)
#       )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

# bikes compartilhadas ----------------------------------------------------

    
    #bikes compartilhadas
    
    if (decisao_muni$fonte_bikecomp == "muni"){
      
      dados_bikecomp <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                         sigla_muni, sigla_muni),
                                 layer = "bike_comp"
      )
    }
    # mapview(dados_bikecomp)
    
    dados_bikecomp <- dados_bikecomp %>% st_transform(decisao_muni$epsg) %>%
      mutate(Tipo = "Bikepoa")
    
    
    #mapa
    cores_bikecomp <- c('#d96e0a')
    
    
    map_bikecomp <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              fill = "#d8faf0",
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.1,
              alpha= 0.8)  +
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(fill = "#5766cc"),
              
              # fill = "#d96e0a",
              size = 1.3,
              color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542"
                                   
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
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
                                                color = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "aglomerados"),
              
              fill = "#0f805e",
              size = 1.3,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
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
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c("urb" = "#d8faf0",
                                    "areas" = "grey25",
                                    "aglomerados" = "#0f805e"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e")
                                                  # colour = c("grey25", "white", "white")
        ))
        # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
        #                     order = 1)
      ) +
      ggnewscale::new_scale_color() +
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      geom_sf(data = st_transform(dados_bikecomp, 3857),
              aes(color = 'bk'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              size = 0.8) +
      
      scale_color_manual(name = "Infraestrutura Cicloviária",
                         values = c("bk" = "#21367D"),
                         label = c("bk" = "Bicicletas Compartilhadas")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white")))) +
      
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
      legend.text=element_text(size=23, family = "encode_sans_light"),
      legend.title=element_text(size=28, family = "encode_sans_bold"),
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
      legend.spacing.y = unit(0.1, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni)
    

# Mapa bikes compartilhadas antigo ----------------------------------------


    
    
    map_bikecomp <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_bikecomp,3857),colour = cores_bikecomp, alpha = 1, size = .8) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_manual(name = "Tipo",
                        values = cores_bikecomp
      ) +
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
    

# Mapa bikes compartilahdas zoom antigas ---------------------------------------


    map_bikecomp_zoom <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_bikecomp,3857),colour = cores_bikecomp, alpha = 1, size = 2) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_manual(name = "Tipo",
                        values = cores_bikecomp
      ) +
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
      ) +
      coord_sf(ylim = c(-3503224,-3516303), xlim = c(-5707898,-5692457), expand = FALSE)
    

    

# Mapa bikes compartilhadas novo zoom ------------------------------------------

    map_bikecomp_zoom <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              fill = "#d8faf0",
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.1,
              alpha= 0.8)  +
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(fill = "#5766cc"),
              
              # fill = "#d96e0a",
              size = 1.3,
              color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
      
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542"
                                   
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
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
                                                color = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "aglomerados"),
              
              fill = "#0f805e",
              size = 1.3,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
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
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c("urb" = "#d8faf0",
                                    "areas" = "grey25",
                                    "aglomerados" = "#0f805e"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e")
                                                  # colour = c("grey25", "white", "white")
        ))
        # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
        #                     order = 1)
      ) +
      ggnewscale::new_scale_color() +
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      geom_sf(data = st_transform(dados_bikecomp, 3857),
              aes(color = 'bk'),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              size = 0.8) +
      
      scale_color_manual(name = "Infraestrutura Cicloviária",
                         values = c("bk" = "#21367D"),
                         label = c("bk" = "Bicicletas Compartilhadas")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white")))) +
      
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
      legend.text=element_text(size=23, family = "encode_sans_light"),
      legend.title=element_text(size=28, family = "encode_sans_bold"),
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
      legend.spacing.y = unit(0.1, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      guides(fill = guide_legend(byrow = TRUE)) +
      # coord_sf(ylim = c(0.8941684/2*(-5692457--5714870)-3503224,0.8941684/2*(-5714870--5692457)-3516303), xlim = c(-5714870,-5692457), expand = FALSE)+
      coord_sf(ylim = c(0.8941684*22413-3518969,-3518969), xlim = c(-5714870,-5692457), expand = FALSE)

# Escrita dos mapas -------------------------------------------------------

    
    # width = 16; height = 16
    # map_empregos
    ggsave(map_bikecomp,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/2-bikes_compartilhadas_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    ggsave(map_bikecomp_zoom,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/2-bikes_compartilhadas_%s_zoom.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )

# Paraciclos --------------------------------------------------------------

    
    #map paraciclos

    
    if (decisao_muni$fonte_paraciclos == "muni_e_osm"){
      
      dados_paraciclos_muni <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                        sigla_muni, sigla_muni),
                                layer = "paraciclos"
      ) %>% mutate(Tipo = "Público", id=1) %>% st_transform(decisao_muni$epsg) %>%
        # select(name = Name, id, Tipo)%>%
        st_zm(drop = T)
      
      dados_paraciclos_osm <- read_sf(sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                                              sigla_muni, sigla_muni),
                                      layer = "paraciclos") %>% st_transform(decisao_muni$epsg) %>%
        select(name, id = osm_id, Tipo)
      paraciclos <- rbind(dados_paraciclos_muni, dados_paraciclos_osm) 
      # paraciclos <- dados_paraciclos_muni
    } else if (decisao_muni$fonte_paraciclos == "osm"){
      
      
      dados_paraciclos_osm <- read_sf(sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                                              sigla_muni, sigla_muni),
                                      layer = "paraciclos") %>% st_transform(decisao_muni$epsg) %>%
        select(id = osm_id, Tipo)
      paraciclos <- dados_paraciclos_osm
      
    }
      
    # mapview(paraciclos)
    
    paraciclos <- paraciclos %>% st_transform(decisao_muni$epsg)
    
    paraciclos_buffer <- paraciclos %>% st_transform(decisao_muni$epsg) %>%
      st_buffer(80) %>% st_union() %>% st_as_sf()
    
    
    dados_oportunidades <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
    empregos2 <- dados_oportunidades %>% select(n_jobs)
    
    
    # empregos <- aopdata::read_landuse(city = "poa", year = 2019, geometry = TRUE) 
    # empregos2 <- empregos %>% select(n_jobs = T001)
    # mapview(empregos, zcol = "T001")
    # empregos <- read_sf(sprintf('../data-raw/empregos/%s_empregos2018.gpkg', sigla_muni))
    
    
    dados_empregos_intersect <- empregos2 %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(paraciclos_buffer %>% mutate(id = 1))
    
    dados_empregos_intersect <- empregos2 %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(paraciclos_buffer)
    
    dados_empregos_join <- empregos2 %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_join(paraciclos_buffer %>% mutate(id = 1)) %>%
      drop_na(id)
    

    
    dados_empregos_intersect2 <- dados_empregos_intersect %>%
      mutate(area = st_area(.)) %>%
      mutate(area2 = as.numeric(area)) %>%
      mutate(proporcao = area2/90288.20) %>%
      mutate(n_jobs = proporcao*n_jobs)
      # filter(area2 > 60000.0)
    empregos_near_paraciclos <- 100*sum(dados_empregos_intersect2$n_jobs)/sum(empregos2$n_jobs)
    

# Mapa com paraciclos e empregos ------------------------------------------


    hist(empregos2$n_jobs)
    max_jobs <- 300
    dados_empregos <- empregos2 %>% filter(n_jobs >0) %>%
      mutate(n_jobs = ifelse(n_jobs > max_jobs, max_jobs, n_jobs))
    
    limits <- c(0, max_jobs)
    breaks <- seq(0,max_jobs,max_jobs/4)
    labels <- c(seq(0,max_jobs-max_jobs/4,max_jobs/4), paste0("> ", max_jobs))
    # hist(dados_empregos$n_jobs)
    
    map_paraciclos_empregos <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(dados_empregos, 3857),
              aes(fill = n_jobs),
              colour = NA,
              alpha=.8,
              size = 0)+
      

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
    # ggnewscale::new_scale_color() +
    geom_sf(data = simplepolys %>% st_transform(3857),
            # aes(size = 2),
            aes(color = "urb"),
            # color = "grey45",
            # aes(fill = '#CFF0FF'),
            fill = NA,
            # stroke = 2,
            # size = 2,
            linewidth = 0.8,
            alpha= 0.7)  +
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "ag"),
              
              # fill = "#d96e0a",
              linewidth = 0.3,
              fill = "#2B6CB0",
              show.legend = "polygon",
              alpha = 0.5)+
      
      
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("ag", "urb", "areas"),
                         values = c("urb" = "#8F040E",
                                    "areas" = "grey45",
                                    "ag" = "#2B6CB0"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "ag" = "Aglomerados subnormais")
      )+
      
      guides(color = guide_legend(override.aes = list(fill = c("#2B6CB0", "white", "white"),
                                                      linewidth = c(1,1,1),
                                                      alpha = c(0.5,0.5,0.5)),
                                  order = 1))+
      
      ggnewscale::new_scale_color() +
      
      geom_sf(data = st_transform(dados_empregos_intersect2, 3857),
              aes(color = 'buffer30'),
              # color = '#0f805e',
              # color = NA,
              fill = "#21367D",
              alpha = 0.7,
              linewidth = 0.4) +
      
      scale_color_manual(name = "Infraestrutura de Cicloviária",
                         values = c("buffer30" = "#21367D"),
                         label = c("buffer30" = "Área de paraciclos")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("#21367D")),
                             order = 3)) +
      # viridis::scale_fill_viridis(option = 'inferno',
      #                    direction = 1,
      #                      name = "Nº de Empregos",
      #                      breaks = breaks,
      # 
      #                      labels = labels,
      #                    limits = limits)+
      
      
    scale_fill_gradientn(
      name = "Nº de Empregos",
      colors =colors_orange ,
      
      # colours = hcl.colors(n = 10,palette = "heat",rev = T),
      # values = NULL,
      space = "Lab",
      na.value = NA,
      # guide = "colourbar",
      aesthetics = "fill",
      breaks = breaks,
      limits = limits,
      labels = labels
      # colors
    ) +
      
      
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
    
    geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey85", size = .4) +
      
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
      legend.title=element_text(size=30, family = "encode_sans_bold"),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.20, 0.40),
      legend.box.background = element_rect(fill=alpha('white', 0.7),
                                           colour = "#A09C9C",
                                           linewidth = 0.8,
                                           linetype = "solid"),
      legend.background = element_blank(),
      # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
      #                                      colour = "#E0DFE3"),
      legend.spacing.y = unit(0.2, 'cm'),
      legend.box.just = "left"
      # legend.margin = margin(t = -80)
    ) +
      # guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni)
      
    
    
    
    # width = 16; height = 16
    # map_empregos
    ggsave(map_paraciclos_empregos,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/20_paraciclos_empregos_2%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = 16.5, height = 16.5, units = "cm" )
    
        
    
    
    
    
    
    #mapa
    cores_paraciclos <- c('#33b099', '#d96e0a')

    

# Paraciclos novo sem zoom ------------------------------------------------

    map_paraciclos <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              fill = "#d8faf0",
              aes(color = "urb"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              linetype = "solid",
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.1,
              alpha= 0.8)  +
      
      new_scale_fill() +
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = "grey70", linewidth = 0.8) +
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      scale_fill_manual(name = "População",
                        breaks = c("1", "2", "3", "4"),
                        values = c("1" = "#FEF8ED",
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542"
                                   
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
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(fill = "#5766cc"),
              
              # fill = "#d96e0a",
              size = 1.3,
              color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      
      guides(#fill = guide_legend(byrow = TRUE),
        # color = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e"))),
        fill = guide_legend(override.aes = list(fill = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
                                                color = c("#FEF8ED", "#FED49A", "#FDA065", "#D96542")),
                            order = 1,
                            byrow = TRUE)
      ) +
      
      new_scale_fill() +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(color = "aglomerados"),
              
              fill = "#0f805e",
              size = 1.3,
              # color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      
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
      
      scale_color_manual(name = "Uso do solo",
                         breaks = c("areas", "urb", "aglomerados"),
                         values = c("urb" = "#d8faf0",
                                    "areas" = "grey25",
                                    "aglomerados" = "#0f805e"),
                         label = c("urb" = "Área urbanizada",
                                   "areas" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)],
                                   "aglomerados" = "Aglomerados Subnormais")
      )+
      
      guides(#fill = guide_legend(byrow = TRUE),
        colour = guide_legend(override.aes = list(fill = c("white", "#d8faf0", "#0f805e")
                                                  # colour = c("grey25", "white", "white")
        ))
        # fill = guide_legend(override.aes = list(fill = "#FEF8ED", "#FED49A", "#FDA065", "#D96542"),
        #                     order = 1)
      ) +
      
      ggnewscale::new_scale_color() +
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      geom_sf(data = st_transform(paraciclos, 3857),
              aes(color = Tipo),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              size = 0.8) +
      
      
      scale_color_manual(name = "Infraestrutura Cicloviária",
                         values = c(
                           "Público" = "#E9D906",
                           "Privado" = "#21367D"),
                         label = c("Público" = "Paraciclo Público",
                                   "Privado" = "Paraciclo Privado")
      )+
      guides(#fill = guide_legend(byrow = TRUE),
        color = guide_legend(override.aes = list(fill = c("white", "white")))) +
    
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
      legend.text=element_text(size=23, family = "encode_sans_light"),
      legend.title=element_text(size=28, family = "encode_sans_bold"),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.20, 0.30),
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
      guides(fill = guide_legend(byrow = TRUE)) +
      aproxima_muni(sigla_muni = sigla_muni)
    
    
    ggsave(map_paraciclos,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/3-paraciclos_%s_new2.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    

# Paraciclos antigo sem zoom -------------------------------------------------------

    
    map_paraciclos <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(paraciclos,3857),aes(color = Tipo), alpha = 1, size = .8) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      # geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_color_manual(name = "Tipo",
                        values = cores_paraciclos
      ) +
      
      
      
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
    # map_emprego
    ggsave(map_paraciclos,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/3-paraciclos_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    

# Mapa de paraciclos com zoom novo ----------------------------------------

    
    map_paraciclos_zoom <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      
      # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
      geom_sf(data = st_transform(pop_counts, 3857),
              aes(fill = quintil),
              colour = NA,
              alpha=1,
              size = 0)+
      
      # labs(color = 'Infraestrutura Cicloviária',
      #      fill = 'População') +
      
      geom_sf(data = assentamentos,
              # aes(fill = "#d96e0a"),
              aes(fill = "#5766cc"),
              
              # fill = "#d96e0a",
              size = 1.3,
              color = NA,
              show.legend = "polygon",
              alpha = 0.9)+
      
      # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
      #         color = NA,alpha = .7, linewidth = 1) +
      geom_sf(data = st_transform(paraciclos, 3857),
              aes(color = Tipo),
              # color = '#0f805e',
              # color = NA,
              alpha = 1,
              size = 0.8) +
      
      scale_color_manual(name = "Infraestrutura Cicloviária",
                         values = c("#0f805e" = "#0f805e",
                                    "Público" = "grey30",
                                    "Privado" = "#33b099"),
                         label = c("Público" = "Paraciclo Público",
                                   "Privado" = "Paraciclo Privado")
      )+
      
      
      ggnewscale::new_scale_color() +
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              aes(color = "areas_urbanizada"),
              # color = "grey45",
              # aes(fill = '#CFF0FF'),
              fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.8,
              alpha= 0.7)  +
      scale_color_manual(name = "Área Urbanizada",
                         values = c("areas_urbanizada" = "grey50"),
                         label = c("areas_urbanizada" = munis_list$munis_df$name_muni[which(munis_list$munis_df$abrev_muni == sigla_muni)])
      )+
      
      
      scale_fill_manual(values = c("1" = "#FEF8ED",
                                   
                                   "2" = "#FED49A",
                                   "3" = "#FDA065",
                                   "4" = "#D96542",
                                   
                                   # "#33b099" = "#33b099",
                                   "#5766cc" = "#5766cc"),
                        label = c("1" = "25% menos populosos",
                                  
                                  "2" = "25% a 50% menos populosos",
                                  "3" = "25% a 50% mais populosos",
                                  "4" = "25% mais populosos",
                                  # "#33b099" = "Cobertura de 300m",
                                  "#5766cc" = "Aglomerados subnormais")) +
      labs(fill = "População") +
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
      legend.text=element_text(size=23, family = "encode_sans_light"),
      legend.title=element_text(size=28, family = "encode_sans_bold"),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 10),
      legend.position = c(0.25, 0.30),
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
      guides(fill = guide_legend(byrow = TRUE)) +
      coord_sf(ylim = c(0.8941684*22413-3518969,-3518969), xlim = c(-5714870,-5692457), expand = FALSE)
    
    
    
    ggsave(map_paraciclos_zoom,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/3-paraciclos_zoom_%s_new.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )    
    
    

# Mapa paraciclos com zoom antigo ----------------------------------------

    
    
    
    
    map_paraciclos_zoom <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(paraciclos,3857),aes(color = Tipo), alpha = 1, size = .8) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_color_manual(name = "Tipo",
                        values = cores_paraciclos
      ) +
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
      )  +
      coord_sf(ylim = c(-3503224,-3516303), xlim = c(-5707898,-5692457), expand = FALSE)
    ggsave(map_paraciclos_zoom,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/3-paraciclos_zoom_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    
  }
  
  # 2. Aplica funcao ------------------------------------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=faz_grafico_e_salva)
  
  
}
