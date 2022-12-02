#Acesso físico transporte público

#10_3-mapas cicloviários e de transporte público

#geraação de mapas

# rm(list = ls())


source('./R/fun/setup.R')
library(patchwork)

width <- 16.5
height <- 16.5

sigla_muni <- 'poa'

#gráficos de ciclovias

graficos <- function(munis = "all"){
  
  
  
  faz_grafico_e_salva <- function(sigla_muni, width = 16.5, height = 16.5){
    
    message(paste("Rodando",sigla_muni, "\n"))
    
    path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
    
    dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
    
    
    # path_muni_data_censo <- sprintf('../data-raw/censo_2021_info_muni_treated_v2/muni_%s.rds',sigla_muni)
    # path_muni_hex <- sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni)
    # path_muni_setor <- sprintf('../data-raw/setores_censitarios/2019/setores_%s_2019.rds',sigla_muni)
    path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
    
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
    
    
    #dados de bikes
    sigla_municipio <- sigla_muni
    decisao_muni <- read_excel('../planilha_municipios.xlsx',
                               sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
    
    
    #TP
    
    if (decisao_muni$fonte_dados_tp == "muni_shape"){
      
      dados_peds <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                         sigla_muni, sigla_muni),
                                 layer = "tp_peds")
      
      dados_linhas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                    sigla_muni, sigla_muni),
                            layer = "tp_linhas")
      
      
    }
    # mapview(dados_ciclovias)
    
    dados_peds <- dados_peds %>% st_transform(decisao_muni$epsg)
    dados_linhas <- dados_linhas %>% st_transform(decisao_muni$epsg)
    
    dados_peds_buffer_300 <- dados_peds %>% st_buffer(300) %>% st_union() %>% st_as_sf()
    dados_peds_buffer_500 <- dados_peds %>% st_buffer(500) %>% st_union() %>% st_as_sf()
    
    area_natend300 <- data_contorno %>% st_transform(decisao_muni$epsg) %>%
      st_difference(dados_peds_buffer_300)%>%
      st_union() %>% st_as_sf()
    
    area_natend500 <- data_contorno  %>% st_transform(decisao_muni$epsg) %>%
      st_difference(dados_peds_buffer_500)%>%
      st_union() %>% st_as_sf()
    
    
    
    
    # mapview(area_natend300)
    
    #mapa localização
    cores_ciclo <- c('#5766cc', '#33b099', '#d96e0a')
    
    
    
    #mapa localização
    
    map_linhas_tp <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_linhas,3857), aes(color = '#d96e0a'),
              alpha = 1) +
      scale_color_manual(values = c("#d96e0a" ="#d96e0a"),
                        labels = c("#d96e0a"="Linhas de\nTransporte Coletivo")
      ) +
      labs(color = "")+
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +

      # tema_populacao()
      theme(
        strip.text.x = element_text(size=rel(1.2)),
        strip.background = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        legend.background = element_blank(),
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
        strip.text = element_text(size = 10),
        legend.position = c(0.25, 0.12),
        legend.spacing.y = unit(0.2, 'cm')
        # legend.margin = margin(t = -80)
      ) +
      guides(fill = guide_legend(byrow = TRUE))
    
    
    
    
    # width = 16; height = 16
    # map_empregos
    ggsave(map_linhas_tp,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/5-linhas_tp_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
    
    
    # Mapa - Buffer PEDS -------------------------------------------------
    
    # dados_peds_buffer_300

    map_peds300_buffer <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              # aes(color = "#21367d"),
              aes(color = 'black'),
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.3,
              alpha= 0.8)  +
      # theme_map() +
      geom_sf(data = st_transform(dados_peds_buffer_300, 3857),
              aes(fill = '#33b099'),
              # fill = '#33b099',
              # fill = NA,
              color = NA,
              alpha = .5,
              size = 0.1) +
      
      geom_sf(data = assentamentos,
              aes(fill = "#d96e0a"),
              # fill = "#d96e0a",
              size = 1.3,
              color = NA,
              show.legend = "polygon",
              alpha = 0.8)+
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      labs(fill = '') +
      labs(color = "") +
      
      scale_fill_manual(values = c('#33b099'='#33b099',
                                   "#d96e0a" ="#d96e0a"
                                   # '#CFF0FF' = "#CFF0FF"
                                   ),
                        labels = c('#33b099'="Cobertura de 300m",
                                   "#d96e0a"="Aglomerados\nSubnormais"
                                   # '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)"
                                   )
      ) +
      scale_color_identity(labels = c("black" = "Área urbanizada\n(Mapbiomas 2021)",
                                      blue = ""), guide = "legend") +
      
      
      # tema_populacao()
      theme(
        strip.text.x = element_text(size=rel(1.2)),
        strip.background = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        legend.background = element_blank(),
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
        strip.text = element_text(size = 10),
        legend.position = c(0.25, 0.15),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(-0.45,0,0,0, unit="cm")
        # legend.margin = margin(t = -80)
      ) +
      guides(fill = guide_legend(byrow = TRUE))
    # width = 16; height = 16
    # map_empregos
    ggsave(map_peds300_buffer,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/6-peds_buffer_300m_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
    
    #buffer 500m
    
    map_peds500_buffer <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      
      geom_sf(data = simplepolys %>% st_transform(3857),
              # aes(size = 2),
              # aes(color = "#21367d"),
              aes(color = 'black'),
              # fill = NA,
              # stroke = 2,
              # size = 2,
              linewidth = 0.3,
              alpha= 0.8)  +
      # theme_map() +
      geom_sf(data = st_transform(dados_peds_buffer_500, 3857),
              aes(fill = '#33b099'),
              # fill = '#33b099',
              # fill = NA,
              color = NA,
              alpha = .5,
              size = 0.1) +
      
      geom_sf(data = assentamentos,
              aes(fill = "#d96e0a"),
              # fill = "#d96e0a",
              size = 1.3,
              color = NA,
              show.legend = "polygon",
              alpha = 0.8)+
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      labs(fill = '') +
      labs(color = "") +
      
      scale_fill_manual(values = c('#33b099'='#33b099',
                                   "#d96e0a" ="#d96e0a"
                                   # '#CFF0FF' = "#CFF0FF"
      ),
      labels = c('#33b099'="Cobertura de 500m",
                 "#d96e0a"="Aglomerados\nSubnormais"
                 # '#CFF0FF'="Área urbanizada\n(Mapbiomas 2021)"
      )
      ) +
      scale_color_identity(labels = c("black" = "Área urbanizada\n(Mapbiomas 2021)",
                                      blue = ""), guide = "legend") +
      
      
      # tema_populacao()
      theme(
        strip.text.x = element_text(size=rel(1.2)),
        strip.background = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        legend.background = element_blank(),
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
        strip.text = element_text(size = 10),
        legend.position = c(0.25, 0.15),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(-0.45,0,0,0, unit="cm")
        # legend.margin = margin(t = -80)
      ) +
      guides(fill = guide_legend(byrow = TRUE))
    # width = 16; height = 16
    # map_empregos
    ggsave(map_peds500_buffer,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/7-peds_buffer_500m_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
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
    
    #aqui esta usando os dados os dados civlovirarios. verificar
    
    dados_hex_intersect_300 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(dados_peds_buffer_300)
    dados_hex_intersect_300 <- dados_hex_intersect %>%
      mutate(area = st_area(.))
    
    mapview(dados_hex_intersect_300)
    id_hex_intersects <- dados_hex_intersect_300$id_hex %>% unique()
    
    #dados da microssimulação
    
    data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                   sigla_muni, sigla_muni))
    #ajeitar o formato
    grid_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/grid_muni_%s.RDS',
                                   sigla_muni, sigla_muni))
    
    #checar setores com todos os renda_class_pc == n_col
    
    lista_tract <- data_micro %>% group_by(code_tract, renda_class_pc) %>%
      summarise(n = n()) %>% ungroup() %>%
      group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
                                         n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
      filter(n_classes > n_classes_col) %>% pull(code_tract)
    
    data_micro2 <- data_micro %>% filter(code_tract %in% lista_tract) %>% select(1:10, V0606, hex) %>%
      mutate(V0606 = as.factor(V0606))
    
    levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
    data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))
    
    data_micro_bus <- data_micro2 %>% filter(hex %in% id_hex_intersects)
    mapview(data_micro_bus)
    #recorte nos dados da microssimulacao
    
    recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
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
    
    
    
    plot_cleveland_bus300 <- ggplot(recorte_rr, aes(prop, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 1.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003"),
      )+
      scale_size(range=c(0,12),
                 # breaks=c(1,4,8,10,2),
                 # labels=c("1","4","8","10","25+"),
                 name = "Habitantes",
                 guide="legend")
    p_b300_ciclo <- plot_cleveland_bus300 + scale_color_manual(name = "Gênero e Cor",
                                                           values = c("Homens Brancos"="#5766cc",
                                                                      "Homens Pretos"="#21367d",
                                                                      "Mulheres Brancos" = "#33b099",
                                                                      "Mulheres Pretos"="#0f805e"),
                                                           labels = c("Homens Brancos",
                                                                      "Homens Pretos",
                                                                      "Mulheres Brancas",
                                                                      "Mulheres Pretas"))+
      # scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
      #                    limits = c(0,5), # baixa complexidade car                   
      #                    breaks = seq(0,5, by=5))+ # media complexidade a pé                   
      scale_y_discrete(expand = c(0.4,0.4)) +
      labs(title = "Proporção da população com acesso à infraestrutura cicloviária")+  
      #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
      # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      # theme_minimal() +
      xlab("% de habitantes do recorte") +
      ylab("Quartil de renda per capta") +
      scale_x_continuous(labels = scales::percent,
                         limits = c(0.2,0.6),
                         breaks = seq(0.2,0.6, 0.05)) +
      scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil")) +
      theme(#axis.title = element_blank(),
        panel.grid.minor = element_line(),
        legend.background = element_blank(),
        panel.background = element_blank(),
        legend.direction = "vertical",
        legend.position = "bottom",
        text = element_text(family = "sans",
                            # face = "bold",
                            size = 14),
        
        plot.title = element_text(size = 12, margin = margin(b=10)),
        plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))
    
    
    
    #escrita do gráfico
    
    ggsave(p_c_ciclo,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/10-ciclovias_cleveland_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = 15, height = 10, units = "cm" )
    
    
    
    
    dados_hex_intersect_500 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(dados_peds_buffer_500)
    dados_hex_intersect_500 <- dados_hex_intersect_500 %>%
      mutate(area = st_area(.))
    
    dados_hex_intersect_n300 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(area_natend300)
    dados_hex_intersect_n300 <- dados_hex_intersect_n300 %>%
      mutate(area = st_area(.))
    
    # mapview(dados_hex_intersect_n500)
    
    dados_hex_intersect_n500 <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(area_natend500)
    dados_hex_intersect_n300 <- dados_hex_intersect_n300 %>%
      mutate(area = st_area(.))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #bikes compartilhadas
    if (decisao_muni$bike_comp == 1){
    if (decisao_muni$fonte_bikecomp == "muni"){
      
      dados_bikecomp <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                        sigla_muni, sigla_muni),
                                layer = "bike_comp"
      )
    }
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
      # theme_map() +
      geom_sf(data = st_transform(dados_bikecomp,3857),aes(color = Tipo), alpha = 1, size = .8) +
      
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
    # width = 16; height = 16
    # map_empregos
    ggsave(map_bikecomp,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/2-bikes_compartilhadas_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    # Paraciclos --------------------------------------------------------------
    
    
    #map paraciclos
    
    
    if (decisao_muni$fonte_paraciclos == "muni_e_osm"){
      
      dados_paraciclos_muni <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                               sigla_muni, sigla_muni),
                                       layer = "paraciclos"
      ) %>% mutate(Tipo = "Público", id=1) %>% st_transform(decisao_muni$epsg) %>%
        select(name = Name, id, Tipo) %>% st_zm(drop = T)
      
      dados_paraciclos_osm <- read_sf(sprintf('../data-raw/dados_municipais_osm/muni_%s/muni_%s.gpkg',
                                              sigla_muni, sigla_muni),
                                      layer = "paraciclos") %>% st_transform(decisao_muni$epsg) %>%
        select(name, id = osm_id, Tipo)
      paraciclos <- rbind(dados_paraciclos_muni, dados_paraciclos_osm) 
      
    }
    # mapview(dados_bikecomp)
    
    paraciclos <- paraciclos %>% st_transform(decisao_muni$epsg)
    
    
    #mapa
    cores_paraciclos <- c('#33b099', '#d96e0a')
    
    map_paraciclos <- ggplot() +
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
      scale_fill_manual(name = "Tipo",
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
    # map_empregos
    ggsave(map_paraciclos,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/3-paraciclos_%s.png", sigla_muni, sigla_muni),
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