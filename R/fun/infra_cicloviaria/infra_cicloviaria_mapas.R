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
    
    
    #ciclovias
    
    if (decisao_muni$fonte_ciclo == "muni"){
      
      dados_ciclovias <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                         sigla_muni, sigla_muni),
                                 layer = "infra_cicloviaria"
                                )
    }
    # mapview(dados_ciclovias)
    
    dados_ciclovias <- dados_ciclovias %>% st_transform(decisao_muni$epsg) %>%
      mutate(Tipo = ifelse(TIPO == "CICLOVIA",
                                         "Ciclovia",
                                         ifelse(TIPO == "CICLOFAIXA",
                                                "Ciclofaixa",
                                                "Compartilhado")))
      
    
    #mapa localização
    cores_ciclo <- c('#5766cc', '#33b099', '#d96e0a')
    
    
    
    #mapa localização
    
    map_ciclovias <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_ciclovias,3857),aes(color = Tipo), alpha = 1) +
      
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,color = 'grey70', size = .1) +
      
      
      
      # facet_wrap(~dado, labeller = labeller_grupos) +
      scale_fill_manual(name = "Tipo",
                        values = cores_ciclo,
                        breaks = c('CICLOVIA', 'CICLOFAIXA', 'COMPARTILHADO'),
                        labels = c('Ciclovia', 'Ciclofaixa', 'Compartilhado')
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
    ggsave(map_ciclovias,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/1-ciclovias_%s.png", sigla_muni, sigla_muni),
           dpi = 300,
           width = width, height = height, units = "cm" )
    
    
    

# Mapa - Buffer Ciclovias -------------------------------------------------

    dados_ciclovias_buffer <- dados_ciclovias %>% st_transform(decisao_muni$epsg) %>%
      st_buffer(300) %>% st_union() %>% st_as_sf()
    dados_ciclovias_buffer2 <- dados_ciclovias_buffer %>% as.data.frame()
    # mapview(dados_ciclovias_buffer)
    
    
    
    map_ciclovias_buffer <- ggplot() +
      geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
      coord_equal() +
      scale_fill_identity()+
      # nova escala
      new_scale_fill() +
      # theme_map() +
      geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),fill = '#21367d',
              color = 'grey70',alpha = .7, size = 0.1) +
  
      # geom_sf(data = st_transform(bairros,3857),fill = NA,color = 'grey80', size = .2) +
      
      geom_sf(data = st_transform(data_contorno,3857),fill = NA,colour = NA, size = .1) +

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
    ggsave(map_ciclovias_buffer,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/4-ciclovias_buffer_%s.png", sigla_muni, sigla_muni),
           dpi = 400,
           width = width, height = height, units = "cm" )
    
    
    
    
    
    #intersect com hex
    
    dados_hex_intersect <- dados_hex %>%
      st_as_sf() %>%
      st_transform(decisao_muni$epsg) %>%
      st_intersection(dados_ciclovias_buffer)
    dados_hex_intersect <- dados_hex_intersect %>%
      mutate(area = st_area(.))
    
    id_hex_intersects <- dados_hex_intersect$id_hex %>% unique()
    
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
    
    data_micro_ciclo <- data_micro2 %>% filter(hex %in% id_hex_intersects)
    
    
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
    
    #Acesso infra Ciclo
    #Parte 1 - Recorte de cor e gênero
    levels(data_micro2$V0606) <- c("Branca", "Preta", "Amarela", "Parda", "Indígena")
    recorte_cg <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
                                         cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                                                         cor == "pretos" ~ "Pretos",
                                                         cor == "brancos" ~ "Brancos")) %>%
      group_by(V0606, genero, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(V0606, genero) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"])
    
    
    ggplot(recorte_cg,
           aes(y = prop, x = genero, fill = V0606)) + 
      geom_col(position = position_dodge(),
               width = .5) + 
      scale_fill_manual(values = c("#33b099", "#5766cc", "grey70", "blue", "pink")) +
      geom_text(aes(label = scales::percent(prop), group = V0606),position = position_dodge(width = .5),
                vjust = -0.5, size = 4.5) +
      
      geom_text(aes(label = scales::label_number(suffix = "K \n(hab)", scale = 1e-3)(n), group = V0606),
                position = position_dodge(width = .5),
                vjust = 1.5, size = 4.5, colour = "white",
                fontface = "bold") +
      ylab("Proporção de\nHabitantes (%)")+
      scale_y_continuous(labels = scales::percent,
                         limits = c(0,1))+
      labs(fill = "Cor") +
      theme_bar_plots()
      
    
    #Gráfico 2 - Recorte de Raça e Cor
      
    
    recorte_rr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      group_by(V0606) %>%
      mutate(
             quintil_renda = ntile(Rend_pc, 5)) %>%
      ungroup() %>%
      group_by(V0606, quintil_renda, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(V0606, quintil_renda) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"])
    
    
    ggplot(recorte_rr,
           aes(y = prop, x = quintil_renda, fill = V0606)) + 
      geom_col(position = position_dodge(),
               width = .7) + 
      scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "blue", "grey70")) +
      geom_text(aes(label = scales::percent(prop), group = V0606),position = position_dodge(width = .7),
                vjust = -0.5, size = 3.5) +
      
      geom_text(aes(label = scales::label_number(suffix = "K\n(hab)", scale = 1e-3)(n), group = V0606),
                position = position_dodge(width = .7),
                vjust = 1.5, size = 3.5, colour = "white",
                fontface = "bold") +
      ylab("Proporção de\nHabitantes (%)")+
      scale_y_continuous(labels = scales::percent,
                         limits = c(0,1))+
      labs(fill = "Cor") +
      theme_bar_plots()
    
    
    #Recorte 3 - Gênero e renda
    
    
    recorte_gr <- data_micro2 %>% mutate(teste = ifelse(is.element(hex,id_hex_intersects), "OK","N"))  %>% 
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      filter(!renda_class == "Total_u10") %>%
      
      mutate(
        quintil_renda = ntile(Rend_pc, 5)) %>%
      
      group_by(genero, quintil_renda, teste) %>% summarise(n = n()) %>% 
      ungroup() %>% group_by(genero, quintil_renda) %>% 
      summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"])
    
    
    ggplot(recorte_gr,
           aes(y = prop, x = quintil_renda, fill = genero)) + 
      geom_col(position = position_dodge(),
               width = .7) + 
      scale_fill_manual(values = c("#33b099", "#5766cc")) +
      geom_text(aes(label = scales::percent(prop), group = genero),position = position_dodge(width = .7),
                vjust = -0.5, size = 3.5) +
      
      geom_text(aes(label = scales::label_number(suffix = "K\n(hab)", scale = 1e-3)(n), group = genero),
                position = position_dodge(width = .7),
                vjust = 1.5, size = 3.5, colour = "white",
                fontface = "bold") +
      ylab("Proporção de\nHabitantes (%)")+
      scale_y_continuous(labels = scales::percent,
                         limits = c(0,1))+
      labs(fill = "Gênero") +
      theme_bar_plots()
    
    #Acessibilidade a empregos por TP
    dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
    data_acess <- read_rds(sprintf('../r5r/accessibility/muni_%s/acc_%s.rds',
                                   sigla_muni, sigla_muni))
    
    
    dados_acc <- left_join(dados_hex, data_acess, by = c("id_hex"="origin")) %>% st_as_sf()
    
    
    data_micro_acc <- data_micro2 %>% left_join(dados_acc %>% filter(mode == "transit"), by = c("hex"= "id_hex"))
    
    #recorte de renda e cor
    
    recorte_rr_acc <- data_micro_acc %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      filter(!renda_class == "Total_u10") %>%
      group_by(V0606) %>%
      mutate(
        quintil_acc = ntile(CMATT60, 5)) %>%
      ungroup() %>%
      group_by(V0606, quintil_acc) %>% summarise(opp = mean(CMATT60)) %>% 
      drop_na(quintil_acc)
    
    

    
    ggplot(recorte_rr_acc,
           aes(y = opp, x = quintil_acc, fill = V0606)) + 
      geom_col(position = position_dodge(),
               width = .7) + 
      scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "blue", "grey70")) +
      geom_text(aes(label = scales::label_number(suffix = "K", scale = 1e-3)(opp), group = V0606),
                position = position_dodge(width = .7),
                vjust = -0.5, size = 3.5) +
      ylab("Ooportunidades\nAcessíveis")+
      scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3))+
      labs(fill = "Cor") +
      theme_bar_plots() +
      theme(
        legend.position = c(0.1,0.85)
      )
    
    #recorte de genero e cor
    
    
    recorte_cg_acc <- data_micro_acc %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      group_by(V0606, genero) %>% summarise(opp = mean(CMATT60, na.rm = T)) 
    
    
    ggplot(recorte_cg_acc,
           aes(y = opp, x = genero, fill = V0606)) + 
      geom_col(position = position_dodge(),
               width = .7) + 
      scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "blue", "grey70")) +
      geom_text(aes(label = scales::label_number(suffix = "K", scale = 1e-3)(opp), group = V0606),
                position = position_dodge(width = .7),
                vjust = -0.5, size = 3.5) +
      ylab("Oportunidades\n Acessíveis")+
      scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3),
                         limits = c(0,500000))+
      labs(fill = "Cor") +
      theme_bar_plots() +
      theme(legend.position = c(0.1,0.9))
    
    #teste 
  #quintil de renda
   
    
    recorte_rr_acc_renda <- data_micro_acc %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
             cor = case_when(cor == "pard_am_ing" ~ "outros",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 5)) %>%
      group_by(V0606, quintil_renda) %>% summarise(opp = mean(CMATT15, na.rm = T),
                                                 pop = n())
      # drop_na(quintil_acc)
    
    teste <- data_micro_acc %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
             cor = case_when(cor == "pard_am_ing" ~ "outros",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      # filter(V0606 %in% "Amarela") 
      group_by(V0606) %>%
      mutate(quintil_renda = ntile(Rend_pc, 5)) %>% select(1:10, V0606, CMATT60, quintil_renda)
    
    teste2 <- teste %>% group_by(quintil_renda) %>%
      summarise(media = mean(Rend_pc, na.rm = T),
                max = max(Rend_pc))
    
    
    
    
    ggplot(teste,
           aes(y = Rend_pc, group = quintil_renda)) + 
      geom_boxplot(position = position_dodge(),
               width = .7) + 
      scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "#d96e0a", "grey70")) +
      geom_text(aes(label = opp, group = V0606),position = position_dodge(width = .7),
                vjust = -0.5, size = 3.5) +
      ylab("Oportunidades\nAcessíveis")+
      scale_y_continuous(labels = scales::label_number(suffix = "M", scale = 1e-6))+
      labs(fill = "Cor") +
      theme_bar_plots() +
      theme(
        legend.position = c(0.1,0.9)
      )
    
    
    

    ggplot(recorte_rr_acc_renda,
           aes(y = opp, x = quintil_renda, fill = V0606)) + 
      geom_col(position = position_dodge(),
               width = .7) + 
      scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "#d96e0a", "grey70")) +
      geom_text(aes(label = opp, group = V0606),position = position_dodge(width = .7),
                vjust = -0.5, size = 3.5) +
      ylab("Proporção de\nHabitantes (%)")+
      scale_y_continuous(labels = scales::label_number(suffix = "M\n(Oportunidades)", scale = 1e-6))+
      labs(fill = "Cor") +
      theme_bar_plots() +
      theme(
        legend.position = c(0.1,0.9)
      )
    
    
    #recorte de renda e gênero
    
    recorte_rg_acc <- data_micro_acc %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      group_by(genero) %>%
      mutate(
        quintil_acc = ntile(CMATT60, 5)) %>%
      ungroup() %>%
      group_by(genero, quintil_acc) %>% summarise(opp = mean(CMATT60)) %>% 
      drop_na(quintil_acc)
    
    
    
    
    ggplot(recorte_rg_acc,
           aes(y = opp, x = quintil_acc, fill = genero)) + 
      geom_col(position = position_dodge(),
               width = .7) + 
      scale_fill_manual(values = c("#33b099", "#5766cc")) +
      geom_text(aes(label = scales::label_number(suffix = "K", scale = 1e-3)(opp), group = genero),
                position = position_dodge(width = .7),
                vjust = -0.5, size = 3.5) +
      ylab("oportunidades\nAcessíveis")+
      scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3))+
      labs(fill = "Cor") +
      theme_bar_plots() +
      theme(
        legend.position = c(0.1,0.85)
      )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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
    
    
    # width = 16; height = 16
    # map_empregos
    ggsave(map_bikecomp,
           device = "png",
           filename =  sprintf("../data/map_plots_transports/muni_%s/2-bikes_compartilhadas_%s.png", sigla_muni, sigla_muni),
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