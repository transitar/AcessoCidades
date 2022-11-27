#Gráficos de Desiguadades na Acessibilidade


#geraação de mapas

# rm(list = ls())


source('./R/fun/setup.R')
library(patchwork)

sigla_muni <- 'poa'
width <- 14
height <- 10

sigla_muni <- 'poa'
mode1 <- "walk"
oportunidade <- "empregos"
titulo_leg <- "Empregos"
sigla_op <- "TT"
# time <- c(15,30,45,60)
time <- 30
type_acc <- "CMA"


# Temas dos gráficos ------------------------------------------------------

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

theme_cleveland_plots <- function(base_size) {
  
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
      axis.title.x = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      legend.position = "bottom",
      legend.background = element_rect(size = 0.5,
                                       linetype = "solid",
                                       colour = "grey70")
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank()
      
    )
}


faz_grafico_e_salva <- function(sigla_muni,
                                mode1,
                                oportunidade,
                                titulo_leg,
                                sigla_op,
                                width = 16.5,
                                height = 16.5){
  
  
  message(paste("Rodando",sigla_muni, "\n"))
  
  path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
  
  dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
  
  path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
  
  data_contorno <- read_rds(path_contorno)
  
  maptiles <- read_rds(path_maptiles)
  

# Dados da Microssimulação ------------------------------------------------

  data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                 sigla_muni, sigla_muni))
  
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
  
  
  #remocão dos habitantes de cor amarela e indígena
  levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
  data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))


# Dados de Acessibilidade -------------------------------------------------

  dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))

  mapas_cma <- function(sigla_muni,
                        type_acc,
                        mode1,
                        oportunidade,
                        sigla_op,
                        titulo_leg,
                        time,
                        cols = 2,
                        width = 14,
                        height = 10){
    
    message(paste("Rodando Gráfixos de ",sigla_muni, "\n"))
    
    if (type_acc == "CMA"){
      
      data_acess <- read_rds(sprintf('../r5r/accessibility/muni_%s/acc_%s.rds',
                                     sigla_muni, sigla_muni))
      
      
      dados_acc <- left_join(dados_hex, data_acess, by = c("id_hex"="origin")) %>% st_as_sf()
      
    } else if (type_acc == "TMI") {
      
      data_acess_tmi <- read_rds(sprintf('../r5r/accessibility/muni_%s/tmi/acc_tmi_%s.rds',
                                         sigla_muni, sigla_muni))
      
      
      dados_acc <- left_join(dados_hex, data_acess_tmi, by = c("id_hex"="origin")) %>% st_as_sf()
      DT <- dados_acc
      
      #inserir drop na em cada gráfico
      dados_acc_maps <- dados_acc %>% mutate_if(is.numeric, list(~na_if(., Inf)))
      dados_acc <- dados_acc_maps
    }
    sigla_munii <- sigla_muni
    # abrir acess
    acess <- dados_acc %>% filter(sigla_muni == sigla_munii)
    cols <- which(names(acess) %in% paste0(type_acc, sigla_op, time))
    acess <- acess %>% filter(mode == mode1) %>%
      select(id_hex, sigla_muni, cols)
    acess2 <- acess %>% gather(ind, valor, which(names(acess) %in% paste0(type_acc,sigla_op, time)))
    acess <- acess2
    
    # ajustar levels
    #modificar aqui para compatibilizar com tmi
    acess <- acess %>%
      mutate(ind = factor(ind, 
                          levels = paste0(type_acc,sigla_op, time),
                          labels = paste0(time, " Minutos")))
    
    
    #junção com os dados da microssimulação
    
    #Aqui, acess já foi filtrado pelo modo e data_micro já não contém amarelos e indigenas
    data_micro_acc <- data_micro2 %>% left_join(acess, by = c("hex"= "id_hex"))
    
    #precisa modificar os temas lá em cima para cada nível
    #por hora, funcionando apenas o tema para um único mapa
    if (type_acc == "CMA") {
      
      if(length(time) == 1){
        # modificar pelo cleveland quando configurá-lo
        tema <- theme_bar_plots()
        dpi_mapa <- 400
      } else if (length(time) == 2){
        tema <- theme_for_CMA_2maps()
        dpi_mapa <- 500
      } else if (length(time) == 4){
        tema <- theme_for_CMA_4maps()
        dpi_mapa <- 600
      }
      
    } else if (type_acc == "TMI") {
      tema <- theme_for_tmi
    }
    
    #plot recorte 1
    
    recorte_rr_acc <- data_micro_acc %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 4)) %>%
      ungroup() %>%
      group_by(cor, quintil_renda, genero) %>% summarise(opp = mean(valor, na.rm = TRUE),
                                                         n = n()) %>% 
      drop_na(quintil_renda)  %>%
      mutate(class = paste(genero, cor))
    
    # recorte_rr_acc
    # %>% group_by(cor, genero) %>% 
    #   summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"])
    
    # data_micro_acc_sf <- data_micro_acc %>%
    #   group_by(hex, V0606) %>%
    #   summarise(n = n())
    # 
    # data_micro_acc_sf <- data_micro_acc_sf %>% left_join(dados_hex, by = c("hex"= "id_hex")) %>%
    #   st_as_sf()
    
    
     ggplot(data = data_micro_acc_sf)+
    geom_sf()
    #faz o plot
  
    
    plot <- ggplot(recorte_rr_acc, aes(opp, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 2.5) +

      xlab("Nº de Oportunidades Acessíveis") +
      ylab("Quartil de renda per capta") +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003"),
                        )+
      scale_size(range=c(0,12),
               # breaks=c(1,4,8,10,2),
               # labels=c("1","4","8","10","25+"),
               name = "Habitantes",
               guide="legend")
    plot+ scale_color_manual(name = "Gênero e Cor",
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
      labs(title = "Quantidade de empregos acessíveis a pé")+  
           #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
           # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_blank(),
            legend.direction = "vertical",
            legend.position = "bottom",
            text = element_text(family = "sans",
                                # face = "bold",
                                size = 14),
            plot.title = element_text(size = 16, margin = margin(b=10)),
            plot.subtitle = element_text(size=14, color = "darkslategrey", margin = margin(b = 25)),
            plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0))
    
    
    quintil1 <- data_micro_acc %>%
      # mutate(total = "total") %>% 
      mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
             cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                             cor == "pretos" ~ "Pretos",
                             cor == "brancos" ~ "Brancos")) %>%
      mutate(
        quintil_renda = ntile(Rend_pc, 10)) %>%
      filter(quintil_renda == 10)
    
    ggplot(quintil1, aes(x = Rend_pc, fill = quintil_renda)) +
      geom_histogram() +
      facet_wrap(~quintil_renda)
    
    teste  <- data_micro_acc %>% filter(Rend_pc >50)
    summary(teste)
      
    plot_rr_acc <- ggplot(recorte_rr_acc,
           aes(y = opp, x = quintil_renda, fill = cor)) + 
      geom_col(position = position_dodge(),
               width = .7) + 
      scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
      geom_text(aes(label = scales::label_number(suffix = "K",
                                                 scale = 1e-3)(opp),
                    group = cor),
                position = position_dodge(width = .7),
                vjust = -0.5, size = 3.5) +
      ylab(sprintf("%s\nacessíveis", titulo_leg))+
      scale_y_continuous(labels = scales::label_number(suffix = "K",
                                                       scale = 1e-3))+
      labs(fill = "Cor") +
      theme_bar_plots() +
      theme(
        legend.position = c(0.1,0.85)
      )
    
    
    
    
    
    # fazer plots
    #adicionar if com escala viridis para o tmi
    plot3 <- ggplot()+
      # geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
      # coord_equal() +
      # scale_fill_identity()+
      # # nova escala
      # new_scale_fill() +
      geom_sf(data = st_transform(data_micro_acc_sf, 3857), aes(fill = n), colour = NA, alpha=.7, size = 0)+
      viridis::scale_fill_viridis(option = "B"
                                  # , limits = c(0, 0.72)
                                  # , breaks = c(0.001, 0.35, 0.7)
                                  # , labels = c(0, "35", "70%")
      ) +
      scale_color_manual(values = 'transparent')+
      facet_wrap(~V0606, ncol = 2)+
      labs(fill = sprintf("%s\nacessíveis", titulo_leg)) +
      theme(plot.title = element_text(hjust = 0.5, size = rel(1)))
    
    if (mode1 == "bike"){
      modo <- "bicicleta"
    } else if (mode1 == "transit"){
      modo <- "transporte_publico"
    } else if (mode == "walk"){
      modo <- "caminhada"
    }
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/%s/%s/%s/', sigla_munii, modo, type_acc ,oportunidade)))
    
    
    ggsave(plot3, 
           file= sprintf("../data/map_plots_acc/muni_%s/%s/%s/%s/%s_%s_%s_%s.png",
                         sigla_munii, modo, type_acc , oportunidade, sigla_munii, type_acc , sigla_op, paste(time, collapse = '')), 
           dpi = dpi_mapa, width = width, height = height, units = "cm")
    
    
    
    
  }
  
  
  

  
  
  #recorte de renda e cor
  
  
  
  
  
  

  
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
  
  
  # 2. Aplica funcao ------------------------------------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=faz_grafico_e_salva)
  
  
  
  
  
  
  
}