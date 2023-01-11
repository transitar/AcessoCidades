#Gráficos de Desiguadades na Acessibilidade


#geraação de mapas

# rm(list = ls())


source('./R/fun/setup.R')
library(patchwork)
library(showtext)

# sigla_muni <- 'pal'
width <- 15
height <- 10

sigla_muni <- 'poa'
mode1 <- "transit"
oportunidade <- "empregos"
titulo_leg <- "Empregos"
sigla_op <- "TT"
# time <- c(15,30,45,60)
time <- 45
type_acc <- "CMA"



font_add("encode_sans", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-VariableFont_wdth,wght.ttf')
font_add("encode_sans_regular", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Regular.ttf')
font_add("encode_sans_bold", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Bold.ttf')
font_add("encode_sans_light", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Light.ttf')
# sigla_muni <- 'pal'


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
  
  max_pop <- nrow(data_micro)
  
  grid_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/grid_muni_%s.RDS',
                                 sigla_muni, sigla_muni))
  
  #checar setores com todos os renda_class_pc == n_col
  
  # lista_tract <- data_micro %>% dplyr::group_by(code_tract, renda_class_pc) %>%
  #   dplyr::summarise(n = dplyr::n()) %>% ungroup() %>%
  #   group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
  #                                      n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
  #   filter(n_classes > n_classes_col) %>% pull(code_tract)
  
  data_micro2 <- data_micro %>%
    # filter(code_tract %in% lista_tract) %>%
    select(1:12, V0606, hex) %>%
    mutate(V0606 = as.factor(V0606))
  
  
  #remocão dos habitantes de cor amarela e indígena
  levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
  data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))


# Dados de Acessibilidade -------------------------------------------------

  dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))

  mapas_cma_clev <- function(sigla_muni,
                        type_acc,
                        mode1,
                        oportunidade,
                        sigla_op,
                        titulo_leg,
                        time,
                        cols = 2,
                        width = 14,
                        height = 10){
    
    showtext_auto()
    font_add("encode_sans", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-VariableFont_wdth,wght.ttf')
    font_add("encode_sans_regular", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Regular.ttf')
    font_add("encode_sans_bold", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Bold.ttf')
    font_add("encode_sans_light", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Light.ttf')
    
    
    message(paste("Rodando Gráfixos de",sigla_muni, "\n"))
    
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
    
    if (type_acc == "CMA"){
    dados_hex <- readr::read_rds(sprintf("../data/dados_hex/muni_%s/dados_hex_%s.rds",
                                         sigla_muni, sigla_muni))
    
    empregos_tot <- sum(dados_hex$n_jobs, na.rm = T)
    saude_tot <- sum(dados_hex$S001, na.rm = T)
    saude_n1 <- sum(dados_hex$S002, na.rm = T)
    saude_n2 <- sum(dados_hex$S003, na.rm = T)
    saude_n3 <- sum(dados_hex$S004, na.rm = T)
    educacao_tot <- sum(dados_hex$E001, na.rm = T)
    educacao_n1 <- sum(dados_hex$E002, na.rm = T)
    educacao_n2 <- sum(dados_hex$E003, na.rm = T)
    educacao_n3 <- sum(dados_hex$E004, na.rm = T)
    matriculas_tot <- sum(dados_hex$M001, na.rm = T)
    matriculas_n1 <- sum(dados_hex$M002, na.rm = T)
    matriculas_n2 <- sum(dados_hex$M003, na.rm = T)
    matriculas_n3 <- sum(dados_hex$M004, na.rm = T)
    lazer_tot2 <- sum(dados_hex$lazer_tot, na.rm = T)
    paraciclos_tot <- sum(dados_hex$paraciclos, na.rm = T)
    bikes_comp_tot <- sum(dados_hex$n_bikes, na.rm = T)
    
    
    
    acess2 <- dados_acc %>%
      mutate(across(.cols = matches("CMATT"),
                    ~ .x/empregos_tot)) %>%
      mutate(across(.cols = matches("CMAST"),
                    ~ .x/saude_tot)) %>%
      mutate(across(.cols = matches("CMASB"),
                    ~ .x/saude_n1)) %>%
      mutate(across(.cols = matches("CMASM"),
                    ~ .x/saude_n2)) %>%
      mutate(across(.cols = matches("CMASA"),
                    ~ .x/saude_n3)) %>%
      mutate(across(.cols = matches("CMAET"),
                    ~ .x/educacao_tot)) %>%
      mutate(across(.cols = matches("CMAEI"),
                    ~ .x/educacao_n1)) %>%
      mutate(across(.cols = matches("CMAEF"),
                    ~ .x/educacao_n2)) %>%
      mutate(across(.cols = matches("CMAEM"),
                    ~ .x/educacao_n3)) %>%
      mutate(across(.cols = matches("CMAMT"),
                    ~ .x/matriculas_tot)) %>%
      mutate(across(.cols = matches("CMAMI"),
                    ~ .x/matriculas_n1)) %>%
      mutate(across(.cols = matches("CMAMF"),
                    ~ .x/matriculas_n2)) %>%
      mutate(across(.cols = matches("CMAMM"),
                    ~ .x/matriculas_n3)) %>%
      mutate(across(.cols = matches("CMALZ"),
                    ~ .x/lazer_tot2)) %>%
      mutate(across(.cols = matches("CMAPR"),
                    ~ .x/paraciclos_tot)) %>%
      mutate(across(.cols = matches("CMABK"),
                    ~ .x/bikes_comp_tot))
    
    
    acess <- acess2
    } else if (type_acc == "TMI"){
      acess <- dados_acc
    }
    
    rm(dados_acc, dados_acc_maps); gc()
    
    if (type_acc == "CMA"){
    sigla_munii <- sigla_muni
    # abrir acess
    acess <- acess %>% filter(sigla_muni == sigla_munii)
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
    
    } else if (type_acc == "TMI"){
      
      sigla_munii <- sigla_muni
      # abrir acess
      acess <- acess %>% filter(sigla_muni == sigla_munii)
      cols <- which(names(acess) %in% paste0(type_acc, sigla_op))
      acess <- acess %>% filter(mode == mode1) %>%
        select(id_hex, sigla_muni, cols)
      acess2 <- acess %>% gather(ind, valor, which(names(acess) %in% paste0(type_acc,sigla_op)))
      acess <- acess2
      
      
    }
    #junção com os dados da microssimulação
    
    #Aqui, acess já foi filtrado pelo modo e data_micro já não contém amarelos e indigenas
    data_micro_acc <- data_micro2 %>% left_join(acess, by = c("hex"= "id_hex"))
    rm(acess, acess2); gc()
    #precisa modificar os temas lá em cima para cada nível
    #por hora, funcionando apenas o tema para um único mapa
    # if (type_acc == "CMA") {
    #   
    #   if(length(time) == 1){
    #     # modificar pelo cleveland quando configurá-lo
    #     tema <- theme_bar_plots()
    #     dpi_mapa <- 400
    #   } else if (length(time) == 2){
    #     tema <- theme_for_CMA_2maps()
    #     dpi_mapa <- 500
    #   } else if (length(time) == 4){
    #     tema <- theme_for_CMA_4maps()
    #     dpi_mapa <- 600
    #   }
    #   
    # } else if (type_acc == "TMI") {
    #   tema <- theme_for_tmi
    # }
    
    #plot recorte 1
    if (type_acc == "CMA"){
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
                                                         n =dplyr::n()) %>% 
      drop_na(quintil_renda)  %>%
      mutate(class = paste(genero, cor))
    } else if(type_acc == "TMI") {
      
      recorte_rr_acc <- data_micro_acc %>%
        # mutate(total = "total") %>% 
        mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
               cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                               cor == "pretos" ~ "Pretos",
                               cor == "brancos" ~ "Brancos")) %>%
        mutate(
          quintil_renda = ntile(Rend_pc, 4)) %>%
        ungroup() %>%
        group_by(cor, quintil_renda, genero) %>% summarise(tempo = mean(valor, na.rm = TRUE),
                                                           n = dplyr::n()) %>% 
        drop_na(quintil_renda)  %>%
        mutate(class = paste(genero, cor))
      
    }
    # recorte_rr_acc
    rm(data_micro_acc); gc()
    # %>% group_by(cor, genero) %>% 
    #   summarise(prop = round(n[teste == "OK"]/sum(n),digits = 2), n = n[teste == "OK"])
    
    # data_micro_acc_sf <- data_micro_acc %>%
    #   group_by(hex, V0606) %>%
    #   summarise(n = n())
    # 
    # data_micro_acc_sf <- data_micro_acc_sf %>% left_join(dados_hex, by = c("hex"= "id_hex")) %>%
    #   st_as_sf()
    
    
    #  ggplot(data = data_micro_acc_sf)+
    # geom_sf()
    #faz o plot
    
    pop_max <- round(max(recorte_rr_acc$n),digits = -4)
    break_max <- round(max(recorte_rr_acc$n),digits = -5)
    break_leap <- break_max/4
    escala <- ifelse(pop_max > 150000, 12, ifelse( pop_max > 100000, 15, ifelse(pop_max > 50000, 17, 19)))
    
    if (type_acc == "CMA"){
      
      range_tot <- max(recorte_rr_acc$opp)-min(recorte_rr_acc$opp)

      if (range_tot <= 0.002){
        passo <- 0.0025
        extend <- 0.01
      } else if (range_tot <= 0.05) {
        passo <- 0.005
        extend <- 0.01
      } else if (range_tot <= 0.10){
        passo <- 0.01
        extend <- 0.01
      } else {
        passo <- 0.025
        extend <- 0.01
      }
  
    range1 <- round(ifelse( (min(recorte_rr_acc$opp) - extend)<0, 0, min(recorte_rr_acc$opp) - extend), digits = 2)
    range2 <- round(ifelse( (max(recorte_rr_acc$opp) + extend)>1, 1, max(recorte_rr_acc$opp) + extend), digits = 2)
    # by1 <- round((range2 -range1)/100, 2)
    plot <- ggplot(recorte_rr_acc, aes(opp, as.character(quintil_renda))) +
      geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
      geom_point(aes(color = class, size= n), shape = 1, stroke = 2.5) +
      guides(fill=guide_legend(title="Gênero e cor")) +
      scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003"),
                        )+
      scale_size_continuous( range = c(0,escala),
                             limits = c(0,break_max),
                             breaks = c(break_leap,break_leap*2,break_leap*3),
                             name = "Habitantes",
                             guide = "legend")
    cma_clev <- plot+ scale_color_manual(name = "Gênero e Cor",
                                         values = c("Homens Brancos"="#ADBEF0",
                                                    "Homens Pretos"="#174DE8",
                                                    "Mulheres Brancos" = "#EBB814",
                                                    "Mulheres Pretos"="#B39229"),
                                         labels = c("Homens Brancos",
                                                    "Homens Pretos",
                                                    "Mulheres Brancas",
                                                    "Mulheres Pretas"))+
      scale_x_continuous(labels = scales::percent, # baixa complexidade car
                         limits = c(range1,range2), # baixa complexidade car
                         breaks = seq(range1,range2, by=passo))+ # media complexidade a pé
      scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil"),
                       expand = c(0.35,0.35)) +
      # scale_y_discrete(expand = c(0.35,0.35)) +
      # labs(title = sprintf()"Quantidade de empregos acessíveis a pé")+  
           #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
           # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
      # theme_minimal() +
      xlab(paste0("% de ", titulo_leg, " Acessíveis")) +
      ylab("Quartil de renda per capta") +
      theme(#axis.title = element_blank(),
        panel.grid.minor = element_line(),
        panel.background = element_blank(),
        text = element_text(family = "sans",
                            # face = "bold",
                            size = 20),
        #titulo
        plot.title = element_text(size = 35, margin = margin(b=10), family = "encode_sans_bold"),
        plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 30, margin = margin(t=10), color = "grey70", hjust = 0),
        #legenda
        legend.title = element_text(size = 35, family = "encode_sans_bold"),
        legend.text = element_text(size = 30, family = "encode_sans_light"),
        legend.direction = "vertical",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.spacing.y = unit(0.2, "cm"),
        #Eixos
        axis.text = element_text(size = 30, family = "encode_sans_light"),
        axis.title = element_text(size = 35, family = "encode_sans_bold")) +
      guides(size = guide_legend(order = 2))
    
    } else if (type_acc=="TMI"){
      
      range_tot <- max(recorte_rr_acc$tempo)-min(recorte_rr_acc$tempo)
      
      if (range_tot <= 2){
        passo <- 0.25
        extend <- 1
      } else if (range_tot <= 5) {
        passo <- 0.5
        extend <- 1
      } else if (range_tot <= 10){
        passo <- 1
        extend <- 1
      } else {
        passo <- 2.5
        extend <- 1
      }
      # library(plyr)
      range1 <- plyr::round_any(round(ifelse( (min(recorte_rr_acc$tempo) - extend/100)<0, 0, min(recorte_rr_acc$tempo) - extend/100), digits = 0)%/% 1, extend/100)
      range2 <- plyr::round_any(round(ifelse( (max(recorte_rr_acc$tempo) + extend/100)>180, 180, max(recorte_rr_acc$tempo) + extend/100), digits = 0) %/% 1, extend/100)

      

      # by1 <- round((range2 -range1)/100, 2)
      plot <- ggplot(recorte_rr_acc, aes(tempo, as.character(quintil_renda))) +
        geom_line(aes(group = quintil_renda), linewidth = 1.5, color = "grey70")+
        geom_point(aes(color = class, size= n), shape = 1, stroke = 2.5) +
        guides(fill=guide_legend(title="Gênero e cor")) +
        scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a", "#cc3003"),
        )+
        scale_size_continuous( range = c(0,escala),
                               limits = c(0,break_max),
                               breaks = c(break_leap,break_leap*2,break_leap*3),
                               name = "Habitantes",
                               guide = "legend")
      
      cma_clev <- plot + scale_color_manual(name = "Gênero e Cor",
                                            values = c("Homens Brancos"="#ADBEF0",
                                                       "Homens Pretos"="#174DE8",
                                                       "Mulheres Brancos" = "#EBB814",
                                                       "Mulheres Pretos"="#B39229"),
                                            labels = c("Homens Brancos",
                                                       "Homens Pretos",
                                                       "Mulheres Brancas",
                                                       "Mulheres Pretas"))+
        scale_x_continuous(labels = scales::number, # baixa complexidade car
                           limits = c(range1,range2), # baixa complexidade car
                           breaks = seq(range1,range2, by=passo))+ # media complexidade a pé
        scale_y_discrete(labels = c("1º Quartil", "2º Quartil", "3º Quartil", "4º Quartil"),
                         expand = c(0.35,0.35)) +
        # scale_y_discrete(expand = c(0.5,0.5)) +
        # labs(title = sprintf()"Quantidade de empregos acessíveis a pé")+  
        #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
        # subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
        # theme_minimal() +
        xlab(paste0("Tempo Mínimo de Acesso a ", titulo_leg, " (min)")) +
        ylab("Quartil de renda per capta") +
        theme(#axis.title = element_blank(),
          panel.grid.minor = element_line(),
          panel.background = element_blank(),
          text = element_text(family = "sans",
                              # face = "bold",
                              size = 20),
          #titulo
          plot.title = element_text(size = 35, margin = margin(b=10), family = "encode_sans_bold"),
          plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
          plot.caption = element_text(size = 30, margin = margin(t=10), color = "grey70", hjust = 0),
          #legenda
          legend.title = element_text(size = 35, family = "encode_sans_bold"),
          legend.text = element_text(size = 30, family = "encode_sans_light"),
          legend.direction = "vertical",
          legend.position = "bottom",
          legend.background = element_blank(),
          legend.spacing.y = unit(0.2, "cm"),
          #Eixos
          axis.text = element_text(size = 30, family = "encode_sans_light"),
          axis.title = element_text(size = 35, family = "encode_sans_bold")) +
        guides(size = guide_legend(order = 2))
      
      
      
      
    }
    
    if (mode1 == "bike"){
      modo <- "bicicleta"
    } else if (mode1 == "transit"){
      modo <- "transporte_publico"
    } else if (mode1 == "walk"){
      modo <- "caminhada"
    }
    
    
    suppressWarnings(dir.create(sprintf('../data/map_plots_desigualdade/muni_%s/%s/%s/%s/', sigla_muni, modo, type_acc ,oportunidade)))
    
    
    ggsave(cma_clev, 
           file= sprintf("../data/map_plots_acc_desigualdade/muni_%s/%s/%s/%s/%s_%s_%s_%s.png",
                         sigla_muni, modo, type_acc , oportunidade, sigla_muni, type_acc , sigla_op, paste(time, collapse = '')), 
           dpi = 350, width = width, height = height, units = "cm")
    
    # cma_clev
    
    rm(cma_clev); gc()
    
    # quintil1 <- data_micro_acc %>%
    #   # mutate(total = "total") %>% 
    #   mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
    #          cor = case_when(cor == "pard_am_ing" ~ "Pretos",
    #                          cor == "pretos" ~ "Pretos",
    #                          cor == "brancos" ~ "Brancos")) %>%
    #   mutate(
    #     quintil_renda = ntile(Rend_pc, 10)) %>%
    #   filter(quintil_renda == 10)
    # 
    # ggplot(quintil1, aes(x = Rend_pc, fill = quintil_renda)) +
    #   geom_histogram() +
    #   facet_wrap(~quintil_renda)
    # 
    # teste  <- data_micro_acc %>% filter(Rend_pc >50)
    # summary(teste)
    #   
    # plot_rr_acc <- ggplot(recorte_rr_acc,
    #        aes(y = opp, x = quintil_renda, fill = cor)) + 
    #   geom_col(position = position_dodge(),
    #            width = .7) + 
    #   scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
    #   geom_text(aes(label = scales::label_number(suffix = "K",
    #                                              scale = 1e-3)(opp),
    #                 group = cor),
    #             position = position_dodge(width = .7),
    #             vjust = -0.5, size = 3.5) +
    #   ylab(sprintf("%s\nacessíveis", titulo_leg))+
    #   scale_y_continuous(labels = scales::label_number(suffix = "K",
    #                                                    scale = 1e-3))+
    #   labs(fill = "Cor") +
    #   theme_bar_plots() +
    #   theme(
    #     legend.position = c(0.1,0.85)
      # )
    
    
    
    
    
    # fazer plots
    #adicionar if com escala viridis para o tmi
  #   plot3 <- ggplot()+
  #     # geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
  #     # coord_equal() +
  #     # scale_fill_identity()+
  #     # # nova escala
  #     # new_scale_fill() +
  #     geom_sf(data = st_transform(data_micro_acc_sf, 3857), aes(fill = n), colour = NA, alpha=.7, size = 0)+
  #     viridis::scale_fill_viridis(option = "B"
  #                                 # , limits = c(0, 0.72)
  #                                 # , breaks = c(0.001, 0.35, 0.7)
  #                                 # , labels = c(0, "35", "70%")
  #     ) +
  #     scale_color_manual(values = 'transparent')+
  #     facet_wrap(~V0606, ncol = 2)+
  #     labs(fill = sprintf("%s\nacessíveis", titulo_leg)) +
  #     theme(plot.title = element_text(hjust = 0.5, size = rel(1)))
  #   
  #   if (mode1 == "bike"){
  #     modo <- "bicicleta"
  #   } else if (mode1 == "transit"){
  #     modo <- "transporte_publico"
  #   } else if (mode == "walk"){
  #     modo <- "caminhada"
  #   }
  #   
  #   suppressWarnings(dir.create(sprintf('../data/map_plots_acc/muni_%s/%s/%s/%s/', sigla_munii, modo, type_acc ,oportunidade)))
  #   
  #   
  #   ggsave(plot3, 
  #          file= sprintf("../data/map_plots_acc/muni_%s/%s/%s/%s/%s_%s_%s_%s.png",
  #                        sigla_munii, modo, type_acc , oportunidade, sigla_munii, type_acc , sigla_op, paste(time, collapse = '')), 
  #          dpi = dpi_mapa, width = width, height = height, units = "cm")
  #   
  #   
  #   
  #   
  # }
  # 
  # 
  # 
  # 
  # 
  # 
  # #recorte de renda e cor
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #recorte de genero e cor
  # 
  # 
  # # recorte_cg_acc <- data_micro_acc %>%
  # #   # mutate(total = "total") %>% 
  # #   mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
  # #          cor = case_when(cor == "pard_am_ing" ~ "Pretos",
  # #                          cor == "pretos" ~ "Pretos",
  # #                          cor == "brancos" ~ "Brancos")) %>%
  # #   group_by(V0606, genero) %>% summarise(opp = mean(CMATT60, na.rm = T)) 
  # # 
  # # 
  # # ggplot(recorte_cg_acc,
  # #        aes(y = opp, x = genero, fill = V0606)) + 
  # #   geom_col(position = position_dodge(),
  # #            width = .7) + 
  # #   scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "blue", "grey70")) +
  # #   geom_text(aes(label = scales::label_number(suffix = "K", scale = 1e-3)(opp), group = V0606),
  # #             position = position_dodge(width = .7),
  # #             vjust = -0.5, size = 3.5) +
  # #   ylab("Oportunidades\n Acessíveis")+
  # #   scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3),
  # #                      limits = c(0,500000))+
  # #   labs(fill = "Cor") +
  # #   theme_bar_plots() +
  # #   theme(legend.position = c(0.1,0.9))
  # # 
  # # #teste 
  # # #quintil de renda
  # # 
  # # 
  # # recorte_rr_acc_renda <- data_micro_acc %>%
  # #   # mutate(total = "total") %>% 
  # #   mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
  # #          cor = case_when(cor == "pard_am_ing" ~ "outros",
  # #                          cor == "pretos" ~ "Pretos",
  # #                          cor == "brancos" ~ "Brancos")) %>%
  # #   mutate(
  # #     quintil_renda = ntile(Rend_pc, 5)) %>%
  # #   group_by(V0606, quintil_renda) %>% summarise(opp = mean(CMATT15, na.rm = T),
  # #                                                pop = n())
  # # # drop_na(quintil_acc)
  # # 
  # # teste <- data_micro_acc %>%
  # #   # mutate(total = "total") %>% 
  # #   mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
  # #          cor = case_when(cor == "pard_am_ing" ~ "outros",
  # #                          cor == "pretos" ~ "Pretos",
  # #                          cor == "brancos" ~ "Brancos")) %>%
  # #   # filter(V0606 %in% "Amarela") 
  # #   group_by(V0606) %>%
  # #   mutate(quintil_renda = ntile(Rend_pc, 5)) %>% select(1:10, V0606, CMATT60, quintil_renda)
  # # 
  # # teste2 <- teste %>% group_by(quintil_renda) %>%
  # #   summarise(media = mean(Rend_pc, na.rm = T),
  # #             max = max(Rend_pc))
  # # 
  # # 
  # # 
  # # 
  # # ggplot(teste,
  # #        aes(y = Rend_pc, group = quintil_renda)) + 
  # #   geom_boxplot(position = position_dodge(),
  # #                width = .7) + 
  # #   scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "#d96e0a", "grey70")) +
  # #   geom_text(aes(label = opp, group = V0606),position = position_dodge(width = .7),
  # #             vjust = -0.5, size = 3.5) +
  # #   ylab("Oportunidades\nAcessíveis")+
  # #   scale_y_continuous(labels = scales::label_number(suffix = "M", scale = 1e-6))+
  # #   labs(fill = "Cor") +
  # #   theme_bar_plots() +
  # #   theme(
  # #     legend.position = c(0.1,0.9)
  # #   )
  # # 
  # # 
  # # 
  # # 
  # # ggplot(recorte_rr_acc_renda,
  # #        aes(y = opp, x = quintil_renda, fill = V0606)) + 
  # #   geom_col(position = position_dodge(),
  # #            width = .7) + 
  # #   scale_fill_manual(values = c("#33b099", "#5766cc", "yellow", "#d96e0a", "grey70")) +
  # #   geom_text(aes(label = opp, group = V0606),position = position_dodge(width = .7),
  # #             vjust = -0.5, size = 3.5) +
  # #   ylab("Proporção de\nHabitantes (%)")+
  # #   scale_y_continuous(labels = scales::label_number(suffix = "M\n(Oportunidades)", scale = 1e-6))+
  # #   labs(fill = "Cor") +
  # #   theme_bar_plots() +
  # #   theme(
  # #     legend.position = c(0.1,0.9)
  # #   )
  # # 
  # # 
  # # #recorte de renda e gênero
  # # 
  # # recorte_rg_acc <- data_micro_acc %>%
  # #   # mutate(total = "total") %>% 
  # #   mutate(genero = ifelse(age_sex %like% 'w', "Mulher", "Homem"),
  # #          cor = case_when(cor == "pard_am_ing" ~ "Pretos",
  # #                          cor == "pretos" ~ "Pretos",
  # #                          cor == "brancos" ~ "Brancos")) %>%
  # #   group_by(genero) %>%
  # #   mutate(
  # #     quintil_acc = ntile(CMATT60, 5)) %>%
  # #   ungroup() %>%
  # #   group_by(genero, quintil_acc) %>% summarise(opp = mean(CMATT60)) %>% 
  # #   drop_na(quintil_acc)
  # # 
  # # 
  # # 
  # # 
  # # ggplot(recorte_rg_acc,
  # #        aes(y = opp, x = quintil_acc, fill = genero)) + 
  # #   geom_col(position = position_dodge(),
  # #            width = .7) + 
  # #   scale_fill_manual(values = c("#33b099", "#5766cc")) +
  # #   geom_text(aes(label = scales::label_number(suffix = "K", scale = 1e-3)(opp), group = genero),
  # #             position = position_dodge(width = .7),
  # #             vjust = -0.5, size = 3.5) +
  # #   ylab("oportunidades\nAcessíveis")+
  # #   scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3))+
  # #   labs(fill = "Cor") +
  # #   theme_bar_plots() +
  # #   theme(
  # #     legend.position = c(0.1,0.85)
  # #   )
  }
  
  library("future")
  plan(multisession)
  
  # lista_modos <- c(rep("walk", 14), rep("bike", 14))
  # lista_modos <- c(rep("transit", 14), rep("walk", 14), rep("bike", 14))
  lista_modos <- c(rep("transit", 16), rep("walk", 16), rep("bike", 16))
  
  # lista_oportunidade <- rep(c("empregos",
  #                             rep("matriculas",4),
  #                             rep("escolas", 4),
  #                             rep("saude", 4),
  #                             "lazer"),2)
  # lista_oportunidade <- rep(c("empregos",
  #                             rep("matriculas",4),
  #                             rep("escolas", 4),
  #                             rep("saude", 4),
  #                             "lazer"),3)
  lista_oportunidade <- rep(c("empregos",
                              rep("matriculas",4),
                              rep("escolas", 4),
                              rep("saude", 4),
                              "lazer",
                              "bikes_compartilhadas",
                              "paraciclos"),3)
  
  # lista_siglaop <- rep(c("TT",
  #                        "MT", "MI", "MF", "MM",
  #                        "ET", "EI", "EF", "EM",
  #                        "ST", "SB", "SM", "SA",
  #                        "LZ"), 2)
  # lista_siglaop <- rep(c("TT",
  #                        "MT", "MI", "MF", "MM",
  #                        "ET", "EI", "EF", "EM",
  #                        "ST", "SB", "SM", "SA",
  #                        "LZ"), 3)
  lista_siglaop <- rep(c("TT",
                         "MT", "MI", "MF", "MM",
                         "ET", "EI", "EF", "EM",
                         "ST", "SB", "SM", "SA",
                         "LZ", "BK", "PR"), 3)
  
  # lista_titulo_leg <- rep(c("Empregos",
  #                           rep("Matrículas",4),
  #                           rep("Escolas", 4),
  #                           rep("Eq. de Saúde", 4),
  #                           "Eq. de Lazer"), 2)
  # lista_titulo_leg <- rep(c("Empregos",
  #                           rep("Matrículas",4),
  #                           rep("Escolas", 4),
  #                           rep("Eq. de Saúde", 4),
  #                           "Eq. de Lazer"), 3)
  lista_titulo_leg <- rep(c("Empregos",
                            rep("Matrículas",4),
                            rep("Escolas", 4),
                            rep("Eq. de Saúde", 4),
                            "Eq. de Lazer",
                            "Est. de B. Comp.",
                            "Paraciclos"), 3)
  # lista_tempos <- c(rep(15,14), rep(30, 14))
  # lista_tempos <- c(rep(45, 14), rep(15,14), rep(30, 14))
  lista_tempos <- c(rep(45, 16), rep(15,16), rep(30, 16))
  
  lista_args <- list(lista_modos, lista_oportunidade, lista_siglaop, lista_titulo_leg, lista_tempos)
  
  furrr::future_pwalk(.l = lista_args, .f = mapas_cma_clev,
                      sigla_muni = 'poa',
                      type_acc = "CMA",
                      cols = 1,
                      width = 15,
                      height = 10)
  
  
  

# Aplicacao da funcao para tmi --------------------------------------------

  lista_modos <- c(rep("walk", 9), rep("bike", 9))
  # lista_modos <- c(rep("transit", 9), rep("walk", 9), rep("bike", 9))
  # lista_modos <- c(rep("transit", 11), rep("walk", 11), rep("bike", 11))
  
  lista_oportunidade <- rep(c(
    
    rep("escolas", 4),
    rep("saude", 4),
    "lazer"),2)
  # lista_oportunidade <- rep(c(
  #   
  #   rep("escolas", 4),
  #   rep("saude", 4),
  #   "lazer"),3)
  
  # lista_oportunidade <- rep(c(
  # 
  #   rep("escolas", 4),
  #   rep("saude", 4),
  #   "lazer",
  #   "bikes_compartilhadas",
  #   "paraciclos"),3)
  
  # lista_siglaop <- rep(c(
  #   "ET", "EI", "EF", "EM",
  #   "ST", "SB", "SM", "SA",
  #   "LZ", "BK", "PR"), 3)
  lista_siglaop <- rep(c(
    "ET", "EI", "EF", "EM",
    "ST", "SB", "SM", "SA",
    "LZ"), 2)
  # lista_siglaop <- rep(c(
  #   "ET", "EI", "EF", "EM",
  #   "ST", "SB", "SM", "SA",
  #   "LZ"), 3)
  
  
  lista_titulo_leg <- rep(c(
    rep("Escolas", 4),
    rep("Eq. de Saúde", 4),
    "Eq. de Lazer"), 2)
  # lista_titulo_leg <- rep(c(
  #   rep("Escolas", 4),
  #   rep("Eq. de Saúde", 4),
  #   "Eq. de Lazer"), 3)
  # lista_titulo_leg <- rep(c(
  #   rep("Escolas", 4),
  #   rep("Eq. de Saúde", 4),
  #   "Eq. de Lazer",
  #   "Est. de B. Comp.",
  #   "Paraciclos"), 3)
  lista_tempos <- c(rep(15,9), rep(30, 9))
  # lista_tempos <- c(rep(45, 9), rep(15,9), rep(30, 9))
  # lista_tempos <- c(rep(45, 11), rep(15,11), rep(30, 11))
  
  lista_args <- list(lista_modos, lista_oportunidade, lista_siglaop, lista_titulo_leg, lista_tempos)
  
  seed = TRUE
  
  furrr::future_pwalk(.l = lista_args, .f = mapas_cma_clev,
                      sigla_muni = 'poa',
                      type_acc = "TMI",
                      cols = 1,
                      width = 15,
                      height = 10)
  
  # mapas_cma_clev(sigla_muni = "poa",type_acc = "TMI", mode1 = "walk", oportunidade = "saude",
  #                sigla_op = "ST", titulo_leg = "Saúde", time = 30)
  
  
  
  
  
  
  # 2. Aplica funcao ------------------------------------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=faz_grafico_e_salva)
  
  
  
  
  
  
  
}
