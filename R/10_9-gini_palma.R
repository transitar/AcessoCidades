#gini e palma

rm(list = ls()); gc()

source('./R/fun/setup.R')
sigla_muni <- 'pal'
width <- 15
height <- 10

sigla_muni <- 'pal'
mode1 <- "transit"
type_acc <- "CMA"
ind_selec <- c("CMATT60", "CMAST60", "CMAET60", "CMAMT60", "CMALZ60", "CMABK60", "CMAPR60")

library(showtext)
showtext_auto()
font_add("encode_sans", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-VariableFont_wdth,wght.ttf')
font_add("encode_sans_regular", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Regular.ttf')
font_add("encode_sans_bold", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Bold.ttf')
font_add("encode_sans_light", 'C:/Users/nilso/AppData/Local/Microsoft/Windows/Fonts/EncodeSans-Light.ttf')

desigualdade_function <- function(sigla_muni, mode1, ind_select, type_acc){
  
  message(paste("Rodando",sigla_muni, "\n"))
  
  path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
  
  dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
  
  path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
  
  data_contorno <- read_rds(path_contorno)
  
  maptiles <- read_rds(path_maptiles)
  
  data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                sigla_muni, sigla_muni))
  
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
  
  
  dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
  
  
  if (type_acc == "CMA"){
    
    data_acess <- read_rds(sprintf('../r5r/accessibility/muni_%s/acc_%s.rds',
                                   sigla_muni, sigla_muni)) %>% filter(mode == mode1)
    
    
    dados_acc <- left_join(dados_hex, data_acess, by = c("id_hex"="origin")) %>% st_as_sf()
    
  } else if (type_acc == "TMI") {
    
    data_acess_tmi <- read_rds(sprintf('../r5r/accessibility/muni_%s/tmi/acc_tmi_%s.rds',
                                       sigla_muni, sigla_muni)) %>% filter(mode == mode1)
    
    
    dados_acc <- left_join(dados_hex, data_acess_tmi, by = c("id_hex"="origin")) %>% st_as_sf()
    DT <- dados_acc
    
    #inserir drop na em cada gráfico
    dados_acc_maps <- dados_acc %>% mutate_if(is.numeric, list(~na_if(., Inf)))
    dados_acc <- dados_acc_maps
  }
  
  if (type_acc == "CMA"){
    
    if (mode1 == "transit"){
  names_cols <- names(dados_acc)[which(names(dados_acc) %like% type_acc & names(dados_acc) %like% "45")]
  } else if (mode1 == "bike"){
    names_cols <- names(dados_acc)[which(names(dados_acc) %like% type_acc & names(dados_acc) %like% "30")]
  } else if (mode1 == "walk"){
    names_cols <- names(dados_acc)[which(names(dados_acc) %like% type_acc & names(dados_acc) %like% "15")]
  }
  
  
  } else {
    names_cols <- names(dados_acc)[which(names(dados_acc) %like% type_acc)]
  }
  #gini
  gini_data_frame <<- data.frame(indicador = names_cols, gini = NA)
  
  gini_data_frame <<- gini_data_frame %>% mutate(mode = mode1, acc_ind = type_acc)
  #palma
  palma_data_frame <<- data.frame(indicador = names_cols, palma = NA)
  
  palma_data_frame <<- palma_data_frame %>% mutate(mode = mode1, acc_ind = type_acc)
  
  #dados finais
  acess <- dados_acc %>% select(id_hex, mode, names_cols)
  
  data_micro_acc <- data_micro2 %>% left_join(acess, by = c("hex"= "id_hex"))
  #dados finais usados
  data_micro_acc <- data_micro_acc %>% mutate(percentil = ntile(Rend_pc, 100))
  
  # ind <- "CMATT60"

  gini_func <- function(ind){
    
    gini_values <- data_micro_acc %>% select(ind) %>% st_drop_geometry() %>% drop_na()
    gini_ind <- DescTools::Gini(gini_values[[ind]])
    gini_data_frame$gini[which(gini_data_frame$indicador == ind)] <<- gini_ind
    gini_data_frame$mode[which(gini_data_frame$indicador == ind)] <<- mode1
    
  }
  # mode1 <- "transit"
  walk(.x = names_cols, .f = gini_func)

  gini_selec <<- gini_data_frame %>%
    filter(indicador %in% ind_selec)
  
  teste <- data_micro_acc %>% filter(percentil >= 90) %>% st_as_sf()
  # mapview(teste, zcol ="percentil")
  
  palma_func_i <- function(ind){
    
    palma_values_up <- data_micro_acc %>% filter(percentil >= 90) %>% st_drop_geometry() %>%
      drop_na(ind) %>% select(ind)
    palma_up <- mean(palma_values_up[[ind]], na.rm = T)
    palma_values_down <- data_micro_acc %>% filter(percentil <= 40) %>% st_drop_geometry() %>%
      drop_na(ind) %>% select(ind)
    palma_down <- mean(palma_values_down[[ind]], na.rm = T)
    
    palma_ratio <- palma_up/palma_down
    
    
    palma_data_frame$palma[which(palma_data_frame$indicador == ind)] <<- palma_ratio
    palma_data_frame$mode[which(gini_data_frame$indicador == ind)] <<- mode1
    
  }
  # mode1 <- "transit"
  walk(.x = names_cols, .f = palma_func_i)
  
  palma_selec <<- palma_data_frame %>%
    filter(indicador %in% ind_selec)
  
}


# Aplicação da Função para CMA -----------------------------------------------------



desigualdade_function(sigla_muni = "pal",
              mode1 = "transit",
              ind_select = c("CMATT60", "CMAST60", "CMAET60", "CMAMT60", "CMALZ60", "CMABK60", "CMAPR60"),
              type_acc = "CMA")
gini_temp_transit <- gini_data_frame
gini_temp_transit_selec <- gini_selec
palma_temp_transit <- palma_data_frame
palma_temp_transit_selec <- palma_selec

desigualdade_function(sigla_muni = "pal",
              mode1 = "bike",
              ind_select = c("CMATT15", "CMAST15", "CMAET15", "CMAMT15", "CMALZ15", "CMABK15", "CMAPR15"),
              type_acc = "CMA")
gini_temp_bike <- gini_data_frame
gini_temp_bike_selec <- gini_selec
palma_temp_bike <- palma_data_frame
palma_temp_bike_selec <- palma_selec

desigualdade_function(sigla_muni = "pal",
              mode1 = "walk",
              ind_select = c("CMATT15", "CMAST15", "CMAET15", "CMAMT15", "CMALZ15", "CMABK15", "CMAPR15"),
              type_acc = "CMA")
gini_temp_walk <- gini_data_frame
gini_temp_walk_selec <- gini_selec
palma_temp_walk <- palma_data_frame
palma_temp_walk_selec <- palma_selec


gini_all <- rbind(gini_temp_transit, gini_temp_bike, gini_temp_walk) %>%
  mutate(key = paste(indicador, mode))
gini_all_select <- rbind(gini_temp_transit_selec, gini_temp_bike_selec, gini_temp_walk_selec) %>%
  mutate(key = paste(indicador, mode))



palma_all <- rbind(palma_temp_transit, palma_temp_bike, palma_temp_walk) %>%
  mutate(key = paste(indicador, mode))
palma_all_select <- rbind(palma_temp_transit_selec, palma_temp_bike_selec, palma_temp_walk_selec) %>%
  mutate(key = paste(indicador, mode))

desigualdade_all_cma <- gini_all %>%
  left_join(palma_all %>% select(key, palma), by = "key") %>%
  select(indicador, mode, gini, palma)

desigualdade_select_cma <- gini_all_select %>%
  left_join(palma_all_select %>% select(key, palma), by = "key") %>%
  select(indicador, mode, gini, palma)

#escrita

#indicadores de desigualdade all
suppressWarnings(dir.create(sprintf('../data/ind_desigualdade/muni_%s/',
                                    sigla_muni), recursive = TRUE))

write.xlsx(desigualdade_all_cma, sprintf('../data/ind_desigualdade/muni_%s/muni_%s_desigualdade_cma_all.xlsx',
                                     sigla_muni, sigla_muni))
write.xlsx(desigualdade_select_cma, sprintf('../data/ind_desigualdade/muni_%s/muni_%s_desigualdade_cma_select.xlsx',
                                     sigla_muni, sigla_muni))


# TMI ---------------------------------------------------------------------

desigualdade_function(sigla_muni = "pal",
                      mode1 = "transit",
                      ind_select = c("TMIST", "TMIET", "TMILZ", "TMIBK", "TMIPR"),
                      type_acc = "TMI")
gini_temp_transit <- gini_data_frame
gini_temp_transit_selec <- gini_selec
palma_temp_transit <- palma_data_frame
palma_temp_transit_selec <- palma_selec

sigla_muni <- 'pal'
mode1 <- "bike"
type_acc <- "TMI"
ind_select = c("TMIST", "TMIET", "TMILZ", "TMIBK", "TMIPR")
desigualdade_function(sigla_muni = "poa",
                      mode1 = "bike",
                      ind_select = c("TMIST", "TMIET", "TMILZ", "TMIBK", "TMIPR"),
                      type_acc = "TMI")
gini_temp_bike <- gini_data_frame
gini_temp_bike_selec <- gini_selec
palma_temp_bike <- palma_data_frame
palma_temp_bike_selec <- palma_selec

sigla_muni <- 'pal'
mode1 <- "walk"
type_acc <- "TMI"
ind_select = c("TMIST", "TMIET", "TMILZ", "TMIBK", "TMIPR")

desigualdade_function(sigla_muni = "pal",
                      mode1 = "walk",
                      ind_select = c("TMIST", "TMIET", "TMILZ", "TMIBK", "TMIPR"),
                      type_acc = "TMI")
gini_temp_walk <- gini_data_frame
gini_temp_walk_selec <- gini_selec
palma_temp_walk <- palma_data_frame
palma_temp_walk_selec <- palma_selec


gini_all_tmi <- rbind(gini_temp_transit, gini_temp_bike, gini_temp_walk) %>%
  mutate(key = paste(indicador, mode))
gini_all_select_tmi <- rbind(gini_temp_transit_selec, gini_temp_bike_selec, gini_temp_walk_selec) %>%
  mutate(key = paste(indicador, mode))



palma_all_tmi <- rbind(palma_temp_transit, palma_temp_bike, palma_temp_walk) %>%
  mutate(key = paste(indicador, mode))
palma_all_select_tmi <- rbind(palma_temp_transit_selec, palma_temp_bike_selec, palma_temp_walk_selec) %>%
  mutate(key = paste(indicador, mode))

desigualdade_all_tmi <- gini_all_tmi %>%
  left_join(palma_all_tmi %>% select(key, palma), by = "key") %>%
  select(indicador, mode, gini, palma)

desigualdade_select_tmi <- gini_all_select_tmi %>%
  left_join(palma_all_select_tmi %>% select(key, palma), by = "key") %>%
  select(indicador, mode, gini, palma)

#escrita

#indicadores de desigualdade all
suppressWarnings(dir.create(sprintf('../data/ind_desigualdade/muni_%s/',
                                    sigla_muni), recursive = TRUE))

write.xlsx(desigualdade_all_tmi, sprintf('../data/ind_desigualdade/muni_%s/muni_%s_desigualdade_tmi_all.xlsx',
                                     sigla_muni, sigla_muni))
write.xlsx(desigualdade_all_tmi, sprintf('../data/ind_desigualdade/muni_%s/muni_%s_desigualdade_tmi_select.xlsx',
                                     sigla_muni, sigla_muni))





# Graficos CMA ---------------------------------------------------------------

# mode1 <- "bike"
dados_cma <- desigualdade_all_cma %>% filter(indicador %in% ifelse(mode == "transit", c(
  "CMALZ45",
  "CMAMT45",
  "CMAST45",
  "CMATT45"), ifelse(mode == "bike", c(
    "CMALZ30",
    "CMAMT30",
    "CMAST30",
    "CMATT30"), c(
      "CMALZ15",
      "CMAMT15",
      "CMAST15",
      "CMATT15")))) %>%
  mutate(indicador = substr(indicador, start = 1L, stop = 5L))

dados_cma <- dados_cma %>% mutate(mode = case_when(mode=="transit" ~ "Transporte Público (45 min)",
                                                   mode=="bike" ~ "Bicicleta (30 min)",
                                                   mode=="walk" ~ "Caminhada (15 min)"))
plot_gini_cma <- ggplot(dados_cma,
       aes(y = gini, x = indicador, fill = mode)) +
  geom_col(position = position_dodge(),
           width = .7) +
  scale_y_continuous(
                     breaks = seq(0,1,0.1),
                     limits = c(0,1)) +
  labs(fill = "Modo de Transporte") +
  geom_text(aes(label = scales::label_number(suffix = "",
                                             decimal.mark = "," ,
                                             scale = 1,
                                             accuracy = 0.001)(gini),
                group = mode),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            hjust = 0.5,
            size = 5,
            colour = "grey30",
            # fontface = "bold",
            family = "encode_sans_light",
            angle = 0) +

  # scale_fill_discrete(#values = c("#33b099", "#5766cc", "#d96e0a"),
  #   labels = c("Bicicleta",
  #              "Transporte Publico",
  #              "Caminhada")) +
  scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
  # scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a"),
  #                   labels = c("#33b099" = "Bicicleta",
  #                              "#5766cc" = "Transporte Publico",
  #                              "#d96e0a" = "Caminhada")
  # ) +
    scale_x_discrete(breaks = c(
                                "CMALZ",
                                "CMAMT",
                                "CMAST",
                                "CMATT"),
                     labels = c(
                                "Lazer",
                                "Matrículas Totais",
                                "Eq. de Saúde Totais",
                                "Empregos")) +
  ylab("Índice de Gini\nMedida Cumulativa de Oportunidades") +
  xlab("Oportunidade") +
  theme_light() +
  theme(#axis.title = element_blank(),
    # panel.grid.minor = element_line(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                    colour = "white"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    text = element_text(family = "sans",
                        # face = "bold",
                        size = 14),
    
    plot.title = element_text(size = 12, margin = margin(b=10)),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    legend.title = element_text(size = 18, family = "encode_sans_bold"),
    legend.text = element_text(size = 12, family = "encode_sans_regular"),
    axis.text = element_text(size = 14, family = "encode_sans_light"),
    axis.title = element_text(size = 18, family = "encode_sans_bold"))

ggsave(plot_gini_cma, 
       file= sprintf('../data/ind_desigualdade/muni_%s/7-muni_%s_grafico_desigualdade_cma.png',
                     sigla_muni, sigla_muni), 
       dpi = 200, width = width, height = height, units = "cm")


# Grafico Gini TMI --------------------------------------------------------


# mode1 <- "bike"
dados_tmi <- desigualdade_all_tmi %>%
  filter(indicador %in%  c("TMIST", "TMIET", "TMILZ"))
dados_tmi <- dados_tmi %>% mutate(mode = case_when(mode=="transit" ~ "Transporte Público",
                                                   mode=="bike" ~ "Bicicleta",
                                                   mode=="walk" ~ "Caminhada"))
plot_gini_tmi <- ggplot(dados_tmi,
                        aes(y = gini, x = indicador, fill = mode)) +
  geom_col(position = position_dodge(),
           width = .7) +
  scale_y_continuous(
    breaks = seq(0,1,0.1),
    limits = c(0,1)) +
  labs(fill = "Modo de Transporte") +
  geom_text(aes(label = scales::label_number(suffix = "",
                                             decimal.mark = "," ,
                                             scale = 1,
                                             accuracy = 0.001)(gini),
                group = mode),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            hjust = 0.5,
            size = 5,
            colour = "grey30",
            # fontface = "bold",
            family = "encode_sans_light",
            angle = 0) +
  
  # scale_fill_discrete(#values = c("#33b099", "#5766cc", "#d96e0a"),
  #   labels = c("Bicicleta",
  #              "Transporte Publico",
  #              "Caminhada")) +
  scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
  # scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a"),
  #                   labels = c("#33b099" = "Bicicleta",
  #                              "#5766cc" = "Transporte Publico",
  #                              "#d96e0a" = "Caminhada")
  # ) +
  scale_x_discrete(breaks = c(

    "TMIET",
    "TMILZ",

    "TMIST"),
    labels = c(

      "Escolas",
      "Lazer",

      "Saúde")) +
  ylab("Índice de Gini\nMedida de Tempo Mínimo de Acesso") +
  xlab("Oportunidade") +
  theme_light() +
  theme(#axis.title = element_blank(),
    # panel.grid.minor = element_line(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                    colour = "white"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    text = element_text(family = "sans",
                        # face = "bold",
                        size = 14),
    
    plot.title = element_text(size = 12, margin = margin(b=10)),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    legend.title = element_text(size = 18, family = "encode_sans_bold"),
    legend.text = element_text(size = 14, family = "encode_sans_regular"),
    axis.text = element_text(size = 14, family = "encode_sans_light"),
    axis.title = element_text(size = 18, family = "encode_sans_bold"))

ggsave(plot_gini_tmi, 
       file= sprintf('../data/ind_desigualdade/muni_%s/8-muni_%s_grafico_desigualdade_tmi.png',
                     sigla_muni, sigla_muni), 
       dpi = 200, width = width, height = height, units = "cm")



# Grafico de Razao de Palma - CMA -----------------------------------------

plot_palma_cma <- ggplot(dados_cma,
                         aes(y = palma, x = indicador, fill = mode)) +
  geom_col(position = position_dodge(),
           width = .7) +
  geom_hline(yintercept = 1, linetype =2, color = "grey70", linewidth = 1.2)+
  scale_y_continuous(
    breaks = seq(0,3,0.25),
    limits = c(0,3)) +
  labs(fill = "Modo de Transporte") +
  geom_text(aes(label = scales::label_number(suffix = "",
                                             decimal.mark = "," ,
                                             scale = 1,
                                             accuracy = 0.001)(palma),
                group = mode),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            hjust = 0.5,
            size = 5,
            colour = "black",
            fontface = "bold",
            angle = 0) +
  # )+
  scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
  # scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a"),
  #                   labels = c("#33b099" = "Bicicleta",
  #                              "#5766cc" = "Transporte Publico",
  #                              "#d96e0a" = "Caminhada")
  # ) +
  scale_x_discrete(breaks = c(
    # "TMIBK",
    "CMALZ",
    "CMAMT",
    # "TMIPR",
    "CMAST",
    "CMATT"),
    labels = c(
      # "Bicicletas\nCompartilhadas",
      "Lazer",
      "Matrículas",
      # "Paraciclos",
      "Saúde",
      "Empregos")) +
  ylab("Razão de Palma\nMedida de Tempo Mínimo de Acesso") +
  xlab("Oportunidade") +
  theme_light() +
  theme(#axis.title = element_blank(),
    # panel.grid.minor = element_line(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                    colour = "white"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    text = element_text(family = "sans",
                        # face = "bold",
                        size = 14),
    
    plot.title = element_text(size = 12, margin = margin(b=10)),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    legend.title = element_text(size = 18, family = "encode_sans_bold"),
    legend.text = element_text(size = 14, family = "encode_sans_regular"),
    axis.text = element_text(size = 14, family = "encode_sans_light"),
    axis.title = element_text(size = 18, family = "encode_sans_bold"))

ggsave(plot_palma_cma, 
       file= sprintf('../data/ind_desigualdade/muni_%s/9-muni_%s_grafico_razao_palma_cma_select.png',
                     sigla_muni, sigla_muni), 
       dpi = 200, width = width, height = height, units = "cm")

# Grafico Palma TMI --------------------------------------------------------
#

# mode1 <- "bike"
# dados_tmi <- desigualdade_all_tmi %>%
#   filter(indicador %in%  c("TMIST", "TMIET", "TMILZ", "TMIBK", "TMIPR"))
# dados_tmi <- dados_tmi %>% mutate(mode = case_when(mode=="transit" ~ "Transporte Público",
#                                                    mode=="bike" ~ "Bicicleta",
#                                                    mode=="walk" ~ "Caminhada"))
plot_palma_tmi <- ggplot(dados_tmi,
                        aes(y = palma, x = indicador, fill = mode)) +
  geom_col(position = position_dodge(),
           width = .7) +
  geom_hline(yintercept = 1, linetype =2, color = "grey70", linewidth = 1.2)+
  scale_y_continuous(
    breaks = seq(0,2,0.25),
    limits = c(0,2)) +
  labs(fill = "Modo de Transporte") +
      geom_text(aes(label = scales::label_number(suffix = "",
                                                 decimal.mark = "," ,
                                                 scale = 1,
                                                 accuracy = 0.001)(palma),
                    group = mode),
                position = position_dodge(width = 0.7),
                vjust = -0.5,
                hjust = 0.5,
                size = 6,
                colour = "black",
                fontface = "bold",
                angle = 0) +
      # )+
  scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
  # scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a"),
  #                   labels = c("#33b099" = "Bicicleta",
  #                              "#5766cc" = "Transporte Publico",
  #                              "#d96e0a" = "Caminhada")
  # ) +
  scale_x_discrete(breaks = c(
    # "TMIBK",
    "TMIET",
    "TMILZ",
    # "TMIPR",
    "TMIST"),
    labels = c(
      # "Bicicletas\nCompartilhadas",
      "Escolas",
      "Lazer",
      # "Paraciclos",
      "Saúde")) +
  ylab("Razão de Palma\nMedida de Tempo Mínimo de Acesso") +
  xlab("Oportunidade") +
  theme_light() +
  theme(#axis.title = element_blank(),
    # panel.grid.minor = element_line(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                    colour = "white"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    text = element_text(family = "sans",
                        # face = "bold",
                        size = 14),
    
    plot.title = element_text(size = 12, margin = margin(b=10)),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    legend.title = element_text(size = 18, family = "encode_sans_bold"),
    legend.text = element_text(size = 14, family = "encode_sans_regular"),
    axis.text = element_text(size = 14, family = "encode_sans_light"),
    axis.title = element_text(size = 18, family = "encode_sans_bold"))

ggsave(plot_palma_tmi, 
       file= sprintf('../data/ind_desigualdade/muni_%s/muni_%s_grafico_razao_palma_tmi_select.png',
                     sigla_muni, sigla_muni), 
       dpi = 200, width = width, height = height, units = "cm")



# Médias de Acessibilidade ------------------------------------------------



# Razão de Medias por Cor CMA - Valores ----------------------------------------------------------


razao_medias_function_cor <- function(sigla_muni, type_acc){
  
  message(paste("Rodando",sigla_muni, "\n"))
  
  path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
  
  dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
  
  path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni)
  
  data_contorno <- read_rds(path_contorno)
  
  maptiles <- read_rds(path_maptiles)
  
  data_micro <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                 sigla_muni, sigla_muni))
  
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
  
  
  dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
  
  modos <- c("transit", "walk", "bike")
  
  # i <- 'transit'
  
  razoes_data_frame <<- data.frame(indicador = NA,
                                       media = NA,
                                       modo = NA,
                                       acc_ind = NA,
                                       recorte = NA)
  # i <- "transit"
  for (i in modos){
  
  if (type_acc == "CMA"){
    
    data_acess <- read_rds(sprintf('../r5r/accessibility/muni_%s/acc_%s.rds',
                                   sigla_muni, sigla_muni)) %>% filter(mode == i)
    
    
    dados_acc <- left_join(dados_hex, data_acess, by = c("id_hex"="origin")) %>% st_as_sf()
    
  } else if (type_acc == "TMI") {
    
    data_acess_tmi <- read_rds(sprintf('../r5r/accessibility/muni_%s/tmi/acc_tmi_%s.rds',
                                       sigla_muni, sigla_muni)) %>% filter(mode == i)
    
    
    dados_acc <- left_join(dados_hex, data_acess_tmi, by = c("id_hex"="origin")) %>% st_as_sf()
    DT <- dados_acc
    
    #inserir drop na em cada gráfico
    dados_acc_maps <- dados_acc %>% mutate_if(is.numeric, list(~na_if(., Inf)))
    dados_acc <- dados_acc_maps
  }
  
    if (type_acc == "CMA"){
    if (i == "transit"){
  names_cols <- names(dados_acc)[which(names(dados_acc) %like% type_acc & names(dados_acc) %like% "45")]
    } else if (i == "bike"){
      names_cols <- names(dados_acc)[which(names(dados_acc) %like% type_acc & names(dados_acc) %like% "30")]
    } else if (i == "walk"){
      names_cols <- names(dados_acc)[which(names(dados_acc) %like% type_acc & names(dados_acc) %like% "15")]
    }
      } else {
      names_cols <- names(dados_acc)[which(names(dados_acc) %like% type_acc)]
    }
    
    
    
  data_micro3 <- data_micro2 %>%
    mutate(quintil_renda = ntile(Rend_pc, 4)) %>%
    # filter(quintil_renda %in% c(1,2,3)) %>%
    # filter(hex %in% id_hex_intersects_bus_ntad300) %>%
    # st_drop_geometry() %>%
    # mutate(total = "total") %>% 
    mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
           cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                           cor == "pretos" ~ "Pretos",
                           cor == "brancos" ~ "Brancos"))
  
  data_micro_acc <- data_micro3 %>%
    # st_drop_geometry() %>%
    left_join(dados_acc, by = c("hex"="id_hex"))
    
  

  #gini
  medias_cor_data_frame <- data.frame(indicador = names_cols,
                                       media = NA,
                                       modo = i,
                                       acc_ind = type_acc,
                                       recorte = "cor")
  
  medias_cor_resp_data_frame <- data.frame(indicador = names_cols,
                                            media = NA,
                                            modo = i,
                                            acc_ind = type_acc,
                                            recorte = "responsavel")

  medias_genero_data_frame <- data.frame(indicador = names_cols,
                                            media = NA,
                                            modo = i,
                                            acc_ind = type_acc,
                                          recorte = "genero")
  
  
  #palma
  # palma_data_frame <<- data.frame(indicador = names_cols, palma = NA)
  # 
  # palma_data_frame <<- palma_data_frame %>% mutate(mode = mode1, acc_ind = type_acc)
  
  #dados finais
  acess <- dados_acc %>% select(id_hex, mode, names_cols)
  
  data_micro_acc <- data_micro3 %>% left_join(acess, by = c("hex"= "id_hex"))
  #dados finais usados
  data_micro_acc <- data_micro_acc %>% mutate(percentil = ntile(Rend_pc, 100))
  
  # ind <- "CMATT60"
  # ind <- "TMILZ"
  
  cor_func <- function(ind){
    
    dados_brancos <- data_micro_acc %>% filter(cor == "Brancos") %>% 
      # st_drop_geometry() %>% 
      drop_na(ind) %>%
      select(ind)
    # dados_brancos <- dados_brancos[ind]
    media_brancos <- mean(dados_brancos[[ind]], na.rm = T)
    
    dados_pretos <- data_micro_acc %>% filter(cor == "Pretos") %>% 
      # st_drop_geometry() %>% 
      drop_na(ind) %>%
      select(ind)
    # dados_brancos <- dados_brancos[ind]
    media_pretos <- mean(dados_pretos[[ind]], na.rm = T)
    
    razao_medias <- media_brancos/media_pretos
    
    
    medias_cor_data_frame$media[which(medias_cor_data_frame$indicador == ind)] <<- razao_medias
    # gini_data_frame$mode[which(gini_data_frame$indicador == ind)] <<- mode1
    
  }
  walk(.x = names_cols, .f = cor_func)
  # 
  # head(medias_cor_data_frame)
  # 
  # head(medias_genero_data_frame)

  genero_func <- function(ind){

    dados_homens <- data_micro_acc %>% filter(genero == "Homens") %>%
      # st_drop_geometry() %>%
      drop_na(ind) %>%
      select(ind)
    # dados_brancos <- dados_brancos[ind]
    media_homens <- mean(dados_homens[[ind]], na.rm = T)

    dados_mulheres <- data_micro_acc %>% filter(genero == "Mulheres") %>%
      # st_drop_geometry() %>%
      drop_na(ind) %>%
      select(ind)
    # dados_brancos <- dados_brancos[ind]
    media_mulheres <- mean(dados_mulheres[[ind]], na.rm = T)

    razao_medias <- media_homens/media_mulheres

    medias_genero_data_frame$media[which(medias_genero_data_frame$indicador == ind)] <<- razao_medias
    # gini_data_frame$mode[which(gini_data_frame$indicador == ind)] <<- mode1

  }
  # 
  walk(.x = names_cols, .f = genero_func)
  # 
  # head(medias_cor_data_frame)
  # 
  # head(medias_genero_data_frame)

  genero_resp_func <- function(ind){

    dados_homens <- data_micro_acc %>% filter(genero == "Homens") %>%
      st_drop_geometry() %>%
      drop_na(ind) %>%
      select(ind)
    # dados_brancos <- dados_brancos[ind]
    media_homens <- mean(dados_homens[[ind]], na.rm = T)

    dados_mulheres <- data_micro_acc %>% filter(genero == "Mulheres") %>%
      st_drop_geometry() %>%
      drop_na(ind) %>%
      select(ind)
    # dados_brancos <- dados_brancos[ind]
    media_mulheres <- mean(dados_mulheres[[ind]], na.rm = T)

    razao_medias <- media_homens/media_mulheres


    medias_cor_resp_data_frame$media[which(medias_cor_resp_data_frame$indicador == ind)] <<- razao_medias
    # gini_data_frame$mode[which(gini_data_frame$indicador == ind)] <<- mode1

  }
  # 
  walk(.x = names_cols, .f = genero_resp_func)
  
  # medias_modo <- medias_cor_data_frame
  medias_modo <<- rbind(medias_cor_data_frame, medias_cor_resp_data_frame, medias_genero_data_frame)
  
  # palma_selec <<- palma_data_frame %>%
  #   filter(indicador %in% ind_selec)
  
  razoes_data_frame <<- rbind(razoes_data_frame, medias_modo)
  
  
  
  }
  
  # razoes_data_frame <<- razoes_data_frame %>% drop_na()
  
  return(razoes_data_frame)
  
}

medias_razoes_acc <- razao_medias_function_cor(sigla_muni = "pal", "CMA")
# medias_razoes_acc_cor <- razao_medias_function_cor(sigla_muni = "pal", "CMA")
medias_razoes_acc <- medias_razoes_acc_cor %>% drop_na()
# head(medias_razoes_acc_cor)


# Gráfico de Razao de medias recorte de cor -------------------------------
medias_razoes_acc_cor2 <- medias_razoes_acc %>%
  filter(recorte == "cor") %>%
  filter(indicador %in% c("CMATT45", "CMATT30", "CMATT15")) %>%
  mutate(modo = case_when(modo == "transit" ~ "Transporte Público",
                          modo == "bike" ~ "Bicicleta",
                          modo == "walk" ~ "Caminhada"))

plot_razoes_cma_cor <- ggplot(medias_razoes_acc_cor2,
                         aes(y = media, x = indicador, fill = modo)) +
  geom_col(position = position_dodge(),
           width = .7) +
  geom_hline(yintercept = 1, linetype =2, color = "grey70", linewidth = 1.2)+
  scale_y_continuous(
    breaks = seq(0,1.5,0.25),
    limits = c(0,1.5)) +
  labs(fill = "Modo de Transporte") +
  geom_text(aes(label = scales::label_number(suffix = "",
                                             decimal.mark = "," ,
                                             scale = 1,
                                             accuracy = 0.001)(media),
                group = modo),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            hjust = 0.5,
            size = 5,
            colour = "black",
            fontface = "bold",
            angle = 0) +
  # )+
  scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
  # scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a"),
  #                   labels = c("#33b099" = "Bicicleta",
  #                              "#5766cc" = "Transporte Publico",
  #                              "#d96e0a" = "Caminhada")
  # ) +
  scale_x_discrete(breaks = c(
    # "TMIBK",
    "CMATT15",
    "CMATT30",
    # "TMIPR",
    # "CMATT45",
    "CMATT45"),
    labels = c(
      # "Bicicletas\nCompartilhadas",
      "Empregos (15 min)",
      "Empregos (30 min)",
      # "Paraciclos",
      # "Saúde",
      "Empregos (45 min)")) +
  ylab("Razão de Médias de\nAcessibilidade CMA de brancos / negros") +
  xlab("Oportunidade") +
  theme_light() +
  theme(#axis.title = element_blank(),
    # panel.grid.minor = element_line(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                    colour = "white"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    text = element_text(family = "sans",
                        # face = "bold",
                        size = 14),
    
    plot.title = element_text(size = 12, margin = margin(b=10)),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    legend.title = element_text(size = 18, family = "encode_sans_bold"),
    legend.text = element_text(size = 14, family = "encode_sans_regular"),
    axis.text = element_text(size = 14, family = "encode_sans_light"),
    axis.title = element_text(size = 18, family = "encode_sans_bold"))

ggsave(plot_razoes_cma_cor, 
       file= sprintf('../data/ind_desigualdade/muni_%s/1-muni_%s_grafico_razoes_cor_cma.png',
                     sigla_muni, sigla_muni), 
       dpi = 200, width = width, height = height, units = "cm")


# Gráfico de Razao de medias recorte de genero ----------------------------

medias_razoes_acc_gen2 <- medias_razoes_acc %>%
  filter(recorte == "genero") %>%
  filter(indicador %in% c("CMATT45", "CMATT30", "CMATT15")) %>%
  mutate(modo = case_when(modo == "transit" ~ "Transporte Público",
                          modo == "bike" ~ "Bicicleta",
                          modo == "walk" ~ "Caminhada"))

plot_razoes_cma_gen <- ggplot(medias_razoes_acc_gen2,
                              aes(y = media, x = indicador, fill = modo)) +
  geom_col(position = position_dodge(),
           width = .7) +
  geom_hline(yintercept = 1, linetype =2, color = "grey70", linewidth = 1.2)+
  scale_y_continuous(
    breaks = seq(0,1.5,0.25),
    limits = c(0,1.5)) +
  labs(fill = "Modo de Transporte") +
  geom_text(aes(label = scales::label_number(suffix = "",
                                             decimal.mark = "," ,
                                             scale = 1,
                                             accuracy = 0.001)(media),
                group = modo),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            hjust = 0.5,
            size = 5,
            colour = "black",
            fontface = "bold",
            angle = 0) +
  # )+
  scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
  # scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a"),
  #                   labels = c("#33b099" = "Bicicleta",
  #                              "#5766cc" = "Transporte Publico",
  #                              "#d96e0a" = "Caminhada")
  # ) +
  scale_x_discrete(breaks = c(
    # "TMIBK",
    "CMATT15",
    "CMATT30",
    # "TMIPR",
    # "CMATT45",
    "CMATT45"),
    labels = c(
      # "Bicicletas\nCompartilhadas",
      "Empregos (15 min)",
      "Empregos (30 min)",
      # "Paraciclos",
      # "Saúde",
      "Empregos (45 min)")) +
  ylab("Razão de Médias de\nAcessibilidade CMA de homens / mulheres") +
  xlab("Oportunidade") +
  theme_light() +
  theme(#axis.title = element_blank(),
    # panel.grid.minor = element_line(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                    colour = "white"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    text = element_text(family = "sans",
                        # face = "bold",
                        size = 14),
    
    plot.title = element_text(size = 12, margin = margin(b=10)),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    legend.title = element_text(size = 18, family = "encode_sans_bold"),
    legend.text = element_text(size = 14, family = "encode_sans_regular"),
    axis.text = element_text(size = 14, family = "encode_sans_light"),
    axis.title = element_text(size = 18, family = "encode_sans_bold"))

ggsave(plot_razoes_cma_gen, 
       file= sprintf('../data/ind_desigualdade/muni_%s/2-muni_%s_grafico_razoes_genero_cma.png',
                     sigla_muni, sigla_muni), 
       dpi = 200, width = width, height = height, units = "cm")



# Gráfico de Razao de medias recorte de responsavel -------------------------------

medias_razoes_acc_resp2 <- medias_razoes_acc %>%
  filter(recorte == "responsavel") %>%
  filter(indicador %in% c("CMATT45", "CMATT30", "CMATT15")) %>%
  mutate(modo = case_when(modo == "transit" ~ "Transporte Público",
                          modo == "bike" ~ "Bicicleta",
                          modo == "walk" ~ "Caminhada"))

plot_razoes_cma_resp <- ggplot(medias_razoes_acc_resp2,
                              aes(y = media, x = indicador, fill = modo)) +
  geom_col(position = position_dodge(),
           width = .7) +
  geom_hline(yintercept = 1, linetype =2, color = "grey70", linewidth = 1.2)+
  scale_y_continuous(
    breaks = seq(0,1.5,0.25),
    limits = c(0,1.5)) +
  labs(fill = "Modo de Transporte") +
  geom_text(aes(label = scales::label_number(suffix = "",
                                             decimal.mark = "," ,
                                             scale = 1,
                                             accuracy = 0.001)(media),
                group = modo),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            hjust = 0.5,
            size = 5,
            colour = "black",
            fontface = "bold",
            angle = 0) +
  # )+
  scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
  # scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a"),
  #                   labels = c("#33b099" = "Bicicleta",
  #                              "#5766cc" = "Transporte Publico",
  #                              "#d96e0a" = "Caminhada")
  # ) +
  scale_x_discrete(breaks = c(
    # "TMIBK",
    "CMATT15",
    "CMATT30",
    # "TMIPR",
    # "CMATT45",
    "CMATT45"),
    labels = c(
      # "Bicicletas\nCompartilhadas",
      "Empregos (15 min)",
      "Empregos (30 min)",
      # "Paraciclos",
      # "Saúde",
      "Empregos (45 min)")) +
  ylab("Razão de médias de\nacessibilidade CMA de eesponsavéis homens / mulheres") +
  xlab("Oportunidade") +
  theme_light() +
  theme(#axis.title = element_blank(),
    # panel.grid.minor = element_line(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                    colour = "white"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    text = element_text(family = "sans",
                        # face = "bold",
                        size = 14),
    
    plot.title = element_text(size = 12, margin = margin(b=10)),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    legend.title = element_text(size = 18, family = "encode_sans_bold"),
    legend.text = element_text(size = 14, family = "encode_sans_regular"),
    axis.text = element_text(size = 14, family = "encode_sans_light"),
    axis.title = element_text(size = 18, family = "encode_sans_bold"))

ggsave(plot_razoes_cma_resp, 
       file= sprintf('../data/ind_desigualdade/muni_%s/3-muni_%s_grafico_razoes_responsavel_cma.png',
                     sigla_muni, sigla_muni), 
       dpi = 200, width = width, height = height, units = "cm")







# Dados Razão de Medias por Cor TMI ---------------------------------------------

medias_razoes_acc_tmi <- razao_medias_function_cor(sigla_muni = "pal", "TMI")
medias_razoes_acc_tmi2 <- medias_razoes_acc_tmi %>% drop_na()
# head(medias_razoes_acc_cor)





# Gráfico de Razao de medias TMI recorte de cor ---------------------------



medias_razoes_acc_cor_tmi2 <- medias_razoes_acc_tmi %>%
  filter(recorte == "cor") %>%
  filter(indicador %in% c("TMIST", "TMIET", "TMILZ")) %>%
  mutate(modo = case_when(modo == "transit" ~ "Transporte Público",
                          modo == "bike" ~ "Bicicleta",
                          modo == "walk" ~ "Caminhada"))


plot_razoes_cor_tmi <- ggplot(medias_razoes_acc_cor_tmi2,
                              aes(y = media, x = indicador, fill = modo)) +
  geom_col(position = position_dodge(),
           width = .7) +
  geom_hline(yintercept = 1, linetype =2, color = "grey70", linewidth = 1.2)+
  scale_y_continuous(
    breaks = seq(0,1.5,0.25),
    limits = c(0,1.5)) +
  labs(fill = "Modo de Transporte") +
  geom_text(aes(label = scales::label_number(suffix = "",
                                             decimal.mark = "," ,
                                             scale = 1,
                                             accuracy = 0.001)(media),
                group = modo),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            hjust = 0.5,
            size = 5,
            colour = "black",
            fontface = "bold",
            angle = 0) +
  # )+
  scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
  # scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a"),
  #                   labels = c("#33b099" = "Bicicleta",
  #                              "#5766cc" = "Transporte Publico",
  #                              "#d96e0a" = "Caminhada")
  # ) +
  scale_x_discrete(breaks = c(
    # "TMIBK",
    "TMIET",
    "TMILZ",
    # "TMIPR",
    # "CMATT45",
    "TMIST"),
    labels = c(
      # "Bicicletas\nCompartilhadas",
      "Escolas",
      "Eq. de lazer",
      # "Paraciclos",
      # "Saúde",
      "Eq. de saúde")) +
  ylab("Razão de Médias de\nTMI de brancos / negros") +
  xlab("Oportunidade") +
  theme_light() +
  theme(#axis.title = element_blank(),
    # panel.grid.minor = element_line(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                    colour = "white"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    text = element_text(family = "sans",
                        # face = "bold",
                        size = 14),
    
    plot.title = element_text(size = 12, margin = margin(b=10)),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    legend.title = element_text(size = 18, family = "encode_sans_bold"),
    legend.text = element_text(size = 14, family = "encode_sans_regular"),
    axis.text = element_text(size = 14, family = "encode_sans_light"),
    axis.title = element_text(size = 18, family = "encode_sans_bold"))

ggsave(plot_razoes_cor_tmi, 
       file= sprintf('../data/ind_desigualdade/muni_%s/4-muni_%s_grafico_razoes_cor_tmi.png',
                     sigla_muni, sigla_muni), 
       dpi = 200, width = width, height = height, units = "cm")


# Gráfico de Razao de medias TMI recorte de genero -------------------------

medias_razoes_acc_genero_tmi2 <- medias_razoes_acc_tmi %>%
  filter(recorte == "genero") %>%
  filter(indicador %in% c("TMIST", "TMIET", "TMILZ")) %>%
  mutate(modo = case_when(modo == "transit" ~ "Transporte Público",
                          modo == "bike" ~ "Bicicleta",
                          modo == "walk" ~ "Caminhada"))


plot_razoes_genero_tmi <- ggplot(medias_razoes_acc_genero_tmi2,
                              aes(y = media, x = indicador, fill = modo)) +
  geom_col(position = position_dodge(),
           width = .7) +
  geom_hline(yintercept = 1, linetype =2, color = "grey70", linewidth = 1.2)+
  scale_y_continuous(
    breaks = seq(0,1.5,0.25),
    limits = c(0,1.5)) +
  labs(fill = "Modo de Transporte") +
  geom_text(aes(label = scales::label_number(suffix = "",
                                             decimal.mark = "," ,
                                             scale = 1,
                                             accuracy = 0.001)(media),
                group = modo),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            hjust = 0.5,
            size = 5,
            colour = "black",
            fontface = "bold",
            angle = 0) +
  # )+
  scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
  # scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a"),
  #                   labels = c("#33b099" = "Bicicleta",
  #                              "#5766cc" = "Transporte Publico",
  #                              "#d96e0a" = "Caminhada")
  # ) +
  scale_x_discrete(breaks = c(
    # "TMIBK",
    "TMIET",
    "TMILZ",
    # "TMIPR",
    # "CMATT45",
    "TMIST"),
    labels = c(
      # "Bicicletas\nCompartilhadas",
      "Escolas",
      "Eq. de lazer",
      # "Paraciclos",
      # "Saúde",
      "Eq. de saúde")) +
  ylab("Razão de Médias de\nTMI de homens / mulheres") +
  xlab("Oportunidade") +
  theme_light() +
  theme(#axis.title = element_blank(),
    # panel.grid.minor = element_line(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                    colour = "white"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    text = element_text(family = "sans",
                        # face = "bold",
                        size = 14),
    
    plot.title = element_text(size = 12, margin = margin(b=10)),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    legend.title = element_text(size = 18, family = "encode_sans_bold"),
    legend.text = element_text(size = 14, family = "encode_sans_regular"),
    axis.text = element_text(size = 14, family = "encode_sans_light"),
    axis.title = element_text(size = 18, family = "encode_sans_bold"))

ggsave(plot_razoes_genero_tmi, 
       file= sprintf('../data/ind_desigualdade/muni_%s/5-muni_%s_grafico_razoes_genero_tmi.png',
                     sigla_muni, sigla_muni), 
       dpi = 200, width = width, height = height, units = "cm")


# Gráfico de Razao de medias TMI recorte de genero do Responsável ---------


medias_razoes_acc_resp_tmi2 <- medias_razoes_acc_tmi %>%
  filter(recorte == "responsavel") %>%
  filter(indicador %in% c("TMIST", "TMIET", "TMILZ")) %>%
  mutate(modo = case_when(modo == "transit" ~ "Transporte Público",
                          modo == "bike" ~ "Bicicleta",
                          modo == "walk" ~ "Caminhada"))


plot_razoes_resp_tmi <- ggplot(medias_razoes_acc_resp_tmi2,
                                 aes(y = media, x = indicador, fill = modo)) +
  geom_col(position = position_dodge(),
           width = .7) +
  geom_hline(yintercept = 1, linetype =2, color = "grey70", linewidth = 1.2)+
  scale_y_continuous(
    breaks = seq(0,1.5,0.25),
    limits = c(0,1.5)) +
  labs(fill = "Modo de Transporte") +
  geom_text(aes(label = scales::label_number(suffix = "",
                                             decimal.mark = "," ,
                                             scale = 1,
                                             accuracy = 0.001)(media),
                group = modo),
            position = position_dodge(width = 0.7),
            vjust = -0.5,
            hjust = 0.5,
            size = 5,
            colour = "black",
            fontface = "bold",
            angle = 0) +
  # )+
  scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a")) +
  # scale_fill_manual(values = c("#33b099", "#5766cc", "#d96e0a"),
  #                   labels = c("#33b099" = "Bicicleta",
  #                              "#5766cc" = "Transporte Publico",
  #                              "#d96e0a" = "Caminhada")
  # ) +
  scale_x_discrete(breaks = c(
    # "TMIBK",
    "TMIET",
    "TMILZ",
    # "TMIPR",
    # "CMATT45",
    "TMIST"),
    labels = c(
      # "Bicicletas\nCompartilhadas",
      "Escolas",
      "Eq. de lazer",
      # "Paraciclos",
      # "Saúde",
      "Eq. de saúde")) +
  ylab("Razão de Médias de\nTMI de responsáveis homens / mulheres") +
  xlab("Oportunidade") +
  theme_light() +
  theme(#axis.title = element_blank(),
    # panel.grid.minor = element_line(),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid",
                                    colour = "white"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    text = element_text(family = "sans",
                        # face = "bold",
                        size = 14),
    
    plot.title = element_text(size = 12, margin = margin(b=10)),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0),
    legend.title = element_text(size = 18, family = "encode_sans_bold"),
    legend.text = element_text(size = 14, family = "encode_sans_regular"),
    axis.text = element_text(size = 14, family = "encode_sans_light"),
    axis.title = element_text(size = 18, family = "encode_sans_bold"))

ggsave(plot_razoes_resp_tmi, 
       file= sprintf('../data/ind_desigualdade/muni_%s/6-muni_%s_grafico_razoes_genero_responsavel_tmi.png',
                     sigla_muni, sigla_muni), 
       dpi = 200, width = width, height = height, units = "cm")



