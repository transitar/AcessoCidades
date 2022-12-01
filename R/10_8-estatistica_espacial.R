rm(list = ls());gc()

source('./R/fun/setup.R')

# novo hamburgo 

sf_use_s2(use_s2 = FALSE)

width <- 16.5
height = 16.5

sigla_muni <- "poa"

# manaus coord_sf(xlim = c(-6700693,-6654021), ylim = c(-354102,-325873), expand = FALSE)

faz_lisa <- function(sigla_muni){

setor_muni <- read_rds(sprintf('../data-raw/setores_censitarios/2019/setores_%s_2019.rds',sigla_muni)) %>% 
  mutate(area = st_area(.)) %>% 
  select(code_tract,area) %>% 
  mutate(code_tract = as.character(code_tract))

setor_muni2 <- read_rds(sprintf('../data-raw/censo_2021_info_muni/muni_%s.rds',sigla_muni)) %>%
  select(code_tract = Cod_setor, Situacao_setor, Tipo_setor ) %>% 
  mutate(code_tract = as.character(code_tract))

setor_muni <- left_join(setor_muni, setor_muni2, by ="code_tract")

# slice(1:100)

mapview(setor_muni)

variaveis_muni <- read_rds(sprintf('../data-raw/censo_2021_info_muni_treated_v2/muni_%s.rds',sigla_muni)) %>% 
  mutate(Cod_setor = as.character(Cod_setor))

setor_var <- setor_muni %>% 
  left_join(variaveis_muni, by = c("code_tract"="Cod_setor")) %>% st_as_sf()

mapview(setor_var, zcol = "P001")

hex_muni <- read_rds(sprintf('../data/hex_municipio/hex_2019_%s_09.rds',sigla_muni)) #%>% 
# slice(1:100)

mapview(hex_muni)

intersect <- st_intersection(setor_var,hex_muni) %>% mutate(area_inter = st_area(.)) %>% 
  mutate(prop = as.numeric(area_inter/area)) %>% 
  mutate(across(starts_with("P0"),~ .x*prop)) %>% 
  st_set_geometry(NULL) %>% 
  group_by(id_hex) %>% 
  summarise(across(starts_with("P0"),~ round(sum(.x,na.rm = T),digits = 0)))


# 
# teste <-  st_intersection(setor_var,hex_muni) %>% mutate(area_inter = st_area(.)) %>% 
#   mutate(prop = as.numeric(area_inter/area)) %>% filter(id_hex == "898044582d7ffff")

(sum(intersect$P011,na.rm = T) - sum(setor_var$P011,na.rm = T)) /sum(setor_var$P011,na.rm = T)

intersect_geom <- hex_muni %>% left_join(intersect, by = "id_hex") 

data_complete <- intersect_geom %>% 
  mutate(area = st_area(.)/10^6) %>%
  rowwise() %>% 
  mutate(Ptot_mulheres = sum(P006,P007,P008,P009,P010),
         Ptot_homens = sum(P001,P002,P003,P004,P005)) %>% 
  mutate(Ptot = Ptot_homens + Ptot_mulheres) %>% 
  filter(Ptot_mulheres > 1) %>%
  filter(Ptot_homens > 1) %>% 
  mutate(renda_per_capita= P011/Ptot)

data_complete2 <- data_complete %>%
  mutate(Ptot_brancos = sum(P001, P006),
         Ptot_pretos = sum(P002, P004, P007, P009)) %>%
  mutate(dif_brancos_pretos = Ptot_brancos - Ptot_pretos,
         dif_homens_mulheres = Ptot_homens - Ptot_mulheres)

# mapview(data_complete2, zcol = 'dif_brancos_pretos')


#lisa map

library(sfweight)
library(rgeoda)

# Lisa de cor

queen_w <- queen_weights(data_complete2,order = 1)

# calculate LISA as per GEODA
lisa <- local_moran(queen_w, data_complete2["dif_brancos_pretos"])

data_complete2$cluster_bp <- as.factor(lisa$GetClusterIndicators())
levels(data_complete2$cluster_bp) <- lisa$GetLabels()

tema <- function(base_size) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.key.width=unit(1.0,"line"),
      legend.key.height = unit(0.4,"cm"),
      legend.text=element_text(size=rel(0.5), angle = 0, vjust = 0),
      legend.title=element_text(size=rel(0.6), hjust = -0.5),
      strip.text = element_blank()
      # strip.text = element_text(size=rel(0.9))
      # plot.title = element_text(size = rel(1.5)),
      
      
      
    )
}

# ggplot(data = data_complete2) +
#   geom_sf(aes(fill = cluster_bp), color = NA)



map_lisa_cor <- ggplot()+
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = st_transform(data_complete2 %>% filter(cluster_hm != "Low-High"),
                              3857), aes(fill = cluster_bp), colour = NA, alpha=1, size = 0)+
  geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
  # geom_sf(data = simplepolys %>% st_transform(3857),
  #         # aes(size = 2),
  #         aes(color = "#8E8E8E"),
  #         fill = NA,
  #         # stroke = 2,
  #         # size = 2,
  #         linewidth = .7,
  #         alpha= .1)  +
  
  geom_sf(data = assentamentos,
          aes(colour = "white"),
          fill = NA,
          size = 1.3)+
  
  scale_color_identity(labels = c("white" = "Assentamentos\nPrecarios\n(IBGE 2019)",
                                  blue = ""), guide = "legend") +
  labs(color = "")+
  
  scale_fill_manual(values = c('Not significant' = 'grey70', 'High-High' = '#21367D',
                               'Low-Low' = '#CC3003'
                               # 'Low-High' = '#D96E0A'
                               # 'Low-High' = NA
                               ),
                    labels = c('Not significant' = 'Não Significativo',
                               'High-High' = 'Maior predominância\nde brancos',
                               'Low-Low' = 'Menor predominância\nde brancos'
                               # 'Low-High' = ''
                               # 'Low-High' = 'Baixo-Alto'
                               )) +
  
  # scale_fill_distiller("Renda per capita\n(Salarios Minimos)",
  #                      type = "div",
  #                      palette = "GnBu",
  #                      direction = 1,
  #                      breaks = seq(0,10,2),
  #                      labels = c("0","2", "4", "6","8", ">10"),
  #                      limits = c(0,10)) +
  
  
  # scale_fill_gradientn(
  #   name = "Renda Media",
  #   colors = colors_acc ,
  #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
  #   # values = NULL,
  #   space = "Lab",
  #   na.value = NA,
  #   # guide = "colourbar",
  #   aesthetics = "fill",
#   # colors
# ) +

# scale_fill_continuous(palette = "Blues",
#                   aesthetics = "fill")+
# viridis::scale_fill_viridis(option = "cividis"
# 
# #                             labels = scales::label_percent(accuracy = .1, decimal.mark = ",")
# # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
# #                               scale = ifelse(sigla_op== "TT",1e-3,1))
# #                      limits = c(0,500000))+
# # , limits = c(0, 0.72)
# # , breaks = c(0.001, 0.35, 0.7)
# # , labels = c(0, "35", "70%")
# ) +

# scale_color_manual(values = 'transparent')+
# facet_wrap(~ind, ncol = 2)+
tema()+
  labs(fill = "Agrupamento") +
  theme(legend.title = element_text(size=rel(0.9), hjust = 0),
        axis.ticks.length = unit(0,"pt"),
        legend.margin = margin(t = 0),
        legend.position = c(0.22, 0.2),
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.text =element_text(size=rel(0.9), hjust = 0, vjust = 1)
        # legend.background = element_rect(fill = "white", color = NA)
        # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
        #       # plot.background = element_rect(fill = "#eff0f0",
        #       #                                 colour = NA)
        # legend.background = element_rect(fill = "white",
        #                                  colour = NA)
        #       
  )
# map_lisa_cor

ggsave(map_lisa_cor,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/3-lisa_cor_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = width, height = height, units = "cm" )

# mapview(data_complete2, zcol = "dif_percent_cor")

data_complete2 <- data_complete2 %>%
  mutate(dif_percent_cor = dif_brancos_pretos/Ptot,
         dif_percent_genero = dif_homens_mulheres/Ptot)
#histograma de diferenca de pop por cor

hist_difcor <- ggplot(data_complete2, aes(x = dif_brancos_pretos)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 fill = '#d96e0a') + 
  scale_y_continuous(labels = scales::percent)+
  xlab("Diferenca absoluta entre habitantes\nde cor branca e de cor preta") +
  ylab('Porcentagem de hexagonos')+
  # geom_text(mean(dif_brancos_pretos, na.rm = T))+
  theme_minimal() +
  theme(axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.3))
        )
ggsave(hist_difcor,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/3-hist_diff_cor_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 10, height = height, units = "cm" )


hist_difgenero <- ggplot(data_complete2, aes(x = dif_homens_mulheres)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 fill = '#d96e0a') + 
  scale_y_continuous(labels = scales::percent)+
  xlab("Diferenca absoluta entre\nhabitantes homens e mulheres") +
  ylab('Porcentagem de hexagonos')+
  # geom_text(mean(dif_brancos_pretos, na.rm = T))+
  theme_minimal() +
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.3))
  )

# hist_difgenero
# mapview(data_complete2 , zcol  = 'dif_homens_mulheres')

suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))

ggsave(hist_difgenero,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/4-hist_diff_cor_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 10, height = height, units = "cm" )























# Lisa de Genero

queen_w <- queen_weights(data_complete2,order = 1)

# calculate LISA as per GEODA
lisa_genero <- local_moran(queen_w, data_complete2["dif_homens_mulheres"])

data_complete2$cluster_hm <- as.factor(lisa_genero$GetClusterIndicators())
levels(data_complete2$cluster_hm) <- lisa_genero$GetLabels()



map_lisa_genero <- ggplot()+
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = st_transform(data_complete2 %>% filter(cluster_hm != "Low-High"),
                              3857),
          aes(fill = cluster_hm), colour = NA, alpha=1, size = 0)+
  geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
  # geom_sf(data = simplepolys %>% st_transform(3857),
  #         # aes(size = 2),
  #         aes(color = "#8E8E8E"),
  #         fill = NA,
  #         # stroke = 2,
  #         # size = 2,
  #         linewidth = .7,
  #         alpha= .1)  +
  
  geom_sf(data = assentamentos,
          aes(colour = "white"),
          fill = NA,
          size = 1.3)+
  
  scale_color_identity(labels = c("white" = "Assentamentos\nPrecarios\n(IBGE 2019)",
                                  blue = ""), guide = "legend") +
  labs(color = "")+
  
  scale_fill_manual(values = c('Not significant' = 'grey70', 'High-High' = '#CC3003',
                               'Low-Low' = '#21367D'
                               # 'Low-High' = '#D96E0A'
                               ),
                    labels = c('Not significant' = 'Não Significativo',
                               'High-High' = 'Menor predominância\nde mulheres',
                               'Low-Low' = 'Maior predominância\nde mulheres'
                               # 'Low-High' = 'Baixo-Alto'
                               )) +
  
  # scale_fill_distiller("Renda per capita\n(Salarios Minimos)",
  #                      type = "div",
  #                      palette = "GnBu",
  #                      direction = 1,
  #                      breaks = seq(0,10,2),
  #                      labels = c("0","2", "4", "6","8", ">10"),
  #                      limits = c(0,10)) +
  
  
  # scale_fill_gradientn(
#   name = "Renda Media",
#   colors = colors_acc ,
#   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
#   # values = NULL,
#   space = "Lab",
#   na.value = NA,
#   # guide = "colourbar",
#   aesthetics = "fill",
#   # colors
# ) +

# scale_fill_continuous(palette = "Blues",
#                   aesthetics = "fill")+
# viridis::scale_fill_viridis(option = "cividis"
# 
# #                             labels = scales::label_percent(accuracy = .1, decimal.mark = ",")
# # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
# #                               scale = ifelse(sigla_op== "TT",1e-3,1))
# #                      limits = c(0,500000))+
# # , limits = c(0, 0.72)
# # , breaks = c(0.001, 0.35, 0.7)
# # , labels = c(0, "35", "70%")
# ) +

# scale_color_manual(values = 'transparent')+
# facet_wrap(~ind, ncol = 2)+
tema()+
  labs(fill = "Agrupamento") +
  theme(legend.title = element_text(size=rel(0.9), hjust = 0),
        axis.ticks.length = unit(0,"pt"),
        legend.margin = margin(t = 0),
        legend.position = c(0.22, 0.2),
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.text =element_text(size=rel(0.9), hjust = 0, vjust = 1)
        # legend.background = element_rect(fill = "white", color = NA)
        # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
        #       # plot.background = element_rect(fill = "#eff0f0",
        #       #                                 colour = NA)
        # legend.background = element_rect(fill = "white",
        #                                  colour = NA)
        #       
  )
# map_lisa_genero
# map_pop_density

suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))

ggsave(map_lisa_genero,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/4-lisa_genero_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = width, height = height, units = "cm" )






# Lisa de Renda

#dados de renda da microssimulacao

dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                    sigla_muni, sigla_muni))
#checar setores com todos os renda_class_pc == n_col

lista_tract <- dados_simulacao %>% group_by(code_tract, renda_class_pc) %>%
  summarise(n = n()) %>% ungroup() %>%
  group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
                                     n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
  filter(n_classes > n_classes_col) %>% pull(code_tract)

data_micro2 <- dados_simulacao %>% filter(code_tract %in% lista_tract) %>% select(1:10, V0606, hex) %>%
  mutate(V0606 = as.factor(V0606))


#remocão dos habitantes de cor amarela e indígena
levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
# data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))

data_micro2 <- data_micro2 %>%
  # mutate(total = "total") %>% 
  mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
         cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                         cor == "pretos" ~ "Pretos",
                         cor == "brancos" ~ "Brancos"))

renda <- dados_simulacao %>%
  group_by(hex) %>%
  summarise(renda = mean(Rend_pc, na.rm =T)) %>% left_join(data_mhex, by = c("hex" = "id_hex")) %>%
  st_as_sf() %>% drop_na(h3_resolution) %>%
  mutate(renda = ifelse(renda>10, 10, renda))



queen_w <- queen_weights(renda,order = 1)

# calculate LISA as per GEODA
lisa_renda <- local_moran(queen_w, renda["renda"])

renda$cluster_renda <- as.factor(lisa_renda$GetClusterIndicators())
levels(renda$cluster_renda) <- lisa_renda$GetLabels()



map_lisa_renda <- ggplot()+
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = st_transform(renda %>% filter(cluster_renda != "Low-High"), 3857), aes(fill = cluster_renda), colour = NA, alpha=1, size = 0)+
  geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
  # geom_sf(data = simplepolys %>% st_transform(3857),
  #         # aes(size = 2),
  #         aes(color = "#8E8E8E"),
  #         fill = NA,
  #         # stroke = 2,
  #         # size = 2,
  #         linewidth = .7,
  #         alpha= .1)  +
  
  geom_sf(data = assentamentos,
          aes(colour = "white"),
          fill = NA,
          size = 1.3)+
  
  scale_color_identity(labels = c("white" = "Assentamentos\nPrecarios\n(IBGE 2019)",
                                  blue = ""), guide = "legend") +
  labs(color = "")+
  
  scale_fill_manual(values = c('Not significant' = 'grey70', 'High-High' = '#CC3003',
                               'Low-Low' = '#21367D'
                               # 'Low-High' = '#D96E0A'
                               ),
                    labels = c('Not significant' = 'Não Significativo',
                               'High-High' = 'Predominância de\nvalores de renda\nper capita maiores',
                               'Low-Low' = 'Predominância de\nvalores de renda\nper capita menores'
                               # 'Low-High' = 'Baixo-Alto'
                               )) +
  
  # scale_fill_distiller("Renda per capita\n(Salarios Minimos)",
  #                      type = "div",
  #                      palette = "GnBu",
  #                      direction = 1,
  #                      breaks = seq(0,10,2),
  #                      labels = c("0","2", "4", "6","8", ">10"),
  #                      limits = c(0,10)) +
  
  
  # scale_fill_gradientn(
#   name = "Renda Media",
#   colors = colors_acc ,
#   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
#   # values = NULL,
#   space = "Lab",
#   na.value = NA,
#   # guide = "colourbar",
#   aesthetics = "fill",
#   # colors
# ) +

# scale_fill_continuous(palette = "Blues",
#                   aesthetics = "fill")+
# viridis::scale_fill_viridis(option = "cividis"
# 
# #                             labels = scales::label_percent(accuracy = .1, decimal.mark = ",")
# # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
# #                               scale = ifelse(sigla_op== "TT",1e-3,1))
# #                      limits = c(0,500000))+
# # , limits = c(0, 0.72)
# # , breaks = c(0.001, 0.35, 0.7)
# # , labels = c(0, "35", "70%")
# ) +

# scale_color_manual(values = 'transparent')+
# facet_wrap(~ind, ncol = 2)+
tema()+
  labs(fill = "Agrupamento") +
  theme(legend.title = element_text(size=rel(0.9), hjust = 0),
        axis.ticks.length = unit(0,"pt"),
        legend.margin = margin(t = 0),
        legend.position = c(0.22, 0.2),
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.text =element_text(size=rel(0.9), hjust = 0, vjust = 1)
        # legend.background = element_rect(fill = "white", color = NA)
        # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
        #       # plot.background = element_rect(fill = "#eff0f0",
        #       #                                 colour = NA)
        # legend.background = element_rect(fill = "white",
        #                                  colour = NA)
        #       
  )
# map_lisa_renda
# map_pop_density

suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))

ggsave(map_lisa_renda,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/5-lisa_renda_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = width, height = height, units = "cm" )


hist_renda <- ggplot(renda, aes(x = renda)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 fill = '#d96e0a') + 
  scale_y_continuous(labels = scales::percent)+
  xlab("Renda per capita\n(sálarios mínimos)") +
  ylab('Porcentagem de hexagonos')+
  # geom_text(mean(dif_brancos_pretos, na.rm = T))+
  theme_minimal() +
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.3))
  )
ggsave(hist_renda,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/5-hist_renda_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 10, height = height, units = "cm" )









# Famílas Chefiadas por Homens ou Mulheres --------------------------------



resp_dom <- dados_simulacao %>%
  group_by(hex, resp_home) %>%
  summarise(n = n(),
            r_pcm = mean(Rend_pc, na.rm = T),
            r_pm = mean(Rend_pes, na.rm = T)) %>% left_join(data_mhex, by = c("hex" = "id_hex")) %>%
  st_as_sf() %>% drop_na(h3_resolution) %>%
  # mutate(r_pcm = ifelse(r_pcm>10, 10, r_pcm),
  #        r_pm = ifelse(r_pm>10, 10, r_pm)) %>%
  filter(resp_home != "Resp_dep") %>%
  ungroup()

resp_dom_geral <- resp_dom %>% spread(key = resp_home, value = n) %>%
  group_by(hex) %>%
  mutate(n_tot_homens = sum(Resp_masc, na.rm = T),
         n_tot_mulheres = sum(Resp_fem, na.rm = T),
         renda_media_resp_p_homem = r_pm[which(is.na(Resp_fem) == T)],
         renda_media_resp_pc_homem = r_pcm[which(is.na(Resp_fem) == T)],
         renda_media_resp_p_mulher = r_pm[which(is.na(Resp_masc) == T)],
         renda_media_resp_pc_mulher = r_pcm[which(is.na(Resp_masc) == T)])


resp_dom_geral <- resp_dom %>% spread(key = resp_home, value = n) %>%
  group_by(hex) %>%
  mutate(n_tot_homens = sum(Resp_masc, na.rm = T),
         n_tot_mulheres = sum(Resp_fem, na.rm = T)) %>%
  distinct(hex, .keep_all = T) %>%
  select(hex, n_tot_homens, n_tot_mulheres)

resp_dom_renda <- resp_dom %>%
  group_by(hex) %>%
  mutate(dif_rend_pc_hm = ifelse(is_empty(r_pcm[resp_home == "Resp_masc"]) == F &
                                is_empty(r_pcm[resp_home == "Resp_fem"]) == F ,
                                r_pcm[resp_home == "Resp_masc"] - r_pcm[resp_home == "Resp_fem"], NA),
         dif_rend_pes_hm = ifelse(is_empty(r_pcm[resp_home == "Resp_masc"]) == F &
                                   is_empty(r_pcm[resp_home == "Resp_fem"]) == F ,
                                 r_pcm[resp_home == "Resp_masc"] - r_pcm[resp_home == "Resp_fem"], NA)
         
         ) %>% distinct(hex, .keep_all = T) %>%
  select(hex, dif_rend_pc_hm, dif_rend_pes_hm)

resp_dom_all <- resp_dom_geral %>% left_join(resp_dom_renda %>% st_drop_geometry(),
                                             by = "hex") %>%
  mutate(dif_hm = n_tot_homens-n_tot_mulheres)


# resp_dom_geral <- resp_dom %>%
#   group_by(hex) %>%
#   summarise(dif_hm = n[2] - n[1],
#             dif_renda_pc_hm = r_pcm[2] - r_pcm[1],
#             dif_renda_pes_hm = r_pm[2] - r_pm[1])

# resp_dom_h <- resp_dom %>% filter(resp_home == "Resp_masc")
# resp_dom_m <- resp_dom %>% filter(resp_home == "Resp_fem")

#Lisa de diferenca de responsaveis

renda_homens <- resp_dom %>% filter(resp_home == "Resp_masc") %>%
  select(hex, r_pcm_h = r_pcm, r_pm_h = r_pm)
renda_mulheres <- resp_dom %>% filter(resp_home == "Resp_fem") %>%
  select(hex, r_pcm_m = r_pcm, r_pm_m = r_pm)

renda_resp <- resp_dom_all %>% left_join(renda_homens %>% st_drop_geometry(), by = "hex") %>%
  left_join(renda_mulheres %>% st_drop_geometry(), by = "hex") %>%
  mutate(dif_rend_pc_hm_percent = (dif_rend_pc_hm/r_pcm_m),
         dif_rend_pes_hm_percent = (dif_rend_pes_hm/r_pm_m))

renda_resp2 <- renda_resp %>% mutate_if(is.numeric, list(~na_if(., Inf)))
renda_resp2 <- renda_resp2 %>% mutate_if(is.numeric, list(~na_if(., -Inf)))

renda_resp_sem_na <- renda_resp2 %>% drop_na(dif_rend_pc_hm_percent, dif_rend_pes_hm_percent) %>%
  ungroup() %>%
  mutate(amplitude = (max(dif_rend_pc_hm_percent, na.rm = T)- min(dif_rend_pc_hm_percent, na.rm = T)),
         dif_rend_pc_hm_percent_pad = (dif_rend_pc_hm_percent - min(dif_rend_pc_hm_percent, na.rm =T))/ (max(dif_rend_pc_hm_percent, na.rm = T)- min(dif_rend_pc_hm_percent, na.rm = T)))
# max(renda_resp_sem_na$dif_rend_pc_hm_percent, na.rm = T)
# 
# t.test(renda_resp_sem_na$dif_rend_pc_hm_percent_pad)
# 
# mean(renda_resp_sem_na$dif_rend_pc_hm_percent_pad) * (max(renda_resp_sem_na$dif_rend_pc_hm_percent)- min(renda_resp_sem_na$dif_rend_pc_hm_percent)) + min(renda_resp_sem_na$dif_rend_pc_hm_percent)
# 
# 0.03281424 * (max(renda_resp_sem_na$dif_rend_pc_hm_percent)- min(renda_resp_sem_na$dif_rend_pc_hm_percent)) + min(renda_resp_sem_na$dif_rend_pc_hm_percent)
# 0.03530561 * (max(renda_resp_sem_na$dif_rend_pc_hm_percent)- min(renda_resp_sem_na$dif_rend_pc_hm_percent)) + min(renda_resp_sem_na$dif_rend_pc_hm_percent)


# summary(renda_resp_sem_na$dif_rend_pc_hm_percent)
t.test(renda_resp_sem_na$dif_rend_pc_hm_percent)

t.test(renda_resp_sem_na$dif_rend_pes_hm_percent)

t.test(renda_resp2$r_pcm_h, renda_resp2$r_pcm_m, paired = T)

t.test(renda_resp2$dif_rend_pc_hm)


#teste de medias sem agregar por hex
rendas_mulheres <- data_micro2 %>% filter(resp_home == "Resp_fem")
rendas_homens <- data_micro2 %>% filter(resp_home == "Resp_masc")
t.test(rendas_homens$Rend_pc, rendas_mulheres$Rend_pc)


queen_mw <- queen_weights(resp_dom_all, order = 1)

# calculate LISA as per GEODA
lisa_mw <- local_moran(queen_mw, resp_dom_all["dif_hm"])

queen_rendas <- queen_weights(renda_resp_sem_na, order = 1)
lisa_rpc_mw <- local_moran(queen_rendas, renda_resp_sem_na["dif_rend_pc_hm_percent"])
lisa_rp_mw <- local_moran(queen_rendas, renda_resp_sem_na["dif_rend_pes_hm_percent"])

resp_dom_all$cluster_mw <- as.factor(lisa_mw$GetClusterIndicators())
renda_resp_sem_na$cluster_rpc_mw <- as.factor(lisa_rpc_mw$GetClusterIndicators())
renda_resp_sem_na$cluster_rp_mw <- as.factor(lisa_rp_mw$GetClusterIndicators())

levels(resp_dom_all$cluster_mw) <- lisa_mw$GetLabels()
levels(renda_resp_sem_na$cluster_rpc_mw) <- lisa_rpc_mw$GetLabels()
levels(renda_resp_sem_na$cluster_rp_mw) <- lisa_rpc_mw$GetLabels()

#Mapa 4.1 - Lisa da diferenca espacial de responsaveis homens e mulheres

map_lisa_resp_hm <- ggplot()+
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = st_transform(resp_dom_all, 3857), aes(fill = cluster_mw), colour = NA, alpha=1, size = 0)+
  geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
  # geom_sf(data = simplepolys %>% st_transform(3857),
  #         # aes(size = 2),
  #         aes(color = "#8E8E8E"),
  #         fill = NA,
  #         # stroke = 2,
  #         # size = 2,
  #         linewidth = .7,
  #         alpha= .1)  +
  
  geom_sf(data = assentamentos,
          aes(colour = "white"),
          fill = NA,
          size = 1.3)+
  
  scale_color_identity(labels = c("white" = "Assentamentos\nPrecarios\n(IBGE 2019)",
                                  blue = ""), guide = "legend") +
  labs(color = "")+
  
  scale_fill_manual(values = c('Not significant' = 'grey70',
                               'High-High' = '#CC3003',
                               'Low-Low' = '#21367D',
                               'Low-High' = '#5766cc',
                               'High-Low' = '#33b099'
  ),
  labels = c('Not significant' = 'Não Significativo',
             'High-High' = '+ responsáveis do gênero Masc',
             'Low-Low' = '+ responsáveis do gênero Fem',
             'Low-High' = '+ resp. do gênero Fem\nao redor de resp. do gênero Masc',
             'High-Low' = '+ resp. do gênero Masc\nao redor de resp. do gênero Fem'
             # 'Low-High' = 'Baixo-Alto'
  )) +
  
  # scale_fill_distiller("Renda per capita\n(Salarios Minimos)",
  #                      type = "div",
  #                      palette = "GnBu",
  #                      direction = 1,
  #                      breaks = seq(0,10,2),
  #                      labels = c("0","2", "4", "6","8", ">10"),
  #                      limits = c(0,10)) +
  
  
  # scale_fill_gradientn(
#   name = "Renda Media",
#   colors = colors_acc ,
#   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
#   # values = NULL,
#   space = "Lab",
#   na.value = NA,
#   # guide = "colourbar",
#   aesthetics = "fill",
#   # colors
# ) +

# scale_fill_continuous(palette = "Blues",
#                   aesthetics = "fill")+
# viridis::scale_fill_viridis(option = "cividis"
# 
# #                             labels = scales::label_percent(accuracy = .1, decimal.mark = ",")
# # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
# #                               scale = ifelse(sigla_op== "TT",1e-3,1))
# #                      limits = c(0,500000))+
# # , limits = c(0, 0.72)
# # , breaks = c(0.001, 0.35, 0.7)
# # , labels = c(0, "35", "70%")
# ) +

# scale_color_manual(values = 'transparent')+
# facet_wrap(~ind, ncol = 2)+
tema()+
  labs(fill = "Agrupamento") +
  theme(legend.title = element_text(size=rel(0.9), hjust = 0),
        axis.ticks.length = unit(0,"pt"),
        legend.margin = margin(t = 0),
        legend.position = c(0.30, 0.2),
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.text =element_text(size=rel(0.9), hjust = 0, vjust = 1)
        # legend.background = element_rect(fill = "white", color = NA)
        # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
        #       # plot.background = element_rect(fill = "#eff0f0",
        #       #                                 colour = NA)
        # legend.background = element_rect(fill = "white",
        #                                  colour = NA)
        #       
  )
# map_lisa_resp_hm
# map_pop_density

suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))

ggsave(map_lisa_resp_hm,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/6-lisa_resp_hm_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = width, height = height, units = "cm" )


# Histograma H-M ----------------------------------------------------------

dados_histograma_hm <- resp_dom_all %>%
  gather(variavel, valor, 2:3)

hist_resp_dif_hm <- ggplot(resp_dom_all, aes(x = dif_hm)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 alpha = 1,
                 position= "identity",
                 fill = "#d96e0a") + 
  scale_y_continuous(labels = scales::percent)+
  xlab("Diferença de responsáveis\ndo gênero masculino e\nfeminino no hexágono") +
  ylab('Porcentagem de hexagonos') +
  # geom_text(mean(dif_brancos_pretos, na.rm = T))+
  theme_minimal() +
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.3))
  )
ggsave(hist_resp_dif_hm,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/6-hist_renda_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 10, height = height, units = "cm" )



#Mapa lisa da diferenca de renda per capita do responsavel por genero


#lisa de renda per capta

# map_lisa_resp_rpc_hm <- ggplot()+
#   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
#   coord_equal() +
#   scale_fill_identity()+
#   # nova escala
#   new_scale_fill() +
#   geom_sf(data = st_transform(renda_resp_sem_na, 3857), aes(fill = cluster_rpc_mw), colour = NA, alpha=1, size = 0)+
#   geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
#   # geom_sf(data = simplepolys %>% st_transform(3857),
#   #         # aes(size = 2),
#   #         aes(color = "#8E8E8E"),
#   #         fill = NA,
#   #         # stroke = 2,
#   #         # size = 2,
#   #         linewidth = .7,
#   #         alpha= .1)  +
#   
#   geom_sf(data = assentamentos,
#           aes(colour = "white"),
#           fill = NA,
#           size = 1.3)+
#   
#   scale_color_identity(labels = c("white" = "Assentamentos\nPrecarios\n(IBGE 2019)",
#                                   blue = ""), guide = "legend") +
#   labs(color = "")+
#   
#   scale_fill_manual(values = c('Not significant' = 'grey70',
#                                'High-High' = '#CC3003',
#                                'Low-Low' = '#21367D',
#                                'Low-High' = '#5766cc',
#                                'High-Low' = '#33b099'
#   ),
#   labels = c('Not significant' = 'Não Significativo',
#              'High-High' = '> Diferença percentual entre a renda per capita\ndo responsável do gênero masculino e do\nresponsável do gênero feminino',
#              'Low-Low' = '< Diferença percentual entre a renda per capita\ndo responsável do gênero masculino e do\nresponsável do gênero feminino',
#              'Low-High' = 'Hexágono de menor diferênça próximo\nde hexágono de maior diferença',
#              'High-Low' = 'Hexágono de menor diferênça próximo\nde hexágono de maior diferença'
#              # 'Low-High' = 'Baixo-Alto'
#   )) +
#   
#   # scale_fill_distiller("Renda per capita\n(Salarios Minimos)",
#   #                      type = "div",
#   #                      palette = "GnBu",
#   #                      direction = 1,
#   #                      breaks = seq(0,10,2),
#   #                      labels = c("0","2", "4", "6","8", ">10"),
#   #                      limits = c(0,10)) +
#   
#   
#   # scale_fill_gradientn(
# #   name = "Renda Media",
# #   colors = colors_acc ,
# #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
# #   # values = NULL,
# #   space = "Lab",
# #   na.value = NA,
# #   # guide = "colourbar",
# #   aesthetics = "fill",
# #   # colors
# # ) +
# 
# # scale_fill_continuous(palette = "Blues",
# #                   aesthetics = "fill")+
# # viridis::scale_fill_viridis(option = "cividis"
# # 
# # #                             labels = scales::label_percent(accuracy = .1, decimal.mark = ",")
# # # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
# # #                               scale = ifelse(sigla_op== "TT",1e-3,1))
# # #                      limits = c(0,500000))+
# # # , limits = c(0, 0.72)
# # # , breaks = c(0.001, 0.35, 0.7)
# # # , labels = c(0, "35", "70%")
# # ) +
# 
# # scale_color_manual(values = 'transparent')+
# # facet_wrap(~ind, ncol = 2)+
# tema()+
#   labs(fill = "Agrupamento") +
#   theme(legend.title = element_text(size=rel(0.9), hjust = 0),
#         axis.ticks.length = unit(0,"pt"),
#         legend.margin = margin(t = 0),
#         legend.position = c(0.30, 0.2),
#         legend.direction = "vertical",
#         legend.box = "vertical",
#         legend.text =element_text(size=rel(0.9), hjust = 0, vjust = 1)
#         # legend.background = element_rect(fill = "white", color = NA)
#         # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
#         #       # plot.background = element_rect(fill = "#eff0f0",
#         #       #                                 colour = NA)
#         # legend.background = element_rect(fill = "white",
#         #                                  colour = NA)
#         #       
#   )
# # map_lisa_renda
# # map_pop_density
# 
# suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
# 
# ggsave(map_lisa_resp_rpc_hm,
#        device = "png",
#        filename =  sprintf('../data/map_plots_population/muni_%s/7-lisa_diff_renda_pc_responsavel_%s.png',
#                            sigla_muni,
#                            sigla_muni),
#        dpi = 300,
#        width = width, height = height, units = "cm" )
# 
# 
# hist_renda <- ggplot(renda, aes(x = renda)) +
#   geom_histogram(aes(y = (..count..)/sum(..count..)),
#                  fill = '#d96e0a') + 
#   scale_y_continuous(labels = scales::percent)+
#   xlab("Renda per capita\n(sálarios mínimos)") +
#   ylab('Porcentagem de hexagonos')+
#   # geom_text(mean(dif_brancos_pretos, na.rm = T))+
#   theme_minimal() +
#   theme(axis.title = element_text(size = rel(1.5)),
#         axis.text = element_text(size = rel(1.3))
#   )
# ggsave(hist_renda,
#        device = "png",
#        filename =  sprintf('../data/map_plots_population/muni_%s/5-hist_renda_%s.png',
#                            sigla_muni,
#                            sigla_muni),
#        dpi = 300,
#        width = 10, height = height, units = "cm" )
# 
# 
# #Lisa da diferença de renda da pessoa responsavel do sexo masculino e do sexo feminino
# 
# map_lisa_resp_rp_hm <- ggplot()+
#   geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
#   coord_equal() +
#   scale_fill_identity()+
#   # nova escala
#   new_scale_fill() +
#   geom_sf(data = st_transform(renda_resp_sem_na, 3857), aes(fill = cluster_rp_mw), colour = NA, alpha=1, size = 0)+
#   geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey70", size = 2) +
#   # geom_sf(data = simplepolys %>% st_transform(3857),
#   #         # aes(size = 2),
#   #         aes(color = "#8E8E8E"),
#   #         fill = NA,
#   #         # stroke = 2,
#   #         # size = 2,
#   #         linewidth = .7,
#   #         alpha= .1)  +
#   
#   geom_sf(data = assentamentos,
#           aes(colour = "white"),
#           fill = NA,
#           size = 1.3)+
#   
#   scale_color_identity(labels = c("white" = "Assentamentos\nPrecarios\n(IBGE 2019)",
#                                   blue = ""), guide = "legend") +
#   labs(color = "")+
#   
#   scale_fill_manual(values = c('Not significant' = 'grey70', 'High-High' = '#CC3003',
#                                'Low-Low' = '#21367D'
#                                # 'Low-High' = '#D96E0A'
#   ),
#   labels = c('Not significant' = 'Não Significativo',
#              'High-High' = 'Maiores diferenças\nde renda da pessoa\nresponsável entre os\n
#              gênero masculino e\n do gênero feminino',
#              'Low-Low' = 'Maiores diferenças\nde renda da pessoa\nresponsável entre os\n
#              gênero feminino e\n do gênero masculino'
#              # 'Low-High' = 'Baixo-Alto'
#   )) +
#   
#   # scale_fill_distiller("Renda per capita\n(Salarios Minimos)",
#   #                      type = "div",
#   #                      palette = "GnBu",
#   #                      direction = 1,
#   #                      breaks = seq(0,10,2),
#   #                      labels = c("0","2", "4", "6","8", ">10"),
#   #                      limits = c(0,10)) +
#   
#   
#   # scale_fill_gradientn(
# #   name = "Renda Media",
# #   colors = colors_acc ,
# #   # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
# #   # values = NULL,
# #   space = "Lab",
# #   na.value = NA,
# #   # guide = "colourbar",
# #   aesthetics = "fill",
# #   # colors
# # ) +
# 
# # scale_fill_continuous(palette = "Blues",
# #                   aesthetics = "fill")+
# # viridis::scale_fill_viridis(option = "cividis"
# # 
# # #                             labels = scales::label_percent(accuracy = .1, decimal.mark = ",")
# # # labels = scales::label_number(suffix = ifelse(sigla_op== "TT","K",""),
# # #                               scale = ifelse(sigla_op== "TT",1e-3,1))
# # #                      limits = c(0,500000))+
# # # , limits = c(0, 0.72)
# # # , breaks = c(0.001, 0.35, 0.7)
# # # , labels = c(0, "35", "70%")
# # ) +
# 
# # scale_color_manual(values = 'transparent')+
# # facet_wrap(~ind, ncol = 2)+
# tema()+
#   labs(fill = "Agrupamento") +
#   theme(legend.title = element_text(size=rel(0.9), hjust = 0),
#         axis.ticks.length = unit(0,"pt"),
#         legend.margin = margin(t = 0),
#         legend.position = c(0.22, 0.2),
#         legend.direction = "vertical",
#         legend.box = "vertical",
#         legend.text =element_text(size=rel(0.9), hjust = 0, vjust = 1)
#         # legend.background = element_rect(fill = "white", color = NA)
#         # theme(plot.title = element_text(hjust = 0.5, size = rel(1)),
#         #       # plot.background = element_rect(fill = "#eff0f0",
#         #       #                                 colour = NA)
#         # legend.background = element_rect(fill = "white",
#         #                                  colour = NA)
#         #       
#   )
# # map_lisa_renda
# # map_pop_density
# 
# suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))
# 
# ggsave(map_lisa_renda,
#        device = "png",
#        filename =  sprintf('../data/map_plots_population/muni_%s/5-lisa_renda_%s.png',
#                            sigla_muni,
#                            sigla_muni),
#        dpi = 300,
#        width = width, height = height, units = "cm" )
# 
# 
# hist_renda <- ggplot(renda, aes(x = renda)) +
#   geom_histogram(aes(y = (..count..)/sum(..count..)),
#                  fill = '#d96e0a') + 
#   scale_y_continuous(labels = scales::percent)+
#   xlab("Renda per capita\n(sálarios mínimos)") +
#   ylab('Porcentagem de hexagonos')+
#   # geom_text(mean(dif_brancos_pretos, na.rm = T))+
#   theme_minimal() +
#   theme(axis.title = element_text(size = rel(1.5)),
#         axis.text = element_text(size = rel(1.3))
#   )
# ggsave(hist_renda,
#        device = "png",
#        filename =  sprintf('../data/map_plots_population/muni_%s/5-hist_renda_%s.png',
#                            sigla_muni,
#                            sigla_muni),
#        dpi = 300,
#        width = 10, height = height, units = "cm" )
}
