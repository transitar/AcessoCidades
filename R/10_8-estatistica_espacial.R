rm(list = ls());gc()

source('./R/fun/setup.R')

# novo hamburgo 

# sf_use_s2(use_s2 = TRUE)

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

sigla_muni <- 'noh'

# manaus coord_sf(xlim = c(-6700693,-6654021), ylim = c(-354102,-325873), expand = FALSE)

faz_lisa <- function(sigla_muni){

# setor_muni <- read_rds(sprintf('../data-raw/setores_censitarios/2019/setores_%s_2019.rds',sigla_muni)) %>%
#   mutate(area = st_area(.)) %>%
#   select(code_tract,area) %>%
#   mutate(code_tract = as.character(code_tract))
# 
  message(paste("Rodando",sigla_muni, "\n"))
  
  path_contorno <- sprintf('../data-raw/municipios/2019/municipio_%s_2019.rds', sigla_muni)
  
  #path hexagonos com os dados
  
  #dados_hex <- 
  
  # dados_hex <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni)) %>%
  #   st_as_sf()
  
  
  # path_muni_data_censo <- sprintf('../data-raw/censo_2021_info_muni_treated_v2/muni_%s.rds',sigla_muni)
  # path_muni_hex <- sprintf('../data/hex_municipio/hex_2019_%s_09.rds', sigla_muni)
  # path_muni_setor <- sprintf('../data-raw/setores_censitarios/2019/setores_%s_2019.rds',sigla_muni)
  path_maptiles <- sprintf('../data/maptiles_crop/2019/mapbox_2/maptile_crop_mapbox_%s_2019.rds',sigla_muni) 
  
  # data_muni <- read_rds(path_muni_data_censo) %>%  mutate(Cod_setor = as.character(Cod_setor))
  # data_msetor <- read_rds(path_muni_setor)
  # data_mhex <- read_rds(path_muni_hex)
  data_contorno <- read_rds(path_contorno)
  
  maptiles <- read_rds(path_maptiles)
  
  
  
  
  sigla_municipio <- sigla_muni
  decisao_muni <- read_excel('../planilha_municipios.xlsx',
                             sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
  
  
  dados_areas <- read_sf(sprintf('../data-raw/dados_municipais_recebidos/muni_%s/muni_%s.gpkg',
                                 sigla_muni, sigla_muni), layer= "areas") %>% st_transform(decisao_muni$epsg)
  
  area_urbanizada <- read_sf(sprintf('../data-raw/mapbiomas/area_urbanizada/usosolo_%s.gpkg',
                                     sigla_muni)) %>% filter(DN == 24) %>%
    st_make_valid() %>%
    st_union()
  # mapview(simplepolys)
  
  # sigla_municipio <- sigla_muni
  # decisao_muni <- read_excel('../planilha_municipios.xlsx',
  #                            sheet = 'dados') %>% filter(sigla_muni == sigla_municipio)
  
  simplepolys <- st_make_valid(area_urbanizada) %>% st_simplify(area_urbanizada, dTolerance = 300) %>%
    st_make_valid() %>%
    st_transform(decisao_muni$epsg) %>%
    st_buffer(2) %>%
    st_union() 
  
  assentamentos <- read_rds(sprintf('../data-raw/assentamentos_precarios/muni_%s_assentamentos_precarios/muni_%s.rds',
                                    sigla_muni, sigla_muni)) %>% st_transform(3857) %>%
    mutate(title = "Assentamentos Precários")
# setor_muni2 <- read_rds(sprintf('../data-raw/censo_2021_info_muni/muni_%s.rds',sigla_muni)) %>%
#   select(code_tract = Cod_setor,
#          # Situacao_setor,
#          # Tipo_setor
#          ) %>%
#   mutate(code_tract = as.character(code_tract))

# setor_muni <- left_join(setor_muni, setor_muni2, by ="code_tract")

dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                    sigla_muni, sigla_muni))
#checar setores com todos os renda_class_pc == n_col


data_micro2 <- dados_simulacao %>% select(1:12, V0606, hex) %>%
  mutate(V0606 = as.factor(V0606))


#remocão dos habitantes de cor amarela e indígena
levels(data_micro2$V0606) <- c("Brancos", "Pretos", "Amarelos", "Pardos", "Indígenas")
data_micro2 <- data_micro2 %>% filter(!V0606 %in% c("Amarelos", "Indígenas"))

data_micro2 <- data_micro2 %>%
  # mutate(total = "total") %>% 
  mutate(genero = ifelse(age_sex %like% 'w', "Mulheres", "Homens"),
         cor = case_when(cor == "pard_am_ing" ~ "Pretos",
                         cor == "pretos" ~ "Pretos",
                         cor == "brancos" ~ "Brancos"))



# slice(1:100)

# mapview(setor_muni)

# variaveis_muni <- read_rds(sprintf('../data-raw/censo_2021_info_muni_treated_v2/muni_%s.rds',sigla_muni)) %>%
#   mutate(Cod_setor = as.character(Cod_setor))
# 
# setor_var <- setor_muni %>%
#   left_join(variaveis_muni, by = c("code_tract"="Cod_setor")) %>% st_as_sf()

# mapview(setor_var, zcol = "P001")

hex_muni <- read_rds(sprintf('../data/hex_municipio/hex_2019_%s_09.rds',sigla_muni)) #%>%
# slice(1:100)

# mapview(hex_muni)

# intersect <- st_intersection(setor_var,hex_muni) %>% mutate(area_inter = st_area(.)) %>%
#   mutate(prop = as.numeric(area_inter/area)) %>%
#   mutate(across(starts_with("P0"),~ .x*prop)) %>%
#   st_set_geometry(NULL) %>%
#   group_by(id_hex) %>%
#   summarise(across(starts_with("P0"),~ round(sum(.x,na.rm = T),digits = 0)))


# 
# teste <-  st_intersection(setor_var,hex_muni) %>% mutate(area_inter = st_area(.)) %>% 
#   mutate(prop = as.numeric(area_inter/area)) %>% filter(id_hex == "898044582d7ffff")

# (sum(intersect$P011,na.rm = T) - sum(setor_var$P011,na.rm = T)) /sum(setor_var$P011,na.rm = T)

# intersect <- hex_muni %>% left_join(intersect, by = c("id_hex"="id_hex"))

dados_micro_sf <- hex_muni %>% left_join(data_micro2, by = c("id_hex"="hex"))

#poa -> id
#outros -> id_g

data_complete <- dados_micro_sf %>% st_drop_geometry() %>%
  drop_na(id_g) %>%
  group_by(id_hex, genero, cor) %>%
  summarise(n = n(), renda_per_capita = mean(Rend_pc))


data_complete_genero <- dados_micro_sf %>% st_drop_geometry() %>%
  drop_na(id_g) %>%
  group_by(id_hex, genero) %>%
  summarise(n = n(),
            renda_per_capita = mean(Rend_pc))  %>%
  
  mutate(dif_homens_mulheres = ifelse(is_empty(n[genero == "Homens"]) == F &
                                       is_empty(n[genero == "Mulheres"]) == F ,
                                      n[genero == "Homens"] - n[genero == "Mulheres"], NA),
         dif_renda_homens_mulheres = ifelse(is_empty(renda_per_capita[genero == "Homens"]) == F &
                                             is_empty(renda_per_capita[genero == "Mulheres"]) == F ,
                                           renda_per_capita[genero == "Homens"] - renda_per_capita[genero == "Mulheres"], NA)
  ) %>% distinct(id_hex, .keep_all = T) %>%
  left_join(hex_muni, by = "id_hex") %>% st_as_sf()


data_complete_cor <- dados_micro_sf %>% st_drop_geometry() %>%
  drop_na(id_g) %>%
  group_by(id_hex, cor) %>%
  summarise(n = n(),
            renda_per_capita = mean(Rend_pc)) %>%
  ungroup() %>%
  group_by(id_hex) %>%
  
  mutate(dif_pretos_brancos = ifelse(is_empty(n[cor == "Brancos"]) == F &
                                   is_empty(n[cor == "Pretos"]) == F ,
                                   n[cor == "Pretos"] - n[cor == "Brancos"],
                                   
                                   ifelse(is_empty(n[cor == "Pretos"]) == F &
                                            is_empty(n[cor == "Brancos"]) == T,
                                          n[cor == "Pretos"] - 0,
                                          ifelse(is_empty(n[cor == "Brancos"]) == F &
                                                   is_empty(n[cor == "Pretos"]) == T,
                                                 -n[cor == "Pretos"], NA)))
                                   ,
         dif_brancos_pretos = ifelse(is_empty(n[cor == "Brancos"]) == F &
                                       is_empty(n[cor == "Pretos"]) == F ,
                                     n[cor == "Brancos"] - n[cor == "Pretos"],
                                     
                                     ifelse(is_empty(n[cor == "Pretos"]) == F &
                                              is_empty(n[cor == "Brancos"]) == T,
                                            -n[cor == "Pretos"],
                                            ifelse(is_empty(n[cor == "Brancos"]) == F &
                                                     is_empty(n[cor == "Pretos"]) == T,
                                                   n[cor == "Pretos"], NA)))
         ,
         
         
         
         dif_renda_brancos_pretos = ifelse(is_empty(renda_per_capita[cor == "Pretos"]) == F &
                                             is_empty(renda_per_capita[cor == "Brancos"]) == F ,
                                           renda_per_capita[cor == "Brancos"] - renda_per_capita[cor == "Pretos"],
                                           
                                           ifelse(is_empty(renda_per_capita[cor == "Brancos"]) == F &
                                                    is_empty(renda_per_capita[cor == "Pretos"]) == T,
                                                  renda_per_capita[cor == "Brancos"] - 0,
                                                  ifelse(is_empty(renda_per_capita[cor == "Pretos"]) == F &
                                                           is_empty(renda_per_capita[cor == "Brancos"]) == T,
                                                         -renda_per_capita[cor == "Pretos"], NA)))
         
  ) %>% distinct(id_hex, .keep_all = T) %>%
  left_join(hex_muni, by = "id_hex") %>% st_as_sf() %>% drop_na(dif_pretos_brancos)

# data_complete_cor$dif_pretos_brancos[which(data_complete_cor$id_hex== "89817101d0bffff")] <- NA

# mapview(data_complete_cor, zcol = "dif_pretos_brancos")
  
  # mapview(data_complete_cor, zcol = "dif_renda_brancos_pretos")


  # rowwise() %>% 
  # mutate(Ptot_mulheres = sum(P006,P007,P008,P009,P010),
  #        Ptot_homens = sum(P001,P002,P003,P004,P005)) %>% 
  # mutate(Ptot = Ptot_homens + Ptot_mulheres) %>% 
  # filter(Ptot_mulheres > 1) %>%
  # filter(Ptot_homens > 1) %>% 
  # mutate(renda_per_capita= P011/Ptot)



# data_complete <- intersect %>%
#   mutate(area = st_area(.)/10^6) %>%
#   rowwise() %>%
#   mutate(Ptot_mulheres = sum(P006,P007,P008,P009,P010),
#          Ptot_homens = sum(P001,P002,P003,P004,P005)) %>%
#   mutate(Ptot = Ptot_homens + Ptot_mulheres) %>%
#   filter(Ptot_mulheres > 1) %>%
#   filter(Ptot_homens > 1) %>%
#   mutate(renda_per_capita= P011/Ptot)
# # 
# data_complete2 <- data_complete %>%
#   mutate(Ptot_brancos = sum(P001, P006),
#          Ptot_pretos = sum(P002, P004, P007, P009)) %>%
#   mutate(dif_pretos_brancos = Ptot_pretos - Ptot_brancos,
#          dif_homens_mulheres = Ptot_homens - Ptot_mulheres)
# 
# ggplot(data_complete2) +
#   geom_histogram(aes(dif_pretos_brancos), bins = 10)
# ggplot(data_complete_cor) +
#   geom_histogram(aes(dif_pretos_brancos), bins = 10)

# mapview(data_complete2, zcol = 'dif_pretos_brancos')
# mapview(data_complete2, zcol = 'dif_pretos_brancos')
# mapview(data_complete_cor, zcol = 'dif_pretos_brancos')
# 
# hist(data_complete2$dif_pretos_brancos)
# hist(data_complete_cor$dif_pretos_brancos)
#lisa map

library(sfweight)
library(rgeoda)

# Lisa de cor

queen_w <- queen_weights(data_complete_cor,order = 1)

# calculate LISA as per GEODA
lisa <- local_moran(queen_w, data_complete_cor["dif_brancos_pretos"])

data_complete_cor$cluster_bp <- as.factor(lisa$GetClusterIndicators())
levels(data_complete_cor$cluster_bp) <- lisa$GetLabels()


tema <- function(base_size) {
  
  
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
    legend.text=element_text(size=30, family = "encode_sans_light"),
    legend.title=element_text(size=35, family = "encode_sans_bold"),
    plot.title = element_text(hjust = 0, vjust = 4),
    strip.text = element_text(size = 10),
    legend.position = c(0.22, 0.25),
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
  )
  
  
  
}




# tema <- function(base_size) {
#   
#   # theme_void(base_family="Roboto Condensed") %+replace%
#   theme_void() %+replace%
#     
#     theme(
#       legend.position = "bottom",
#       plot.margin=unit(c(0,0,0,0),"mm"),
#       legend.key.width=unit(1.0,"line"),
#       legend.key.height = unit(0.4,"cm"),
#       legend.text=element_text(size=rel(0.5), angle = 0, vjust = 0),
#       legend.title=element_text(size=rel(0.6), hjust = -0.5),
#       strip.text = element_blank()
#       # strip.text = element_text(size=rel(0.9))
#       # plot.title = element_text(size = rel(1.5)),
#       
#       
#       
#     )
# }

# ggplot(data = data_complete2) +
#   geom_sf(aes(fill = cluster_bp), color = NA)


# Mapa lisa cor novo ----------------------------------------------------

data_complete_cor <- data_complete_cor %>% filter(cluster_bp != "Undefined")

map_lisa_cor <- ggplot()+
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = st_transform(data_complete_cor,
                              3857), aes(fill = cluster_bp), colour = NA, alpha=1, size = 0)+
  geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey60", linewidth = 0.3) +

  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "bairros"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  
  geom_sf(data = assentamentos,
          aes(colour = "assentamentos"),
          fill = "#0A7E5C",
          linewidth = 0.3,
          alpha = 0.3)+
  
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "urb"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  
  scale_color_manual(name = "Uso do solo",
                     breaks = c("assentamentos", "bairros", "urb"),
                     values = c("assentamentos" = "#0A7E5C",
                                "urb" = "#FEF3DB",
                                "bairros" = "grey50"),
                     label = c("urb" = "Área urbanizada",
                               "assentamentos" = "Aglomerados subnormais",
                               "bairros" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)])
  )+
  
  scale_fill_manual(name = "Agrupamentos",
                    
                    breaks = c('Not significant',
                               'High-High',
                               'Low-Low',
                               'Low-High',
                               'High-Low'),
                    
                    values = c('Not significant' = 'grey70',
                               'High-High' = '#CC3003',
                               'Low-Low' = '#21367D',
                               'Low-High' = '#8D86C9',
                               'High-Low' = '#f1ed0c'
                               ),
                    labels = c('Not significant' = 'Não significativo',
                               'High-High' = '> Predominância de brancos',
                               'Low-Low' = '> Predominância de negros',
                               'Low-High' = '+ Negros próximos a brancos',
                               'High-Low' = '+ Brancos próximos a negros'
                               )) +
  
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
  
  tema()+
  
  theme(legend.position = c(0.22,0.31)) +

  aproxima_muni(sigla_muni = sigla_muni) +
  
  guides(color = guide_legend(override.aes = list(fill = c("#0A7E5C", "white", "white"),
                                                  color = c("#0A7E5C", "grey50", "#fde9be"),
                                                  linewidth = c(1,1,1)),
                              order = 2))
  
# map_lisa_cor

ggsave(map_lisa_cor,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/3-lisa_cor_%s_new.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = width, height = height, units = "cm" )

# mapview(data_complete2, zcol = "dif_percent_cor")

data_complete2 <- data_complete_cor #%>%
  # mutate(dif_percent_cor = dif_brancos_pretos/Ptot,
  #        dif_percent_genero = dif_homens_mulheres/Ptot)
#histograma de diferenca de pop por cor

# hist_difcor <- ggplot(data_complete2, aes(x = dif_brancos_pretos)) +
#   geom_histogram(aes(y = (..count..)/sum(..count..)),
#                  fill = '#d96e0a') + 
#   scale_y_continuous(labels = scales::percent)+
#   xlab("Diferenca absoluta entre habitantes\nde cor branca e de cor preta") +
#   ylab('Porcentagem de hexagonos')+
#   # geom_text(mean(dif_brancos_pretos, na.rm = T))+
#   theme_minimal() +
#   theme(axis.title = element_text(size = rel(1.3)),
#         axis.text = element_text(size = rel(1.3))
#         )
# ggsave(hist_difcor,
#        device = "png",
#        filename =  sprintf('../data/map_plots_population/muni_%s/3-hist_diff_cor_%s.png',
#                            sigla_muni,
#                            sigla_muni),
#        dpi = 300,
#        width = 10, height = height, units = "cm" )


# hist_difgenero <- ggplot(data_complete2, aes(x = dif_homens_mulheres)) +
#   geom_histogram(aes(y = (..count..)/sum(..count..)),
#                  fill = '#d96e0a') + 
#   scale_y_continuous(labels = scales::percent)+
#   xlab("Diferenca absoluta entre\nhabitantes homens e mulheres") +
#   ylab('Porcentagem de hexagonos')+
#   # geom_text(mean(dif_brancos_pretos, na.rm = T))+
#   theme_minimal() +
#   theme(axis.title = element_text(size = rel(1.5)),
#         axis.text = element_text(size = rel(1.3))
#   )

# hist_difgenero
# mapview(data_complete2 , zcol  = 'dif_homens_mulheres')



# LISA MAP diferenca de cor antigo -----------------------------------------------


map_lisa_cor <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  
  # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
  geom_sf(data = st_transform(data_complete_cor, 3857),
          aes(fill = cluster_bp),
          colour = NA,
          alpha=1,
          size = 0)+
  
  scale_fill_manual(name = "Agrupamentos",
    ,values = c('Not significant' = 'grey70', 'High-High' = '#21367D',
                               'Low-Low' = '#CC3003'
                               # 'Low-High' = '#D96E0A'
                               # 'Low-High' = NA
  ),
  labels = c('Not significant' = 'Não Significativo',
             'High-High' = '> predominância de negros',
             'Low-Low' = '< predominância de negros'
             # 'Low-High' = ''
             # 'Low-High' = 'Baixo-Alto'
  )) +
  
  # labs(color = 'Infraestrutura Cicloviária',
  #      fill = 'População') +
  
  ggnewscale::new_scale_color() +
  
  geom_sf(data = assentamentos,
          # aes(fill = "#d96e0a"),
          aes(color = "#0f805e"),
          
          # fill = "#d96e0a",
          linewidth = 1.0,
          fill = NA,
          show.legend = "polygon",
          alpha = 0.9)+
  
  # geom_sf(data = st_transform(dados_ciclovias_buffer, 3857),aes(fill = '#33b099'),
  #         color = NA,alpha = .7, linewidth = 1) +
  # geom_sf(data = st_transform(dados_linhas, 3857),
  #         aes(color = '#0f805e'),
  #         # color = '#0f805e',
  #         # color = NA,
  #         alpha = 1,
  #         linewidth = 1.0) +
  
  # scale_color_manual(name = "Uso do solo",
  #                    values = c("##0f805e" = "##0f805e"),
  #                    label = c("##0f805e" = "Assentamentos precários")
  # )+
  geom_sf(data = dados_areas %>% st_transform(3857),
        # aes(size = 2),
        aes(color = "grey60"),
        # color = "grey45",
        # aes(fill = '#CFF0FF'),
        fill = NA,
        # stroke = 2,
        # size = 2,
        linewidth = 0.7,
        alpha= 0.7) +
  
  
  # ggnewscale::new_scale_color() +
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "grey45"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.8,
          alpha= 0.7)  +
  
  scale_color_manual(name = "Uso do solo",
                     values = c("#0f805e" = "#0f805e",
                                "grey45" = "grey45",
                                "grey60" = "grey60"),
                     label = c("#0f805e" = "Assentamentos precários",
                               "grey45" = "Área urbana",
                               "grey60" = "Unidades de Planejamento")) +
  # scale_color_manual(name = "Área Urbanizada",
  #                    values = c("grey45" = "grey45"),
  #                    label = c("grey45" = "Palmas")
  # )+
  
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
  tema() +
  aproxima_muni(sigla_muni = sigla_muni)



suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))

ggsave(map_lisa_cor,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/3-lisa_cor_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 16.5, height = 16.5, units = "cm" )




# Histograma de diferença de cor ------------------------------------------


hist_difcor <- ggplot(data_complete_cor, aes(x = dif_brancos_pretos)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 fill = '#d96e0a') + 
  scale_y_continuous(labels = scales::percent)+
  xlab("Diferença entre brancos e negros") +
  ylab('Porcentagem de hexagonos')+
  theme_minimal() +
  theme(
    # strip.text.x = element_text(size=rel(1.2)),
    # strip.background = element_blank(),
    # panel.background = element_rect(fill = NA, colour = NA),
    # plot.margin=unit(c(0,0,0,0),"mm"),
    # legend.margin = margin(unit(c(10,10,5,10),"mm")),
    # legend.key.width=unit(2,"line"),
    # legend.key.height = unit(1,"line"),
    # legend.key = element_blank(),
    axis.text=element_text(size=50), #, family = "encode_sans_light"),
    axis.title=element_text(size=60),#, family = "encode_sans_bold"),
    plot.title = element_text(size = 50),#, family = "encode_sans_bold"),
    # strip.text = element_text(size = 60) ,
    legend.position = "bottom",
    # legend.box.background = element_rect(fill=alpha('white', 0.7),
    #                                      colour = "#A09C9C",
    #                                      linewidth = 0.8,
    #                                      linetype = "solid"),
    # legend.background = element_blank(),
    # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
    #                                      colour = "#E0DFE3"),
    # legend.spacing.y = unit(0.1, 'cm'),
    # legend.box.just = "left"
    # legend.margin = margin(t = -80)
  )




suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))

ggsave(hist_difcor,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/3-hist_diff_cor_%s_new.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 16.5, height = 16.5, units = "cm" )





# Lisa de genero antigo ---------------------------------------------------




















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






# Dados Lisa de Renda -----------------------------------------------------------


# Lisa de Renda

#dados de renda da microssimulacao

dados_simulacao <- read_rds(sprintf('../data/microssimulacao/muni_%s/micro_muni_%s.RDS',
                                    sigla_muni, sigla_muni))
#checar setores com todos os renda_class_pc == n_col
# 
# lista_tract <- dados_simulacao %>% group_by(code_tract, renda_class_pc) %>%
#   summarise(n = n()) %>% ungroup() %>%
#   group_by(code_tract) %>% summarise(n_classes = length(code_tract), 
#                                      n_classes_col = length(code_tract[renda_class_pc == "n_col"])) %>%
#   filter(n_classes > n_classes_col) %>% pull(code_tract)

data_micro2 <- dados_simulacao %>%
  # filter(code_tract %in% lista_tract)%>%
  select(1:12, V0606, hex) %>%
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
  summarise(renda = mean(Rend_pc, na.rm =T)) %>% left_join(hex_muni, by = c("hex" = "id_hex")) %>%
  st_as_sf() %>% drop_na(h3_resolution)
  # mutate(renda = ifelse(renda>10, 10, renda))



queen_w <- queen_weights(renda,order = 1)

# calculate LISA as per GEODA
lisa_renda <- local_moran(queen_w, renda["renda"])

renda$cluster_renda <- as.factor(lisa_renda$GetClusterIndicators())
levels(renda$cluster_renda) <- lisa_renda$GetLabels()

# mapview(renda, zcol = "renda")


# Lisa de Renda Antigo ----------------------------------------------------


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
  )+
  aproxima_muni(sigla_muni = sigla_muni)
# map_lisa_renda
# map_pop_density

# Mapa Lisa de renda novo ------------------------------------------------------

# mapview(renda, zcol = "cluster_renda")

renda <- renda %>% filter(cluster_renda != "Undefined")

map_lisa_renda <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  
  # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
  geom_sf(data = st_transform(renda, 3857),
          aes(fill = cluster_renda),
          colour = NA,
          alpha=1,
          size = 0)+
  
  geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey60", linewidth = 0.3) +
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "bairros"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  
  geom_sf(data = assentamentos,
          aes(colour = "assentamentos"),
          fill = "#0A7E5C",
          linewidth = 0.3,
          alpha = 0.3)+
  
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "urb"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  
  scale_color_manual(name = "Uso do solo",
                     breaks = c("assentamentos", "bairros", "urb"),
                     values = c("assentamentos" = "#0A7E5C",
                                "urb" = "#FEF3DB",
                                "bairros" = "grey50"),
                     label = c("urb" = "Área urbanizada",
                               "assentamentos" = "Aglomerados subnormais",
                               "bairros" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)])
  )+
  
  scale_fill_manual(name = "Agrupamentos",
                    
                    breaks = c('Not significant',
                               'High-High',
                               'Low-Low',
                               'Low-High',
                               'High-Low'),
                    
                    values = c('Not significant' = 'grey70',
                               'High-High' = '#CC3003',
                               'Low-Low' = '#21367D',
                               'Low-High' = '#8D86C9',
                               'High-Low' = '#f1ed0c'),
                    labels = c('Not significant' = 'Não significativo',
                               'High-High' = 'Maiores rendas',
                               'Low-Low' = 'Menores rendas',
                               'Low-High' = 'Renda menor próximo a maior',
                               'High-Low' = 'Renda maior próximo a menor')
                    ) +
  
  
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
  
  tema() +
  
  theme(legend.position = c(0.22,0.31)) +
  
  aproxima_muni(sigla_muni = sigla_muni) +
  
  guides(color = guide_legend(override.aes = list(fill = c("#0A7E5C", "white", "white"),
                                                  color = c("#0A7E5C", "grey50", "#fde9be"),
                                                  linewidth = c(1,1,1)),
                              order = 2))


suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))

ggsave(map_lisa_renda,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/5-lisa_renda_%s_new.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = width, height = height, units = "cm" )




# Histograma de renda -----------------------------------------------------




hist_renda <- ggplot(renda, aes(x = renda)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 fill = '#d96e0a') + 
  scale_y_continuous(labels = scales::percent)+
  xlab("Renda per capita (SM)") +
  ylab('Porcentagem de hexagonos')+
  # geom_text(mean(dif_brancos_pretos, na.rm = T))+
  theme_minimal() +
  theme(
    # strip.text.x = element_text(size=rel(1.2)),
    # strip.background = element_blank(),
    # panel.background = element_rect(fill = NA, colour = NA),
    # plot.margin=unit(c(0,0,0,0),"mm"),
    # legend.margin = margin(unit(c(10,10,5,10),"mm")),
    # legend.key.width=unit(2,"line"),
    # legend.key.height = unit(1,"line"),
    # legend.key = element_blank(),
    axis.text=element_text(size=50), #, family = "encode_sans_light"),
    axis.title=element_text(size=60),#, family = "encode_sans_bold"),
    plot.title = element_text(size = 50),#, family = "encode_sans_bold"),
    # strip.text = element_text(size = 60) ,
    legend.position = "bottom",
    # legend.box.background = element_rect(fill=alpha('white', 0.7),
    #                                      colour = "#A09C9C",
    #                                      linewidth = 0.8,
    #                                      linetype = "solid"),
    # legend.background = element_blank(),
    # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
    #                                      colour = "#E0DFE3"),
    # legend.spacing.y = unit(0.1, 'cm'),
    # legend.box.just = "left"
    # legend.margin = margin(t = -80)
  )
  
  # 
  # theme(axis.title = element_text(size = rel(1.5)),
  #       axis.text = element_text(size = rel(1.3))
  # ) 
  
ggsave(hist_renda,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/5-hist_renda_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 16, height = 16, units = "cm" )









# Famílas Chefiadas por Homens ou Mulheres --------------------------------



resp_dom <- dados_simulacao %>%
  group_by(hex, resp_home) %>%
  summarise(n = n(),
            r_pcm = mean(Rend_pc, na.rm = T),
            r_pm = mean(Rend_pes, na.rm = T)) %>% left_join(hex_muni, by = c("hex" = "id_hex")) %>%
  st_as_sf() %>% drop_na(h3_resolution) %>%
  # mutate(r_pcm = ifelse(r_pcm>10, 10, r_pcm),
  #        r_pm = ifelse(r_pm>10, 10, r_pm)) %>%
  filter(resp_home != "Resp_dep") %>%
  ungroup()

# resp_dom_geral <- resp_dom %>% spread(key = resp_home, value = n) %>%
#   group_by(hex) %>%
#   mutate(n_tot_homens = sum(Resp_masc, na.rm = T),
#          n_tot_mulheres = sum(Resp_fem, na.rm = T),
#          renda_media_resp_p_homem = r_pm[which(is.na(Resp_fem) == T)],
#          renda_media_resp_pc_homem = r_pcm[which(is.na(Resp_fem) == T)],
#          renda_media_resp_p_mulher = r_pm[which(is.na(Resp_masc) == T)],
#          renda_media_resp_pc_mulher = r_pcm[which(is.na(Resp_masc) == T)])


# resp_dom_geral <- resp_dom %>% spread(key = resp_home, value = n) %>%
#   group_by(hex) %>%
#   mutate(n_tot_homens = sum(Resp_masc, na.rm = T),
#          n_tot_mulheres = sum(Resp_fem, na.rm = T)) %>%
#   distinct(hex, .keep_all = T) %>%
#   select(hex, n_tot_homens, n_tot_mulheres)

resp_dom_renda <- resp_dom %>%
  group_by(hex) %>%
  
  mutate(dif_rend_pc_hm = ifelse(is_empty(r_pcm[resp_home == "Resp_masc"]) == F &
                                    is_empty(r_pcm[resp_home == "Resp_fem"]) == F ,
                                  r_pcm[resp_home == "Resp_masc"] - r_pcm[resp_home == "Resp_fem"],
                                  
                                  ifelse(is_empty(r_pcm[resp_home == "Resp_masc"]) == F &
                                           is_empty(r_pcm[resp_home == "Resp_fem"]) == T,
                                         r_pcm[resp_home == "Resp_masc"] - 0,
                                         ifelse(is_empty(r_pcm[resp_home == "Resp_fem"]) == F &
                                                  is_empty(r_pcm[resp_home == "Resp_masc"]) == T,
                                                -r_pcm[resp_home == "Resp_fem"], NA))),
         dif_resp_hm  = ifelse(is_empty(n[resp_home == "Resp_masc"]) == F &
                                 is_empty(n[resp_home == "Resp_fem"]) == F ,
                               n[resp_home == "Resp_masc"] - n[resp_home == "Resp_fem"],
                               
                               ifelse(is_empty(n[resp_home == "Resp_masc"]) == F &
                                        is_empty(n[resp_home == "Resp_fem"]) == T,
                                      n[resp_home == "Resp_masc"] - 0,
                                      ifelse(is_empty(n[resp_home == "Resp_fem"]) == F &
                                               is_empty(n[resp_home == "Resp_masc"]) == T,
                                             -n[resp_home == "Resp_fem"], NA)))
  ) %>%
  # mutate(dif_rend_pc_hm = ifelse(is_empty(r_pcm[resp_home == "Resp_masc"]) == F &
  #                               is_empty(r_pcm[resp_home == "Resp_fem"]) == F ,
  #                               r_pcm[resp_home == "Resp_masc"] - r_pcm[resp_home == "Resp_fem"], NA),
  #        dif_rend_pes_hm = ifelse(is_empty(r_pcm[resp_home == "Resp_masc"]) == F &
  #                                  is_empty(r_pcm[resp_home == "Resp_fem"]) == F ,
  #                                r_pcm[resp_home == "Resp_masc"] - r_pcm[resp_home == "Resp_fem"], NA)
  #        
  #        ) %>%
  distinct(hex, .keep_all = T) %>%
  select(hex, dif_rend_pc_hm, dif_resp_hm)



    
# resp_dom_all <- resp_dom_renda %>% left_join(resp_dom_renda %>% st_drop_geometry(),
#                                              by = "hex") %>%
#   mutate(dif_hm = n_tot_homens-n_tot_mulheres)





# resp_dom_all <- resp_dom_geral %>% left_join(resp_dom_renda %>% st_drop_geometry(),
#                                              by = "hex") %>%
#   mutate(dif_hm = n_tot_homens-n_tot_mulheres)


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

# renda_resp <- resp_dom_all %>% left_join(renda_homens %>% st_drop_geometry(), by = "hex") %>%
#   left_join(renda_mulheres %>% st_drop_geometry(), by = "hex") %>%
#   mutate(dif_rend_pc_hm_percent = (dif_rend_pc_hm/r_pcm_m),
#          dif_rend_pes_hm_percent = (dif_rend_pes_hm/r_pm_m))
# 
# renda_resp2 <- renda_resp %>% mutate_if(is.numeric, list(~na_if(., Inf)))
# renda_resp2 <- renda_resp2 %>% mutate_if(is.numeric, list(~na_if(., -Inf)))
# 
# 
# 
# renda_resp_sem_na <- renda_resp2 %>% drop_na(dif_rend_pc_hm_percent, dif_rend_pes_hm_percent) %>%
#   ungroup() %>%
#   mutate(amplitude = (max(dif_rend_pc_hm_percent, na.rm = T)- min(dif_rend_pc_hm_percent, na.rm = T)),
#          dif_rend_pc_hm_percent_pad = (dif_rend_pc_hm_percent - min(dif_rend_pc_hm_percent, na.rm =T))/ (max(dif_rend_pc_hm_percent, na.rm = T)- min(dif_rend_pc_hm_percent, na.rm = T)))
# # max(renda_resp_sem_na$dif_rend_pc_hm_percent, na.rm = T)
# # 
# # t.test(renda_resp_sem_na$dif_rend_pc_hm_percent_pad)
# # 
# # mean(renda_resp_sem_na$dif_rend_pc_hm_percent_pad) * (max(renda_resp_sem_na$dif_rend_pc_hm_percent)- min(renda_resp_sem_na$dif_rend_pc_hm_percent)) + min(renda_resp_sem_na$dif_rend_pc_hm_percent)
# # 
# # 0.03281424 * (max(renda_resp_sem_na$dif_rend_pc_hm_percent)- min(renda_resp_sem_na$dif_rend_pc_hm_percent)) + min(renda_resp_sem_na$dif_rend_pc_hm_percent)
# # 0.03530561 * (max(renda_resp_sem_na$dif_rend_pc_hm_percent)- min(renda_resp_sem_na$dif_rend_pc_hm_percent)) + min(renda_resp_sem_na$dif_rend_pc_hm_percent)
# 
# 
# # summary(renda_resp_sem_na$dif_rend_pc_hm_percent)
# t.test(renda_resp_sem_na$dif_rend_pc_hm_percent)
# 
# t.test(renda_resp_sem_na$dif_rend_pes_hm_percent)
# 
# t.test(renda_resp2$r_pcm_h, renda_resp2$r_pcm_m, paired = T)
# 
# t.test(renda_resp2$dif_rend_pc_hm)
# 
# 
# #teste de medias sem agregar por hex
# rendas_mulheres <- data_micro2 %>% filter(resp_home == "Resp_fem")
# rendas_homens <- data_micro2 %>% filter(resp_home == "Resp_masc")
# t.test(rendas_homens$Rend_pc, rendas_mulheres$Rend_pc)


# LISA genero Rsponsavel --------------------------------------------------

renda_resp_sem_na <- resp_dom_renda %>% drop_na()

queen_mw <- queen_weights(renda_resp_sem_na, order = 1)

# calculate LISA as per GEODA
lisa_mw <- local_moran(queen_mw, renda_resp_sem_na["dif_resp_hm"])

# queen_rendas <- queen_weights(renda_resp_sem_na, order = 1)
# 
# lisa_rpc_mw <- local_moran(queen_rendas, renda_resp_sem_na["dif_rend_pc_hm_percent"])
# lisa_rp_mw <- local_moran(queen_rendas, renda_resp_sem_na["dif_rend_pes_hm_percent"])

renda_resp_sem_na$cluster_mw <- as.factor(lisa_mw$GetClusterIndicators())
# renda_resp_sem_na$cluster_rpc_mw <- as.factor(lisa_rpc_mw$GetClusterIndicators())
# renda_resp_sem_na$cluster_rp_mw <- as.factor(lisa_rp_mw$GetClusterIndicators())

levels(renda_resp_sem_na$cluster_mw) <- lisa_mw$GetLabels()
# levels(renda_resp_sem_na$cluster_rpc_mw) <- lisa_rpc_mw$GetLabels()
# levels(renda_resp_sem_na$cluster_rp_mw) <- lisa_rpc_mw$GetLabels()

#Mapa 4.1 - Lisa da diferenca espacial de responsaveis homens e mulheres



# Mapa Lisa Diferenca de nuemros de responsáveis ANTIGO -------------------




map_lisa_resp_hm <- ggplot()+
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = st_transform(renda_resp_sem_na, 3857), aes(fill = cluster_mw), colour = NA, alpha=1, size = 0)+
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
        legend.position = c(0.74, 0.26),
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
  ) + aproxima_muni(sigla_muni = sigla_muni)
# map_lisa_resp_hm
# map_pop_density


# Mapa LISA de responsáveis NOVO ------------------------------------------

renda_resp_sem_na <- renda_resp_sem_na %>% filter(cluster_mw != "Undefined")


map_lisa_responsaveis <- ggplot() +
  geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  
  
  # c("#FEF5EC","#F5AF72","#E88D23","#d96e0a","#EF581B")
  geom_sf(data = st_transform(renda_resp_sem_na, 3857),
          aes(fill = cluster_mw),
          colour = NA,
          alpha=1,
          size = 0)+
  
  
  geom_sf(data = st_transform(data_contorno, 3857), fill = NA, colour = "grey60", linewidth = 0.3) +
  
  geom_sf(data = dados_areas %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "bairros"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7) +
  
  geom_sf(data = assentamentos,
          aes(colour = "assentamentos"),
          fill = "#0A7E5C",
          linewidth = 0.3,
          alpha = 0.3)+
  
  geom_sf(data = simplepolys %>% st_transform(3857),
          # aes(size = 2),
          aes(color = "urb"),
          # color = "grey45",
          # aes(fill = '#CFF0FF'),
          fill = NA,
          # stroke = 2,
          # size = 2,
          linewidth = 0.3,
          alpha= 0.7)  +
  
  scale_color_manual(name = "Uso do solo",
                     breaks = c("assentamentos", "bairros", "urb"),
                     values = c("assentamentos" = "#0A7E5C",
                                "urb" = "#FEF3DB",
                                "bairros" = "grey50"),
                     label = c("urb" = "Área urbanizada",
                               "assentamentos" = "Aglomerados subnormais",
                               "bairros" = munis_recorte_limites$legenda[which(munis_recorte_limites$abrev_muni==sigla_muni)])
  )+
  
  
  scale_fill_manual(name = "Agrupamentos",
                    
                    breaks = c('Not significant',
                               'High-High',
                               'Low-Low',
                               'Low-High',
                               'High-Low'),
                    
                    values = c('Not significant' = 'grey70',
                               'High-High' = '#CC3003',
                               'Low-Low' = '#21367D',
                               'Low-High' = '#8D86C9',
                               'High-Low' = '#f1ed0c'),
                    labels = c('Not significant' = 'Não significativo',
                               'High-High' = '+ Responsáveis do gênero masculino',
                               'Low-Low' = '+ Responsáveis do gênero feminino',
                               'Low-High' = '+ Resp. fem. próximos a + resp. masc.  ',
                               'High-Low' = '+ Resp. masc. próximos a + resp. fem.')
  ) +
  

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
  
  tema() +
  
  theme(legend.position = c(0.22,0.29),
        legend.text=element_text(size=25, family = "encode_sans_light"),
        legend.title=element_text(size=27, family = "encode_sans_bold")
        ) +
  
  aproxima_muni(sigla_muni = sigla_muni) +
  
  guides(color = guide_legend(override.aes = list(fill = c("#0A7E5C", "white", "white"),
                                                  color = c("#0A7E5C", "grey50", "#fde9be"),
                                                  linewidth = c(1,1,1)),
                              order = 2))








suppressWarnings(dir.create(sprintf('../data/map_plots_population/muni_%s/', sigla_muni)))

ggsave(map_lisa_responsaveis,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/6-lisa_resp_hm_%s_new2.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = width, height = height, units = "cm" )


# Histograma H-M ----------------------------------------------------------

dados_histograma_hm <- renda_resp_sem_na %>%
  gather(variavel, valor, 2:3)

hist_resp_dif_hm <- ggplot(renda_resp_sem_na, aes(x = dif_resp_hm)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 alpha = 1,
                 position= "identity",
                 fill = "#d96e0a") + 
  scale_y_continuous(labels = scales::percent)+
  xlab("Nº de responsáveis homens - mulheres") +
  ylab('Porcentagem de hexagonos') +
  # geom_text(mean(dif_brancos_pretos, na.rm = T))+
  theme_minimal() +
  theme(
    # strip.text.x = element_text(size=rel(1.2)),
    # strip.background = element_blank(),
    # panel.background = element_rect(fill = NA, colour = NA),
    # plot.margin=unit(c(0,0,0,0),"mm"),
    # legend.margin = margin(unit(c(10,10,5,10),"mm")),
    # legend.key.width=unit(2,"line"),
    # legend.key.height = unit(1,"line"),
    # legend.key = element_blank(),
    axis.text=element_text(size=50), #, family = "encode_sans_light"),
    axis.title=element_text(size=60),#, family = "encode_sans_bold"),
    plot.title = element_text(size = 50),#, family = "encode_sans_bold"),
    # strip.text = element_text(size = 60) ,
    legend.position = "bottom",
    # legend.box.background = element_rect(fill=alpha('white', 0.7),
    #                                      colour = "#A09C9C",
    #                                      linewidth = 0.8,
    #                                      linetype = "solid"),
    # legend.background = element_blank(),
    # legend.background = element_rect(fill=alpha('#F4F4F4', 0.5),
    #                                      colour = "#E0DFE3"),
    # legend.spacing.y = unit(0.1, 'cm'),
    # legend.box.just = "left"
    # legend.margin = margin(t = -80)
  )
ggsave(hist_resp_dif_hm,
       device = "png",
       filename =  sprintf('../data/map_plots_population/muni_%s/7-hist_responsaveis_%s.png',
                           sigla_muni,
                           sigla_muni),
       dpi = 300,
       width = 16, height = 16, units = "cm" )



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
