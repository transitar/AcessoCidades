# Gráfico de taxa de motorização
source('R/fun/setup.R')

dados_motor <- read_xlsx('../data/DENATRAN/DENATRAN_jan_2022.xlsx')
dados_motor <- setDT(dados_motor)[,2:ncol(dados_motor)]

norte <- c("AM","PA","RO","RR", "AP", "AC", "TO")
nordeste <- c("MA","PI","CE","RN", "PB", "PE", "AL", "SE", "BA")
sudeste <- c("MG","ES","SP","RJ")
sul <- c("RS","SC","PR")
centro <- c("MT","MS","GO","DF")

dados_motor <- dados_motor %>% mutate(regiao = case_when(UF %in% norte ~ "Norte",
                                                         UF %in% nordeste ~ "Nordeste",
                                                         UF %in% sudeste ~ "Sudeste",
                                                         UF %in% sul ~ "Sul",
                                                         UF %in% centro ~ "Centro-Oeste"))

dados_motor <- dados_motor %>% left_join(munis_names %>%
                                           select(cod_ibge, muni_abrev) %>%
                                           mutate(cod_ibge = as.character(cod_ibge)),
                                         by = c("CODE"="cod_ibge"))
dados_motor$muni_abrev <- factor(x = dados_motor$muni_abrev, levels = c(
  "arj",
  "bac",
  "nss",
  "sac",
  "vic",
  "slz",
  "man",
  "pal",
  "bel", 
  "dou",
  "cit",
  "com",
  "poa",
  "noh"), labels = c(
    "Aracaju/SE",
    "B. Coqueiros/SE",
    "N.S. Socorro/SE",
    "S. Cristóvão/SE",
    "V. Conquista/BA",
    "São Luís/MA",
    "Manaus/AM",
    "Palmas/TO",
    "Belém/PA",
    "Dourados/MS",
    "C. Itapemirim/ES",
    "Contagem/MG",
    "Porto Alegre/RS",
    "Novo Hamburgo/RS"
  ))

showtext_auto()
options(scipen = 100000000)
font_add("encode_sans", '../data/fontes/EncodeSans-VariableFont_wdth,wght.ttf')
font_add("encode_sans_regular", '../data/fontes/EncodeSans-Regular.ttf')
font_add("encode_sans_bold", '../data/fontes/EncodeSans-Bold.ttf')
font_add("encode_sans_light", '../data/fontes/EncodeSans-Light.ttf')

labels_data <- dados_motor %>%
  group_by(muni_abrev) %>%
  filter(ANO == 2022) %>%
  ungroup()

labels_data <- labels_data %>% mutate(position = case_when(muni_abrev == 'São Luís/MA' ~ MOTO_RATE,
                                                           muni_abrev == 'Manaus/AM' ~ MOTO_RATE,
                                                           muni_abrev == 'Belém/PA' ~ MOTO_RATE,
                                                           muni_abrev == 'C. Itapemirim/ES' ~ MOTO_RATE,
                                                           muni_abrev == 'Porto Alegre/RS' ~ MOTO_RATE,
                                                           muni_abrev == 'Palmas/TO' ~ MOTO_RATE,
                                                           muni_abrev == 'Dourados/MS' ~ MOTO_RATE,
                                                           muni_abrev == "N.S. Socorro/SE" ~ 0.25,
                                                           muni_abrev == "B. Coqueiros/SE" ~ 0.265,
                                                           muni_abrev == "S. Cristóvão/SE" ~ 0.28,
                                                           muni_abrev == "V. Conquista/BA" ~ 0.41,
                                                           muni_abrev == "Aracaju/SE" ~ 0.425,
                                                           muni_abrev == "Contagem/MG" ~ 0.445,
                                                           muni_abrev == "Novo Hamburgo/RS" ~ 0.65))

grid_h <- data.frame(xend = as.factor(rep(2022, 8)),
                     x = rep(0, 8),
                     y = seq(0.05,0.75,0.1),
                     yend = seq(0.05,0.75,0.1))
grid_h_secondary <- data.frame(xend = as.factor(rep(2022, 7)),
                     x = rep(0, 7),
                     y = seq(0.1,0.7,0.1),
                     yend = seq(0.1,0.7,0.1))

grafico_mot <- ggplot(dados_motor) +
  geom_vline(xintercept = dados_motor %>% distinct(ANO) %>% pull(ANO), linetype = "solid", color = "grey92", linewidth = rel(0.5)) +
  geom_segment(
    data = grid_h,
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "solid",
    color = "grey92",
    linewidth = rel(0.5)
  ) + 
  
  geom_segment(
    data = grid_h_secondary,
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "solid",
    color = "grey92",
    linewidth = rel(0.25)
  ) + 
  # geom_hline(yintercept = seq(0.05,0.75,0.1), linetype = "solid", color = "grey92", linewidth = rel(0.5)) +
  geom_line(aes(x=as.factor(ANO),
                y=MOTO_RATE,
                group = muni_abrev,
                color=as.factor(regiao)#,
                # size= NHabitantes
  )) +
  
  geom_point(aes(x=as.factor(ANO),
                y=MOTO_RATE,
                group = muni_abrev,
                color=as.factor(regiao)),
                size= 0.5
  ) +
  labs(#title="Taxa de motorização",
       x="Ano",
       y="Taxa de motorização") +
  # scale_x_discrete( breaks = c("RMA/SE", "Vitória da C./Ba", "São Luís/MA", "Manaus/AM", "Palmas/TO", 
  #                            "Belém/PA", "Dourados/MS", "C. de Itapemirim/ES", "Contagem/MG", "Porto Alegre/RS", "Novo Hamburgo/RS"))+
  scale_color_manual(
    name = "Região",
    breaks = c("Nordeste", "Norte", "Centro-Oeste", "Sudeste", "Sul"),                    # Ordem desejada das categorias
    labels = c("Nordeste", "Norte", "Centro-Oeste", "Sudeste", "Sul"),
    values = c("Nordeste"="#d96e0a", "Norte"="#cc3003", "Centro-Oeste"="#0f805e", "Sudeste"="#5766cc", "Sul"="#21367d")
    
  )+
  
  scale_y_continuous(labels = scales::number(seq(0.05,0.75,0.1),accuracy = 0.01),
                     breaks = seq(0.05,0.75,0.1),
                     limits = c(0.05,0.75)) +
  
  scale_x_discrete(labels = seq(2010,2022,1),
                     breaks = seq(2010,2022,1),
                     limits = factor(seq(2010,2024,1))) +
  
  # ggrepel::geom_text_repel(
  geom_text(
    data = labels_data,
    aes(label = muni_abrev, x = ANO, y = position, color = regiao),
    hjust = 0,
    vjust = 0,
    size = 10,
    # min.segment.length = 100,
    nudge_x = 0.1 # Adjust the horizontal position of labels
  ) +
  
  theme_bw() +# Cores especificadas para cada categoria 
  theme(#axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    text = element_text(size = 30, family = "encode_sans_light"),
    #titulo
    plot.title = element_text(size = 35, margin = margin(b=10), family = "encode_sans_bold"),
    plot.subtitle = element_text(size=10, color = "darkslategrey", margin = margin(b = 25)),
    plot.caption = element_text(size = 30, margin = margin(t=10), color = "grey70", hjust = 0),
    plot.background = element_blank(),
    #legenda
    legend.title = element_text(size = 35, family = "encode_sans_bold"),
    legend.text = element_text(size = 30, family = "encode_sans_light"),
    legend.direction = "vertical",
    legend.position = "right",
    legend.background = element_blank(),
    legend.spacing.y = unit(0.2, "cm"),
    legend.key = element_blank(),
    #Eixos
    axis.text = element_text(size = 30, family = "encode_sans_light", angle = 90),
    axis.title = element_text(size = 35, family = "encode_sans_bold")) +
  guides(size = guide_legend(order = 2)) + 
  # scale_size_continuous( range = c(4,12),
  #                        limits = c(0,2100000),
  #                        breaks = c(500000,1000000,2000000), #ajuste de escala 
  #                        name = "N° Habitantes",
  #                        guide = "legend") +
  guides(color=guide_legend(override.aes=list(size=0.5))) #ajuste tamanho da bolinha da legenda 

ggsave(grafico_mot, 
       file= "../data/DENATRAN/grafico_motorizacao6.png", 
       dpi = 350, width = 20.5, height = 13, units = "cm")

