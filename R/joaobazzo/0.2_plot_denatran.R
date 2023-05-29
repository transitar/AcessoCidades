# Libraries ----
rm(list=ls())
gc(reset = T)

# install.packages('easypackages')
easypackages::packages('geobr'
                       , 'gtfs2gps'
                       , 'openxlsx'
                       , 'ggrepel'
                       , 'data.table'
                       , 'magrittr'
                       , 'ggplot2'
                       , 'patchwork'
                       , 'mapview','ggspatial'
                       , 'raster'
                       , 'ggnewscale'
                       , 'rayshader'
                       , 'progressr'
                       , 'pbapply'
                       , 'extrafont'
                       , 'extrafontdb'
                       , 'aopdata')

den <- readr::read_rds("data/DENATRAN/DENATRAN_jan.rds")
den[,local := "country"]


#
# CIT --------------
## aumento relativo (2001-2020) / facet_grid --------------
#
den_es <- copy(den)[UF == "ES"]
den_es$MUNICIPIO %>% unique() %>% sort()
den_es[,local := fifelse(MUNICIPIO == "CACHOEIRO DE ITAPEMIRIM",
                      "city","state")]
den1 <- rbind(den,den_es,den_es[local == "city",])

names(den)
temp <- data.table::copy(den1)
temp <- temp[, 
       lapply(.SD,function(i){sum(as.numeric(i),na.rm = TRUE)}
              ),by = .(local,ANO)
       ,.SDcols = c("TOTAL_AUTOS","TOTAL_MOTOS","pop")]
temp <- temp[,list(
  "AUTOS_PER_POP" = sum(TOTAL_AUTOS)/sum(pop),
  "TOTAL_POP" = sum(pop),
  "TOTAL_AUTOS" = sum(TOTAL_AUTOS),
  "MOTOS_PER_POP" = sum(TOTAL_MOTOS)/sum(pop),
  "MOTO_AUTO_PER_POP" = (sum(TOTAL_MOTOS) + sum(TOTAL_AUTOS))/sum(pop),
  "TOTAL_MOTOS" = sum(TOTAL_MOTOS))
  , by = .(local,ANO)]

temp[]

temp[,AUTOS_PER_POP_rel := AUTOS_PER_POP/min(AUTOS_PER_POP), by = .(local)]
temp[,MOTOS_PER_POP_rel := MOTOS_PER_POP/min(MOTOS_PER_POP), by = .(local)]

temp_br <- data.table::melt(data = temp,
                            id.vars = c('local','ANO'),
                            measure.vars =  c('AUTOS_PER_POP','MOTOS_PER_POP',
                                              'AUTOS_PER_POP_rel','MOTOS_PER_POP_rel'))
temp_br[,type := fifelse(variable %like% "_rel","rel","abs") ]
temp_br[,variable := gsub("_rel","",variable)]

temp_br[,ano_f := factor(ANO
                         ,levels = 2010:2021
                         ,labels = 10:21)]
temp_br[,variable_f := factor(variable
                              ,levels = c('AUTOS_PER_POP'
                                          ,'MOTOS_PER_POP')
                              ,labels = c('Automóveis'
                                          ,'Motocicletas'))]
temp_br[,local_f := factor(local
                         ,levels = c('city'
                                     ,'state','country')
                         ,labels = c('Cachoeiro de Itapemirim / ES'
                                     ,'Estado do ES','Brasil'))]
## check niveis -----
temp_br[ANO == 2021,]
temp_br[ANO == 2021 & variable %like% "AUTOS",]
temp_br[ANO == 2021 & type == "abs" & local == "city",]
temp_br[ANO == 2021 & type == "rel" & local == "city",]
temp_br[ANO == 2021 & type == "rel",]

#
pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")
## abs -----------
p1 <- ggplot(temp_br[type == "abs"],
             aes(x = ano_f,y = value, group = local_f)) +
  geom_line(aes(color = local_f), linewidth=.8, alpha=.8) +
  geom_point(aes(color = local_f), shape = 1,size = 0.75)+
  scale_color_manual(values = pal[c(1,3,2)]) + 
  scale_x_discrete(breaks = c(10,13,15,17,19,21))+
  facet_wrap(facets = vars(variable_f),scales = "free") +
  scale_y_continuous(n.breaks = 6)+
  labs(color = "Local"
       ,x = NULL,y = "Taxa de Motorização\n(veículos/habitante)")+
  theme(legend.position = "none",
        plot.margin=unit(c(0,2,0,1),"mm"),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        panel.background = element_rect(fill = "white",colour = NA),
        panel.grid.major.x = element_line(colour = "grey92",linewidth = 0.35),
        legend.title=element_text(size=rel(0.9)),
        strip.text = element_text(size=rel(0.85)) ,
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92"),
        axis.title.y = element_text(size=rel(0.85)),
        axis.ticks = element_line(linewidth = 0.5,colour = "grey92"),
        #axis.text.x = element_text(angle = 0,vjust = 0.75),
        #axis.text.x = element_blank(),
        text = element_text(family = "Encode Sans"))

p1
## relativo -=----
p2 <- ggplot(temp_br[type == "rel"],
             aes(x = ano_f,y = value, group = local_f)) +
  geom_line(aes(color = local_f), linewidth=.8, alpha=.8) +
  geom_point(aes(color = local_f), shape = 1,size = 0.75)+
  scale_color_manual(values = pal[c(1,3,2)]) + 
  scale_x_discrete(breaks = c(10,13,15,17,19,21))+
  scale_y_continuous(n.breaks = 6)+
  facet_wrap(facets = vars(variable_f),scales = "free") +
  labs(color = "Região"
       ,x = "Ano",y = "Crescimento da motorização\nem relação a 2010"
       ,caption = "Fonte: DENATRAN (2010 - 2021)")+
  theme(legend.position = "bottom",
        plot.margin=unit(c(2,0,0,1),"mm"),
        plot.caption = element_text(size=rel(0.75)),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        strip.text = element_text(size=rel(0.85)) ,
        strip.background =  element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.grid.major.x = element_line(colour = "grey92",linewidth = 0.35),
        panel.background = element_rect(fill = "white",colour = NA),
        legend.title=element_text(size=rel(0.9)),
        axis.title.y = element_text(size=rel(0.85)),
        axis.text.x = element_text(angle = 0,vjust = 0.75),
        axis.ticks = element_line(linewidth = 0.5,colour = "grey92"),
       # axis.line = element_line(colour = "grey92"),
        text = element_text(family = "Encode Sans")) +
  guides(color = guide_legend(title.position = "top"
                              ,title.hjust = 0.5))

p2

pf <- p1 / p2

ggsave("figures/bra_cit/tx_motorizacao.png",scale=0.85,
       width = 20,height = 17.5,dpi = 300,units = "cm")
#
# MANAUS --------------
## aumento relativo (2001-2020) / facet_grid --------------
#
den_es <- copy(den)[UF == "AM"]
den_es$MUNICIPIO %>% unique() %>% sort()
den_es[,local := fifelse(MUNICIPIO == "MANAUS",
                         "city","state")]
den1 <- rbind(den,den_es,den_es[local == "city",])

names(den)
temp <- data.table::copy(den1)
temp <- temp[, 
             lapply(.SD,function(i){sum(as.numeric(i),na.rm = TRUE)}
             ),by = .(local,ANO)
             ,.SDcols = c("TOTAL_AUTOS","TOTAL_MOTOS","pop")]
temp <- temp[,list(
  "AUTOS_PER_POP" = sum(TOTAL_AUTOS)/sum(pop),
  "TOTAL_POP" = sum(pop),
  "TOTAL_AUTOS" = sum(TOTAL_AUTOS),
  "MOTOS_PER_POP" = sum(TOTAL_MOTOS)/sum(pop),
  "MOTO_AUTO_PER_POP" = (sum(TOTAL_MOTOS) + sum(TOTAL_AUTOS))/sum(pop),
  "TOTAL_MOTOS" = sum(TOTAL_MOTOS))
  , by = .(local,ANO)]

temp[]

temp[,AUTOS_PER_POP_rel := AUTOS_PER_POP/min(AUTOS_PER_POP), by = .(local)]
temp[,MOTOS_PER_POP_rel := MOTOS_PER_POP/min(MOTOS_PER_POP), by = .(local)]

temp_br <- data.table::melt(data = temp,
                            id.vars = c('local','ANO'),
                            measure.vars =  c('AUTOS_PER_POP','MOTOS_PER_POP',
                                              'AUTOS_PER_POP_rel','MOTOS_PER_POP_rel'))
temp_br[,type := fifelse(variable %like% "_rel","rel","abs") ]
temp_br[,variable := gsub("_rel","",variable)]

temp_br[,ano_f := factor(ANO
                         ,levels = 2010:2021
                         ,labels = 10:21)]
temp_br[,variable_f := factor(variable
                              ,levels = c('AUTOS_PER_POP'
                                          ,'MOTOS_PER_POP')
                              ,labels = c('Automóveis'
                                          ,'Motocicletas'))]
temp_br[,local_f := factor(local
                           ,levels = c('city'
                                       ,'state','country')
                           ,labels = c('Manaus / AM'
                                       ,'Estado do AM','Brasil'))]
## check niveis -----
temp_br[ANO == 2021,]
temp_br[ANO == 2021 & variable %like% "AUTOS",]
temp_br[ANO == 2021 & type == "abs" & local == "city",]
temp_br[ANO == 2021 & type == "rel" & local == "city",]
temp_br[ANO == 2021 & type == "rel",]

#
pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")
## abs -----------
p1 <- ggplot(temp_br[type == "abs"],
             aes(x = ano_f,y = value, group = local_f)) +
  geom_line(aes(color = local_f), linewidth=.8, alpha=.8) +
  geom_point(aes(color = local_f), shape = 1,size = 0.75)+
  scale_color_manual(values = pal[c(1,3,2)]) + 
  scale_x_discrete(breaks = c(10,13,15,17,19,21))+
  facet_wrap(facets = vars(variable_f),scales = "free") +
  scale_y_continuous(n.breaks = 6)+
  labs(color = "Local"
       ,x = NULL,y = "Taxa de Motorização\n(veículos/habitante)")+
  theme(legend.position = "none",
        plot.margin=unit(c(0,2,0,1),"mm"),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        panel.background = element_rect(fill = "white",colour = NA),
        panel.grid.major.x = element_line(colour = "grey92",linewidth = 0.35),
        legend.title=element_text(size=rel(0.9)),
        strip.text = element_text(size=rel(0.85)) ,
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92"),
        axis.title.y = element_text(size=rel(0.85)),
        axis.ticks = element_line(linewidth = 0.5,colour = "grey92"),
        #axis.text.x = element_text(angle = 0,vjust = 0.75),
        #axis.text.x = element_blank(),
        text = element_text(family = "Encode Sans"))

p1
## relativo -=----
p2 <- ggplot(temp_br[type == "rel"],
             aes(x = ano_f,y = value, group = local_f)) +
  geom_line(aes(color = local_f), linewidth=.8, alpha=.8) +
  geom_point(aes(color = local_f), shape = 1,size = 0.75)+
  scale_color_manual(values = pal[c(1,3,2)]) + 
  scale_x_discrete(breaks = c(10,13,15,17,19,21))+
  scale_y_continuous(n.breaks = 6)+
  facet_wrap(facets = vars(variable_f),scales = "free") +
  labs(color = "Região"
       ,x = "Ano",y = "Crescimento da motorização\nem relação a 2010"
       ,caption = "Fonte: DENATRAN (2010 - 2021)")+
  theme(legend.position = "bottom",
        plot.margin=unit(c(2,0,0,1),"mm"),
        plot.caption = element_text(size=rel(0.75)),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        strip.text = element_text(size=rel(0.85)) ,
        strip.background =  element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.grid.major.x = element_line(colour = "grey92",linewidth = 0.35),
        panel.background = element_rect(fill = "white",colour = NA),
        legend.title=element_text(size=rel(0.9)),
        axis.title.y = element_text(size=rel(0.85)),
        axis.text.x = element_text(angle = 0,vjust = 0.75),
        axis.ticks = element_line(linewidth = 0.5,colour = "grey92"),
        # axis.line = element_line(colour = "grey92"),
        text = element_text(family = "Encode Sans")) +
  guides(color = guide_legend(title.position = "top"
                              ,title.hjust = 0.5))

p2

pf <- p1 / p2
pf
ggsave("figures/bra_man/tx_motorizacao.png",scale=0.85,
       width = 20,height = 17.5,dpi = 300,units = "cm")
# VITORIA DA CONQUISTA  --------------
## aumento relativo (2001-2020) / facet_grid --------------
#
dir.create("figures/bra_vit/")
den_es <- copy(den)[UF == "BA"]
den_es$MUNICIPIO %>% unique() %>% sort()
den_es[,local := fifelse(MUNICIPIO == "VITORIA DA CONQUISTA",
                         "city","state")]
den1 <- rbind(den,den_es,den_es[local == "city",])

names(den)
temp <- data.table::copy(den1)
temp <- temp[, 
             lapply(.SD,function(i){sum(as.numeric(i),na.rm = TRUE)}
             ),by = .(local,ANO)
             ,.SDcols = c("TOTAL_AUTOS","TOTAL_MOTOS","pop")]
temp <- temp[,list(
  "AUTOS_PER_POP" = sum(TOTAL_AUTOS)/sum(pop),
  "TOTAL_POP" = sum(pop),
  "TOTAL_AUTOS" = sum(TOTAL_AUTOS),
  "MOTOS_PER_POP" = sum(TOTAL_MOTOS)/sum(pop),
  "MOTO_AUTO_PER_POP" = (sum(TOTAL_MOTOS) + sum(TOTAL_AUTOS))/sum(pop),
  "TOTAL_MOTOS" = sum(TOTAL_MOTOS))
  , by = .(local,ANO)]

temp[]

temp[,AUTOS_PER_POP_rel := AUTOS_PER_POP/min(AUTOS_PER_POP), by = .(local)]
temp[,MOTOS_PER_POP_rel := MOTOS_PER_POP/min(MOTOS_PER_POP), by = .(local)]

temp_br <- data.table::melt(data = temp,
                            id.vars = c('local','ANO'),
                            measure.vars =  c('AUTOS_PER_POP','MOTOS_PER_POP',
                                              'AUTOS_PER_POP_rel','MOTOS_PER_POP_rel'))
temp_br[,type := fifelse(variable %like% "_rel","rel","abs") ]
temp_br[,variable := gsub("_rel","",variable)]

temp_br[,ano_f := factor(ANO
                         ,levels = 2010:2021
                         ,labels = 10:21)]
temp_br[,variable_f := factor(variable
                              ,levels = c('AUTOS_PER_POP'
                                          ,'MOTOS_PER_POP')
                              ,labels = c('Automóveis'
                                          ,'Motocicletas'))]
temp_br[,local_f := factor(local
                           ,levels = c('city'
                                       ,'state','country')
                           ,labels = c('Vitória da Conquista / BA'
                                       ,'Estado da BA','Brasil'))]
## check niveis -----
temp_br[ANO == 2021,]
temp_br[ANO == 2021 & variable %like% "AUTOS",]
temp_br[ANO == 2021 & type == "abs" & local == "city",]
temp_br[ANO == 2021 & type == "rel" & local == "city",]
temp_br[ANO == 2021 & type == "rel",]

#
pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")
## abs -----------
p1 <- ggplot(temp_br[type == "abs"],
             aes(x = ano_f,y = value, group = local_f)) +
  geom_line(aes(color = local_f), linewidth=.8, alpha=.8) +
  geom_point(aes(color = local_f), shape = 1,size = 0.75)+
  scale_color_manual(values = pal[c(1,3,2)]) + 
  scale_x_discrete(breaks = c(10,13,15,17,19,21))+
  facet_wrap(facets = vars(variable_f),scales = "free") +
  scale_y_continuous(n.breaks = 6)+
  labs(color = "Local"
       ,x = NULL,y = "Taxa de Motorização\n(veículos/habitante)")+
  theme(legend.position = "none",
        plot.margin=unit(c(0,2,0,1),"mm"),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        panel.background = element_rect(fill = "white",colour = NA),
        panel.grid.major.x = element_line(colour = "grey92",linewidth = 0.35),
        legend.title=element_text(size=rel(0.9)),
        strip.text = element_text(size=rel(0.85)) ,
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92"),
        axis.title.y = element_text(size=rel(0.85)),
        axis.ticks = element_line(linewidth = 0.5,colour = "grey92"),
        #axis.text.x = element_text(angle = 0,vjust = 0.75),
        #axis.text.x = element_blank(),
        text = element_text(family = "Encode Sans"))

p1
## relativo -=----
p2 <- ggplot(temp_br[type == "rel"],
             aes(x = ano_f,y = value, group = local_f)) +
  geom_line(aes(color = local_f), linewidth=.8, alpha=.8) +
  geom_point(aes(color = local_f), shape = 1,size = 0.75)+
  scale_color_manual(values = pal[c(1,3,2)]) + 
  scale_x_discrete(breaks = c(10,13,15,17,19,21))+
  scale_y_continuous(n.breaks = 6)+
  facet_wrap(facets = vars(variable_f),scales = "free") +
  labs(color = "Região"
       ,x = "Ano",y = "Crescimento da motorização\nem relação a 2010"
       ,caption = "Fonte: DENATRAN (2010 - 2021)")+
  theme(legend.position = "bottom",
        plot.margin=unit(c(2,0,0,1),"mm"),
        plot.caption = element_text(size=rel(0.75)),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        strip.text = element_text(size=rel(0.85)) ,
        strip.background =  element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92"),
        panel.grid.major.x = element_line(colour = "grey92",linewidth = 0.35),
        panel.background = element_rect(fill = "white",colour = NA),
        legend.title=element_text(size=rel(0.9)),
        axis.title.y = element_text(size=rel(0.85)),
        axis.text.x = element_text(angle = 0,vjust = 0.75),
        axis.ticks = element_line(linewidth = 0.5,colour = "grey92"),
        # axis.line = element_line(colour = "grey92"),
        text = element_text(family = "Encode Sans")) +
  guides(color = guide_legend(title.position = "top"
                              ,title.hjust = 0.5))

p2

pf <- p1 / p2
pf
ggsave("figures/bra_vit/tx_motorizacao.png",scale=0.85,
       width = 20,height = 17.5,dpi = 300,units = "cm")
# End --------