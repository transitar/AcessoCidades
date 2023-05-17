# Load ----
rm(list=ls())
gc(reset = TRUE)
easypackages::packages('data.table'
                       ,'magrittr'
                       ,'ggplot2'
                       , 'patchwork')

#' @convencoes Pallete
#' @param gender = "Reds"
#' @param idade = "Greens"
#' @param activ = "Blues"
#' @param race = "Purples"
#' 
## CIT  -----------
rm(list=ls())
gc(reset = TRUE)
dt_acid <- readr::read_rds("data/datasus/muni_by_mode.rds")
dt_acid <- dt_acid[causa_name == "total",]
esIcities <- geobr::read_municipality() %>% 
  setDT() %>% .[abbrev_state == "ES",code_muni] %>% 
  as.character()

# acidentes
dados_acid <- rbind(copy(dt_acid)[municipio_codigo == "3201209"
                                  ,area := "Cachoeiro de Itapemirim"],
                    copy(dt_acid)[municipio_codigo %in% esIcities
                                  ,area := "Estado do ES"],
                    copy(dt_acid)[,area := "Brasil"])
dados_acid <- dados_acid[!is.na(area)]
dados_acid <- dados_acid[,list(
  "deaths" = sum(deaths,na.rm = TRUE)
),by = .(area,year)]

dados_acid[year == 2019,]
# populacao
dt_pop_muni <- copy(dt_acid)[,list("pop" = pop[1])
                             ,by = .(year,municipio_codigo)]
dt_pop <- rbindlist(list(
  copy(dt_pop_muni)[municipio_codigo == "3201209",] %>% 
    .[,area := "Cachoeiro de Itapemirim"] ,
  copy(dt_pop_muni)[municipio_codigo %in% esIcities,
                    list("pop" = sum(pop,na.rm = TRUE))
                    ,by = .(year)] %>% 
    .[,area := "Estado do ES"],
  copy(dt_pop_muni)[, list("pop" = sum(pop,na.rm = TRUE))
                    ,by = .(year)] %>% 
    .[,area := "Brasil"]
)
,use.names = TRUE,fill = TRUE)
dt_pop[,municipio_codigo := NULL]

# merge data
dados_acid[1]
dt_pop[1]
dados_acid[dt_pop ,on = c("year","area"),pop := i.pop]
dados_acid[,year_f := as.character(gsub("^20","",year))]
dados_acid <- dados_acid[!is.na(area)]

dados_acid[,deaths_rel := deaths / (pop / 100000 )]

pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")


dados_acid[year == 2019]
ggplot(dados_acid,
       aes(x = as.factor(year_f),y = deaths_rel,group = area)) +
  geom_line(aes(color = area), linewidth=.8, alpha=.8) +
  geom_point(aes(color = area), shape = 1,size = 0.75)+
  scale_color_manual(values = pal[c(1,3,2)]) + 
  # scale_x_discrete(breaks = c(10,13,15,17,19,21))+
  #facet_wrap(facets = vars(variable_f),scales = "free") +
  scale_y_continuous(n.breaks = 8)+
  labs(color = "Local"
       ,x = NULL
       ,y = "Óbitos a cada 100 mil hab."
       ,caption = "Fonte: DATASUS (2013 - 2019).")+
  theme(legend.position = "right",
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

ggsave("figures/bra_cit/tx_obito.png",scale=0.85,
       width = 15,height = 10,dpi = 300,units = "cm")
## Manaus  -----------
rm(list=ls())
gc(reset = TRUE)
dt_acid <- readr::read_rds("data/datasus/muni_by_mode.rds")
dt_acid <- dt_acid[causa_name == "total",]
esIcities <- geobr::read_municipality() %>% 
  setDT() %>% .[abbrev_state == "AM",code_muni] %>% 
  as.character()

# acidentes
dados_acid <- rbind(copy(dt_acid)[municipio_codigo == "1302603"
                                  ,area := "Manaus"],
                    copy(dt_acid)[municipio_codigo %in% esIcities
                                  ,area := "Estado do AM"],
                    copy(dt_acid)[,area := "Brasil"])
dados_acid <- dados_acid[!is.na(area)]
dados_acid <- dados_acid[,list(
  "deaths" = sum(deaths,na.rm = TRUE)
),by = .(area,year)]

dados_acid[year == 2019,]
# populacao
dt_pop_muni <- copy(dt_acid)[,list("pop" = pop[1])
                             ,by = .(year,municipio_codigo)]
dt_pop <- rbindlist(list(
  copy(dt_pop_muni)[municipio_codigo == "1302603",] %>% 
    .[,area := "Manaus"] ,
  copy(dt_pop_muni)[municipio_codigo %in% esIcities,
                    list("pop" = sum(pop,na.rm = TRUE))
                    ,by = .(year)] %>% 
    .[,area := "Estado do AM"],
  copy(dt_pop_muni)[, list("pop" = sum(pop,na.rm = TRUE))
                    ,by = .(year)] %>% 
    .[,area := "Brasil"]
)
,use.names = TRUE,fill = TRUE)
dt_pop[,municipio_codigo := NULL]

# merge data
dados_acid[1]
dt_pop[1]
dados_acid[dt_pop ,on = c("year","area"),pop := i.pop]
dados_acid[,year_f := as.character(gsub("^20","",year))]
dados_acid <- dados_acid[!is.na(area)]

dados_acid[,deaths_rel := deaths / (pop / 100000 )]

pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")


dados_acid[year == 2019]
ggplot(dados_acid,
       aes(x = as.factor(year_f),y = deaths_rel,group = area)) +
  geom_line(aes(color = area), linewidth=.8, alpha=.8) +
  geom_point(aes(color = area), shape = 1,size = 0.75)+
  scale_color_manual(values = pal[c(1,3,2)]) + 
  # scale_x_discrete(breaks = c(10,13,15,17,19,21))+
  #facet_wrap(facets = vars(variable_f),scales = "free") +
  scale_y_continuous(n.breaks = 8)+
  labs(color = "Local"
       ,x = NULL
       ,y = "Óbitos a cada 100 mil hab."
       ,caption = "Fonte: DATASUS (2013 - 2019).")+
  theme(legend.position = "right",
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

ggsave("figures/bra_man/tx_obito.png",scale=0.85,
       width = 15,height = 10,dpi = 300,units = "cm")

# end ----