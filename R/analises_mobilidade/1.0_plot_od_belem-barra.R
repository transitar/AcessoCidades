rm(list=ls())

library(data.table)
library(janitor)
library(ggplot2)
library(magrittr)


c1 <-c(rep(NA,6),40563,21250,18585,19408,19452,20071)
c2 <-c(23114,16667,19185,18986,18033,19021,40913,23242,19078,19450,19820,21815) 
c3 <-c(24635,19679,17205,20582,19329,21125,41019,22315,18989,20787,20286,NA)

mes <- c("Jan","Fev","Mar","Abr","Mai","Jun"
         ,"Jul","Ago","Set","Out","Nov","Dez")
cdt <- data.table(year = c(rep(2013,12),
                           rep(2014,12),
                           rep(2015,12)),
                  mes = rep(mes,3),
                  total = c(c1,c2,c3))
cdt[,mes_f := factor(x = mes,levels = c("Jan","Fev","Mar","Abr","Mai","Jun"
                                        ,"Jul","Ago","Set","Out","Nov","Dez"))]
pal <- c("#5766cc","#33b099","#d96e0a")

ggplot(data = cdt) + 
  geom_col(aes(x = mes_f,y = total,fill = as.factor(year))
           ,position = "dodge2")+
  #geom_col(aes(x = horario_f,y = perc,fill = variable_f)
  #         ,position = "dodge")+
  labs(y = "Volume de demanda total",fill = "Ano"
       ,caption = "Dados indisponíveis em: Jan-Jun/2013 e Dez/2015."
       ,x = NULL)+
  scale_fill_manual(values = pal)+
  theme(legend.position = "right",
        plot.margin=unit(c(0,2,0,1),"mm"),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        panel.background = element_rect(fill = "white",colour = NA),
        legend.title=element_text(size=rel(0.9))
        ,plot.caption = element_text(size=rel(0.55)),
        strip.text = element_text(size=rel(1.3)) ,
        panel.grid.major.y = element_line(colour = "grey92"),
        axis.text.x = element_text(angle = 0,vjust = 0.75),
        text = element_text(family = "Encode Sans"))

ggplot2::ggsave(filename = "figures/bra_belem/fluxo_periods.png"
                , scale = 0.85
                , width = 21
                , bg = "white"
                , height = 10
                , units = "cm"
                , dpi = 300)

  