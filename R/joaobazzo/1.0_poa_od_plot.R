rm(list=ls())

library(data.table)
library(janitor)
library(ggplot2)
library(magrittr)
library()
library()

#c1 <- c('Total de viagens', 23902,512895,241653,392613,153443,565783,311688,2201977,1190,2203168)
c2 <- c('Viagens de TI', 4895,129873,56388,78533,36481,131063,110799,548032,453,548485)
c3 <- c('Viagens de TP', 14793,237204,109069,131962,77104,250514,132533,953181,448,953629)
c4 <- c('Viagens a Pé /bicicleta', 2548,130529,71889,167691,38418,167229,60539,638842,233,639075)
c5 <- c('Viagens em outros modos', 1666,15290,4307,14426,1440,16977,7817,61922,56,61978)

cdt <- do.call(what = rbind,args = list(c2,c3,c4,c5))
cdt <- as.data.frame(t(cdt))
names(cdt) <- janitor::make_clean_names(cdt[1,])
cdt <- cdt[-1,]
cdt <- as.data.table(cdt)

# remove tres ultimas linhas
cdt <- cdt[1:(.N-3),]

cdt$horario <- c("00:00 - 05:59","06:00 - 08:59","09:00 - 11:59","12:00 - 13:59",
                 "14:00 - 15:59","16:00 - 18:59","19:00 - 23:59")

cdt <- data.table::melt.data.table(data = cdt
                                   ,id.vars = "horario"
                                   ,measure.vars = setdiff(names(cdt),"horario"))
cdt[,value := as.numeric(value)]
cdt[,perc := round(100 * value / sum(value),1)]

cdt[,variable_f := factor(x = variable
                          ,levels = c("viagens_de_ti","viagens_de_tp"
                                       ,"viagens_a_pe_bicicleta","viagens_em_outros_modos")
                          ,labels = c("Transporte \nIndividual"
                                      ,"Transporte público"
                                      ,"A pé / Bicicleta","Outros"))]

cdt[,horario_f := factor(x = horario
                         ,levels = c("00:00 - 05:59", "06:00 - 08:59"
                                     , "09:00 - 11:59", "12:00 - 13:59"
                                     ,"14:00 - 15:59","16:00 - 18:59"
                                     , "19:00 - 23:59")
                         ,labels = c("00:00 -\n05:59", "06:00 -\n08:59"
                                     , "09:00 -\n11:59", "12:00 -\n13:59",
                                     "14:00 -\n15:59","16:00 -\n18:59",
                                     "19:00 -\n23:59"))]
cdt[]
pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")

sum_cdt <- cdt[,sum(perc),by = horario_f]
sum_cdt <- cdt[,{
  total <- sum(perc) 
  v2 <- cumsum(perc)
  v3 <- total - v2
  v4 <- v3 + perc/2
  list(variable_f,total,"total_label" = paste(total,"%"),"y" = v4,"label" = perc)
  },by = .(horario_f)]
sum_cdt

ggplot(data = cdt) + 
  geom_col(aes(x = horario_f,y = perc,fill = variable_f)
           ,position = "stack")+
  #geom_col(aes(x = horario_f,y = perc,fill = variable_f)
  #         ,position = "dodge")+
  labs(y = "Percentual (%)",fill = "Modo de\ntransporte"
       ,x = NULL)+
  geom_text(data = sum_cdt[,.SD[1],by = .(horario_f)]
            ,aes(x = horario_f,y = total,label = total_label,fontface=2)
            ,vjust =-0.25,hjust = 0.5
            ,family = "Encode Sans",  size= 3.15)+
  geom_text(data = sum_cdt[horario_f != "00:00 -\n05:59" & 
                             variable_f != "Outros"]
            ,aes(x = horario_f,y = y,label = label)
            ,vjust =-0.25,hjust = 0.5,color = "grey90"
            ,family = "Encode Sans",  size= 3.0)+
  scale_fill_manual(values = pal)+
  theme(legend.position = "right",
        plot.margin=unit(c(0,2,0,1),"mm"),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        panel.background = element_rect(fill = "white",colour = NA),
        legend.title=element_text(size=rel(0.9)),
        strip.text = element_text(size=rel(1.3)) ,
        panel.grid.major.y = element_line(colour = "grey92"),
        axis.text.x = element_text(angle = 0,vjust = 0.75),
        text = element_text(family = "Encode Sans"))

ggplot2::ggsave(filename = "figures/poa_od_periods.png"
                , scale = 0.85
                , width = 21
                , bg = "white"
                , height = 14
                , units = "cm"
                , dpi = 300)

