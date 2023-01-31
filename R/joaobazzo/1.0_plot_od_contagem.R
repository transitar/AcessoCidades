rm(list=ls())

library(data.table)
library(janitor)
library(ggplot2)
library(magrittr)


# Renda ------
c1 <- c("Municipais","10 - 15 SM"    ,78,7,15)
c2 <- c("Municipais","5 - 10 SM"     ,78,6,16)
c3 <- c("Municipais","3 - 5 SM"      ,66,11,23)
c4 <- c("Municipais","2 - 3 SM"      ,54,16,30)
c5 <- c("Municipais","1 - 2 SM"      ,27,27,46)
c6 <- c("Municipais","Menos que 1 SM",14,28,58)
c7 <- c("Municipais","Sem renda"     ,14,17,69)

d1 <- c("Intermunicipais","10 - 15 SM"    ,77,23,0)
d2 <- c("Intermunicipais","5 - 10 SM"     ,66,34,0)
d3 <- c("Intermunicipais","3 - 5 SM"      ,59,41,0)
d4 <- c("Intermunicipais","2 - 3 SM"      ,48,48,4)
d5 <- c("Intermunicipais","1 - 2 SM"      ,33,61,5)
d6 <- c("Intermunicipais","Menos que 1 SM",17,70,13)
d7 <- c("Intermunicipais","Sem renda"     ,24,60,16)

dt <- do.call(rbind,list(c1,c2,c3,c4,c5,c6,c7,d1,d2,d3,d4,d5,d6,d7))
dt <- data.table::data.table(dt)
names(dt) <- c("Tipo","Renda","Individual","Coletivo","Não-motorizado")

dt


cdt <- data.table::melt.data.table(data = dt
                                   ,id.vars = c("Tipo","Renda")
                                   ,measure.vars = setdiff(names(dt),c("Tipo","Renda"))
                                   ,variable.name = "Modo"
                                   ,value.name = "pp")
cdt
cdt[,pp := as.numeric(pp)]

cdt[,Tipo := factor(x = Tipo
                    ,levels = c("Municipais","Intermunicipais"))]
cdt[,Modo_f := factor(x = Modo
                      ,levels = c("Não-motorizado",
                                  "Coletivo",
                                  "Individual"))]
cdt[,Renda_f := factor(x = Renda
                       ,levels = c("Sem renda"
                                   ,"Menos que 1 SM"
                                   ,"1 - 2 SM"
                                   ,"2 - 3 SM"
                                   ,"3 - 5 SM"
                                   ,"5 - 10 SM"
                                   ,"10 - 15 SM"
                       ))]
cdt <- cdt[order(Tipo)]
cdt <- cdt[order(Modo_f)]


sum_cdt <- cdt[,{
  total <- sum(pp) 
  v2 <- cumsum(pp)
  v3 <- total - v2
  v4 <- v3 + pp/2
  list(Modo_f,total,"total_label" = paste(total,"%"),"y" = v4,"label" = pp)
},by = .(Renda_f,Tipo)]
sum_cdt

pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")

cdt[sum_cdt, on = c("Renda_f","Modo_f","Tipo")
    ,":="(label = i.label,
          y = i.y)]
cdt[label == 0,label := ""]

ggplot(cdt) + 
  geom_col(aes(x = Renda_f,y = pp,fill = Modo_f)
           ,position = "stack")+
  #geom_col(aes(x = horario_f,y = perc,fill = variable_f)
  #         ,position = "dodge")+
  scale_y_continuous(expand = c(0, 0))+
  labs(y = "Percentual (%)",fill = "Modo de\ntransporte"
       ,x = NULL)+
  geom_text(aes(x = Renda_f,y = y,label = label,fontface=2)
            ,vjust =-0.10,hjust = +0.2,color = "grey20"
            ,family = "Encode Sans",  size= 3)+
  scale_fill_manual(values = pal
                    ,breaks = c("Individual","Coletivo","Não-motorizado"))+
  facet_grid(~Tipo)+
  coord_flip()+
  theme(legend.position = "bottom",
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

#dir.create("figures/bra_contagem/")
ggplot2::ggsave(filename = "figures/bra_contagem/od_periods.png"
                , scale = 0.85
                , width = 21
                , bg = "white"
                , height = 14
                , units = "cm"
                , dpi = 300)

# Genero -----
rm(list=ls())

c1 <- c("Municipais","Total"    ,20.7,26.3,53)
c2 <- c("Municipais","Masculino",17.4,35,47.6)
c3 <- c("Municipais","Feminino" ,24,17.6,58.4)

d1 <- c("Intermunicipais","Total"    ,54.9,38.2,6.9)
d2 <- c("Intermunicipais","Masculino",44.6,48.9,6.5)
d3 <- c("Intermunicipais","Feminino" ,66.6,26.1,7.3)

dt <- do.call(rbind,list(c1,c2,c3,d1,d2,d3))
dt <- data.table::data.table(dt)
names(dt) <- c("Tipo","Gênero","Coletivo","Individual","Não-motorizado")

dt


cdt <- data.table::melt.data.table(data = dt
                                   ,id.vars = c("Tipo","Gênero")
                                   ,measure.vars = setdiff(names(dt),c("Tipo","Gênero"))
                                   ,variable.name = "Modo"
                                   ,value.name = "pp")
cdt
cdt[,pp := as.numeric(pp)]

cdt[,Tipo := factor(x = Tipo
                    ,levels = c("Municipais","Intermunicipais"))]
cdt[,Modo_f := factor(x = Modo
                      ,levels = c("Não-motorizado",
                                  "Coletivo",
                                  "Individual"))]
cdt[,Genero_f := factor(x = Gênero
                        ,levels = c("Total"
                                    ,"Masculino"
                                    ,"Feminino"))]
cdt <- cdt[order(Tipo)]
cdt <- cdt[order(Modo_f)]
cdt <- cdt[order(Genero_f,decreasing = T)]


sum_cdt <- cdt[,{
  total <- sum(pp) 
  v2 <- cumsum(pp)
  v3 <- total - v2
  v4 <- v3 + pp/2
  list(Modo_f,total,"total_label" = paste(total,"%"),"y" = v4,"label" = pp)
},by = .(Genero_f,Tipo)]
sum_cdt

#pal <- c("#5766cc","#d96e0a","#33b099","#cc3003")
pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")
cdt[sum_cdt, on = c("Genero_f","Modo_f","Tipo")
    ,":="(label = i.label,
          y = i.y)]
cdt[label == 0,label := ""]
cdt <- cdt[order(Genero_f,decreasing = T)]

ggplot(cdt) + 
  geom_col(aes(x = Genero_f,y = pp,fill = Modo_f)
           ,position = "stack",width = 0.5)+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_discrete(breaks = c("Total","Masculino","Feminino"))+
  labs(y = "Percentual (%)",fill = "Modo de\ntransporte"
       ,x = NULL)+
  geom_text(aes(x = Genero_f,y = y,label = label,fontface=2)
            ,vjust =+0.30,hjust = +0.8,color = "grey20"
            ,family = "Encode Sans",  size= 2.5)+
  scale_fill_manual(values = pal
                    ,breaks = c("Individual","Coletivo","Não-motorizado"))+
  facet_grid(~Tipo)+
  coord_flip()+
  theme(legend.position = "bottom",
        plot.margin=unit(c(1,3,0,1),"mm"),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        panel.background = element_rect(fill = "white",colour = NA),
        legend.title=element_text(size=rel(0.9)),
        strip.text = element_text(size=rel(1.3)) ,
        panel.grid.major.y = element_line(colour = "grey92"),
        axis.text.x = element_text(angle = 0,vjust = 0.75),
        text = element_text(family = "Encode Sans"))

#dir.create("figures/bra_contagem/")
ggplot2::ggsave(filename = "figures/bra_contagem/od_periods_gen.png"
                , scale = 0.85
                , width = 18
                , bg = "white"
                , height = 10
                , units = "cm"
                , dpi = 300)
# End-----