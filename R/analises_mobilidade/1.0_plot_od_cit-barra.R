# PLOT 1------
rm(list=ls())

library(data.table)
library(janitor)
library(ggplot2)
library(magrittr)

v1 <- c(255.12,194.97,244.23,253.56,239.56,292.45,267.56,325.12)
v1 <- v1/max(v1)
v1 <- v1 * 3.69
v2 <- c('Tempo de viagem', 'Cobertura'
        ,'Tempo de espera', 'Segurança'
        ,'Conforto', 'Limpeza'
        ,'Informação\nao usuário'
        ,'Condutores')
df <- data.table("name" = v2,"value" = v1)
order_name <- df[order(value,decreasing = FALSE),.SD,.SDcols = "name"]
order_name <- order_name$name
order_name
df[,name_f := factor(x = name
                     ,levels = order_name
                     ,labels = order_name)]
df

pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")


ggplot(data = df) + 
  geom_col(aes(x = name_f,y = value)
           ,position = "dodge2"
           ,fill = "grey85"
           ,color = "black"
           ,width = .5) + 
  scale_y_continuous(breaks = 1:5
                     ,labels = paste0(
                       c('Péssimo','Ruim','Regular','Bom','Ótimo')
                       ," (",1:5,")")
                     ,limits = c(0,5))+
  labs(y = NULL,fill = NULL,x = NULL
       ,title = "Avaliação do transporte público"
       ,caption = "Fonte: Adaptado do Plano de Mobilidade Urbana (LOGIT, 2013).") +
  geom_text(aes(x = name_f,y = value,label = round(value,2),fontface=2)
            ,vjust =-0.25,hjust = 0.5
            ,family = "Encode Sans",  size= 3.15)+
  theme(legend.position = "none",
        plot.margin=unit(c(1,2,2,1),"mm"),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        panel.background = element_rect(fill = "white",colour = NA),
        legend.title=element_text(size=rel(0.9)),
        strip.text = element_text(size=rel(1.3)) ,
        panel.grid.major.y = element_line(colour = "grey92"),
        axis.text.x = element_text(angle = 45,vjust = 0.95,hjust = +1),
        text = element_text(family = "Encode Sans"))

ggplot2::ggsave(filename = "figures/bra_cit/avaliacao_tp.png"
                , scale = 0.85
                , width = 21
                , bg = "white"
                , height = 14
                , units = "cm"
                , dpi = 300)
# PLOT 2 -----------
rm(list=ls())
library(data.table)
library(janitor)
library(ggplot2)
library(magrittr)

v1 <- c(76,8.1,6.1,5.4,2.4,1.7,0.3)
v2 <- c('Trabalho','Lazer','Estudo','Outros','Saúde'
        ,'Compras','Levar/Acompanhar\n Estudo')
df <- data.table("name" = v2,"value" = v1)
order_name <- df[order(value,decreasing = FALSE),.SD,.SDcols = "name"]
order_name <- order_name$name
order_name
df[,name_f := factor(x = name
                     ,levels = order_name
                     ,labels = order_name)]
df

pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")


ggplot(data = df) + 
  geom_col(aes(x = name_f,y = value)
           ,position = "dodge2"
           ,fill = "grey85"
           ,color = "black"
           ,width = .5) + 
  labs(y = "Percentual das respostas (%)",fill = NULL,x = NULL
       ,title = NULL
       ,caption = "Fonte: Adaptado do Plano de Mobilidade Urbana (LOGIT, 2013).") +
  geom_text(aes(x = name_f,y = value
                ,label = paste0(round(value,2),"%"),fontface=2)
            ,vjust =-0.25,hjust = 0.5
            ,family = "Encode Sans",  size= 3.15)+
  theme(legend.position = "none",
        plot.margin=unit(c(1,2,2,1),"mm"),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        panel.background = element_rect(fill = "white",colour = NA),
        legend.title=element_text(size=rel(0.9)),
        strip.text = element_text(size=rel(1.3)) ,
        panel.grid.major.y = element_line(colour = "grey92"),
        axis.text.x = element_text(angle = 45,vjust = 0.95,hjust = +1),
        text = element_text(family = "Encode Sans"))

ggplot2::ggsave(filename = "figures/bra_cit/motivo_uso_bici.png"
                , scale = 0.7
                , width = 21
                , bg = "white"
                , height = 14
                , units = "cm"
                , dpi = 300)

# PLOT 3 - frequencia  -----------
rm(list=ls())
library(data.table)
library(janitor)
library(ggplot2)
library(magrittr)

v1 <- c(0,25.64,44.8,299.11)
v1 <- v1/max(v1)
v1 <- v1 * 80
v2 <- c('1','2','3-4','5 ou mais')
df <- data.table("name" = v2,"value" = v1)
order_name <- df[order(value,decreasing = FALSE),.SD,.SDcols = "name"]
order_name <- order_name$name
order_name
df[,name_f := factor(x = name
                     ,levels = order_name
                     ,labels = order_name)]
df

pal <- c("#5766cc","#33b099","#d96e0a","#cc3003")


ggplot(data = df) + 
  geom_col(aes(x = name_f,y = value)
           ,position = "dodge2"
           ,fill = "grey85"
           ,color = "black"
           ,width = .5) + 
  labs(y = "Percentual das respostas (%)",fill = NULL
       ,x = "Dias por semana"
       ,title = NULL
       ,caption = "Fonte: Adaptado do Plano de Mobilidade Urbana (LOGIT, 2013).") +
  geom_text(aes(x = name_f,y = value
                ,label = paste0(round(value,1),"%"),fontface=2)
            ,vjust =-0.25,hjust = 0.5
            ,family = "Encode Sans",  size= 3.15)+
  theme(legend.position = "none",
        plot.margin=unit(c(1,2,2,1),"mm"),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        panel.background = element_rect(fill = "white",colour = NA),
        legend.title=element_text(size=rel(0.9)),
        strip.text = element_text(size=rel(1.3)) ,
        panel.grid.major.y = element_line(colour = "grey92"),
        text = element_text(family = "Encode Sans"))

ggplot2::ggsave(filename = "figures/bra_cit/freq_uso_bici.png"
                , scale = 0.7
                , width = 21
                , bg = "white"
                , height = 14
                , units = "cm"
                , dpi = 300)

# End----------