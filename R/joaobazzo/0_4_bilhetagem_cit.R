# Load ----
rm(list=ls())
gc(reset = TRUE)

# install.packages('easypackages')
easypackages::packages('geobr', 'gtfs2gps', 'openxlsx', 'ggrepel'
                       , 'data.table', 'magrittr', 'ggplot2', 'patchwork'
                       , 'mapview','ggspatial', 'raster', 'ggnewscale', 'rayshader'
                       , 'progressr', 'pbapply', 'extrafont', 'extrafontdb'
                       , 'aopdata')


# read

prep_data <- function(sheet_name,month_code){
  # read
  dt_file <- "data-raw/bra_cit/dados TRANSPORTE Cachoeiro -.xlsx"
  dt <- openxlsx::read.xlsx(dt_file,sheet = sheet_name,startRow = 3)
  names(dt) <- janitor::make_clean_names(names(dt))
  data.table::setDT(dt)
  dt[,aplicacao_f := aplicacao]
  # change groups
  dt[aplicacao %in% c("ESCOLAR","PROFESSOR"
                        ,"ESCOLAR PREFEITURA","ESCOLAR MARATAIZES")
     ,aplicacao_f := "ESCOLAR / PROFESSOR"]
  dt[aplicacao %in% c("CIDADÃO","CIDADÃO MARATAIZES")
     ,aplicacao_f := "CIDADÃO"]
  dt[aplicacao %in% c("SENIOR","SENIOR MTZ")
     ,aplicacao_f := "SENIOR"]
  dt[aplicacao %in% c("VALE TRANSPORTE","VT MARATAIZES")
     ,aplicacao_f := "VALE TRANSPORTE"]
  dt[!(aplicacao %in% c("ESCOLAR","PROFESSOR","ESCOLAR PREFEITURA"
                          ,"ESCOLAR MARATAIZES"
                          ,"CIDADÃO","CIDADÃO MARATAIZES"
                          ,"VALE TRANSPORTE","VT MARATAIZES",
                        "VENDA A BORDO","SENIOR"
                        ,"SENIOR MTZ"))
     ,aplicacao_f := "OUTROS"]
  
  
  dt[,unique(aplicacao_f)]
  dt[aplicacao_f == "OUTROS",unique(aplicacao)]
  # fix data
  dt[,date := sprintf("%s-%s-%s",data_ano,month_code,data_dia) %>% as.Date()]
  dt[,date_week := format(date,"%A")]
  
  dt <- dt[,{
    total <- sum(quantidade_total,na.rm = TRUE)
    dias <- uniqueN(date)
    list("media" = total/dias,"total" = total,'dias' = dias)
    },by = .(aplicacao_f,date_week)] 
  
  dt[,date_week_f := factor(date_week,
                                  levels = c("segunda","terça","quarta","quinta","sexta"
                                             ,"sábado","domingo"))]
  dt[,mes := month_code]
  return(dt)
}

dt_file <- "data-raw/bra_cit/dados TRANSPORTE Cachoeiro -.xlsx"
openxlsx::getSheetNames(dt_file)
july <- prep_data(sheet_name = "julho-2022",month_code = "07")
ago <- prep_data(sheet_name = "agosto-2022",month_code = "08")
set <- prep_data(sheet_name = "eet-2022",month_code = "09")
tmp_plot <- data.table::rbindlist(list(july,ago,set))

# text ------------
tmp_plot1 <- copy(tmp_plot)
tmp_plot1[,util := fifelse(date_week_f %in% c('sábado','domingo')
                          ,"weekend","weekday")]
tmp_plot1 <- tmp_plot1[,{
  total <- sum(total)
  dias <- sum(dias)
  media <- total / dias
  list(total,dias,media)
  },by = .(util,aplicacao_f)]
tmp_plot1[,prop := round(100 * media/sum(media),2),by =.(util)]
tmp_plot1[,sum(media),by = .(util)]

tmp_plot2 <- copy(tmp_plot)
tmp_plot2[,sum(total),by = .(date_week_f,mes)]


# plot---------

tmp_plot[,date_week_f := factor(date_week,
                          levels = c("segunda","terça","quarta","quinta","sexta"
                                     ,"sábado","domingo")
                          ,labels = c("2ª","3ª","4ª","5ª","6ª"
                                      ,"Sáb.","Dom."))]
tmp_plot[,aplicacao_f := factor(aplicacao_f,
                                levels = c("VENDA A BORDO","VALE TRANSPORTE"
                                           ,"ESCOLAR / PROFESSOR"
                                           ,"CIDADÃO","SENIOR","OUTROS")
                                ,labels = c("Venda à bordo","Vale-Transporte",
                                            "Escolar / Professor"
                                            ,"Cidadão","Sênior","Outros"))]
tmp_plot[,mes_f := factor(mes,
                          levels = c('07','08','09')
                          ,labels = c("Julho","Agosto","Setembro"))]

pal <- c("#5766cc","#33b099","#d96e0a","yellow4","#cc3003","grey60")

p1 <- ggplot(tmp_plot)+
  geom_bar(aes(x = date_week_f,y = media,fill = aplicacao_f)
           ,stat = "identity",position = "stack")+
  scale_fill_manual(values = pal) + 
  facet_wrap(~mes_f)+
  labs(fill = "Aplicação"
       ,x = NULL,y = "Passagens (média diária)"
       ,caption = NULL)+
  theme(legend.position = "none",
        plot.margin=unit(c(2,0,0,1),"mm"),
        plot.caption = element_text(size=rel(0.75)),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        strip.text = element_text(size=rel(0.85)) ,
        #strip.background =  element_blank(),
        #strip.text.x = element_blank(),
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
p1
p2 <- ggplot(tmp_plot)+
  geom_col(aes(x = date_week_f,y = media,fill = aplicacao_f)
           ,position = "fill")+
  scale_fill_manual(values = pal) + 
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~mes_f)+
  labs(fill = "Aplicação"
       ,x = NULL,y = "Proporção (%)"
       ,caption = NULL)+
  theme(legend.position = "bottom",
        plot.margin=unit(c(2,0,0,1),"mm"),
        plot.caption = element_text(size=rel(0.75)),
        legend.key.width=unit(1,"line"),
        legend.key.height = unit(0.4,"cm"),
        legend.text=element_text(size=rel(0.8)),
        strip.text = element_text(size=rel(0.85)) ,
        #strip.background =  element_blank(),
        #strip.text.x = element_blank(),
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

p1 / p2


ggplot2::ggsave(filename = "figures/bra_cit/bilhetagem.png",
                scale = 1.2,width = 16.5,height = 15,
                bg = "white",
                units = "cm",dpi = 300)

vec <- c('16/06/2022','29/06/2022','14/08/2022','07/09/2022')
vec %>% as.Date("%d/%m/%Y") %>% format("%A")
tmp_plot[,util := fifelse(date_week %in% c("sabado","domingo"),
                          "fds","util")]
tmp_plot[,sum(media),by = .(mes_f,date_week)] %>% 
  .[date_week == "domingo"]
  .[date_week == "sábado"]
