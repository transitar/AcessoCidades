#Load packages
source('./fun/setup.R')
sigla_muni <- "man"
# 01. People microdata ----------------------------------------------------
#Set
code_state<-13

#Read data
dict_variables<-read_excel("data/dic_variables.xlsx")

data_micro_p<-read.fortran(paste0("data/00_microdados/Amostra_Pessoas_",as.character(code_state),".txt"),
                      format=dict_variables$FORMATO,
                       col.names=dict_variables$VARIAVEIS)
saveRDS(data_micro_p,paste0("data/00_microdados/01_data_micro_p_",as.character(code_state),".RDS"))

# data_micro_p <- read_rds(paste0("data/00_microdados/01_data_micro_p_",as.character(code_state),".RDS"))
#Create a complete database with id

# cod_munis <- c(308,4805,605,6701)
cod_munis <- c(2603)

data_micro_p2<- data_micro_p %>% filter (V0002 %in% cod_munis)
data_micro_p2<- data_micro_p2 %>% mutate(id_g = 1:nrow(data_micro_p2))
saveRDS(data_micro_p2,sprintf("data/00_microdados/01_data_micro_p_%s.RDS", sigla_muni))


data_micro_p2 <- read_rds(sprintf("data/00_microdados/01_data_micro_p_%s.RDS", sigla_muni))

data_micro_p_renda <- data_micro_p2 %>% select(V0002,V6525, V0606, V0601) %>%
  mutate(genero = case_when(V0601 == "1" ~ "Homens",
                         V0601 == "2" ~ "Mulheres"),
         cor = case_when(V0606 == "1" ~ "Brancos",
                         V0606 == "2" ~ "Pretos",
                         V0606 == "3" ~ "am_ind_remover",
                         V0606 == "4" ~ "Pretos",
                         V0606 == "5" ~ "am_ind_remover"),
         muni = sigla_muni)
write_rds(data_micro_p_renda, sprintf('../../data-raw/renda_amostra_censo_2010/muni_%s.rds', sigla_muni))


# V0653 - No trabalho principal, quantas horas trabalhava habitualmente por semana
# V0662 – Qual o tempo habitual gasto de deslocamento de sua casa até o trabalho
# V0629 – Curso que frequenta
# V0628 – Frequenta escola ou creche (pública/particular)
# V6529	RENDIMENTO MENSAL DOMICILIAR EM JULHO DE 2010 					
# V6530	RENDIMENTO DOMICILIAR, SALÁRIOS MÍNIMOS, EM JULHO DE 2010 					
# V6531	RENDIMENTO DOMICILIAR PER CAPITA EM JULHO DE 2010 					
# V6532	RENDIMENTO DOMICILIAR PER CAPITA, EM Nº DE SALÁRIOS MÍNIMOS, EM JULHO DE 2010 					
# V6527	RENDIMENTO MENSAL TOTAL EM JULHO DE 2010 					
# V6528	RENDIMENTO MENSAL TOTAL EM Nº DE SALÁRIOS MÍNIMOS EM JULHO DE 2010 					
# V6529	RENDIMENTO DOMICILIAR (DOMICÍLIO PARTICULAR) EM JULHO DE 2010  					
# V6530	RENDIMENTO DOMICILIAR (DOMICÍLIO PARTICULAR) EM Nº DE SALÁRIOS MÍNIMOS EM JULHO DE 2010  					
# V6531	RENDIMENTO DOMICILIAR (DOMICÍLIO PARTICULAR) PER CAPITA EM JULHO DE 2010 					
# V6532	RENDIMENTO DOMICILIAR (DOMICÍLIO PARTICULAR) PER CAPITA EM Nº DE SALÁRIOS MÍNIMOS EM JULHO DE 2010 					
