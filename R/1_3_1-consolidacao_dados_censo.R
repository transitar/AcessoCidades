# consolida todas as tabelas do censo em uma única 

source('R/fun/setup.R')

# 1 - definindo a tabela com informacoes padroes  -------------------------

# lendo arquivos Basico.csv 

lista.diretorios <- list.dirs(path = '../data-raw/censo_2010/AM_20171016/',full.names = TRUE)

lista.diretorios[9]

arquivos.basicos.path <- list.files(path = lista.diretorios,pattern="(Basico_).*\\.xls$")

# read_excel(path = '../data-raw/censo_2010/AM_20171016/AM/Base informaçoes setores2010 universo AM/EXCEL/Basico_AM.xls')

lista_dos_basicos <- lapply(lista.diretorios,FUN = list.files,pattern="(Basico_).*\\.xls$",full.names = TRUE) %>% unlist()

caminho <- lista_dos_basicos[9]

consolida_basico <- function(caminho) {
  
  ler_basico <- read_excel(caminho)
  
  # df <- ler_basico %>% 
  #   select(1:33) %>% 
  #   mutate_at(c(1:21), as.character) %>% 
  #   mutate_at(c(22:33), ~ str_replace(., ",", ".")) %>% 
  #   mutate_at(c(22:33), as.numeric)
  
  df <- ler_basico %>% 
    select(1:21) %>% 
    mutate_at(c(1:21), as.character) %>% 
    select(Cod_setor,Cod_UF,Cod_municipio)
  # mutate_at(c(22:33), ~ str_replace(., ",", ".")) %>% 
  # mutate_at(c(22:33), as.numeric)
  
  
  return(df)
  
  
}


# tabela_rs <- read.csv(lista_dos_basicos[9])
# 
# teste <- readLines(lista_dos_basicos[9])
# 
# teste2 <- 
# 
# 
# dat_3 <- read.csv(textConnection(paste0(teste, collapse="\n")),
#                   header=TRUE, stringsAsFactors=FALSE)


tabela_basico <- map_df(.x = lista_dos_basicos,.f = consolida_basico)





