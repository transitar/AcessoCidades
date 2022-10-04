# consolida todas as tabelas do censo em uma única 

source('R/fun/setup.R')

# 1 - definindo a tabela com informacoes padroes  -------------------------

# lendo arquivos Basico.csv 

lista.diretorios <- list.dirs(path = '../data-raw/censo_2010_A/',full.names = TRUE)

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

head(tabela_basico)

# 2 - juntando tabela das variáveis  --------------------------------------

caminhos_s_basico <-  data.frame(caminhos = list.files(path = lista.diretorios,pattern = '.xls|.XLS',full.names = T)) %>% 
  filter(str_detect(caminhos,pattern = 'Basico') == FALSE) %>% 
  unlist()

caminho <- caminhos_s_basico[1]

rm(caminho)

consolida_variaveis_censo <- function(caminho) {
  
  message(paste("rodando", which(caminhos_s_basico %in% caminho)))
  
  tabela <- read_excel(caminho) %>% select(-2)
  
  estado <- str_sub(string = caminho,start = -6,end = -5)
  
  prefix <- tabela_variaveis_censo %>% mutate(caminho = caminho) %>% 
    filter(str_detect(string = caminho,pattern = original_name) == TRUE) %>% 
    pull(prefix_name)
  
  tabela2 <- tabela %>% 
    rename_at(2:ncol(tabela),~ paste0(prefix,"_", .x))
    
  write_rds(x = tabela2,file = sprintf('../data-raw/censo_2010_treated_A/%s_%s.rds',prefix,estado))
  
  
}

walk(.x = caminhos_s_basico,.f = consolida_variaveis_censo)

length(caminhos_s_basico)

# OKKKK


# 3 - dados para cada municipio de interesse  -----------------------------

sigla_muni <- "con"

salvar_dados_censo_municipio <- function(munis = "all"){
  
  filter_censo_total_municipio <- function(sigla_muni){
    
    message(paste("rodando", sigla_muni))
    
    code_munis <- munis_list$munis_metro[abrev_muni == sigla_muni]$code_muni %>% 
      unlist()
    
    code_uf <- munis_list$munis_df[abrev_muni == sigla_muni]$abrev_estado %>% unlist()
    
    path_arquivos_UF <- list.files(path = '../data-raw/censo_2010_treated_A/',pattern = paste0(code_uf,".rds"),full.names = T)
    
    lista.dfs <- lapply(path_arquivos_UF, read_rds)
    
    lista_setores_censitarios <- tabela_basico %>% filter(Cod_municipio %in% code_munis) %>% pull(Cod_setor)
    
    lista.dfs2 <- map(lista.dfs, ~filter(.x, Cod_setor %in% lista_setores_censitarios))
    
    df_final <- reduce(lista.dfs2, left_join, by = 'Cod_setor')
    
    write_rds(x = df_final,file = sprintf('../../data-raw/censo_2021_info_muni/muni_%s.rds',sigla_muni))
    
  }
  
  # 2. Aplica funcao ------------------------------------------
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=filter_censo_total_municipio)
  
  
}

salvar_dados_censo_municipio(munis = "all")



# 4 - Seleção das variáveis de interesse --------------------------------------

lista_diretorios_dados_muni <- list.dirs(path = '../data-raw/censo_2021_info_muni/',full.names = TRUE)

path_munis <- list.files(path = lista_diretorios_dados_muni, pattern= "(muni).*\\.rds$", full.names = TRUE)


# sigla_muni <- "bel"

dados_vars_interesse <- censo$variaveis_interess %>%
  left_join(tabela_variaveis_censo, by = c("planilha_censo"="original_name"))

cols_gen_cor <- dados_vars_interesse %>%
  rowwise() %>% 
  mutate(cols_vec = list(paste0(prefix_name,"_V", seq_variaveis)))

#Lista com os nomes das variável a serem construídas
var_list <- cols_gen_cor$variavel


consolida_variaveis_interesse <- function(munis = "all") {
  
  
  vamo <- function(sigla_muni) {
    
    message(paste("rodando", sigla_muni))
    
    arq_munis <- read_rds(file = sprintf('../data-raw/censo_2021_info_muni/muni_%s.rds',sigla_muni))
    
    
    tudo <- map(.x = var_list, .f = coleta_variaveis, arq_munis = arq_munis)
    
    
    variaveis_munis <- reduce(tudo, left_join, by = 'Cod_setor')
    
    
    write_rds(variaveis_munis,sprintf('../data-raw/censo_2021_info_muni_treated/muni_%s.rds',sigla_muni))
    
  }
  
  
  if (munis == "all") {
    
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro$abrev_muni
    
  } else (x = munis)
  
  
  lapply(X=x, FUN=vamo)
  
  
}


consolida_variaveis_interesse(munis = "all")


