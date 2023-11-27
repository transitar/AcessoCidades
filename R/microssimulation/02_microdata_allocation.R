#Load packages
# rm(list = ls())
source('./fun/fun.R')


# 01. Read the data -------------------------------------------------------
#Set
city<-"tau"
res <- 9

char_state<-"CE"
sigla_muni <- "tau"
city_name <- "tau"
code_state<-23
code_muni_<-c(2313302)
# code_muni_<-c(2800308,2804805,2800605,2806701)

#Hex grid with data
grid_with_data<-grid_with_data(state = char_state,city_name = city)
# grid_with_data <- hex_grid_with_data
# grid_with_data <- read_rds('../../data/dados_hex/muni_con/dados_hex_con.rds')
# write_rds(grid_with_data, )
#Microsimulation data
list_files<-list.files("data/03_microsimulated",
                       full.names = TRUE,
                       include.dirs = TRUE,
                       pattern = ".RDS")

files <- data_frame(path = list_files[grepl("01_microsim_puma_",list_files)]) %>%
  mutate(cod_state = str_match(path, "puma_(.*?).RDS")[,2]) %>%
  mutate(cod_state2 = substr(cod_state, 1L, 2L)) %>%
  filter(cod_state2 == code_state)
files <- files$path

microsim_data<- purrr::map_df(files,
                              readRDS)

#Census tract data
census_tract<- read_census_tract(code_tract =code_state,year = 2010) %>% 
                filter(code_muni %in% code_muni_) %>%
                st_transform(31981)

# 02. Preparing the data --------------------------------------------------

set.seed(300)
sample_sequence<-sample(nrow(microsim_data))
microsim_data<-microsim_data[sample_sequence,] 
row.names(microsim_data)<-1:nrow(microsim_data)

microsim_data_men<-microsim_data %>% filter (!grepl("w",age_sex))
microsim_data_wom<-microsim_data %>% filter (grepl("w",age_sex))

# 4. Allocate data to Hex -------------------------------------------------
# grid <- read_rds(paste0("data/02_shp/",tolower(substr(city,1,3)),"_res_9/grid_raw.rds")) %>%
#   rename(h3_address = id_hex)
#Hex by census tract

# grid_raw <- read_rds(paste0("data/02_shp/",tolower(substr(city,1,3)),"_res_9/grid_raw.rds")) %>%
#   select(id_hex = h3_address)
# 
# write_rds(grid_raw,paste0("data/02_shp/",tolower(substr(city,1,3)),"_res_9/grid_raw.rds")) 

hex_tract<-hex_per_tract(state = char_state,
                         hex_grid_path = paste0("data/02_shp/",tolower(substr(city,1,3)),"_res_9/grid_raw.rds"),
                         microsim_data_path = paste0("data/03_microsimulated/complete/microsim_data_", sigla_muni, ".RDS"))

#Allocate individuals to Hex
lista_setores<-unique(hex_tract$code_tract)
purrr::walk(lista_setores,allocate_ind)

#Create micro database
list.files<-list.files(path = "data/03_microsimulated/complete/allocated/",pattern = ".RDS",full.names=TRUE)
list.files <- data_frame(path = list.files) %>%
  mutate(cod_state = str_match(path, "men_(.*?).RDS")[,2]) %>%
  mutate(cod_state2 = substr(cod_state, 1L, 2L)) %>%
  filter(cod_state2 == code_state)
list.files <- list.files$path


data.micro.final<-purrr::map_df(list.files,readRDS)

data.micro.final<- data.micro.final %>% 
                  select(id_g,code_tract,hex,age_sex,renda_class,renda_class_pc,
                         resp_home,educ,deficiencia,cor,Rend_pes,Rend_pc)

data.micro.final<- data.micro.final %>% 
  mutate (educ= cut(as.numeric(as.character(educ)), breaks = c(0,2,3,4,6),
                     labels=c("Baixo","Medio","Alto","ND")))
              # Educação
              
              #Baixo - Inferior ao ensino médio (nível 1)
              #Médio - Ensino médio completo (nível 2)
              #Alto - Superior completo (nível 3)

census_micro_complete<-readRDS(sprintf("data/00_microdados/01_data_micro_p_%s.RDS", sigla_muni)) %>% mutate(id_g = as.factor(as.character(id_g)))

data.micro.final<- data.micro.final %>% left_join(census_micro_complete ,by=c("id_g"))

write_rds(data.micro.final,sprintf("data/03_microsimulated/complete/data.micro.final_%s.RDS", sigla_muni))

