
# create munis_df
library(magrittr)
library(data.table)
library(geobr)
library(sidrar)
library(readr)
library(purrr)
rm(list=ls())
gc(reset=T)

# 1) read_data ------
all_munis <- geobr::read_municipality(simplified = T)

munis_fnp <- function(code_muni2) {
  
  muni_geom <- geobr::read_municipality(code_muni = code_muni2) %>% 
    data.table::setDT()
}

all_munis <- map_df(.x = code_muni, .f = munis_fnp)
## 2.3) Pop 2010 -----

# Tabela 6579 - População residente estimada
# Tabela 202 - População residente, por sexo e situação do domicílio
info2020 <- sidrar::info_sidra(x = 202)
### download CENSUS
popcenso_br <- sidrar::get_sidra(x = 202
                                 , variable = 93
                                 , period = as.character(2010)
                                 , geo = "City"
                                 , classific = c("c1"))

# fix names
data.table::setDT(popcenso_br)
names(popcenso_br) <- janitor::make_clean_names(names(popcenso_br))
popcenso_br <- popcenso_br[situacao_do_domicilio == "Total"]
popcenso_br <- popcenso_br[,.SD,.SDcols = c("municipio_codigo","ano","valor")]

## 2.4) Pop projection---------

sidrar::info_sidra(x = 6579)
#pop_proj_br <- readRDS("../ubanformbr/data/table_6579_ibge.rds")
pop_proj_br <- lapply(2011:2023,function(i){
  message(i)
  sidrar::get_sidra(x = 6579
                    , variable = 9324
                    , period = as.character(i)
                    , geo = "City")
}) %>% data.table::rbindlist()

setDT(pop_proj_br)
names(pop_proj_br) <- janitor::make_clean_names(names(pop_proj_br))
pop_proj_br <- pop_proj_br[,.SD,.SDcols = c("municipio_codigo","ano","valor")]

rbind_pop <- rbind(popcenso_br,pop_proj_br)
readr::write_rds(rbind_pop,"../data/pop_2010-2021.rds",compress="gz")


pop <- read_rds("../data/pop_2010-2021.rds")
xlsx::write.xlsx(pop, "../data/pop_2010-2021.xlsx")
