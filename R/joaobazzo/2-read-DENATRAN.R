#
# initial config----------------
source('./R/fun/setup.R')

rm(list=ls())
gc(reset = T)
# install.packages('easypackages')
easypackages::packages('geobr'
                       , 'gtfs2gps'
                       , 'openxlsx'
                       , 'ggrepel'
                       , 'data.table'
                       , 'magrittr'
                       , 'ggplot2'
                       , 'XLConnect'
                       , 'patchwork'
                       , 'mapview','ggspatial'
                       , 'raster'
                       , 'ggnewscale'
                       , 'rayshader'
                       , 'progressr'
                       , 'pbapply'
                       , 'extrafont'
                       , 'extrafontdb'
                       , 'aopdata')
`%nin%` = Negate(`%in%`)
ls_initial_list <- ls()

# basic list of states abbreviation

state_geobr <- geobr::read_state() %>% 
  data.table::setDT() 
state <- state_geobr %>% 
  dplyr::select(abbrev_state) %>% 
  unlist() %>% as.vector()

state <- munis_list$munis_df$abrev_estado


# function read individually since 2001 -------------
# 
# fix name columns
toupper_noaccent <- function(i){
  stringi::stri_trans_general(i,id = "Latin-ASCII") %>% 
    toupper() %>% stringr::str_replace_all("-"," ")  %>% 
    stringr::str_replace_all("'","")
}

data_fix <- function(dt,uf,muni,year,state){
  
  # working files
  
  # wb <- XLConnect::loadWorkbook("data-raw/DENATRAN/2001/Frota Tipo-Munic 2001.xls")
  # dt <- XLConnect::readWorksheet(wb,sheet = 1,startRow = 3)
  # year = 2001
  # state = state
  # uf = "Unidades.da.Federação"
  # muni <- "Municípios"
  # 
  
  # to DT and rename
  dt <- data.table::setDT(dt)
  data.table::setnames(dt,c(uf,muni),c("UF","MUNICIPIO"))
  
  colnames(dt) <- toupper_noaccent(colnames(dt))
  dt[, UF := toupper_noaccent(UF)]
  dt[, MUNICIPIO := toupper_noaccent(MUNICIPIO)]
  
  # year and state filter 
  dt[,ANO := year]
  dt <- dt[UF %in% state,]
  
  # remove comma
  remove_comma <- function(i){gsub(",","",i)}
  dt[,colnames(dt) := lapply(.SD,remove_comma), .SDcols = colnames(dt)]
  
  # as.numeric
  numeric_cols <- colnames(dt)[colnames(dt) %nin% c("UF","MUNICIPIO")]
  dt[,(numeric_cols) := lapply(.SD,as.numeric), .SDcols = numeric_cols]
  
  # only certain columns
  veh_columns <- c("UF","MUNICIPIO",
                   "AUTOMOVEL","CAMINHONETE","CAMIONETA",
                   "MOTOCICLETA","MOTONETA","ANO")
  
  dt <- dt[,.SD,.SDcols = veh_columns]
  return(dt)
}

#
#  list files-------------------
#

dtfiles <- data.table(path = c(
  "data-raw/DENATRAN/frota_2010/Frota_Municipios/Frota Munic Jan2010.xls",
  "data-raw/DENATRAN/frota_2011/Frota Munic. 2011/Frota Munic. JAN 2011.xls",
  "data-raw/DENATRAN/frota_2012/Frota Munic. 2012/Frota Munic. JAN.2012.xls",
  "data-raw/DENATRAN/frotamunic-jan-2013 (1)/Frota Munic.JAN.2013.xls",
  "data-raw/DENATRAN/frotamunicjan2014.xlsx",
  "data-raw/DENATRAN/Frota_por_Municipio_e_Tipo-JAN_15.xls",
  "data-raw/DENATRAN/Frota_por_Municipio_e_Tipo-JAN_16.xlsx",
  "data-raw/DENATRAN/frota_munic_janeiro_2017.xls",
  "data-raw/DENATRAN/frota_munic_janeiro_2018.xls",
  "data-raw/DENATRAN/frota_munic_modelo_janeiro_2019.xls",
  "data-raw/DENATRAN/frota_munic_modelo_janeiro_2020.xls",
  "data-raw/DENATRAN/frota_municipio_modelo_janeiro_2021.xlsx"),
  sheet =    c(2   ,2   ,2   ,2   ,2   ,2   ,2   ,1   ,1   ,1   ,1   ,1   ),
  startrow = c(2   ,2   ,3   ,3   ,3   ,2   ,3   ,3   ,3   ,3   ,3   ,2   ),
  year =     c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
)


dtfiles <- data.table(path = c(
  "../data-raw/DENATRAN/frota_munic_modelo_janeiro_2022.xlsx"),
  sheet =    c(1),
  startrow = c(2),
  year =     c(2022)
)

#
# check if this reading properly--------
# PS: for january, it's okay!
#

#for(i in 1:nrow(dtfiles)){ 
#  if(dtfiles$path[i] %like% ".xlsx"){
#    wb <- readxl::read_xlsx(path = dtfiles$path[i],sheet = dtfiles$sheet[i]
#                            ,skip = dtfiles$startrow[i])
#  }else{
#    wb <- readxl::read_xls(path = dtfiles$path[i],sheet = dtfiles$sheet[i]
#                           ,skip = dtfiles$startrow[i])
#  }
#  setDT(wb)
#  message(dtfiles$year[i])
#  print(head(wb,1))
#}

#
# read and fix data ----------------
i = 1
#
obs <- lapply(1:nrow(dtfiles),function(i){ # i = 1
  
  message(dtfiles$year[i])
  
  if(dtfiles$path[i] %like% ".xlsx"){
    temp <- readxl::read_xlsx(path = dtfiles$path[i],sheet = dtfiles$sheet[i]
                              ,skip = dtfiles$startrow[i])
  }else{
    temp <- readxl::read_xls(path = dtfiles$path[i],sheet = dtfiles$sheet[i]
                             ,skip = dtfiles$startrow[i])
  }
  setDT(temp)
  veh <- data_fix(dt = temp,
                  uf = names(temp)[1],
                  muni = names(temp)[2],
                  year = dtfiles$year[i], state = state)
  return(veh)
}) %>% data.table::rbindlist()

#
# sum categories -----
#

obs[,TOTAL_AUTOS := AUTOMOVEL + CAMINHONETE + CAMIONETA]
obs[,TOTAL_MOTOS := MOTOCICLETA + MOTONETA]

#
# add municipality area --------------
#

muni_geom <- geobr::read_municipality(code_muni = "all") %>% 
  data.table::setDT()

muni_geom<- munis_names
muni_geom[,name_muni := toupper_noaccent(muni)]
muni_geom[,muni_uf := paste0(name_muni,"-",uf)]

muni_geom[,name_muni := toupper_noaccent(name_muni)]
muni_geom[,muni_uf := paste0(name_muni,"-",abbrev_state)]
# 
# change it to dictionary----------
#
obs[MUNICIPIO %like% "CACH"]
obs[,MUNI_UF := paste0(MUNICIPIO,"-",UF)]
obs[MUNI_UF %in% "AMPARO DA SERRA-MG",MUNI_UF := "AMPARO DO SERRA-MG"]
obs[MUNI_UF %in% "ARMACAO DE BUZIOS-RJ",MUNI_UF := "ARMACAO DOS BUZIOS-RJ"]
obs[MUNI_UF %in% "ASSU-RN" ,MUNI_UF := "ACU-RN"]
obs[MUNI_UF %in% "BELA VISTA DO CAROBA-PR",MUNI_UF := "BELA VISTA DA CAROBA-PR"]
obs[MUNI_UF %in% "BELEM DE SAO FRANCISCO-PE",MUNI_UF := "BELEM DO SAO FRANCISCO-PE"]
obs[MUNI_UF %in% "FLORINEA-SP",MUNI_UF := "FLORINIA-SP"]
obs[MUNI_UF %in% "COUTO DE MAGALHAES-TO",MUNI_UF := "COUTO MAGALHAES-TO"]
obs[MUNI_UF %in% "DONA EUZEBIA-MG",MUNI_UF := "DONA EUSEBIA-MG"]
obs[MUNI_UF %in% "GOUVEA-MG",MUNI_UF := "GOUVEIA-MG"]
obs[MUNI_UF %in% "GRACCHO CARDOSO-SE",MUNI_UF := "GRACHO CARDOSO-SE"]
obs[MUNI_UF %in% "LAGEADO GRANDE-SC",MUNI_UF := "LAJEADO GRANDE-SC"]
obs[MUNI_UF %in% "LAGEDO DO TABOCAL-BA",MUNI_UF := "LAJEDO DO TABOCAL-BA"]
obs[MUNI_UF %in% "LAGOA DO ITAENGA-PE",MUNI_UF := "LAGOA DE ITAENGA-PE"]
obs[MUNI_UF %in% "MOGI MIRIM-SP",MUNI_UF := "MOJI MIRIM-SP"]
obs[MUNI_UF %in% "MUQUEM DO SAO FRANCISCO-BA",MUNI_UF := "MUQUEM DE SAO FRANCISCO-BA"]
obs[MUNI_UF %in% "NOVA DO MAMORE-RO",MUNI_UF := "NOVA MAMORE-RO"]  
obs[MUNI_UF %in% "VILA NOVA DO MAMORE-RO",MUNI_UF := "NOVA MAMORE-RO"]  
obs[MUNI_UF %in% "PARATI-RJ",MUNI_UF := "PARATY-RJ"]
obs[MUNI_UF %in% "PINHAL DO SAO BENTO-PR",MUNI_UF := "PINHAL DE SAO BENTO-PR"]
obs[MUNI_UF %in% "PRESIDENTE CASTELO BRANCO-SC",MUNI_UF := "PRESIDENTE CASTELLO BRANCO-SC"]
obs[MUNI_UF %in% "QUELUZITA-MG",MUNI_UF := "QUELUZITO-MG"]
obs[MUNI_UF %in% "SANTA CRUZ DO MONTE CASTELO-PR",MUNI_UF := "SANTA CRUZ DE MONTE CASTELO-PR" ]
obs[MUNI_UF %in% "SAO DOMINGOS DE POMBAL-PB",MUNI_UF := "SAO DOMINGOS-PB"]
obs[MUNI_UF %in% "SAO FRANCISCO DE ASSIS DO PIAU-PI",MUNI_UF := "SAO FRANCISCO DE ASSIS DO PIAUI-PI"]
obs[MUNI_UF %in% "SAO LOURENCO DOESTE-SC",MUNI_UF := "SAO LOURENCO DO OESTE-SC"]
obs[MUNI_UF %in% "SAO MIGUEL DOESTE-SC",MUNI_UF := "SAO MIGUEL DO OESTE-SC"]
obs[MUNI_UF %in% "SAO TOME DAS LETRAS-MG",MUNI_UF := "SAO THOME DAS LETRAS-MG"]
obs[MUNI_UF %in% "SAO VALERIO DA NATIVIDADE-TO",MUNI_UF := "SAO VALERIO-TO"]
obs[MUNI_UF %in% "TRAJANO DE MORAIS-RJ",MUNI_UF := "TRAJANO DE MORAES-RJ"]
obs[MUNI_UF %in% "VILA BELA DA SANTISSIMA TRINDA-MT",MUNI_UF := "VILA BELA DA SANTISSIMA TRINDADE-MT"]       
obs[MUNI_UF %in% "PRESIDENTE JUSCELINO-RN",MUNI_UF := "SERRA CAIADA-RN"] 
obs[MUNI_UF %in% "GOIO ERE-PR" ,MUNI_UF := "GOIOERE-PR"]
obs[MUNI_UF %in% "AMAPARI-AP", MUNI_UF := "PEDRA BRANCA DO AMAPARI-AP"]
obs[MUNI_UF %in% "BARAO D0 MONTE ALTO-MG", MUNI_UF :="BARAO DO MONTE ALTO-MG"]
obs[MUNI_UF %in% "BARRA DO SUL-SC", MUNI_UF := "BALNEARIO BARRA DO SUL-SC"]
obs[MUNI_UF %in% "BARAO D0 MONTE ALTO-MG", MUNI_UF := "BARAO DO MONTE ALTO-MG"]
obs[MUNI_UF %in% "JANUARIO CICCO-RN", MUNI_UF := "BOA SAUDE-RN"]
obs[MUNI_UF %in% "BOM JESUS-GO", MUNI_UF := "BOM JESUS DE GOIAS-GO"]
obs[MUNI_UF %in% "BRODOSQUI-SP", MUNI_UF := "BRODOWSKI-SP"]
obs[MUNI_UF %in% "CAMPOS DOS GOITACAZES-RJ", MUNI_UF := "CAMPOS DOS GOYTACAZES-RJ"]
obs[MUNI_UF %in% "IPAUCU-SP", MUNI_UF := "IPAUSSU-SP"]
obs[MUNI_UF %in% "SALMORAO-SP", MUNI_UF := "SALMOURAO-SP"]
obs[MUNI_UF %in% "SANTA ROSA-AC", MUNI_UF := "SANTA ROSA DO PURUS-AC"]
obs[MUNI_UF %in% "SAO LUIS DO PARAITINGA-SP", MUNI_UF := "SAO LUIZ DO PARAITINGA-SP"]
obs[MUNI_UF %in% "MUNHOZ DE MELO-PR", MUNI_UF := "MUNHOZ DE MELLO-PR"]
obs[MUNI_UF %in% "ITAMARACA-PE", MUNI_UF := "ILHA DE ITAMARACA-PE"]
obs[MUNI_UF %in% "SUD MENUCCI-SP", MUNI_UF := "SUD MENNUCCI-SP"]
obs[MUNI_UF %in% "SUZANOPOLIS-SP", MUNI_UF := "SUZANAPOLIS-SP"]
obs[MUNI_UF %in% "POTIRIGUA-BA", MUNI_UF := "POTIRAGUA-BA"]
obs[MUNI_UF %in% "SANTA RITA DO IBITIPOCA-MG", MUNI_UF := "SANTA RITA DE IBITIPOCA-MG"]
obs[MUNI_UF %in% "LUIZ EDUARDO MAGALHAES-BA", MUNI_UF := "LUIS EDUARDO MAGALHAES-BA"]
obs[MUNI_UF %in% "BARAO DO MONTE ALTO-MG", MUNI_UF := "BARAO DE MONTE ALTO-MG"]
obs[MUNI_UF %in% "MALHADA DAS PEDRAS-BA", MUNI_UF := "MALHADA DE PEDRAS-BA"]
obs[MUNI_UF %in% "ITAPAGE-CE", MUNI_UF := "ITAPAJE-CE"]
obs[MUNI_UF %in% "ITABIRINHA DE MANTENA-MG", MUNI_UF := "ITABIRINHA-MG"]
obs[MUNI_UF %in% "VILA ALTA-PR", MUNI_UF := "ALTO PARAISO-PR"]
obs[MUNI_UF %in% "SAO VICENTE DO SERIDO-PB", MUNI_UF := "SERIDO-PB"]
obs[MUNI_UF %in% "SANTA CARMEN-MT", MUNI_UF := "SANTA CARMEM-MT"]
obs[MUNI_UF %in% "PIUI-MG", MUNI_UF := "PIUMHI-MG"]
obs[MUNI_UF %in% "PICARRAS-SC", MUNI_UF := "BALNEARIO PICARRAS-SC"]
obs[MUNI_UF %in% "BALNEARIO DE PICARRAS-SC",MUNI_UF := "BALNEARIO PICARRAS-SC"]
obs[MUNI_UF %in% "MARACANI-BA", MUNI_UF := "MACARANI-BA"]
# = 

obs[muni_geom,on = c('MUNI_UF' = 'muni_uf'),geometry := i.geom]
obs[muni_geom,on = c('MUNI_UF' = 'muni_uf'),CODE := i.code_muni]

obs <- obs[,CODE := as.character(CODE)][!is.na(ANO),]

#
# add population info from IBGE  ---------------------
#
obs <- obs %>% mutate(muni_uf = paste0(MUNICIPIO, "-", UF))

ibge <- readr::read_rds("data/pop_2010-2021.rds")
ibge[,ano := as.character(ano)]
obs[,ANO := as.character(ANO)]

obs[muni_geom,on = c("muni_uf" = "muni_uf"),pop := i.pop_2022]

obs2 <- obs %>% drop_na(pop)

dourados <- obs %>% filter(MUNICIPIO == "DOURADOS")
dourados$pop <- 243368

obs2 <- rbind(obs2, dourados)
# motorization rate
#
head(obs,2)
obs2[, AUTOS_PER_POP := TOTAL_AUTOS / pop]
obs2[, MOTOS_PER_POP := TOTAL_MOTOS / pop]
obs2[, MOTO_RATE := (TOTAL_AUTOS + TOTAL_MOTOS) / pop]

#
# write files -------------------
#

dir.create("../data/DENATRAN")
readr::write_rds(obs2,"../data/DENATRAN/DENATRAN_jan.rds")
write.xlsx(obs2, "../data/DENATRAN/DENATRAN_jan_2022.xlsx")

