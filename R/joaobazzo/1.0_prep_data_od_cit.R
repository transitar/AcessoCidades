# Load ----

rm(list=ls())
gc(reset = TRUE)

# install.packages('easypackages')
easypackages::packages('geobr'
                       , 'gtfs2gps'
                       , 'openxlsx'
                       , 'ggrepel'
                       , 'data.table'
                       , 'magrittr'
                       , 'ggplot2'
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

# Bairros----------------
muni13 <- geobr::read_census_tract(code_tract = 3201209
                                   ,year = 2020,simplified = FALSE)
mapview::mapview(muni13)


muni_br <- readr::read_rds("data-raw/bra_cit/setores_cit_2019.rds")
muni_br$id <- 1:nrow(muni_br)
#mapview(muni_br["id"])
setDT(muni_br)
muni_br[,zt := NA_integer_]
# add local zones--------------
muni_br[id %in% c(44,43,38,280,42,41,40,269,39), zt := 1]
muni_br[id %in% c(34,35), zt := 27]
muni_br[id %in% c(278,279,29,33,268,31,32), zt := 3]
muni_br[id %in% c(36,21,37), zt := 2]
muni_br[id %in% c(22,23,24,25,265,266,267,26,27,30,28), zt := 4]
muni_br[id %in% c(45,270,46), zt := 7]
muni_br[id %in% c(251,50,10,271,49), zt := 6]
muni_br[id %in% c(257,47,51,52,57,20,47), zt := 30]
muni_br[id %in% c(19,17,53,54,55), zt := 5]
muni_br[id %in% c(56,258,259,61,60,58,86,66,252,63,64,65,59), zt := 8]
muni_br[id %in% c(62,281), zt := 29]
muni_br[id %in% c(75,76,78,79,260,80,261,272,81,82,91,83,84,77,282), zt := 10] 
muni_br[id %in% c(93,92,283,253,143,144), zt := 12]
muni_br[id %in% c(95,226,227,96,94,98,99,100,101
                  ,275,103,104,106,108,107,105,102,276,218,97), zt := 13]
muni_br[id %in% c(109,110,127,122,120), zt := 15]
muni_br[id %in% c(121,123,124,125,126,128,277,135,136,8,138,140,142,133,139
                  ,137,232,286,134,129,7,131,9,121,141,225,132,130) , zt := 14]
muni_br[id %in% c(235,114,115,112,111,119,118,117,284,285,5,113,116,6), zt := 16]
muni_br[id %in% c(162,161,254), zt := 18]
muni_br[id %in% c(164,163,170,169,168,165,173,172,171,167,165,166,159), zt := 19]
muni_br[id %in% c(174,177,155,220,222,178,174,175,176,3,2), zt := 20]
muni_br[id %in% c(185,255,183,189,190,4,188,187,186,185,256,184), zt := 21]
muni_br[id %in% c(223,192,224,193,191,1,197
                  ,249,198,249,196,195,194,192), zt := 23]
muni_br[id %in% c(181,180,179,182,221,228,219,229,230), zt := 22]
muni_br[id %in% c(231,200,208,203,202,199,201,203), zt := 28]
muni_br[id %in% c(204,205,262,207,216,217,210,209,206), zt := 25]
muni_br[id %in% c(211,212,263,213,264,214,215), zt := 26]
muni_br[id %in% c(11,12,14,13,18,15,16), zt := 9]
muni_br[id %in% c(150,157,87,273,90
                  ,89,88,74,274,70,72,73,85,71,69,250,156,158), zt := 11]
muni_br[id %in% c(68,67,14), zt := 24]
muni_br[id %in% c(145,146,147,149,152,151,154,153,160,148), zt := 17]

# union -----
muni_br <- sf::st_as_sf(muni_br)

mapview(muni_br[is.na(muni_br$zt),])
mapview(muni_br[!is.na(muni_br$zt),])
mapview(muni_br["zt"])
# extra zones
extra_zones <- sf::st_read("data-raw/bra_cit/extra_zones_joao.gpkg")
names(extra_zones) <- c("zt","geom")
extra_zones[4,]$zt <- 14
# merge

merge <- rbind(muni_br[,"zt"],extra_zones)

# union
merge$zt <- paste0("zt_",merge$zt)
my_zones <- unique(merge$zt)

merge_zt <- lapply(my_zones, function(i){ # i = "zt_17"
  tmp <- merge[merge$zt == i,]
  tmp <- sf::st_union(tmp)
  if(i %in% paste0("zt_",c(15,13,12,17,14,18))) tmp <- sf::st_buffer(tmp,0.000025)
  tmp <- sf::st_sf(data.frame("zt" = i,geom = tmp))
  return(tmp)
  }) %>% data.table::rbindlist() %>%  sf::st_as_sf()

mapview(merge_zt["zt"])

# Tabela 1 e 3 -----
setDT(merge_zt)
merge_zt[zt == "zt_1" ,":="(o_tp = 45  ,d_tp = 29 ,o_ti = 28  , d_ti = 48 )] 
merge_zt[zt == "zt_2" ,":="(o_tp = 1   ,d_tp = 1  ,o_ti = 7   , d_ti = 3  )]
merge_zt[zt == "zt_3" ,":="(o_tp = 7   ,d_tp = 9  ,o_ti = 28  , d_ti = 18 )] 
merge_zt[zt == "zt_4" ,":="(o_tp = 5   ,d_tp = 3  ,o_ti = 12  , d_ti = 18 )] 
merge_zt[zt == "zt_5" ,":="(o_tp = 12  ,d_tp = 23 ,o_ti = 280 , d_ti = 194)]  
merge_zt[zt == "zt_6" ,":="(o_tp = 35  ,d_tp = 11 ,o_ti = 57  , d_ti = 49 )] 
merge_zt[zt == "zt_7" ,":="(o_tp = 8   ,d_tp = 11 ,o_ti = 23  , d_ti = 12 )] 
merge_zt[zt == "zt_8" ,":="(o_tp = 6   ,d_tp = 3  ,o_ti = 79  , d_ti = 70 )] 
merge_zt[zt == "zt_9" ,":="(o_tp = 183 ,d_tp = 231,o_ti = 199 , d_ti = 393)]  
merge_zt[zt == "zt_10",":="(o_tp = 37  ,d_tp = 38 ,o_ti = 21  , d_ti = 16 )] 
merge_zt[zt == "zt_11",":="(o_tp = 23  ,d_tp = 32 ,o_ti = 178 , d_ti = 113)]  
merge_zt[zt == "zt_12",":="(o_tp = 17  ,d_tp = 8  ,o_ti = 40  , d_ti = 65 )] 
merge_zt[zt == "zt_13",":="(o_tp = 74  ,d_tp = 73 ,o_ti = 218 , d_ti = 103)]  
merge_zt[zt == "zt_14",":="(o_tp = 58  ,d_tp = 38 ,o_ti = 149 , d_ti = 149)]  
merge_zt[zt == "zt_15",":="(o_tp = 5   ,d_tp = 2  ,o_ti = 11  , d_ti = 10 )] 
merge_zt[zt == "zt_16",":="(o_tp = 36  ,d_tp = 47 ,o_ti = 98  , d_ti = 68 )] 
merge_zt[zt == "zt_17",":="(o_tp = 43  ,d_tp = 66 ,o_ti = 95  , d_ti = 101)]  
merge_zt[zt == "zt_18",":="(o_tp = 36  ,d_tp = 27 ,o_ti = 19  , d_ti = 13 )] 
merge_zt[zt == "zt_19",":="(o_tp = 43  ,d_tp = 14 ,o_ti = 16  , d_ti = 19 )] 
merge_zt[zt == "zt_20",":="(o_tp = 22  ,d_tp = 9  ,o_ti = 34  , d_ti = 29 )] 
merge_zt[zt == "zt_21",":="(o_tp = 5   ,d_tp = 7  ,o_ti = 54  , d_ti = 61 )] 
merge_zt[zt == "zt_22",":="(o_tp = 47  ,d_tp = 33 ,o_ti = 35  , d_ti = 49 )] 
merge_zt[zt == "zt_23",":="(o_tp = 6   ,d_tp = 8  ,o_ti = 50  , d_ti = 133)]  
merge_zt[zt == "zt_24",":="(o_tp = 9   ,d_tp = 19 ,o_ti = 40  , d_ti = 97 )] 
merge_zt[zt == "zt_25",":="(o_tp = 12  ,d_tp = 33 ,o_ti = 118 , d_ti = 110)]  
merge_zt[zt == "zt_26",":="(o_tp = 44  ,d_tp = 46 ,o_ti = 90  , d_ti = 83 )] 
merge_zt[zt == "zt_27",":="(o_tp = 6   ,d_tp = 0  ,o_ti = 4   , d_ti = 6  )] 
merge_zt[zt == "zt_28",":="(o_tp = 2   ,d_tp = 0  ,o_ti = 12  , d_ti = 8  )] 
merge_zt[zt == "zt_29",":="(o_tp = 1   ,d_tp = 4  ,o_ti = 12  , d_ti = 46 )]  
merge_zt[zt == "zt_30",":="(o_tp = 13  ,d_tp = 16 ,o_ti = 64  , d_ti = 39 )]  
merge_zt[zt == "zt_NA",":="(o_tp = 24  ,d_tp = 24 ,o_ti = 303 , d_ti = 251)]  
merge_zt
readr::write_rds(sf::st_as_sf(merge_zt),"data/bra_cit/zona_trafego.rds")










