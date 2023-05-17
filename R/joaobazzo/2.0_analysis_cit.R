muni13 <- geobr::read_census_tract(code_tract = 3201209
                                   ,year = 2020,simplified = FALSE)
mapview::mapview(muni13)

# Libraries ----
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

muni2 <- readr::read_rds("data/bra_cit/zona_trafego.rds")
muni2
mapview::mapview(muni2[muni2$zt != "zt_NA","o_tp"])
muni2[order(muni2$o_tp,decreasing = TRUE),"o_tp"]
mapview::mapview(muni2[muni2$zt != "zt_NA","d_tp"])
muni2[order(muni2$o_tp,decreasing = TRUE),"d_tp"]

mapview::mapview(muni2[muni2$zt != "zt_NA","o_tp"])
mapview::mapview(muni2[muni2$zt != "zt_NA","o_tp"])
mapview::mapview(muni2[muni2$zt != "zt_NA","o_tp"])


# analysis----------
muni2 <- readr::read_rds("data/bra_cit/zona_trafego.rds")
setDT(muni2)
muni2[,r_tp := round(100 * o_tp / (o_tp + o_ti),2)]
muni2[,r_ti := round(100 * o_ti / (o_tp + o_ti),2)]

muni2 <- sf::st_as_sf(muni2)

mapview(muni2)  
mapview(muni2["r_ti"])
