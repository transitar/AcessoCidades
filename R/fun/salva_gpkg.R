
sigla_muni <- "poa"

salva_gpkg <- function(sigla_muni) {

if (file.exists(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))){
a <- read_rds(sprintf('../data/dados_hex/muni_%s/dados_hex_%s.rds', sigla_muni, sigla_muni))
write_sf(a, sprintf('../data/dados_hex/muni_%s/dados_hex_%s.gpkg', sigla_muni, sigla_muni))
}
if (file.exists(sprintf('../data/saude/cnes/muni_%s_saude_cnes/muni_%s_cnes_geocoded_2019.gpkg', sigla_muni, sigla_muni))){
b <- read_rds(sprintf('../data/saude/cnes/muni_%s_saude_cnes/muni_%s_cnes_geocoded_2019.rds', sigla_muni, sigla_muni))
write_sf(b, sprintf('../data/saude/cnes/muni_%s_saude_cnes/muni_%s_cnes_geocoded_2019.gpkg', sigla_muni, sigla_muni))
}
if (file.exists(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm.rds', sigla_muni, sigla_muni))){
c <- read_rds(sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm.rds', sigla_muni, sigla_muni))
write_sf(c, sprintf('../data-raw/lazer/osm/muni_%s_lazer_osm/muni_%s_lazer_osm.gpkg', sigla_muni, sigla_muni))
}
}

walk(munis_list$munis_df$abrev_muni, salva_gpkg)
