# 04.0-setup_r5r

# load packages -----------------------------------------------------------
source('./R/fun/setup.R')


# copy new gtfs to r5r folders -------------------------

# create folders

sigla_muni <- 'noh'
ano <- 2022

#criação dos diretório de rede dos municípios
create_diretorios <- function(sigla_muni){
  
  dir.create(sprintf("../r5r/network/2022/muni_%s", sigla_muni), recursive = TRUE)
  
}


walk(munis_list$munis_df$abrev_muni, create_diretorios)

# define gtfs name for each city
gtfs_files <- read_excel("../data-raw/gtfs_names/gtfs_files.xlsx", sheet = "gtfs_files")

gtfs_files <- gtfs_files %>%
  mutate(gtfs_path = sprintf("../data-raw/gtfs/muni_%s/%s/%s.zip", sigla_muni, ano, gtfs))

sigla_muni1 <- "rma"; ano1 <- 2022
# sigla_muni1 <- "rec"; ano1 <- 2019
# sigla_muni1 <- "spo"; ano1 <- 2019
copy_gtfs <- function(sigla_muni1, ano1) {
  
  message(paste("rodando", sigla_muni1))
  
  gtfs_files1 <- gtfs_files %>% filter(ano == ano1 & sigla_muni == sigla_muni1)
  
  # delete any gtfs
  if (dir.exists(sprintf("../r5/network/%s/muni_%s", ano1, sigla_muni1))) {
    
    a <- dir(sprintf("../../r5/network/%s/muni_%s", ano1, sigla_muni1), pattern = ".zip$", full.names = TRUE)
    file.remove(a)
    
    
  }
  
  # copy gtfs updated ---------------------
  dir.create(sprintf("../r5r/network/%s/muni_%s/", ano1, sigla_muni1), recursive = TRUE)
  # copy
  purrr::walk(gtfs_files1$gtfs_path,
              file.copy,
              to = sprintf("../r5r/network/%s/muni_%s/", ano1, sigla_muni1),
              overwrite = TRUE)
}


walk(munis_list$munis_modo[ano_modo == 2022 & modo == "todos"]$abrev_muni, copy_gtfs, ano1 = 2017)
# walk(munis_list$munis_modo[ano_modo == 2018 & modo == "todos"]$abrev_muni, copy_gtfs, ano1 = 2018)
# walk(munis_list$munis_modo[ano_modo == 2019 & modo == "todos"]$abrev_muni, copy_gtfs, ano1 = 2019)

#stopped here
sigla_muni1 <- 'noh'
ano1 <- 2022
# copy other r5 files ----------------------------
copy_r5_files <- function(sigla_muni1, ano1){
  
  # malha viaria
  malha_viaria_dir <- sprintf("../data-raw/malha_viaria/%s/muni_%s/muni_%s_%s.osm.pbf",ano1, sigla_muni1, sigla_muni1, ano1)
  
  file.copy(from = malha_viaria_dir,
            to = sprintf("../r5r/network/%s/muni_%s/", ano1, sigla_muni1))
  
  # topografia
  topografia_dir <- sprintf("../data-raw/topodata/2020/muni_%s/topografia_%s.tif", sigla_muni1, sigla_muni1)
  
  file.copy(from = topografia_dir,
            to = sprintf("../r5r/network/%s/muni_%s/", ano1, sigla_muni1))
  
}


walk(munis_list$munis_modo[ano_modo == 2017]$abrev_muni, copy_r5_files, ano1 = 2017)
walk(munis_list$munis_modo[ano_modo == 2018]$abrev_muni, copy_r5_files, ano1 = 2018)
walk(munis_list$munis_modo[ano_modo == 2019]$abrev_muni, copy_r5_files, ano1 = 2019)





