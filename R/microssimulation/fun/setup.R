ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  
}

lpak<- function(pkg){sapply(pkg, require, character.only = TRUE)}

#List of packeges needed
packages <- c("tidyverse",
              "sp",
              "sf",
              "mapview",
              "data.table",
              "scales",
              "httr",
              'geobr',
              "tmap",
              "leaflet",
              "purrr",
              "MetBrewer",
              "elevatr",
              "magick",
              "glue",
              "fontawesome",
              "grid",
              "svgtools",
              "rstudioapi",
              "readr",
              "readxl",
              "stringr",
              "ipfp",
              "BSDA",
              "progress",
              "furrr",
              "aopdata",
              "basemaps",
              "r5r")

#Running the functions
suppressMessages(ipak(packages))
suppressMessages(lpak(packages))

rm(packages,lpak,ipak)
