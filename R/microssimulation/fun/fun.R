#Load packages
source('./fun/setup.R')


# age_agg and gender classification ---------------------------------------
func_age<-
  function(data,age,first_col,col_name){
    data<- data %>%
      mutate(
        across(V022:V134,~as.numeric(.x)))
    
    data_orig<-data
    data<- sapply(seq(first_col+1,ncol(data)-age+1,by=age),
                function(i) {
                  if (i==(first_col+1)) 
                    rowSums(data[,first_col:(i+age-1)])
                  else rowSums(data[,i:(i+age-1)])})
    colnames(data)<-sapply(seq(age,100,by=age),function(i) return(paste0(col_name, as.character(i))))    
    data<- cbind(data_orig[,1:(first_col-1)],data)
    return(data)
    }


# Remove number of household heads from income data (remove duplic --------
sub_function<-function(start,end,data,by){
  for (i in seq(start,end)){
    data[i]<-data[i]-data[i+by]
  }
  return(data)
}


#  Create the matrices ----------------------------------------------------

create_mat<- function(data, first_col, last_col){
  for(i in 1:(length(data))){
    if (i==first_col){
      micro_matrix <- model.matrix(~ data[,colnames(data)[i]] - 1)
    }
    if (i>first_col && i<=last_col){
      micro_matrix<-cbind(micro_matrix,
                                 model.matrix(~ data[,colnames(data)[i]] - 1))
      }
  }
  colnames(micro_matrix)<-substr(colnames(micro_matrix),
                                        26,
                                        nchar(colnames(micro_matrix)))
  return(micro_matrix)
}

# Create the grid ---------------------------------------------------------

#Function edited from Daniel Herszenhut
generate_hex_grid <- function(city_name = "Fortaleza", res = 9) {
  router<-tolower(substr(city_name,1,3))
  code_muni<-as.numeric(as.data.frame(lookup_muni(city_name)[1])%>% 
                          mutate(code_st = tolower(substr(code_muni,1,2))) %>% 
                          filter(grepl(code_state,code_st)) %>%
                          select(code_muni))
  city <- read_municipality(code_muni)
  if (res <= 9) {
    for_crs <- st_crs(city)
    for_bbox <- st_bbox(city)
    xmin <- for_bbox["xmin"]
    xmax <- for_bbox["xmax"]
    ymin <- for_bbox["ymin"]
    ymax <- for_bbox["ymax"]
    shape_bbox <- st_polygon(list(rbind(c(xmin, ymax), c(xmax, ymax), c(xmax, ymin), c(xmin, ymin), c(xmin, ymax)))) %>% 
      st_sfc() %>% 
      st_set_crs(for_crs) %>% 
      st_as_sf()

    city <- city %>% st_transform(31984)
    hex_ids <- h3jsr::polygon_to_cells(shape_bbox, res = res, simple = FALSE)
    hex_grid <- unlist(hex_ids$h3_polyfillers) %>% 
      h3jsr::cell_to_polygon(simple = FALSE) %>% 
      st_transform(31984) %>% 
      st_intersection(city) %>% 
      st_transform(for_crs) %>% 
      dplyr::select(h3_address, geometry)
    
  } else {
    
    hex_ids <- h3jsr::polyfill(city, res = res, simple = FALSE)
    
    hex_grid <- unlist(hex_ids$h3_polyfillers) %>%
      h3jsr::h3_to_polygon(simple = FALSE) %>%
      dplyr::select(h3_address, geometry)
    
  }
  
  router_folder <- paste0("./data/02_shp/", router, "_res_", res)
  if (!file.exists(router_folder)) dir.create(router_folder)
  
  grid_data_path <- paste0(router_folder, "/grid_raw.rds")
  
  readr::write_rds(hex_grid, grid_data_path)
  
}


# Aggregate data to grid and return the grid ------------------------------

grid_with_data <- function(state = "CE",
                            city_name = "Fortaleza", res = 9) {
  #generate_hex_grid(city_name=city_name, res=res)
  router<-tolower(substr(city_name,1,3))
  router<-city
  
  # read data from statistical grid
  grid_stat<- read_statistical_grid(code_grid = state) %>%
    mutate(
      POP = ifelse(is.na(POP), 0, POP),
      MASC = ifelse(is.na(MASC), 0, MASC),
      FEM = ifelse(is.na(FEM), 0, FEM))%>% 
    select(-c("ID_UNICO","nome_1KM","nome_5KM","nome_10KM","nome_50KM",
              "nome_100KM","nome_500KM","QUADRANTE","DOM_OCU")) %>% 
    st_transform(31981) %>% 
    st_make_valid()

  # read given resolution hexagonal grid and aggregate data to it
  router_folder <- paste0("./data/02_shp/", router, "_res_", res)
  # hex_grid <- readr::read_rds(paste0(router_folder, "/grid_raw.rds"))
  hex_grid <- readr::read_rds(paste0(router_folder, "/grid_raw.rds"))
  hex_grid_crs <- st_crs(hex_grid)
  hex_grid <- hex_grid %>% st_transform(31981)
  hex_grid_with_census <- st_interpolate_aw(grid_stat, hex_grid, extensive = TRUE)
# mapview(grid_stat)
  hex_grid_with_data <- hex_grid %>% 
    st_join(hex_grid_with_census,join=st_equals) %>%
    st_as_sf() %>% 
    st_set_crs(31981) %>% 
    mutate(MASC= round(MASC,0),
           FEM= round(FEM,0),
           POP= round(POP,0)) %>%
    st_as_sf() %>%
    st_transform(31981)
  
  grid_data_path <- paste0(router_folder, "/grid_with_data.rds")
  readr::write_rds(hex_grid_with_data, grid_data_path)
  write.csv(hex_grid_with_data, paste0(router_folder, "/grid_with_data.csv"))
  return(hex_grid_with_data)
}


# Create Hex per tract ----------------------------------------------------

hex_per_tract<- function( state = "CE",
                          hex_grid_path = "data/02_shp/for_res_9/grid_raw.rds",
                          microsim_data_path = "data/03_microsimulated/complete/microsim_data.RDS"){

grid_blank<- read_rds(hex_grid_path) %>% 
  st_transform(31981)

#Divide os hex pelos setores (um hex, mais de um setor)
dd<- st_intersection(grid_blank,census_tract) %>% 
      select("id_hex","code_tract","geometry")

#Coloca os dados de MASC e FEM nos hex divididos

grid_stat<- read_statistical_grid(code_grid = state) %>%
  mutate(
    POP = ifelse(is.na(POP), 0, POP),
    MASC = ifelse(is.na(MASC), 0, MASC),
    FEM = ifelse(is.na(FEM), 0, FEM))%>% 
  select(-c("ID_UNICO","nome_1KM","nome_5KM","nome_10KM","nome_50KM",
            "nome_100KM","nome_500KM","QUADRANTE","DOM_OCU")) %>% 
  st_transform(31981) %>% 
  st_make_valid()

dd2<- st_interpolate_aw(grid_stat,dd,extensive = TRUE)

#Adiciona setor e codigo do Hex aos Hex divididos
dd_with_data <- dd %>% 
  st_join(dd2,join=st_equals) %>%
  st_as_sf() %>% 
  st_set_crs(31981)

#Soma FEM e MASC por setor
dd_per_tract<- dd_with_data %>% group_by(code_tract) %>%
                summarise(MASC_tract=sum(MASC),
                          FEM_tract=sum(FEM)) %>% 
                st_drop_geometry()


#Adiciona soma do setor no dd_with_data
dd_with_data <- dd_with_data %>% left_join(dd_per_tract,
                                           by=c("code_tract"="code_tract"))

#Percentual das pessoas do setor para cada hex dividido
dd_with_data<- dd_with_data %>% mutate(perc_masc = MASC/MASC_tract,
                                       perc_fem = FEM/FEM_tract)

#Somatorio dos individuos segundo microssimulação
microsim_data<-readRDS(microsim_data_path)
dd_micro<- microsim_data %>% group_by(code_tract) %>%
          summarise(n_masc = sum(!grepl("w",age_sex)),
                    n_fem = sum(grepl("w",age_sex)))
                    
#Add somatorio segundo microssimulação to dd_with_data
dd_with_data <- dd_with_data %>% left_join(dd_micro,
                                           by=c("code_tract"="code_tract"))

dd_with_data<- dd_with_data %>% 
                mutate(across(n_masc:n_fem,~ifelse(is.na(.x),0,.x)))


summary(dd_with_data)
#Calculate n individuals by the Hex part

dd_with_data<- dd_with_data %>% 
  mutate(n_masc_hex = floor(perc_masc*n_masc),
        n_fem_hex = floor(perc_fem*n_fem)) 

return(dd_with_data)
}





# Adjust number of aggregate values ---------------------------------------

set.seed(100)
abs
distr_ind<- function(x){
  data<- inc_pc_data[x,]
  x2 <- sample(which(colnames(inc_pc_data)=="inc_pc_0"):which(colnames(inc_pc_data)=="inc_pc_2_plus"),
               data$dif,replace = TRUE)
  data[,i]<-data[,i]+1
  data<- data %>% select(code_tract,inc_pc_0:inc_pc_2_plus)
  return(data)
}  

# Allocate individuals to Hex ---------------------------------------------

allocate_ind<- function(id_census_tract){
  router_folder<-"data/03_microsimulated/complete/allocated"
  if (!file.exists(router_folder)) dir.create(router_folder)
  
  data_grid <- hex_tract %>% 
    filter(code_tract == id_census_tract)
  
  #Men allocation
  micro <- microsim_data_men %>% 
    filter(code_tract == id_census_tract) 
  df<-data.frame(n_ind=data_grid$n_masc_hex,
                 id_hex=data_grid$id_hex) %>% filter(n_ind>0)
  n_ind <- df$n_ind
  id_hex <- df$id_hex
  f<-min(nrow(micro),sum(n_ind))
  lista <- rep(n_ind,n_ind)
  lista.id_hex <- rep(id_hex,n_ind)
  lista.id_hex<-sample(lista.id_hex,f)
  lista<-sample(lista,f)
  micro <- micro %>% sample_n(f) %>%
    mutate(lista, hex = lista.id_hex)
  
  saveRDS(micro,paste0("data/03_microsimulated/complete/allocated/micro_men_",id_census_tract,".RDS"))
  
  #Women allocation
  micro <- microsim_data_wom %>%
    filter(code_tract == id_census_tract)
  df<-data.frame(n_ind=data_grid$n_fem_hex,
                 id_hex=data_grid$id_hex) %>% filter(n_ind>0)
  n_ind <- df$n_ind
  id_hex <- df$id_hex
  f<-min(nrow(micro),sum(n_ind))
  lista <- rep(n_ind,n_ind)
  lista.id_hex <- rep(id_hex,n_ind)
  lista.id_hex<-sample(lista.id_hex,f)
  lista<-sample(lista,f)
  micro <- micro %>% sample_n(f) %>%
    mutate(lista, hex = lista.id_hex)

  saveRDS(micro,paste0("data/03_microsimulated/complete/allocated/micro_women_",id_census_tract,".RDS"))
}

# Data basico
func_data_basico<- function(path_data){

  data_basico_census=path_data
  
  #Data com rendimento
  
  data_basico<- read_xls(data_basico_census) %>%
  
  # data_basico<- fread(data_basico_census,sep=";") %>%
    # data_basico<- read.csv(data_basico_census,sep=";") %>%
    rename(code_tract=Cod_setor)%>%
    mutate(
      code_tract= as.factor(code_tract),
      # across(V001:V012,~as.numeric(ifelse(.x == "X" | is.na(.x), NA,
      #                                     sub(",", ".", sub(".", "", .x, fixed=TRUE), fixed=TRUE))))
      
      across(V001:V012,~as.numeric(ifelse(.x == "X" | is.na(.x), NA,
                                          .x)))
      
    ) %>%
    filter(!is.na(V011) & !is.na(V012)) %>%
    select(code_tract,V002,V009,V010,V011,V012) %>%
    mutate(renda_media09 = V009/510,
           renda_media11 = V011/510,
           variancia09 = V010/(510*510),
           variancia11= V012/(510*510)
    ) %>%
    mutate(across(renda_media09:variancia11,~ifelse(is.na(.x),0,.x)))
  return(data_basico)
}










