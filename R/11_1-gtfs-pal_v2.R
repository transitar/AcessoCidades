#GTFS Palmas v2
rm(list = ls())
gc()
source('R/fun/setup.R')

sf_use_s2(use_s2 = FALSE)

# calendar.txt ------------------------------------------------------------
# Primero aquivo do GTFS. Especifica planos planos de operação do sistema.
# Foi criado esquemas de operação para os dias úteis, sábadaos e domingos. Contudo, serão 
# codificadas apenas as viagens dos dias úteis.

calendar <- data.frame(service_id = c("D","S","U"),
                       monday = c(0,0,1),
                       tuesday = c(0,0,1),
                       wednesday = c(0,0,1),
                       thursday = c(0,0,1),
                       friday = c(0,0,1),
                       saturday = c(0,1,0),
                       sunday = c(1,0,0),
                       start_date = 20220801,
                       end_date = 20220831)

write.table(calendar, file = '../11 - GTFS/muni_pal/gtfs_files/calendar.txt',sep = ',', na = "",
            row.names = F, quote = F)


# fare_attributes.txt -----------------------------------------------------

fare_attributes <- data.frame(
  fare_id = "tarifa",
  price = 4.80,
  currency_type = "BRL",
  payment_method = 0,
  transfers = 0
)

write.table(fare_attributes, file = '../11 - GTFS/muni_pal/gtfs_files/fare_attributes.txt',sep = ',', na = "",
            row.names = F, quote = F)



# feed_info.txt -----------------------------------------------------------

feed_info <- data.frame(
  feed_publisher_name = "SETURB",
  feed_publisher_url = "https://seturb.com.br/",
  feed_lang = "pt",
  feed_start_date = "20220801",
  feed_end_date = "20220831",
  feed_version = "1.0@20220801"
)
write.table(feed_info, file = '../11 - GTFS/muni_pal/gtfs_files/feed_info.txt',sep = ',', na = "",
            row.names = F, quote = F)

# stops.txt ---------------------------------------------------------------

#Palmas forneceu um shapefile datado de 2018 com os pontos de parada e um aqruivo .txr
#do sistema de monitoriamento gps datado de Maio de 2022 com alguns pontos de parada a mais
#do que no shapefile de 2018.

#como os pontos de parada do sistema de gps já possuem id único, estes serão preservados e ao juntar
#as duas bases, serão criados novos ids para os pontos de parada que estão na base shapefile mas não
#na base de gps

# Pontos de parada do arquivo de GPS

# No arquivo txt do sistema de GPS haviam 2 pontos de parada com apenas a longitude. A latitude foi
#estimada com base nos pontos com id antecedente e no ponto com id descendente, identificados manualmente
#no QGIS. Os pontos com coodenadas incompletas foram colocados entre o ponto anterior e o ponto posterior,
#na metade da distância entre os dois pontos ao longo da via.

# Leitura dos pontos de parada do GPS
stops0 <- readLines('../11 - GTFS/muni_pal/gps_files/arquivo1-3_2.txt')

stops00 <- stops0[nzchar(stops0)]
stops01 <- stops00[2:(length(stops00)-2)]
df1 <- strsplit(stops01, ";")
df1 <- do.call(rbind, df1)
df1 <- as.data.frame(df1)

names(df1) <- c('stop_id','stop_name','stop_lon','stop_lat')

#dados dos pontos de parada do GPS:
df2 <- df1 %>% st_as_sf(.,coords = c("stop_lat","stop_lon"), crs = 4326)

stops_gps <- df2 %>% st_transform(crs = "+proj=longlat + datum=WGS84") %>%
  mutate(stop_code = stop_id,
         stop_name = stop_name,
         stop_desc = NA,
         stop_lat = unlist(map(.$geometry,2)),
         stop_lon = unlist(map(.$geometry,1)),
         zone_id = NA,
         stop_URL = NA,
         location_type = 0,
         parent_station = NA,
         stop_timezone = NA,
         wheelchair_boarding = NA,
         level_id = NA,
         platform_code = NA
  )

stops_gps_txt <- stops_gps %>% st_drop_geometry()

#stops do arquivo shapefile

stops <- read_sf('../11 - GTFS/muni_pal/Pontos_Onibus_Out_2018.shp')

#Os pontos presentes no shape mas não no GPS, apesar de mais antigos, foram mantidos pois é possível que
#parte do sistema de Palmas não esteja monitorado por GPS, como normalmente é o caso do transporte
#complementar por topics

#pontos presentes no shape que não estão no GPS:

#Foi aplicado um buffer de 80m nos pontos de parada do GPS. As paradas até essa distância no shape foram
#como as mesmas do gps. Verificou-se que essa distância não causava overlaping de pontos

stops_gps_buffer <- stops_gps %>% st_transform(31982) %>% st_buffer(80)
stops_shape_gps <- stops %>% st_transform(31982) %>% st_join(stops_gps_buffer)
stops_apenas_shape <- stops_shape_gps %>% filter(is.na(stop_id == TRUE))

#157 pontos de paradas apenas no shape
#criação do id único para os 157 pontos

id_stops_gps <- stops_gps %>% distinct(stop_id)

stops_apenas_shape_id <- stops_apenas_shape %>% group_by(stop_id) %>%
  mutate(stop_id = seq(from = as.numeric(max(stops_gps$stop_id))+1,
                       to = as.numeric(max(stops_gps$stop_id))+length(stop_id),
                       by = 1)) %>% st_zm(drop = TRUE)

#Estrutura do aquivo gtfs stops.txt
stops2 <- stops_apenas_shape_id %>% select(stop_id) %>%
  st_transform(4326) %>%
  mutate(stop_code = NA,
         stop_name = NA,
         stop_desc = NA)

stops2$stop_lat <- as.data.frame(st_coordinates(stops2))$Y
stops2$stop_lon <- as.data.frame(st_coordinates(stops2))$X

stops2 <- stops2 %>%
  mutate(zone_id = NA,
         stop_URL = NA,
         location_type = 0,
         parent_station = NA,
         stop_timezone = NA,
         wheelchair_boarding = NA,
         level_id = NA,
         platform_code = NA
  )

stops2$stop_id <- seq(from = (as.numeric(max(stops_gps_txt$stop_id))+1000+1),
                      to = (as.numeric(max(stops_gps_txt$stop_id))+1000 +nrow(stops2)),
                      by = 1)

stops_shapes_txt <- stops2 %>% st_drop_geometry()
#Junção dos pontos no gps com os pontos do shape e remoção das vírgulas nos nomes dos pontos


stops_all <- rbind(stops_gps_txt,
                   stops_shapes_txt) %>%
  mutate(stop_name = gsub(",", " ", stop_name))
nrow(stops_all)
# stops_all %>% distinct(stop_id) %>% nrow()

#escrita do aquivo stops.txt
write.table(stops_all, file = '../11 - GTFS/muni_pal/gtfs_files/stops.txt',sep = ',', na = "",
            row.names = F, quote = F)

# agency.txt --------------------------------------------------------------
#Escrita do aquivo agency.txt

agency <- data.frame(agency_id = 1,
                     agency_name = "SETURB",
                     agency_url = "https://seturb.com.br/",
                     agency_timezone = "Brazil/East",
                     agency_lang = "pt",
                     agency_phone = "6332251248",
                     agency_fare_url = NA)

write.table(agency, file = '../11 - GTFS/muni_pal/gtfs_files/agency.txt',sep = ',', na = "",
            row.names = F, quote = F)


# routes.txt --------------------------------------------------------------

#novamente, há linhas no aquivo shapefile, datado de 2020 e no arquivo txt do gps datado de
#maio de 2022

#leitura do txt de linhas do gps e transofrmação em dara.frame

linhas0 <- readLines('../11 - GTFS/muni_pal/gps_files/arquivo1-4.txt')

linhas00 <- linhas0[nzchar(linhas0)]
linhas01 <- linhas00[4:(length(linhas00)-2)]
dflinhas <- strsplit(linhas01, ";")
dflinhas <- do.call(rbind, dflinhas)

dflinhas <- as.data.frame(dflinhas)

names(dflinhas) <- c('route_id','sentido','stop_sequence','stop_id')

lista_linhas_gps <- dflinhas %>%
  mutate(cod_linha = paste0(route_id, substr(sentido, start = 1L, stop = 1L))) %>%
  distinct(cod_linha, .keep_all = T)




#Existem paradas referenciadas no arquivo de rotas do gps que não estão no arquivo de paradas. Foram
#removidas.

stops_all_sf <- stops_all %>% st_as_sf(.,coords = c("stop_lon","stop_lat"), crs = 4326)

linhas_gps_info_stops <- dflinhas %>% left_join(stops_all, by = "stop_id") %>%
  drop_na(stop_lat) %>%
  st_as_sf(.,coords = c("stop_lon","stop_lat"), crs = 4326) %>%
  mutate(cod_linha = paste0(route_id, substr(sentido, start = 1L, stop = 1L)))

#here
#alguns pontos do itinerário de algumas linhas não estão no arquivo de PEDs

linhas_gps_info_stops %>% distinct(route_id) %>% nrow()

#arquivo shapefile com o nome das linhas

shapes <- st_read('../11 - GTFS/muni_pal/itinerarios2.gpkg') %>%
  st_zm(drop = T) %>%
  st_transform(4326) %>% mutate(route_id = substr(Name_1, start = 1L, stop = 3L),
                                agency_id = NA,
                                route_short_name = substr(Name_1, start = 1L, stop = 3L),
                                route_long_name = ITINERARIO,
                                route_desc = NA,
                                route_type = 3,
                                route_url = NA,
                                route_color = NA,
                                route_text_color = NA,
                                route_text_color = NA) %>%
  select(-Name_1,-ITINERARIO, -Exten_Km) %>%  distinct(route_id, .keep_all = T)
# mapview(shapes)
shapes_st <- shapes %>% st_drop_geometry()

#comparativo das linhas presentes no GPS e no shapefile

#linhas no shapefile: 66
n_lines_shape <- shapes %>% distinct(route_id) %>% nrow()
#linhas no gps: 54
linhas_gps <- linhas_gps_info_stops %>% distinct(route_id, .keep_all = T)

# nrow(linhas_gps)
#linhas no quadro de horários:
arquivo <- '../11 - GTFS/muni_pal/Quadro de Horários anexo à Ordem de Serviço Nº 12 2022 - 1º a 31 de Agosto 2022_2.xlsx'
n_abas <- length((excel_sheets(arquivo)))
wb <- loadWorkbook(arquivo)
sheets <- getSheets(wb) 
sheets <- sheets[4:(length(sheets)-5)]
names <- names(sheets)
names_linhas <- substr(names, start = 1L, stop = 3L)

lista_linhas_apenas_gps  <- lista_linhas_gps %>% filter(!cod_linha %in% names) %>% filter(!route_id %in% names)
#linhas presentes no GPS mas não na tabela de horários: 1 LUZIM e 80T ida e volta 
#linha 1 LUZIM atende o lado oeste do reservatório UHE Luís Eduardo Magalhães (Não está no shape)
#linha 80T atende Taquarussu do Porto (zona rural) e santa terezinha do TO (Não está no shape)
#linha 10L atende tende o lado oeste do reservatório UHE Luís Eduardo Magalhães (Não está no shape)
#linha 30L atende tende o lado oeste do reservatório UHE Luís Eduardo Magalhães (Não está no shape)


# teste <- linhas_gps_info_stops %>% filter(cod_linha == "30LI")
# mapview(teste)

#linhas que estão no quadro de horários mas não no gps?
#linha 610 - porém essa linha não tem nenhum horário no quadro de horários

as.data.frame(names) %>% filter(!names %in% lista_linhas_gps$cod_linha) %>% filter(!names %in% lista_linhas_gps$route_id)

#remoção da linha 610
names <- names[!610]
#Apenas a linha 610 não estava nos arquivos de GPS, porém essa linha não tinha nenhum horário
#no quadro de horários e foi removida
#Assim, todas as linhas utilizadas estão no gps, e este será o utulizado

#sttoped here


# shapes <- st_read('../11 - GTFS/muni_pal/Linhas_Onibus_Mar_2020.shp') %>% st_zm(drop = T) %>%
#   st_transform(4326)

# shapes <- st_read('../11 - GTFS/muni_pal/itinerarios.gpkg') %>% st_transform(4326)
# mydata2 <- st_collection_extract(shapes, "LINESTRING")

mapview(shapes)

routes <- linhas_gps %>%
  select(route_id) %>%
  left_join(shapes_st, by = "route_id") %>%
  mutate( agency_id = 1,
          route_short_name = ifelse(is.na(route_short_name) == T, route_id, route_short_name),
#                             route_long_name = NA,
#                             route_desc = NA,
                            route_type = 3,
#                             route_url = NA,
#                             route_color = NA,
#                             route_text_color = NA,
                            route_text_color = NA)
#   select(-Name_1,-ITINERARIO, -Exten_Km) %>% st_drop_geometry() %>% distinct(route_id, .keep_all = T)
# nrow(routes)
routes_st <- routes %>% st_drop_geometry()
routes_st$route_id <- str_remove(routes_st$route_id, pattern = " ")
routes_st$route_short_name <- str_remove(routes_st$route_short_name, pattern = " ")
nrow(routes_st)

routes_st <- routes_st %>% filter(! route_id %like% "140") %>%
  filter(! route_id %like% "620")

# routes_st %>% distinct_all() %>% nrow()

write.table(routes_st, file = '../11 - GTFS/muni_pal/gtfs_files/routes.txt',sep = ',', na = "",
            row.names = F, quote = F)


#linhas que estão no shape mas não no gps
# linhas_faltando1 <- linhas2 %>% left_join(routes, by ='route_id')
# linhas_faltando2 <- routes %>% left_join(linhas2, by ='route_id')

#linhas que tem ida e volta no gps mas não tem na planilha de horas



# routes.txt ---------------------------------------------------------------

trips0 <- readLines('../11 - GTFS/muni_pal/gps_files/arquivo1-5.txt')

trips00 <- trips0[nzchar(trips0)]
trips01 <- trips00[3:(length(trips00)-2)]

dftrips <- strsplit(trips01, ";")
dftrips <- do.call(rbind, dftrips)
# df1 <- df1[,-1]
dftrips <- as.data.frame(dftrips)
# df1[] <- lapply(df1, function(x) as.numeric(as.character(x)))

# names(df1) <- gsub('"', '', strsplit(dat[1], ',')[[1]][-1], fixed = TRUE)
names(dftrips) <- c('data','route_id','sentido','veiculo', 'hora_inicio', 'hora_fim')

#trips por dia

trips_03 <- dftrips %>% filter(data== "03/05/2022") %>% arrange(route_id, hora_inicio)

#testes
teste_010_ida <- trips_03 %>% filter(route_id == "010", sentido=="IDA")
teste_010_volta <- trips_03 %>% filter(route_id == "010", sentido=="VOLTA")

teste_021_ida <- trips_03 %>% filter(route_id == "021", sentido=="IDA") #bateu
teste_021_volta <- trips_03 %>% filter(route_id == "021", sentido=="VOLTA") #Não há

teste_041_ida <- trips_03 %>% filter(route_id == "041", sentido=="IDA") #bateu
teste_041_volta <- trips_03 %>% filter(route_id == "041", sentido=="VOLTA") #bateu


trips_04 <- dftrips %>% filter(data== "04/05/2022") %>% arrange(route_id, hora_inicio)
trips_05 <- dftrips %>% filter(data== "05/05/2022") %>% arrange(route_id, hora_inicio)

#trips da planilha
arquivo <- '../11 - GTFS/muni_pal/Quadro de Horários anexo à Ordem de Serviço Nº 12 2022 - 1º a 31 de Agosto 2022_2.xlsx'
n_abas <- length((excel_sheets(arquivo)))
dados <- tibble()

library(xlsx)
wb <- loadWorkbook(arquivo)
sheets <- getSheets(wb) 

sheets <- sheets[4:(length(sheets)-5)]
names <- names(sheets)
names_linhas <- substr(names, start = 1L, stop = 3L)

#tempos de percurso

tempo_iti <- read_excel(arquivo, sheet = "Layout", col_names = T, skip = 5) %>%
  select(1:4) %>%
  mutate(tempo_viagem = as.POSIXct(`Tempo Percurso`,
                                   origin = "1899-12-31 00:00:00")) %>%
  
  # mutate(tempo_viagem = ymd_hms(`Tempo Percurso`)) %>%
  mutate(hora2 = strftime(tempo_viagem, format = "%H:%M:%S")
)
  
# Partidas programadas
# i <- names[1]

for (i in names) {
  dados_i <- read_excel(arquivo, sheet = i, col_names = F, skip = 6)
  dados_i <- dados_i %>% subset(select = 1:6)
  names(dados_i) <- c("V1","V2","V3","V4","V5","V6")
  dados_i <- dados_i %>%  gather(horario, valor, 1:6) %>% na.omit() %>% mutate(Trecho = i)
  dados <- rbind(dados, dados_i)
}

library(hms)

#trips
dados2 <- dados %>% filter(valor %like% ":") %>%
  mutate(valor = str_remove(valor, pattern = " Mir| Cap| Pal")) %>%
  mutate(valor = as.POSIXct(valor, format = "%H:%M")) %>%
  mutate(valor = strftime(valor, format = "%H:%M:%S"))


#os tempos de viagem do gps estõ irrealistas (ex: linha 80T ida, possui 84,3 km de itinerário)
# pórem iniciou uma viagem 19:10 e encerrou 19:22 (420 km/h)

#cálculo da velocidade média do sistema 

#Os temos de percuso estão na planilha de horários (aqui, na variável dados 2)
#juunção dos tempos de percuso com a extensão dos itinerários


  # mutate(valor = ymd_hms(valor))

# routes2 <- routes %>% filter(route_id %in% names_linhas)

# write.table(routes2, file = '../11 - GTFS/muni_pal/gtfs_files/routes.txt',sep = ',', na = "",
            # row.names = F, quote = F)

# dados2 <- dados %>% mutate(x = substr(valor, start = 3L, stop = 3L)) %>% filter(x==":") %>% select(-x)

#construção do arquivo trips.txt

# shapes.txt --------------------------------------------------------------

#refazer o shapes.txt a partir do gps

#linhas_gps_info_stops



# shapes_gps <- linhas_gps_info_stops %>% group_by(cod_linha) %>%
#   st_cast("LINESTRING")
# 
# library(sp)
# library(maptools)
# library(leaflet)
# points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
#   
#   # Convert to SpatialPointsDataFrame
#   coordinates(data) <- c(long, lat)
#   
#   # If there is a sort field...
#   if (!is.null(sort_field)) {
#     if (!is.null(id_field)) {
#       data <- data[order(data[[id_field]], data[[sort_field]]), ]
#     } else {
#       data <- data[order(data[[sort_field]]), ]
#     }
#   }
#   
#   # If there is only one path...
#   if (is.null(id_field)) {
#     
#     lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
#     
#     return(lines)
#     
#     # Now, if we have multiple lines...
#   } else if (!is.null(id_field)) {  
#     
#     # Split into a list by ID field
#     paths <- sp::split(data, data[[id_field]])
#     
#     sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
#     
#     # I like for loops, what can I say...
#     for (p in 2:length(paths)) {
#       id <- paste0("line", as.character(p))
#       l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
#       sp_lines <- spRbind(sp_lines, l)
#     }
#     
#     return(sp_lines)
#   }
# }
# 
# dat <- linhas_gps_info_stops %>% select(shape_id = cod_linha, route_id, shape_pt_sequence = stop_sequence)
# 
# #here
# 
# linhas_gps_info_stops %>% filter(cod_linha == "1 LUZIMV") %>% mapview(zcol = "stop_sequence")
# 
# 
# dat$shape_pt_lat <- as.data.frame(st_coordinates(dat))$Y
# dat$shape_pt_lon <- as.data.frame(st_coordinates(dat))$X
# 
# dat <- dat %>% st_drop_geometry()
# dat2 <- dat %>% select(-route_id)
# #parei aqui
# 
# v_lines <- points_to_line(data = dat2, 
#                           long = "shape_pt_lon", 
#                           lat = "shape_pt_lat", 
#                           id_field = "shape_id", 
#                           sort_field = "shape_pt_sequence")
# 
# leaflet(data = v_lines) %>%
#   addTiles() %>%
#   addPolylines()
# 
# 
# shapes <- st_read('../11 - GTFS/muni_pal/Linhas_Onibus_Mar_2020.shp') %>% st_zm(drop = T) %>%
#   st_transform(4326)
library(gsubfn)
shapes2 <- st_read('../11 - GTFS/muni_pal/itinerarios.gpkg') %>%
  st_transform(4326) %>% 
  st_zm(drop = T) # %>%
  # mutate(route_id = separate(Name_1, sep = "\\("), into =  c("linha","sentido"))

# teste <- shapes2[1,1] %>% st_drop_geometry()

route_id <- separate(shapes2, c("Name_1"), sep = "\\(", into = c("linha","sentido"))[1] %>%
  st_drop_geometry()
route_id2 <- str_remove(route_id$linha, pattern = " ")
route_id2 <- str_remove(route_id2, pattern = " ")
route_id <- data.frame(route_id = route_id2)

# shapes2 <- shapes2[1:ncol(shapes2)-1]
sentido <- separate(shapes2, c("Name_1"), sep = "\\(", into = c("linha","sentido"))[2] %>%
  st_drop_geometry()
sentido2 <- str_remove(sentido$sentido, pattern = "\\)")
sentido <- data.frame(sentido = sentido2)
sentido2 <- substr(sentido$sentido, start = 1L, stop = 1L)
sentido <- data.frame(sentido = sentido2)

# shapes2 <- shapes2[1:ncol(shapes2)-1]
# separate(teste, c("Name_1"), sep = "\\(", into = c("linha","sentido"))[1]

# str_split(teste, pattern = "\\(")
# shapes2$route_id <- strapplyc(shapes2$Name_1, "(.*)(", simplify = T)

shapes2 <- cbind(route_id, sentido, shapes2)
  
shapes2 <- shapes2 %>%
  mutate(sentido = toupper(sentido)) %>%
  mutate(shape_id = paste0('shape', route_id, "-", sentido))


shapes_gtfs <- shapes2 %>%
  select(-Name_1,-ITINERARIO, -Exten_Km, -route_id, -sentido) %>% st_as_sf()


#mais segmentos nas linhas
# teste <- shapes2 %>%
#   filter(shape_id== "shape012-I") %>% st_transform(31982) %>% st_segmentize(units::set_units(5, m)) %>%
#   st_cast(to = "MULTIPOINT") %>% st_cast("POINT") %>% group_by(shape_id) %>%
#   mutate(shape_pt_sequence = 1:length(shape_id)) %>%
#   st_buffer(25)

shapes_segment <- shapes_gtfs %>%
  st_transform(31982) %>% st_segmentize(units::set_units(25, m)) %>%
  st_cast(to = "MULTIPOINT") %>% st_cast("POINT") %>% group_by(shape_id) %>%
  mutate(shape_pt_sequence = 1:length(shape_id)) %>% st_transform(4326)
shapes_segment$shape_pt_lat <- as.data.frame(st_coordinates(shapes_segment))$Y
shapes_segment$shape_pt_lon <- as.data.frame(st_coordinates(shapes_segment))$X

shapes_segment_txt <- shapes_segment %>% ungroup() %>%
  mutate(shape_dist_traveled = NA) %>%
  select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence, shape_dist_traveled) %>%
  st_drop_geometry()

#Escrita do arquivo sahpes.txt
write.table(shapes_segment_txt, file = '../11 - GTFS/muni_pal/gtfs_files/shapes.txt',sep = ',', na = "",
            row.names = F, quote = F)
  

#adicionar o tempo de partida e chegada nas viagens

#uma trip

dados_viagens <- dados2 %>% left_join(tempo_iti, by= c('Trecho'='Codigo')) %>%
  select(route_id = Trecho , hora_partida = valor, route_name = Nome, Km, tempo_viagem = hora2)

dados_viagens <- dados_viagens %>%
  mutate(hora_partida = as.POSIXct(hora_partida, format = "%H:%M:%S"),
         tempo_viagem = as.difftime(tempo_viagem, format = "%H:%M:%S"))
dados_viagens2 <- dados_viagens %>%
  mutate(hora_chegada = hora_partida+tempo_viagem)


#arquivo de trips

trips_gtfs <- dados_viagens2 %>%
  mutate(route_id2 = substr(route_id, start = 1L, stop = 3L),
         sentido = str_sub(route_id, start = -1, end = -1),
         service_id = "U",
         sentido = ifelse(sentido %in% c("I","V"), sentido, "I")) %>%
  group_by(route_id) %>%
  mutate(trip_seq = 1:length(route_id)) %>% ungroup() %>%
  
  mutate(menor_100 = ifelse(trip_seq/100>1, 0, 1)) %>%
  
  mutate(menor_100 = ifelse(menor_100 ==1,
                            ifelse(trip_seq/10>1, 1,2),menor_100)) %>%
  mutate(trip_seq = as.character(trip_seq)) %>%
  
  mutate(trip_seq = ifelse(menor_100 == 0, trip_seq, ifelse(
    menor_100 == 1, paste0("0", trip_seq), paste0("00", trip_seq)
    
  ))) %>%
  
  mutate(trip_seq = ifelse(trip_seq %in% c("0010", "0100"), sub(".", "", trip_seq), trip_seq)) %>%
  
  mutate(trip_id = paste0(service_id, route_id2, "-V", trip_seq, "-", sentido),
         trip_headsign = NA,
         trip_short_name = NA,
         direction_id = NA,
         block_id = NA,
         shape_id = paste0("shape", route_id2, "-", sentido),
         wheelchair_accessible = NA)

trips_gtfs_salvar <- trips_gtfs %>%
  select(route_id=route_id2,
         service_id,
         trip_id,
         trip_headsign,
         trip_short_name,
         direction_id,
         block_id,
         shape_id,
         wheelchair_accessible
         )

#aqui faltam as viagens das 5 linhas que estão no GPS e não estão na planilha de partidas

#trips03

trips_so_gps <- trips_03 %>% filter(hora_inicio %like% ":") %>%
  filter(hora_fim %like% ":") %>% filter(route_id %in% lista_linhas_apenas_gps$route_id)

# route_id <- separate(shapes2, c("Name_1"), sep = "\\(", into = c("linha","sentido"))[1] %>%
#   st_drop_geometry()
# route_id2 <- str_remove(route_id$linha, pattern = " ")
# route_id <- data.frame(route_id = route_id2)
# 
# # shapes2 <- shapes2[1:ncol(shapes2)-1]
# sentido <- separate(shapes2, c("Name_1"), sep = "\\(", into = c("linha","sentido"))[2] %>%
#   st_drop_geometry()
# sentido2 <- str_remove(sentido$sentido, pattern = "\\)")
# sentido <- data.frame(sentido = sentido2)



trips_gtfs_gps <- trips_so_gps %>%
  mutate(route_id2 = route_id,
         service_id = "U",
         sentido = ifelse(sentido %in% c("I","V"), sentido, "I")) %>%
  group_by(route_id) %>%
  mutate(trip_seq = 1:length(route_id)) %>% ungroup() %>%
  
  mutate(menor_100 = ifelse(trip_seq/100>1, 0, 1)) %>%
  
  mutate(menor_100 = ifelse(menor_100 ==1,
                            ifelse(trip_seq/10>1, 1,2),menor_100)) %>%
  mutate(trip_seq = as.character(trip_seq)) %>%
  
  mutate(trip_seq = ifelse(menor_100 == 0, trip_seq, ifelse(
    menor_100 == 1, paste0("0", trip_seq), paste0("00", trip_seq)
    
  ))) %>%
  
  mutate(trip_seq = ifelse(trip_seq %in% c("0010", "0100"), sub(".", "", trip_seq), trip_seq)) %>%
  
  mutate(trip_id = paste0(service_id, route_id2, "-V", trip_seq, "-", sentido),
         trip_headsign = NA,
         trip_short_name = NA,
         direction_id = NA,
         block_id = NA,
         shape_id = paste0("shape", route_id2, "-", sentido),
         wheelchair_accessible = NA)

trips_gtfs_gps_salvar <- trips_gtfs_gps %>%
  select(route_id=route_id2,
         service_id,
         trip_id,
         trip_headsign,
         trip_short_name,
         direction_id,
         block_id,
         shape_id,
         wheelchair_accessible
  )

trips_gtfs_gps_salvar$trip_id <- str_remove(trips_gtfs_gps_salvar$trip_id, pattern = " ")
trips_gtfs_gps_salvar$shape_id <- str_remove(trips_gtfs_gps_salvar$shape_id, pattern = " ")

trips_gtfs_all <- rbind(trips_gtfs_salvar, trips_gtfs_gps_salvar)


trips_gtfs_all$route_id <- str_remove(trips_gtfs_all$route_id, pattern = " ")


trips_gtfs_all <- trips_gtfs_all %>% filter(! trip_id %like% "140") %>%
  filter(! trip_id %like% "620")


nrow(trips_gtfs_all)
trips_gtfs_all %>% distinct(trip_id) %>% nrow()

trips_gtfs_all <- trips_gtfs_all %>% mutate(shape_id = NA)


write.table(trips_gtfs_all, file = '../11 - GTFS/muni_pal/gtfs_files/trips.txt',sep = ',', na = "",
            row.names = F, quote = F)

# trips_gtfs_gps_horarios <- trips_gtfs_gps_salvar 

# stop_times.txt ----------------------------------------------------------

#shape : shapes_segment
#horarios: trips_gtfs
#routes: routes2


shapes_km <- shapes_gtfs %>% st_as_sf() %>%
  mutate(distancia = st_length(.)) %>%
  mutate(cod_linha = paste0(substr(shape_id, start = 6L, stop = 8L),
                            substr(shape_id, start = 10L, stop = 10L)))


names_linhas <- data.frame(linha = names) %>%
  mutate(nlinha = substr(linha, 1L, 3L),
         sentido = substr(linha, 4L, 4L)) %>%
  mutate(sentido = case_when(sentido == "I" ~ "I",
                             sentido == "V" ~ "V",
                             sentido == "" ~ "I")) %>%
  mutate(cod_linha = paste0(nlinha, sentido))

shapes_km_tabela <- shapes_km %>%
  filter(cod_linha %in% names_linhas$cod_linha)

#tempos_percuso tem a extensão em km da tabela, mas difere um pouco da extensão do shape
tempos_percuso <- tempo_iti %>%
  mutate(nlinha = substr(Codigo, 1L, 3L),
         sentido = substr(Codigo, 4L, 4L)) %>%
  mutate(sentido = case_when(sentido == "I" ~ "I",
                             sentido == "V" ~ "V",
                             sentido == "" ~ "I")) %>%
  mutate(cod_linha = paste0(nlinha, sentido)) %>%
  select(-nlinha, -sentido, - Codigo)

dados_vm <- left_join(tempos_percuso ,shapes_km_tabela, by = c("cod_linha"="cod_linha")) %>%
  mutate(tempo_iti = ymd_hms(tempo_viagem)) %>%
  mutate(tempo_secs = as.numeric(seconds(tempo_iti) - seconds(ymd_hms("1899-12-31 00:00:00")))) %>%
  mutate(distancia = as.numeric(distancia)) %>%
  mutate(vm = (distancia/tempo_secs)*3.6)

rurais <- c("670I", "670V", "450I", "450I", "450V", "640I", "640V", "650I", "650V", "630I", "630V")
vm_rurais <- dados_vm %>% filter(cod_linha %in% rurais)
vm_rurais <- mean(vm_rurais$vm)
vm_urbanas <- dados_vm %>% filter(!cod_linha %in% rurais)
vm_urbanas <- mean(vm_urbanas$vm)



trips_gps_juntar <- trips_gtfs_gps %>% select(route_id,hora_partida = hora_inicio,
                                       hora_chegada = hora_fim,
                                       trip_id, shape_id)
trips_gps_juntar$shape_id <- str_remove(trips_gps_juntar$shape_id, pattern = " ")
trips_gps_juntar$trip_id <- str_remove(trips_gps_juntar$trip_id, pattern = " ")

trips_gps_juntar <- trips_gps_juntar %>%
  mutate(vm = ifelse(route_id == "80T",
                     vm_rurais,
                     vm_urbanas)) %>% left_join(shapes_km, by = "shape_id") %>%
  mutate(tempo_viagem = (as.numeric(distancia)/1000)/vm) %>%
  mutate(tempo_viagem = as.difftime(tempo_viagem, units = "hours"))


#necessário o tempo_viagem
trips_gps_juntar <- trips_gps_juntar %>%
  mutate(hora_partida = as.POSIXct(hora_partida, format = "%H:%M")) %>%
  # mutate(hora_chegada = as.POSIXct(hora_chegada, format = "%H:%M")) %>%
  # mutate(tempo_viagem = hora_chegada - hora_partida) %>%
  mutate(hora_chegada = hora_partida + tempo_viagem) %>%
  select(route_id, hora_partida, hora_chegada, trip_id, shape_id, tempo_viagem)

trips_gtfs_tabela_juntar <- trips_gtfs %>%
  select(route_id, hora_partida, hora_chegada, trip_id, shape_id, tempo_viagem)

trips_all_stoptimes <- rbind(trips_gtfs_tabela_juntar,
                             trips_gps_juntar)


stop_times0 <- trips_all_stoptimes %>% group_by(trip_id) %>% left_join(shapes_segment) %>% ungroup()

stop_times0 <- stop_times0 %>% group_by(trip_id) %>%
  mutate(arrival_time = ifelse(shape_pt_sequence == min(shape_pt_sequence),
                              hora_partida,
         ifelse(shape_pt_sequence == max(shape_pt_sequence),
                hora_chegada,
                NA)))
stop_times0 <- stop_times0 %>%
  mutate(arrival_time = as.POSIXct(arrival_time, origin = "1970-01-01"))


stop_times11 <- stop_times0 %>% group_by(trip_id) %>%
  mutate(aumento = tempo_viagem/max(shape_pt_sequence)*shape_pt_sequence) %>%
  mutate(arrival_time3 = (hora_partida+aumento))



# stop_times1 <- stop_times0 %>% group_by(trip_id) %>%
#   mutate(arrival_time2 = ifelse(is.na(arrival_time)==T,
#                                tempo_viagem/max(shape_pt_sequence),
#                                arrival_time)) %>% group_by(trip_id) %>%
#   mutate(arrival_time3 = cumsum(arrival_time2)) %>%
#   mutate(arrival_time3 = as.POSIXct(arrival_time3, origin = "1970-01-01"))

# teste <- stop_times11 %>% distinct(trip_id)
# teste[1,1]

teste0 <- stop_times11 %>%
  filter(trip_id== "U140-V002-I") %>%
  st_as_sf(.,coords = c("shape_pt_lon","shape_pt_lat"), crs = 4326) %>%
  st_transform(31982) %>%
  st_buffer(25)
  
mapview(teste0, zcol = "shape_pt_sequence")




#asem segmentos nas linhas
# teste <- shapes2 %>% st_cast(to = "MULTIPOINT") %>% st_cast("POINT")%>%
#   filter(shape_id== "shape012-I") %>% st_transform(31982) %>% st_buffer(10)
# paradas0 <- stops

# teste2 <- shapes2 %>%
#   filter(shape_id== "shape012-I") %>% st_transform(31982) %>% st_buffer(20)

#dados de PEDs do shape enviado pela prefeitura de Palmas
paradas0 <-stops_all_sf %>% st_transform(31982)

#Dados de PEDs dos aquivos de monitoramento GPS enviado pela prefeitura de Palmas

# paradas0 <- stops_gps %>% st_transform(31982)

stop_times_shapes <- stop_times11 %>% drop_na(shape_pt_lat, shape_pt_lon) %>%
  st_as_sf(.,coords = c("shape_pt_lon","shape_pt_lat"), crs = 4326) %>% st_transform(31982) %>%
  st_buffer(25)

# mapview(stop_times_shapes)

stop_times_peds <- st_join(paradas0, stop_times_shapes) %>% drop_na(shape_id) %>% st_as_sf() %>%
  group_by(trip_id) %>%
  distinct(stop_id, .keep_all = T) %>% arrange(shape_pt_sequence)

stop_times_peds2 <- stop_times_peds %>%
  group_by(trip_id) %>% arrange(shape_pt_sequence)


testez <- stop_times_peds %>% filter(trip_id == "U140-V002-I")


# mapview(testez, zcol = "shape_pt_sequence")

#estrutura do arquivo stop_times.txt

dia <- day(stop_times_peds2$arrival_time3)
dia_certo <- min(dia)

stop_times_peds3 <- stop_times_peds2 %>%
  mutate(dia = day(arrival_time3))

stop_times <- stop_times_peds3 %>%
  st_drop_geometry() %>%
  select(trip_id,
         arrival_time = arrival_time3,
         departure_time = hora_partida,
         stop_id,
         dia) %>%
  mutate(arrival_time = strftime(arrival_time, format = "%H:%M:%S")) %>%
  mutate(departure_time = arrival_time) %>%
  group_by(trip_id) %>%
  mutate(stop_sequence = 1:length(trip_id),
         stop_headsign = NA,
         pickup_type = NA,
         drop_off_type = NA,
         shape_dist_traveled = NA) %>%
  ungroup() %>%
  arrange(trip_id, stop_sequence) %>%
  mutate(departure_time = ifelse(dia > dia_certo,
                                 sub(pattern = "(.{1}).", replacement = "24", x = departure_time),
                                 departure_time),
         arrival_time = ifelse(dia > dia_certo,
                                 sub(pattern = "(.{1}).", replacement = "24", x = arrival_time),
                                 arrival_time)) %>% select(-dia)

stop_times <- fread('../11 - GTFS/muni_pal/gtfs_files/stop_times.txt')

stop_times_txt <- stop_times %>% filter(! trip_id %like% "140") %>%
  filter(! trip_id %like% "620")

stop_times %>% distinct(trip_id) %>% nrow()

write.table(stop_times_txt, file = '../11 - GTFS/muni_pal/gtfs_files/stop_times.txt',sep = ',', na = "",
            row.names = F, quote = F)




gtfs_files <- list.files('../11 - GTFS/muni_pal/gtfs_files/', full.names = T)

zip(zipfile = "../11 - GTFS/muni_pal/gtfs_files/gtfs_pal.zip", files = gtfs_files)




#
teste3 <- st_join(paradas0, teste0) %>% drop_na(shape_id) %>% st_as_sf() %>%
  distinct(stop_id, .keep_all = T) %>% arrange(shape_pt_sequence)
mapview(teste3, zcol="shape_pt_sequence")

teste4 <- teste3 %>%
  mutate(diff_time = arrival_time3-lag(arrival_time3)) %>%
  filter(diff_time > 19)
mapview(teste4, zcol="shape_pt_sequence")

mapview(teste3) + mapview(teste, color = "#00a394")

teste3 %>% slice(1:10) %>% mapview()

 mapview(stops2)+ mapview(teste2, color = "#00a394")

shapes3 <- shapes2 %>% st_cast(to = "MULTIPOINT") %>% st_cast("POINT") %>%
  



shapes4 <- shapes3 %>%
  mutate(shape_pt_lat = unlist(map(.$geometry,2)),
         shape_pt_lon = unlist(map(.$geometry,1))) %>% st_drop_geometry() %>%
  group_by(shape_id) %>%
  mutate(shape_pt_sequence = 1:length(shape_id))

write.table(shapes4, file = '../11 - GTFS/muni_pal/gtfs_files/shapes.txt',sep = ',', na = "",
            row.names = F, quote = F)


