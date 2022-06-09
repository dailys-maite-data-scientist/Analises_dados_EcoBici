#*********************************************************************#
#             Data Analytics Visualization Challenge                  #
#                           Mercado Libre                             #                
#*********************************************************************#
#Librerias
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(lubridate)

#TABLA: Viajes
#Cargar los datos de los viajes
viajes <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/Tablas_tratadas/Total_viajes.csv", sep=',', stringsAsFactors = F, encoding="uft-8")

summary(viajes)

#TABLA: recorridos-realizados
#Cargar los datos de los recorridos
recorrido14 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/Tablas_tratadas/recorridos/recorridos-realizados-2014.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

recorrido15 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/recorridos-realizados-2015.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

recorrido16 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/recorridos-realizados-2016.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

recorrido17 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/recorridos-realizados-2017.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

recorrido18 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/recorridos-realizados-2018.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

recorrido19 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/recorridos-realizados-2019.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

recorrido20 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/trips_2020.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

recorrido21 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/trips_2021.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

#Union de tablas
recorrido18 <- select(recorrido18, -id_usuario)
recorrido15_18 <- bind_rows(recorrido15, recorrido16, recorrido17, recorrido18)
recorrido19_21 <- bind_rows(recorrido19, recorrido20, recorrido21)

#Crear columnas necesarias
recorrido14_FO <- ymd_hms( recorrido14$ORIGEN_FECHA)
recorrido14_FD <- ymd_hms( recorrido14$DESTINO_FECHA)

duracion_viaje14 <- as.integer(recorrido14_FD - recorrido14_FO)/60

recorrido15_FO <- ymd_hms( recorrido15_18$fecha_origen_recorrido)
recorrido15_FD <- ymd_hms( recorrido15_18$fecha_destino_recorrido)

duracion_viaje15_18 <- as.integer(recorrido15_FD - recorrido15_FO)

duracion_viaje19_21 <- (recorrido19_21$duracion_recorrido)/60

recorrido14 <- cbind(recorrido14, duracion_viaje14)
recorrido15_18 <- cbind(recorrido15_18, duracion_viaje15_18)
recorrido19_21 <- cbind(recorrido19_21, duracion_viaje19_21)

recorridos_part1 <- cbind(recorrido14$ORIGEN_FECHA, recorrido14$duracion_viaje14)
recorridos_part2 <- cbind(recorrido15_18$fecha_origen_recorrido, recorrido15_18$duracion_viaje15_18)
recorridos_part3 <- cbind(recorrido19_21$fecha_origen_recorrido, recorrido19_21$duracion_viaje19_21)

recorridos_All <- rbind(recorridos_part2, recorridos_part3)

hora15_hora <- hour(recorrido15_FO)

hora15 <- hour(recorrido15_FO)
for (i in 1:length(hora15)) {
  if(hora15[i] >= 6 && hora15[i] <= 12){
    hora15[i] = "Mañana"
  }
  
  else if(hora15[i] > 12 && hora15[i] <= 18){
    hora15[i] = "Tarde"
  }
  
  else if(hora15[i] > 18 && hora15[i] <= 24){
    hora15[i] = "Noche"
  }
  
  else (hora15[i] = "Madrugada")
}

recorrido15_18 <- cbind(recorrido15_18, hora15, hora15_hora)

recorrido19_FO <- ymd_hms( recorrido19_21$fecha_origen_recorrido)
hora19_hora <- hour(recorrido19_FO)

hora19 <- hour(recorrido19_FO)
for (i in 1:length(hora19)) {
  if(hora19[i] >= 6 && hora19[i] <= 12){
    hora19[i] = "Mañana"
  }
  
  else if(hora19[i] > 12 && hora19[i] <= 18){
    hora19[i] = "Tarde"
  }
  
  else if(hora19[i] > 18 && hora19[i] <= 24){
    hora19[i] = "Noche"
  }
  
  else (hora19[i] = "Madrugada")
}

recorrido19_21 <- cbind(recorrido19_21, hora19, hora19_hora)

#Modificar Id_estaciones para hacer join
temp2 <- str_sub(recorrido19_21$id_estacion_origen, end = -10)
temp3 <- str_sub(recorrido19_21$id_estacion_destino, end = -10)

recorrido19_21$id_estacion_origen <- as.numeric(temp2)
recorrido19_21$id_estacion_destino <- as.numeric(temp3)

temp4 <- str_sub(recorrido19_21$id_usuario, end = -10)
recorrido19_21$id_usuario <- as.numeric(temp4)

#Salvar tablas
write.csv(recorrido14, "recorrido14.csv", row.names = FALSE)
write.csv(recorrido15_18, "recorrido15_18.csv", row.names = FALSE, sep = ";", dec = ",")
write.csv(recorrido19_21, "recorrido19_21.csv", row.names = FALSE)
write.table(recorridos_All, "recorridos_All.csv", row.names = FALSE, sep = ";", dec = ",")


#TABELA: Ciclovias
#Cargar los datos de las ciclovías
ciclovias <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/ciclo_temp.csv", sep=';', stringsAsFactors = F, encoding="uft-8")

summary(ciclovias)

#Tiene NA la tabela?
apply(X = is.na(ciclovias), MARGIN = 2, FUN = sum)

#Remover columnas con muchos NA o irrelevantes
ciclovias_tratada = select (ciclovias, -c (nomanter, observa, ciclo_obse))

#Separar lattitud y longitud 
ciclovias_tratada$WKT = str_sub(ciclovias_tratada$WKT, start = 19, end = -3) # começa no 19 caractere

ciclovias_tratada = ciclovias_tratada %>% 
  separate(data = ., col = WKT, 
           into = c("coordenada1", "coordenada2"), sep = ",")

ciclovias_tratada = ciclovias_tratada %>% 
  separate(data = ., col = coordenada1, 
           into = c("Longitud1", "Latitud1"), sep = "[[:space:]]+")

ciclovias_tratada = ciclovias_tratada %>% 
  separate(data = ., col = coordenada2, 
           into = c("Longitud2", "Latitud2"), sep = "[[:space:]]+")

#Convertir string en double
ciclovias_tratada$Longitud1<- as.double(ciclovias_tratada$Longitud1)
ciclovias_tratada$Longitud2<- as.double(ciclovias_tratada$Longitud2)
ciclovias_tratada$Latitud1<- as.double(ciclovias_tratada$Latitud1)
ciclovias_tratada$Latitud2<- as.double(ciclovias_tratada$Latitud2)

#Renombrar y transformar el tipo de dato de la columna Comuna.
for (i in 1:length(ciclovias_tratada$COMUNA)) {
  temp <- ciclovias_tratada$COMUNA[i]
  ciclovias_tratada$COMUNA[i] <- paste("Comuna", temp, sep = " ")
}
                                    
#Salvar tablas
write.csv(ciclovias_tratada, "ciclovias_tratada.csv", row.names = FALSE)

#TABELA: nuevas-estaciones-bicicletas-publicas
#Cargar los datos de las estaciones
estaciones <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/datos/nuevas-estaciones-bicicletas-publicas.csv", 
                       sep=',', stringsAsFactors = F, encoding="UFT-8")

summary(estaciones)

#Tiene NA la tabela?
apply(X = is.na(estaciones), MARGIN = 2, FUN = sum)

#Separar lattitud y longitud 
estaciones$WKT = str_sub(estaciones$WKT, start = 8, end = -2) 

estaciones = estaciones %>% 
  separate(data = ., col = WKT, 
           into = c("Longitud", "Latitud"), sep = "[[:space:]]+")


#Convertir string en double
estaciones$Longitud<- as.double(estaciones$Longitud)
estaciones$Latitud<- as.double(estaciones$Latitud)

#Salvar tablas
write.csv(estaciones, "estaciones.csv", row.names = FALSE)


#TABLA: Usuarios_ecobici
#Cargar los datos de los usuarios

usuarios15 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/Datos/Tablas_tratadas/recorridos/usuarios-ecobici-2015.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

usuarios16 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/Datos/Tablas_tratadas/recorridos/usuarios-ecobici-2016.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

usuarios17 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/Datos/Tablas_tratadas/recorridos/usuarios-ecobici-2017.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

usuarios18 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/Datos/Tablas_tratadas/recorridos/usuarios-ecobici-2018.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

usuarios19 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/Datos/Tablas_tratadas/recorridos/usuarios_ecobici_2019.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

usuarios20 <- read.csv("C:/Users/COPEL3/Documents/Maite/Meli/Datos/Tablas_tratadas/recorridos/usuarios_ecobici_2020.csv", 
                        sep=',', stringsAsFactors = F, encoding="UFT-8")

#Union de tablas
usuarioso15_18 <- bind_rows(usuarios15, usuarios16, usuarios17, usuarios18)
usuarios19_20 <- bind_rows(usuarios19, usuarios20)

#Normalizar datos genero
for (i in 1:length(usuarios19_20$genero_usuario)) {
  if (usuarios19_20$genero_usuario[i] == "FEMALE"){
    usuarios19_20$genero_usuario[i] <- "F"
  }
  if (usuarios19_20$genero_usuario[i] == "MALE"){
    usuarios19_20$genero_usuario[i] <- "M"
  }
  if (usuarios19_20$genero_usuario[i] == "OTHER"){
    usuarios19_20$genero_usuario[i] <- "OTRO"
  }
  if (usuarios19_20$genero_usuario[i] == " "){
    usuarios19_20$genero_usuario[i] <- "No declarado"
  }
}

#Crear columnas necesarias
Fecha_alta19 <- dmy(usuarios19_20$fecha_alta)
Fecha_alta15 <- ymd(usuarioso15_18$fecha_alta)

usuarios19_20 <- cbind(usuarios19_20, Fecha_alta19)
usuarioso15_18 <- cbind(usuarioso15_18, Fecha_alta15)

#Salvar tablas
write.csv(usuarioso15_18, "usuarioso15_18.csv", row.names = FALSE)
write.csv(usuarios19_20, "usuarios19_20.csv", row.names = FALSE)






