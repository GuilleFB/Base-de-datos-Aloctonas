# Creación de la base de datos de Aloctonas

rm(list=ls(all=TRUE)) # Borrar todo los objetos
invisible(capture.output(gc())) # Limpiar la memoria

library(readxl)
library(RColorBrewer)
library(readr)
library(tidyr)
library(dplyr)
library(rgdal)
library(raster)
library(knitr)
library(sp)
library(sf)
library(readxl)
library(gdistance)
library(rgeos)
library(ggplot2)
library(ggimage)
library(maptools)
library(sf)
library(smoothr)
library(worms)
library(stringi)
library(lubridate)
library(mapview)
library(stringr)
library(beepr)
#library(xlsx)
library(tidygeocoder)
library(rvest)
library(magrittr)
library(jsonlite)
library(rgbif)
library(writexl)

print("Elegir la carpeta Archivos de la base de datos de Aloctonas")
path=choose.dir()
setwd(path) # Elegir la carpeta Archivos de la base de datos de Aloctonas

Archivos_excel=dir()[which(str_detect(dir(), ".xlsx"))]


# "BD_primeros registros_CAN.xlsx",  ####
# "BD_primeros registros_ESAL.xlsx", ####
# "BD_primeros registros_LEBA.xlsx", ####
# "BD_primeros registros_NOR.xlsx",  #### 
# "BD_primeros registros_SUD.xlsx"   ####
# EAI5

BD_Primeros=NULL
demarcacion=c("CAN", "ESAL", "LEBA", "NOR", "SUD")


for (i in 1:5) {

  Paginas=excel_sheets(paste("BD_primeros registros_",demarcacion,".xlsx",sep="")[i])
  
  Excel=read_excel(paste("BD_primeros registros_",demarcacion,".xlsx",sep="")[i], sheet = Paginas[1])
  
  Excel=data.frame(Excel)
  
  Prueba=colnames(Excel)[which(is.na(Excel[1,]))]
  
  Excel[1,which(is.na(Excel[1,]))]=Prueba
  
  Prueba=Excel[1,which(is.na(Excel[2,]))]
  
  Excel[2,which(is.na(Excel[2,]))]=Prueba
  
  colnames(Excel)=Excel[2,]
  Excel2=Excel[-c(1,2),]
  
  Excel2=data.frame(Excel2, row.names = c(1:length(Excel2[,1])))
  # "Scientific.name" "Latitude (N)" "Longitude (E)"
  
  temp=data.frame(Excel2[,c("Scientific.name", "Latitude..N.", "Longitude..E.", "Year", "Reference", "Other.relevant.references")])
  
  temp$Archivo=rep(paste("BD_primeros registros_",demarcacion,".xlsx",sep="")[i], length(temp[,1]))
  temp$Demarcacion=rep(demarcacion[i], length(temp[,1]))
  temp$EAI=rep("EAI5", length(temp[,1]))
  temp$Coord_Originales=paste(temp$Longitude..E.,temp$Latitude..N.,sep="_")
  BD_Primeros=rbind(BD_Primeros,temp)
  
    
}

Abundancia=rep(NA, length(BD_Primeros$Scientific.name))

BD_Primeros=cbind(BD_Primeros,Abundancia)

colnames(BD_Primeros)=c("Specie","Latitud","Longitud","Year_First_record","First_Reference","Other_relevant_reference","Archivo","Demarcacion","EAI","Coord_Originales","Abundancia")

BD_primeros_registros=BD_Primeros

BD_Primeros$Specie=trimws(BD_Primeros$Specie, "both", whitespace = "[ \\h\\v]") #elimina los espacios finales de todas las especies


# "BD_registros completos_todas demarcaciones.xlsx" ####
# EAI5

Paginas=excel_sheets("BD_registros completos_todas demarcaciones.xlsx")

Excel=read_excel("BD_registros completos_todas demarcaciones.xlsx", sheet = Paginas[1])

Excel=data.frame(Excel)

Prueba=colnames(Excel)[which(is.na(Excel[1,]))]

Excel[1,which(is.na(Excel[1,]))]=Prueba

Prueba=Excel[1,which(is.na(Excel[2,]))]

Excel[2,which(is.na(Excel[2,]))]=Prueba

colnames(Excel)=Excel[2,]

Excel2=Excel[-c(1,2),]

Excel2=data.frame(Excel2, row.names = c(1:length(Excel2[,1])))

# 11, 23, 24, 26, 27 Columnas

temp=data.frame(Excel2[,c(11, 23, 24, 26, 27)])

# Juntar coordenadas en dos unicas columnas combinando en las que hay y no hay
temp$Lat.Y.1[which(is.na(temp$Lat.Y.1))]=temp$Lat.Y[which(is.na(temp$Lat.Y.1))]
temp$Long.X.1[which(is.na(temp$Long.X.1))]=temp$Long.X[which(is.na(temp$Long.X.1))]

Excel2$Archivo=rep("BD_registros completos_todas demarcaciones.xlsx", length(Excel2[,1]))

# Transformar Fechas numericas en fechas normales
a=which(as.numeric(Excel2$Fecha.de.regist..Exacta)>10000)

if (length(a)!=0) {
  Excel2$Fecha.de.regist..Exacta.[a]=as.character(as.Date(as.numeric(Excel2$Fecha.de.regist..Exacta.[a])
                                                          ,origin = "1899-12-30"))  
}


# Convertir lugares en coordenadas
temp0=length(Excel2$Zona)
temp01=rep(0,temp0)
Todo=data.frame(Zona=temp01,Lat.Y=temp01,Lat.Y.1=temp01,Long.X=temp01,Long.X.1=temp01)
temp1=which(!is.na(Excel2$Zona))
temp2=which(!is.na(Excel2$Lat.Y))
temp3=which(!is.na(Excel2$Lat.Y.1))
temp4=which(!is.na(Excel2$Long.X))
temp5=which(!is.na(Excel2$Long.X.1))

# Sustituto las lineas que tienen datos en el Excel2 por un 1
Todo$ID=seq(1:temp0)
Todo$Zona[temp1]=1
Todo$Lat.Y[temp2]=1
Todo$Lat.Y.1[temp3]=1
Todo$Long.X[temp4]=1
Todo$Long.X.1[temp5]=1

oca=which(Todo$Zona==0) # para saber que lineas tienen NA's
Todo=Todo[-oca,]# elimino las lineas que contenian NA's

Todo$Suma=rowSums(Todo[,c(2,3,4,5)]) # sumo las lineas para saber cuales tenian datos de coordenadas en el Excel 2
Todo=Todo[-which(Todo$Suma>=1),] # para eliminar las lineas que si que tiene coordenadas reales y quedarme solo con las que tiene un sitio escrito en letras

# Añadir a la zona el nombre de España para centrar mas la busqueda
tempXX=paste(Excel2$Zona[Todo$ID],
             #Excel2$Localidades.muestreadas[Todo$ID],
             ", España",sep = "")

# Limpiar de caracteres extraños
tempXX=str_replace(tempXX, "\\s*\\([^\\)]+\\)?", "")
tempXX=str_replace_all(tempXX, "[:digit:]", "")
tempXX=str_replace_all(tempXX, "\\)", "")
tempXX=str_replace_all(tempXX, "\\(", "")
tempXX=stri_replace_all_charclass(tempXX, "\\p{WHITE_SPACE}", " ")
tempXX=trimws(tempXX, "both", whitespace = "[ \\h\\v]")
#View(data.frame(tempXX))

#tempXX=Excel2$Zona[Todo$ID] # Sin añadir España

# Coordenadas a partir de Nombres ####

# Modificar si quiero el registro de coordenadas de arcgis
# arcgis=geo(tempXX, method = "arcgis", 
           #api_options = list(iq_region = "eu"), full_results = TRUE)
arcgis=NA
##########################################################


# iq=NULL
# for (i in 1:length(tempXX)) {
#   iq_temp=geo(tempXX[i], method = "iq", 
#               api_options = list(iq_region = "eu"), full_results = TRUE)
#   iq=rbind(iq,iq_temp)
#   Sys.sleep(5)
# }
# Geocoding con IQ, no es muy bueno.
# iq=geo(tempXX, method = "iq",
#        api_options = list(iq_region = "eu"), full_results = TRUE)
# 
# 
# iq_2=geo(iq$address[which(is.na(iq$lat))], method = "iq",
#        api_options = list(iq_region = "eu"), full_results = TRUE)
# 
# iq[which(is.na(iq$lat)),]=iq_2



#plot(bing$long,bing$lat)
plot(arcgis$long,arcgis$lat)
#plot(iq$long,iq$lat)

#bing=data.frame(ID=Todo$ID,bing)
arcgis=data.frame(ID=Todo$ID,arcgis)


#mapview(arcgis[!is.na(arcgis$lat),], xcol = "long", ycol = "lat", crs = 4326, grid = F)
#mapview(temp[!is.na(temp$Lat.Y.1),], xcol = "Long.X.1", ycol = "Lat.Y.1", crs = 4326, grid = F)

# Modificar si quiero el registro de coordenadas de arcgis
temp$Lat.Y.1[arcgis$ID]=arcgis$arcgis
temp$Long.X.1[arcgis$ID]=arcgis$arcgis

# temp$Lat.Y.1[arcgis$ID]=arcgis$lat
# temp$Long.X.1[arcgis$ID]=arcgis$long
##########################################################


Specie=Excel2$nombre.aceptado
Latitud=temp$Lat.Y.1
Longitud=temp$Long.X.1
Year_First_record=paste(as.character(Excel2$Fecha.Asociada..Cita..años.que.cubre.la.publi.),as.character(Excel2$Fecha.de.regist..Exacta.),sep="_") # Fecha asociada_Fecha exacta
First_Reference=paste(Excel2$Artículo,Excel2$Ref.Manag.,Excel2$Ref..carpeta,sep="_") # Primer numero es Ref. Manager, segundo numero Ref. carpeta
Other_relevant_reference=rep(NA,length(Specie))
Archivo=rep("BD_registros completos_todas demarcaciones.xlsx", length(Specie))
EAI=rep("EAI5", length(Specie))

temp=Excel2[,c("Sudatlántico", "Norantlántico", "Canarias", "Levante.Balear", "Estrecho.Alborán")]

for (i in 1:dim(temp)[2]) {
  temp[,i]=as.numeric(temp[,i])
}

tempDemarca=c("SUD","NOR", "CAN", "LEBA", "ESAL")

for (i in 1:length(tempDemarca)) {
  temp[which(temp[,i]==1),i]=tempDemarca[i]
}

Demarcacion=unite(temp, Demarcacion ,c(1:5),  sep = "-", remove = TRUE)

Abundancia2=unite(Excel2, Abundancia ,c(94,95,105),  sep = "/", remove = TRUE)
Abundancia=Abundancia2$Abundancia
Coord_Originales=paste(Excel2$Localidades.muestreadas,Excel2$Zona,Excel2$Long.X,Excel2$Lat.Y,Excel2$Long.X.1,Excel2$Lat.Y.1,sep="_")

tempBD=cbind(Specie,
             Latitud,
             Longitud,
             Year_First_record,
             First_Reference,
             Other_relevant_reference,
             Archivo,
             Demarcacion,
             EAI,
             Coord_Originales,
             Abundancia
             )

UTM_Mod=which(as.numeric(tempBD$Latitud)>100)

#25830 ETRS89 // 32630 WGS84 UTM +proj=utm +zone=31 +datum=WGS84 +units=m +no_defs  // 4326 WGS84
#WGS84+GRS80 / Mercator "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#ED50 / UTM zone 30N "+proj=utm +zone=30 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
#ETRS89-extended / LAEA Europe "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#ETRS89 / UTM zone 31N "+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# capaWGS84_UTM_31N <- spTransform(demarcaciones, CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"))
# capaWGS84_UTM_30N <- spTransform(demarcaciones, CRS("+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs"))

# plot(capaETRS89_UTM_31N) ### ### Puntos en Cataluña
# points(as.numeric(tempBD$Longitud[UTM_Mod]),as.numeric(tempBD$Latitud[UTM_Mod]))
# 
# plot(capaETRS89_UTM_30N) ### ### Puntos en Cataluña
# points(as.numeric(tempBD$Longitud[UTM_Mod]),as.numeric(tempBD$Latitud[UTM_Mod]))

for (i in 1:length(UTM_Mod)) {
  if (as.numeric(tempBD$Latitud[UTM_Mod[i]])>4295601) {
    p1 = st_point(c(as.numeric(tempBD$Longitud[UTM_Mod[i]]),as.numeric(tempBD$Latitud[UTM_Mod[i]])))
    sfc = st_sfc(p1, crs = "+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    sfct = st_transform(sfc, 4326)
    tempBD$Latitud[UTM_Mod[i]]=as.character(sfct[[1]][2])
    tempBD$Longitud[UTM_Mod[i]]=as.character(sfct[[1]][1])
  } else {
  p1 = st_point(c(as.numeric(tempBD$Longitud[UTM_Mod[i]]),as.numeric(tempBD$Latitud[UTM_Mod[i]])))
  sfc = st_sfc(p1, crs = "+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  sfct = st_transform(sfc, 4326)
  tempBD$Latitud[UTM_Mod[i]]=as.character(sfct[[1]][2])
  tempBD$Longitud[UTM_Mod[i]]=as.character(sfct[[1]][1])
  }
}

BD_registros_completos_todas_demarcaciones=tempBD
#View(BD_registros_completos_todas_demarcaciones)

#tempBD$Latitud[7]="41.131" # Comprobar si es as?
#BD_registros_completos_todas_demarcaciones$Latitud[7]="41.131"
plot(BD_registros_completos_todas_demarcaciones$Longitud,
     BD_registros_completos_todas_demarcaciones$Latitud)

tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)
head(BD_Primeros)


# "Coincidencias_INFRAROK1120_Lista_Especies_Robert_Comas.xlsx" #### 
# EAI4

Paginas=excel_sheets("Coincidencias_INFRAROK1120_Lista_Especies_Robert_Comas.xlsx")

temp=NULL
#for (i in 1:3) {
  #Excel=data.frame(read_excel(Archivos_excel[7], sheet = Paginas[i]))
   Excel_Algas=data.frame(read_excel("Coincidencias_INFRAROK1120_Lista_Especies_Robert_Comas.xlsx", sheet = Paginas[1]))
   Excel_Invert=data.frame(read_excel("Coincidencias_INFRAROK1120_Lista_Especies_Robert_Comas.xlsx", sheet = Paginas[2]))
   Excel_Peces=data.frame(read_excel("Coincidencias_INFRAROK1120_Lista_Especies_Robert_Comas.xlsx", sheet = Paginas[3]))
  
  #Excel=data.frame(Excel)[,c("Genero_especie","Latitud","Longitud")]
  
  #temp=rbind(temp,Excel)
#}
  
# Intento de extraer la densidad de cada lugar (est? sin terminar)
# Peces=unique(Excel_Peces$Genero_especie) 
# Sitios=unique(Excel_Peces$ID_DIVE)
# 
# for (i in 1:length(Sitios)) {
#   for (j in 1:length(Peces)) {
#     temp=which(Excel_Peces$ID_DIVE==Sitios[1]&Excel_Peces$Genero_especie==Peces[1])
#     temp2=c(Excel_Peces)
#   }
# }

   temp_Excel_Algas=data.frame(Excel_Algas[,c("Genero_especie","Latitud","Longitud","FECHA","Presencia.0.50","CAMPAÑA")])
   temp_Excel_Invert=data.frame(Excel_Invert[,c("Genero_especie","Latitud","Longitud","FECHA","N","CAMPAÑA")])
   temp_Excel_Peces=data.frame(Excel_Peces[,c("Genero_especie","Latitud","Longitud","FECHA","N","CAMPAÑA")])

   colnames(temp_Excel_Algas)=c("Genero_especie","Latitud","Longitud","FECHA","N","CAMPAÑA")

temp=rbind(temp_Excel_Algas,temp_Excel_Invert, temp_Excel_Peces)
class(temp)

Archivo=rep("Coincidencias_INFRAROK1120_Lista_Especies_Robert_Comas.xlsx",length(temp$Genero_especie))
Year_First_record=as.character(temp$FECHA)
First_Reference=rep(NA,length(temp$Genero_especie))
Other_relevant_reference=temp$CAMPAÑA
Demarcacion=rep("LEBA",length(temp$Genero_especie))
Abundancia=temp$N
EAI=rep("EAI4", length(temp$Genero_especie))
Coord_Originales=paste(temp$Longitud,temp$Latitud,sep="_")

tempBD=cbind(Specie=temp$Genero_especie,
             Latitud=as.character(temp$Latitud),
             Longitud=as.character(temp$Longitud),
             Year_First_record,
             First_Reference,
             Other_relevant_reference,
             Archivo,
             Demarcacion,
             EAI,
             Coord_Originales,
             Abundancia)

Coincidencias_INFRAROK1120_Lista_Especies_Robert_Comas=data.frame(tempBD)
#plot(Coincidencias_INFRAROK1120_Lista_Especies_Robert_Comas$Longitud,
#     Coincidencias_INFRAROK1120_Lista_Especies_Robert_Comas$Latitud)
tempBD=data.frame(tempBD)
tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)
head(BD_Primeros)


# "Copia de FD_20N17_EAI_DM_20LEBA-CATALU_D1A.xlsx" ####
# EAI4

Paginas=excel_sheets("Copia de FD_20N17_EAI_DM_20LEBA-CATALU_D1A.xlsx")

Excel=read_excel("Copia de FD_20N17_EAI_DM_20LEBA-CATALU_D1A.xlsx", sheet = Paginas[3])

Excel=data.frame(Excel, row.names = c(1:length(Excel$`DM Marina`)))

Excel[2,which(is.na(Excel[2,]))]=colnames(Excel)[which(is.na(Excel[2,]))]

colnames(Excel)=Excel[2,]

Excel=Excel[-c(1,2),]
names(Excel)

unique(Excel$DM.Marina)

Excel$DM.Marina="LEBA"
Archivo=rep("Copia de FD_20N17_EAI_DM_20LEBA-CATALU_D1A.xlsx", length(Excel$Especie))

First_Reference=rep(NA,length(Excel$Especie))
Other_relevant_reference=rep(NA,length(Excel$Especie))
Abundancia=rep(NA,length(Excel$Especie))
EAI=rep("EAI4", length(Excel$Especie))
Coord_Originales=paste(Excel$Longitud,Excel$Latitud,sep="_")

temp1=as.Date(as.numeric(Excel$Fecha), origin = "1899-12-30")

temp1[which(is.na(temp1))]=Excel$Fecha[which(is.na(temp1))]

temp3=which(temp1<as.Date("1-1-1906", format="%d-%m-%Y"))

temp1=as.character(temp1)

temp1[temp3]="2013"

#25830 ETRS89 // 32630 WGS84 UTM +proj=utm +zone=31 +datum=WGS84 +units=m +no_defs  // 4326 WGS84
#WGS84+GRS80 / Mercator "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#ED50 / UTM zone 30N "+proj=utm +zone=30 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
#ETRS89-extended / LAEA Europe "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#ETRS89 / UTM zone 30N "+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

for (i in 1:length(Excel$Especie)) {
  if (is.na(Excel$Latitud[i])) {
    Excel$Latitud[i]=NA
    Excel$Longitud[i]=NA
    next
  }
  p1 = st_point(c(as.numeric(Excel$Latitud[i]),as.numeric(Excel$Longitud[i])))
  sfc = st_sfc(p1, crs = "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")
  sfct = st_transform(sfc, 4326)
  Excel$Latitud[i]=sfct[[1]][2]
  Excel$Longitud[i]=sfct[[1]][1]
}

tempBD=cbind(Specie=Excel$Especie,
             Latitud=as.character(Excel$Latitud), # esta mal en el EXCEL original
             Longitud=as.character(Excel$Longitud), # esta mal en el EXCEL original
             Year_First_record=temp1,
             First_Reference,
             Other_relevant_reference=Excel$...23,
             Archivo,
             Demarcacion=Excel$DM.Marina,
             EAI,
             Coord_Originales,
             Abundancia)

Copia_FD_20N17_EAI_DM_20LEBA_CATALU_D1A=data.frame(tempBD)

# plot(Copia_FD_20N17_EAI_DM_20LEBA_CATALU_D1A$Longitud,Copia_FD_20N17_EAI_DM_20LEBA_CATALU_D1A$Latitud)

tempBD=data.frame(tempBD)
tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)
head(BD_Primeros)


# "Datos invasoras 2008-2020_EIJF.xlsx" #### 
# EAI3

Paginas=excel_sheets("Datos invasoras 2008-2020_EIJF.xlsx")

Estaciones=data.frame(read_excel("Datos invasoras 2008-2020_EIJF.xlsx", sheet = Paginas[8]))

Estaciones$Nom[which(Estaciones$Nom=="Na Ginalet")]="Na Guinalet"
Estaciones$Nom[which(Estaciones$Nom=="Roca de sa Sals")]="Roca de Sa Sal"
Estaciones$Nom[which(Estaciones$Nom=="Far (Aire)")]="Far Illa de l'Aire"
Estaciones$Nom[which(Estaciones$Nom=="Cala Blanes")]="Cala'n Blanes"
Estaciones$Nom[which(Estaciones$Nom=="Cavalleria")]="Cap Cavalleria"

temp=NULL
for (i in 1:7) {
  Excel=data.frame(read_excel("Datos invasoras 2008-2020_EIJF.xlsx", sheet = Paginas[i]))
  
  temp=rbind(temp,Excel)
}

temp$Latitud=rep(NA,length(temp[,1]))
temp$Longitud=rep(NA,length(temp[,1]))
temp$Coord_Originales=rep(NA,length(temp[,1]))

for (i in 1:length(Estaciones[,1])) {
  temp$Latitud[which(temp$location==Estaciones[i,2])]=Estaciones[i,3]
  temp$Longitud[which(temp$location==Estaciones[i,2])]=Estaciones[i,4]
  temp$Coord_Originales[which(temp$location==Estaciones[i,2])]=paste(Estaciones[i,2],Estaciones[i,4],Estaciones[i,3],sep="_")
}

tempBD=cbind(Specie=temp$species,
             Latitud=as.character(temp$Latitud),
             Longitud=as.character(temp$Longitud),
             Year_First_record=as.character(temp$date),
             First_Reference=rep(NA,length(temp$date)),
             Other_relevant_reference=rep(NA,length(temp$date)),
             Archivo=rep("Datos invasoras 2008-2020_EIJF.xlsx", length(temp$date)),
             Demarcacion=rep("LEBA", length(temp$date)),
             EAI=rep("EAI3", length(temp$date)),
             Coord_Originales=temp$Coord_Originales,
             Abundancia=temp$Mean.abundance....)

Datos_invasoras_2008_2020_EIJF=data.frame(tempBD)
# plot(Datos_invasoras_2008_2020_EIJF$Longitud, Datos_invasoras_2008_2020_EIJF$Latitud)

tempBD=data.frame(tempBD)
tempBD=tempBD[-which(is.na(tempBD$Specie)),]
tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)
head(BD_Primeros)


# "Datos_NIS_LIC_MENORCA_0721.xlsx" ####
# EAI1

Paginas=excel_sheets("Datos_NIS_LIC_MENORCA_0721.xlsx")

Coordenadas=data.frame(read_excel("Datos_NIS_LIC_MENORCA_0721.xlsx", sheet = Paginas[6]))
Sitios=data.frame(read_excel("Datos_NIS_LIC_MENORCA_0721.xlsx", sheet = Paginas[7]))
Sitios=Sitios[19:30,1:2]
colnames(Sitios)=Sitios[1,1:2]
Sitios=Sitios[-1,]
Sitios$ID_ZONA=as.numeric(Sitios$ID_ZONA)

temp=NULL
for (i in 1:3) {
  Estaciones=data.frame(read_excel("Datos_NIS_LIC_MENORCA_0721.xlsx", sheet = Paginas[i]))[,c("ZONA","Genero_especie")]
  temp=rbind(temp, Estaciones)
}

temp$Latitud=rep(NA,length(temp[,1]))
temp$Longitud=rep(NA,length(temp[,1]))
temp$date=rep(NA,length(temp[,1]))
temp$Coord_Originales=rep(NA,length(temp[,1]))

for (i in 1:length(Sitios[,1])) {
  temp[which(temp$ZONA==Sitios[i,1]),c(3,4,5)]=unique(Coordenadas[which(Sitios[i,2]==Coordenadas$Zona),c(7,8,3)])
  temp[which(temp$ZONA==Sitios[i,1]),6]=paste(unique(Coordenadas[which(Sitios[i,2]==Coordenadas$Zona),c(4,5,6,8,7)]),collapse = "_")
}

temp=temp[,-1]

temp$Archivo=rep("Datos_NIS_LIC_MENORCA_0721.xlsx", length(temp[,1]))


tempBD=cbind(Specie=temp$Genero_especie,
             Latitud=as.character(temp$Latitud),
             Longitud=as.character(temp$Longitud),
             Year_First_record=as.character(as.POSIXct(temp$date, origin = "1970-01-01", tz = "UTC")),
             First_Reference=rep(NA,length(temp$date)),
             Other_relevant_reference=rep(NA,length(temp$date)),
             Archivo=temp$Archivo,
             Demarcacion=rep("LEBA", length(temp$date)),
             EAI=rep("EAI1", length(temp$date)),
             Coord_Originales=temp$Coord_Originales,
             Abundancia=rep(NA,length(temp$date))
             )

Datos_NIS_LIC_MENORCA_0721=data.frame(tempBD)
# plot(Datos_NIS_LIC_MENORCA_0721$Longitud, Datos_NIS_LIC_MENORCA_0721$Latitud)

tempBD=data.frame(tempBD)
tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)
head(BD_Primeros)


# "Datos_NIS_RAS_0721.xlsx" ####
# EAI2

Paginas=excel_sheets("Datos_NIS_RAS_0721.xlsx")

Coordenadas=data.frame(read_excel("Datos_NIS_RAS_0721.xlsx", sheet = Paginas[3]))
Sitios=data.frame(read_excel("Datos_NIS_RAS_0721.xlsx", sheet = Paginas[2]))
Sitios=Sitios[31:65,1:2]
colnames(Sitios)=Sitios[1,1:2]
Sitios=Sitios[-1,]
Sitios$`ID_ZONA/ID_RAS`=as.numeric(Sitios$`ID_ZONA/ID_RAS`)

temp=data.frame(read_excel("Datos_NIS_RAS_0721.xlsx", sheet = Paginas[1]))

temp$Latitud=rep(NA,length(temp[,1]))
temp$Longitud=rep(NA,length(temp[,1]))
temp$Coord_Originales=rep(NA,length(temp[,1]))

for (i in 1:length(Sitios[,1])) {
  temp[which(temp$ZONA==Sitios[i,1]),c(13,14)]=unique(Coordenadas[which(Sitios[i,2]==Coordenadas$Zona),c(6,7)])
  temp[which(temp$ZONA==Sitios[i,1]),15]=paste(unique(Coordenadas[which(Sitios[i,2]==Coordenadas$Zona),c(3,4,5,7,6)]),collapse = "_")
}


temp$Archivo=rep("Datos_NIS_RAS_0721.xlsx", length(temp[,1]))

tempBD=cbind(
  Specie=temp$Genero_especie,
  Latitud=as.character(temp$Latitud),
  Longitud=as.character(temp$Longitud),
  Year_First_record=as.character(temp$FECHA),
  First_Reference=rep(NA,length(temp$CAMPAÑA)),
  Other_relevant_reference=rep(NA,length(temp$CAMPAÑA)),
  Archivo=temp$Archivo,
  Demarcacion=rep("LEBA", length(temp$CAMPAÑA)),
  EAI=rep("EAI2", length(temp$CAMPAÑA)),
  Coord_Originales=temp$Coord_Originales,
  Abundancia=temp$N
)

Datos_NIS_RAS_0721_copy=data.frame(tempBD)
# plot(Datos_NIS_RAS_0721_copy$Longitud,Datos_NIS_RAS_0721_copy$Latitud)

tempBD=data.frame(tempBD)
tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)
head(BD_Primeros)


# "Especies_Aloctonas_MEDITS_todo_RC_19042021.xlsx" ####
# EAI4

Paginas=excel_sheets("Especies_Aloctonas_MEDITS_todo_RC_19042021.xlsx")

temp=data.frame(read_excel("Especies_Aloctonas_MEDITS_todo_RC_19042021.xlsx", sheet = Paginas[1]))

temp$Archivo=rep("Especies_Aloctonas_MEDITS_todo_RC_19042021.xlsx", length(temp[,1]))

temp$EAI=rep("EAI4", length(temp[,1]))

tempBD=cbind(
  Specie=temp$ESPECIE,
  Latitud=as.character(temp$LATITUD_INI),
  Longitud=as.character(temp$LONGITUD_INI),
  Year_First_record=temp$FECHA,
  First_Reference=rep(NA,length(temp$CAMPAÑA)),
  Other_relevant_reference=temp$CAMPAÑA,
  Archivo=temp$Archivo,
  Demarcacion=temp$Demarcación,
  EAI=temp$EAI,
  Coord_Originales=paste(temp$LONGITUD_INI,temp$LATITUD_INI,sep="_"),
  Abundancia=temp$ABUNDANCIA
)

tempBD[which(is.na(tempBD[,4])),4]=temp$AÑO[which(is.na(tempBD[,4]))]

Especies_Aloctonas_MEDITS_todo_RC_19042021=data.frame(tempBD)
# plot(Especies_Aloctonas_MEDITS_todo_RC_19042021$Longitud, Especies_Aloctonas_MEDITS_todo_RC_19042021$Latitud)

tempBD=data.frame(tempBD)
tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)
head(BD_Primeros)


# "Especies_Exotiques_Invasores_GVA_092021.xlsx" ####
# EAI4

Paginas=excel_sheets("Especies_Exotiques_Invasores_GVA_092021.xlsx")

temp=data.frame(read_excel("Especies_Exotiques_Invasores_GVA_092021.xlsx", sheet = Paginas[2]))

Coord_Originales=paste(temp$UTM.1X1,temp$UTM.10X10,temp$Topònim,temp$Municipi,temp$Província,sep="_")

############# WARNING!!!!! Mucho tiempo de proceso
# capa <- readOGR(dsn = "D:/2021 - ALOCTONAS", layer = "UTM 1x1 PIBAL")
# capaWGS84 <- spTransform(capa, CRS("+init=epsg:4326"))
#writeOGR(capaWGS84, ".", "capaWGS84", driver = "ESRI Shapefile") #also you were missing the driver argument
capaWGS84 <- readOGR(dsn = path, layer = "capaWGS84")

Coordenadas=NULL
for (i in 1:length(temp$UTM.1X1)) {
  if (is.na(temp$UTM.1X1[i])){
    temp2_Coordenadas=c(NA,NA)
    Coordenadas=rbind(Coordenadas,temp2_Coordenadas)
    next}
  temp_Coordenadas=capaWGS84@polygons[[which(capaWGS84$COD1X1==temp$UTM.1X1[i])]]@Polygons[[1]]@coords
  temp2_Coordenadas=c((max(temp_Coordenadas[,1])+min(temp_Coordenadas[,1]))/2,
                      (max(temp_Coordenadas[,2])+min(temp_Coordenadas[,2]))/2)
  Coordenadas=rbind(Coordenadas,temp2_Coordenadas)
}

temp$Archivo=rep("Especies_Exotiques_Invasores_GVA_092021.xlsx", length(temp[,1]))

tempBD=cbind(
  Specie=temp$Espècie,
  Latitud=as.character(Coordenadas[,2]),
  Longitud=as.character(Coordenadas[,1]),
  Year_First_record=temp$Any,
  First_Reference=NA,
  Other_relevant_reference=temp$Tipus.observació,
  Archivo=temp$Archivo,
  Demarcacion=rep("LEBA",length(temp$Espècie)),
  EAI=rep("EAI4", length(temp$Espècie)),
  Coord_Originales=Coord_Originales,
  Abundancia=rep(NA,length(temp$Espècie))
)

tempBD=data.frame(tempBD)
tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)
head(BD_Primeros)


# "Exoticas FD EEI D_ESAL, LEBA y SUD.xlsx" ####

# EAI3
#### Datum ETRS89

temp1=data.frame(read_excel("Exoticas FD EEI D_ESAL.xlsx", sheet = 1))
temp2=data.frame(read_excel("Exoticas FD EEI D_LEBA.xlsx", sheet = 1))
temp3=data.frame(read_excel("Exoticas FD EEI D_SUD.xlsx", sheet = 1))

colnames(temp1)[which(!is.na(temp1[2,]))]=temp1[2,which(!is.na(temp1[2,]))]
colnames(temp2)[which(!is.na(temp2[2,]))]=temp2[2,which(!is.na(temp2[2,]))]
colnames(temp3)[which(!is.na(temp3[2,]))]=temp3[2,which(!is.na(temp3[2,]))]

temp1=temp1[-c(1,2),]
temp2=temp2[-c(1,2),]
temp3=temp3[-c(1,2),]

temp1=data.frame(temp1, row.names = 1:length(temp1[,1]))
temp2=data.frame(temp2, row.names = 1:length(temp2[,1]))
temp3=data.frame(temp3, row.names = 1:length(temp3[,1]))

colnames(temp3)=colnames(temp1)

temp1$Archivo=rep("Exoticas FD EEI D_ESAL.xlsx", length(temp1[,1]))
temp2$Archivo=rep("Exoticas FD EEI D_LEBA.xlsx", length(temp2[,1]))
temp3$Archivo=rep("Exoticas FD EEI D_SUD.xlsx", length(temp3[,1]))

temp_final=rbind(temp1,temp2,temp3)
Coord_Originales=paste(temp_final$CC.AA.que.corresponde,temp_final$Coordenada.X,temp_final$Coordenada.Y,sep="_")

#head(temp_final)
  for (i in 1:length(temp_final[,1])) {
  temp_point=as.numeric(c(temp_final$Coordenada.X[i],temp_final$Coordenada.Y[i]))
  if (sum(is.na(temp_point))>=1) {
    temp_final$Coordenada.X[i]=NA
    temp_final$Coordenada.Y[i]=NA
    next
  }
  temp_point_2=st_point(temp_point)
  temp_point_3=st_sfc(temp_point_2, crs=25830 )
  temp_point_4=st_transform(temp_point_3, crs=4326)
  temp_final$Coordenada.X[i]=as.character(temp_point_4[[1]][1])
  temp_final$Coordenada.Y[i]=as.character(temp_point_4[[1]][2])
}
  
tempBD=cbind(
  Specie=temp_final$Especie.EI..ver.pestaña.7.,
  Latitud=temp_final$Coordenada.Y,
  Longitud=temp_final$Coordenada.X,
  Year_First_record=as.character(temp_final$Fecha.o.periodo),
  First_Reference=rep(NA,length(temp_final$Especie.EI..ver.pestaña.7.)),
  Other_relevant_reference=temp_final$Observaciones..comentarios,
  Archivo=temp_final$Archivo,
  Demarcacion=temp_final$DM.Marina,
  EAI=rep("EAI3", length(temp_final$Coordenada.X)),
  Coord_Originales=Coord_Originales,
  Abundancia=temp_final$Abundancia..número.de.individuos.
) 

#plot(temp_final$Coordenada.X,temp_final$Coordenada.Y)

tempBD=data.frame(tempBD)
tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)

#mapview(pruebas, xcol = "Longitud", ycol = "Latitud", crs = 4326, grid = F) 
#25830 ETRS89 - 32630 WGS84 UTM - 4326 WGS84 - 42310 WGS84+GRS80 / Mercator
#WGS84+GRS80 / Mercator ("+proj=merc +lat_ts=0 +lon_0=0 +k=1.000000 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs no_defs")
#mapview(pruebas2, xcol = "Longitud", ycol = "Latitud", crs = 4326, grid = F)



# "Marines al litoral català.xlsx" #### 
# EAI2

Paginas=excel_sheets("Marines al litoral català.xlsx")

temp=data.frame(read_excel("Marines al litoral català.xlsx", sheet = Paginas[2]))

#Le añado el 31T porque es el cuadrante que le toca. Las originales no lo llevan. 
temp$UTM.10...10.km[which(temp$UTM.10...10.km!="-")]=paste("31T",temp$UTM.10...10.km[which(temp$UTM.10...10.km!="-")],sep="")

temp$UTM.1...1.km[which(temp$UTM.1...1.km!="-")]=paste("31T",temp$UTM.1...1.km[which(temp$UTM.1...1.km!="-")],sep="")

temp$Archivo=rep("Marines al litoral català.xlsx", length(temp[,1]))

temp$Coord_Originales=paste(temp$UTM.10...10.km,temp$UTM.1...1.km,temp$UTM_X,temp$UTM_Y,sep="_")

############# WARNING!!!!! Mucho tiempo de proceso
# capa10 <- readOGR(dsn = "D:/2021 - ALOCTONAS", layer = "UTM 10x10 PIBAL")
# capaWGS84_10 <- spTransform(capa10, CRS("+init=epsg:4326")) # WGS84
#writeOGR(capaWGS84_10, ".", "capaWGS84_10", driver = "ESRI Shapefile")
capaWGS84_10 <- readOGR(dsn = path, layer = "capaWGS84_10")

#mapview(capaWGS84_10)

Coordenadas10=NULL
for (i in 1:length(temp$UTM.10...10.km)) {
  if (temp$UTM.10...10.km[i]=="-"){
    temp2_Coordenadas=c(NA,NA)
    Coordenadas10=rbind(Coordenadas10,temp2_Coordenadas)
    next}
  temp_Coordenadas=capaWGS84_10@polygons[[which(capaWGS84_10$CUADRICULA==temp$UTM.10...10.km[i])]]@Polygons[[1]]@coords
  temp2_Coordenadas=c((max(temp_Coordenadas[,1])+min(temp_Coordenadas[,1]))/2,
                      (max(temp_Coordenadas[,2])+min(temp_Coordenadas[,2]))/2)
  Coordenadas10=rbind(Coordenadas10,temp2_Coordenadas)
}
Coordenadas10=data.frame(Coordenadas10)

#mapview(Coordenadas10[which(!is.na(Coordenadas10$X1)),], xcol = "X1", ycol = "X2", crs = 4326, grid = F)


Coordenadas1=NULL
for (i in 1:length(temp$UTM.1...1.km)) {
  if (temp$UTM.1...1.km[i]=="-"){
    temp2_Coordenadas=c(NA,NA)
    Coordenadas1=rbind(Coordenadas1,temp2_Coordenadas)
    next}
  temp_Coordenadas=capaWGS84@polygons[[which(capaWGS84$COD1X1==temp$UTM.1...1.km[i])]]@Polygons[[1]]@coords
  temp2_Coordenadas=c((max(temp_Coordenadas[,1])+min(temp_Coordenadas[,1]))/2,
                      (max(temp_Coordenadas[,2])+min(temp_Coordenadas[,2]))/2)
  Coordenadas1=rbind(Coordenadas1,temp2_Coordenadas)
}
Coordenadas1=data.frame(Coordenadas1)

#mapview(Coordenadas1[which(!is.na(Coordenadas1$X1)),], xcol = "X1", ycol = "X2", crs = 4326, grid = F)

Coordenadas1[which(is.na(Coordenadas1$X1)),]=Coordenadas10[which(is.na(Coordenadas1$X1)),]

#mapview(Coordenadas1[which(!is.na(Coordenadas1$X1)),], xcol = "X1", ycol = "X2", crs = 4326, grid = F)


for (i in 1:length(temp[,1])) {
  temp_point=as.numeric(c(temp$UTM_X[i],temp$UTM_Y[i]))
  if (temp$UTM_X[i]=="-") {
    temp$UTM_X[i]=NA
    temp$UTM_Y[i]=NA
    next
  }
  temp_point_2=st_point(temp_point)
  temp_point_3=st_sfc(temp_point_2, crs="+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs" )
  temp_point_4=st_transform(temp_point_3, crs=4326)
  temp$UTM_X[i]=as.character(temp_point_4[[1]][1])
  temp$UTM_Y[i]=as.character(temp_point_4[[1]][2])
}



temp[which(is.na(temp$UTM_X)),c(7,8)]=Coordenadas1[which(!is.na(Coordenadas1$X1)),]

#plot(temp$UTM_X,temp$UTM_Y)

tempBD=cbind(
  Specie=temp$Espècie,
  Latitud=temp$UTM_Y,
  Longitud=temp$UTM_X,
  Year_First_record=temp$Any,
  First_Reference=paste(temp$Autor.s,temp$Font,temp$Referencia,sep=" / "),
  Other_relevant_reference=paste(temp$Estatus.a.Catalunya, temp$Localitat, sep= " / "),
  Archivo=temp$Archivo,
  Demarcacion=rep("LEBA",length(temp$Espècie)),
  EAI=rep("EAI2", length(temp$Espècie)),
  Coord_Originales=temp$Coord_Originales,
  Abundancia=rep(NA,length(temp$Espècie))
)

tempBD=data.frame(tempBD)
tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)


# Especies sueltas Bases de datos #### 

temp_path=paste(path,dir()[which(!str_detect(dir(),"\\."))],sep="\\") # Selecciona automaticamente la carpeta donde estan las tablas sueltas

Dir_Especies_tablas_datos=dir(temp_path) 

Archivos_excel=Dir_Especies_tablas_datos[which(str_detect(Dir_Especies_tablas_datos, ".xlsx"))] # Selecciona solo los archivos *.xlsx

Excel=read_excel(paste(temp_path, Archivos_excel[1], sep="\\"))

Excel=data.frame(Excel)

tempBD1=cbind(
  Specie=gsub("_"," ",Archivos_excel[1]),
  Latitud=Excel$Lat,
  Longitud=Excel$Long,
  Year_First_record=Excel$Fecha,
  First_Reference=NA,
  Other_relevant_reference=Excel$Referencia,
  Archivo=Archivos_excel[1],
  Demarcacion=NA,
  EAI=NA,
  Coord_Originales=paste(Excel$Nom,Excel$Long,Excel$Lat,sep="_"),
  Abundancia=NA
)
tempBD1=data.frame(tempBD1)

tempBD1$Year_First_record[32]=as.character(as.Date(as.numeric(tempBD1$Year_First_record[32]), origin = "1899-12-30"))

tempBD1$Year_First_record[32]=as.character(format(as.Date(tempBD1$Year_First_record[32], format="%Y-%m-%d"),"%d/%m/%Y")) 


Excel=read_excel(paste(temp_path,
                       Archivos_excel[2], sep="\\"))

Excel=data.frame(Excel)

tempBD2=cbind(
  Specie=gsub("_"," ",Archivos_excel[2]),
  Latitud=Excel$Lat,
  Longitud=Excel$Long,
  Year_First_record=NA,
  First_Reference=NA,
  Other_relevant_reference=Excel$Reference,
  Archivo=Archivos_excel[2],
  Demarcacion=NA,
  EAI=NA,
  Coord_Originales=paste(Excel$local,Excel$zona,Excel$Provincia,Excel$CCAA,Excel$Long,Excel$Lat,sep="_"),
  Abundancia=NA
)
tempBD2=data.frame(tempBD2)


Excel=read_excel(paste(temp_path,
                       Archivos_excel[3], sep="\\"))

Excel=data.frame(Excel)

tempBD3=cbind(
  Specie=gsub("_"," ",Archivos_excel[3]),
  Latitud=Excel$Lat,
  Longitud=Excel$Long,
  Year_First_record=Excel$Fecha,
  First_Reference=NA,
  Other_relevant_reference=Excel$Referencia,
  Archivo=Archivos_excel[3],
  Demarcacion=NA,
  EAI=NA,
  Coord_Originales=paste(Excel$Nom,Excel$Long,Excel$Lat,sep="_"),
  Abundancia=NA
)
tempBD3=data.frame(tempBD3)

temp=which(tempBD3$Year_First_record>40000)

tempBD3$Year_First_record[temp]=as.character(as.Date(as.numeric(tempBD3$Year_First_record[temp]), origin = "1899-12-30"))

tempBD3$Year_First_record[temp]=as.character(format(as.Date(tempBD3$Year_First_record[temp], format="%Y-%m-%d"),"%d/%m/%Y"))

Excel=read_excel(paste(temp_path,
                       Archivos_excel[4], sep="\\"))

Excel=data.frame(Excel)

tempBD4=cbind(
  Specie=gsub("_"," ",Archivos_excel[4]),
  Latitud=Excel$Lat,
  Longitud=Excel$Long,
  Year_First_record=Excel$Fecha,
  First_Reference=NA,
  Other_relevant_reference=Excel$Referencia,
  Archivo=Archivos_excel[4],
  Demarcacion=NA,
  EAI=NA,
  Coord_Originales=paste(Excel$Lugar,Excel$Long,Excel$Lat,sep="_"),
  Abundancia=NA
)
tempBD4=data.frame(tempBD4)

temp=which(!is.na(as.numeric(tempBD4$Year_First_record)))

tempBD4$Year_First_record[temp]=as.character(as.Date(as.numeric(tempBD4$Year_First_record[temp]), origin = "1899-12-30"))

tempBD4$Year_First_record[temp]=as.character(format(as.Date(tempBD4$Year_First_record[temp], format="%Y-%m-%d"),"%d/%m/%Y")) 

tempBD4$Year_First_record[c(31,32,33)]=gsub(pattern = "Ago-",replacement = "",tempBD4$Year_First_record[c(31,32,33)])

Excel=read_excel(paste(temp_path,
                       Archivos_excel[5], sep="\\"))

Excel=data.frame(Excel)

tempBD5=cbind(
  Specie=gsub("_"," ",Archivos_excel[5]),
  Latitud=Excel$Lat,
  Longitud=Excel$Long,
  Year_First_record=Excel$Fecha,
  First_Reference=NA,
  Other_relevant_reference=Excel$Referencia,
  Archivo=Archivos_excel[5],
  Demarcacion=NA,
  EAI=NA,
  Coord_Originales=paste(Excel$Lugar,Excel$Long,Excel$Lat,sep="_"),
  Abundancia=NA
)
tempBD5=data.frame(tempBD5)

temp=which(tempBD5$Year_First_record>30000)

tempBD5$Year_First_record[temp]=as.character(as.Date(as.numeric(tempBD5$Year_First_record[temp]), origin = "1899-12-30"))

tempBD5$Year_First_record[temp]=as.character(format(as.Date(tempBD5$Year_First_record[temp], format="%Y-%m-%d"),"%d/%m/%Y")) 


Excel=read_excel(paste(temp_path,
                       Archivos_excel[6], sep="\\"))

Excel=data.frame(Excel)

tempBD6=cbind(
  Specie=gsub("_"," ",Archivos_excel[6]),
  Latitud=Excel$Lat,
  Longitud=Excel$Long,
  Year_First_record=Excel$Fecha,
  First_Reference=NA,
  Other_relevant_reference=Excel$Referencia,
  Archivo=Archivos_excel[6],
  Demarcacion=NA,
  EAI=NA,
  Coord_Originales=paste(Excel$Nom,Excel$Long,Excel$Lat,sep="_"),
  Abundancia=NA
)
tempBD6=data.frame(tempBD6)


Excel=read_excel(paste(temp_path,
                       Archivos_excel[7], sep="\\"))

Excel=data.frame(Excel)

tempBD7=cbind(
  Specie=gsub("_"," ",Archivos_excel[7]),
  Latitud=Excel$Lat,
  Longitud=Excel$Long,
  Year_First_record=Excel$Fecha,
  First_Reference=NA,
  Other_relevant_reference=Excel$Referencia,
  Archivo=Archivos_excel[7],
  Demarcacion=NA,
  EAI=NA,
  Coord_Originales=paste(Excel$Lugar,Excel$Long,Excel$Lat,sep="_"),
  Abundancia=NA
)
tempBD7=data.frame(tempBD7)

temp=which(tempBD7$Year_First_record>30000)

tempBD7$Year_First_record[temp]=as.character(as.Date(as.numeric(tempBD7$Year_First_record[temp]), origin = "1899-12-30"))

tempBD7$Year_First_record[temp]=as.character(format(as.Date(tempBD7$Year_First_record[temp], format="%Y-%m-%d"),"%d/%m/%Y")) 


Excel=read_excel(paste(temp_path,
                       Archivos_excel[8], sep="\\"))

Excel=data.frame(Excel)

tempBD8=cbind(
  Specie=gsub("_"," ",Archivos_excel[8]),
  Latitud=Excel$Lat,
  Longitud=Excel$Long,
  Year_First_record=Excel$Fecha,
  First_Reference=NA,
  Other_relevant_reference=Excel$Referencia,
  Archivo=Archivos_excel[8],
  Demarcacion=NA,
  EAI=NA,
  Coord_Originales=paste(Excel$Lugar,Excel$Long,Excel$Lat,sep="_"),
  Abundancia=NA
)
tempBD8=data.frame(tempBD8)

temp=which(tempBD8$Year_First_record>39000)

tempBD8$Year_First_record[temp]=as.character(as.Date(as.numeric(tempBD8$Year_First_record[temp]), origin = "1899-12-30"))

tempBD8$Year_First_record[temp]=as.character(format(as.Date(tempBD8$Year_First_record[temp], format="%Y-%m-%d"),"%d/%m/%Y")) 


Excel=read_excel(paste(temp_path,
                       Archivos_excel[9], sep="\\"))

Excel=data.frame(Excel)

tempBD9=cbind(
  Specie=gsub("_"," ",Archivos_excel[9]),
  Latitud=Excel$Lat,
  Longitud=Excel$Long,
  Year_First_record=Excel$Fecha,
  First_Reference=NA,
  Other_relevant_reference=Excel$Referencia,
  Archivo=Archivos_excel[9],
  Demarcacion=NA,
  EAI=NA,
  Coord_Originales=paste(Excel$Lugar,Excel$Long,Excel$Lat,sep="_"),
  Abundancia=NA
)
tempBD9=data.frame(tempBD9)

temp=which(!is.na(as.numeric(tempBD9$Year_First_record))&as.numeric(tempBD9$Year_First_record)>2020)

tempBD9$Year_First_record[temp]=as.character(as.Date(as.numeric(tempBD9$Year_First_record[temp]), origin = "1899-12-30"))

tempBD9$Year_First_record[temp]=as.character(format(as.Date(tempBD9$Year_First_record[temp], format="%Y-%m-%d"),"%d/%m/%Y")) 

tempBD9$Year_First_record[c(55:59)]=gsub(pattern = "Ago-",replacement = "",tempBD9$Year_First_record[c(55:59)])

tempBD=rbind(tempBD1,tempBD2,tempBD3,tempBD4,tempBD5,tempBD6,tempBD7,tempBD8,tempBD9)

# Eliminar la extension .xlsx de los nombres de las especies
tempBD$Specie=gsub(".xlsx", "", tempBD$Specie)

# Añadir las demarcaciones en la tabla
plot(tempBD$Longitud,tempBD$Latitud) #plot para visualizarlas

tempBD$Demarcacion[which(tempBD$Longitud>0)]="LEBA"

tempBD$Demarcacion[which(tempBD$Longitud<0&tempBD$Latitud>41)]="NOR"
  
tempBD$Demarcacion[which(tempBD$Latitud<38)]="ESAL"

tempBD=data.frame(tempBD)
tempBD$Specie=trimws(tempBD$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,tempBD)


# Correccion de coordenadas sin formato fijo ####
# Buscar la posicion 748 que puede haber un error!!!!!!!!!!!!!

BD_Primeros[which(is.na(BD_Primeros$Latitud)&!is.na(BD_Primeros$Longitud)),c(2,3)] # coordenadas con solo datos en Longitud
BD_Primeros[which(!is.na(BD_Primeros$Latitud)&is.na(BD_Primeros$Longitud)),c(2,3)] # coordenadas con solo datos en Latitud

Lineas=which(is.na(BD_Primeros$Longitud)&!is.na(BD_Primeros$Latitud))
temp=BD_Primeros[Lineas,c(2,3)]

rio_Tea=c(41.8738614,-8.8604669)
BD_Primeros[Lineas[c(1,2)],2]=rio_Tea[1]
BD_Primeros[Lineas[c(1,2)],3]=rio_Tea[2]

Coordenadas1=NULL
for (i in 3:5) {
  coordenada_utm1x1=substr(strsplit(temp$Latitud[i], ": ")[[1]][2], 1, 9) # Separa y obtiene la coordenada utm1x1 eliminado el titulo y los dos ultimos digitos
  temp_Coordenadas=capaWGS84@polygons[[which(capaWGS84$COD1X1==coordenada_utm1x1)]]@Polygons[[1]]@coords # Obtiene del mapa transformado a WGS84 el cuadro que corresponde a mi coordenada
  temp2_Coordenadas=c((max(temp_Coordenadas[,2])+min(temp_Coordenadas[,2]))/2,
                      (max(temp_Coordenadas[,1])+min(temp_Coordenadas[,1]))/2) # Transforma los puntos del cuadrado en un punto medio para poder insertar en la tabla
  Coordenadas1=rbind(Coordenadas1,temp2_Coordenadas)
}
BD_Primeros[Lineas[-c(1,2)],c(2,3)]=Coordenadas1

# Buscar las posiciones que contienen un º, ' o '' tanto en Long como Lat
temp=which(str_detect(BD_Primeros$Latitud, "º")|
             str_detect(BD_Primeros$Latitud, "'")|
             str_detect(BD_Primeros$Latitud, "'")|
             str_detect(BD_Primeros$Longitud, "º")|
             str_detect(BD_Primeros$Longitud, "'")|
             str_detect(BD_Primeros$Longitud, "'"))

prueba_coordenadas=BD_Primeros$Latitud[temp]

prueba_coordenadas=data.frame(Longitud=BD_Primeros$Longitud[temp], 
                              Latitud=BD_Primeros$Latitud[temp], 
                              Posicion=temp)

prueba_coordenadas[which(is.na(prueba_coordenadas$Longitud)),]
prueba_coordenadas[which(is.na(prueba_coordenadas$Latitud)),]

a=stri_trans_general(gsub("\\s+", " ",prueba_coordenadas$Longitud),"Latin-ASCII")
a=gsub("[\ ]", "", a)
a=gsub("\"", "''", a)
a=gsub(",", ".", a)
a=gsub("'", "\'", a)
#a=gsub("?", "''", a)
f=stri_trans_general(stri_replace_all_charclass(a, "\\p{WHITE_SPACE}", " "),"Latin-ASCII")

Longitud=NULL
for (i in seq(length(f))) {
  x=strsplit(f[i], "[EWOºª\'\''\u92\u94()]")[[1]]
  x=str_replace_all(x, "[a-zA-Z]", "")
  x=stri_replace_all_charclass(x, "\\p{WHITE_SPACE}", "")
  
  if (length(which(x==""))>=1) {
    x=x[-which(x=="")]
  }
  
  # Evento especial, solo se da en 4 posiciones
  if (sum(i==c(5,8,124,215))>0) {
    suma=(as.numeric(x[1])+as.numeric(x[2]))/2 
    Longitud=rbind(Longitud,suma)
    next
  }
  
  if (length(which(str_detect(x, "/")|str_detect(x, "-")))>=1){
    temp=x[1:(which(str_detect(x, "/")|str_detect(x, "-")))-1]
    z=x[which(str_detect(x, "/")|str_detect(x, "-")):length(x)]
    z=gsub("/", "", z)
    z=gsub("-", "", z)
    temp=as.numeric(temp)
    z=as.numeric(z)
    if (length(temp)==3) {
      suma1=temp[1] + temp[2]/60 + temp[3]/3600 
    } else if (length(temp)==2) {
      suma1=temp[1] + temp[2]/60
    } else {
      suma1=temp[1] 
    }
    if (length(z)==3) {
      suma2=temp[1] + z[2]/60 + z[3]/3600 
    } else if (length(z)==2) {
      suma2=z[1] + z[2]/60
    } else {
      suma2=z[1] 
    }
    suma=(suma1+suma2)/2
    
    if (str_detect(f[i], "E")){
      suma=(-suma)
    }
    Longitud=rbind(Longitud,suma)
    next
  }
  
  y=as.numeric(x)
  
  if (length(y)>=3) {
    suma=y[1] + y[2]/60 + y[3]/3600 
  } else if (length(y)==2) {
    suma=y[1] + y[2]/60
  } else {
    suma=y[1] 
  }
  
  if (str_detect(f[i], "W")){
    suma=(-suma)
  }
  
  if (str_detect(f[i], "O")){
    suma=(-suma)
  }
  
  if (str_detect(f[i], "E")){
    suma=(-suma)
  }
  
  if (is.na(suma)) {
    suma=f[i]
    Longitud=rbind(Longitud,suma)
    next
  }
  Longitud=rbind(Longitud,suma)
}

# Latitud ###
a=stri_trans_general(gsub("\\s+", " ",prueba_coordenadas$Latitud),"Latin-ASCII")
a=gsub("[\ ]", "", a)
a=gsub("\"", "''", a)
a=gsub(",", ".", a)
a=gsub("'", "\'", a)
#a=gsub("", "''", a)
f=stri_trans_general(stri_replace_all_charclass(a, "\\p{WHITE_SPACE}", " "),"Latin-ASCII")

Latitud=NULL
for (i in 1:length(f)) {
  x=strsplit(f[i], "[Nºª\'\''\u92\u94]")[[1]]
  x=str_replace_all(x, "[a-zA-Z]", "")
  x=stri_replace_all_charclass(x, "\\p{WHITE_SPACE}", "")
  
  if (length(which(x==""))>=1) {
    x=x[-which(x=="")]
  }
  
  # Evento especial, solo se da en 4 posiciones
  if (sum(i==c(5,8,124,215))>0) {
    suma=39.961399
    Latitud=rbind(Latitud,suma)
    next
  }
  
  if (length(which(str_detect(x, "/")|str_detect(x, "-")))>=1){
    temp=x[1:(which(str_detect(x, "/")|str_detect(x, "-")))-1]
    z=x[which(str_detect(x, "/")|str_detect(x, "-")):length(x)]
    z=gsub("/", "", z)
    z=gsub("-", "", z)
    temp=as.numeric(temp)
    z=as.numeric(z)
    if (length(temp)==3) {
      suma1=temp[1] + temp[2]/60 + temp[3]/3600 
    } else if (length(temp)==2) {
      suma1=temp[1] + temp[2]/60
    } else {
      suma1=temp[1] 
    }
    if (length(z)==3) {
      suma2=temp[1] + z[2]/60 + z[3]/3600 
    } else if (length(z)==2) {
      suma2=z[1] + z[2]/60
    } else {
      suma2=z[1] 
    }
    #suma=(suma1+suma2)/2
    suma=suma1
    
    Latitud=rbind(Latitud,suma)
    next
  }
  
  y=as.numeric(x)
  
  if (length(y)>=3) {
    suma=y[1] + y[2]/60 + y[3]/3600 
  } else if (length(y)==2) {
    suma=y[1] + y[2]/60
  } else {
    suma=y[1] 
  }
  
  if (is.na(suma)) {
    suma=f[i]
    Latitud=rbind(Latitud,suma)
    next
  }
  Latitud=rbind(Latitud,suma)
}

BD_Primeros$Latitud[prueba_coordenadas$Posicion]=Latitud
BD_Primeros$Longitud[prueba_coordenadas$Posicion]=Longitud

BD_Primeros$Latitud=as.numeric(BD_Primeros$Latitud)
BD_Primeros$Longitud=as.numeric(BD_Primeros$Longitud)

plot(BD_Primeros$Longitud,BD_Primeros$Latitud)

temp=which(BD_Primeros$Latitud>38.56
           &BD_Primeros$Latitud<40
           &BD_Primeros$Longitud>-4.31
           &BD_Primeros$Longitud<(-0.6))
BD_Primeros$Longitud[temp]=-BD_Primeros$Longitud[temp]

temp=which(BD_Primeros$Latitud>39.85
           &BD_Primeros$Latitud<42.52
           &BD_Primeros$Longitud>-3.33
           &BD_Primeros$Longitud<(-0.4))
BD_Primeros$Longitud[temp]=-BD_Primeros$Longitud[temp]

temp=which(BD_Primeros$Latitud>36.34
           &BD_Primeros$Latitud<36.86
           &BD_Primeros$Longitud>2.54
           &BD_Primeros$Longitud<4.14)
BD_Primeros$Longitud[temp]=-BD_Primeros$Longitud[temp]

temp=which(BD_Primeros$Latitud>13
           &BD_Primeros$Latitud<15)
BD_Primeros$Latitud[temp]=41.131      

temp=which(BD_Primeros$Longitud>4.51)

BD_Primeros$Longitud[temp]=-BD_Primeros$Longitud[temp]

# Comprueba si hay erores en la demarcacion
# BD_Primeros[which(BD_Primeros$Latitud<41.5&str_detect(BD_Primeros$Demarcacion, "NOR")),] 
# BD_Primeros[which(BD_Primeros$Latitud>41.5&str_detect(BD_Primeros$Demarcacion, "ESAL")),]
# which(BD_Primeros$Latitud<41.5&str_detect(BD_Primeros$Demarcacion, "CAN"))

BD_Primeros$Longitud=as.numeric(BD_Primeros$Longitud)
BD_Primeros$Latitud=as.numeric(BD_Primeros$Latitud)

Coleccion_NA=BD_Primeros[which(is.na(BD_Primeros$Latitud)|is.na(BD_Primeros$Longitud)),]

#mapview(BD_Primeros[which(!is.na(BD_Primeros$Latitud)&!is.na(BD_Primeros$Longitud)),], xcol = "Longitud", ycol = "Latitud", crs = 4326, map.types = "CartoDB.Positron", grid = F)

# Prueba=BD_Primeros
# 
# Prueba$Latitud=as.numeric(Prueba$Latitud)
# Prueba$Longitud=as.numeric(Prueba$Longitud)
# # ############# ESTO ESTA MAL?!?!?!?!?!?!?!
# mapview(Prueba[which(#str_detect(Prueba$scientificname, Nombre)&
#   !is.na(Prueba$Longitud)
#   #&str_detect(Prueba$Year_First_record, "2018")
#   #&Prueba$Latitud<100 #Para eliminar los datos UTM
# ),-29],
# map.types = "CartoDB.Positron",
# xcol = "Longitud", ycol = "Latitud",
# zcol = "Specie", col.regions = "red",
# alpha=1, alpha.regions = 1,
# crs = 4326, grid = F)



# Plantilla Base de datos #####


Excel=read_excel("Plantilla_BD.xlsx")

Excel=data.frame(Excel)

plantilla=cbind(
  Specie=Excel$Specie,
  Latitud=Excel$Latitud,
  Longitud=Excel$Longitud,
  Year_First_record=Excel$Year,
  First_Reference=Excel$First_Reference,
  Other_relevant_reference=paste(Excel$Other_relevant_reference,paste("Mendeley_Tag:",Excel$Mendeley_Tag,sep=""),sep="; "),
  Archivo="Plantilla_BD.xlsx",
  Demarcacion=Excel$Demarcacion,
  EAI=NA,
  Coord_Originales=paste(Excel$Longitud,Excel$Latitud,Excel$Coordinates_estimated,sep="_"),
  Abundancia=Excel$Abundancia
)
plantilla=data.frame(plantilla)

plot(as.numeric(plantilla$Longitud),as.numeric(plantilla$Latitud))

plantilla$Specie=trimws(plantilla$Specie, "both", whitespace = "[ \\h\\v]")

BD_Primeros=rbind(BD_Primeros,plantilla)


# WORMS - Codificar los nombres bien de cada especie ####


BD_Primeros$Specie=stri_trans_general(stri_replace_all_charclass(BD_Primeros$Specie, "\\p{WHITE_SPACE}", " "),"Latin-ASCII") # elimina el espacio \\p{WHITE_SPACE} por el normal
BD_Primeros$Specie=trimws(BD_Primeros$Specie, "both", whitespace = "[ \\h\\v]")

Species_unique=stri_trans_general(stri_replace_all_charclass(as.character(BD_Primeros$Specie), "\\p{WHITE_SPACE}", " "),"Latin-ASCII") #elimina el espacio \\p{WHITE_SPACE} por el normal y transforma en ASCII (elimina acentos o letras raras)
Species_unique=sort(unique(Species_unique))

a=which(is.na(Species_unique))
if (length(a)>0) {
  Species_unique=Species_unique[-a]
}

Species_unique_data=data.frame(Species_unique,Species_unique)

if (sum(which(Species_unique_data[,2]=="Cladocarpus sigma folini")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Cladocarpus sigma folini")
} else {Species_unique_data[which(Species_unique_data[,2]=="Cladocarpus sigma folini"),2]="Cladocarpus sigma var. folini"} ########### Solucion basta porque la funcion de worms no encuentra el nombre

if (sum(which(Species_unique_data[,2]=="Cystoseira humilis myriophylloides")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Cystoseira humilis myriophylloides")
} else {Species_unique_data[which(Species_unique_data[,2]=="Cystoseira humilis myriophylloides"),2]="Cystoseira humilis var. myriophylloides"} # "cystoseira humilis myriophylloides"

if (sum(which(Species_unique_data[,2]=="Ectocarpus silicuosus hiemalis")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Ectocarpus silicuosus hiemalis")
} else {Species_unique_data[which(Species_unique_data[,2]=="Ectocarpus silicuosus hiemalis"),2]="Ectocarpus siliculosus var. hiemalis"}

if (sum(which(Species_unique_data[,2]=="Ectocarpus silicuosus pygmaeus")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Ectocarpus silicuosus pygmaeus")
} else {Species_unique_data[which(Species_unique_data[,2]=="Ectocarpus silicuosus pygmaeus"),2]="Ectocarpus siliculosus var. pygmaeus"}

if (sum(which(Species_unique_data[,2]=="Feldmannophycus okamurae")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Feldmannophycus okamurae")
} else {Species_unique_data[which(Species_unique_data[,2]=="Feldmannophycus okamurae"),2]="Caulacanthus okamurae"} #"Feldmannophycus okamurae" https://notulaealgarum.org/2020/documents/Notulae%20Algarum%20No.%20162.pdf

if (sum(which(Species_unique_data[,2]=="Hydroides dianthus complex")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Hydroides dianthus complex")
} else {Species_unique_data[which(Species_unique_data[,2]=="Hydroides dianthus complex"),2]="Hydroides dianthus"} #"Hydroides dianthus complex"

if (sum(which(Species_unique_data[,2]=="Nicidion cariboea ex Eunice cariboea")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Nicidion cariboea ex Eunice cariboea")
} else {Species_unique_data[which(Species_unique_data[,2]=="Nicidion cariboea ex Eunice cariboea"),2]="Nicidion cariboea"} #"Nicidion cariboea ex (no aceptado) Eunice cariboea"

if (sum(which(Species_unique_data[,2]=="Pennaria disticha australis")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Pennaria disticha australis")
} else {Species_unique_data[which(Species_unique_data[,2]=="Pennaria disticha australis"),2]="Pennaria disticha"} #"Pennaria disticha australis"

if (sum(which(Species_unique_data[,2]=="Scorpora notata")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Scorpora notata")
} else {Species_unique_data[which(Species_unique_data[,2]=="Scorpora notata"),2]="Scorpaena notata"} #"Scorpora notata"

if (sum(which(Species_unique_data[,2]=="Syllis \037ivipara")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Syllis \037ivipara")
} else {Species_unique_data[which(Species_unique_data[,2]=="Syllis \037ivipara"),2]="Syllis vivipara"} #""Syllis \037ivipara""

if (sum(which(Species_unique_data[,2]=="Wurdermannia magna")<1)>0) {
  print("Nombre no coincide")
  beep(9)
  stop("Wurdermannia magna")
} else {Species_unique_data[which(Species_unique_data[,2]=="Wurdermannia magna"),2]="Wurdemannia"} #"Wurdermannia magna"

temp=which(Species_unique_data[,2]=="Haplosporidium pinnae"
           |Species_unique_data[,2]=="Mona blanca"
           |Species_unique_data[,2]=="Myxobolus portucalensis")

Species_unique_data=Species_unique_data[-temp,]

temp=wormsbymatchnames(Species_unique_data[,2], marine_only = F)
beep(8)

# Bucle por lotes
# c=length(Species_unique_data$Species_unique)/32
# a=1:32
# Especies=NULL
# for (i in 1:c) {
#   if (any(is.na(Species_unique_data[a,2]))) {
#     stop("Existe algun NA")
#   }
#   temp=wormsbymatchnames(Species_unique_data[a,2] 
#                          ,marine_only = F)
#   Especies=rbind(Especies,temp)
#   print(round(i/length(Species_unique_data[,2])*100,2))
#   a=a+32
# }
# a=NULL

# ########### WORRMS #####################################
# Especies=Species_unique_data[,2]
# b=NULL
# for (i in 1:length(Especies)) {
#   a=wm_records_names(Especies[i],marine_only=F,fuzzy = T)
#   b=c(b,a)
#   print(round(i/length(Especies)*100,2)) 
# }


Species_unique_data=data.frame(Species_unique_data,temp)
head(Species_unique_data)

matriz=matrix(NA, ncol = length(colnames(Species_unique_data)), nrow = length(BD_Primeros$Specie))

colnames(matriz)=colnames(Species_unique_data)

BD_Primeros=data.frame(BD_Primeros,matriz)
head(BD_Primeros)

for (i in 1:length(Species_unique_data[,1])) {
  temp=which(BD_Primeros$Specie==Species_unique_data[i,1])
  BD_Primeros[temp,12:40]=Species_unique_data[i,]
}

BD_Primeros=BD_Primeros[,-c(12,13)]

names(BD_Primeros)

which(is.na(BD_Primeros$Specie))
a=which(is.na(BD_Primeros$scientificname))
which(is.na(BD_Primeros$valid_name))

BD_Primeros$scientificname[a]=BD_Primeros$Specie[which(is.na(BD_Primeros$scientificname))]

# "Specie_raw"               "Latitud"                  "Longitud"                 "Date"       
# "First_Reference"          "Other_relevant_reference" "Archivo"                  "Demarcacion"             
# "EAI"                      "Abundancia"               "Specie_BD"                "AphiaID"                 
# "url"                      "Scientific_name"          "authority"                "status"                  
# "unacceptreason"           "taxonRankID"              "rank"                     "valid_AphiaID"           
# "valid_name"               "valid_authority"          "parentNameUsageID"        "kingdom"                 
# "phylum"                   "class"                    "order"                    "family"                  
# "genus"                    "citation"                 "lsid"                     "Marine"                
# "Brackish"                 "Freshwater"               "Terrestrial"              "Extinct"               
# "match_type"               "modified"

# Homogeneizar todas las Demarcaciones ####
a=gsub("NA", "",BD_Primeros$Demarcacion)
b=gsub("0", "",a)
c=gsub("-", "",b)

c[which(c=="")]=NA

BD_Primeros$Demarcacion=c

temp1=unique(BD_Primeros$Demarcacion)

temp2=c("CAN",         "ESAL",        "LEBA",        "NOR",         "SUD",         NA,            "SUD-ESAL",    
        "LEBA-ESAL",    "NOR-LEBA",     "SUD-LEBA-ESAL", "SUD-NOR",       "SUD")
for (i in 1:length(temp1)) {
  BD_Primeros$Demarcacion[which(BD_Primeros$Demarcacion==temp1[i])]=temp2[i]
}
# Limpiar Fechas quitandole el .0
BD_Primeros$Year_First_record=gsub("\\.0", "",as.character(BD_Primeros$Year_First_record))

# # Arreglar algunos nombre ####
# temp=which(BD_Primeros$scientificname=="Pinctada radiata")
# if (length(temp)!=0) {
#   BD_Primeros$scientificname[temp]="Pinctada imbricata radiata"
# }

# Eliminar la Dorada (Sparus aurata) de las demarcaciones en las que no es Invasora
BD_Primeros=BD_Primeros[-which(BD_Primeros$scientificname=="Sparus aurata"&BD_Primeros$Demarcacion!="CAN"),]


# "Biodiversidad_Puertos.xlsx" ####

Excel=read_excel("Biodiversidad_Puertos.xlsx")

Excel=data.frame(Excel)

puertos=unique(Excel$Port.name)
coord_puertos=data.frame(matrix(NA,length(puertos),ncol = 3))
colnames(coord_puertos)=c("Puerto","Lat","Long")
coord_puertos$Puerto=puertos

# Puerto de Las Palmas,28.137676,-15.421732
# Puerto de Santa Cruz de Tenerife,28.482591,-16.234388
# Port de Palma,39.56199708738822, 2.6316127991318172
# C.N. Colònia de Sant Pere,39.737210,3.274337
# C.N. Can Picafort,39.765847,3.159551
# R.C.N. Port de Pollença, 39.904520,3.085949

lati=c(28.137676,28.482591,39.561997,39.737210,39.765847,39.904520)
long=c(15.421732,16.234388,2.6316127,3.274337,3.159551,3.085949)

for (i in 1:length(puertos)) {
  coord_puertos[which(coord_puertos$Puerto==puertos[i]),"Lat"]=lati[i]
  coord_puertos[which(coord_puertos$Puerto==puertos[i]),"Long"]=long[i]
}

Excel$Latitud=NA
Excel$Longitud=NA

for (i in 1:length(coord_puertos$Puerto)) {
  temp=which(Excel$Port.name==coord_puertos$Puerto[i])
  Excel$Latitud[temp]=coord_puertos$Lat[i]
  Excel$Longitud[temp]=coord_puertos$Long[i]
}


unlist(str_split(Excel$MS.Subdivision.MRU,pattern = "/"))
Excel$Demarcacion=data.frame(matrix(unlist(str_split(Excel$MS.Subdivision.MRU,pattern = "/")), nrow=length(str_split(Excel$MS.Subdivision.MRU,pattern = "/")), byrow=TRUE))[,3]

Todo_EAI=matrix(NA, nrow = length(Excel$Species), ncol = length(colnames(BD_Primeros)))
colnames(Todo_EAI)=colnames(BD_Primeros)
Todo_EAI=as.data.frame(Todo_EAI)

Todo_EAI$Year_First_record=Excel$Sampling.year
Todo_EAI$Specie=Excel$Species
Todo_EAI$Latitud=Excel$Latitud
Todo_EAI$Longitud=Excel$Longitud
Todo_EAI$scientificname=NA
Todo_EAI$Archivo="Biodiversidad_Puertos.xlsx"
Todo_EAI$Coord_Originales=Excel$Port.name
Todo_EAI$Other_relevant_reference=paste(Excel$Project, Excel$Assessor, Excel$Funding, Excel$Reference, sep = "_")
Todo_EAI$Demarcacion=Excel$Demarcacion
Todo_EAI$kingdom=NA
Todo_EAI$phylum=NA
Todo_EAI$class=NA
Todo_EAI$order=NA
Todo_EAI$family=NA
Todo_EAI$genus=NA
Todo_EAI$EAI="EAI5"

Todo_EAI$Specie=stri_replace_all_charclass(Todo_EAI$Specie, "\\p{WHITE_SPACE}", " ")
Todo_EAI$Specie=trimws(Todo_EAI$Specie, "both", whitespace = "[ \\h\\v]")
Todo_EAI$Specie=gsub(x = Todo_EAI$Specie, pattern = "cf. ", replacement = "")

Todo_EAI$Specie[which(Todo_EAI$Specie=="Patella candei crenata (d´Orbigny, 1838)")]="Patella candei"
Todo_EAI$Specie[which(Todo_EAI$Specie=="Lumbrineridae gen sp.")]="Lumbrineridae"
Todo_EAI$Specie[which(Todo_EAI$Specie=="Sepulidae gen sp.")]="Serpulidae"
Todo_EAI$Specie[which(Todo_EAI$Specie=="Spionidae gen sp.")]="Spionidae"

nombres=unique(Todo_EAI$Specie)
nombres=nombres[-which(nombres=="\"Leiochrides fauveli\"")]

temp=wormsbymatchnames(stri_trans_general(nombres,"Latin-ASCII") #elimina el espacio \\s+ por el normal y transforma en ASCII (elimina acentos o letras raras)
                       ,marine_only = F)


columnas=which(colnames(Todo_EAI)%in%colnames(temp))

temp=data.frame(nombres,temp)

for (i in 1:length(temp[,1])) {
  temp1=which(Todo_EAI$Specie==temp[i,1])
  Todo_EAI[temp1,columnas]=temp[i,-1]
}

a=which(is.na(Todo_EAI$scientificname))
Todo_EAI$Specie[a]="Leiochrides fauveli"

BD_Primeros=rbind(BD_Primeros, Todo_EAI)

# GBIF #####
# EAI5
temp=unique(BD_Primeros$Archivo)[1:5] # El nombre de las bases de datos de primeros registros

Filas=which(BD_Primeros$Archivo%in%temp) # Las filas que contienen datos de los primeros registros

# Selecciona todas las especies de primeros registros
#BD_EAI_primeros=sort(unique(BD_Primeros[Filas,"valid_name"])) # usar el nombre valido de la especie por WORMS
BD_EAI_primeros=sort(unique(BD_Primeros[Filas,"scientificname"])) 

Columnas=c("acceptedScientificName","scientificName","decimalLatitude","decimalLongitude","locality","stateProvince","collectionCode", # Columnas que quiero que tenga la tabla que voy a crear. Los nombres son de la base de datos del GBIF
           "eventDate","kingdom","phylum","class","order","family","genus","identifiedBy","institutionCode","depth","occurrenceRemarks")
# Nombre columnas
# acceptedScientificName, el nombre cientifico aceptado
# scientificName, nombre cientifico en la base de datos del GBIF

# Extraccion de datos de GBIF 
Todo=NULL

for (i in 1:length(BD_EAI_primeros)) {
  prueba=occ_search(scientificName=BD_EAI_primeros[i], country="ES", limit = 100000) #Utilizando el paquete del gbif me busca las especies que solo estan presentes en "ES" Espa?a
  #Usar limite de 100000 para que me recoja todos los datos. Por defecto son solo 500.
  prueba=data.frame(prueba$data)
  
  if (dim(prueba)[1]==0) {
    next  
  } else {
    temp=matrix(NA,nrow = length(prueba$key),ncol = length(Columnas)) # me crea una tabla vacia para completar con los datos que obtengo de cada especie
    temp=as.data.frame(temp)
    colnames(temp)=Columnas
    for (j in 1:length(Columnas)) {
      if (length(which(colnames(prueba)==Columnas[j]))==0) {
        next
      }
      temp[,j]=prueba[,which(colnames(prueba)==Columnas[j])]
    }
    
    temp$scientificname_BD=BD_EAI_primeros[i]
    Todo=rbind(Todo, temp)
    
    print(paste(round(i/length(BD_EAI_primeros)*100,2),"%", sep = " ")) # Medidor de progreso
  }
  
}
head(Todo)
# Busca los nombres que son BOLD y los modifica para poner la especie real
temp=which(str_detect(Todo$scientificName, "BOLD")|str_detect(Todo$acceptedScientificName, "BOLD"))
temp2=sort(unique(temp))
Todo$acceptedScientificName[temp2]=Todo$scientificname_BD[temp2]

# Elimino especies que por demarcacion no deberian de estar
Todo=Todo[-which(Todo$decimalLatitude>30&str_detect(Todo$scientificName,"Sparus aurata")),] # Elimino la dorada de la peninsula por no ser considerada aqui invasora
Todo=Todo[-which(Todo$decimalLatitude>30&str_detect(Todo$scientificName,"Dicentrarchus labrax")),] # Elimino la lubina de la peninsula por no ser considerada aqui invasora
Todo=Todo[-which(Todo$decimalLatitude>30&str_detect(Todo$scientificName,"Argyrosomus regius")),] # Elimino la corvina de la peninsula por no ser considerada aqui invasora


# Buscar que especies y coordenadas coinciden con la BD_EAI ####
# temp3=NULL
# for (i in 1:length(BD_EAI_primeros)) {
#   
# 
#   temp=which(str_detect(BD_EAI$scientificname,BD_EAI_primeros[i]))
#   
#   for (j in 1:length(temp)) {
#   
#     temp2=which(str_detect(Todo$scientificName,BD_EAI_primeros[i])&
#             Todo$decimalLatitude==as.numeric(BD_EAI[temp[j],"Latitud"])&
#             Todo$decimalLongitude==as.numeric(BD_EAI[temp[j],"Longitud"]))
#     
#     temp3=c(temp3,temp2)
#     print(paste(round(i/length(BD_EAI_primeros)*100,2),"% -", round(j/length(temp)*100,2), "%", sep = " ")) # Medidor de progreso
#   }
# }
# 
# sort(unique(temp3)) # Filas de la tabla Todo, que se repiten con la base de datos. No las elimino porque aportan informacion relevante
# Todo[temp3,]

# Corregir coordenadas que estan fuera de España ####
temp=which(Todo$decimalLatitude<20.07)

Todo$locality[temp][1]="Puerto de Lanzarote, Lanzarote, España"
Todo$locality[temp][5]="Puerto de Tragove, Cambados, ría de Arosa, España"

a=geo(Todo$locality[temp], method = "bing", api_options = list(iq_region = "eu"), full_results = TRUE)
a=data.frame(a)
a$lat=as.numeric(a$lat)
a$long=as.numeric(a$long)

Todo$decimalLatitude[temp]=a$lat
Todo$decimalLongitude[temp]=a$long

#Eliminar lineas que no contienen datos de coordenadas ni de sitio o localidad
temp=which(is.na(Todo$decimalLatitude)
           &is.na(Todo$decimalLongitude)
           &is.na(Todo$locality)
           &is.na(Todo$stateProvince))
if (length(temp)!=0) {
  Todo=Todo[-temp,]
}

# Mapa visual
# mapview(Todo[which(!is.na(Todo$decimalLatitude)),], xcol = "decimalLongitude", ycol = "decimalLatitude", map.types = "CartoDB.Positron",crs = 4326, grid = F) # mapa con todos los datos de todas las especies

# mapshot(mapview(Todo[which(!is.na(Todo$decimalLatitude)),], xcol = "decimalLongitude", ycol = "decimalLatitude", map.types = "CartoDB.Positron",crs = 4326, grid = F) # mapa con todos los datos de todas las especies
# , url = "Mapa_Especies_GBIF_13012022.html")

#Añadir las demarcaciones a la base de datos del GBIF
Todo$Demarcacion=NA
NOR=which(Todo$decimalLatitude>41.66471&Todo$decimalLongitude<(-1.57104))
Todo$Demarcacion[NOR]="NOR"

LEBA=which(Todo$decimalLatitude<42.759&Todo$decimalLongitude>(-2.192616))
Todo$Demarcacion[LEBA]="LEBA"

ESAL=which(Todo$decimalLatitude<37.5726&Todo$decimalLongitude>(-5.9163)&Todo$decimalLongitude<(-2.192616))
Todo$Demarcacion[ESAL]="ESAL"

SUD=which(Todo$decimalLatitude<37.286&Todo$decimalLongitude<(-5.9163)&Todo$decimalLongitude>(-7.543))
Todo$Demarcacion[SUD]="SUD"

CAN=which(Todo$decimalLatitude<32.38)
Todo$Demarcacion[CAN]="CAN"

# View(Todo[which(is.na(Todo$Demarcacion)),])

# Añadir a la BD_EAI la base de datos de GBIF ####
head(BD_Primeros)
head(Todo)

# Crear una matriz vacia del tamaño de los datos de Todo para añadir al BD_EAI
Todo_EAI=matrix(NA, nrow = length(Todo$acceptedScientificName), ncol = length(colnames(BD_Primeros)))
colnames(Todo_EAI)=colnames(BD_Primeros)
Todo_EAI=as.data.frame(Todo_EAI)

Todo_EAI$Year_First_record=Todo$eventDate
Todo_EAI$Specie=Todo$acceptedScientificName
Todo_EAI$Latitud=Todo$decimalLatitude
Todo_EAI$Longitud=Todo$decimalLongitude
Todo_EAI$scientificname=Todo$scientificname_BD
Todo_EAI$Archivo="Base de datos GBIF"
Todo_EAI$Coord_Originales=paste(Todo$decimalLatitude, Todo$decimalLongitude, Todo$locality, Todo$stateProvince, sep = "_")
Todo_EAI$Other_relevant_reference=paste(Todo$collectionCode, Todo$identifiedBy, Todo$institutionCode, Todo$occurrenceRemarks, sep = "_")
Todo_EAI$Demarcacion=Todo$Demarcacion
Todo_EAI$kingdom=Todo$kingdom
Todo_EAI$phylum=Todo$phylum
Todo_EAI$class=Todo$class
Todo_EAI$order=Todo$order
Todo_EAI$family=Todo$family
Todo_EAI$genus=Todo$genus
Todo_EAI$EAI="EAI5"

Todo_EAI$Specie[which(Todo_EAI$Specie=="Balanus amphitrite variegatus Darwin, 1854")]="Balanus amphitrite"
Todo_EAI$Specie[which(Todo_EAI$Specie=="Codium fragile subsp. atlanticum (Cotton) P.C.Silva")]="Codium fragile subsp. atlanticum"
Todo_EAI$Specie[which(Todo_EAI$Specie=="Ulva lactuca var. myriotrema (P.L.Crouan & H.M.Crouan) Bornet, 1843")]="Ulva lactuca var. myriotrema"

Todo_EAI$Specie=stri_replace_all_charclass(Todo_EAI$Specie, "\\p{WHITE_SPACE}", " ")
Todo_EAI$Specie=trimws(Todo_EAI$Specie, "both", whitespace = "[ \\h\\v]")
nombres=unique(Todo_EAI$Specie)

temp=wormsbymatchnames(stri_trans_general(nombres,"Latin-ASCII") #elimina el espacio \\s+ por el normal y transforma en ASCII (elimina acentos o letras raras)
                       ,marine_only = F)


columnas=which(colnames(Todo_EAI)%in%colnames(temp))

temp=data.frame(nombres,temp)

for (i in 1:length(temp[,1])) {
  temp1=which(Todo_EAI$Specie==temp[i,1])
  Todo_EAI[temp1,columnas]=temp[i,-1]
}

a=which(is.na(Todo_EAI$scientificname))
Todo_EAI$Specie[a]
if (length(a)!=0) {
  Todo_EAI$scientificname[a]="Synedropsis roundii"
}


BD_Primeros=rbind(BD_Primeros, Todo_EAI)

write.csv2(Todo_EAI,
           file = paste("BD_GBIF_",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".csv",sep=""),
           sep="\t",
           row.names = F,
           fileEncoding = "UTF-8")

# write_xlsx(Todo_EAI,
#            file = paste("BD_GBIF_",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".xlsx",sep=""))


# Observadores del mar #####

r <- read_html('https://www.observadoresdelmar.es/Map/GetMapMarkers?InitProjectId=')

if (exists("r")) {
    
data <- data.frame(jsonlite::fromJSON(html_text(r)))

data$ObservacioLatitud=round(as.numeric(data$ObservacioLatitud),4)
data$ObservacioLongitud=round(as.numeric(data$ObservacioLongitud),4)

temp=which(data$ObservacioLongitud<181&data$ObservacioLongitud>(-181)&
             data$ObservacioLatitud<91&data$ObservacioLatitud>(-91))
if (length(temp)!=0) {
  data=data[temp,]
}

temp=which(data$ObservacioLongitud<8&data$ObservacioLongitud>(-21)&
             data$ObservacioLatitud<46&data$ObservacioLatitud>24)
if (length(temp)!=0) {
  data=data[temp,]
}

temp=which(data$IdProject==9|data$IdProject==10|data$IdProject==17)
if (length(temp)!=0) {
  data=data[temp,]
}


plot(data$ObservacioLongitud,
     data$ObservacioLatitud)

data$Nombre=NA
data$Fecha=NA
data$Comentarios=NA
data$Depth=NA
for (i in 1:length(data$IdObervacio)) {
  temp=paste(paste("https://www.observadoresdelmar.es/Map/GetObservacioInfo?IDObserv=",data[i,1],"&IdProject=",data[i,2],sep=""))
  
  json_file=jsonlite::fromJSON(temp)
  
  if(!is.null(json_file$Especie)){
    data$Nombre[i]=json_file$Especie
  }
  if(!is.null(json_file$observacio_data)){
    data$Fecha[i]=json_file$observacio_data
  }
  if(!is.null(json_file$NombreProyecto)){
    data$ProjectClassification[i]=json_file$NombreProyecto
  }
  if(!is.null(json_file$ObservacioComentaris)){
    data$Depth[i]=data$Comentarios[i]=json_file$ObservacioComentaris
  }
  if(!is.null(json_file$ObservacioFondaria)){
    data$Depth[i]=json_file$ObservacioFondaria
  }
}

temp=which(data$Nombre=="")
if (length(temp)!=0) {
  data=data[-temp,]
}

Observadores_EAI=matrix(NA, nrow = length(data$Nombre), ncol = length(colnames(BD_Primeros)))
colnames(Observadores_EAI)=colnames(BD_Primeros)
Observadores_EAI=as.data.frame(Observadores_EAI)

Observadores_EAI$Year_First_record=data$Fecha
Observadores_EAI$Specie=data$Nombre
Observadores_EAI$Latitud=data$ObservacioLatitud
Observadores_EAI$Longitud=data$ObservacioLongitud
Observadores_EAI$scientificname=NA
Observadores_EAI$Archivo="Observadores del mar_WEB"
Observadores_EAI$Coord_Originales=paste(data$ObservacioLatitud, data$ObservacioLongitud, sep = "_")
Observadores_EAI$Other_relevant_reference=paste(data$IdObervacio, data$IdProject, data$ProjectClassification, data$Comentarios, sep = "_")
Observadores_EAI$Demarcacion=NA
Observadores_EAI$kingdom=NA
Observadores_EAI$phylum=NA
Observadores_EAI$class=NA
Observadores_EAI$order=NA
Observadores_EAI$family=NA
Observadores_EAI$genus=NA
Observadores_EAI$EAI="EAI5"

Observadores_EAI=Observadores_EAI[-which(Observadores_EAI$Specie=="NOT VALID"),]
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="Pinctada imbricaa radiata")]="Pinctada imbricata radiata"
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="Rugulopteryx Okamurae")]="Rugulopteryx okamurae"
Observadores_EAI=Observadores_EAI[-which(Observadores_EAI$Specie=="alga filamentosa"),]
Observadores_EAI=Observadores_EAI[-which(Observadores_EAI$Specie=="És una espècie filamentosa (grup ectocarpals)"),]
Observadores_EAI=Observadores_EAI[-which(Observadores_EAI$Specie=="Alga filamentosa"),]
Observadores_EAI=Observadores_EAI[-which(Observadores_EAI$Specie=="No identificada"),]
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="Baliste Carolinensis")]="Baliste carolinensis"
Observadores_EAI=Observadores_EAI[-which(Observadores_EAI$Specie=="alga filamentosa que recubreix tot el roquer"),]
Observadores_EAI=Observadores_EAI[-which(Observadores_EAI$Specie=="filamentosas"),]
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="Codium fragile sp. fragile")]="Codium fragile subsp. fragile"
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="Probablement Rissoella verruculosa")]="Rissoella verruculosa"
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="podría ser Padina pavonica, con la foto actual no puedo precisar")]="Padina pavonica"
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="ANEMONA COMUN (Anemonia sulcata variedad viridis)")]="Anemonia viridis"
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="Posiblemente Dictyota cyanoloma")]="Dictyota cyanoloma"
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="Caranx Crysos??")]="Caranx crysos"
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="C. crambe")]="Crambe crambe"
Observadores_EAI$Specie[which(Observadores_EAI$Specie=="C. crambe")]="Crambe crambe"
Observadores_EAI=Observadores_EAI[-which(Observadores_EAI$Specie=="Cianobacterias"),]

Observadores_EAI$Specie=stri_replace_all_charclass(Observadores_EAI$Specie, "\\p{WHITE_SPACE}", " ")
Observadores_EAI$Specie=trimws(Observadores_EAI$Specie, "both", whitespace = "[ \\h\\v]")
nombres=unique(Observadores_EAI$Specie)

temp=wormsbymatchnames(stri_trans_general(nombres,"Latin-ASCII") #elimina el espacio \\s+ por el normal y transforma en ASCII (elimina acentos o letras raras)
                       ,marine_only = F)

columnas=which(colnames(Observadores_EAI)%in%colnames(temp))

temp=data.frame(nombres,temp)

for (i in 1:length(temp[,1])) {
  temp1=which(Observadores_EAI$Specie==temp[i,1])
  Observadores_EAI[temp1,columnas]=temp[i,-1]
}

# Añadir las demarcaciones a la base de datos del Observadores del mar
NOR=which(Observadores_EAI$Latitud>41.66471&Observadores_EAI$Longitud<(-1.57104))
if (length(NOR)!=0) {
Observadores_EAI$Demarcacion[NOR]="NOR"
}
LEBA=which(Observadores_EAI$Latitud<42.759&Observadores_EAI$Longitud>(-2.192616))
if (length(LEBA)!=0) {
Observadores_EAI$Demarcacion[LEBA]="LEBA"
}
ESAL=which(Observadores_EAI$Latitud<37.5726&Observadores_EAI$Longitud>(-5.9163)&Observadores_EAI$Longitud<(-2.192616))
if (length(ESAL)!=0) {
Observadores_EAI$Demarcacion[ESAL]="ESAL"
}
SUD=which(Observadores_EAI$Latitud<37.286&Observadores_EAI$Longitud<(-5.9163)&Observadores_EAI$Longitud>(-7.543))
if (length(SUD)!=0) {
Observadores_EAI$Demarcacion[SUD]="SUD"
}
CAN=which(Observadores_EAI$Latitud<32.38)
if (length(CAN)!=0) {
  Observadores_EAI$Demarcacion[CAN]="CAN"
}

BD_Primeros=rbind(BD_Primeros,Observadores_EAI)

write.csv2(Observadores_EAI,
           file = paste("BD_Observadores_",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".csv",sep=""),
           sep="\t",
           row.names = F,
           fileEncoding = "UTF-8")

# write_xlsx(Observadores_EAI,
#            file = paste("BD_Observadores_",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".xlsx",sep=""))
} else {print("OBSERVADORES DEL MAR FALLA")}

# DIVERSIMAR ####
diversimar <- read_html('https://diversimar.cesga.es/visor/data/observacionesT.json')

if (exists("diversimar")) {
data <- data.frame(jsonlite::fromJSON(html_text(diversimar)))

plot(data$features.properties$long_4326,data$features.properties$lat_4326)

temp=which(BD_Primeros$Demarcacion=="NOR")

nombres_NOR=sort(unique(BD_Primeros$scientificname[temp]))

data$features.properties$scientific_name=trimws(data$features.properties$scientific_name, "both", whitespace = "[ \\h\\v]")
data$features.properties$scientific_name=stri_replace_all_charclass(data$features.properties$scientific_name, "\\p{WHITE_SPACE}", " ")

temp=which(data$features.properties$scientific_name%in%nombres_NOR)

data_Diversimar=data$features.properties[temp,]

EAI_Diversimar=matrix(NA, nrow = length(data_Diversimar$scientific_name), ncol = length(colnames(BD_Primeros)))
colnames(EAI_Diversimar)=colnames(BD_Primeros)
EAI_Diversimar=as.data.frame(EAI_Diversimar)

EAI_Diversimar$Year_First_record=data_Diversimar$obs_date
EAI_Diversimar$Specie=data_Diversimar$scientific_name
EAI_Diversimar$Latitud=data_Diversimar$lat_4326
EAI_Diversimar$Longitud=data_Diversimar$long_4326
EAI_Diversimar$scientificname=NA
EAI_Diversimar$Archivo="DIVERSIMAR_Red de Observación de la biodiversidad marina y pesquera de Galicia y Cantábrico"
EAI_Diversimar$Coord_Originales=paste(data_Diversimar$lat_4326, data_Diversimar$long_4326, data_Diversimar$location,sep = "_")
EAI_Diversimar$Other_relevant_reference=paste(data_Diversimar$id_observation, data_Diversimar$autor, sep = "_")
EAI_Diversimar$Demarcacion="NOR"
EAI_Diversimar$kingdom=NA
EAI_Diversimar$phylum=NA
EAI_Diversimar$class=NA
EAI_Diversimar$order=NA
EAI_Diversimar$family=NA
EAI_Diversimar$genus=NA
EAI_Diversimar$EAI="EAI5"

nombres=sort(unique(EAI_Diversimar$Specie))

temp=wormsbymatchnames(stri_trans_general(nombres,"Latin-ASCII") #elimina el espacio \\s+ por el normal y transforma en ASCII (elimina acentos o letras raras)
                       ,marine_only = F)

columnas=which(colnames(EAI_Diversimar)%in%colnames(temp))

temp=data.frame(nombres,temp)

for (i in 1:length(temp[,1])) {
  temp1=which(EAI_Diversimar$Specie==temp[i,1])
  EAI_Diversimar[temp1,columnas]=temp[i,-1]
}

BD_Primeros=rbind(BD_Primeros,EAI_Diversimar)

write.csv2(EAI_Diversimar,
          file = paste("BD_EAI_Diversimar_",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".csv",sep=""),
          sep="\t",
          row.names = F,
          fileEncoding = "UTF-8")
} else {print("DIVERSIMAR FALLA")}

# Guardar base de datos completa ####
write.csv2(BD_Primeros,
           file = paste("BD_COMPLETA_",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".csv",sep=""),
           sep="\t",
           row.names = F,
           fileEncoding = "UTF-8")

# write_xlsx(BD_Primeros,
#            file = paste("BD_COMPLETA_",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".xlsx",sep=""))

# Estatus especies ####
Estatus=NULL

demarcacion=c("CAN", "ESAL", "LEBA", "NOR", "SUD")

Nombres_columnas=c("Especie", "EASIN_check", "EASIN_Remarks", "Status_IEO_2018",
                   "Status_JRC_IUCN_2018", "Tsiamis_2019", "Status_IEO_2021", "Establishment_success","Demarcacion")

Archivos_excel=dir()[which(str_detect(dir(), ".xlsx"))]
for (i in 1:5) {
  
  Paginas=excel_sheets(Archivos_excel[i])
  
  Excel=read_excel(Archivos_excel[i], sheet = Paginas[1])
  
  Excel=data.frame(Excel)
  
  temp=Excel[-c(1,2),c(7,52:58)]
  
  temp$Demarcacion=rep(demarcacion[i], length(temp$Scientific.name))
  
  colnames(temp)=Nombres_columnas
  
  Estatus=rbind(Estatus,temp)
}


#Estatus=Estatus[-which(is.na(Estatus$Especie)),]

# poner el nombre cientifico en las del estatus ya que las he recogido directamente de las tablas de excel de primeros registros
Estatus$Especie=trimws(Estatus$Especie, "both", whitespace = "[ \\h\\v]")
Estatus$Especie=stri_replace_all_charclass(Estatus$Especie, "\\p{WHITE_SPACE}", " ")

nombres=sort(unique(stri_trans_general(Estatus$Especie,"Latin-ASCII")))

temp=which(nombres=="Haplosporidium pinnae"
           |nombres=="Mona blanca"
           |nombres=="Myxobolus portucalensis")

nombres=nombres[-temp]

temp=wormsbymatchnames(stri_trans_general(nombres,"Latin-ASCII") #elimina el espacio \\s+ por el normal y transforma en ASCII (elimina acentos o letras raras)
                       ,marine_only = F)

temp=data.frame(nombres,temp)

a=which(is.na(temp$scientificname))
temp$scientificname[a]=temp$nombres[a]

for (i in 1:length(temp[,1])) {
  filas=which(Estatus$Especie==temp[i,1])
  Estatus$Especie[filas]=temp[i,"scientificname"]
}

BD_Estatus=Estatus

# Comprueba que las especies de la base de datos de estado coincidan con la general de primeros registros
if (length(Estatus$Especie)==length(which(str_detect(BD_Primeros$Archivo, "BD_primeros registros_")))){
  print("Los datos coinciden, todo OK")
  }

# Poner el estatus completo
estatus=unique(BD_Estatus$Status_IEO_2021)
estatus2=c(NA,"Excluded","Range expanding","Cryptogenic","Alien","Crypto-expanding","Debatable","Excluded","Questionable")

for (i in 2:length(estatus)) {
  temp=which(BD_Estatus$Status_IEO_2021==estatus[i])
  BD_Estatus$Status_IEO_2021[temp]=estatus2[i] 
}

write.csv2(BD_Estatus,
           file = paste("Base_Datos_ESTATUS",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".csv",sep=""),
           row.names = F,
           fileEncoding = "UTF-8")

# write_xlsx(BD_Estatus,
#            paste("Base_Datos_ESTATUS",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".xlsx"))

# CARGAR BASE DE DATOS ESTATUS ####
# BD_Estatus <- read.csv2("D:/2021 - ALOCTONAS/Base de datos a modificar/Archivos/Base_datos_Estatus_EAI_2022-01-30.csv")

# Eliminacion de especies no EAI ####
# Basandome en los archivos de primeros registros de cada demarcación he seleccionado las especies EAI

Especies_EAI=unique(BD_Estatus$Especie) # En BD_Estatus estan todas las especies invasoras revisadas, extraidas de los archivos de primeros registros por demarcacion

# Especies_EAI=Especies_EAI[-which(is.na(Especies_EAI))]

temp=which(BD_Primeros$scientificname%in%Especies_EAI)

length(temp)==length(unique(temp))

BD_Primeros=BD_Primeros[temp,]

BD_Primeros$Longitud=as.numeric(BD_Primeros$Longitud)
BD_Primeros$Latitud=as.numeric(BD_Primeros$Latitud)

BD_Primeros$Longitud[which(BD_Primeros$Longitud>15)]=BD_Primeros$Longitud[which(BD_Primeros$Longitud>15)]*(-1)

Cabrera_coor=c(39.14390726804751, 2.944507257240857)
Cabrera=which(str_detect(BD_Primeros$Coord_Originales,pattern = "Cabrera")&!str_detect(BD_Primeros$Coord_Originales,pattern = "39"))

BD_Primeros[Cabrera,c("Latitud","Longitud")][1]=Cabrera_coor[1]
BD_Primeros[Cabrera,c("Latitud","Longitud")][2]=Cabrera_coor[2]


a=which(BD_Primeros$Latitud>33&BD_Primeros$valid_name=="Sparus aurata"|
          BD_Primeros$Latitud>33&BD_Primeros$valid_name=="Dicentrarchus labrax"|
          BD_Primeros$Latitud>33&BD_Primeros$valid_name=="Argyrosomus regius")

BD_Primeros=BD_Primeros[-a,]

#################### NO FUNCIONA BIEN, MODIFICAR
data = list(
  Puertos=c("Puerto de valencia","puerto de barcelona", "puerto de a coruña", "puerto de vigo", 
  "puerto de alicante", "puerto de ceuta","Puerto de Cudillero","Puerto de Gijón","Puerto de la Cruz",
  "Puerto de Málaga","Puerto de Tragobe","Puerto del Carmen", "puerto saladillo"),
  Lat=c(39.44788543606941, 41.34694436881236, 43.363622725102566, 42.230312267975705,
        38.33324161692196, 35.8952712858774, 43.56751732068425, 43.560460923703204, 
        28.46754830689097,36.71047108549424, 42.51842664270793,28.921117659261526, 36.119990945317724),
  Long=c( -0.31718422021873255, 2.1683640639124127, -8.388872150648087, -8.741366361948165,
         -0.49310501169754795, -5.317587018053679, -6.14998015770064, -5.692251478611518, 
         -16.243736203514935, -4.417132764620697, -8.824886373331005, -13.674328971252775, -5.440087289920226)
)

for (i in 1:length(data$Puertos)) {
  temp=which(str_detect(BD_Primeros$Coord_Originales,fixed(data[["Puertos"]][i], ignore_case=TRUE))&is.na(BD_Primeros$Latitud))
  BD_Primeros[temp,"Latitud"]=data[["Lat"]][i]
  BD_Primeros[temp,"Longitud"]=data[["Long"]][i]
}

plot(BD_Primeros$Longitud,BD_Primeros$Latitud)

# Poner el Estatus IEO que le corresponde ####
# Estatus_EAI=NULL
# for (i in 1:length(BD_Primeros$Specie)) {
#   if (length(str_split(BD_Primeros$Demarcacion[i],"-")[[1]])==3) {
#     a=str_split(BD_Primeros$Demarcacion[i],"-")[[1]][1]
#     b=str_split(BD_Primeros$Demarcacion[i],"-")[[1]][2]
#     c=str_split(BD_Primeros$Demarcacion[i],"-")[[1]][3]
#     
#     aa=BD_Estatus$Status_IEO_2021[which(BD_Primeros$valid_name[i]==BD_Estatus$Especie
#                                      &a==BD_Estatus$Demarcacion)]
#     
#     bb=BD_Estatus$Status_IEO_2021[which(BD_Primeros$valid_name[i]==BD_Estatus$Especie
#                                      &b==BD_Estatus$Demarcacion)]
#     
#     cc=BD_Estatus$Status_IEO_2021[which(BD_Primeros$valid_name[i]==BD_Estatus$Especie
#                                      &c==BD_Estatus$Demarcacion)]
#     if (length(aa)==0){
#       aa=NA
#     }
#     if (length(bb)==0){
#       bb=NA
#     }
#     if (length(cc)==0){
#       cc=NA
#     }
#     
#     temp=paste(aa,bb,cc,sep = "/")
#     } else if (length(str_split(BD_Primeros$Demarcacion[i],"-")[[1]])==2) {
#     a=str_split(BD_Primeros$Demarcacion[i],"-")[[1]][1]
#     b=str_split(BD_Primeros$Demarcacion[i],"-")[[1]][2]
#     
#     aa=BD_Estatus$Status_IEO_2021[which(BD_Primeros$valid_name[i]==BD_Estatus$Especie
#                                      &a==BD_Estatus$Demarcacion)]
#     
#     bb=BD_Estatus$Status_IEO_2021[which(BD_Primeros$valid_name[i]==BD_Estatus$Especie
#                                      &b==BD_Estatus$Demarcacion)]
#       if (length(aa)==0){
#         aa=NA
#         }
#       if (length(bb)==0){
#         bb=NA
#         }
#     
#     temp=paste(aa,bb,sep = "/")
#     
#     } else {
#   
#   temp=BD_Estatus$Status_IEO_2021[which(BD_Primeros$valid_name[i]==BD_Estatus$Especie
#                                         &BD_Primeros$Demarcacion[i]==BD_Estatus$Demarcacion)]
#     }
#   if (length(temp)==0){
#     temp=NA
#   }
#   Estatus_EAI=c(Estatus_EAI, temp)
# }
# BD_Primeros$Estatus_EAI_IEO_2021=Estatus_EAI

write.csv2(BD_Primeros,
           file = paste("Base_datos_SOLO_EAI_",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".csv",sep=""),
           row.names = F,
           fileEncoding = "UTF-8")

# write_xlsx(BD_Primeros,
#            paste("Base_datos_SOLO_EAI_",format(as.Date(Sys.Date(),format="%Y-%m-%d"),"%d%m%y"),".xlsx",sep=""))

# mapview(BD_Primeros[which(!is.na(BD_Primeros$Latitud)&!is.na(BD_Primeros$Longitud)),], xcol = "Longitud", ycol = "Latitud", crs = 4326, map.types = "CartoDB.Positron", grid = F)


# CARGAR BASE DE DATOS SOLO EAI####
# BD_EAI <- read.csv2("D:/2021 - ALOCTONAS/Base de datos a modificar/Archivos/Base_datos_final_solo_EAI_280122.csv")
# colnames(BD_EAI)[1]="ID"

length(sort(unique(BD_Primeros$scientificname))) # Especies unicas en total 1130
length(sort(unique(BD_EAI$scientificname))) # Especies unicas EAI en total 583
length(sort(unique(BD_Estatus$Especie))) # 583


# Función para buscar una especie ####
Buscar_Especie = function(x){
  temp=which(str_detect(BD_Primeros$scientificname,as.character(x))|str_detect(BD_Primeros$Specie,as.character(x)))
  temp2=BD_Primeros[temp,c("scientificname","Longitud","Latitud","Demarcacion")]
  print(temp2)
  mapview(temp2[which(!is.na(temp2$Latitud)&!is.na(temp2$Longitud)),], xcol = "Longitud", ycol = "Latitud", crs = 4326, map.types = "CartoDB.Positron", grid = F)
}

Buscar_Especie("Magallana gigas") # Funcion para buscar una especie por nombre en la BD_general