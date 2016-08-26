#Scrip para cargar los datos recolectados en Recreo en la encuesta
#el directorio de trabajo es d:\Archivos\Drive\FCM\Recreo
#Lo que debemos hacer ahora es solo quedarnos con los radios 9 10 20 21 23 25 27 28
#Para poder sacar lo que no nos sirve poligonos = poligonos[poligonos$DPA_PROVIN !=20,]

#Que es donde se realizo la encuesta
#Encontre una forma de realizar los graficos utilizando los mapas de google
#con la key de API de geocoding de Google, pude sacar las coordenadas con python

#Cargamos las librerias necesarias
library(rgdal) #Nos permite leer el shp file, tambi?n se puede usar maptools()
library(RColorBrewer) #Para la manipulaci?n de lo paleta de colores
library(classInt) #Formaci?n de las clases necesarias para asignarle los colores
library(GISTools)
library(maps)
library(dismo)
rm(list = ls())
#constante con los datos CRS para la proyeccion a utilizar
wgs84 <- '+proj=longlat +datum=WGS84'
#Seteamos el directorio de trabajo
setwd("D:/Archivos/Drive/FCM/Recreo")
#Se usa para el IPEC la designacion de R0xxx que corresponde a los radios censales
calles <- readOGR("E8592.shp",layer = "E8592")
#le asignamos los datos sobre la proyeccion a utilizar basados en 
#EPSG 5347 que corresonde al sector 5 de argentina los datos fueron extraidos de EPSG <- make_EPSG()
#EPSG[3493,]
proj4string(calles)<-CRS("+proj=tmerc +lat_0=-90 +lon_0=-60 +k=1 +x_0=5500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#transformamos los datos con la constante CRS
calles<-spTransform(calles,wgs84)
#Hacemos lo mismo para los radios censales
recreo <- readOGR("R8592.shp",layer = "R8592")
proj4string(recreo)<-CRS("+proj=tmerc +lat_0=-90 +lon_0=-60 +k=1 +x_0=5500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
calles<-spTransform(calles,wgs84)

#los datos solo lo podemos ver 
#View(recreo@data) Vemos los datos
plot(recreo)
plot(calles,add=TRUE)
SP <- SpatialPoints(cbind(-60.72613, -31.49046), proj4string=CRS("+proj=tmerc +lat_0=-90 +lon_0=-60 +k=1 +x_0=5500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

centro_recreo <- recreo[recreo$Rad2010 != c(28,29,26,18),] #No funciona
#plot(centro_recreo)
centro_recreo = centro_recreo[centro_recreo$Rad2010 != 28,]
centro_recreo = centro_recreo[centro_recreo$Rad2010 != 29,]
centro_recreo = centro_recreo[centro_recreo$Rad2010 != 26,]
centro_recreo = centro_recreo[centro_recreo$Rad2010 != 16,]
centro_recreo = centro_recreo[centro_recreo$Rad2010 != 18,]
map_recreo <- spTransform(centro_recreo, CRS("+proj=longlat +datum=WGS84"))
#Dibujo Final
plot(map_recreo)
plot(calles,add=TRUE)
#Utilizando el paquete dismo y gmap
# con esto marcamos el sector donde queremos enfocarnos

e = extent(-60.746454,-60.722765,-31.484558,-31.496067)
#cremos el mapa con 
recreo_google = gmap(e, exp = 1,type = 'roadmap',scale = 2,style = "element:labels|visibility:off")
recreo_google = gmap(e, lonlat = TRUE, type = 'roadmap',scale = 2,style = "element:labels|visibility:off",key='AIzaSyBpwY_CHchN-fmAhV3B483FyEOhS3pcaPw')
#Graficamos 
plot(recreo_google,interpolate=TRUE)


#Para graficar los puntos sobre el mapa
#Cargo los datos
personas <-read.csv("d:/Archivos/Drive/FCM/Recreo/final_google.csv",sep=",",dec=".")
personas <-read.csv("encuestas_casas.csv",sep=",",dec=".")
#Reacomodo las columnas necesito primero la long y despues la latitud

personas<-personas[c(1,2,3,4,5,6,8,7)]
#Asigno las coordenadas a una variables para poder graficarla
puntos<- personas[4:5]

puntos<-puntos[-167,]
puntos<-na.omit(puntos)
coordinates(puntos)<- ~lon + lat
projection(puntos)<-wgs84



#grafico los puntos
points(puntos,col='red',cex=0.5,pch=15)
#Lo tengo que transformar en coordenadas x y
pto_xy = Mercator(puntos)
#A?adimos los puntos
points(pto_xy,cex=3,pch=20,col='red',add=TRUE)
#Para graficar los puntos tambien puedo utlilizar el comando coordinates, pero para hacerlo
#debe estar primero la long y luego la latitud.



radio <-rep("Radio ",10)
poblacion <- read.csv("Recreo.csv", sep=";", dec=",")
radio <-rep("Radio ",10)
nradio <-c(recreo@data["Rad2010"])
nradio
nradio <-as.data.frame(nradio)
nradio<-as.character(nradio[,1])
RadiosCensales <- paste(radio,nradio,sep="")
centroides <- coordinates(recreo)
datos <-as.matrix(poblacion[,2:3])
rownames(datos)<-RadiosCensales
personas <-poblacion[,2]
personas <-as.data.frame(personas)
names(personas)<-"personas"
row.names(personas)<-row.names(recreo)
personas
#no se encuentra el radio censal 08
recreo.data <-SpatialPolygonsDataFrame(recreo,personas)
#creamos un vector con la los habitantes
plotvar <- recreo.data$personas
plotvar
nclr <- 5
#creamos la paleta de colores
plotclr <- brewer.pal(nclr,"Oranges")
#aqui creamos las divisiones
class <- classIntervals(round(plotvar,0),nclr,style="quantile")
#asignamos los colores de acuerdo a la paleta y la division de clases
colcode <- findColours(class,plotclr)
#aqui comienza los comandos para formar el JPG

jpeg("Recreo.jpeg",quality=100,height = 3072,width = 2048)
tiff(filename = "grande.tiff", width = 2048, height = 3072,compression="none",pointsize=3,res=600,type="cairo")
plot (recreo.data, col=colcode, border="grey", axes=T)
lines(calles,cex=0.2,col='cyan')
lines(recreo,cex=0.6, col='red',lty=5)
#plot (calles, add=TRUE, style="p")
title(main = "Personas en la Ciudad de Recreo",cex=15)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=1.3)
text(centroides,RadiosCensales,cex=1.5)
text(5432200, 6509000,"Cantidad de Habitantes",cex=1.2)
north.arrow(5427000,6512000,len=100,lab="Norte",cex=2)
map.scale(5427000,6511600,400,"100 metros",4,1)
dev.off()
