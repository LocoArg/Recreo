#Cargamos las librerias necesarias
library(rgdal) #Nos permite leer el shp file, también se puede usar maptools()
library(RColorBrewer) #Para la manipulación de lo paleta de colores
library(classInt) #Formación de las clases necesarias para asignarle los colores
library(GISTools)
library(maps)
#Seteamos el directorio de trabajo
setwd("~/Practicas en R/Trabajos/Recreo")
#Se usa para el IPEC la designacion de R0xxx que corresponde a los radios censales
calles <- readOGR("E8592.shp",layer="E8592")
recreo <- readOGR("R8592.shp",layer="R8592")
#los datos solo lo podemos ver 
View(recreo@data)
plot(recreo)
plot(calles,add=TRUE)
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
