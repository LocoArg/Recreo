#Cargamos las librerias necesarias
library(rgdal) #Nos permite leer el shp file, también se puede usar maptools()
library(RColorBrewer) #Para la manipulación de lo paleta de colores
library(classInt) #Formación de las clases necesarias para asignarle los colores
#Seteamos el directorio de trabajo
setwd("C:/Users/Emiliano/Desktop/Laguna Paiva") 
#Se usa para el IPEC la designacion de R0xxx que corresponde a los radios censales
calles <- readOGR("E0220.shp",layer="E0220")
laguna <- readOGR("R0220.shp",layer="R0220")
#los datos solo lo podemos ver 
View(laguna@data)
plot(laguna,add=TRUE)
radio <-rep("Radio ",11)
#aqui se asigna un vector porque no pude sacarlo de la base de datos que está en laguna@data["Rad2010"]
#   Rad2010
#0       13
#1       04
#2       05
#3       08
#4       06
#5       09
#6       10
#7       07
#8       31
#9       11
#10      12
#nradio <-c("13","04","05","08","06","09","10","07","31","11","12")
nradio <-c(laguna@data["Rad2010"])
nradio <-as.data.frame(nradio)
nradio<-as.character(nradio[,1])
RadiosCensales <- paste(radio,nradio,sep="")
#calculamos los centros para graficar las etiquetas
centroides <- coordinates(laguna)
plot(laguna)
plot(calles,add=TRUE)
#text(centroides,RadiosCensales,cex=0.7)
#leemos el archivo donde se encuentran los datos referidos a los radios censales
poblacion <- read.csv("poblacion1.txt", sep=";", dec=".")
poblacion
#tomamos los datos que tienen informacion de la población
datos <-as.matrix(poblacion[,2:3])
datos
#asignamos los mismos indices(nombres de fila) de las etiquetas contruidas a los datos
rownames(datos)<-RadiosCensales
datos
#construimos un nuevo conjunto de datos solo con la informacion de las cantidad de habitantes
personas <-poblacion[,2]
personas <-as.data.frame(personas)
names(personas)<-"personas"
#asignamos el nombre de fila del shapefile a los informacion de habitantes por radio censal
row.names(personas)<-row.names(laguna)
personas
#construimos un nuevo objeto shape file con la informacion de los habitantes más la georeferencial
laguna.data <-SpatialPolygonsDataFrame(laguna,personas)
#creamos un vector con la los habitantes
plotvar <- laguna.data$personas
plotvar
#vector para definir la cantidad de grupos
nclr <- 5
#creamos la paleta de colores
plotclr <- brewer.pal(nclr,"Blues")
#aqui creamos las divisiones
class <- classIntervals(round(plotvar,0),nclr,style="quantile")
#asignamos los colores de acuerdo a la paleta y la division de clases
colcode <- findColours(class,plotclr)
#aqui comienza los comandos para formar el JPG
jpeg("Laguna_nuevo.jpeg",quality=100,height = 3072,width = 2048)
plot (laguna.data, col=colcode, border="grey", axes=T)
plot (calles, add=TRUE, style="p")
title(main = "Personas en la Ciudad de Laguna Paiva",cex=3)
legend("bottomright",legend = names(attr(colcode,"table")),
       fill= attr(colcode,"palette"),cex=4)
text(centroides,RadiosCensales,cex=2)
text(5438200, 6535400,"Cantidad de Habitantes",cex=1.5)
dev.off()