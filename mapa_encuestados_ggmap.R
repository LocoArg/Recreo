#Nuevo mapa Recreo utilizando el paquete ggmap
library(ggplot2)
library(ggmap)
recreo_1 <-get_map(location = "Recreo, Santa Fe Argentina",zoom = 15,source = "osm",maptype = "watercolor" )
recreo_2<-get_map(location="Recreo, Santa Fe Argentina", zoom=16)
ggmap(recreo_1, extent = "normal") + geom_density2d(data = df.prueba, 
aes(x = lon, y = lat), size = 0.3) + stat_density2d(data = df.prueba, 
aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
ggmap(recreo_2, extent = "normal") + geom_density2d(data = df.prueba, 
 aes(x = lon, y = lat), size = 0.2) + stat_density2d(data = df.prueba, 
 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.03, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
ggmap(recreo_1) + geom_point(aes(x = lon, y = lat, size = .05), data = df.prueba, alpha =.3) + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.title.y=element_blank(), axis.text.y = element_blank(), legend.position = 'none')
#Puntos con errores

jpeg("proba.jpeg",quality=100,height = ,width = 2048)
ggmap(recreo_1) + geom_point(aes(x = lon, y = lat, size = 5), data = casas, alpha =.3) +
theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.title.y=element_blank(), axis.text.y = element_blank(), legend.position = 'none')
dev.off()

#Puntos con errores
jpeg("FONDO.jpeg",quality=100,height = 3072,width = 2048)
plot(recreo)
plot(calles,add=TRUE)
dev.off()



lista = c(3,764,549,576,299,671,567,571,353)
casas = subset(df.casas, !(ID_Casa %in% lista))
puntos <- casas[,4:5]
coordinates(puntos)<- ~lon + lat
projection(puntos)<-'+proj=longlat +datum=WGS84'

ggmap(recreo_1) + geom_point(aes(x = lon, y = lat, size = "0.01"), data = casas, alpha =.3) + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),axis.title.y=element_blank(), axis.text.y = element_blank(), legend.position = 'none')



ggmap(recreo_1) + geom_point(aes(x = lon, y = lat, size = "0.01"), data = puntos, alpha =.3) + 
  geom_text(aes(label=ID_Casa),data = casas ,hjust=0, vjust=0)




