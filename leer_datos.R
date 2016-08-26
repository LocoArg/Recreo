library(dplyr)
library(tidyr)
encuestas <-read.csv('encuestas.csv',header=TRUE,sep=",")
casas <-read.csv('encuestas_casas.csv',header=TRUE,sep=",")
union <-read.csv('encuestas_Acasas.csv',header=TRUE,sep=",")
fechas <-read.csv('encuestas_fechas.csv',header=TRUE,sep=",")
df.encuestas <-tbl_df(encuestas)
df.fechas <-tbl_df(fechas)
df.casas <-tbl_df(casas)
df.union <-tbl_df(union)
df.casas <- df.casas[,1:5]
df.union <-df.union[,1:2]
rm("casas","encuestas","fechas","union")
df.encuestas$ID_Encuesta <-as.numeric(levels(df.encuestas$ID_Encuesta)[df.encuestas$ID_Encuesta])
df.encuestas$Etnia<-factor(df.encuestas$Etnia,levels = c("Criollo","Mocovi","Otro"))
df.encuestas$Genero <-factor(df.encuestas$Genero,levels = c("Femenino","Masculino"))
df.encuestas$Sexo<-factor(df.encuestas$Sexo,levels = c("Mujer","Varón"))

df.prueba <- left_join(df.union,df.encuestas,by="ID_Encuesta")
df.prueba <- left_join(df.prueba,df.casas,by="ID_Casa")
#Falta crear las preguntas a responder
#para aplicar las capas de informacion tendria que hacer subset de datos tomando como informacion los puntos.
#Genero vs Sexo
#Escolaridad
#Obra Social
#Lugar de Asistencia comparado con los centros mostrar en el mapa
#Ver QPaq para resumir la informacion sobre habitos 
#AF y cumplimiento 


#Para obtener las mujeres
mujeres <-dplyr::filter(df.prueba,Sexo == "Mujer") %>% dplyr::select(lat,lon)
varones <-dplyr::filter(df.prueba,Sexo == "Varón") %>% dplyr::select(lat,lon)
