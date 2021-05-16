#-------------------------------------------------------------------------------
# Creacion:         25-03-2021
# Objetivo:         Crear base conjunta del promedio de los pixeles por cada región
# Ruta:             D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/2. Resultados/1. Estadística-zonal-bases
# Input:            "b2019-2.dbf"
# Output:           "btotal.dbf"
#-------------------------------------------------------------------------------

# Descripción:     

# Se crea una base de datos general en donde se tiene el promedio del valor de 
# los pixeles por región (ageb)


rm(list=ls()) 

library(data.table); library(base); library(foreign)
library(dplyr); library(tidyverse); library(readxl)

directorio="D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/2. Resultados/1. Estadística-zonal-bases"
setwd(directorio)

# Abrir bases vía loop, recortarlas y pegarlas nuevamente

l <- dir(pattern = "*.dbf")
for (k in 2:length(l)){
  b<-read.dbf(l[k])%>%select(CVEGEO,MEAN)%>%filter(MEAN>0)
  n<-paste(l[k])%>%substr(1,7)
  names(b)<-c("CVEGEO",paste(n))
  write.dbf(b,paste(directorio,n,".dbf",sep=""))
  #assign(n, b)
}

b<-read.dbf(l[1])
rm("k","l","n")

# Tomo las bases recortadas de la nueva ubicación

setwd(paste(directorio,"/Bases",sep=""))
l2 <- dir(pattern = "*.dbf")

i.lista <- llply(l2,read.dbf)

d <- Reduce(function(x, y) merge(x, y, 
                                 all=T, by=c("CVEGEO")), i.lista, accumulate=F)

b.total<-merge(b,d,all=T, by=c("CVEGEO"))%>%filter(b2015.1>0)

write.dbf(b.total,paste(directorio,"/btotal.dbf",sep=""))



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Creacion:         25-03-2021
# Objetivo:         Unir bases de datos del INE por trimestre
# Ruta:             D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/2. Resultados/1. Estadística-zonal-bases
# Input:            PL_20170131.xlsx    Estos archivos vienen del INE
# Output:           
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


# Descripción:     

# Unir bases de datos del INE por trimestre


rm(list=ls()) 
directorio2="D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/0. Datos/INE"
setwd(directorio2)

l <- dir(pattern = "*.xlsx")
#k=2
#for (k in 1:length(l)){
#  b<-read_excel(l[k])%>%
#    select("ENTIDAD","DISTRITO","MUNICIPIO","SECCION","LISTA")
#  b<-b%>%mutate(k=paste(b$ENTIDAD,b$DISTRITO,b$MUNICIPIO,b$SECCION, sep=""))
#  n<-paste(l[k])%>%substr(4,9)
#  b<-b%>%select("k","LISTA")%>%data.frame()
#  names(b)<-c("k",paste("t",n, sep=""))
#  write.dbf(b, paste(directorio2, "/Bases/t", n, ".dbf", sep=""))
#}

k=2

for (k in 1:length(l)){
  b<-read_excel(l[k])%>%
    select("ENTIDAD","MUNICIPIO","LISTA")%>%
    filter(MUNICIPIO>0)%>%
    filter(ENTIDAD==1 | ENTIDAD==11 | ENTIDAD==22 | ENTIDAD==24)
  b=b%>%mutate(k=paste(b$ENTIDAD, b$MUNICIPIO, sep=""))
  b=aggregate(b[c("LISTA")], by=list(k=b$k), FUN=sum)
  n<-paste(l[k])%>%substr(4,9)
  b<-b%>%select("k","LISTA")%>%data.frame()
  names(b)<-c("k",paste("t",n, sep=""))
  write.dbf(b, paste(directorio2, "/Bases/ent-t", n, ".dbf", sep=""))
}

rm("b")


# Tomo las bases recortadas de la nueva ubicación
library(plyr)

setwd(paste(directorio2,"/Bases",sep=""))
l2 <- dir(pattern = "*.dbf")

i.lista <- llply(l2,read.dbf)

d=Reduce(function(x, y) merge(x, y, 
                                 all=T, by=c("k")), i.lista, accumulate=F)%>%select(c(1,2,7,10,14))

#b.total<-merge(b,d,all=T, by=c("CVEGEO"))%>%filter(b2015.1>0)

write.dbf(d,paste(directorio2,"/btotal_ine.dbf",sep=""))
write.csv(d,paste(directorio2,"/btotal_ine.csv",sep=""))




