#-------------------------------------------------------------------------------
# Creacion:         12-03-2021
# Autor:            Julio C.
# Contacto:         jcms2665@gmail.com
# Objetivo:         Pegado de la base de datos
# Datos:             
# Github:           https://github.com/jcms2665/FLACSO-MATE-II
#-------------------------------------------------------------------------------



# =========================  APPEND - AGEB  =======================================

rm(list=ls()) 

library(jsonlite); library(httr); library(date); library(data.table)
library(lubridate); library(stringr); library(kableExtra); library(janitor)
library(dplyr); library(base); library(foreign); library(gdata)
library(rlist); library(readxl)

#== Juntar bases de datos

setwd("D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/0. Datos/INEGI/AGEB-Población")


listcsv <- dir(pattern = "*.csv")
listcsv

#== Se crea una lista y ahi se guardan todos los archivos
ldf <- list()
k=1
for (k in 1:length(listcsv)){
  ldf[[k]] <-read.csv(listcsv[k])
}

a<-do.call(rbind, ldf)
a<-a%>%rename(ENTIDAD=ï..ENTIDAD)


# CODIGO AGEB 13 DÍGITOS = EE + MMM + LLLL + AAAA

a<-a%>%mutate(ENTIDAD_ok=ifelse(nchar(a$ENTIDAD)==2,ENTIDAD,paste("0",ENTIDAD, sep="")))
a<-a%>%mutate(MUN_ok=ifelse(nchar(a$MUN)==3,MUN,
                            ifelse(nchar(a$MUN)==2,paste("0",MUN, sep=""),paste("00",MUN, sep=""))))
a<-a%>%mutate(LOC_ok=ifelse(nchar(a$LOC)==4,LOC,
                            ifelse(nchar(a$LOC)==3,paste("0",LOC, sep=""),
                                   ifelse(nchar(a$LOC)==2,paste("00",LOC, sep=""),paste("000",LOC, sep="")))))

a<-a%>%mutate(CVEGEO=paste(ENTIDAD_ok,MUN_ok,LOC_ok,AGEB, sep=""))
a<-a%>%mutate(t=nchar(a$CVEGEO))

#a<-a%>%select("CVEGEO","NOM_LOC", "P_18YMAS","P_18YMAS_F","P_18YMAS_M","POBTOT","POBFEM","POBMAS","VIVTOT", "TVIVHAB","TOTHOG","POBHOG" )
#names(a)<-c("CVEGEO","NOM_LOC", "P18","P_18YMAS_F","P_18YMAS_M","POBTOT","POBFEM","POBMAS","VIVTOT", "TVIVHAB","TOTHOG","POBHOG" )

a<-a%>%select("CVEGEO","NOM_LOC", "P_18YMAS","POBTOT","VIVTOT", "TOTHOG","POBHOG", "ENTIDAD_ok","MUN_ok" )
names(a)<-c("CVEGEO","NOM_LOC", "P18","PTOT","VIV", "HOG","PHOG","ENTIDAD","MUN" )

#  & ENTIDAD==9 & MUN==3

entidad<-a%>%filter(NOM_LOC=="Total de la entidad")
municipios<-a%>%filter(NOM_LOC=="Total del municipio")
localidad<-a%>%filter(NOM_LOC=="Total de la localidad urbana")
agebs<-a%>%filter(NOM_LOC=="Total AGEB urbana")
manzana<-a%>%filter(NOM_LOC!="Total de la entidad" & NOM_LOC!="Total del municipio" & NOM_LOC!="Total de la localidad urbana" & NOM_LOC!="Total AGEB urbana")

agebs<-agebs%>%select("CVEGEO", "P18","PTOT","VIV", "HOG","PHOG") 


write.csv(manzana,"m.csv")
write.csv(agebs,"a.csv")
write.csv(localidad,"l.csv")
write.csv(municipios,"m.csv")
write.csv(entidad,"e.csv")

write.dbf(municipios,"m.dbf")


#-------------------------------------------------------------------------------
# Creacion:         21-03-2021
# Autor:            Julio C.
# Contacto:         jcms2665@gmail.com
# Objetivo:         Analizar la población vs estadística zonal
# Datos:            tabla.dbf 
# Github:           
#-------------------------------------------------------------------------------


rm(list=ls()) 

library(data.table); library(kableExtra)
library(dplyr); library(base); library(foreign)
library(psych)
setwd("D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/2. Resultados/1. Estadística-zonal")

t<-read.dbf("tabla.dbf")
t.1<-t%>%select("P18","PHOG","VIV","PTOT","SUM","MEAN")%>%filter(MEAN>0 & VIV>0)


t.1<-data.matrix(t.1, rownames.force = NA)

pairs.panels(t.1, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


cor(t.1)
