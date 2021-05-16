#--------
# EBLUPs based on a spatio-temporal FH model


# Preparar el entorno de trabajo

library(date); library(data.table);
library(dplyr);library(base)
library(foreign); library(survey)
library(readxl);library(tidyr)
library(reshape)

rm(list=ls())



#------------------------------------------------------------------------
# A partir de las máscaras, elige los municipios para obtener 
# la distancia


# Ruta general
r1="D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/"

# Resultados
res="D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/2. Resultados/3. Modelo/"


# Municipios que tienen datos
# Se obtuvo usando las máscaras de cobertura urbana y los shapes

setwd(paste(r1,"2. Resultados/1. Estadística-zonal-bases/E2/", sep = ""))
datos=read_excel("resumen-promedio-estados-general.xlsx", "narrative-387")
datos=datos%>%mutate(CVEGEO=ifelse(nchar(CVE)==4,paste("0",CVE,sep=""), CVE))

datos=datos%>%filter(CVE_ENT==1)   # Filtro para casos particulares
d=datos[,c("CVEGEO","n")]


# Matriz de distancia con los municipios seleccionados
  
setwd(paste(r1,"0. Datos/INEGI/SHAPES/shape_mx", sep = ""))
sh=read.dbf("00mun.dbf"); sh=sh[,1:4]   # Dejo la base original

sh=merge(sh, d, by=c("CVEGEO"), all.x = TRUE)
write.dbf(sh,"00mun.dbf")

rm("sh","d","datos")

#------------------------------------------------------------------------

# La matriz (luego de que ya se calculó la distancia)
# Ruta: 1. Programas/ArcGis
library(textshape)

setwd(paste(r1,"1. Programas/ArcGis", sep=""))
distancia=read_excel("aguascalientes.xlsx", "matriz4")
distancia=column_to_rownames(distancia, loc = 1)
saveRDS(distancia,paste(res,"distancia.RDS",sep = ""))


distancia=readRDS(paste(res,"distancia.RDS",sep = ""))

#------------------------------------------------------------------------
# Varianza de los estimadores (paso por paso)
# Se obtienen todos los valores, no es necesario hacer filtro en este paso 

#setwd(paste(r1,"1. Programas/ArcGis", sep=""))
#distancia=read_excel("aguascalientes.xlsx", "matriz4")
#d=distancia[,1]%>%mutate(n=1)

# Datos de la intercensal para 2015

setwd("D:/OneDrive - El Colegio de México A.C/0. Tesis/4 Datos/DESK/INEGI/INTERCENSAL 2015")

ei2015=read.csv("PERSONAS.csv")
ei=ei2015
ei=ei%>%mutate(t1=ifelse(nchar(ENT)==1,paste("0",ENT,sep=""), ENT))
ei=ei%>%mutate(t2=ifelse(nchar(MUN)==1,paste("00",MUN,sep=""), ifelse(nchar(MUN)==2,paste("0",MUN,sep=""), MUN)))
ei=ei%>%mutate(CVEGEO=paste(t1,t2,sep=""))

ei=merge(ei, d, by=c("CVEGEO"), all.x = TRUE)

# se carga el archivo con los bloques para las estimaciones

setwd("D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/2. Resultados/1. Estadística-zonal-bases/E2/")
datos=read_excel("resumen-promedio-estados-general.xlsx", "narrative-387")
datos=datos%>%mutate(CVEGEO=ifelse(nchar(CVE)==4,paste("0",CVE,sep=""), CVE))
d2=datos[, c("CVEGEO","n")]
names(d2)=c("CVEGEO","bloque")

ei=merge(ei, d2, by=c("CVEGEO"), all.x = TRUE)

# Casos válidos
ei=ei%>%filter(n==1 & bloque>=1)

# Estimación de la varianza tomando en cuenta el diseño muestral
datalist = list()
j=1
for(i in 1:10){
  #e=ei%>%filter(bloque==i)
  ds<-svydesign(id=~UPM, strata=~ESTRATO, weight=~FACTOR, data=ei, nest=TRUE)
  options(survey.lonely.psu="adjust")
  
  svy<-svytotal(~factor(CVEGEO), ds, deff=TRUE)%>%data.frame()
  svy=svy%>%mutate(C=rownames(svy))
  
  datalist[[j]] <- svy
  j=j+1
}

mun_2015<-do.call(rbind, datalist)

setwd("D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/1. Programas/R")

write.csv(mun_2015,"mun_2015.csv")

# Se quita el texto

mun_2015=read.csv("mun_2015.csv")
mun_2015=mun_2015%>%
  mutate(CVEGEO=ifelse(nchar(CVE)==4,paste("0",CVE,sep=""), CVE))%>%
  select(CVEGEO,total, SE)

# cargar los datos estadística zonal

setwd("D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/2. Resultados/1. Estadística-zonal-bases/E2/")
base=read_excel("resumen-promedio-estados-general.xlsx", "narrative-387")
base=base%>%mutate(CVEGEO=ifelse(nchar(CVE)==4,paste("0",CVE,sep=""), CVE))



base=merge(base, mun_2015, by=c("CVEGEO"), all.x = TRUE)

base=merge(base, mun_2015, by=c("CVEGEO"), all.x = TRUE)

write.csv(base,"base.csv")

# El acomodo de la base se hizo en excel 


#-------------- AGUASCALIENTES --------------
library(reshape)
setwd("D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/2. Resultados/3. Modelo")
b=read_excel("base_general_v2.xlsx", "base")

cob_urb<-b[,c(1:7)]%>%pivot_longer(
  cols = starts_with("n"),
  names_to = "Tiempo",
  values_to = "valor",
  values_drop_na = TRUE
)%>%arrange(CVEGEO, Tiempo)

p_est<-b[,c(1,13:18)]%>%pivot_longer(
  cols = starts_with("p_est"),
  names_to = "Tiempo",
  values_to = "valor",
  values_drop_na = TRUE
  )%>%arrange(CVEGEO, Tiempo)

pob<-b[,c(1,25:30)]%>%pivot_longer(
  cols = starts_with("p"),
  names_to = "Tiempo",
  values_to = "valor",
  values_drop_na = TRUE
)%>%arrange(CVEGEO, Tiempo)


se<-b[,c(1,36:41)]%>%pivot_longer(
  cols = starts_with("SE"),
  names_to = "Tiempo",
  values_to = "valor",
  values_drop_na = TRUE
)%>%arrange(CVEGEO, Tiempo)


censo<-read_excel("base_general_v2.xlsx", "CENSO")
censo=censo%>%
  mutate(CVEGEO=ifelse(nchar(CVE)==4,paste("0",CVE,sep=""), CVE))


#d5<-melt(b[,c(1,8:13)], id=c("CVEGEO"))%>%arrange(CVEGEO, variable)

bf=cbind(cob_urb,p_est[,3],pob[,3],se[,3])
names(bf)=c("CVE","TIEMPO","LUCES","P_EST","POB","VAR")

bf=bf%>%mutate(TIME=substr(TIEMPO,2,5))%>%data.frame()
bf=bf%>%
  mutate(CVEGEO=ifelse(nchar(CVE)==4,paste("0",CVE,sep=""), CVE))
bf=bf%>%mutate(AREA=substr(CVEGEO,1,2))

ejemplo=bf%>%filter(TIEMPO=="n2020")

ejemplo=merge(ejemplo, censo, by=c("CVEGEO"), all.x = T)

attach(ejemplo)
FH <- mseFH(P_EST ~ LUCES +factor(AREA), VAR)
cv.FH <- 100 * sqrt(FH$mse) / FH$est$eblup
results <- data.frame(CVEGEO = CVEGEO, CONAPO = POB, ESTIMACION = P_EST, eblup.FH = FH$est$eblup, cv.FH)
results <-merge(results, censo, by=c("CVEGEO"), all.x = T)
detach(ejemplo)
View(results)

results=results%>%mutate(d=abs(1-(CONAPO/CENSO)))
table(results$d)

# prueba 1

setwd("D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/1. Programas/ArcGis")

setwd("D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/1. Programas/ArcGis/")
distancia=read_excel("matriz_peso_municipios_seleccionados.xlsx","matriz4")
distancia=distancia[,-1]

#--------
SFH <- mseSFH(POB ~ as.factor(AREA)-1, VAR, distancia, data = ejemplo)

cv.SFH <- 100 * sqrt(SFH$mse) / SFH$est$eblup

results <- data.frame(DIR = grapes$grapehect,
                      cv.DIR = 100 * abs(sqrt(grapes$var) / grapes$grapehect),
                      eblup.SFH = SFH$est$eblup, cv.SFH)


data(iris)
str(iris)
n <- nrow(iris)
i_boot <- sample(n, replace = TRUE)
data_boot <- iris[i_boot, ]
