
#-------------------------------------------------------------------------------
# Creacion:         01-05-2021
# Autor:            Julio C.
# Contacto:         jcms2665@gmail.com
# Objetivo:         Comparación entre los datos del INE vs INEGI
# Datos:             
# Github:           
#-------------------------------------------------------------------------------

rm(list=ls()) 
library(data.table)
library(readxl); library(dplyr);library(tidyr)
setwd("D:/OneDrive - El Colegio de México A.C/0. Tesis/5 Datos-Silvan/5. Ejericicio_2/")

i=read.csv("UT_21_00965_pdln_edmslm_20210331.txt", header = TRUE, sep = ",")%>%filter(NB_ENTIDAD!=0)

i.1=i%>%select(c(1, 36, 38, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 37, 39, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61))
i.1=mutate_all(i.1, function(x) as.numeric(as.character(x)))
ine.1=aggregate(i.1, by=list(ENT=i.1$ENTIDAD), FUN=sum)
ine=ine.1[,-2]
rm("i.1","ine.1")
v1=names(ine)


# Bases de INEGI


b=read_excel("INEGI.xlsx", "INEGI")%>%data.frame()

b1=b%>%mutate(H_PADRON_65_Y_MAS_HOMBRES=rowSums(b[,c("H_65_69","H_70_74","H_75_79","H_80_84","H_85_y_más","H_No_especificado")]))
b2=b1%>%mutate(M_PADRON_65_Y_MAS_MUJERES=rowSums(b1[,c("M_65_69","M_70_74","M_75_79","M_80_84","M_85_y_más","M_No_especificado")]))
names(b2)
inegi=b2[ ,c(1, 36:46, 78, 61:71, 79)]%>%data.frame()
rm("b1","b2")

names(ine)=names(inegi)
rm("b","i")

write.csv(ine, "ine.csv")
write.csv(inegi, "inegi.csv")

#-----------  Primero INE

dif=matrix(0, nrow=(dim(ine)[1]), ncol=dim(ine)[2]-1 )
for (i in 1:32) {
  for (j in 1:24) {
    dif[i,j]=round(abs(1-(ine[i,j+1]/inegi[i,j+1]))*100,2)
  }
}

names(dif)=l=names(ine[, 2:25])

dif=dif%>%mutate(r5 = rowSums(u[,c("Y60_A_65","Y65_MAS")]))


write.csv(dif, "dif.csv")
