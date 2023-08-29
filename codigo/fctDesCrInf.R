library(tidyverse)

################################################################################
#
#
##                 FACTORES DESNUTRICIÓN CRÓNICA INFANTIL                   ####
#
################################################################################

#Autor: Aaron Sanchez
#Fecha: 28 de agosto de 2023
#Secretaría Técnica Ecuador Crece sin Desnutrición Crónica Infantil

#Repositorio de Github: https://github.com/ainsanchez/desCrInf.git

#Accedo a la base descargada en: 
#https://www.ecuadorencifras.gob.ec/salud-salud-reproductiva-y-nutricion/

desInf <-  read.csv("bases_ori/9_BDD_ENS2018_f5_des_inf.csv")

head(desInf)

#Identifico variables relacionadas a la desnutrición crónica infantil en 
#concordancia con el diccionario de variables
names(desInf)

#Aplico el primer filtro para trabajar únicamente con infantes menores a 
#2 años de edad a la fecha de realizada la encuesta, y variables de interés
#conforme al ensayo

desInf2 <- desInf %>% 
  filter(f5_s1_100_6 < 2) %>% 
  select(area, prov, f5_s2_200, f5_s2_206, region, etnia, dcronica, nivins_mef,
         f5_s11_2_18, f5_s11_3_25, f5_s10_4_28, f5_s5_503, f5_s5_505,
         f5_s10_1_6, f5_s10_2_7, f5_s11_2_14, f5_s2_215)

#Aplico tablas de frecuencia para identificar la completitud de estas variables

for (i in 1:length(desInf2)){
  print(table(desInf2[,i], useNA = "always"))
}

#Con base en los resultados, procedo a eliminar variables que no tienen datos con
#una tasa alta de valores perdidos o nulos (>20%)

desInf2 <- desInf2 %>% 
  select(area, prov, f5_s2_200, etnia, dcronica, f5_s10_1_6, f5_s10_2_7)

#Renombro las variables codificadas para mejorar la interpretación de los 
#resultados

varNames <- c("centroCDI",
              "lugarJuegosSeguro",
              "permisoJuegosEnsucie")

names(desInf2)[c(3,6,7)] <- varNames

#Elimino los valores perdidos de las variables objetivo y de las variables 
#explicativas, considerando que son mínimos

for (i in 1:length(desInf2)){
  desInf2 <- desInf2 %>% 
    filter(is.na(desInf2[,i])==F)
}

#Codifico a las variables conforme a su naturaleza
desInf2$prov <- as.character(desInf2$prov)

#Genero un modelo de regresión logística para estimar los factores que influyen
#en la desnutrición crónica infantil de niños menores de 2 años
fctDesCrInf <- glm(dcronica~., family = "binomial", data=desInf2)
summary(fctDesCrInf)

#Interpretación:
#De acuerdo a los resultados del modelo de regresión logística, se puede notar 
# que el factor de etnia indígena es el más significativo, guarda consistencia 
# con los factores socioeconómicos, y tiene la mayor magnitud para determinar la 
# desnutrición crónica infantil, con un odds ratio de 0,6172. Además, es 
# llamativo que la asistencia a centros CDI está relacionada con una probabilidad
# de 0,35 en tener desnutrición crónica infantil, lo cual puede ser un indicador
# de la ausencia de los padres en casa durante el día. Finalmente, La tercera 
# variable significativa es que aquellos niños que tienen permiso de sus padres 
# para jugar ensuciándose y en desorden, tienen una probabilidad de 0,26 de tener
# desnutrición crónica infantil, lo cual tiene relación a los malos hábitos de
# salud en el hogar.
