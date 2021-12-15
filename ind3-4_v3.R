"Descripción: Este script forma parte del protocolo para el monitoreo ecológico de las playas de anidación
de tortugas marinas. Este script unifica los datos del indicador 3 (Porcentaje de emergencia de las nidadas
en la playa y el indicador 4 (Fertilidad de las nidadas en la playa)

Entrada: datos protocolo para el monitoreo ecológico de las playas de anidación de tortugas marinas SINAC
         en formato .xlsm
Salida: datos unificados
Fecha de creación: 8 de septiembre 2020
Fecha de modificación: 21 de septiembre 2020
Autora: Abelis
"

#Activar librerias
library (readxl) # definir tipos de variables
library(plyr) # Calculos estadisticos
library(reshape2) # Transformacion de marco de datos con función melt
library(ggplot2) # Vizualización de datos
library(janitor) #cambiar formato de fecha de excel a yyyy-mm-dd


#Limpiar escritorio de trabajo
rm(list = ls())

# Definir area de conservación, area silvestre protegida
##  Nombre de area de conservación
AC <- readline(prompt="Favor ingresar nombre de área de conservación: ")
ACOPAC  

## Nombre de area silvestre protegida
ASP <- readline(prompt="Favor ingresar nombre de área silvestre protegida: ")
RNVSPHPM  

#Definir especie
especie <- readline(prompt="Especie: ")
Lora 
#Verde

## Importar datos
  #con paquete readxl
df <- read_xlsx("PARQUE NACIONAL SANTA ROSA.xlsm",
                sheet = "F 2017 Lora Indicador 3",
                col_types = c("date", "date","date", rep("numeric",12)),
                range = "B7:P370", col_names = TRUE)

View(df)


## Definir variables y marco de datos
#fecha1<- na.omit(df$"Fecha exhumación")
fecha <- format(as.Date(df[1][!is.na(df[1])]))
año_ <-format(as.Date(fecha), "%Y") 
mes_ <-factor(format(as.Date(fecha), "%Y-%m") )
dia_ <-format(as.Date(fecha), "%Y-%m-%d") 
hora <- format(as.POSIXct(df$Hora,format="%Y-%m-%d %H:%M:%S"),format='%H:%M:%S') # Definir hora : formato %H:%M:%S ## Fijarse en hora original
hora <- hora[!is.na(hora)]
embrion_0 <- df[4][!is.na(df[4])]
embrion_1 <- df[5][!is.na(df[5])]
embrion_2 <- df[6][!is.na(df[6])]
embrion_3 <- df[7][!is.na(df[7])]
embrion_4 <- df[8][!is.na(df[8])]
cascaras <- df[9][!is.na(df[9])]
vivas <- df[10][!is.na(df[10])]
muertas <- df[11][!is.na(df[11])]
huevos <- df[12][!is.na(df[12])]
eclosión <- df[13][!is.na(df[13])]
emergencia <- df[14][!is.na(df[14])]
fertilidad <- df[15][!is.na(df[15])]


df_ind3_4 <- data.frame (AC, ASP, especie, fecha, año_ ,mes_,
                        embrion_0,
                        embrion_1, embrion_2, 
                        embrion_3, embrion_4, 
                        cascaras , vivas,
                        muertas, huevos ,
                        eclosión , emergencia, fertilidad )
View(df_ind3_4)
################################################################################

## Repetir para cada especie por temporada


#Calculo de promedios
prom_mensual_eclosion <- ddply(df_ind3_4, .(mes_) , summarize, mean=mean(eclosión))
prom_mensual_eclosion$ind<- "3"
prom_mensual_emergencia <- ddply(df_ind3_4, .(mes_) , summarize, mean=mean(emergencia))
prom_mensual_fertilidad <- ddply(df_ind3_4, .(mes_) , summarize, mean=mean(fertilidad))

# Marco de datos con promedios
df_emerg <- data.frame(prom_mensual_fertilidad,
                       prom_mensual_eclosion$mean,
                       prom_mensual_emergencia$mean
                       )
#Concatenar columnas 
df_emerg <- melt(df_emerg,id.vars="mes_") # melt: transformar marco de datos con mes_ como factor

View(df_emerg)


### Combinar datos por año
##agregar marco de datos a datos por año 
#df_2017_lora<-df1
#df_2017_verde<-df1

##concatenar datos por año
#df_17_lv<- rbind (df_2017_lora, df_2017_verde)

#View(df_17_lv)

##Visualizacion de datos

  # Diagrama de cajas
  ggplot(df_ind3_4, aes(x=mes_, y=eclosión)) + 
    geom_boxplot() +  theme_bw() +
    stat_summary(fun = "mean", color="red")+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    labs(x ="Fecha (AAAA-MM)", y = "Porcentaje de eclosión (%)") 


help("stat_summary")

  # Diagrama de barras
  ggplot(df_emerg, aes(x = mes_, y = value, fill = variable)) +
    geom_bar(position="dodge", stat = "identity") + theme_bw() +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    labs(x ="Fecha (AAAA-MM)", y = "Porcentaje promedio (%)")+
    scale_fill_discrete(name = "Indicador", labels = c("4: Fertilidad", "Eclosión", "3: Emergencia" ))
  
  
  
