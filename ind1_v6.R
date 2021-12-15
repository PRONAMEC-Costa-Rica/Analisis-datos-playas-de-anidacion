"Descripción: Este script forma parte del protocolo para el monitoreo ecol?gico de las playas de anidaci?n
de tortugas marinas. Este script unifica los datos del indicador 1 (?rea f?sica disponible como sitio de 
anidaci?n)

Creaci?n: 8 de septiembre 2020
?ltima modificaci?n: 11 de diciembre 2020
Autora: Abelis
"
#Activar librerias
library(openxlsx) #leer archivos grandes de excel
library(janitor) #cambio de formato de fecha de excel a yyyy-mm-dd
library(stringr) #utilizar funcion count
library(ggplot2) #visualizaci?n de datos


#Limpiar escritorio de trabajo
rm(list = ls())

# Definir area de conservaci?n, area silvestre protegida
##  Nombre de area de conservaci?n
AC <- readline(prompt="Favor ingresar nombre de ?rea de conservaci?n: ")
ACOPAC  

## Nombre de area silvestre protegida
ASP <- readline(prompt="Favor ingresar nombre de ?rea silvestre protegida: ")
RNVSPHPM  


## Importar datos
#libreria openxlsx
"Para el indicador 1, todos los datos se encuentran en la hoja Formulario Indicador 1 "

df<-read.xlsx("Datos.xlsm", 
              sheet = "Formulario Indicador 1")
View(df)

## Definir variables
    #Indices para selecci?n de columnas
      num_col <- length(df)
      i <- c(1, seq(4, num_col, 4)) #seleccionar columnas de mes
      j <- c(2, seq(5, num_col, 4)) #seleccionar columnas de fecha
      k <- c(3, 6, seq(10, num_col, 4)) #seleccionar columnas de temporada y area
      l <- c( seq(7, num_col, 4)) #seleccionar columnas de tasa de cambio
      

mes <- df[2:13, i]
fecha <- na.omit(excel_numeric_to_date(as.numeric(unlist(df[2:13, j]))))
mes_ <- factor(format(as.Date(fecha), "%Y-%m") )
dia_ <- format(as.Date(fecha), "%Y-%m-%d") 
area <- na.omit(as.numeric(unlist(df[2:13, k])))
prom_area <- na.omit(as.numeric(unlist(df[14, k])))
tasa_cambio <- na.omit(as.numeric(df[15, l]))

View(area)


#####
## Extraci?n de temporada_area
columnas <- colnames(df)
ind_prom_area <- !is.na(as.numeric(df[14, 1:length(colnames(df))]))

temporada_area <-list()
for (i in 1: length(colnames(df))){
  if (ind_prom_area[i] == TRUE){
    temporada_area = append(temporada_area , columnas[i])
  }}
  temporada_area <- unlist(temporada_area)

#####
## Extraci?n de temporada_tasa
columnas <- colnames(df)
ind_tasa_cambio <- !is.na(as.numeric(df[15, 1:length(colnames(df))]))

temporada_tasa <-list()
for (i in 1: length(colnames(df))){
  if (ind_tasa_cambio[i] == TRUE){
    temporada_tasa = append(temporada_tasa, columnas[i-1])
  }}
temporada_tasa <- unlist(temporada_tasa)

#####

c(NA, temporada_tasa)

#####
#Creaci?n de marco de datos
# Definir marco de datos completos
df_compl<-data.frame(Area_conservacion = AC,
                    Area_sil_proteg = ASP,
                    Fecha = fecha,
                    Area_muestreada = area
                    )
View(df_compl)


# Definir marco de datos con datos de area promedio
df_area<-data.frame(Area_conservacion = AC,
                Area_sil_proteg = ASP,
                Temporada = temporada_area,
                Prom_Area_sitio_anidacion = prom_area
                )
View(df_area)
#####

#####
#Creaci?n de marco de datos
# Definir marco de datos con datos de tasa de cambio
df_tasa<-data.frame(Area_conservacion= AC,
                    Area_sil_proteg= ASP,
                    Temporada = temporada_tasa,
                    Tasa_cambio = tasa_cambio
                    )
View(df_tasa)
#####


##Visualizacion de datos
#Datos de ?rea
#####
ggplot(na.omit(df_area), aes(x = Temporada, y = Prom_Area_sitio_anidacion)) +
  geom_point(stat = "identity") + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x ="Temporada (AAAA-AA)", y = "?rea disponible como sitio de anidaci?n (m2)") 

#####

#Datos de tasa
#####
ggplot(df_tasa, aes(x = Temporada, y = Tasa_cambio, group = 1)) +
  geom_line() + geom_point(stat = "identity") +  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Indicador 1: ?rea f?sica disponible como sitio de anidaci?n") +
  labs(x ="Temporada (AAAA-AA)", y = "Tasa de cambio del ?rea disponible \n como sitio de anidaci?n") 
