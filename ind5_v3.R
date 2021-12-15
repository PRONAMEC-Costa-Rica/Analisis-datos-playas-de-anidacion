"Descripción: Este script forma parte del protocolo para el monitoreo ecológico de las playas de anidación
de tortugas marinas. Este script unifica los datos del indicador 5 (Temperatura de incubación de las nidadas

Creación: 8 de septiembre 2020
Modificación: 21 de septiembre 2020
Autora: Abelis
"

library (readxl) # lectura archivos de excel con función read_xlsx
library(data.table)
#library(openxlsx) #leer archivos grandes de excel
library (readxl) #definir tipos de variables
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
especie <- readline(prompt="Favor ingresar nombre de especie: ")
Lora 

#Definir temporada
Temporada  <- readline(prompt="Favor ingresar temporada: ")
2017


## Importar datos

df <- read_xlsx("Datos.xlsm", 
                sheet = "F 2017 Verde Indicador 5", skip = 2)

View(df)


## Definir variables
#definir cantidad de nidos para definir número de columnas!

    #Indices para selección de columnas
      num_col_tot <- length(df)
      a<-seq(2,num_col_tot,4) #seleccionar columnas de fecha
      b<-seq(3,num_col_tot,4) #seleccionar columnas de temperatura
      num_col_datos <- sum(unlist(lapply(!is.na(as.numeric(df[3,b])), as.numeric)))
      
fecha <- na.omit(excel_numeric_to_date(as.numeric(unlist(df[3:102,a]))))
cod_tot <- na.omit(as.numeric(unlist(df[1,a])))
cod_datos <-cod_tot[1:num_col_datos]
temporada <- format(as.Date(fecha), "%Y") 
temperatura <-na.omit(as.numeric(unlist(df[3:102,b])))
fecha_datos <- fecha[1:length(temperatura)] 

View(fecha_datos)

length(temporada)

#Promedios
prom_temp_ind <- na.omit(as.numeric(unlist(df[101,b])))
prom_temp_total <- mean(prom_temp_ind)



# Definir marco de datos 
df1<-data.frame(Area_conservacion= AC,
                Area_sil_proteg= ASP,
                Temporada= Temporada,                 
                Fecha= fecha_datos,
                Temperatura= temperatura, 
                Especie = especie)

View(df1)

View(df1)

#vector con longitud de columnas de temperatura
len<- c()
for (i in a){
  len[i]<-length(df[i][!is.na(df[i])]) #número de filas
}
len<- len[!is.na(len)]


# Definir número de nido (código)
for (i in 1:len[1]){
  df1$Codigo[i]<- 1
for (i in len[1]+1:sum(len[2]))
  df1$Codigo[i]<- 2
for (i in ((sum(len[1:2])+1):(sum(len[1:3]))))
  df1$Codigo[i]<- 3
for (i in ((sum(len[1:3])+1):(sum(len[1:4]))))
  df1$Codigo[i]<- 4
}
View(df1)


# Definir especie
for (i in (1: length(df1$Temperatura)))
  df1$Especie[i]<- 'Lora'


View(df1)



##Visualizacion de datos

# Diagrama de cajas
ggplot(df_ind3_4, aes(x=mes_, y=eclosión)) + 
  geom_boxplot() +  theme_bw() +
  stat_summary(fun = "mean", color="red")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  labs(x ="Fecha (AAAA-MM)", y = "Porcentaje de eclosión (%)") 


