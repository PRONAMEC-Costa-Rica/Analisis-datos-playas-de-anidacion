"Descripción: Este script forma parte del protocolo para el monitoreo ecológico de las playas de anidación
de tortugas marinas. Este script unifica los datos del indicador 2 (número de nidos por especie).
Verificar que cada columna de nidos según la especie tenga la misma cantidad de datos. Agregar cero cuando
en el monitoreo no presenta nidos

Cambiar orden de variables a formato ind3

Creación: 8 de septiembre 2020
Última modificación: 21 de septiembre 2020
Autora: Abelis
"

#Activar librerias
library(readxl) #lectura archivos de excel con función read_xlsx
library(dplyr) # Calculos estadisticos
library(janitor) #cambio de formato de fecha de excel a yyyy-mm-dd
library(reshape2) # Transformacion de marco de datos con función melt
library(ggplot2) #visualización de datos

#####library(lubridate) # lectura de fecha en formato 



#Limpiar escritorio de trabajo
rm(list = ls())
# Definir area de conservación, area silvestre protegida
##  Nombre de area de conservación
AC <- readline(prompt="Favor ingresar nombre de área de conservación: ")
ACOPAC  

## Nombre de area silvestre protegida
ASP <- readline(prompt="Favor ingresar nombre de área silvestre protegida: ")
RNVSPHPM  

## Temporada
Temporada <- readline(prompt="Favor ingresar temporada de monitoreo: ")
2018-2019


## Importar datos
    "Definir casillas con datos con la extención range = ,
    definir temporada con la extención sheet ="


df <- read_xlsx("PARQUE NACIONAL SANTA ROSA.xlsm", 
               sheet = "F 2017 Indicador 2", range = "B5:L370", col_names = TRUE, na ="")

View(df)
colnames(df)

## Marco de datos completo
# Definir variables y marco de datos 
fecha <-  df$Fecha
año_ <-factor(format(as.Date(fecha), "%Y"))
mes_ <-factor(format(as.Date(fecha), "%Y-%m"))
nido_lora <- as.numeric(unlist(df[4]))
nido_verde <- as.numeric(unlist(df[5]))
nido_carey <- as.numeric(unlist(df[6]))
nido_baula <- as.numeric(unlist(df[7]))
saqueado_lora <- as.numeric(unlist(df[8]))
saqueado_verde <- as.numeric(unlist(df[9]))
saqueado_carey <- as.numeric(unlist(df[10]))
saqueado_baula <- as.numeric(unlist(df[11]))

#Marco de datos
df_ind2 <- data.frame(fecha,
                      nido_lora , nido_verde,
                      nido_carey, nido_baula, 
                      saqueado_lora, saqueado_verde, 
                      saqueado_carey , saqueado_baula
                      )
View(df_ind2)
                        
# Cambio de estructura de marco de datos

df_ind2_2 <- melt(df_ind2, id.vars="fecha", na.rm = TRUE) # melt: transformar marco de datos con fecha como factor
var_est_sp <- colsplit(df_ind2_2$variable, "_", names = c("estado", "especie"))
df_ind2_3 <- data.frame(Fecha = df_ind2_2$fecha,
                        Estado_nido = factor(var_est_sp$estado),
                        Especie = factor(var_est_sp$especie),
                        Num_nido = df_ind2_2$value)

View(df_ind2_3)

# Calculo de sumatorias

suma_nido_lora <- sum(filter(df_ind2_3, Especie == "lora" & Estado_nido == "nido")$Num_nido)
suma_nido_verde <- sum(filter(df_ind2_3, Especie == "verde" & Estado_nido == "nido")$Num_nido)
suma_nido_carey <- sum(filter(df_ind2_3, Especie == "carey" & Estado_nido == "nido")$Num_nido)
suma_nido_baula <- sum(filter(df_ind2_3, Especie == "baula" & Estado_nido == "nido")$Num_nido)
suma_saqueado_lora <- sum(filter(df_ind2_3, Especie == "lora" & Estado_nido == "saqueado")$Num_nido)
suma_saqueado_verde <- sum(filter(df_ind2_3, Especie == "verde" & Estado_nido == "saqueado")$Num_nido)
suma_saqueado_carey <- sum(filter(df_ind2_3, Especie == "carey" & Estado_nido == "saqueado")$Num_nido)
suma_saqueado_baula <- sum(filter(df_ind2_3, Especie == "baula" & Estado_nido == "saqueado")$Num_nido)

sumas_nidos <- c(suma_nido_lora, suma_nido_verde,
                 suma_nido_carey, suma_nido_baula, 
                 suma_saqueado_lora, suma_saqueado_verde,
                 suma_saqueado_carey, suma_saqueado_baula
                 )

sumas_nidos <- (data.frame (sumas_nidos))

#Definir variables
  columnas <-colnames(df)
  especie <- rep(c("Lora", " Verde", " Carey", " Baula"), 2)
  estado_nido <-c(rep("no_saqueado", 4), rep("saqueado",4))
  

#Definir marco de datos con sumatorias (indicador)
  ind_2<-data.frame(Temporada= Temporada,
                  Area_conservacion= AC,
                  Area_sil_proteg= ASP,
                  Especie = especie, 
                  Estado_nido = estado_nido,
                  Suma_nido = sumas_nidos
                  )

ind_17_18 <- ind_2
################################################################################

ind_18_19 <- ind_2

View(ind_17_18)

#concatenar datos por año
df_17_18<- rbind (ind_17_18, ind_18_19)

df_17_18<- ind_17_18


#########################################################################################################################
##Visualización de datos

ggplot(df_17_18, aes(  x = Temporada, y = sumas_nidos, color = Estado_nido, group= Especie, shape= Especie)) +
  geom_point() +  geom_line() +  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  ggtitle("Indicador 2: número de nidos por especie") +
  labs(x ="Temporada", y = "Número de nidos") +
  scale_color_discrete(name = "Estado del nido", labels = c("No saqueado", "Saqueado"))



##ver con scatter

