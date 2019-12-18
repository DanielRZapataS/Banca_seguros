############################################################################
#INICIO PROYECTO
###########################################################################
#Instalaci?n paquetes y librer?as
    # install.packages("contrib.url")
    # install.packages("data.table")
    # install.packages("outliers")
    # install.packages("ggplot2")
    # install.packages("readr")
    # install.packages("plyr")
    # install.packages("dplyr")
    # install.packages("caTools")
    # install.packages("bitops")
    # install.packages("rprojroot")
    # install.packages("rmarkdown")
    # install.packages("lubridate")
    # install.packages("naniar")
    # install.packages("pastecs")
    # install.packages("hablar")
    # install.packages("mice")
    # install.packages("VIM")
    # install.packages("scales")
    # install.packages("RColorBrewer")
    # install.packages("gridExtra")
    # install.packages("ggridges")
    # install.packages("plotly")


#Librer?as 
library(data.table)
library(outliers)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(naniar)
library(pastecs)
#library(hablar)
library(mice)
library(VIM)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(ggridges)
library(plotly)


#Directorio y workspace
# setwd("C:/Users/jsepul6/Desktop/J/")
setwd("C:/Users/crios5/Documents/Proyecto")

getwd()

rm(list = ls())
gc()

options(scipen=999) # scientific notation
set.seed(1)

#############################################################
# BASE DE DATOS PERSONA NATURAL
#############################################################

#Especificaci?n nombre base de datos 
base_pn <- "CRM_PN_2019_01.csv"

#Leer base de datos
datos_pn<- fread(base_pn, colClasses = "character", na.strings=c("","NA"))   ## Lee vac?os como NA
names(datos_pn)<- tolower(names(datos_pn))                                  ## Cambiar nombres de las variables de may?sculas a min?sculas

#Diccionario
dicc_segmento <- fread("dic_segmento.csv", colClasses = "character", header = T)

#Creaci?n y modificaci?n de variables

#Cambiar formato de fechas
for(col in names(datos_pn)) set(datos_pn, i=which(datos_pn[[col]]==201901), j=col, value=20190131)
datos_pn[, periodo := as.Date(datos_pn$periodo,format ="%Y%m%d")]
datos_pn[, crm_fecha_nacimiento := as.Date(datos_pn$crm_fecha_nacimiento, format = "%d/%m/%Y")]
datos_pn[, crm_fecha_vinculacion_banco := as.Date(datos_pn$crm_fecha_vinculacion_banco, format = "%d/%m/%Y")]

datos_pn[, edad := round(interval(start = datos_pn$crm_fecha_nacimiento,
                                  end = datos_pn$periodo) /
                           duration(num = 1, units = "years"))]

datos_pn[, antiguedad := round(interval(start = datos_pn$crm_fecha_vinculacion_banco,
                                        end = periodo) /
                                 duration(num = 1, units = "months"))]

#Cambiar estrato 0 por NA
datos_pn$crm_estrato[datos_pn$crm_estrato == 0] <- NA


#Conversi?n de variables num?ricas
datos_pn[, c("crm_valor_activos", "crm_valor_ing_bru_mes", "crm_valor_egreso_mes", "crm_valor_pasivos")
         := datos_pn[, lapply(.SD, as.numeric), 
                     .SDcols = c("crm_valor_activos", "crm_valor_ing_bru_mes", "crm_valor_egreso_mes", "crm_valor_pasivos")]]   

#Creaci?n de los segmentos principales

#Creaci?n de diccionario con el nombre de los segmentos principales
dicc_segmento$segmento <- factor(dicc_segmento$segmento, ordered = FALSE)

#Creaci?n de data table que une la base de datos con la nueva variable "segmento"
dt2 <- merge(datos_pn, dicc_segmento, by= "crm_nombre_segmento")


#Especificar nombre de las variables y asignar "preferente plus" a las personas que se encuentran en el segmento 'preferente' y tienen el c?digo de subsegmento 35.
dt2$crm_codigo_subsegmento<-factor(dt2$crm_codigo_subsegmento)

dt2$segmento_final <-
  as.factor(ifelse(
    dt2$segmento == "Masivo",
    "Masivo",
    ifelse(
      dt2$segmento == "Micro",
      "Micro",
      ifelse(
        dt2$segmento == "Pj",
        "Pj",
        ifelse(
          dt2$segmento == "Premium",
          "Premium",
          ifelse(
            dt2$segmento == "Preferente" &
              is.na(dt2$crm_codigo_subsegmento) == TRUE ,
            "Preferente",
            ifelse(
              dt2$segmento == "Preferente" &
                dt2$crm_codigo_subsegmento == "35" ,
              "Preferente Plus",
              "NA"
            )
          )
        )
      )
    )
  ))

#Creaci?n data table con los segmentos relevantes para el estudio (i.e. No tener en cuenta los segmentos correspondientes a Persona Jur?dica (Pj))
dt3 <- dt2[segmento_final=="Masivo" | segmento_final== "Premium" | segmento_final== "Preferente" | segmento_final=="Preferente Plus" |segmento_final== "Micro"]

#Creaci?n subsegmento 
dt3[, subsegmento := ifelse(dt3$crm_codigo_subsegmento =="35", "Preferente Plus",
                            ifelse(dt3$rm_codigo_subsegmento == "40", "Otro", "NA"))]

#Creaci?n de segmentos t?cticos
dt3$tacticos<-as.factor(ifelse(dt3$edad<=13, "Infantil",
                               ifelse(dt3$edad>= 14 & dt3$edad <= 17, "Adolescente",
                                      ifelse(dt3$edad>= 18 & dt3$edad <= 25, "Joven",
                                             ifelse(dt3$edad>= 26 & dt3$edad <= 59, "Adulto", "Experiencia")))))


#Construcci?n de Indice de productos





##########################################################################
#BASE DE DATOS TENENCIA DE PRODUCTOS
##########################################################################

#Cargar base de datos
base_tenencia<- "TENENCIA_2019_01.csv"
datos_t<- fread(base_tenencia, colClasses = "character", na.strings=c("","NA"))
names(datos_t)<- tolower(names(datos_t))


#Creaci?n variables num?ricas
datos_t[, c("cant_tarj_credito", "cant_ctas_nomina", "cant_ctas_ahorro", "cant_crediservice",
            "cant_cdt", "cant_ordinario", "cant_libranza", "cant_libre_destino", "cant_otros",
            "cant_vehiculo", "cant_ctas_corriente", "cant_leasing", "cant_vivienda",
            "cant_fomento", "cant_microcredito", "cant_activo_pyme", "cant_constructor") 
        := datos_t[, lapply(.SD, as.numeric), 
                   .SDcols= c("cant_tarj_credito", "cant_ctas_nomina", "cant_ctas_ahorro", "cant_crediservice", "cant_cdt", 
                              "cant_ordinario", "cant_libranza", "cant_libre_destino", "cant_otros",
                              "cant_vehiculo", "cant_ctas_corriente", "cant_leasing", "cant_vivienda",
                              "cant_fomento", "cant_microcredito", "cant_activo_pyme", "cant_constructor")]]

#Creaci?n columnas monoproducto y multiproducto
datos_t$count <- rowSums(datos_t[, 4:20], na.rm = TRUE)
datos_t$monmulti <- as.factor(ifelse(datos_t$count==1, "Monoproducto", "Multiproducto"))

#Merge con base persona natural
setnames(dt3, "crm_numero_identificacion", "numero_identificacion")
dt <- merge(dt3, datos_t, by= "numero_identificacion")

#Cambiar nombre productos
setnames(dt, old = c('cant_tarj_credito', 'cant_ctas_nomina', 'cant_ctas_ahorro', 'cant_crediservice',
                     'cant_cdt', 'cant_ordinario', 'cant_libranza', 'cant_libre_destino', 'cant_otros',
                     'cant_vehiculo', 'cant_ctas_corriente', 'cant_leasing', 'cant_vivienda',
                     'cant_fomento', 'cant_microcredito', 'cant_activo_pyme', 'cant_constructor'),
         new = c('Tarjeta_Credito', 'Cuenta_Nomina', 'Cuenta_Ahorro', 'Crediservice', 'CDT', 'Ordinario',
                 'Libranza', 'Libredestino', 'Otros', 'Vehiculo', 'Cuenta_Corriente', 'Leasing', 'Vivienda',
                 'Fomento', 'Microcredito', 'Activo_Pyme', 'Constructor'))

#Cambiar g?nero "D" por NA
dt$crm_genero[dt$crm_genero == "D"] <- NA
dt3$crm_genero[dt3$crm_genero =="D"] <- NA

###################################################################################
# PARA ANALISIS CR?DITO ESTUDIANTIL
###################################################################################

#Leer tabla de variables de cr?dito (diccionario)
dicc_credito <- fread("CREDITOS_201901.csv", colClasses = "character", header = T)   ###De esta base nos interesa el c?digo de cr?dito

#Leer diccionario tipo de ID para realizar posteriormente el merge
dicc_tipo_id <- fread("SARC_TIPO_IDENTIFICACION.csv", colClasses = "character", header = T)    ###De este diccionario nos interesa unir la base de cr?dito con tipo de id

#Diccionario final cr?dito
dicc_credito_final<-left_join(dicc_credito, dicc_tipo_id, by=c('tipo_identificacion'))

#Merge de la base de datos grande con el diccionario final
dt_cred <- merge(dt, dicc_credito_final, by = c("numero_identificacion", "crm_tipo_identificacion"))


#Especificar nombre de las variables. Asignar c?digos 61, 152, 155 y 156 a "Crediestudiantil"
dt_cred$codigo_credito<-factor(dt_cred$codigo_credito)
dt_cred$Crediestudiantil<-as.factor(ifelse(dt_cred$codigo_credito == 61 | dt_cred$codigo_credito ==152 | dt_cred$codigo_credito ==155 | dt_cred$codigo_credito== 156, "Crediestudiantil", "Otros"))

positions <- c("Micro", "Masivo", "Preferente", "Preferente Plus", "Premium") 
 