library(data.table)
memory.limit(size = 25000) #Ayudar a la memoria
rm(list=ls()) #ambiente
gc() #ram

#El objetivo de este Script es generar los archivos de par?metros necesarios
#para correr el modelo de optimizaci?n de banca seguros. 

#El script genera una carpeta para cada caso donde en su interior est? el archivo
#de Xpress y el archivo de par?metros, que por defecto debe llamarse model.dat


#Datos necesarios para el inicio


#Ruta donde se crearan las carpetas
ruta <- "//bdbemcfs/Analytics/optimizacion_bancaseguros"
#modeloDeOpti
ruta_modelo_max_ventas <- paste0(ruta, "/", "modelo_maximizar_ventas.mos")
ruta_modelo_min_registros <- paste0(ruta, "/", "modelo_minimizar_registros.mos")
#datosALeer
datosOpti <- fread(paste0(ruta, "/", "datos_opti.csv"))
#maximo por deciles
maximo <- fread(paste0(ruta, "/", "maximo.csv"))
#Ventas Minimas por seguro
ventas_y_registros <- fread(paste0(ruta, "/", "ventas.csv"))


#Extraer periodos 
periodos <- unique(datosOpti$periodo)

#Eliminar NA
#ic5
datosOpti[is.na(ic_5_cancer), ic_5_cancer:= efectividad_esp_cancer]
datosOpti[is.na(ic_5_fraude), ic_5_fraude:= efectividad_esp_fraude]
datosOpti[is.na(ic_5_pif), ic_5_pif:= efectividad_esp_pif]
#ic95
datosOpti[is.na(ic_95_cancer), ic_95_cancer:= efectividad_esp_cancer]
datosOpti[is.na(ic_95_fraude), ic_95_fraude:= efectividad_esp_fraude]
datosOpti[is.na(ic_95_pif), ic_95_pif:= efectividad_esp_pif]

#Crear transformaciones de los parametros que se adapten al formato de Xpress

#1. N?mero de clientes
datosOpti[, nOpti := paste0("(\"",grupos, "\")",clientes)]

#2. Efectividad promedio para cada grupo segun el seguro
#2.1 Cancer
datosOpti[,k_cancer := paste0("(\"",grupos, "\" \"cancer\")", round(efectividad_esp_cancer,4))]
#2.2 Fraude
datosOpti[,k_fraude := paste0("(\"",grupos, "\" \"fraude\")", round(efectividad_esp_fraude,4))]
#2.3 PIF
datosOpti[,k_pif := paste0("(\"",grupos, "\" \"PIF\")", round(efectividad_esp_pif, 4))]

#3. Efectividad en el caso optimista, tomando los valores superiores del IC del 95%
#3.1 Cancer
datosOpti[,k_cancer_opt := paste0("(\"",grupos, "\" \"cancer\")", round(ic_95_cancer,4))]
#3.2Fraude
datosOpti[,k_fraude_opt := paste0("(\"",grupos, "\" \"fraude\")", round(ic_95_fraude,4))]
#3.3 PIF
datosOpti[,k_pif_opt := paste0("(\"",grupos, "\" \"PIF\")", round(ic_95_pif, 4))]

#3. Efectividad en el caso perimista, tomando los valores inferiores del IC del 95%
#3.1 Cancer
datosOpti[,k_cancer_pes := paste0("(\"",grupos, "\" \"cancer\")", round(ic_5_cancer,4))]
#3.2Fraude
datosOpti[,k_fraude_pes := paste0("(\"",grupos, "\" \"fraude\")", round(ic_5_fraude,4))]
#3.3 PIF
datosOpti[,k_pif_pes := paste0("(\"",grupos, "\" \"PIF\")", round(ic_5_pif, 4))]

#4. Decil. Identificar  a que deciles pertenece a un grupo para un seguro
#4.1 Cancer
datosOpti[, decil_cancer_opti := paste0("(\"",grupos, "\" \"cancer\")", as.numeric(gsub("decil_", "",decil_cancer)))]
#4.2 Fraude
datosOpti[, decil_fraude_opti := paste0("(\"",grupos, "\" \"fraude\")", as.numeric(gsub("decil_", "",decil_fraude)))]
#4.3 PIF
datosOpti[, decil_pif_opti := paste0("(\"",grupos, "\" \"PIF\")", as.numeric(gsub("decil_", "",decil_pif)))]

#5.maximo. Cuanto puedo gastar de un decil por seguro
maximo[, maximo_opti := paste0("(", decil, ")", maximo)]

#Creacion de carpetas y archivos

for(i in 1:length(periodos))
{
  datosPeriodo <- datosOpti[periodo == periodos[i]]
  datosVentas <- ventas_y_registros[periodo == periodos[i]]
  #Par?metro de n?mero de clientes
  n <- c("n:[",
         datosPeriodo[periodo == periodos[i]]$nOpti,
         "]")
  
  #Par?metro de efectividad de un grupo por seguro
  k<- c("k:[",
        datosPeriodo$k_cancer, 
        datosPeriodo$k_fraude, 
        datosPeriodo$k_pif, 
        "]")
  
  #Parametro de efectividad en escenario "Optimista"
  k_opt<- c("k:[",
            datosPeriodo$k_cancer_opt, 
            datosPeriodo$k_fraude_opt, 
            datosPeriodo$k_pif_opt, 
            "]")
  
  #Parametro de efectividad en escenario "Pesimista"
  k_pes<- c("k:[",
            datosPeriodo$k_cancer_pes, 
            datosPeriodo$k_fraude_pes, 
            datosPeriodo$k_pif_pes, 
            "]")
  
  #Parametro del decil para un seguro
  decil <- c("decil:[", 
             datosPeriodo$decil_cancer_opti, 
             datosPeriodo$decil_fraude_opti, 
             datosPeriodo$decil_pif_opti, 
             "]")
  
  #Parametro del numero minimo de ventas
  v <- c(paste0("v:",datosVentas$ventas[1])) 
  #Parametro del numero maximo de registros
  r <- c(paste0("r:",datosVentas$registros[1])) 
         
  
  limite_max <- c("maximo:[",
                  maximo$maximo_opti, 
                  "]")
  
  #Escenario optimista
  ruta_carpeta <- paste0(ruta, "/",periodos[i], "_opt" )
  dir.create(ruta_carpeta)
  archivo <- file(paste0(ruta_carpeta, "/", "model.dat"))
  writeLines(c(n, k_opt, decil, v, r, limite_max), archivo)
  file.copy(ruta_modelo_max_ventas, ruta_carpeta )
  file.copy(ruta_modelo_min_registros, ruta_carpeta )
  close(archivo)
  
  #Escenario pesimista
  ruta_carpeta <- paste0(ruta, "/",periodos[i], "_pes" )
  dir.create(ruta_carpeta)
  archivo <- file(paste0(ruta_carpeta, "/", "model.dat"))
  writeLines(c(n, k_pes, decil, v, r,  limite_max), archivo)
  file.copy(ruta_modelo_max_ventas, ruta_carpeta )
  file.copy(ruta_modelo_min_registros, ruta_carpeta )
  close(archivo)
  
  #Escenario promedio
  ruta_carpeta <- paste0(ruta, "/",periodos[i], "_prom" )
  dir.create(ruta_carpeta)
  archivo <- file(paste0(ruta_carpeta, "/", "model.dat"))
  writeLines(c(n, k, decil, v,r,  limite_max), archivo)
  file.copy(ruta_modelo_max_ventas, ruta_carpeta )
  file.copy(ruta_modelo_min_registros, ruta_carpeta )
  close(archivo)
  
}



