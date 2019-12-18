##Construye la tabla de saldos de cuentas de ahorro y corrientes
print("Importando saldos de cuentas de ahorro")

#Imprime la ruta de la información de saldos
ruta_saldos <- paste0(info_path, "/Detalle_Productos")
print(paste("La ruta de los saldos de cuentas es", ruta_saldos))

#Busca los archivos de saldos en Información Centralizada
archivos_saldos <- list.files(ruta_saldos, full.names = T)
archivos_saldos <- grep("AHORROS", archivos_saldos, value = T)
fechas_archivos <- as.yearmon(substr(archivos_saldos, 88, 94), format = "%Y_%m")
archivos_saldos <- data.table(ruta_analytics = archivos_saldos, fecha = fechas_archivos)[order(-fecha)]

#Busca los archivos en la carpeta de staging
archivos_staging <- list.files(paste0(staging_path, "/cuentas_ahorros"), full.names = T)
fechas_archivos <- as.yearmon(substr(archivos_staging, 74, 80), format = "%Y_%m")
archivos_staging <- data.table(ruta_staging = archivos_staging, fecha = fechas_archivos)[order(-fecha)]

#Comparar archivos y encontrar aquellos que no están en staging
archivos_importar <- merge(archivos_saldos, archivos_staging, by = "fecha", all.x = T)
archivos_importar <- archivos_importar[is.na(ruta_staging)]
archivos_importar <- archivos_importar[order(-fecha)]

#Filtrar fecha de inicio
fecha_mas_antigua <- as.yearmon(as.Date(test_month)) - 6/12
archivos_importar <- archivos_importar[fecha >= fecha_mas_antigua]

#Importar diccionario de códigos de subproducto
#diccionario_subproducto <- fread("datos/diccionarios/codigo_subproducto.csv", colClasses =  "character")
diccionario_subproducto <- get_path(dictionary_path, "codigo_subproducto.csv") %>% 
  fread(colClasses =  "character")

#Importar archivos faltantes
for(i in 1:nrow(archivos_importar)) {
  print(paste(
    "Cargando archivo de cuentas de ahorros del mes",
    archivos_importar$fecha[i]
  ))
  saldos_ahorros <-
    fread(archivos_importar$ruta_analytics[i], colClasses = "character")
  
  print(">> Filtrando empresas")
  
  #Filtrar empresas
  saldos_ahorros <- saldos_ahorros[TIPO_ID %in% c("C", "E")]
  
  print(">> Creando el ID")
  
  #Crear ID
  saldos_ahorros[, NUM_ID := sub("^[0]+", "", NUM_ID)]
  saldos_ahorros[, ID := paste0(TIPO_ID, NUM_ID)]
  
  print(">> Formateando variables")
  
  #Formatear numéricas
  variables_numericas <-
    c(
      "CREDITOS_MES",
      "DEBITOS_MES",
      "PROMEDIO_MES",
      "SALDO_DISPONIBLE",
      "SALDO_INICIAL_MES",
      "TASA_INTERES",
      "VALOR_ULTIMO_DEPOSITO"
    )
  saldos_ahorros[, (variables_numericas) := lapply(.SD, as.numeric), .SDcols = variables_numericas]
  
  saldos_ahorros[, CODIGO_SUBPRODUCTO := sub("^[0]+", "", CODIGO_SUBPRODUCTO)]
  
  #Formatear fecha
  
  saldos_ahorros[, FECHA_APERTURA := as.Date(FECHA_APERTURA, format ="%d/%m/%Y")]
  #saldos_ahorros[, FECHA_APERTURA := convertir_fechas_crm(FECHA_APERTURA)]
  saldos_ahorros[, PERIODO := as.yearmon(PERIODO, format = "%Y%m")]
  
  #Eliminar variables innecesarias
  variables_a_eliminar <-
    c(
      "PRODUCTO",
      "TIPO_ID",
      "NUM_ID",
      "NUMERO_PRODUCTO",
      "FUENTE",
      "EMBARGO_INF",
      "MARCA_RECAUDO",
      "MARK_EXON_IVA",
      "NOVEDAD_EMBARGO",
      "PER_EXTRACTO",
      "TASA_INTERES",
      "TIPO_RETENCION",
      "TIPO_RETENC",
      "VALOR_ULTIMO_DEPOSITO"
    )
  saldos_ahorros[, (variables_a_eliminar) := NULL]
  
  #Pegar el diccionario de código subproducto
  saldos_ahorros <-
    merge(saldos_ahorros,
          diccionario_subproducto,
          by = "CODIGO_SUBPRODUCTO",
          all.x = T)
  
  #Filtrar cuentas por código de subproducto
  saldos_ahorros <- saldos_ahorros[CAMPANAS == "1"]
  saldos_ahorros[, CAMPANAS := NULL]
  saldos_ahorros[, APTOS_SEGUROS:= NULL]
  
  #Crear variable de antigüedad de la cuenta
  saldos_ahorros[, ANTIGUEDAD_CTA := Sys.Date() - FECHA_APERTURA]
  
  print(">> Seleccionando la cuenta principal para cada persona")
  
  saldos_ahorros[, cta_ppal := F]
  ids_duplicados <- saldos_ahorros[duplicated(ID)]$ID
  
  #Si sólo tiene una cuenta
  saldos_ahorros[ID %!in% ids_duplicados, cta_ppal := T]
  
  #Si tiene más cuentas se selecciona la de mayor saldo promedio
  saldos_ahorros[ID %in% ids_duplicados, cta_ppal := ifelse(PROMEDIO_MES == max(PROMEDIO_MES), T, cta_ppal), by = ID]
  
  saldos_ahorros <- saldos_ahorros[cta_ppal == T]
  
  #Si tiene más de una cuenta con el mismo saldo promedio, se selecciona la más reciente
  ids_duplicados <- saldos_ahorros[duplicated(ID)]$ID
  saldos_ahorros[ID %in% ids_duplicados, cta_ppal := F]
  saldos_ahorros[ID %in% ids_duplicados, cta_ppal := ifelse(FECHA_APERTURA == max(FECHA_APERTURA), T, cta_ppal), by = ID]
  
  saldos_ahorros <- saldos_ahorros[cta_ppal == T]
  
  #Si tiene más de una cuenta con la misma fecha de apertura, se selecciona cualquiera
  saldos_ahorros <- saldos_ahorros[!duplicated(ID)]
  
  #Eliminar variable artificial usada
  saldos_ahorros[, cta_ppal := NULL]
  saldos_ahorros[, FECHA_APERTURA := NULL]
  print(paste(
    ">>> La base tiene",
    nrow(saldos_ahorros),
    "registros de",
    uniqueN(saldos_ahorros$ID),
    "clientes."
  ))
  
  print(">> Pasando nombres de variables a minúscula")
  #Pasar a minúscula los nombres de las variables
  setnames(saldos_ahorros, names(saldos_ahorros), tolower(names(saldos_ahorros)))
  
  #Eliminar variables innecesarias
  saldos_ahorros[, promedio_semestre:= NULL]
  
  #Guardar la base
  print(paste0("Guardando archivo de cuentas de ahorros del mes",
               archivos_importar$file_name[i]))
  saveRDS(saldos_ahorros, file = paste(
    staging_path, "cuentas_ahorros",
            paste0("ahorros_",
              lubridate::year(as.Date(archivos_importar$fecha[i])),
              "_",
              ifelse(
                nchar(lubridate::month(as.Date(
                  archivos_importar$fecha[i]
                ))) == 1,
                paste0("0", lubridate::month(as.Date(
                  archivos_importar$fecha[i]
                ))),
                lubridate::month(as.Date(archivos_importar$fecha[i]))
              ),
              ".rds"
            )
          , sep = "/"))
  
}
