##Script para generar la tabla de resultados de campañas

##Construye la tabla de saldos de cuentas de ahorro y corrientes
print("Importando archivos de emisiones")

#Busca los archivos de emisiones en datos_emisiones
archivos_emisiones <-
  list.files(emisions_path, pattern = "emisiones", full.names = T)
fechas_archivos <-
  as.yearmon(substr(archivos_emisiones, 75, 81), format = "%Y_%m")
  #as.yearmon(str_extract(archivos_emisiones, '[0-9]+'), format = "%Y%m")
archivos_emisiones <-
  data.table(ruta_emisiones = archivos_emisiones, fecha = fechas_archivos)[order(-fecha)]

#Busca los archivos de envio_campañas_tmk en datos/original
archivos_envios <-
  list.files(paste(original_path, "envio_campañas_tmk", sep = "/"),
             full.names = T)
fechas_archivos <-
  as.yearmon(substr(archivos_envios, 77, 82), format = "%Y%m")
archivos_envios <-
  data.table(ruta_envios = archivos_envios, fecha = fechas_archivos)[order(-fecha)]

#Busca los archivos en la carpeta de staging

archivos_staging <-
  list.files(paste0(staging_path, "/ventas_cta"), full.names = T)
fechas_archivos <-
  as.yearmon(substr(archivos_staging, 68, 74), format = "%Y_%m")
archivos_staging <-
  data.table(ruta_staging = archivos_staging, fecha = fechas_archivos)[order(-fecha)]

#Combinar archivos de emisiones y envíos
archivos_combinados <-
  merge(archivos_emisiones,
        archivos_envios,
        by = "fecha",
        all.x = T)

#Comparar archivos y encontrar aquellos que no están en staging
archivos_importar <-
  merge(archivos_combinados,
        archivos_staging,
        by = "fecha",
        all.x = T)
archivos_importar <- archivos_importar[is.na(ruta_staging)]
archivos_importar <- archivos_importar[order(-fecha)]

if (nrow(archivos_importar) == 0){
  stop("Los archivos de datos ya están creados para las fechas establecidas")
}
  

#Importar archivos faltantes
for (i in 1:nrow(archivos_importar)) {
  print(paste("Cargando archivo de envíos del mes", archivos_importar$fecha[i]))
  envios <-
    as.data.table(read.xlsx(archivos_importar$ruta_envios[i]))
  envios <- envios[grep("STOCK", `Campaña`)]
  
  print(paste(
    "Cargando archivo de emisiones del mes",
    archivos_importar$fecha[i]
  ))
  emisiones <-
    fread(archivos_importar$ruta_emisiones[i], colClasses = "character")
  emisiones[, comprado := 1]
  emisiones[, Definit := paste0(TipoDocAsegurado, NumDocAsegurado)]
  
  print("Pegando las bases de envíos y emisiones")
  ventas <-
    merge(emisiones[, .(Definit, comprado, CodProdSeguro)], envios[, .(Definit, Oferta)], by = "Definit", all.y = T)
  ventas[is.na(comprado), comprado := 0]
  ventas[, CodProdSeguro := NULL]
  ventas[, periodo := archivos_importar$fecha[i]]
  
  print(">> Pasando nombres de variables a minúscula")
  #Pasar a minúscula los nombres de las variables
  setnames(ventas, names(ventas), tolower(names(ventas)))
  
  #Guardar la base
  print(paste(
    "Guardando archivo de ventas del mes",
    archivos_importar$fecha[i]
  ))
  file_name <-  paste0(
    "ventas_",
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
  saveRDS(ventas,
          file = paste(staging_path,
                        "ventas_cta", file_name, sep = "/"))
  
}


