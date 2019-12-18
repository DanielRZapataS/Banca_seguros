##Script para combinar los RDS de CRM, Tenencia y cuentas de ahorro

#Busca los archivos de ventas en staging
archivos_ventas <- list.files(paste0(staging_path, "/ventas_cta"), full.names = T)
fechas_archivos <- as.yearmon(substr(archivos_ventas, 68, 74), format = "%Y_%m")
archivos_ventas <- data.table(ruta_ventas = archivos_ventas, fecha = fechas_archivos)[order(-fecha)]

#Cuadrar fechas automáticamente
fechas_vector <- sort(unique(archivos_ventas$fecha) - 1/12)

#Busca los archivos en la carpeta master
archivos_master <- list.files(modeling_master_path, full.names = T)
fechas_archivos <- as.yearmon(substr(archivos_master, 37, 43), format = "%Y_%m")
archivos_master <- data.table(ruta = archivos_master, fecha = fechas_archivos)[order(-fecha)]

#Comparar archivos y encontrar aquellos que no están en staging
archivos_importar <- as.yearmon(setdiff(fechas_vector, archivos_master$fecha))
if(length(archivos_importar) == 0) stop("Los archivos de datos ya están creados para las fechas establecidas")


for(i in 1:length(archivos_importar)){
  
  print("Importando CRM")
  
  fecha_vector <- strsplit(as.character(as.Date(as.yearmon(archivos_importar[[i]]))), "-")[[1]]
  fecha_rezago <- strsplit(as.character(as.Date(as.yearmon(archivos_importar[[i]]) - 1/12)), "-")[[1]]
  fecha_futuro <- strsplit(as.character(as.Date(as.yearmon(archivos_importar[[i]]) + 1/12)), "-")[[1]]
  fecha_yearmon <- as.yearmon(paste0(fecha_vector[1], fecha_vector[2]), format = "%Y%m")
  crm <- readRDS(paste0(staging_path, "/crm/crm_", fecha_vector[1], "_", fecha_vector[2], ".rds"))
  
  #TODO Quitar variables en el CRM Maker
  variables_a_eliminar <- c("crm_ciiu", "grupo_ciiu")
  crm <- crm[, (variables_a_eliminar) := NULL]
  
  #Importar tenencia
  print("Importando Tenencia de Productos")
  tenencia <- readRDS(paste0(staging_path, "/tenencia_productos/tenencia_", fecha_vector[1], "_", fecha_vector[2], ".rds"))
  
  #Crear id para tenencia
  tenencia[, id := paste0(tipo_identificacion, numero_identificacion)]
  tenencia[, tipo_identificacion := NULL]
  tenencia[, numero_identificacion := NULL]
  
  tenencia[, periodo := NULL]
  
  #Importar Cuentas de ahorro
  print("Importando Cuentas de Ahorro")
  ahorros <- readRDS(paste0(staging_path, "/cuentas_ahorros/ahorros_", fecha_vector[1], "_", fecha_vector[2], ".rds"))
  ahorros[, periodo := NULL]
  
  #Importar el rezago de las cuentas de ahorro
  ahorros_rezago <- readRDS(paste0(staging_path, "/cuentas_ahorros/ahorros_", fecha_rezago[1], "_", fecha_rezago[2], ".rds"))
  ahorros_rezago <- ahorros_rezago[, .(id, creditos_mes, debitos_mes, promedio_mes, saldo_disponible, saldo_inicial_mes)]
  setnames(ahorros_rezago, names(ahorros_rezago), paste0(names(ahorros_rezago), "_lag1"))
  
  #Combinar bases
  ahorros <- merge(ahorros, ahorros_rezago, by.x = "id", by.y = "id_lag1", all.x = T)
  ahorros[is.na(ahorros)] <- 0
  
  master <- merge(ahorros, crm, by.x = "id", by.y = "crm_id")
  master <- merge(master, tenencia, by.x = "id", by.y = "id")
  
  #Cambiar el nombre del id
  setnames(master, "id", "definit")
  
  #Importar ventas
  print("Importando ventas de seguros")
  ventas <- readRDS(paste0(staging_path, "/ventas_cta/ventas_", fecha_futuro[1], "_", fecha_futuro[2], ".rds"))
  ventas[, periodo := NULL]
  colnames(ventas)[colnames(ventas)=="comprado"] <- "target"
  
  master <- merge(master, ventas, by = "definit")
  
  #Guardar master
  print(paste("Guardando archivo de master del mes", master$periodo[1]))
  saveRDS(master, file = os.path.join(
    modeling_master_path,
    paste0("master_", lubridate::year(as.Date(master$periodo[1])), "_", ifelse(nchar(lubridate::month(as.Date(master$periodo[1]))) == 1, paste0("0", lubridate::month(as.Date(master$periodo[1]))), lubridate::month(as.Date(master$periodo[1]))), ".rds")
  ))
}
