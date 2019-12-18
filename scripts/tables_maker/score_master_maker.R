##Script para combinar los RDS de CRM, Tenencia y cuentas de ahorro
#Solo para score


print("Importando CRM")

fecha_vector <-
  strsplit(as.character(get_month(1) ), "-")[[1]]
fecha_rezago <-
  strsplit(as.character(get_month(1) - 1 / 12), "-")[[1]]
fecha_yearmon <-
  as.yearmon(paste0(fecha_vector[1], fecha_vector[2]), format = "%Y%m")
crm <-
  readRDS(paste0(
    staging_path,
    "/crm/crm_",
    fecha_vector[1],
    "_",
    fecha_vector[2],
    ".rds"
  ))

#TODO Quitar variables en el CRM Maker
variables_a_eliminar <- c("crm_ciiu", "grupo_ciiu")
crm <- crm[, (variables_a_eliminar) := NULL]

uniqueN(crm$crm_nombre_estado_civil)
head(crm)
#Importar tenencia
print("Importando Tenencia de Productos")
tenencia <-
  readRDS(
    paste0(
      staging_path,
      "/tenencia_productos/tenencia_",
      fecha_vector[1],
      "_",
      fecha_vector[2],
      ".rds"
    )
  )

#Crear id para tenencia
tenencia[, id := paste0(tipo_identificacion, numero_identificacion)]
tenencia[, tipo_identificacion := NULL]
tenencia[, numero_identificacion := NULL]

tenencia[, periodo := NULL]

#Importar Cuentas de ahorro
print("Importando Cuentas de Ahorro")
ahorros <-
  readRDS(
    paste0(
      staging_path,
      "/cuentas_ahorros/ahorros_",
      fecha_vector[1],
      "_",
      fecha_vector[2],
      ".rds"
    )
  )
ahorros[, periodo := NULL]

#Importar el rezago de las cuentas de ahorro
ahorros_rezago <-
  readRDS(
    paste0(
      staging_path,
      "/cuentas_ahorros/ahorros_",
      fecha_rezago[1],
      "_",
      fecha_rezago[2],
      ".rds"
    )
  )
ahorros_rezago <-
  ahorros_rezago[, .(id,
                     creditos_mes,
                     debitos_mes,
                     promedio_mes,
                     saldo_disponible,
                     saldo_inicial_mes)]
setnames(ahorros_rezago,
         names(ahorros_rezago),
         paste0(names(ahorros_rezago), "_lag1"))

#Combinar bases
ahorros <-
  merge(
    ahorros,
    ahorros_rezago,
    by.x = "id",
    by.y = "id_lag1",
    all.x = T
  )
ahorros[is.na(ahorros)] <- 0

master <- merge(ahorros, crm, by.x = "id", by.y = "crm_id")
master <- merge(master, tenencia, by.x = "id", by.y = "id")

#Cambiar el nombre del id
setnames(master, "id", "definit")

#Guardar master
print(paste("Guardando archivo de master del mes", master$periodo[1]))
saveRDS(master, file = os.path.join(
  scoring_master_path,
  paste0(
    "score_master_",
    lubridate::year(as.Date(master$periodo[1])),
    "_",
    ifelse(
      nchar(lubridate::month(as.Date(master$periodo[1]))) == 1,
      paste0("0", lubridate::month(as.Date(master$periodo[1]))),
      lubridate::month(as.Date(master$periodo[1]))
    ),
    ".rds"
  )
))
