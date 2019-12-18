threshold = 5000

crm_path <-grep("CRM_Persona_Natural",
                list.files(info_path, full.names = T),
                value = T)

# Cargue de diccionarios

## ciuu
dicCiiu <- get_path(dictionary_path, "codigo_ciiu") %>%
  fread(colClasses = "character")
  # read.csv()%>%
  # as.data.table(colClasses = "character")

## segmento comercial
dicc_segmentos <- get_path(dictionary_path, "dic_segmento") %>% 
  fread(colClasses = "character", header = T)
dicc_segmentos[, segmento := factor(segmento)]

## Dane dictionary
dicDane <- get_path(dictionary_path, "BDEPT_CODIGO") %>% fread()

# original tenure data
files_crm <- list.files(crm_path, full.names = T) %>% file.info() %>% data.frame
files_crm$file_path <- row.names(files_crm)
files_crm <- data.table(files_crm)
files_crm[, file_name := substr(files_crm$file_path, 89, 95)]
files_crm <- files_crm[isdir == F]
files_crm <- files_crm[!(grep("INACTIVOS", list.files(crm_path)))]

# staging tenure
files_staging <- data.frame(file_path = list.files(paste(staging_path, "crm", sep = "/"),
                                                   full.names = T))
files_staging <- data.table(files_staging)
files_staging[, file_name := substr(list.files(paste(staging_path, "crm", sep = "/")), 5, 11) ]
#files_staging[, file_name :=  str_extract(gsub("_","", list.files(paste(staging_path, "crm", sep = "/"))), "[[:digit:]]+")]

# comparate

files_crm <- files_crm[file_name %!in% files_staging$file_name]
files_crm[, date := paste(file_name, "01", sep = "_")]
files_crm[, date := as.Date(date, "%Y_%m_%d")]
files_crm <- files_crm[date >= "2018-11-01"]

dicc_segmentos <- get_path(dictionary_path, "dic_segmento") %>% 
  fread(colClasses = "character", header = T)
dicc_segmentos[, segmento := factor(segmento)]

for(i in 1:nrow(files_crm)){
  print(paste("Loading data of crm from period", files_crm$file_name[i]))
  
  crm <-
    fread(
      files_crm$file_path[i],
      colClasses = "character"
    )
  
  names(crm) <- tolower(names(crm))
  
  crm[, periodo := paste0(periodo, "01")]
  crm[, periodo := as.Date(periodo, "%Y%m%d")]
  dates <- grep("fecha", names(crm), value = T)

  if (files_crm$file_name[i] %in% c("2018_11", "2018_12")) {
    crm[, (dates) := lapply(.SD, function(x) {
      x <-  as.Date(x, format = "%d/%m/%Y")
    }), .SDcols = dates]
  }
  if (files_crm$file_name[i] %!in% c("2018_11", "2018_12")) {
    crm[, (dates) := lapply(.SD, function(x) {
      x <-  as.Date(x, format = "%Y-%m-%d")
    }), .SDcols = dates]
  }
  
  # cleanning duplicates ids 
  print("cleanning duplicates ids")
  crm[, crm_id := paste0(crm_tipo_identificacion, crm_numero_identificacion)]
  base_duplicados <- crm[ crm_id %in% crm[duplicated(crm_id), crm_id]][order(-crm_id)]
  
  limpieza_duplicados <-
    base_duplicados[, .(crm_id, crm_fecha_vinculacion_banco)]
  limpieza_duplicados <-
    limpieza_duplicados[order(crm_id,-crm_fecha_vinculacion_banco)]
  limpieza_duplicados[, crm_fecha_vin_lag := shift(crm_fecha_vinculacion_banco, 1, 0, "lead"), by = crm_id]
  limpieza_duplicados <-
    limpieza_duplicados[crm_fecha_vin_lag != "1970-01-01"]
  limpieza_duplicados[, days := crm_fecha_vinculacion_banco - crm_fecha_vin_lag]
  id_duplicads <- limpieza_duplicados[days < 1, crm_id]
  
  crm_no_duplicados <- crm[crm_id %!in% limpieza_duplicados$crm_id ]
  crm_no_duplicados[ crm_id %in% crm_no_duplicados[duplicated(crm_id), crm_id]][order(-crm_id)]
  
  crm_registros_unicos <-
    merge(limpieza_duplicados[, crm_id, crm_fecha_vinculacion_banco],
          crm,
          by = c("crm_id", "crm_fecha_vinculacion_banco"))
  
  crm_registros_unicos <- crm_registros_unicos[crm_id %!in% id_duplicads]
  
  setcolorder(crm_no_duplicados, names(crm_registros_unicos))
  crm <-  rbindlist(list(crm_registros_unicos, crm_no_duplicados))
  rm(crm_no_duplicados, crm_registros_unicos)
  gc()
  
  print("Calculating timing variables")
  # age
  crm[, edad := round(interval(start = crm_fecha_nacimiento,
                               end = periodo) /
                        duration(num = 1, units = "years"))]
  # antiguedad
  crm[, antiguedad := round(interval(start = crm_fecha_vinculacion_banco,
                                     end = periodo) /
                              duration(num = 1, units = "months"))]
  
  fin_vars <- grep("valor|num", names(crm), value=T)
  fin_vars <-   fin_vars[-(grep("identificacion", fin_vars))]
  crm[, (fin_vars) := lapply(.SD, as.numeric), .SDcols = fin_vars]
  
 
  print("Cleaning bad hire_dt observation")
  # eliminar NAs de hire_dt
  crm <- crm[!is.na(crm_fecha_vinculacion_banco)]
  # eliminar fechas de antiguedad incongruentes
  crm <- crm[antiguedad >= -1]
  # eliinar registros con llave NAS
  crm <- crm[!is.na(crm_id)]
  
  # filling char var
  print("Filling character variables")
  crm[crm_genero %!in% c("M", "F"), crm_genero := "UNKNOW"]
  crm[crm_nombre_nivel_educativo == "", crm_nombre_nivel_educativo := "UNKNOW"]
  crm[crm_nombre_estado_civil == "", crm_nombre_estado_civil := "UNKNOW"]
  crm[crm_codigo_tipo_vivienda == "", crm_codigo_tipo_vivienda := "UNKNOW"]
  crm[crm_declara_renta == "", crm_declara_renta := "UNKNOW"]
  # codigo ciuu
  crm[is.na(crm_ciiu), crm_ciiu := 10]
  # codigo ocupacion
  crm[crm_nombre_ocupacion == "", crm_nombre_ocupacion := "UNKNOW"]
  # variables financieras
  print("Filling financial variables")
  finVars <- c("crm_valor_activos",
               "crm_valor_egreso_mes",
               "crm_valor_ing_bru_mes",
               "crm_valor_pasivos")
  # for(i in finVars){
  #   plot <- crm[, mget(i)]
  #   plot <- data.frame(plot)
  #   par(mfrow=c(1,2))
  #   hist(plot[, 1], main = i, xlab = "")
  #   boxplot(plot[, 1], main = i, xlab = "")
  # }
  #
  # cbind(names(summary(crm$crm_valor_ing_bru_mes)),
  #       crm[, lapply(.SD, summary),
  #             .SDcols = finVars])
  
  # los valors extremos positivos de las financieras llenarlos con los valors del 99%
  
  percentil_99 <-
    crm[, lapply(.SD, function(x)
      quantile(x, prob = 0.99, na.rm = TRUE)), .SDcols = finVars]
  crm[crm_valor_ing_bru_mes > percentil_99$crm_valor_ing_bru_mes, crm_valor_ing_bru_mes := percentil_99$crm_valor_ing_bru_mes]
  crm[crm_valor_activos > percentil_99$crm_valor_activos, crm_valor_activos := percentil_99$crm_valor_activos]
  crm[crm_valor_pasivos > percentil_99$crm_valor_pasivos, crm_valor_pasivos := percentil_99$crm_valor_pasivos]
  crm[crm_valor_egreso_mes > percentil_99$crm_valor_egreso_mes, crm_valor_egreso_mes := percentil_99$crm_valor_egreso_mes]
  # llenar valores muy bajos o NAs
  
  crm[, departamento := as.numeric(substr(crm_codigo_ciudad_ppal, 1, 2))]
  crm[is.na(departamento), departamento := 11]
  wrong_departamentos <-
    unique(crm$departamento)[which(unique(crm$departamento) %!in% dicDane$codigo)]
  crm[departamento %in% wrong_departamentos, departamento := 11]
  aux.income <-
    crm[, lapply(.SD, unique), by = .(departamento, crm_estrato, crm_id),
          .SDcols = finVars]
  aux.income <- aux.income[, .(
    median.ingresos = median(crm_valor_ing_bru_mes[crm_valor_ing_bru_mes > threshold], na.rm = T),
    median.activos = median(crm_valor_activos[crm_valor_activos > threshold], na.rm = T),
    median.pasivos = median(crm_valor_pasivos[crm_valor_pasivos > threshold], na.rm = T),
    median.egresos = median(crm_valor_egreso_mes[crm_valor_egreso_mes > threshold], na.rm = T)
  ),
  by = .(departamento, crm_estrato)]
  aux.income[is.na(aux.income)] <- 0
  crm <-
    merge(crm, aux.income, by = c("departamento", "crm_estrato"))
  crm[is.na(crm_valor_ing_bru_mes) |
          crm_valor_ing_bru_mes < threshold,
        crm_valor_ing_bru_mes := median.ingresos]
  crm[is.na(crm_valor_activos) | crm_valor_activos < threshold,
        crm_valor_activos := median.activos]
  crm[is.na(crm_valor_egreso_mes) | crm_valor_egreso_mes < threshold,
        crm_valor_egreso_mes := median.egresos]
  # pasivos si pueden ser 0
  crm[is.na(crm_valor_pasivos) | crm_valor_pasivos < 0 ,
        crm_valor_pasivos := median.pasivos]
  
  print("Merging with aditional dictionaries")
  
  # Nombres de departamentos 
  crm <- merge(crm, dicDane[, .(departamento = as.numeric(codigo),
                                    nombre)], by = "departamento", all.x = T)
  crm[, departamento := NULL]
  setnames(crm, "nombre", "departamento")
  
  # Grupos CIUU
  long <- 4
  for (j in min(crm[, .(nchar(crm_ciiu))]):(long - 1)) {
    crm[, l := nchar(crm_ciiu)]
    crm[, crm_ciiu := ifelse(l < long, paste0(0, crm_ciiu),
                                  crm_ciiu)]
  }
  crm[, l := NULL]
  crm[, crm_ciiu := substr(crm_ciiu, 1, 2)]
  
  crm <-
    merge(crm,
          dicCiiu[, .(grupo_ciiu = letra, codigo = dosDigitos)],
          by.x = "crm_ciiu",
          by.y = "codigo",
          all.x = T)
  # crm[, crm_ciiu := NULL]
  # setnames(crm, "grupo_ciiu", "crm_ciiu")
  
  crm <- merge(crm, dicc_segmentos, by = "crm_nombre_segmento", all.x = T)
  
  crm[, crm_estrato := paste0("estr_", crm_estrato)]
  crm[, crm_estrato := factor(crm_estrato, levels = paste0("estr_", 0:6))]
  crm <- crm[crm_tipo_identificacion %in% c("C", "E")]
  
  print("Selecting variables")
  var_int <-
    c(
      "crm_id"      ,
      "periodo",
      "crm_estrato",
      "crm_genero"     ,
      "crm_nombre_nivel_educativo"      ,
      "crm_nombre_estado_civil"      ,
      "crm_codigo_tipo_vivienda",
      "crm_nombre_ocupacion" ,
      "crm_grupo_ocupacion"      ,
      "crm_valor_activos"      ,
      "crm_valor_ing_bru_mes",
      "crm_valor_egreso_mes"      ,
      "crm_valor_pasivos" ,
      "crm_declara_renta"  ,
      "edad"       ,
      "antiguedad"      ,
      "departamento"     ,
      "crm_ciiu",
      "grupo_ciiu",
      "crm_grupo_segmento"
    )
  
  
  crm <- crm[, mget(var_int)]

  
  print(paste0("Saving staging table: crm_", files_crm$file_name[i]))
  saveRDS(crm, file = paste(
    staging_path,
    "crm",
    paste0("crm_", files_crm$file_name[i] , ".rds"), sep = "/"
  ))
  
  rm(crm)
  gc()
}
