##### make folder ####
dir.create(os.path.join(cargue_path, date_file(0)))

#### cargue resultados modelo ####
resultados <- get_path(results_path, key_searcher = paste0("resultados_producto_", month_process)) %>% 
  fread()

# resultados <- get_path(results_path, "cargue") %>% 
#   get_path(key_searcher = paste0("resultados_producto_", date_file(0))) %>% 
#   fread()

# resultados[, .N, by =decil_cancer]
# resultados[, .N, by =decil_fraude]
# resultados[, .N, by =decil_pif]

#### cargue clientes potencial de campañas #### 
potencial <- paste0(original_path,"/potencial_piloto_cuentas") %>% get_path( key_searcher = date_file(0) ) %>% read.xlsx() %>% data.table

### diccionario de codigo de productos ####
diccionario_productos <- get_path(dictionary_path, "diccionario_codigo_productos")

### filtro DUCC y tarjetas 
potencial <- potencial[is.na(OFERTA)]

### oferta diferente a 675 (pif creditos) y 677 (experiencia cuentas)
#potencial <- potencial[OFERTA %!in% c(675, 677)]

potencial[, tenencia_cancer := ifelse(is.na(TIENE_1059), 0, 1)]
potencial[!is.na(TIENE_1057), tenencia_cancer := 1]
potencial[!is.na(TIENE_615), tenencia_cancer := 1]

#### base de tenencia y clientes potenciales #####
potencial_modelo <-
  potencial[, .(definit = DEFINIT1, 
                tenencia_pif_credito = TIENE_1048,
                tenencia_fraude = TIENE_1097,
                tenencia_pif = TIENE_1056, 
                tenencia_cancer, 
                edad = EDAD)]
#binarias
potencial_modelo[, tenencia_pif_credito := ifelse(is.na(tenencia_pif_credito), 0, 1)]
potencial_modelo[, tenencia_fraude := ifelse(is.na(tenencia_fraude), 0, 1)]
potencial_modelo[, tenencia_pif := ifelse(is.na(tenencia_pif), 0, 1)]

#### filtro de edad de productos ####
pif_edad = 69
fraude_edad = 65
cancer_edad = 55

potencial_modelo <-
  potencial_modelo[edad <= max(c(pif_edad, fraude_edad, cancer_edad))]

##### filtros de duplicados ####
potencial_modelo[duplicated(definit)]
resultados[duplicated(definit)]

##### uniendo ambas tablas ####
seguros_analytics <- merge(resultados, potencial_modelo, by = "definit")
rm(resultados, potencial_modelo)
gc()
# seguros_analytics[, edad_igual := ifelse(edad.x == edad.y, 1, 0)]
# seguros_analytics[, .N, by = edad_igual]
#TODO fix edad variable, should be base on one of the sources 
# seguros_analytics[, edad := edad.y]

###### viabilidad del producto #####
seguros_analytics[, viabilidad_fraude := 
                    ifelse(tenencia_fraude == 1 |edad > fraude_edad,0, 1)]
seguros_analytics[, viabilidad_cancer := 
                    ifelse(tenencia_cancer == 1 |edad > cancer_edad,0, 1)]
seguros_analytics[, viabilidad_pif :=
                    ifelse(tenencia_pif == 1 | edad > pif_edad,0, 1)]

seguros_analytics[, viabilidad_pif :=
                    ifelse(tenencia_pif_credito == 1 ,0, viabilidad_pif)]

seguros_analytics[, posicion_cancer := rank(-pred_cancer)]
seguros_analytics[, posicion_pif := rank(-pred_pif)]
seguros_analytics[, posicion_fraude := rank(-pred_fraude)]


##### numero de registros al call por producto #### 
pif_registros = 42000
fraude_registros = 31000
cancer_registros = 31000

## orden de oferta o priorizacion

oferta_analytics <- list()
## oferta cancer
oferta_analytics[[1]] <- seguros_analytics[posicion_cancer <= cancer_registros / 2]
oferta_analytics[[1]][, oferta_analytics := "cancer"]
## oferta pif
client_exclude <- oferta_analytics[[1]][, definit]
oferta_analytics[[2]] <-
  seguros_analytics[order(posicion_pif)][definit %!in% client_exclude][1:(pif_registros / 2)]
oferta_analytics[[2]][, oferta_analytics := "pif"]
## oferta fraude
client_exclude <- c(oferta_analytics[[1]][, definit], oferta_analytics[[2]][, definit])
oferta_analytics[[3]] <-
  seguros_analytics[order(posicion_fraude)][definit %!in% client_exclude][1:(fraude_registros / 2)]
oferta_analytics[[3]][, oferta_analytics := "fraude"]

oferta_analytics <- rbindlist(oferta_analytics)
oferta_analytics[oferta_analytics == "cancer", decil_oferta := decil_cancer]
oferta_analytics[oferta_analytics == "fraude", decil_oferta := decil_fraude]
oferta_analytics[oferta_analytics == "pif", decil_oferta := decil_pif]
oferta_analytics[, .N, by = .(oferta_analytics, decil_oferta)]


### clientes aleatorios ####
clientes_no_modelo <- seguros_analytics$definit[seguros_analytics$definit %!in% oferta_analytics$definit]

which(clientes_no_modelo %in% oferta_analytics$definit)

set.seed(123)
random_fraude <- seguros_analytics[viabilidad_fraude == 1 & definit %in% clientes_no_modelo, sample(definit, fraude_registros/2)]


clientes_no_modelo <- clientes_no_modelo[clientes_no_modelo %!in% random_fraude]

random_cancer <- seguros_analytics[viabilidad_cancer == 1 & definit %in% clientes_no_modelo , sample(definit, cancer_registros/2)]

clientes_no_modelo <- clientes_no_modelo[clientes_no_modelo %!in% random_cancer]
random_pif <- seguros_analytics[viabilidad_pif == 1 & definit %in% clientes_no_modelo , sample(definit, pif_registros/2)]

oferta_aleatoria_cancer <- seguros_analytics[definit %in% c(random_cancer)]
oferta_aleatoria_cancer[, oferta_aleatoria := "cancer"]

oferta_aleatoria_fraude <- seguros_analytics[definit %in% c(random_fraude)]
oferta_aleatoria_fraude[, oferta_aleatoria := "fraude"]

oferta_aleatoria_pif <- seguros_analytics[definit %in% c(random_pif)]
oferta_aleatoria_pif[, oferta_aleatoria := "pif"]

oferta_final <- rbindlist(list(
  oferta_analytics,
  oferta_aleatoria_cancer,
  oferta_aleatoria_pif, 
  oferta_aleatoria_fraude
), fill = T)

oferta_final[duplicated(definit)]

oferta_final[, oferta_tmk := ifelse(is.na(oferta_aleatoria), oferta_analytics, oferta_aleatoria)]

oferta_final[, grupo := ifelse(is.na(oferta_aleatoria), "tratamiento", "control")]

oferta_final[, .N,  by = .(oferta_tmk, grupo)][order(oferta_tmk)]

oferta_final[oferta_tmk == "fraude", .N, by = .(decil_fraude, grupo)]
oferta_final[oferta_tmk == "cancer", .N, by = .(decil_cancer, grupo)]
oferta_final[oferta_tmk == "pif", .N, by = .(decil_pif, grupo)]

# View(oferta_final[oferta_tmk == "fraude", .(definit, pred_fraude, decil_fraude, grupo, posicion_oferta, oferta_analytics, oferta_aleatoria, oferta_tmk)][order(-pred_fraude)])

oferta_final[, oferta_tmk_codigo := factor(oferta_tmk, levels = c("pif", "cancer", "fraude"),
                                           labels = c("676", "673", "682"))]

ruta_resultados <- get_path("resultados", "cargue") %>% get_path(date_file(0))
oferta_final[duplicated(definit)]
fwrite(potencial, os.path.join(ruta_resultados, paste0("clientes_potencial_", date_file(0),".csv")))
fwrite(seguros_analytics, os.path.join(ruta_resultados, paste0("clientes_potencial_modelo_", date_file(0), ".csv")))
fwrite(oferta_final, os.path.join(ruta_resultados, paste0("cargue_campañas_all_", date_file(0),".csv")))


# oferta_final <- oferta_final[, .(definit, oferta_tmk_codigo, grupo)]

oferta_final_random <- oferta_final[sample(nrow(oferta_final)), .(definit, oferta_tmk_codigo, grupo)]

fwrite(oferta_final_random, os.path.join(ruta_resultados, paste0("cargue_campañas_",date_file(0) ,".csv")))


oferta_final_random <-  os.path.join(ruta_resultados, paste0("cargue_campañas_",date_file(0) ,".csv")) %>% fread()


oferta_final_random[, .N, by = .(oferta_tmk_codigo, grupo)]
oferta_final_random[duplicated(definit)]

nrow(oferta_final_random) == uniqueN(oferta_final_random$definit)

# 
