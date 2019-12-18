options(prompt = "Seguros> ", continue = " ")
set.seed(123)
rm(list = ls())
options(scipen = 999)
##### libraries ####
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(
  data.table,
  dplyr,
  lubridate,
  stringr,
  zoo,
  ggplot2,
  Matrix,
  stringi,
  xgboost,
  h2o,
  openxlsx,
  digest,
  VennDiagram,
  RColorBrewer,
  ompr,
  ompr.roi,
  ROI.plugin.symphony,
  tictoc
)
'%!in%' <<-  Negate('%in%')
source("scripts/files_tools.R")
source("scripts/utils.R")
source("scripts/common_reports.R")
numbers_months <- 0


##### paths ####
info_path <-
  "//bdbemcfs/Analytics/Informacion_Centralizada/Data_Analytics"
root_path <- "///bdbemcfs/Banca_Seguros_Analytics"

original_path <- os.path.join(root_path, "datos/original")
staging_path <- os.path.join(root_path, "datos/staging")
dictionary_path <- os.path.join(root_path, "datos/diccionarios")
modeling_master_path <- "datos/master/master_modeling"
scoring_master_path <- "datos/master/master_scoring"
models_path <- "propension_compra"
plots_product <- "plots/propension_compra/cuentas_ahorro"
emisions_path <- paste(original_path, "emisiones_tmk", sep = "/")
results_path <- c("resultados")
gestion <-
  paste(original_path, "gestion_llamadas_tmk/historial", sep = "/")
cargue_path <-
  os.path.join(root_path,
               "DEDGA-BS_ctas_propension_compra/resultados/cargue")
envios_path <- paste0(original_path, "/envio_campañas_tmk")

##### make folder ####
insumos_opti <-
  os.path.join(root_path,
               "DEDGA-BS_ctas_propension_compra/resultados/insumo_optimizacion")
dir.create(insumos_opti)

months_process <-  sapply(5:1, date_file)
products <- c("cancer", "pif", "fraude")
seguros_analytics <- list()
potencial_modelo_list <- list()
k = months_process[1]
for (k in months_process) {
  ##### selection Models ####
  # load test results
 
  models_path <-  "propension_compra/model_selection"
  file_name <- "pred_test"
  product_test <- list()
  i = products[1]
  for (i in products) {
    product_path <-
      get_path(models_path, paste(k, collapse = "|"))
    product_path <- grep(i, product_path, value = T)
    product_pred_test <- list()
    for (j in product_path) {
      product_pred_test[[j]] <- get_path(j, file_name) %>% fread()
    }
    product_pred_test <- rbindlist(product_pred_test)
    product_pred_test[, decil := cut(
      xgb_auc_pred,
      quantile(xgb_auc_pred, probs = seq(0, 1, by = 0.1)),
      labels = paste0("decil_", seq(1, 10, by = 1)),
      include.lowest = TRUE
    ), by = periodo]
    product_pred_test[, product := i]
    product_test[[i]] <- product_pred_test
  }
  product_test <- rbindlist(product_test)
  vars <-
    c(
      "definit",
      "periodo",
      "target",
      "xgb_auc_pred",
      "xgb_auc_pred_num",
      "decil",
      "product"
    )
  product_test <- product_test[, mget(vars)]
  rm(product_pred_test)
  gc()
  
  ##### decil table ####
  deciles_test <-
    product_test[, .(
      clientes_test = .N,
      efectividad_test = sum(target) / .N,
      score_max = max(xgb_auc_pred),
      score_min = min(xgb_auc_pred)
    ), by = .(product, decil)][order(product, decil)]
  deciles_test[, score_min_ajust := shift(score_max, -1, 0, "lead"), by = .(product)]
  
  ###### confidence interval ####
  set.seed(123)
  bagging_list <- list()
  for (i in 1:1000) {
    aux <-
      product_test[, sample(definit, .N * 0.1), by = .(product, decil)]
    # aux[, .N, by = .(product, decil)]
    aux1 <-
      product_test[definit %in% aux$V1, .(efectividad_test = sum(target) / .N, .N), by = .(product, decil)][order(product, decil)]
    aux1[, var := paste(product, decil, sep = "_")]
    bagging_list[[i]] <-
      dcast(aux1[, .(efectividad_test, var)], . ~ var, value.var = "efectividad_test")
  }
  
  bagging_results <- rbindlist(bagging_list, use.names = T)
  bagging_results[, . := NULL]
  bagging_results <-
    bagging_results[, lapply(.SD, function(x) {
      quantile(x, c(.05, .95))
    })]
  bagging_results[1, percentil := "confidence_5"]
  bagging_results[2, percentil := "confidence_95"]
  bagging_results <- melt(bagging_results)
  bagging_results[grep(pattern = "cancer", variable), product := "cancer"]
  bagging_results[grep(pattern = "fraude", variable), product := "fraude"]
  bagging_results[grep(pattern = "pif", variable), product := "pif"]
  bagging_results[, decil := variable]
  bagging_results[, decil := gsub(pattern = "cancer_|fraude_|pif_", "", decil)]
  bagging_results[, variable := NULL]
  bagging_results <-
    dcast(bagging_results, product + decil ~ percentil, value.var = "value")
  rm(bagging_list)
  gc()
  
  deciles_test <-
    merge(deciles_test, bagging_results, by = c("product", "decil"))
  
  ##### Load score tables ####
  models_path <-  "propension_compra/model_scoring"
  product_score <- list()
  file_name <- "pred_score"
  # i = products[1]
  for (i in products) {
    product_path <-
      get_path(models_path, paste(k, collapse = "|"))
    product_path <- grep(i, product_path, value = T)
    product_score_single <-
      get_path(product_path, file_name) %>% fread()
    product_score_single <-
      product_score_single[!duplicated(definit)]
    product_score_single[, product := i]
    product_score[[i]] <- product_score_single
  }
  product_score <- rbindlist(product_score)
  rm(product_score_single)
  gc()
  
  ##### asign decil and efect_esp
  for (i in 1:nrow(deciles_test)) {
    product_score[product == deciles_test$product[i] &
                    pred > deciles_test$score_min_ajust[i] &
                    pred <= deciles_test$score_max[i],
                  ':='(
                    decil = deciles_test$decil[i],
                    efectividad_esp = deciles_test$efectividad_test[i],
                    ic_5 = deciles_test$confidence_5[i],
                    ic_95 = deciles_test$confidence_95[i]
                  )]
  }
  product_score[is.na(decil), decil := "decil_10"]
  
  for (i in products) {
    product_score[decil == "decil_10" & product == i,
                  efectividad_esp := deciles_test[product == i &
                                                    decil == "decil_10", efectividad_test]]
  }
  
  
  product_score[, decil := factor(decil,
                                  levels = paste0("decil_", 1:10),
                                  ordered = T)]
  product_score_wide <-
    dcast(
      product_score[,-"periodo"],
      definit ~ product,
      value.var = c("pred", "decil", "efectividad_esp", "ic_5", "ic_95")
    )
  
  ##### Load potencial ####
  potencial <-
    "//bdbemcfs/Banca_Seguros_Analytics/datos/original/potencial_piloto_cuentas" %>%
    get_path(key_searcher = k) %>% read.xlsx() %>% data.table
  
  ### oferta diferente a 675 (pif creditos) y 677 (experiencia cuentas)
  #potencial <- potencial[OFERTA %!in% c(675, 677)]
  
  potencial[, tenencia_cancer := ifelse(is.na(TIENE_1059), 0, 1)]
  potencial[!is.na(TIENE_1057), tenencia_cancer := 1]
  potencial[!is.na(TIENE_615), tenencia_cancer := 1]
  
  #### base de tenencia y clientes potenciales #####
  potencial_modelo <-
    potencial[, .(
      definit = DEFINIT1,
      tenencia_pif_credito = TIENE_1048,
      tenencia_fraude = TIENE_1097,
      tenencia_pif = TIENE_1056,
      tenencia_cancer,
      edad = EDAD
    )]
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
  potencial_modelo[, .N]
  ###### viabilidad del producto #####
  potencial_modelo[, viabilidad_fraude :=
                     ifelse(tenencia_fraude == 1 |
                              edad > fraude_edad, 0, 1)]
  potencial_modelo[, viabilidad_cancer :=
                     ifelse(tenencia_cancer == 1 |
                              edad > cancer_edad, 0, 1)]
  potencial_modelo[, viabilidad_pif :=
                     ifelse(tenencia_pif == 1 |
                              edad > pif_edad, 0, 1)]
  
  potencial_modelo[, viabilidad_pif :=
                     ifelse(tenencia_pif_credito == 1 , 0, viabilidad_pif)]
  potencial_modelo[, periodo := k]
  potencial_modelo_list[[k]] <- potencial_modelo
  
  product_score_wide[, periodo := k]
  seguros_analytics[[k]] <-
    merge(product_score_wide,
          potencial_modelo,
          by = c("definit", "periodo"))
  
}

seguros_analytics <- rbindlist(seguros_analytics)
seguros_analytics[, periodo := as.numeric(periodo) +1]
parametros <- seguros_analytics[, .(
  clientes = .N,
  efectividad_esp_cancer = unique(efectividad_esp_cancer),
  ic_5_cancer = unique(ic_5_cancer),
  ic_95_cancer = unique(ic_95_cancer),
  efectividad_esp_fraude = unique(efectividad_esp_fraude),
  ic_5_fraude = unique(ic_5_fraude),
  ic_95_fraude = unique(ic_95_fraude),
  efectividad_esp_pif = unique(efectividad_esp_pif),
  ic_5_pif = unique(ic_5_pif),
  ic_95_pif = unique(ic_95_pif)
),
by = .(periodo, decil_cancer, decil_fraude, decil_pif)]
parametros[, grupos := gsub("decil_",
                            "",
                            paste(decil_cancer, decil_fraude, decil_pif, sep = "-"))]

fwrite(parametros, paste(insumos_opti, "datos_opti.csv", sep = "/"))

fwrite(
  seguros_analytics,
  paste(insumos_opti, "seguros_analytics_201908_201912.csv", sep = "/")
)






 