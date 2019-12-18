options(prompt="Seguros> ", continue=" ") 
set.seed(123)
rm(list = ls())
options(scipen=999)
##### libraries ####
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,
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
info_path <- "//bdbemcfs/Analytics/Informacion_Centralizada/Data_Analytics"
root_path <- "///bdbemcfs/Banca_Seguros_Analytics"

original_path <- os.path.join(root_path, "datos/original")
staging_path <- os.path.join(root_path, "datos/staging")
dictionary_path <- os.path.join(root_path,"datos/diccionarios")
modeling_master_path <- "datos/master/master_modeling"
scoring_master_path <- "datos/master/master_scoring"
models_path <- "propension_compra"
plots_product <- "plots/propension_compra/cuentas_ahorro"
emisions_path <- paste(original_path, "emisiones_tmk", sep = "/")
results_path <- c("resultados")
gestion <- paste(original_path, "gestion_llamadas_tmk/historial", sep = "/")
cargue_path <- os.path.join(root_path, "DEDGA-BS_ctas_propension_compra/resultados/cargue")
envios_path <- paste0(original_path, "/envio_campañas_tmk")

##### make folder ####
insumos_opti <- os.path.join(root_path, "DEDGA-BS_ctas_propension_compra/resultados/insumo_optimizacion")
dir.create(insumos_opti)

months_process <-  sapply(4:1, date_file)
products <- c("cancer", "pif", "fraude")
seguros_analytics <- list()
k = months_process[1]
deciles_test_list <- list()
for(k in months_process) {
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
  deciles_test[, period := k]
  deciles_test_list[[k]] <- merge(deciles_test, bagging_results, by = c("product", "decil"))
  
   
}

deciles_test <- rbindlist(deciles_test_list)
deciles_test[, decil := factor(decil, levels = paste0("decil_", 1:10), ordered = T)]
i = months_process[1]
j = products[1]


for(i in months_process){
  for(j in products){
    aux <- deciles_test[product == j & period == i][order(decil)]
    aux[, decil_num := 1:10]
    p <- ggplot(data = aux, aes(x = decil_num, y = efectividad_test, ymin = confidence_5, ymax = confidence_95)) + 
      geom_point(color = "blue") + 
      geom_ribbon(alpha = .3) +
      scale_x_discrete(limits = 1:10) +
      labs(title = paste("Resultados modelo", j, "en", i),
           x = "Decil de probabilidad",
           y = "Efectividad")
    ggsave(paste0("plots/efectividades_esperadas_meses/", i, "_", j, ".png"), p)
  }
}



