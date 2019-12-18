#### paths ####
#results_path <- "propension_compra/cuentas_ahorro"
cancer_path <- os.path.join(model_scoring_path, "model_to_score_20191022_cancer")
pif_path <- os.path.join(model_scoring_path, "model_to_score_20191022_pif")
fraude_path <- os.path.join(model_scoring_path, "model_to_score_20191022_fraude")
file_name <- "pred_score"

##### load results ####
cancer_results <- get_path(cancer_path, file_name) %>% fread()
pif_results <- get_path(pif_path, file_name) %>% fread()
fraude_results <- get_path(fraude_path, file_name) %>% fread()

##### calculate decils #####
decil_cut <- function(dt, product) {
  dt[, decil := cut(
    pred,
    quantile(pred, probs = seq(0, 1, by = 0.1)),
    labels = paste0("decil_", seq(1, 10, by = 1)),
    include.lowest = TRUE
  )]
  
  names(dt)[3:4] <- paste0(c("pred_", "decil_"), product)
  return(dt)
}
cancer_results <-  decil_cut(cancer_results, "cancer")
pif_results <-  decil_cut(pif_results, "pif")
fraude_results <-  decil_cut(fraude_results, "fraude")

## all clients are the same ??#

intersect(cancer_results$definit, pif_results$definit) %>% length() == nrow(cancer_results)

intersect(cancer_results$definit, fraude_results$definit) %>% length() == nrow(cancer_results)

##### table results 
resultados <- merge(cancer_results, pif_results, by = c("definit", "periodo"))
resultados <- merge(resultados, fraude_results, by = c("definit", "periodo"))

rm(pif_results, fraude_results, cancer_results)
gc()
pred_var <- grep("pred", names(resultados), value = T)
aux <- resultados[, mget(c("definit", pred_var))] 
names(aux)[2:4] <- gsub("pred_", "", pred_var)
aux <- melt(aux, id.vars = c("definit"))
aux[, rank_product := rank(-value, ties.method = "random"), by = definit]
setkey(aux, definit )
head(aux)
aux[, rank_product := paste0("producto_", rank_product)]
aux <- dcast(aux, definit ~rank_product, value.var = "variable")
resultados <- merge(resultados, aux, by = "definit")
rm(aux)
gc()

# fwrite(
#   resultados,
#   paste0(
#     "//bdbemcfs/Banca_Seguros_Analytics/modelos_propension_compra_cuentas/resultados/resultados",
#     "resultados_producto_",
#     date_file(0),
#     ".csv"
#   )
# )

fwrite(resultados, os.path.join(results_path, paste0("resultados_producto_",
                                                     date_file(0),".csv")))

aux <- get_path(results_path, "resultados_producto_201910") %>% 
  fread()

#####product value ####
##Llamar crm para tener en cuenta la edad para cáncer
# crm <- get_path(staging_path, "crm")%>%
#   get_path(key_searcher = "crm_2019_07") %>% 
#   readRDS()
# crm <- crm[, .(crm_id, edad)]
# 
# resultados <- merge(x = resultados, y = crm,
#                     by.x = "definit", by.y = "crm_id", all.x = TRUE)
# 
# ##Se calcula el valor producto  
# product_value <- get_path(product_value_path, "product") %>% fread()
# product_value[, prima_persistencia:= prima_media*persistencia]
# 
# resultados[, prima_cancer := ifelse(edad >= 18 & edad <= 35,13300,
#                                 ifelse(edad >= 36 & edad <= 45,22800,
#                                 ifelse(edad >= 46 & edad <= 50,43500,
#                                 ifelse(edad >= 51 & edad <= 55,60100, 83700))))]
# 
# resultados[, cancer_value:= pred_cancer*prima_cancer*
#              product_value[producto == "cancer",persistencia]]
# resultados[, pif_value:= pred_pif*product_value[producto == "pif",
#                                                       prima_persistencia]]
# resultados[, fraude_value:= pred_fraude*product_value[producto == "fraude",
#                                                       prima_persistencia]]
# 
# value_var <- grep("_value", names(resultados), value = T)
# aux_value <- resultados[, mget(c("definit", value_var))]
# names(aux_value)[2:4] <- gsub("_value", "", value_var)
# aux_value <- melt(aux_value, id.vars = c("definit"))
# aux_value[, rank_product := rank(-value), by = definit]
# setkey(aux_value, definit )
# head(aux_value)
# aux_value[, rank_product := paste0("producto_value_", rank_product)]
# aux_value <- dcast(aux_value, definit ~rank_product, value.var = "variable")
# resultados <- merge(resultados, aux_value, by = "definit")
# head(resultados)
# 
# fwrite(resultados, "//bdbemcfs/Banca_Seguros_Analytics/modelos_propension_compra_cuentas/resultados/resultados/resultados_product_value_201908.csv")

# resultados_paula <- get.path("propension_compra/cuentas_ahorro/cargue_201907", "paula") %>% fread


### graficas ####


# # distribucion 
# aux <- resultados[, mget(c("definit", pred_var))]
# names(aux)[2:4] <- gsub("pred_", "", pred_var)
# aux <- melt(aux, id.vars = c("definit"))
# ggplot(aux, aes(x = value, fill = variable)) + 
#   geom_density(alpha = .3) +
#   labs(fill = c("Seguro"), 
#        title = "Distribución de probabilidades por producto",
#          xlab = "Probabilidad")
# # boxplot 
# 
# ggplot(aux, aes(x = variable, y = value, fill = variable)) + 
#   geom_boxplot() +
#   guides(fill = FALSE)+
#   labs(fill = c("Seguro"), 
#        title = "Distribución de probabilidades por producto",
#        xlab = "Seguro" ,
#        ylab = "Probabilidad")
# 
# ##### diagran venn #### 
# decil_grupos <- resultados[decil_cancer == "decil_10" |
#                                decil_fraude == "decil_10" |
#                                decil_pif == "decil_10", 
#                              .(definit, decil_cancer, decil_pif, decil_fraude)]
# decil_grupos[, decil_cancer := ifelse(decil_cancer == "decil_10", 1, 0)]
# decil_grupos[, decil_pif := ifelse(decil_pif == "decil_10", 1, 0)]
# decil_grupos[, decil_fraude := ifelse(decil_fraude == "decil_10", 1, 0)]
# # decil_grupos[, .N , by = .(decil_cancer, decil_pif, decil_fraude)]
# 
# source("http://www.bioconductor.org/biocLite.R")
# biocLite("limma")
# library(limma)
# a <- vennCounts(decil_grupos[, -"definit"])
# vennDiagram(a)
# 
# interseccion_3 <- decil_grupos[decil_cancer == 1 & decil_pif == 1 & decil_fraude == 1, definit]
# 
# interseccion_3 <- resultados[definit %in% interseccion_3]
# interseccion_3[, .N , by = producto_1][order(-N)]
# 
# interseccion_cancer_pif <- decil_grupos[decil_cancer == 1 & decil_pif == 1 & decil_fraude == 0, definit]
# interseccion_cancer_pif <- resultados[definit %in% interseccion_cancer_pif]
# interseccion_cancer_pif[, .N , by = producto_1][order(-N)]
# 
# 
# interseccion_cancer_fraude <- decil_grupos[decil_cancer == 1 & decil_pif == 0 & decil_fraude == 1, definit]
# interseccion_cancer_fraude <- resultados[definit %in% interseccion_cancer_fraude]
# interseccion_cancer_fraude[, .N , by = producto_1][order(-N)]
# 
# interseccion_pif_fraude <- decil_grupos[decil_cancer == 0 & decil_pif == 1 & decil_fraude == 1, definit]
# interseccion_pif_fraude <- resultados[definit %in% interseccion_pif_fraude]
# interseccion_pif_fraude[, .N , by = producto_1][order(-N)]
