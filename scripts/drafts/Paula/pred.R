install.packages("data.table")
install.packages("dplyr")
install.packages("pastecs")

library(data.table)
library(dplyr)
library(pastecs)
library(reshape2)

getwd()

ruta_cancer <- paste0(getwd(),"/propension_compra/model_to_score_20190717_cancer")
cancer <- list.files(ruta_cancer, ".csv")
cancer <- cancer[1]

cancer_pred <- fread(paste0(ruta_cancer, "/", cancer),
                     colClasses = "character",
                     na.strings = c("","NA"))

head(cancer_pred)

ruta_pif <- paste0(getwd(),"/propension_compra/model_to_score_20190717_pif")
pif <- list.files(ruta_pif, ".csv")
pif <- pif[1]

pif_pred <- fread(paste0(ruta_pif, "/", pif),
                     colClasses = "character",
                     na.strings = c("","NA"))

ruta_fraude <- paste0(getwd(),"/propension_compra/model_to_score_20190717_fraude")
fraude <- list.files(ruta_fraude, ".csv")
fraude <- fraude[1]

fraude_pred <- fread(paste0(ruta_fraude, "/", fraude),
                     colClasses = "character",
                     na.strings = c("","NA"))

all_pred <- merge(x = cancer_pred, y = pif_pred, by = "definit")
colnames(all_pred)[colnames(all_pred)=="pred.x"] <- "cancer_pred"
colnames(all_pred)[colnames(all_pred)=="pred.y"] <- "pif_pred"

all_pred <- merge(x = all_pred, y = fraude_pred, by = "definit")
colnames(all_pred)[colnames(all_pred)=="pred"] <- "fraude_pred"

all_pred[, decil_cancer := cut(as.numeric(cancer_pred),
                           quantile(as.numeric(all_pred$cancer_pred), probs = seq(0, 1, by = 0.1)),
                           labels = paste("decil", seq(1, 10, by = 1)),
                           include.lowest = TRUE)]

all_pred[, decil_pif := cut(as.numeric(pif_pred),
                           quantile(as.numeric(all_pred$pif_pred), probs = seq(0, 1, by = 0.1)),
                           labels = paste("decil", seq(1, 10, by = 1)),
                           include.lowest = TRUE)]

all_pred[, decil_fraude := cut(as.numeric(fraude_pred),
                           quantile(as.numeric(all_pred$fraude_pred), probs = seq(0, 1, by = 0.1)),
                           labels = paste("decil", seq(1, 10, by = 1)),
                           include.lowest = TRUE)]

all_pred[, c("periodo.y","periodo"):=NULL] 
colnames(all_pred)[colnames(all_pred)=="periodo.x"] <- "periodo"

fwrite(all_pred, "//bdbemcfs/Banca_Seguros_Analytics/modelo_analytics/propension_compra/
       cuentas_ahorro/cargue_201907/prediccionproduct_paula.csv")

## Para caracterizar la tabla resultante se pasa de de wide to long inicialmente

##Identificar las variables
pred_var <- grep("pred", names(all_pred), value = T)
##Tomar las variables identificadas
aux <- all_pred[, mget(c("definit", pred_var))]
##Cambiar los nombres de las variables quitandoles el pred
names(aux)[2:4] <- gsub("_pred", "", pred_var)
##Se aplica el melt para  poner las variables de productos en una sola columna
aux <- melt(aux, id.vars = c("definit"))
head(aux)
##Se ranquean los tres productos por cliente, y se pone el menos para que 1 sea para el mayor
aux[, rank_product := rank(-value), by = definit]
head(aux)



