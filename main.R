## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999)
# Change prompt
options(prompt="Seguros> ", continue=" ") 

if (!require("pacman")) install.packages("pacman")
# suppose your code needs tm, party, stringi, caret, e1071, randomforest and gbm
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
  tm,
  Metrics, 
  DALEX,
  gridExtra,
  Ckmeans.1d.dp 
)



h2o.init()

options(scipen = 999)
'%!in%' <<-  Negate('%in%')
source("scripts/files_tools.R")
source("scripts/utils.R")
source("scripts/common_reports.R")
source("scripts/xgboost_tuning.R")

info_path <- "//bdbemcfs/Analytics/Informacion_Centralizada/Data_Analytics"
root_path <- "///bdbemcfs/Banca_Seguros_Analytics"

original_path <- os.path.join(root_path, "datos/original")
staging_path <- os.path.join(root_path, "datos/staging")
dictionary_path <- os.path.join(root_path,"datos/diccionarios")
modeling_master_path <- "datos/master/master_modeling"
scoring_master_path <- "datos/master/master_scoring"
model_selection_path <- "propension_compra_2/model_selection"
# model_selection2_path <- "propension_compra_2/model_selection"
model_scoring_path <- "propension_compra_2/model_scoring"
plots_product <- "plots/propension_compra/cuentas_ahorro"
emisions_path <- paste(original_path, "emisiones_tmk", sep = "/")
envios_path <- paste(original_path, "envios_campañas_tmk")
cargue_path <- "resultados/cargue"
results_path <- "resultados/resultados"
product_value_path <- os.path.join(root_path, "datos/original/valor_producto")

pn_products <-
  c(
    "ahorro",
    "tarj",
    "nomina",
    "crediservice",
    "cdt",
    "libranza",
    "libre_destino",
    "vivienda",
    "ctas_corrientes",
    "vehiculos"
  )
products <- c("cancer", "pif", "fraude")

cancer <- c(673)
pif <- c(676)
fraude <- c(682)

test_month <- c("2019-10-01")
month_process <- date_file(0)
