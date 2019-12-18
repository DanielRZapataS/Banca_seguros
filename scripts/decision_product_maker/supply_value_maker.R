
### cargue agosto
cargue <- get_path(cargue_path, "cargue_campañas_all_201908") %>% 
  fread()
### product value
resultados_value<- get_path(results_path, "resultados_producto_201908") %>% 
  fread()
resultados_value <- resultados_value[, .(definit, producto_value_1, producto_value_2, producto_value_3)]
### diccionario de codigo de productos ####
diccionario_productos <- get_path(dictionary_path, "diccionario_codigo_productos")

##### merge cargue y product value

cargue_value <- merge(x = cargue, y = resultados_value, 
                      by = "definit", all.x = TRUE)

##### definicion de oferta ##### 

# oferta 1
cargue_value[producto_value_1 == "fraude" , 
                  oferta_value_analytics := ifelse(viabilidad_fraude != 1,
                                             producto_value_2, 
                                             producto_value_1)]

cargue_value[producto_value_1 == "cancer" , 
                  oferta_value_analytics := ifelse( viabilidad_cancer != 1, 
                                              producto_value_2, 
                                              producto_value_1)]

cargue_value[producto_value_1 == "pif" , 
                  oferta_value_analytics := ifelse(viabilidad_pif != 1, 
                                             producto_value_2, 
                                             producto_value_1)]
#oferta 2
cargue_value[oferta_value_analytics == "fraude" , 
                  oferta_value_analytics := ifelse(viabilidad_fraude != 1,
                                             producto_value_3, 
                                             oferta_value_analytics)]

cargue_value[oferta_value_analytics == "cancer" , 
                  oferta_value_analytics := ifelse(viabilidad_cancer != 1, 
                                             producto_value_3, 
                                             oferta_value_analytics)]

cargue_value[oferta_value_analytics == "pif" , 
                  oferta_value_analytics := ifelse(viabilidad_pif != 1, 
                                             producto_value_3, 
                                             oferta_value_analytics)]

# oferta 3
cargue_value[oferta_value_analytics == "fraude" , 
                  oferta_value_analytics := ifelse(viabilidad_fraude != 1,
                                             "sin_producto", 
                                             oferta_value_analytics)]

cargue_value[oferta_value_analytics == "cancer" , 
                  oferta_value_analytics := ifelse(viabilidad_cancer != 1, 
                                             "sin_producto", 
                                             oferta_value_analytics)]

cargue_value[oferta_value_analytics == "pif" , 
                  oferta_value_analytics := ifelse(viabilidad_pif != 1, 
                                             "sin_producto", 
                                             oferta_value_analytics)]

cargue_value[oferta_value_analytics == "sin_producto"]

cargue_value[grupo == "tratamiento", .N, by = oferta_tmk]
cargue_value[grupo == "tratamiento", .N, by = .(oferta_tmk, oferta_value_analytics)][order(oferta_tmk)]

# #### envios
# envios <- as.data.table(read.xlsx("Y:/datos/original/envio_campañas_tmk/Envios_201908.xlsx"))
# envios <- envios[grep("STOCK", `Campaña`)]%>%
#   filter(Oferta == 673 | Oferta ==676 | Oferta == 682 )
# 
# names(envios) <- tolower(names(envios))
# 
# cargue_value <- merge(x = cargue_value, y = envios, 
#                       by = "definit", all.y = TRUE)
# 
# cargue_value[grupo == "tratamiento", .N, by = oferta_tmk]
# cargue_value[grupo == "tratamiento", .N, by = .(oferta_tmk, oferta_value_analytics)][order(oferta_tmk)]

####guardar archivo
fwrite(cargue_value[, .(definit, grupo, oferta_tmk,
                       oferta_value_analytics
)],
os.path.join(cargue_path, "cargue_value_201908.csv"))

