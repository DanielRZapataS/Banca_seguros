library(data.table)
library(dplyr)
library(pastecs)
library(openxlsx)

cargue_path <- paste0(getwd(),"/resultados/cargue_201907")
cargue_201907 <- list.files(cargue_path, ".csv")
cargue_201907 <- cargue_201907[1]
cargue_201907<-fread(paste0(cargue_path, "/", cargue_201907))
table(as.factor(cargue_201907$grupo))
table(as.factor(cargue_201907$oferta_tmk_codigo))


envios_path <- paste("//bdbemcfs/Banca_Seguros_Analytics/datos/original/envio_campañas_tmk")
envios_201908 <- list.files(envios_path, "201908.xlsx")
#envios_201908 <- as.data.table(read.xlsx(envios_201908))
#Error in read.xlsx.default(envios_201908) : File does not exist.
envios_201908 <- as.data.table(
  read.xlsx("//bdbemcfs/Banca_Seguros_Analytics/datos/original/envio_campañas_tmk/Envios_201908.xlsx"))

envios_201908_f <- as.data.table(envios_201908 %>%
                           filter(str_detect(Campaña, "STOCK")) %>%
                           filter(Oferta == 673 |Oferta == 676 |
                                    Oferta == 682))

cargue_envios_201908 <- merge(x = cargue_201907, y = envios_201908_f, 
                              by.x= c('definit', 'oferta_tmk_codigo'), 
                              by.y= c('Definit', 'Oferta'), all.x = T)

cargue_envios_201908[, enviados:= ifelse(is.na(Campaña)==F, 'enviados', 'no enviados')]

table(as.factor(cargue_envios_201908$enviados), by = cargue_envios_201908$grupo)
table(as.factor(cargue_envios_201908$enviados), by = cargue_envios_201908$oferta_tmk_codigo)

class(cargue_envios_201908)
head(cargue_envios_201908)

cargue_envios_201908[, .N, by  = .(oferta_tmk_codigo, grupo, enviados)][order(oferta_tmk_codigo, grupo)]
cargue_envios_201908[enviados == "enviados", .N, oferta_tmk_codigo]

