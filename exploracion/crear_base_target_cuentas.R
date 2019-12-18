#Cargar base de ventas
base_ventas <- fread("datos/staging/ventas/base_target_cuentas.csv", colClasses = "character")
base_ventas[, periodo1 := as.yearmon(as.numeric(periodo1))]
setnames(base_ventas, names(base_ventas), tolower(names(base_ventas)))

#Cargar base de adquisiciones
base_emisiones <- fread("//bdbemcfs/Banca_Seguros_Analytics/datos_gestion_canales/emisiones_2019_05.csv", colClasses = "character")
base_emisiones <- base_emisiones[mes == "Mayo"]
base_emisiones[, id := paste0(TipoDocAsegurado, NumDocAsegurado)]
base_emisiones[, target := 1]
base_emisiones[, periodo1 := as.yearmon("may. 2019")]

bla <- merge(base_ventas, base_emisiones[, .(id, PRODUCTO_SOCIO, periodo1, target)], by.x = c("oferta", "definit", "periodo1"), by.y = c("PRODUCTO_SOCIO", "id", "periodo1"), all.x = T)
bla[periodo1 == "may. 2019" & target == 1, comprado := "1"]

bla[periodo1 == "may. 2019" & comprado == "1"]
bla[, target := NULL]

fwrite(bla, "datos/staging/ventas/base_target_cuentas.csv")
