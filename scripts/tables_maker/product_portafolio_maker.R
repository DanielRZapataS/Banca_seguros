

tenencia_productos_path <-
  grep("Tenencia_Productos", list.files(info_path, full.names = T), value = T)   

 # original tenure data
files_tenencia <- list.files(tenencia_productos_path, full.names = T) %>% file.info() %>% data.frame
files_tenencia$file_path <- row.names(files_tenencia)
files_tenencia <- data.table(files_tenencia)
files_tenencia[, file_name :=  substr(list.files(tenencia_productos_path), 10, 16)]
files_tenencia <- files_tenencia[isdir == F]

# staging tenure
files_staging <-
  data.frame(file_path = list.files(
    paste(staging_path, "tenencia_productos", sep = "/"),
    full.names = T
  ))
files_staging <- data.table(files_staging)
files_staging[, file_name :=  substr(list.files(paste(staging_path, "tenencia_productos", sep = "/")), 10, 16)]

# comparate

files_tenencia <- files_tenencia[file_name %!in% files_staging$file_name]
files_tenencia[, date := paste(file_name, "01", sep = "_")]
files_tenencia[, date := as.Date(date, "%Y_%m_%d")]
files_tenencia <- files_tenencia[date >= "2018-11-01"]
for (i in 1:nrow(files_tenencia)) {
  print(paste("Loading data of product portafolio from period", files_tenencia$file_name[i]))
  
  base <-
    fread(
      files_tenencia$file_path[i],
      colClasses = "character"
    )
  base[, periodo := files_tenencia$file_name[i]]
  names(base) <- tolower(names(base))
  var_int <-
    grep(paste(c(
      "periodo", "identificacion", pn_products
    ), collapse = "|"), names(base), value = T)
  
  base <- base[, mget(var_int)]
  base <- base[tipo_identificacion %!in% c("N")]
  base <- base[, lapply(.SD, function(x){ifelse(x == "", 0, x)})]
  productos <- names(base)[grepl("cant_*", names(base))]
  base[,(productos):=lapply(.SD, as.numeric), .SDcols = productos]
  base[, total_products := rowSums(base[, mget(productos)])]
  base <- base[total_products > 0]
  print(paste0("Saving staging table: staging_", files_tenencia$file_name[i]))
  
  saveRDS(base, file = paste(
    staging_path,
    "tenencia_productos",
    paste0("tenencia_", files_tenencia$file_name[i], ".rds"),
    sep = "/"
           ))
  
  rm(base)
  gc()
}
