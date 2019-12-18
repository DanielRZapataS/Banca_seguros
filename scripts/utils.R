#'Convierte la variable "BIRTHDATE" de CRM a formato fecha
#'@param vector_birthdate Vector con las fechas formato CRM
#'@return Vector con las fechas formato Date
#'convertir_fechas_crm()
convertir_fechas_crm <- function(vector_birthdate) {
  mes <- substr(vector_birthdate, 3, 5)
  mes <- ifelse(mes == "", "UNKNOWN", mes)
  mes <- factor(mes, labels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", 
                                "10", "11", "12", "UNKNOWN"),
                levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", 
                           "OCT", "NOV", "DEC", "UNKNOWN"), ordered = TRUE)
  dia <- substr(vector_birthdate, 1, 2)
  anio <- substr(vector_birthdate, 6, 9)
  birthdate <- paste(anio, mes, dia, sep = "-")
  return(as.Date(birthdate))
}
