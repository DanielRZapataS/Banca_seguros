#' calculate month 
#'
#' @param month_back : numbers of month lag of current day 
#'
#' @return path of file searched 
get_month <- function(month_back){
  current_month <- today() %>% format(., "%Y-%m-%d")
  previous_month <- floor_date(as.Date(current_month)  %m-% months(month_back), "month")
  # previous_month <- substr(previous_month, 1,7)
  # previous_month <- gsub("-", "_", previous_month)
  return(previous_month)
}

#' Looks for the mudule specified by trying a upper case extension and a lower case extension
#' @param file_name: Name of the module to be loaded (character)
#' @param file_encoding: Encoding of the script (character)
#' return: null (void)
import_module <- function(file_name, file_encoding = NULL){
  path <- os.path.join(scripts_path, file_name)
  if(!(ends_with(path, ".R") | ends_with(path, ".r"))){
    if(os.path.exists(paste0(path, ".R"))){
      path = paste0(path, ".R")
    } else if(os.path.exists(paste0(path, ".r"))){
      path = paste0(path, ".r")
    }
  }
  if(is.null(file_encoding)){
    source(path)
  }else{
    source(path, encoding = file_encoding)
  }
  
}


#' Splits the path provided in base and last child
#' @param slash: slash symbol to be used to build the path (character; "/" by default for
#' assuring multi-plataform compatibilities) (character)
#' @return a vector of length 2; the base and the last child (character)
os.path.split <- function(path, slash="/"){
  splitted_path = strsplit(path, slash)[[1]]
  file = tail(splitted_path, n=1)
  path = paste(head(splitted_path, length(splitted_path)-1), collapse = slash)
  return(c(path, file))
}

#' Splits the path provided in path and the extension
#' @param slash: slash symbol to be used to build the path (character; "/" by default for
#' assuring multi-plataform compatibilities) (character)
#' @return a vector of length 2; the whole path and the extension (character)
os.path.splitext <- function(path){
  splitted_path = strsplit(path, "\\.")[[1]]
  file = tail(splitted_path, n=1)
  path = paste(head(splitted_path, length(splitted_path)-1), collapse = ".")
  return(c(path, file))
}

#' Rename for convention
os.path.exists <- file.exists

#' upload files  
#'
#' @param path 
#' @param key_searcher 
#'
#' @return path of file searched 
get_path <- function(path, key_searcher){
  files <- list.files(path)
  searched_file <- grep(key_searcher, files, value = TRUE)
  searched_path <- paste(path, searched_file, sep = "/")
  return(searched_path)
}

#' Function which is intended for printing strings in the R console using the C syntax
pprint <- function(...){cat(sprintf(...), "\n")}

#' Canonizes a path given a kind of slash.
#' @param path: path to be canonized (character)
#' @param slash: slash symbol to be used to build the path (character; "/" by default for
#' assuring multi-plataform compatibilities) (character)
#' @return: the path canonized (character)
normalize_path = function(path, slash="/"){
  path = sub(pattern = "\\/\\/", replacement = slash, path)
  path = sub(pattern = "\\/", replacement = slash, path)
  path = sub(pattern = "\\\\", replacement = slash, path)
  return(path)
}

#' Builds a path from chunks
#' @params ...: All the chunks of paths to be loaded. 
#' @return: the path joined and normalized (character)
os.path.join <- function(...){
  normalize_path(file.path(...), slash = "/")
}


date_file <- function(x){
  x <- get_month(x)
  month_x <- lubridate::month(x)
  year_x <- lubridate::year(x)
  ifelse(nchar(month_x) == 1,
         return(paste0(year_x, "0", month_x)),
         return(paste0(year_x, month_x)) 
  )
}