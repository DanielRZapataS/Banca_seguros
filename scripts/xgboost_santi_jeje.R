
library(data.table)
library(caret)
library(xgboost)

#' Imprime strings en la consola de R sin comillas
pprint <- function(...){cat(sprintf(...), "\n")}


#' Convierte una tabla en formato dgCMatrix
#'
#' @param tabla la tabla que va a ser cambiada de formato
#' @param variables_seleccionadas vector con todas las variables explicativas (features)
#' @param test_logical vector logico OBLIGATORIO donde se debe decir si la tabla que se va a transformar
#' es de tipo test (T) o no (F). Esto es importante porque si es de tipo test, el output no va a tener 
#' el label asignado. Si no es de tipo test, la tabla si lo va a incorporar para el entrenamiento del xgboost.
#' Si no se pone nada, la funcion retornara error.
#' @param target_nombre el nombre de la target en la tabla. Si test_logical es T, no importa.
#' 
#' @return la tabla en formato dgCMatrix lista para la ejecucion del xgboost
#'
#' @examples 
#' tabla_ejemplo <- as.dgcmatrix(tabla_ejemplo, 
#'                               variables_seleccionadas = c("var1", "var2"),
#'                               target_nombre = "variable_respuesta_num",
#'                               test_logical = F)
#' 
as.dgcmatrix <- function(tabla, variables_seleccionadas, target_nombre = "target_nombre", test_logical = NULL) {
  
  tabla <- copy(data.table(tabla))
  features <- tabla[, mget(variables_seleccionadas)]
  clases <- sapply(features, class)
  variables_continuas <- variables_seleccionadas[which(clases == "numeric" | clases == "integer")]
  variables_categoricas <- variables_seleccionadas[which(clases == "character" | clases == "factor")]
  
  if(is.null(test_logical)) {stop("Debe asignarle un valor logico a test_logical. Para mas informacion, consulte la documentacion de la funcion as.dgcmatrix en el scripts xgboost.R.")}
  
  if (length(variables_categoricas) > 0){
    ohe <- dummyVars( ~ ., data = tabla[, mget(variables_categoricas)])
    ohe <- as(data.matrix(predict(ohe, tabla[, mget(variables_categoricas)])), "dgCMatrix")
  }
  
  if (test_logical == F){
    if (target_nombre != "target_nombre") {
      tabla[, target := mget(target_nombre)]
    }
    target_train_dmatrix <- as(data.matrix(tabla$target), 'dgCMatrix')
  }
  
  if (length(variables_categoricas) > 0){
    tabla_dmatrix <- cbind(ohe, data.matrix(tabla[, mget(variables_continuas)]))
    rm(ohe)
  } else {
    tabla_dmatrix <- data.matrix(tabla[, mget(variables_continuas)])
  }
  
  gc()
  
  if (test_logical == T){
    dtabla <- xgb.DMatrix(tabla_dmatrix)
  } else {
    dtabla <- xgb.DMatrix(data = tabla_dmatrix, label = target_train_dmatrix)
  }
  
  gc()
  
  if (target_nombre != "target_nombre" & test_logical == F){
    tabla[, target := NULL]
  }
  
  return(dtabla)
}

#' Crea un modelo xgboost usando training y validation
#' Usa grilla de para tuneo de hiperparametros
#'
#' @param training Data de training. Debe estar en formato data.table o dataframe.
#' @param validation Data de validation. Debe estar en formato data.table o dataframe.
#' @param variables_seleccionadas vector con los nombres de las variables explicativas (features)
#' @param target_nombre nombre entre comillas de como aparece la columna de la target en las tablas.
#' @param grilla_max_depth vector con los valores candidatos a profundidad maxima de los arboles.
#' Default: c(8, 10)
#' @param grilla_eta vector con los valores candidatos a tasa de aprendizaje.
#' Default: c(0.05)
#' @param grilla_gamma vector con los valores candidatos a regularizacion.
#' Default: c(0)
#' @param grilla_alpha vector con los valores candidatos a regularizacion de Lasso.
#' Default: c(1)
#' @param grilla_early_stopping_round vector con los valores candidatos a cuantas iteraciones debe durar 
#' sin mejorar los resultados en validation hasta detenerse.
#' Default: c(30)
#' @param grilla_nrounds vector con los valores candidatos a maxima cantidad de iteraciones en una estimacion.
#' Default: c(500)
#' @param criterio criterio para medir el desempeno en development. Debe ser "auc" o "aucpr". 
#' Esta por default "aucpr".
#'
#' @return modelo tipo xgboost con los parametros tuneados.
#'
#' @examples
#' modelo_xgboost <- xgboost_bdb(training = training,
#'                               validation = validation,
#'                               variables_seleccionadas = c("var1", "var2"),
#'                               target_nombre = "variable_respuesta_num",
#'                               grilla_max_depth = c(10),
#'                               grilla_eta = c(0.05),
#'                               grilla_gamma = c(0),
#'                               grilla_alpha = c(1),
#'                               grilla_early_stopping_round = c(30),
#'                               grilla_nround = c(500),
#'                               criterio = "auc")
#' 
xgboost_bdb <- function(training, validation, variables_seleccionadas, target_nombre, grilla_max_depth = c(2, 3, 4), grilla_eta = c(0.05), grilla_gamma = c(0), grilla_alpha = c(1), grilla_early_stopping_round = c(30), grilla_nrounds = c(500), criterio = "aucpr") {
  
  training <- copy(data.table(training))
  validation <- copy(data.table(validation))
  
  dtrain <- as.dgcmatrix(tabla = training, 
                         variables_seleccionadas = variables_seleccionadas, 
                         target_nombre = target_nombre, 
                         test_logical = F)
  ddev <- as.dgcmatrix(tabla = validation, 
                       variables_seleccionadas = variables_seleccionadas, 
                       target_nombre = target_nombre, 
                       test_logical = F)
  training_validation <- copy(data.table(rbind(training, validation)))
  dtrain_dev <- as.dgcmatrix(training_validation, 
                             variables_seleccionadas = variables_seleccionadas, 
                             target_nombre = target_nombre, 
                             test_logical = F)
  rm(training_validation)
  
  watchlist <- list(train = dtrain, test = ddev)
  
  set.seed(1905)
  
  ######################################################################################
  
  #Tuning de hiperparametros
  
  searchGridSubCol <- expand.grid(max_depth = grilla_max_depth, # c(4, 6, 8, 10, 12, 14)
                                  eta = grilla_eta, #c(0.05, 0.1, 0.15, 0.2, 0.25) 
                                  gamma = grilla_gamma, # 0 normal. 5 si overfitting
                                  alpha = grilla_alpha, # 1
                                  early_stoping_round = grilla_early_stopping_round, # 30
                                  nrounds = grilla_nrounds) # 1000
  
  system.time(
    ErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
      
      current_max_depth <- parameterList[["max_depth"]]
      current_eta <- parameterList[["eta"]]
      current_gamma <- parameterList[["gamma"]]
      current_alpha <- parameterList[["alpha"]]
      current_early_stoping_round <- parameterList[["early_stoping_round"]]
      current_nrounds <- parameterList[["nrounds"]]
      
      xgb.parameters_tuning <- list(booster = "gbtree",
                                    objective = "binary:logistic",
                                    max.depth = current_max_depth,
                                    eta = current_eta,
                                    gamma = current_gamma,
                                    eval_metric = criterio, 
                                    alpha = current_alpha,
                                    early_stoping_round = current_early_stoping_round,
                                    nrounds = current_nrounds)
      
      model_tuning <- xgb.train(
        data = dtrain,
        nround = xgb.parameters_tuning$nrounds,
        params = xgb.parameters_tuning,
        early_stopping_rounds = xgb.parameters_tuning$early_stoping_round,
        verbose = 1,
        watchlist = watchlist)
      
      xvalidationScores <- as.data.frame(model_tuning$evaluation_log)
      xvalidationScores <- data.table(xvalidationScores)
      
      if (criterio == "aucpr"){
        xvalidationScores <- xvalidationScores[order(-test_aucpr)]
        develop <- xvalidationScores$test_aucpr[1]
        train <- xvalidationScores$train_aucpr[1]
      } else if (criterio == "auc") {
        xvalidationScores <- xvalidationScores[order(-test_auc)]
        develop <- xvalidationScores$test_auc[1]
        train <- xvalidationScores$train_auc[1]
      } else {
        stop("Revise el criterio seleccionado para la creacion del modelo xgboost. Actualmente solo se admite auc o aucpr.")
      }
      
      best_iteration <- model_tuning$best_iteration
      
      output <- return(c(develop, train, current_max_depth, current_eta, current_gamma, current_alpha, current_early_stoping_round, current_nrounds, best_iteration))}))
  
  output <- as.data.frame(t(ErrorsHyperparameters))
  varnames <- c("Develop", "Train", "max_depth", "eta", "gamma", "alpha", "early_stoping", "nrounds", "best_iteration")
  names(output) <- varnames
  
  output <- data.table(output)
  
  #Indicador de overfitting
  output[, Overfitting := ifelse(Train/Develop >= 1.03, 1, 0)]
  
  #Ordenar campos
  vars <- c("Overfitting", varnames)
  output <- output[, mget(vars)]
  
  #Primer criterio: que no tenga overfitting. Segundo: resultado en Develop. Tercero: resultado en Train
  output <- output[order(Overfitting, -Develop, -Train)]
  
  pprint("Estos son los mejores resultados:")
  print(head(output, 5))
  
  ##############################################################################################################
  
  xgb.parameters <- list(booster = "gbtree",
                         objective = "binary:logistic",
                         max.depth = output$max_depth[1],
                         eta = output$eta[1],
                         gamma = output$gamma[1],
                         eval_metric = criterio, 
                         alpha = output$alpha[1],
                         nrounds = output$best_iteration[1])
  
  model <- xgb.train(
    data = dtrain_dev,
    nround = xgb.parameters$nrounds,
    params = xgb.parameters,
    early_stopping_rounds = xgb.parameters$early_stoping_round,
    verbose = 1)
  
  pprint("Modelo creado con los hiperparametros tuneados.")
  
  return(model)
}

