#Define i for each product: 1=cancer, 2=pif, 3=fraude
i = 1
##### model meta data ####
print("Making model's fold")
gc()
model_alias_modeling <- paste0("prueba_", today() %>% format(., "%Y%m%d"))
model_selection_path <- os.path.join(model_selection_path, products[i])  
model_type_modeling <- products[i]
# model product folder
model_alias_modeling <- os.path.join(model_selection_path, model_alias_modeling)
dir.create(model_alias_modeling)

# models folder
models_folder <- os.path.join(model_alias_modeling, "models_library")
dir.create(models_folder)

# plots folder 
plots_folder <- os.path.join(model_alias_modeling, "plots")
dir.create(plots_folder)

##### load data ####
print("Loading master tables ")
master_files <- get_path(modeling_master_path, "master")

data_list <- list()
for(j in 1:length(master_files)){
  data_list[[j]] <- readRDS(master_files[j])
}
data <-  rbindlist(data_list, fill= T)
print(paste("Loading data for", products[i]))

if(products[i] == "cancer"){data <- data[oferta == 673]}
if(products[i] == "pif"){data <- data[oferta == 676]}
if(products[i] == "fraude"){data <- data[oferta == 682]}


data <- data[periodo != "2019-04-01"] #?
data <- data[!is.na(target)]
data[, codigo_subproducto := NULL]
data[, target := factor(target)]
data[, oferta := NULL]

character_cols <-
  c(names(data)[sapply(data[, mget(names(data))], is.character)])

character_cols <- character_cols[character_cols != "definit"]
data[, (character_cols) := lapply(.SD, factor), .SDcols = character_cols]

# divinding master table
test <- data[periodo == test_month]
master <- data[periodo != test_month]
set.seed(123)
id <- sample(1:nrow(master), nrow(master)*0.65)
dev <- master[!id]
master <- master[id]

##### separating variabbles #### 

id_variables <- c("definit", "periodo", "target")
cols <- names(master)
cols <- cols[cols %!in% id_variables]
# dropping extra variables
cols <- cols[cols %!in% c("crm_grupo_ocupacion")]

categorical_cols <-
  c(cols[sapply(master[, mget(cols)], is.factor)])

numeric_cols <-
  c(cols[sapply(master[, mget(cols)], is.numeric)])

print("One hot encoding")
# one-hot encode the categorical features
final_cols <- c("target", categorical_cols, numeric_cols)

##### making xgb data ####  
master_dmatrix <- master[, mget(final_cols)]
master_dmatrix <-
  sparse.model.matrix(target ~ . - 1, data = master_dmatrix)

dev_dmatrix <- dev[, mget(final_cols)]
dev_dmatrix <- 
  sparse.model.matrix(target ~ . - 1, data = dev_dmatrix)

test_dmatrix <- test[, mget(final_cols)]
test_dmatrix <- 
  sparse.model.matrix(target ~ . - 1, data = test_dmatrix)

model_cols <- master_dmatrix@Dimnames[[2]]

model_cols <- tolower(model_cols)
model_cols <- 
  stri_trans_general(model_cols,"Latin-ASCII")
model_cols <- tm::removePunctuation(model_cols)
model_cols <- gsub(" ", "", model_cols, fixed = TRUE)

master_dmatrix@Dimnames[[2]] <- model_cols
dev_dmatrix@Dimnames[[2]] <- model_cols
test_dmatrix@Dimnames[[2]] <- model_cols

# separate target
target_train_dmatrix <-
  as(data.matrix(master$target), 'dgCMatrix')
target_dev_dmatrix <-
  as(data.matrix(dev$target), 'dgCMatrix')

dtrain <-
  xgb.DMatrix(data = master_dmatrix, label = target_train_dmatrix)
ddev <- xgb.DMatrix(data = dev_dmatrix, label = target_dev_dmatrix)

gc()
rm(target_dev_dmatrix, target_train_dmatrix)
watchlist <- list(train = dtrain, test = ddev)
# # set random seed for reproducibility
set.seed(1104)

cores <- parallel::detectCores() - 1
##### training grid tuning models ####
searchGridSubCol <- expand.grid(max_depth = c(3,4,5), # c(4, 6, 8, 10, 12, 14)
                                eta = c(0.05, 0.1, 0.3), #c(0.05, 0.1, 0.15, 0.2, 0.25) 
                                gamma = c(1, 5, 10), # 0 normal. 5 si overfitting
                                alpha = c(0, 0.01), # 1
                                early_stoping_round = c(30), # 30
                                nrounds = c(500)) # 1000

system.time(ErrorsHyperparameters <-
              apply(searchGridSubCol, 1, function(parameterList) {
                current_max_depth <- parameterList[["max_depth"]]
                current_eta <- parameterList[["eta"]]
                current_gamma <- parameterList[["gamma"]]
                current_alpha <- parameterList[["alpha"]]
                current_early_stoping_round <-
                  parameterList[["early_stoping_round"]]
                current_nrounds <- parameterList[["nrounds"]]
                
                xgb.parameters_tuning <- list(
                  booster = "gbtree",
                  objective = "binary:logistic",
                  max.depth = current_max_depth,
                  eta = current_eta,
                  gamma = current_gamma,
                  eval_metric = "auc",
                  alpha = current_alpha,
                  early_stoping_round = current_early_stoping_round,
                  nrounds = current_nrounds
                )
                
                model_tuning <- xgb.train(
                  data = dtrain,
                  nround = xgb.parameters_tuning$nrounds,
                  params = xgb.parameters_tuning,
                  early_stopping_rounds = xgb.parameters_tuning$early_stoping_round,
                  verbose = 1,
                  watchlist = watchlist,
                  nthread = cores
                )
                
                xvalidationScores <- as.data.frame(model_tuning$evaluation_log)
                xvalidationScores <- data.table(xvalidationScores)
                
                xvalidationScores <- xvalidationScores[order(-test_auc)]
                develop <- xvalidationScores$test_auc[1]
                train <- xvalidationScores$train_auc[1]
                
                
                best_iteration <- model_tuning$best_iteration
                
                output <-
                  return(
                    c(
                      develop,
                      train,
                      current_max_depth,
                      current_eta,
                      current_gamma,
                      current_alpha,
                      current_early_stoping_round,
                      current_nrounds,
                      best_iteration
                    )
                  )
              }))

output <- as.data.frame(t(ErrorsHyperparameters))
varnames <- c("Develop", "Train", "max_depth", "eta", "gamma", "alpha", "early_stoping", "nrounds", "best_iteration")
names(output) <- varnames

output <- data.table(output)

#Indicador de overfitting
output[, Overfitting := ifelse(Train/Develop >= 1.05, 1, 0)]

#Ordenar campos
vars <- c("Overfitting", varnames)
output <- output[, mget(vars)]

#Primer criterio: que no tenga overfitting. Segundo: resultado en Develop. Tercero: resultado en Train
output <- output[order(Overfitting, -Develop, -Train)]

pprint("Estos son los mejores resultados:")
print(head(output, 5))


xgb.parameters <- list(booster = "gbtree",
                       objective = "binary:logistic",
                       max.depth = output$max_depth[1],
                       eta = output$eta[1],
                       gamma = output$gamma[1],
                       eval_metric = "auc", 
                       alpha = output$alpha[1],
                       nrounds = output$best_iteration[1])

##### training xgboost model ####


# xgboost maximizing AUC of ROC 

print("Training xgboost model using ROC curve")


model_xgb_auc <- xgb.train(
  data = dtrain,
  nround = xgb.parameters$nrounds,
  params = xgb.parameters,
  early_stopping_rounds = xgb.parameters$early_stoping_round,
  verbose = 1 ,
  nthread = cores, 
  watchlist = watchlist)

xgb.save(model_xgb_auc, os.path.join(models_folder, "xgb_auc.model"))

data_id <- data.table(var = c("model_type",
                              "test_month"),
                      value = c(model_type_modeling,
                                test_month)
)

fwrite(data_id, os.path.join(model_alias_modeling, "model_id.csv"))

save(
  final_cols, 
  model_cols,
  master_dmatrix,
  dev_dmatrix,
  test_dmatrix,
  master,
  dev,
  test,
  file = os.path.join(model_alias_modeling, paste0(products[i], "_xgb_objects.RData"))
)


##### Predicciones ####
xgb_auc_preds_master <-  predict(model_xgb_auc, master_dmatrix)
xgb_auc_preds_dev <-  predict(model_xgb_auc, dev_dmatrix)
xgb_auc_preds_test <-  predict(model_xgb_auc, test_dmatrix)

# exportar probabilidades 

master[,':=' (xgb_auc_pred = xgb_auc_preds_master)]
master <- calculate_pred(master, "target", "xgb_auc_pred")

dev[,':=' (xgb_auc_pred = xgb_auc_preds_dev)]
dev <- calculate_pred(dev, "target", "xgb_auc_pred")

test[,':=' (xgb_auc_pred = xgb_auc_preds_test)]
test <- calculate_pred(test, "target", "xgb_auc_pred")

fwrite(master[, .(definit,
                  periodo,
                  target,
                  xgb_auc_pred,
                  xgb_auc_pred_num)],
       os.path.join(model_alias_modeling, "pred_train.csv"))

fwrite(dev[, .(definit,
               periodo,
               target,
               xgb_auc_pred,
               xgb_auc_pred_num)],
       os.path.join(model_alias_modeling, "pred_dev.csv"))

fwrite(test[, .(definit,
                periodo,
                target,
                xgb_auc_pred,
                xgb_auc_pred_num)],
       os.path.join(model_alias_modeling, "pred_test.csv"))


master[, target := as.numeric(target)]
master[, target := ifelse(target == 1, 0, 1)]

dev[, target := as.numeric(target)]
dev[, target := ifelse(target == 1, 0, 1)]

test[, target := as.numeric(target)]
test[, target := ifelse(target == 1, 0, 1)]
##### metrics xgb #####
metrics_xgb <-
  data.frame(train = c(
    Metrics::accuracy(master$target, master$xgb_auc_pred_num),
    Metrics::precision(master$target, master$xgb_auc_pred_num),
    Metrics::recall(master$target, master$xgb_auc_pred_num),
    Metrics::auc(master$target, master$xgb_auc_pred)
  ),
  dev = c(
    Metrics:: accuracy(dev$target, dev$xgb_auc_pred_num),
    Metrics::precision(dev$target, dev$xgb_auc_pred_num),
    Metrics::recall(dev$target, dev$xgb_auc_pred_num),
    Metrics::auc(dev$target, dev$xgb_auc_pred_num)
  ),
  test = c(
    Metrics::accuracy(test$target, test$xgb_auc_pred_num),
    Metrics::precision(test$target, test$xgb_auc_pred_num),
    Metrics::recall(test$target, test$xgb_auc_pred_num),
    Metrics::auc(test$target, test$xgb_auc_pred_num)
  )
  )
rownames(metrics_xgb) <- c("accuracy", "precision", "recall", "auc")
write.csv(metrics_xgb, os.path.join(model_alias_modeling, "metrics_xgb.csv"))

######   Uplift of models on test #####

uplift(
  true = test$target,
  prob = test$xgb_auc_pred,
  filepath = os.path.join(
    model_alias_modeling,
    paste0("xgboost", "_test_uplift.csv")
  ),
  primerCorte = 0.1,
  salto = 0.1
)

# Important variables 
importance_matrix <-
  xgb.importance(feature_names =  model_cols, model = model_xgb_auc)
fwrite(
  importance_matrix,
  os.path.join(model_alias_modeling, "xgb_auc_important_variables.csv")
)


#xgboost explainer
explainer_xgb_auc <- DALEX::explain(
  model_xgb_auc,
  data = dev_dmatrix,
  y = dev$target == 1,
  label = "xgboost auc"
)

##### residual analysis ####
resids_xgb_auc <- DALEX::model_performance(explainer_xgb_auc)

# create comparison plot of residuals for each model
p1 <- plot(resids_xgb_auc)
p2 <- plot(resids_xgb_auc, geom = "boxplot")

plot <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave(file = os.path.join(plots_folder,  "resids.png"),
       plot = plot)

vip_xgb_auc <-
  variable_importance(explainer_xgb_auc,
                      loss_function = loss_root_mean_square, n_sample = 1000)

##### variable importance plots #####
p1 <- plot(vip_xgb_auc,max_vars = 20, show_baseline = T)
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_vip_xgboost_auc.png")), plot = p1)
rm(p1)
# Gain
p1 <- xgb.ggplot.importance(importance_matrix, measure = "Gain", rel_to_first = FALSE, top_n = 20)
p1 <- p1 + ggplot2::ylab("Gain")
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_gain_xgboost_auc.png")), plot = p1)

# Cover
p1 <- xgb.ggplot.importance(importance_matrix, measure = "Cover", rel_to_first = FALSE, top_n = 20)
p1 <- p1 + ggplot2::ylab("Cover")
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_cover_xgboost_auc.png")), plot = p1)

# Frequency
p1 <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = FALSE, top_n = 20)
p1 <- p1 + ggplot2::ylab("Frequency")
ggsave(filename = os.path.join(plots_folder, paste0(products[i], "_frequency_xgboost_auc.png")), plot = p1)

rm(p1)

##### curvas AUC ####

##### AUC Validation y Test ####

xgb_auc_roc_master <- pROC::roc(response = master$target, predictor = master$xgb_auc_pred)
xgb_auc_roc_dev <-pROC::roc(response = dev$target, predictor = dev$xgb_auc_pred)
xgb_auc_roc_test <- pROC::roc(response = test$target, predictor = test$xgb_auc_pred)

rocs <- list("Train" = xgb_auc_roc_master,
             "Validación" = xgb_auc_roc_dev,
             "Test"= xgb_auc_roc_test)
breaks = seq(0, 1, 0.1)
legendTitel = "Modelos"
RocVals <- plyr::ldply(names(rocs), function(rocName) {
  if (class(rocs[[rocName]]) != "roc") {
    stop("Please provide roc object from pROC package")
  }
  data.frame(
    fpr = rev(rocs[[rocName]]$specificities),
    tpr = rev(rocs[[rocName]]$sensitivities),
    names = rep(rocName, length(rocs[[rocName]]$sensitivities)),
    stringAsFactors = T
  )
})
RocVals <- data.table(RocVals)
RocVals <- RocVals[sample(.N, 1000)]
AUC <- sapply(rocs, "[[", "auc")
aucs <- data.frame(AUC)
aucs$AUC <- round(aucs$AUC, 3)
aucs <- t(aucs)
tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
matrix <- tableGrob(aucs, theme=tt)

rocPlot <-
  ggplot(RocVals, aes(x = fpr, y = tpr, colour = names)) +
  geom_segment(aes(
    x = 0,
    y = 1,
    xend = 1,
    yend = 0
  ),
  alpha = 0.5,
  colour = "gray") +
  geom_step() +
  scale_x_reverse(name = "Tasa de falsos positivos  (1 - Especificidad)",
                  limits = c(1, 0),
                  breaks = breaks) +
  scale_y_continuous(name = "Tasa de verdadderos positivos (Sensitividad)",
                     limits = c(0, 1),
                     breaks = breaks) +
  theme_bw() +
  coord_equal() +
  labs(title = paste("Curvas ROC y AUC en validación y test para", products[i]))+
  guides(colour = guide_legend(legendTitel)) +
  theme(axis.ticks = element_line(color = "grey80"))

p1 <- gridExtra::grid.arrange(
  rocPlot,
  matrix,
  nrow = 2,
  as.table = TRUE,
  heights = c(3, 1)
)


ggsave(file = os.path.join(model_alias_modeling, paste0("validacion", "_auc_all.png")),
       plot = p1)


