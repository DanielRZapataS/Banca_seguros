i = 2
w = 201907

for(w in 201907:201911)
{
  ##### model meta data ####
  print("Making model's fold")
  gc()
  model_alias_modeling <-
    paste0("prueba_", w, "_", products[i])
  model_type_modeling <- products[i]
  # model product folder
  model_alias_modeling <- os.path.join(model_selection2_path, model_alias_modeling)
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
  
  # fixing periodo variable that daniel shit on it
  # setnames(data, "comprado", "target")
  data <- data[periodo != "2019-04-01"]
  data <- data[periodo < as.Date(paste0(round(w/100, 0), "-", w-1-round(w/100, 0)*100, "-01" ))]
  
  data <- data[!is.na(target)]
  data[, codigo_subproducto := NULL]
  data[, target := factor(target)]
  data[, oferta := NULL]
  
  character_cols <-
    c(names(data)[sapply(data[, mget(names(data))], is.character)])
  
  character_cols <- character_cols[character_cols != "definit"]
  data[, (character_cols) := lapply(.SD, factor), .SDcols = character_cols]
  
  test_month <- c((paste0(round(w/100, 0), "-", w-2-round(w/100, 0)*100, "-01" )))
  
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
  final_cols <- final_cols[final_cols != "target"]
  
  ##### training xgboost model ####
  
  # xgboost maximizing AUC of ROC 
  
  print("Training xgboost model using ROC curve")
  
  # clases <- master[, lapply(.SD, class)]
  # numericas <- colnames(clases)[clases == "numeric"]
  # cor(master[, mget(numericas)])
  
  model_xgb_auc <- xgboost_bdb(training = master,
                               validation = dev,
                               variables_seleccionadas = final_cols,
                               target_nombre = "target",
                               grilla_max_depth = c(3,4,5),
                               grilla_eta = c(0.05, 0.1, 0.3),
                               grilla_gamma = c(1, 5, 10),
                               grilla_alpha = c(0, 0.01),
                               grilla_early_stopping_round = c(30),
                               grilla_nrounds = c(500),
                               criterio = "auc")
  
  xgb.save(model_xgb_auc, os.path.join(models_folder, "xgb_auc.model"))
  
  data_id <- data.table(var = c("model_type",
                                "test_month"),
                        value = c(model_type_modeling,
                                  test_month))
  
  fwrite(data_id, os.path.join(model_alias_modeling, "model_id.csv"))
  
  #Create dmatrix
  master_dmatrix <- as.dgcmatrix(master, final_cols, "target", F)
  test_dmatrix <- as.dgcmatrix(test, final_cols, "target", F)
  dev_dmatrix <- as.dgcmatrix(dev, final_cols, "target", F)
  
  # test[, prob := predict(model_xgb_auc, test_dmatrix)]
  # dev[, prob := predict(model_xgb_auc, dtest)]
  
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
    xgb.importance(feature_names =  final_cols, model = model_xgb_auc)
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
  
  
  pROC::roc(response = test$target, predictor = test$prob)
  
  imp <- xgb.importance(names(dtest), model_xgb_auc)
  imp
}