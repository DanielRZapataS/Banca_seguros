i = 1
model_to_use <- paste0(products[i],"/prueba_20191217")
score_month <- get_month(1)

model_to_use <- os.path.join(model_selection_path, model_to_use)

##### model meta data ####
print("Making model's fold")
gc()
model_alias_scoring <-
  paste0("model_to_score_", today() %>% format(., "%Y%m%d"))
model_scoring_path <- os.path.join(model_scoring_path, products[i])
model_type_scoring <- products[i]
# model product folder
model_alias_scoring <- os.path.join(model_scoring_path, model_alias_scoring)
dir.create(model_alias_scoring)

# models folder
models_folder <- os.path.join(model_alias_scoring, "models_library")
dir.create(models_folder)


##### load data ####
print("Loading master tables")
master_files <- get_path(modeling_master_path, "master")
date_score <- lubridate::month(score_month)
date_score <-
  ifelse(nchar(date_score) == 1, paste0("0", date_score), date_score)
date_score <- paste(lubridate::year(score_month),
                     date_score,
                     sep = "_")
# master_files <- master_files[-c(grep(date_score, x = master_files))]
data_list <- list()
for(j in 1:length(master_files)){
  data_list[[j]] <- readRDS(master_files[j])
}
master <-  rbindlist(data_list, fill= T)
rm(data_list)
gc()

print(paste("Loading master for", products[i]))
if(products[i] == "cancer"){master <- master[oferta == 673]}
if(products[i] == "pif"){master <- master[oferta == 676]}
if(products[i] == "fraude"){master <- master[oferta == 682]}

# fixing periodo variable that daniel shit on it
# setnames(master, "comprado", "target")
# master <- master[!is.na(target)]
master[, codigo_subproducto := NULL]
master[, target := factor(target)]
master[, oferta := NULL]

character_cols <-
  c(names(master)[sapply(master[, mget(names(master))], is.character)])

character_cols <- character_cols[character_cols != "definit"]
master[, (character_cols) := lapply(.SD, factor), .SDcols = character_cols]

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

model_cols <- master_dmatrix@Dimnames[[2]]

model_cols <- tolower(model_cols)
model_cols <- 
  stri_trans_general(model_cols,"Latin-ASCII")
model_cols <- tm::removePunctuation(model_cols)
model_cols <- gsub(" ", "", model_cols, fixed = TRUE)

master_dmatrix@Dimnames[[2]] <- model_cols



# data_id <- data.table(var = c("model_type",
#                               "test_month"),
#                       value = c(model_type_modeling,
#                                 test_month)
 # )
# 
# fwrite(data_id, os.path.join(model_alias_modeling, "model_id.csv"))



#### scoring ####
print("Load table to score")

master_files <- get_path(scoring_master_path, "master")
date_score <- lubridate::month(score_month)
date_score <-
  ifelse(nchar(date_score) == 1, paste0("0", date_score), date_score)
date_score <- paste(lubridate::year(score_month),
                    date_score,
                    sep = "_")
master_files <- master_files[c(grep(date_score, x = master_files))]
data_list <- list()
for(j in 1:length(master_files)){
  data_list[[j]] <- readRDS(master_files[j])
}
score_table <-  rbindlist(data_list, fill= T)
rm(data_list)
gc()

score_table[, codigo_subproducto := NULL]
score_table[, target := runif(.N)]

character_cols <-
  c(names(score_table)[sapply(score_table[, mget(names(score_table))], is.character)])

character_cols <- character_cols[character_cols != "definit"]
score_table[, (character_cols) := lapply(.SD, factor), .SDcols = character_cols]

# sapply(score_table[, mget(final_cols)], uniqueN)
# sapply(master[, mget(final_cols)], uniqueN)

##### making xgb data ####  
score_table_dmatrix <- score_table[, mget(final_cols)]
score_table_dmatrix <-
  sparse.model.matrix(target ~ . - 1, data = score_table_dmatrix)

model_cols1 <- score_table_dmatrix@Dimnames[[2]]

model_cols1 <- tolower(model_cols1)
model_cols1 <- 
  stri_trans_general(model_cols1,"Latin-ASCII")
model_cols1 <- tm::removePunctuation(model_cols1)
model_cols1 <- gsub(" ", "", model_cols1, fixed = TRUE)

score_table_dmatrix@Dimnames[[2]] <- model_cols1

common_vars <-  intersect(model_cols, model_cols1)
master_dmatrix <- master_dmatrix[, c(which(model_cols %in% common_vars))]
score_table_dmatrix <- score_table_dmatrix[, c(which(model_cols1 %in% common_vars))]

save(
  final_cols, 
  model_cols,
  master_dmatrix,
  master,
  common_vars,
  file = os.path.join(model_alias_scoring, paste0(products[i], "_xgb_objects.RData"))
)

# separate target
target_train_dmatrix <-
  as(data.matrix(master$target), 'dgCMatrix')

dtrain <-
  xgb.DMatrix(data = master_dmatrix, label = target_train_dmatrix)

gc()
rm( target_train_dmatrix)
# # set random seed for reproducibility
set.seed(1104)

cores <- parallel::detectCores() - 1
##### training xgboost model ####
print("Load previous xgboost model")

previous_model <-
  os.path.join( model_to_use, "models_library") 
previous_model <-
  get_path(previous_model, key_searcher = "xgb") %>% xgb.load()
opt_nrounds <-
  xgb.attr(previous_model, "best_iteration") %>% as.numeric()
max_depth <- xgb.attr(previous_model, "max.depth") %>% as.numeric()
eta <- xgb.attr(previous_model, "eta") %>% as.numeric()
gamma <- xgb.attr(previous_model, "gamma") %>% as.numeric()
alpha <- xgb.attr(previous_model, "alpha") %>% as.numeric()

# xgboost maximizing AUC of ROC 

print("Training xgboost model using ROC curve")

xgb.parameters <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = opt_nrounds,
  max.depth = max_depth,
  eta = eta[1],
  gamma = gamma[1],
  eval_metric = "auc", 
  alpha = alpha[1]
)



model_xgb_auc <- xgb.train(
  data = dtrain,
  nround = xgb.parameters$nrounds,
  params = xgb.parameters,
  early_stopping_rounds = xgb.parameters$early_stoping_round,
  verbose = 1 ,
  nthread = cores)

xgb.save(model_xgb_auc, os.path.join(models_folder, "xgb_auc.model"))

score_table[,pred := predict(model_xgb_auc, score_table_dmatrix)]
score_table[, target := NULL]
fwrite(score_table[, .(definit,
                  periodo,
                  pred
)],
os.path.join(model_alias_modeling, "pred_score.csv"))


