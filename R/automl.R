#' Automated machine learning
#' 
#' Automated machine learning with automated feature engineering via pipeline exploration optimization. Utilises h2o.automl as the modelling engine. Duplicate observations are removed based on id features provided to the function. Time sensitive partitioning is also performed if a time sensitive indicator feature is provided. The function is bound by time for both optimization of pipelines as well as model optimization.
#' Due to the function using the h2o library, the top 15 models are saved locally in order to load the models into the cluster at a later stage and perform scoring.
#' 
#' @param train [required | data.frame] Traning set, if no test and validation sets are provided it is considered as the full set and test and validation sets will be created.
#' @param y [optional | character] The name of the target feature contained in the training and validation sets.
#' @param valid [optional | data.frame | default=NULL] Validation set used to optimize model hyper parameters and evaluate against.
#' @param test [optional | data.frame | default=NULL] Test set for model validation.
#' @param x [optional | character | default=NULL] A character vector of predictor features to use. If left NULL then all features in the dataset except for the id, target and time partitioning features will be used.
#' @param id.feats [optional | character | default=NULL] The name or names of id features that will be used to de-duplicate the training set.
#' @param time.partition.feature [optional | character | default=NULL] The name of the time partitioning feature that will be used to create time sensitive train, validation and test sets.
#' @param optimization.metric [optional | character | default="AUTO"] Which metric models should optimize when learning. Options include AUC, logloss, mean_per_class_error, RMSE, MSE, mean_residual_deviance, MAE, RMSLE. When set to AUTO will do AUC for binary classification, mean_per_class_error for multi-class and mean_residual_deviance for regression problems.
#' @param valid.split [optional | numeric | default=0.1] The percentage of data to allocate to the validation set. If no time partitioning is done, then stratefied random sampling is done.
#' @param test.split [optional | numeric | default=0.2] The percentage of data to allocate to the test set. If no time partitioning is done, then stratefied random sampling is done.
#' @param pipeline.search.max.runtime.mins [optional | integer | default=30] The number of minutes allocated to optimized pre-processing pipelines.
#' @param automl.search.max.runtime.mins [optional | integer | default=30] The number of minutes allocated to train models on the optimized dataset. Uses h2o.automl.
#' @param balance.classes [optional | logical | default=FALSE] Should class imbalances be corrected by either up sampling minority cases or down sampling majority cases.
#' @param models [optional | character | default=c("DRF","GLM","GBM","XGBoost","DeepLearning","StackedEnsemble")] The models to fit when running h2o.automl. Note that for Windows operating systems xgboost is not available.
#' @param cv.folds [optional | integer | default=0] The number of folds to cross validate models on. Any value less than 3 will perform no cross validation.
#' @param max.levels [optional | integer | default=100] The maximum number of unique values in the target feature before it is seen as a regression problem.
#' @param data.leakage.cutoff [optional | numeric | default=0.65] The AUC cutoff value for determining which features are predictive in predicting the testing set. Features with a value greater than the cutoff will be removed from the feature set.
#' @param cluster.memory [optional | integer | default=NULL] The maxmimum memory allocated to the h2o cluster in gigabytes. Default of NULL which will auto assign memory.
#' @param min.feature.importance [optional | numeric | default=0.1] The minimum scaled feature importance features need to have before they are removed from the feature space.
#' @param seed [optional | integer | default=NULL] Random seed value for reproducable results.
#' @param output.path [optional | character | default=NULL] Path where function output will save to. Default of NULL, which will save to the current working directory. 
#' @return List of objects and output generated to a specific path
#' @export
#' @examples
#' ## NOT RUN
#' res <- automl(train=iris, valid=iris, y="Species")
#' @author 
#' Xander Horn
automl <- function(train, y, valid = NULL, test = NULL, x = NULL, id.feats = NULL, time.partition.feature = NULL,
                   optimization.metric = "AUTO", valid.split = 0.1, test.split = 0.2, pipeline.search.max.runtime.mins = 30, 
                   automl.search.max.runtime.mins = 30, balance.classes = FALSE, models = c("DRF","GLM","GBM","XGBoost","DeepLearning","StackedEnsemble"),
                   cv.folds = 0, max.levels = 100, data.leakage.cutoff = 0.65, cluster.memory = NULL, min.feature.importance = 0.1, seed = 1,
                   output.path = NULL, pipeline = NULL, return.data = FALSE){
  
  library(caret)
  library(h2o)
  
  info <- list()
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  set.seed(seed)
  options(scipen = 999)
  t.row <- nrow(train)
  
  exp <- describe(data = train, progress = F)
  remove <- as.character(exp[which(exp$all.na == 1 | exp$constant == 1 | exp$duplicate == 1), "feature"])
  remove <- setdiff(remove, c(id.feats, y, time.partition.feature))
  train <- train[,setdiff(names(train), remove)]
  
  if(is.null(test)){
    if(is.null(time.partition.feature) == TRUE){
      ind <- caret::createDataPartition(y = train[,y], p = test.split, list = FALSE)
      test <- train[ind,]
      train <- train[-ind,]
    } else {
      ind <- time.partition(data = train, time.feature = time.partition.feature, valid.sample.size = test.split)
      test <- train[ind$valid,]
      train <- train[ind$train,]
    }
  } else {
    test <- test[,setdiff(names(test), remove)]
  }
  
  if(is.null(valid)){
    ns <- (valid.split * t.row) / nrow(train)
    if(is.null(time.partition.feature) == TRUE){
      ind <- caret::createDataPartition(y = train[,y], p = ns, list = FALSE)
      valid <- train[ind,]
      train <- train[-ind,]
    } else {
      ind <- time.partition(data = train, time.feature = time.partition.feature, valid.sample.size = valid.split)
      valid <- train[ind$valid,]
      train <- train[ind$train,]
    }
  } else {
    valid <- valid[,setdiff(names(valid), remove)]
  }
  
  train <- quick.format(train)
  valid <- quick.format(valid)
  test <- quick.format(test)
  
  if(is.null(time.partition.feature) == FALSE){
    info$data.partitioning <- paste0("Data was partiioned for validation and testing, taking into consideration the time component present in the data. Simply put, all models are evaulated using future data instead of randomly assinging unseen data for evaluation.")
  } else {
    info$data.partitioning <- paste0("Data was partitioned by using random stratified sampling based on the target feature to create validation and testing sets for model evaluation.")
  }
  
  cat("lazy | Checking for data leakage features \n")
  leak <- data.leak(train = train, test = valid, id.feats = c(id.feats,time.partition.feature), seed = 1, progress = FALSE)
  remove <- as.character(leak[which(leak$auc > 0.65), "feature"])
  remove <- setdiff(remove, c(id.feats,time.partition.feature))
  if(length(remove) > 0 & length(remove) != ncol(train)){
    train <- train[, setdiff(names(train), remove)]
    valid <- valid[, setdiff(names(valid), remove)]
    dl <- TRUE
  } else {
    dl <- FALSE
  }
  
  info$data.leakage <- paste0("Data leakage checks were performed to determine if any features causes leakage between datasets. Pruned decision trees were fitted to each feature and then used to predict the training and testing sets. If any feature had an AUC (area under the curve) value for predicting the test set above ",data.leakage.cutoff," the features were flagged and removed.")
  
  cat("lazy | Removing features with low importance contribution \n")
  imp <- feature.importance(data = train, y = y, x = x, verbose = F,cluster.shutdown = F)$importance.table
  x <- setdiff(as.character(imp[which(imp$mean.importance > min.feature.importance), "feature"]), c(id.feats,time.partition.feature))
  
  info$feature.importance <- paste0("To reduce dimensionality and noisy features, only features with a scaled importance value greater than ", min.feature.importance * 100, "% were kept for further pre-processing. Feature importance were calculated by calulating importance for a random forest, lasso regression and light gbm model, whereafter the average importance is calculated and used.") 
  
  if(is.null(pipeline) == TRUE){
    res <- explore.pipelines(train = train, valid = valid, id.feats = c(id.feats,time.partition.feature), x = x, y= y, 
                             cluster.memory = cluster.memory, max.runtime.mins = pipeline.search.max.runtime.mins,seed = seed)
    
    pipeline <- res$best.pipelines$average
    
    info$optimized.pipeline <- paste0(nrow(res$summary)," out of 1536 possible pipelines were explored and the best performing pipeline was selected to be used for further modelling. A total of ", pipeline.search.max.runtime.mins," minutes were used to explore various pipelines.") 
    if(nrow(subset(res$summary, is.na(res$summary$mean.performance) == TRUE)) == nrow(res$summary)){
      stop("No pipeline explored given time limit provided to function, increase pipeline.search.max.runtime.mins to solve this")
    }
    
  } 
  
  cat("lazy | Applying pipeline to data \n")
  pp <- pre.process(data = train, x = x, y = y, id.feats = c(id.feats,time.partition.feature), pipeline = pipeline, verbose = FALSE)
  train <- pp$data
  valid <- pre.process(data = valid, pipeline = pp$pipeline, mapping.list = pp$mapping.list, verbose = FALSE)
  test <- pre.process(data = test, pipeline = pp$pipeline, mapping.list = pp$mapping.list, verbose = FALSE)
  
  if(is.null(time.partition.feature) == FALSE){
    general.info <- data.frame(train.obs = nrow(train),
                               valid.obs = nrow(valid),
                               test.obs = nrow(test),
                               nr.features = ncol(train),
                               train.start = min(train[,time.partition.feature]),
                               train.end = max(train[,time.partition.feature]),
                               valid.start = min(valid[,time.partition.feature]),
                               valid.end = max(valid[,time.partition.feature]),
                               test.start = min(test[,time.partition.feature]),
                               test.end = max(test[,time.partition.feature]))
  } else {
    general.info <- data.frame(train.obs = nrow(train),
                               valid.obs = nrow(valid),
                               test.obs = nrow(test),
                               nr.features = ncol(train))
  }
  
  if(is.null(y) == FALSE){
    if(length(unique(train[,y])) <= max.levels){
      train[,y] <- as.factor(train[,y])
      valid[,y] <- as.factor(valid[,y])
      test[,y] <- as.factor(test[,y])
      if(length(unique(train[,y])) > 2){
        metrics <- c("logloss","mean_per_class_error","r2","RMSE","MSE")
      } else {
        metrics <- c("logloss","AUC","pr_auc","Gini","mean_per_class_error")
      }
    }
    
    if(length(unique(train[,y])) > max.levels){
      train[,y] <- as.numeric(train[,y])
      valid[,y] <- as.numeric(valid[,y])
      test[,y] <- as.numeric(test[,y])
      metrics <- c("r2","mae","mean_residual_deviance","MSE","RMSE")
    }
  }
  
  x <- setdiff(names(train), c(id.feats, y, time.partition.feature))
  imp <- feature.importance(data = train, y = y, x = x, verbose = F,cluster.shutdown = F)
  fi <- imp$importance.table
  x <- setdiff(as.character(fi[which(fi$mean.importance > min.feature.importance), "feature"]), c(id.feats,time.partition.feature,y))

  valid <- valid[,names(train)]
  test <- test[,names(train)]
  
  if(is.null(cluster.memory) == FALSE){
    quiet(h2o::h2o.init(max_mem_size = paste0(cluster.memory,"G")))
  } else {
    quiet(h2o::h2o.init())
  }
  quiet(h2o.removeAll())
  quiet(h2o:::.h2o.garbageCollect())
  
  if(h2o::h2o.xgboost.available() == FALSE){
    models <- setdiff(models, "XGBoost")
  }
  
  if(return.data == TRUE){
    out$data$train <- train
    out$data$valid <- valid
    out$data$test <- test
  }
  
  
  train <- quiet(as.h2o(train))
  valid <- quiet(as.h2o(valid))
  test <- quiet(as.h2o(test))
  
  cat("lazy | Training models \n")
  aml <- h2o.automl(x = x, y = y, training_frame = train, balance_classes = balance.classes,
                    validation_frame = valid,leaderboard_frame = valid,nfolds = cv.folds,
                    max_runtime_secs = (automl.search.max.runtime.mins * 60),seed = seed,
                    include_algos = models,stopping_metric = optimization.metric)
  
  if(balance.classes == TRUE){
    info$optimized.models <- paste0("H2O automl were used as the modelling engine to tune and find the best performing model. A total of ", automl.search.max.runtime.mins," minutes were used to find the best performing model. Due to class imbalance in the target feature, classes were balanced on the training set before fitting models to the training set. Probabilities are re-scaled when predicting to arrive at an accurate probability estimate, for more information on this, please visit http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/balance_classes.html#description.")
  } else {
    info$optimized.models <- paste0("H2O automl were used as the modelling engine to tune and find the best performing model. A total of ", automl.search.max.runtime.mins," minutes were used to find the best performing model.")
  }
  
  model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
  model_ids <- model_ids[1:15]
  model_ids <- model_ids[!is.na(model_ids)]
  out <- list()
  models <- list()
  
  cat("lazy | Evaluating models \n")
  perf <- as.data.frame(expand.grid(model = model_ids,
                                    metric = metrics,
                                    train = NA,
                                    valid = NA,
                                    test = NA))
  perf <- subset(perf, is.na(perf$model) == FALSE)
  
  for(i in 1:nrow(perf)){
    mod <- h2o.getModel(grep(as.character(perf[i, "model"]), model_ids, value = TRUE)[1])
    t1 <- h2o.performance(model = mod, train = TRUE)
    t2 <- h2o.performance(model = mod, valid = TRUE)
    t3 <- h2o.performance(model = mod, newdata = test)
    
    perf[i, "train"] <- t1@metrics[[as.character(perf[i,"metric"])]]
    perf[i, "valid"] <- t2@metrics[[as.character(perf[i,"metric"])]]
    perf[i, "test"] <- t3@metrics[[as.character(perf[i,"metric"])]]
  }
  
  if(is.null(output.path) == TRUE){
    output.path <- getwd()
  }
  output.path <- paste0(output.path,"/lazy_output")
  dir.create(output.path)
  dir.create(paste0(output.path,"/model_objects"))
  
  for(i in 1:length(model_ids)){
    models[[i]] <- h2o.getModel(grep(model_ids[i], model_ids, value = TRUE)[1])
    h2o.saveModel(object = models[[i]], path = paste0(output.path,"/model_objects"), force = TRUE)
    h2o.saveMojo(object = models[[i]], path = paste0(output.path,"/model_objects"), force = TRUE)
  }
  names(models) <- model_ids
  perf$train.valid.overfit.value <- abs(perf$train - perf$valid)
  
  out$leaderboard <- perf
  out$partitions <- general.info
  out$modelling.development.info <- info
  out$data.leak <- leak
  out$feature.importance <- imp
  out$pipeline.search$summary <- res$summary
  out$pipeline.search$plots <- res$plots
  out$pipeline <- pp$pipeline
  out$mapping.list <- pp$mapping.list
  out$models <- models
  
  saveRDS(out, paste0(output.path,"/lazy_automl_output.RDS"))
  cat(paste0("lazy | Output generated to: ", output.path, " \n"))
  
  return(out)
}
