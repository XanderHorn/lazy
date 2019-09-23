#' Automated machine learning
#' 
#' Automated machine learning with automated feature engineering via pipeline exploration optimization. Utilises h2o.automl as the modelling engine. Duplicate observations are removed based on id features provided to the function. Time sensitive partitioning is also performed if a time sensitive indicator feature is provided. The function is bound by time for both optimization of pipelines as well as model optimization.
#' Due to the function using the h2o library, models are saved locally  to enable loading the models into the h2o cluster at a later stage and perform scoring. Note that stacked ensembles will only be trained if the cv folds are set to 3 or above.
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
#' @param pipeline [optional | list | default=NULL] A pre-defined pipeline to train models on.
#' @param return.data [optional | logical | default=TRUE] Return the pre-processed train, validation and test sets.
#' @param output.path [optional | character | default=NULL] Path where function output will save to. Default of NULL, which will save to the current working directory. 
#'
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
                   output.path = NULL, pipeline = NULL, return.data = TRUE){
  
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
  
  if(is.null(time.partition.feature) == FALSE){
    if(class(train[, time.partition.feature]) %in% c("character","factor")){
      train[, time.partition.feature] <- as.Date(train[, time.partition.feature])
    }
    train <- train[order(train[,time.partition.feature]), ]
  }
  
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
    info$time.partitioning <- TRUE
  } else {
    info$time.partitioning <- FALSE
  }
  
  cat("lazy | Checking for data leakage features \n")
  leak <- data.leak(train = train, test = valid, id.feats = c(id.feats,time.partition.feature), seed = seed, progress = FALSE)
  remove <- as.character(leak[which(leak$auc > data.leakage.cutoff), "feature"])
  remove <- setdiff(remove, c(id.feats,time.partition.feature))
  if(length(remove) > 0 & length(remove) != ncol(train)){
    train <- train[, setdiff(names(train), remove)]
    valid <- valid[, setdiff(names(valid), remove)]
    test <- test[, setdiff(names(test), remove)]
    dl <- TRUE
  } else {
    dl <- FALSE
  }
  
  
  info$data.leakage <- TRUE
  info$data.leakage.cutoff <- 0.65
  
  #cat("lazy | Removing features with low importance contribution \n")
  #imp <- feature.importance(data = train, y = y, x = x, verbose = F,cluster.shutdown = F, seed = seed)$importance.table
  #x <- setdiff(as.character(imp[which(imp$mean.importance > min.feature.importance), "feature"]), c(id.feats,time.partition.feature))
  
  if(is.null(pipeline) == TRUE){
    res <- explore.pipelines(train = train, valid = valid, id.feats = c(id.feats,time.partition.feature), y= y, 
                             cluster.memory = cluster.memory, max.runtime.mins = pipeline.search.max.runtime.mins,seed = seed)
    
    pipeline <- res$best.pipelines$average
    
    info$pipelines.explored <- nrow(res$summary)
    
    if(nrow(subset(res$summary, is.na(res$summary$mean.performance) == TRUE)) == nrow(res$summary)){
      stop("No pipeline explored given time limit provided to function, increase pipeline.search.max.runtime.mins to solve this")
    }
    
  } else {
    info$pipelines.explored <- NULL
  }
  
  cat("lazy | Applying pipeline to data \n")
  pp <- pre.process(data = train, x = x, y = y, id.feats = c(id.feats,time.partition.feature), pipeline = pipeline, verbose = FALSE)
  train <- pp$data
  valid <- pre.process(data = valid,id.feats = c(id.feats,time.partition.feature), pipeline = pp$pipeline, mapping.list = pp$mapping.list, verbose = FALSE)
  test <- pre.process(data = test,id.feats = c(id.feats,time.partition.feature), pipeline = pp$pipeline, mapping.list = pp$mapping.list, verbose = FALSE)
  
  if(min.feature.importance > 0){
    info$ml.feature.importance <- TRUE
    info$ml.min.feature.importance <- min.feature.importance
  } else {
    info$ml.feature.importance <- FALSE
    info$ml.min.feature.importance <- 0
  }
  
  
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
  imp <- feature.importance(data = train, y = y, x = x, verbose = F,cluster.shutdown = F, seed = seed)
  fi <- imp$importance.table
  x <- setdiff(as.character(fi[which(fi$mean.importance > min.feature.importance), "feature"]), c(id.feats,time.partition.feature,y))

  valid <- valid[,c(x,y,time.partition.feature,id.feats)]
  test <- test[,c(x,y,time.partition.feature,id.feats)]
  
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
  
  out <- list()
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
    info$ml.balances.classes <- TRUE
  } else {
    info$ml.balances.classes <- FALSE
  }
  
  model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
  model_ids <- model_ids[!is.na(model_ids)]
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
  
  pp$pipeline$settings$ml.time.partitioning <- info$time.partitioning
  pp$pipeline$settings$ml.data.leakage <- info$data.leakage
  pp$pipeline$settings$ml.feature.importance <- info$ml.feature.importance
  pp$pipeline$settings$ml.min.feature.importance <- info$ml.min.feature.importance
  pp$pipeline$settings$ml.balance.classes <- info$ml.balances.classes
  pp$pipeline$settings$ml.cv.folds <- cv.folds
  pp$pipeline$settings$ml.models <- models
  pp$pipeline$settings$ml.model.search.time <- automl.search.max.runtime.mins
  if(is.null(pipeline) == TRUE){
    pp$pipeline$settings$ml.pipeline.search <- TRUE
  } else {
    pp$pipeline$settings$ml.pipeline.search <- FALSE
  }
  pp$pipeline$settings$ml.pipeline.search.time <- pipeline.search.max.runtime.mins
  
  
  
  out$leaderboard <- perf
  out$partitions <- general.info
  out$modelling.development.info <- info
  out$data.leak <- leak
  out$feature.importance <- imp
  if(is.null(pipeline) == TRUE){
    out$pipeline.search$summary <- res$summary
    out$pipeline.search$plots <- res$plots
  }
  out$pipeline <- pp$pipeline
  out$mapping.list <- pp$mapping.list
  out$models <- models
  
  saveRDS(out, paste0(output.path,"/lazy_automl_output.RDS"))
  cat(paste0("lazy | Output generated to: ", output.path, " \n"))
  
  return(out)
}
