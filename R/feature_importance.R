#' Random forest feature importance
#' 
#' Computes feature importance according to random forest, lasso and light gbm models and then calculates the mean imporatance. The provided data set will be downsampled by random stratified sampling to have a maximum of 60k observations if the dataset has more observations than 60k, training and validation sets are then created. Categortical features are converted to numeric by representing each category as a numeric number for simplicity purposes.A
#'
#' @param data [required | data.frame] Dataset containing predictor and target features  
#' @param x [optional | character | default=NULL] A vector of feature names present in the dataset used to predict the target feature. If NULL then all columns in the dataset is used.
#' @param y [required | character] The name of the target feature contained in the dataset
#' @param valid.split [optional | numeric | default=0.2] The percentage of data assigned to the validation partition
#' @param max.class.levels [optional | numeric | default=100] The maximum number of unique values in the target feature before it is considered a regression problem.
#' @param verbose [optional | logical | default=TRUE] Toggles function to be chatty or not
#' @param seed [optional | integer | default=1] The random number seed for reproducable results 
#' @param cluster.shutdown [optional | integer | default=TRUE] Shutdown h2o cluster after completion. 
#' @return List containing a data.frame with feature importance, a feature importance plot and a cumulative feature importance plot
#' @export
#' @examples
#' imp <- feature.importance(data = iris, x = names(iris)[1:4], y = "Species")
#' @author 
#' Xander Horn
feature.importance <- function(data, x = NULL, y, valid.split = 0.2, max.class.levels = 100, cluster.shutdown = TRUE, seed = 1, verbose = TRUE){
  
  library(h2o)
  library(caret)
  library(ggplot2)
  
  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }
  
  if(missing(y)){
    stop("No target feature provided to function in arg 'y'")
  }
  
  if(valid.split <= 0){
    warning("valid.split restricted between 0 and 1, defaulting to 0.2")
  }
  
  if(valid.split >= 1){
    warning("valid.split restricted between 0 and 1, defaulting to 0.2")
  }
  
  set.seed(seed)
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  quiet(h2o::h2o.init())
  quiet(h2o.removeAll())
  quiet(h2o:::.h2o.garbageCollect())
  
  if(is.null(x) == TRUE){
    x <- names(data)
  }
  x <- setdiff(names(data), y)
  
  if(nrow(data) >= 60000){
    data <-  data[caret::createDataPartition(data[,y], p = 0.1270024, list = F), ]
  }
  
  data <- quick.format(data)
  
  ind <- caret::createDataPartition(data[,y], p = valid.split, list = F)
  train <- data[-ind,]
  valid <- data[ind,]
  
  unq <- length(unique(data[, y]))
  if(unq <= max.class.levels){
    if(unq == 2){
      family <- "binomial"
      metric <- "AUC"
    } else {
      family <- "multinomial"
      metric <- "logloss"
    }
    train[, y] <- as.factor(train[, y])
    valid[, y] <- as.factor(valid[, y])
  } else {
    train[, y] <- as.numeric(train[, y])
    valid[, y] <- as.numeric(valid[, y])
    family <- "gaussian"
    metric <- "MSE"
  }
  
  out <- list()
  
  ft <- detect.feats(data = train, x = x, progress = FALSE)
  cat <- as.character(ft[which(ft$category == "categorical"), "feature"])
  if(length(cat) > 0){
    for(i in 1:length(cat)){
      train[,cat[i]] <- as.numeric(as.factor(train[,cat[i]]))
      valid[,cat[i]] <- as.numeric(as.factor(valid[,cat[i]]))
    }
  }
  
  train <- quiet(as.h2o(train[, c(x, y)]))
  valid <- quiet(as.h2o(valid[, c(x, y)]))
  
  if(verbose == TRUE){
    cat("lazy | Random forest importance \n")
    rf <- suppressWarnings(h2o.randomForest(y = y, training_frame = train, validation_frame = valid,
                           ntrees = 100, seed = seed))
  } else {
    rf <- suppressWarnings(quiet(h2o.randomForest(y = y, training_frame = train, validation_frame = valid,
                           ntrees = 100, seed = seed)))
  }
  
  
  rf.imp <- as.data.frame(h2o.varimp(rf))
  rf.imp <- rf.imp[,c("variable","scaled_importance")]
  names(rf.imp) <- c("feature", "rf.importance")
  
  if(verbose == TRUE){
    cat("lazy | Lasso importance \n")
    lr <- suppressWarnings(h2o.glm(y = y, training_frame = train, validation_frame = valid,
                  family = family, alpha = 1, seed = seed))
  } else {
    lr <- suppressWarnings(quiet(h2o.glm(y = y, training_frame = train, validation_frame = valid,
                  family = family, alpha = 1, seed = seed)))
  }
  
  lr.imp <- as.data.frame(h2o.varimp(lr)) 
  lr.imp <- lr.imp[,c("variable","scaled_importance")]
  names(lr.imp) <- c("feature", "lasso.importance")
  
  if(verbose == TRUE){
    cat("lazy | Light gbm importance \n")
    lgb <- suppressWarnings(h2o.xgboost(y = y, training_frame = train, validation_frame = valid,
                       seed = seed, eta = 0.05, colsample_bylevel = 0.8, min_child_weight = 5,
                       max_depth = 10, colsample_bytree = 0.8, reg_lambda = 1,
                       stopping_rounds = 5, subsample = 0.6, ntrees = 165,
                       tree_method = "hist", grow_policy = "lossguide"))
  } else {
    lgb <- suppressWarnings(quiet(h2o.xgboost(y = y, training_frame = train, validation_frame = valid,
                       seed = seed, eta = 0.05, colsample_bylevel = 0.8, min_child_weight = 5,
                       max_depth = 10, colsample_bytree = 0.8, reg_lambda = 1,
                       stopping_rounds = 5, subsample = 0.6, ntrees = 165,
                       tree_method = "hist", grow_policy = "lossguide")))
  }
  
  
  lgb.imp <- as.data.frame(h2o.varimp(lgb)) 
  lgb.imp <- lgb.imp[,c("variable","scaled_importance")]
  names(lgb.imp) <- c("feature", "lightgbm.importance")
  
  imp <- merge(x = rf.imp,
               y = lr.imp,
               by.x = "feature",
               all.x = TRUE)
  
  imp <- merge(x = imp,
               y = lgb.imp,
               by.x = "feature",
               all.x = TRUE)
  
  imp$mean.importance <- rowMeans(imp[,2:4], na.rm = T)
  imp <- imp[order(imp$mean.importance), ]
  imp$mean.importance <- imp$mean.importance / max(imp$mean.importance)
  imp$mean.importance <- ifelse(imp$mean.importance < 0, 0, imp$mean.importance)
  imp$cuml.importance <- cumsum(imp$mean.importance / sum(imp$mean.importance))
  
  imp <- merge(x = imp,
               y = ft,
               by.x = "feature",
               all.x = TRUE)
  
  imp <- imp[order(imp$mean.importance , decreasing = TRUE), ]
  imp <- subset(imp, imp$mean.importance > 0)
  names(imp)[ncol(imp)] <- "feature.class"
  if(nrow(imp) > 30){
    nf <- 30
  } else {
    nf <- nrow(imp)
  }
  
  fplt <- ggplot(data=imp[1:nf,], aes(x = factor(feature, levels = imp$feature),y = mean.importance, fill = mean.importance)) +
    geom_bar(position="dodge",stat="identity") + 
    coord_flip() +
    ggtitle(paste0("Mean importance for top ",nf," features")) + 
    xlab(NULL)
  
  cplt <- qplot(x = seq(1,nrow(imp)), 
                y = 1- imp$cuml.importance, 
                geom='line',
                main = "Mean cumulative importance",
                xlab = "Nr features", 
                ylab = "Cumulative importance")
  
  perf <- data.frame(expand.grid(model = c("randomforest","lasso","lightgbm"),
                                 metric = metric,
                                 train = NA,
                                 valid = NA))
  
  perf[1, "train"] <- rf@model$training_metrics@metrics[[metric]]
  perf[1, "valid"] <- rf@model$validation_metrics@metrics[[metric]]
  perf[2, "train"] <- lr@model$training_metrics@metrics[[metric]]
  perf[2, "valid"] <- lr@model$validation_metrics@metrics[[metric]]
  perf[3, "train"] <- lgb@model$training_metrics@metrics[[metric]]
  perf[3, "valid"] <- lgb@model$validation_metrics@metrics[[metric]]
  
  if(cluster.shutdown == TRUE){
    quiet(h2o.shutdown(prompt = F))
  }
  
  out$importance.table <- imp
  out$importance.plot <- fplt
  out$cuml.importance.plot <- cplt
  out$performance.table <- perf
  
  return(out)
}