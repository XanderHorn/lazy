#' Machine learning pipeline optimization
#' 
#' Machine learning pipelines consist of various methods for data cleaning and feature engineering.
#' 
#' Constructs a grid with all combinations of pipelines then randomly shuffles the grid and explores pipelines along the grid. The function will run until the max runtime in minutes threshold has been reached. Pipelines are evaluated using one or all of the following models, random forest, lasso regression and a light gbm. The mean performance is also calculated and returned. For binary classification problems, Gini is used to evaluatedpipelines. For multiclass classification, logloss is used to evaluate pipelines. For regression mse is used to evaluate pipelines. 
#' The training set is down sampled to a max of 40k observations along with the validation set for faster pipeline exploration.
#' 
#' @param train [required | data.frame] Traning set before any feature engineering or data cleaning is done.
#' @param valid [required | data.frame] Validation set before any feature engineering or data cleaning is done.
#' @param id.feats [optional | character | default=NULL] Names of ID features. Used to de-duplicate the training dataset given ID features, if nothing is provided then no de-duplication is done.
#' @param x [optional | character | default=NULL] Features to include as predictors in the training and validation sets. If NULL then all features in the dataset will be used except for the target feature and ID features.
#' @param y [optional | character] The name of the target feature contained in the training and validation sets.
#' @param cluster.memory [optional | integer | default=NULL] The maxmimum allocated memory in GB designated to the H2O cluster. 
#' @param max.runtime.mins [optional | integer | default=10] The maximum run time in minutes for the function to identify the best possible pipelines. Recommended to increase for datasets with a large number of columns or multi-class problems.
#' @param models [optional | character | default=c('randomforest','lasso','lightgbm')] The models used to identify the best possible pipeline. By default a random forest, lasso and light gbm model is trained on each pipeline and evaluated. Options are randomforest, lasso or lightgbm.
#' @param max.levels [optional | numeric | default=100] The maximum number of unique values in the target feature before it is considered a regression problem.
#' @param progress [optional | logical | default=TRUE] Display a progress bar.
#' @param reduce.dimensionality [optional | logical | default=TRUE] Reduces dimensionality by computing feature importances for each feature and only keeping the top 10 numerical and categorical features. All other feature types are kept along with the top performing features. Used to speed up pipeline search. If the number of features in the dataset is greater than 80, dimensionality will be reduced, else the data is used as is.
#' @param seed [optional | integer | default=1] Random number seed for reproducable results.
#'
#' @return List containing best pipelines, summary frame and pipeline plots
#' @export
#' @examples
#' #Iris dataset used for both train and validation for demonstration purposes only
#' res <- explore.pipelines(train = iris, valid = iris, y = "Species")
#' @author 
#' Xander Horn
explore.pipelines <- function(train, valid, id.feats = NULL, x = NULL, y, cluster.memory = NULL, max.runtime.mins = 10, reduce.dimensionality = TRUE,
                              models = c("randomforest","lasso","lightgbm"), max.levels = 100, progress = TRUE, seed = 1){

  library(caret)
  library(h2o)
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  if(missing(train)){
    stop("Provide training set to function")
  }
  
  if(missing(valid)){
    stop("Provide validation set to function")
  }
  
  if(missing(y)){
    stop("Provide target feature name to function")
  }

  set.seed(seed)
  options(scipen = 999)
  
  if(is.null(x) == TRUE){
    x <- names(train)
  }
  
  x <- setdiff(x, c(y,id.feats))
  
  if(reduce.dimensionality == TRUE & ncol(train) > 80){
    if(progress == TRUE){
      cat("lazy | Reducing dimensionality \n")
    }
    imp <- feature.importance(data = train[,setdiff(names(train), id.feats)],x = x, y = y, seed = seed, verbose = FALSE, cluster.shutdown = F)
    imp <- imp$importance.table
    num <- subset(imp, imp$feature.class == "numeric")
    cat <- subset(imp, imp$feature.class == "categorical")
    
    if(nrow(num) > 0){
      if(nrow(num) > 10){
        num <- as.character(num[1:10, "feature"])
      } else {
        num <- as.character(num[, "feature"])
      }
    } else {
      num <- NULL
    }
    
    
    if(nrow(cat) > 0){
      if(nrow(cat) > 10){
        cat <- as.character(cat[1:10, "feature"])
      } else {
        cat <- as.character(cat[, "feature"])
      }
    } else {
      cat <- NULL
    }
    
    ot <- subset(imp, !imp$feature.class %in% c("numeric","categorical"))$feature
    x <- setdiff(c(num, cat, ot), id.feats)
  }
  
  if(nrow(train) > 40000){
    train <- train[caret::createDataPartition(y = train[,y], p = 40000/nrow(train), list = F),]
  }
  
  if(nrow(valid) > 40000){
    valid <- valid[caret::createDataPartition(y = valid[,y], p = 40000/nrow(valid), list = F),]
  }
  
  if(reduce.dimensionality == TRUE){
    train <- train[,c(x,y,id.feats)]
    valid <- valid[,c(x,y,id.feats)]
  }
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  if(is.null(y) == FALSE){
    if(length(unique(train[,y])) <= max.levels & length(unique(train[,y])) >= 2){
      ml.type <- "multi-class classification"
      family <- "multinomial"
      metric <- "logloss"
      max <- FALSE
    }
    
    if(length(unique(train[,y])) == 2){
      ml.type <- "binary classification"
      family <- "binomial"
      metric <- "AUC"
      max <- TRUE
    }
    
    if(length(unique(train[,y])) > max.levels){
      ml.type <- "regression"
      family <- "gaussian"
      metric <- "MSE"
      max <- FALSE
    }
  }
  
  tmp <- expand.grid(impute.mode = c("auto","encode","median.mode"),
                     categorical.mode = c("onehot.prop", "auto", "target", "ordinal"),
                     outlier.clipping = c(FALSE,TRUE),
                     numeric.transform = c(FALSE,TRUE),
                     tracking = c(FALSE, TRUE),
                     categorical.interactions = c(FALSE, TRUE),
                     kmeans.features = c(FALSE, TRUE),
                     numeric.interactions = c(FALSE, TRUE),
                     freq.encode = c(FALSE, TRUE),
                     stringsAsFactors = FALSE)
  
  pls <- list()
  c <- data.frame()
  for(i in 1:nrow(tmp)){
    pls[[i]] <- design.pipeline(impute.mode = tmp[i, "impute.mode"],
                                categorical.mode = tmp[i, "categorical.mode"],
                                outlier.clipping = tmp[i, "outlier.clipping"],
                                numeric.transform = tmp[i, "numeric.transform"],
                                categorical.tracking = tmp[i, "tracking"],
                                impute.tracking = tmp[i, "tracking"],
                                outlier.tracking = tmp[i, "tracking"],
                                categorical.interactions = tmp[i, "categorical.interactions"],
                                kmeans.features = tmp[i, "kmeans.features"],
                                numeric.interactions = tmp[i, "numeric.interactions"],
                                freq.encode = tmp[i, "freq.encode"],
                                seed = seed)
    
    t <- as.data.frame(t(do.call(rbind, pls[[i]]$settings)), stringsAsFactors = FALSE)
    t$name <- pls[[i]]$name
    c <- rbind(c, t)
  }
  
  names(pls) <- c$name
  c$nr.features <- NA
  c$metric <- metric
  c$randomforest <- NA
  c$lasso <- NA
  c$lightgbm <- NA
  
  if(is.null(cluster.memory) == FALSE){
    quiet(h2o::h2o.init(max_mem_size = paste0(cluster.memory,"G")))
  } else {
    quiet(h2o::h2o.init())
  }
  quiet(h2o.removeAll())
  quiet(h2o:::.h2o.garbageCollect())
  
  # Shuffle pipeline frame
  c <- c[sample(nrow(c)),]

  if(progress == TRUE){
    cat("lazy | Exploring pipelines \n")
    pb <- txtProgressBar(min = 0, max = max.runtime.mins, style = 3)
  }
  
  start.time <- Sys.time()
  for(i in 1:nrow(c)){
    Sys.sleep(1)
    update.time <- Sys.time()
    duration <- as.numeric(difftime(update.time, start.time, units="mins"))
  
    if(duration <= max.runtime.mins){
      
      res <- pre.process(data=train, x = x, y = y, id.feats = id.feats, 
                         pipeline = pls[[which(names(pls) == c[i, "name"])]], verbose = FALSE)
      
      c[i,"nr.features"] = ncol(res$data)
      
      tmp.train <- res$data
      tmp.valid <- pre.process(data=valid,  pipeline = res$pipeline, mapping.list = res$mapping.list, verbose = FALSE)
      
      tmp.train <- tmp.train[,setdiff(names(tmp.train), id.feats)]
      tmp.valid <- tmp.valid[,setdiff(names(tmp.valid), id.feats)]
      
      if(ml.type %in% c("binary classification", "multi-class classification")){
        tmp.train[, y] <- as.factor(tmp.train[, y])
        tmp.valid[, y] <- as.factor(tmp.valid[, y])
      } else {
        tmp.train[, y] <- as.numeric(tmp.train[, y])
        tmp.valid[, y] <- as.numeric(tmp.valid[, y])
      }
      
      tmp.train <- quiet(as.h2o(tmp.train))
      tmp.valid <- quiet(as.h2o(tmp.valid))
      
      if("randomforest" %in% models){
        rf <- quiet(h2o.randomForest(y = y, training_frame = tmp.train, validation_frame = tmp.valid, ntrees = 100, seed = seed))
        c[i, "randomforest"] <- h2o.performance(rf, newdata=tmp.valid)@metrics[metric]
      }
      
      if("lasso" %in% models){
        lr <- quiet(h2o.glm(y = y, training_frame = tmp.train, validation_frame = tmp.valid, family = family, alpha = 1, seed = seed))
        c[i, "lasso"] <- h2o.performance(lr, newdata=tmp.valid)@metrics[metric]
      }
      
      if("lightgbm" %in% models){
        lgb <- quiet(h2o.xgboost(y = y, training_frame = tmp.train, validation_frame = tmp.valid,
                                 seed = seed, eta = 0.05, colsample_bylevel = 0.8, min_child_weight = 5,
                                 max_depth = 10, colsample_bytree = 0.8, reg_lambda = 1,
                                 stopping_rounds = 5, subsample = 0.6, ntrees = 165,
                                 tree_method = "hist", grow_policy = "lossguide"))
        c[i, "lightgbm"] <- h2o.performance(lgb, newdata=tmp.valid)@metrics[metric]
      }
      quiet(h2o.removeAll())
      quiet(h2o:::.h2o.garbageCollect())
      
      if(progress == TRUE){
        setTxtProgressBar(pb, duration)
      }
    } else {
      break
    }
  }
  
  if(progress == TRUE){
    if((duration / max.runtime.mins) != 1){
      setTxtProgressBar(pb, max.runtime.mins)
    }
    cat(" \n")
  }
  
  c$mean.performance <- rowMeans(c[,(which(names(c) == "metric") + 1):ncol(c)])
  c <- c[, c(names(c)[1:32], models, "mean.performance")]
  c$search.duration.mins <- max.runtime.mins
  
  if(nrow(subset(c, is.na(c$mean.performance) == TRUE)) == nrow(c)){
    stop("No pipeline explored given time limit provided to function, increase max.runtime.mins to solve this")
  }
  
  # Select best pipeline
  bst <- list()
  plots <- list()
  c <- subset(c, is.na(c$mean.performance) == FALSE)
  if("randomforest" %in% models){
    if(max == TRUE){
      bst.rf <- which.max(c$randomforest)
      y.rf <- c[order(c$randomforest), ]$randomforest
    } else {
      bst.rf <- which.min(c$randomforest)
      y.rf<- c[order(c$randomforest, decreasing = TRUE), ]$randomforest
    }
    bst$tree.pipeline <- pls[[which(names(pls) == c[bst.rf, "name"])]]
    
    plots$randomforest <- qplot(x = seq(1,nrow(c)), 
                                y = y.rf, 
                                geom='line',
                                main = "Random forest pipeline evolution",
                                xlab = "Nr pipelines", 
                                ylab = paste0("Validation set performance: ", metric))
  } else {
    bst.rf <- NULL
  }
  
  if("lasso" %in% models){
    if(max == TRUE){
      bst.lr <- which.max(c$lasso)
      y.lr <- c[order(c$lasso), ]$lasso
    } else {
      bst.lr <- which.min(c$lasso)
      y.lr <- c[order(c$lasso, decreasing = TRUE), ]$lasso
    }
    bst$linear.pipeline <- pls[[which(names(pls) == c[bst.lr, "name"])]]
    
    plots$lasso <- qplot(x = seq(1,nrow(c)), 
                         y = y.lr, 
                         geom='line',
                         main = "Lasso pipeline evolution",
                         xlab = "Nr pipelines", 
                         ylab = paste0("Validation set performance: ", metric))
  } else {
    bst.lr <- NULL
  }
  
  if("lightgbm" %in% models){
    if(max == TRUE){
      bst.lgb <- which.max(c$lightgbm)
      y.lgbm <- c[order(c$lightgbm), ]$lightgbm
    } else {
      bst.lgb <- which.min(c$lightgbm)
      y.lgbm <- c[order(c$lightgbm, decreasing = TRUE), ]$lightgbm
    }
    bst$boosting.pipeline <- pls[[which(names(pls) == c[bst.lgb, "name"])]]
    plots$lightgbm <- qplot(x = seq(1,nrow(c)), 
                            y = y.lgbm, 
                            geom='line',
                            main = "Light gbm pipeline evolution",
                            xlab = "Nr pipelines", 
                            ylab = paste0("Validation set performance: ", metric))
  } else {
    bst.lgb <- NULL
  }
  
  if(max == TRUE){
    bst.mean <- which.max(c$mean.performance)
    c <- c[order(c$mean.performance), ]
  } else {
    bst.mean <- which.min(c$mean.performance)
    c <- c[order(c$mean.performance, decreasing = TRUE), ]
  }
  
  bst$average <- pls[[which(names(pls) == c[bst.mean, "name"])]]
  plots$mean <- qplot(x = seq(1,nrow(c)), 
                      y = c$mean.performance, 
                      geom='line',
                      main = "Average pipeline evolution",
                      xlab = "Nr pipelines", 
                      ylab = paste0("Validation set performance: ", metric))
  
  quiet(h2o.shutdown(prompt = FALSE))
  cat(paste0("lazy | Exlpored ", nrow(c)," out of ",nrow(tmp)," pipelines \n"))
  
  out <- list()
  out$best.pipelines <- bst
  out$summary <- c
  out$plots <- plots
  
  return(out)
}
