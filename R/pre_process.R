#' Pre-processing of data
#'
#' Pre-processes data based on a defined pipeline. If the mapping list is provided in addition to the pipeline then the function runs in production mode.
#'
#' @param data [required | data.frame] Data frame to pre-process
#' @param x [optional | character | default=NULL] A vector of feature names present in the dataset to be used as input features. If NULL uses all of the features in the dataset except for id and y features if provided.
#' @param y [optional | character | default=NULL] The name of the target feature contained in the dataset. If NULL then data will be processed without relying on the target feature.
#' @param id.feats [optional | character | default=NULL] Names of ID features. If provided then the data set will be de-duplicated according to the ID features provided.
#' @param pipeline [required | list] List object. When initially preparing data the list object is returned by the function design.pipeline else when processing data again eg. a production environment the list object from the function pre.process.
#' @param mapping.list [optional | list | default=NULL] List of mapping tables. If NULL then function will run in development mode and produce a list of mapping tables included in the output.
#' @param verbose [logical | optional | default=TRUE] Chatty or silent function output.
#' @return List containing a pre-processed dataset, mapping list and designed pipeline.
#' @export
#' @examples
#' res <- pre.process(data = iris, y = "Species", pipeline = design.pipeline(pipeline.name = "iris.quick", kmeans.features = T))
#' @author
#' Xander Horn
pre.process <- function(data, x = NULL, y = NULL, id.feats = NULL, pipeline, mapping.list = NULL, verbose = TRUE){

  if(missing(data)){
    stop("Provide data to function in arg 'data'")
  }

  start.time <- Sys.time()

  if(is.null(mapping.list) == TRUE & verbose == TRUE){
    cat("lazy | Running in development mode \n")
  }
  
  set.seed(pipeline$settings$seed)
  data <- as.data.frame(data)

  if(is.null(x) == TRUE){
    x <- names(data)
  }

  if(is.null(y) == FALSE){
    x <- setdiff(x, y)
  }

  if(is.null(id.feats) == FALSE & is.null(mapping.list) == TRUE){
    if(verbose == TRUE){
      cat("lazy | Checking for duplicate observations based on ID features and removing \n")
    }
    data <- de.duplicate(data = data, id.feats = id.feats)
    x <- setdiff(x, id.feats)
  }

  if(is.null(mapping.list) == TRUE){
    check <- FALSE
    mapping.list <- list()
    data <- data[,c(x, y, id.feats)]
  } else {
    check <- TRUE
    x <- pipeline$base.features
  }
  
  if(verbose == TRUE & check == FALSE){
    pr <- TRUE
  } else {
    pr <- FALSE
  }

  data <- quick.format(data)

  if(check == FALSE){
    if(verbose == TRUE){
      cat("lazy | Checking for constant, all missing and duplicated features and removing \n")
    }
    start.eda <- describe(data = data, progress = FALSE)
    start.eda <- subset(start.eda, !start.eda$feature %in% c(id.feats, y))
    remove <- as.character(start.eda[which(start.eda$all.na == 1), "feature"])
    remove <- c(remove, as.character(start.eda[which(start.eda$constant == 1), "feature"]))
    remove <- c(remove, as.character(start.eda[which(start.eda$duplicate == 1), "feature"]))
    data <- data[, setdiff(names(data), remove)]
    start.eda <- subset(start.eda, start.eda$feature %in% names(data))
    pipeline$features$transformed.features <- as.character(start.eda[which(start.eda$skewness >= pipeline$settings$transform.cutoff | start.eda$skewness <= (pipeline$settings$transform.cutoff * -1)), "feature"])
  }

  if(check == FALSE & verbose == TRUE){
    cat("lazy | Detecting feature types \n")
  }
  feats <- detect.feats(data =  data, progress = pr)
  feats <- subset(feats, !feats$feature %in% c(id.feats, y))
  pipeline$features$numeric.features <- as.character(feats[which(feats$category == "numeric"), "feature"])
  pipeline$features$categorical.features <- as.character(feats[which(feats$category == "categorical"), "feature"])
  pipeline$features$indicator.features <- as.character(feats[which(feats$category == "indicator"), "feature"])
  pipeline$features$date.features <- as.character(feats[which(feats$category == "date"), "feature"])
  pipeline$features$text.features <- as.character(feats[which(feats$category == "text"), "feature"])
  remove <- as.character(feats[which(feats$category == "unknown"), "feature"])
  data <- data[, setdiff(names(data), remove)]

  if(length(pipeline$features$numeric.features) < pipeline$settings$numeric.interaction.feats){
    pipeline$settings$numeric.interaction.feats <- length(pipeline$features$numeric.features)
  }

  if(length(pipeline$features$numeric.features) == 0){
    pipeline$settings$outlier.clipping <- FALSE
    pipeline$settings$max.scaling <- FALSE
    pipeline$settings$kmeans.features <- FALSE
    pipeline$settings$numeric.transform <- FALSE
    pipeline$settings$numeric.interactions <- FALSE
  }

  if(length(pipeline$features$transformed.features) == 0){
    pipeline$settings$numeric.transform <- FALSE
  }

  if(length(pipeline$features$categorical.features) == 0){
    pipeline$settings$categorical.encoding <- FALSE
    pipeline$settings$categorical.interactions <- FALSE
  }

  if(length(pipeline$features$date.features) > 0 & pipeline$settings$date.features == TRUE){
    if(check == FALSE & verbose == TRUE){
      cat("lazy | Engineering date features \n")
    }
    date.feats <- date.features(data = data,
                                x = pipeline$features$date.features,
                                progress = pr)
    data <- cbind(data, date.feats)
    dte <- names(date.feats)
  } else {
    dte <- NULL
  }

  if(length(pipeline$features$text.features) > 0 & pipeline$settings$text.features == TRUE){
    if(check == FALSE & verbose == TRUE){
      cat("lazy | Engineering text features \n")
    }
    text.feats <- text.features(data = data,
                                x = pipeline$features$text.features,
                                progress = pr)
    data <- cbind(data, text.feats)
    txt <- names(text.feats)
  } else {
    txt <- NULL
  }

  if(pipeline$settings$numeric.transform == TRUE){
    if(check == FALSE & verbose == TRUE){
      cat("lazy | Transforming numerical skewed features \n")
    }
    trnsfm <- numeric.transformers(data = data,
                                   x = pipeline$features$transformed.features,
                                   transform.type = pipeline$settings$transform.mode,
                                   progress = pr)
    data <- cbind(data, trnsfm)
    skewed <- names(trnsfm)
  } else {
    skewed <- NULL
  }

  if(check == FALSE & pipeline$settings$impute.missing == TRUE){
    if(verbose == TRUE){
      cat("lazy | Mapping feature imputation frames \n")
    }
    mapping.list$impute.mappings <- map.impute(data = data,
                                           x = c(pipeline$features$numeric.features, dte, txt, pipeline$features$categorical.features, pipeline$features$indicator.features, skewed),
                                           progress = pr)
  }

  if(is.null(mapping.list$impute.mappings) == FALSE){
    if(verbose == TRUE){
      cat("lazy | Applying feature imputation frames \n")
    }
    data <- apply.impute.mappings(data = data,
                                  impute.mappings = mapping.list$impute.mappings,
                                  impute.mode = pipeline$settings$impute.mode,
                                  na.threshold = pipeline$settings$impute.threshold,
                                  track.features = pipeline$settings$impute.tracking,
                                  progress = pr)
  }

  if(check == FALSE & pipeline$settings$freq.encode == TRUE & length(c(pipeline$features$categorical.features, pipeline$features$indicator.features)) > 0){
    if(verbose == TRUE){
      cat("lazy | Mapping feature frequency frames \n")
    }
    mapping.list$freq.mappings <- map.freq.encoding(data = data,
                                                    x = c(pipeline$features$categorical.features, pipeline$features$indicator.features),
                                                    progress = pr)
  }

  if(is.null(mapping.list$freq.mappings) == FALSE){
    if(verbose == TRUE){
      cat("lazy | Applying feature frequency frames \n")
    }
    data <- apply.freq.mappings(data = data,
                                freq.mappings = mapping.list$freq.mappings,
                                progress = pr)
  }

  if(is.null(y) == FALSE & check == FALSE & (pipeline$settings$numeric.interactions == TRUE | pipeline$setting$categorical.interactions == TRUE)){
    if(verbose == TRUE){
      cat("lazy | Identifying most important features \n")
    }
    imp <- feature.importance(data = data,
                              x = c(pipeline$features$numeric.features, pipeline$features$categorical.features),
                              y = y,
                              verbose = F,
                              cluster.shutdown = F,
                              seed = pipeline$settings$seed)$importance.table
    
    nf <- subset(imp, imp$feature.class == "numeric")
    cf <- subset(imp, imp$feature.class == "categorical")
    
    if(length(pipeline$features$numeric.features) > 1){
      if(nrow(nf) > pipeline$settings$numeric.interaction.feats){
        pipeline$features$numeric.interaction.feats <- as.character(nf[1:pipeline$settings$numeric.interaction.feats, "feature"])
      } else {
        pipeline$features$numeric.interaction.feats <- as.character(nf[,"feature"])
      }
    }
    
    if(length(pipeline$features$categorical.features) > 1){
      if(nrow(cf) > pipeline$settings$categorical.interaction.feats){
        pipeline$features$categorical.interaction.feats <- as.character(cf[1:pipeline$settings$categorical.interaction.feats, "feature"])
      } else {
        pipeline$features$categorical.interaction.feats <- as.character(cf[,"feature"])
      }
    }
  }

  if(check == FALSE & pipeline$settings$categorical.encoding == TRUE & pipeline$settings$categorical.interactions == TRUE){

    if(length(pipeline$features$categorical.features) >= 2){
      if(verbose == TRUE){
        cat("lazy | Mapping categorical interaction features \n")
      }
      
      mapping.list$categorical.interactions <- map.categorical.interactions(x = pipeline$features$categorical.interaction.feats,
                                                                            n.interactions = pipeline$settings$categorical.interactions.levels)
    }
  }
  
  if(is.null(mapping.list$categorical.interactions) == FALSE){
    if(verbose == TRUE){
      cat("lazy | Applying categorical interaction features \n")
    }
    cints <- apply.categorical.interaction.mappings(data = data,
                                                   categorical.interactions.mappings = mapping.list$categorical.interactions,
                                                   verbose = pr)
    cat <- c(pipeline$features$categorical.features, names(cints))
    data <- cbind(data, cints)
  } else {
    cat <- pipeline$features$categorical.features
  }

  if(check == FALSE & pipeline$settings$categorical.encoding == TRUE){
    if(verbose == TRUE){
      cat("lazy | Mapping feature categorical encoding frames \n")
    }
    mapping.list$categorical.mappings <- map.categorical.encoding(data = data,
                                                              x = cat,
                                                              y = y,
                                                              max.levels = pipeline$settings$categorical.max.levels,
                                                              min.percent = pipeline$settings$categorical.min.percent,
                                                              track.features = pipeline$settings$categorical.tracking,
                                                              seed = pipeline$settings$seed,
                                                              progress = pr)
  }

  if(is.null(mapping.list$categorical.mappings) == FALSE){
    if(verbose == TRUE){
      cat("lazy | Applying feature categorical encoding frames \n")
    }
    data <- apply.categorical.mappings(data = data,
                                       categorical.mappings = mapping.list$categorical.mappings,
                                       map.mode = pipeline$settings$categorical.mode,
                                       progress = pr)
    data <- data[,setdiff(names(data), names(mapping.list$categorical.mappings))]
  }

  if(check == FALSE & pipeline$settings$outlier.clipping == TRUE){
    if(verbose == TRUE){
      cat("lazy | Mapping feature outlier frames \n")
    }
    mapping.list$outlier.mappings <- map.outliers(data = data,
                                              x = pipeline$features$numeric.features,
                                              outlier.mode = pipeline$settings$outlier.mode,
                                              lower.percentile = pipeline$settings$outlier.lower.percentile,
                                              upper.percentile = pipeline$settings$outlier.upper.percentile,
                                              progress = pr)
  }

  if(is.null(mapping.list$outlier.mappings) == FALSE){
    if(verbose == TRUE){
      cat("lazy | Applying feature outlier frames \n")
    }
    data <- apply.outlier.mappings(data = data,
                                   outlier.mappings = mapping.list$outlier.mappings,
                                   progress = pr,
                                   track.features = pipeline$settings$outlier.tracking)
  }

  if(check == FALSE & pipeline$settings$kmeans.features == TRUE){
    if(verbose == TRUE){
      cat("lazy | Mapping feature k-means frames \n")
    }
    mapping.list$kmeans.mappings <- tryCatch({
      map.kmeans.features(data = data,
                          x = pipeline$features$numeric.features,
                          seed = pipeline$settings$seed,
                          progress = pr)
    }, error=function(e) {
      return(NULL)
    })
  }
  
  if(is.null(mapping.list$kmeans.mappings) == FALSE){
    if(verbose == TRUE){
      cat("lazy | Applying feature k-means frames \n")
    }
    data <- apply.kmeans.mappings(data = data,
                                  kmeans.mappings = mapping.list$kmeans.mappings,
                                  progress = pr)
  }
  
  if(check == FALSE & pipeline$settings$numeric.interactions == TRUE & length(pipeline$features$numeric.interaction.feats) >= 2){
    if(verbose == TRUE){
      cat("lazy | Mapping numerc interaction features \n")
    }
    mapping.list$numeric.interactions <- map.numeric.interactions(x = pipeline$features$numeric.interaction.feats)
  }
  
  if(is.null(mapping.list$numeric.interactions) == FALSE){
    if(verbose == TRUE){
      cat("lazy | Applying numerc interaction features \n")
    }
    nint <- apply.numeric.interactions(data = data,
                                       numeric.combination.frame = mapping.list$numeric.interactions,
                                       verbose = pr)
    data <- cbind(data, nint)
  }

  if(check == FALSE & pipeline$settings$max.scaling == TRUE){
    if(verbose == TRUE){
      cat("lazy | Mapping feature scaling frames \n")
    }
    mapping.list$max.scale.mappings <- map.max.scaler(data = data,
                                                      x = pipeline$features$numeric.features,
                                                      progress = pr)
  }

  if(is.null(mapping.list$max.scale.mappings) == FALSE){
    if(verbose == TRUE){
      cat("lazy | Applying feature scaling frames \n")
    }
    data <- apply.max.scale.mappings(data = data,
                                     max.scale.mappings = mapping.list$max.scale.mappings,
                                     progress = pr)
  }

  if(pipeline$name != "Data Exploration" & check == FALSE){
    remove <- apply(data, 2, function(x) length(unique(x)))
    remove <- names(remove)[which(remove == 1)]
    data <- data[, setdiff(names(data), remove)]
  }
  
  if(pipeline$settings$numeric.transform == TRUE){
    data <- data[,setdiff(names(data), pipeline$features$transformed.features)]
  }
  
  if(check == FALSE){
    pipeline$features$final.features <- names(data)
  } 
  
  pipeline$information$created.date <- start.time
  pipeline$information$duration <- Sys.time() - start.time

  out <- list()
  if(check == FALSE){
    out$pipeline <- pipeline
    out$mapping.list <- mapping.list
    out$data <- data
    return(out)
  } else {
    return(data)
  }
}
