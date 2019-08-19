#' Applies categorical mappings
#' 
#' Applies feature engineered mapping tables for categorical features. Uses the output from the function map.categorical.encoding to apply these mappings.
#'
#' @param data [required | data.frame] Dataset containing features to apply mappings to
#' @param categorical.mappings [required | list] Output from function map.categorical.encoding
#' @param map.mode [optional | character | default="auto"] Type of mappings to apply. Options are auto, target, proportional, ordinal, onehot, onehot.prop, report, where auto is a combination between onehot and target. Tracking features are created which flags if a feature has a low proportional category in it. Other types of feature engineering includes, weighted mean noise target encoding, proportional encoding, ordinal proportional encoding, one hot encoding and low proportional one hot encoding which flags all low proportional categories as "other". Report cleans up levels so that the data can be represented in reports and charts.
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return Data frame with newly added features and original features
#' @export
#' @examples
#' ce <- map.categorical.encoding(data = iris,x = "Species", y = "Petal.Width")
#' new_iris <- apply.categorical.mappings(data = iris, categorical.mappings = ce)
#' @author
#' Xander Horn
apply.categorical.mappings <- function(data, categorical.mappings, map.mode = "auto", progress = TRUE){


  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(categorical.mappings)){
    stop("No mapping list provided to function in arg 'categorical.mappings'")
  }

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(categorical.mappings), style = 3)
  }

  data <- as.data.frame(data)

  for(i in 1:length(categorical.mappings)){
    tmp <- categorical.mappings[[i]]

    if(map.mode == "auto"){
      ind <- grep("onehot.all", names(tmp))
      if(length(ind) == 0){
        ind <- grep("mean.target", names(tmp))
      }
      ind <- c(ind, grep("low.prop", names(tmp)))
      ind <- c(1,ind)
    }
    
    if(map.mode == "report"){
      ind <- grep("report", names(tmp))
      ind <- c(1,ind)
    }

    if(map.mode == "target"){
      ind <- grep("mean.target", names(tmp))
      ind <- c(ind, grep("low.prop", names(tmp)))
      ind <- c(1,ind)
    }

    if(map.mode == "proportional"){
      ind <- grep("proportional.encode", names(tmp))
      ind <- c(ind, grep("low.prop", names(tmp)))
      ind <- c(1,ind)
    }

    if(map.mode == "ordinal"){
      ind <- grep("ordinal.encode", names(tmp))
      ind <- c(ind, grep("low.prop", names(tmp)))
      ind <- c(1,ind)
    }

    if(map.mode == "onehot"){
      ind <- grep("onehot.all", names(tmp))
      if(length(ind) == 0){
        ind <- grep("ordinal", names(tmp))
      }
      ind <- c(ind, grep("low.prop", names(tmp)))
      ind <- c(1,ind)
    }

    if(map.mode == "onehot.prop"){
      ind <- grep("onehot.prop", names(tmp))
      ind <- c(ind, grep("low.prop", names(tmp)))
      ind <- c(1,ind)
    }
    
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }

    data <- merge(x = data,
                  y = tmp[,ind],
                  by.x = names(categorical.mappings)[i],
                  all.x = TRUE)
  }

  data <- data[, setdiff(names(data), names(categorical.mappings))]
  if(progress == TRUE){
    cat(" \n")
  }
  return(data)
}
