#' Applies imputation mappings
#'
#' Uses output from map.impute to impute NA values in a dataframe. Also creates tracking features to indicate which observations had NA values.
#'
#' @param data [required | data.frame] Dataset containing features to impute
#' @param impute.mappings [required | list] Output from function map.impute
#' @param impute.mode [optional | character | default="auto"] Imputation mode, options are auto, encode and median.mode. Auto applies a combination bewteen encoding and median.mode imputation based on the na.threshold parameter
#' @param na.threshold [optional | numeric | default=0.1] Threshold for auto impute.mode to apply encoding or median.mode imputation. All features containing Na values above the specified percentage threshold will be imputed using encoding
#' @param track.features [optional | logical | default=TRUE] Creates tracking features that records which observations had NA values present
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return Data frame with imputed original features and tracking features
#' @export
#' @examples
#' imp <- map.impute(data = iris)
#' new_iris <- apply.impute.mappings(iris, imp)
#' @author
#' Xander Horn
apply.impute.mappings <- function(data, impute.mappings, impute.mode = "auto", na.threshold = 0.1, track.features = TRUE, progress = TRUE){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(impute.mappings)){
    stop("No mapping list provided to function in arg 'impute.mappings'")
  }

  if(na.threshold <= 0){
    na.threshold <- 0.1
    warning("`na.threshold` limited between 0 and 1. Defaulting to 0.1")
  }

  if(na.threshold >= 1){
    na.threshold <- 0.1
    warning("`na.threshold` limited between 0 and 1. Defaulting to 0.1")
  }

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(impute.mappings), style = 3)
  }

  data <- as.data.frame(data)

  eda <- describe(data, progress = FALSE)
  eda <- eda[, c("feature", "observations", "missing")]
  eda$pct.missing <- eda$missing / eda$observations

  for(i in 1:length(impute.mappings)){
    tmp <- impute.mappings[[i]]
    feat <- names(impute.mappings)[i]
    pct.missing <- eda[which(eda$feature == feat), "pct.missing"]
    
    if(is.nan(pct.missing) == TRUE){
      pct.missing <- 0
    }

    if(impute.mode == "auto"){
      if(pct.missing >= na.threshold){
        if(track.features == TRUE){
          data[, paste0("lazy.track.impute.encode.",feat)] <- ifelse(is.na(data[, feat]) == TRUE, 1, 0)
        }
        data[, feat] <- ifelse(is.na(data[, feat]) == TRUE, tmp$lazy.impute.encode, data[, feat])
      } else {
        if(track.features == TRUE){
          data[, paste0("lazy.track.impute.median.mode.",feat)] <- ifelse(is.na(data[, feat]) == TRUE, 1, 0)
        }
        data[, feat] <- ifelse(is.na(data[, feat]) == TRUE, tmp$lazy.impute.median.mode, data[, feat])
      }
    }

    if(impute.mode == "median.mode"){
      if(track.features == TRUE){
        data[, paste0("lazy.track.impute.median.mode.",feat)] <- ifelse(is.na(data[, feat]) == TRUE, 1, 0)
      }
      data[, feat] <- ifelse(is.na(data[, feat]) == TRUE, tmp$lazy.impute.median.mode, data[, feat])
    }

    if(impute.mode == "encode"){
      if(track.features == TRUE){
        data[, paste0("lazy.track.impute.encode.",feat)] <- ifelse(is.na(data[, feat]) == TRUE, 1, 0)
      }
      data[, feat] <- ifelse(is.na(data[, feat]) == TRUE, tmp$lazy.impute.encode, data[, feat])
    }

    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  if(progress == TRUE){
    cat(" \n")
  }
  return(data)
}
