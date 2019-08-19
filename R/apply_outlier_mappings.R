#' Applies outlier mapping tables
#' 
#' Clips outliers by applying mapping tables to respective features. Also creates tracking features to identify which observations had outliers present for specific features.
#'
#' @param data [required | data.frame] Dataset containing features
#' @param outlier.mappings [required | list] Output from function map.outliers
#' @param track.features [optional | logical | default=TRUE] Creates tracking features that records which observations had outliers present
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return Data frame with clipped features and all other features
#' @export
#' @examples
#' om <- map.outliers(iris, x = names(iris)[1:4])
#' new_iris <- apply.outlier.mappings(data, om)
#' @author 
#' Xander Horn
apply.outlier.mappings <- function(data, outlier.mappings, progress = TRUE, track.features = TRUE){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }
  
  if(missing(outlier.mappings)){
    stop("No mapping list provided to function in arg 'outlier.mappings'")
  }
  
  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(outlier.mappings), style = 3)
  }
  
  data <- as.data.frame(data)
  
  for(i in 1:length(outlier.mappings)){
    tmp <- outlier.mappings[[i]]
    
    if(track.features == TRUE){
      data[, paste0("lazy.track.outlier.",names(outlier.mappings)[i])] <- ifelse(data[, names(outlier.mappings)[i]] < tmp$lower.outlier, 1, 
                                                                                 ifelse(data[, names(outlier.mappings)[i]] > tmp$upper.outlier, 1, 0))
    }
    
    data[, names(outlier.mappings)[i]] <- ifelse(data[, names(outlier.mappings)[i]] < tmp$lower.outlier, tmp$lower.outlier, data[, names(outlier.mappings)[i]])
    data[, names(outlier.mappings)[i]] <- ifelse(data[, names(outlier.mappings)[i]] > tmp$upper.outlier, tmp$upper.outlier, data[, names(outlier.mappings)[i]])
    
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  if(progress == TRUE){
    cat(" \n")
  }
  return(data)
}

