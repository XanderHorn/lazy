#' Maximum value scaler
#'
#' Scales all numerical feautes to be between 0 and 1.
#'
#' @param data [required | data.frame] Dataset containing numeric features
#' @param x [required | character] A vector of numeric feature names present in the dataset
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return Data frame with features and max value
#' @export
#' @examples
#' res <- map.max.scaler(data = iris, x = names(iris)[1:4])
#' @author
#' Xander Horn
map.max.scaler <- function(data, x, progress = TRUE){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(x)){
    stop("No numeric features specified in arg 'x'")
  }

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  tmp <- data.frame(feature = x,
                    max.val = NA)
  for(i in 1:length(x)){
    if(max(data[, x[i]], na.rm = TRUE) == 0){
      tmp[i, "max.val"] <- (max(data[, x[i]], na.rm = TRUE) + 1)
    } else {
      tmp[i, "max.val"] <- max(data[, x[i]], na.rm = TRUE)
    }

    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  if(progress == TRUE){
    cat(" \n")
  }
  return(tmp)
}
