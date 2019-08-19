#' Apply max scaler mappings
#' 
#' Applies max scaler mappings to new data. Uses the output from the function max.scale.mappings to apply these mappings.
#'
#' @param data [required | data.frame] Dataset containing features to apply mappings to 
#' @param max.scale.mappings [required | list] Output from function max.scale.mappings
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return Data frame with scaled features
#' @export
#' @examples
#' scaled <- map.max.scaler(data = iris, x = names(iris)[1:4])
#' new_iris <- apply.max.scale.mappings(data = iris, max.scale.mappings = scaled)
#' @author 
#' Xander Horn
apply.max.scale.mappings <- function(data, max.scale.mappings, progress = TRUE){
  
  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }
  
  if(missing(max.scale.mappings)){
    stop("No mapping frame provided to function in arg 'max.scale.mappings'")
  }
  
  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = nrow(max.scale.mappings), style = 3)
  }
  data <- as.data.frame(data)

  for(i in 1:nrow(max.scale.mappings)){
    feat <- as.character(max.scale.mappings[i, "feature"])
    data[, feat] <- data[, feat] / max.scale.mappings[i, "max.val"]
    
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  
  if(progress == TRUE){
    cat(" \n")
  }
  return(data)
}
