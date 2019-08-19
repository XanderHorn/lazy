#' Applies frequency mappings
#' 
#' Uses output from map.freq.encoding to count how many times a value of a feature occurred in the training set and applies it to another dataset
#'
#' @param data [required | data.frame] Dataset containing features to apply mapping to
#' @param freq.mappings [required | list] Output from function map.freq.encode
#' @param progress [optional | logical | default=TRUE] Display a progress bar 
#' @return Data frame with frequency features with all other features
#' @export
#' @examples
#' fm <- map.freq.encoding(data = iris)
#' new_iris <- apply.freq.mappings(data = iris, freq.mappings = fm)
#' @author 
#' Xander Horn
apply.freq.mappings <- function(data, freq.mappings, progress = TRUE){
  
  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }
  
  if(missing(freq.mappings)){
    stop("No mapping list provided to function in arg 'freq.mappings'")
  }
  
  data <- as.data.frame(data)
  start.nrcol <- ncol(data)
  
  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(freq.mappings), style = 3)
  }
  
  for(i in 1:length(freq.mappings)){
    
    tmp <- freq.mappings[[i]]
    
    data <- merge(x = data,
                  y = tmp,
                  by.x = names(freq.mappings)[i],
                  all.x = TRUE)
    
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  
  for(i in (start.nrcol + 1):ncol(data)){
    data[,i] <- ifelse(is.na(data[,i]) == TRUE, 0, data[,i])
  }
  if(progress == TRUE){
    cat(" \n")
  }
  return(data)
}
