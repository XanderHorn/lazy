#' Engineers date and time features 
#' 
#' Engineers date and time features from date time features
#'
#' @param data [required | data.frame] Dataset containing date features 
#' @param x [required | character] A vector of date feature names present in the dataset 
#' @return Data frame with date and time engineered features
#' @export
#' @examples
#' iris$date <- as.Date(now())
#' dt <- date.features(data = iris, x = "date")
#' @author 
#' Xander Horn
date.features <- function(data, x, progress = TRUE){

  library(lubridate)
  
  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }
  
  if(missing(x)){
    stop("No date features specified in arg 'x'")
  }
  
  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }
  
  temp <- as.data.frame(data[, x])
  if(length(x) == 1){
    names(temp) <- x
  }
  
  for(i in 1:length(x)){
    
    temp[, paste0("lazy.year.", x[i])] <- lubridate::year(temp[, x[i]])
    temp[, paste0("lazy.quarter.", x[i])] <- lubridate::quarter(temp[, x[i]])
    temp[, paste0("lazy.month.", x[i])] <- lubridate::month(temp[, x[i]])
    temp[, paste0("lazy.week.", x[i])] <- lubridate::week(temp[, x[i]])
    temp[, paste0("lazy.day.", x[i])] <- lubridate::day(temp[, x[i]])
    temp[, paste0("lazy.weekday.", x[i])] <- lubridate::wday(temp[, x[i]], week_start = 1)
    temp[, paste0("lazy.hour.", x[i])] <- lubridate::hour(temp[, x[i]])
    temp[, paste0("lazy.minute.", x[i])] <- lubridate::minute(temp[, x[i]])
    temp[, paste0("lazy.second.", x[i])] <- lubridate::second(temp[, x[i]])
    
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  
  temp <- temp[, setdiff(names(temp), x)]
  if(progress == TRUE){
    cat(" \n")
  }
  return(temp)
}
