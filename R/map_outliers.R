#' Creates mapping tables for detecting outlier values in numeric features
#'
#' Identifies outliers by using either tukey or a percentile based approach. For each feature any value greater than or less than the outlier values are seen as outliers.
#'
#' @param data [required | data.frame] Dataset containing features
#' @param x [required | character] A vector of numeric feature names present in the dataset
#' @param outlier.mode [optional | character | default="tukey"] Mode to identify outliers. Options are tukey or percentile to identify outliers.
#' @param lower.percentile [optional | numeric | default=0.05] Lower percentile used to identify outliers if mode is set to percentile
#' @param upper.percentile [optional | numeric | default=0.95] Upper percentile used to identify outliers if mode is set to percentile
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return List of mapping tables
#' @export
#' @examples
#' om <- map.outliers(iris, x = names(iris)[1:4])
#' @author 
#' Xander Horn
map.outliers <- function(data, x, outlier.mode = "tukey", lower.percentile = 0.05, upper.percentile = 0.95, progress = TRUE){

if(missing(data)){
  stop("No data provided to function in arg 'data'")
}

if(missing(x)){
  stop("No numeric features specified in arg 'x'")
}

if(lower.percentile <= 0){
  lower.percentile <- 0.05
  warning("lower.percentile limited between 0-1, defaulting to 0.05")
}

if(upper.percentile >=  1){
  upper.percentile <- 0.95
  warning("upper.percentile limited between 0-1, defaulting to 0.95")
}

data <- as.data.frame(data)

if(progress == TRUE){
  pb <- txtProgressBar(min = 0, max = length(x), style = 3)
}

mappings <- list()
for(i in 1:length(x)){
  tmp <- data.frame(feature = x[i],
                    lower.outlier = NA,
                    upper.outlier = NA,
                    stringsAsFactors = F)
  
  if(outlier.mode == "tukey"){
    tmp$lower.outlier <- quantile(data[, x[i]], probs = 0.25, na.rm = TRUE)[[1]] - (1.5 * IQR(data[, x[i]], na.rm = TRUE))
    tmp$upper.outlier <- quantile(data[, x[i]], probs = 0.75, na.rm = TRUE)[[1]] + (1.5 * IQR(data[, x[i]], na.rm = TRUE))
  }
  
  if(outlier.mode == "percentile"){
    tmp$lower.outlier <- quantile(data[, x[i]], probs = lower.percentile, na.rm = TRUE)[[1]]
    tmp$upper.outlier <- quantile(data[, x[i]], probs = upper.percentile, na.rm = TRUE)[[1]]
  }
  
  mappings[[i]] <- tmp
  
  if(progress == TRUE){
    setTxtProgressBar(pb, i)
  }
  
}

names(mappings) <- x
if(progress == TRUE){
  cat(" \n")
}
return(mappings)
}