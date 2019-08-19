#' Frequency encoding
#'
#' Creates a list of frequency encoded data.frames for each feature provided.
#'
#' @param data [required | data.frame] Dataset containing categorical features
#' @param x [required | character] A vector of categorical or numerical feature names present in the dataset
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return List of data frames containing engineered mapping features
#' @export
#' @examples
#' res <- map.freq.encoding(iris, names(iris))
#' @author
#' Xander Horn
map.freq.encoding <- function(data, x = NULL, progress = TRUE){

  library(sqldf)

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(is.null(x) == TRUE){
    x = names(data)
  }

  data <- as.data.frame(data)
  data <- data[,x]

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  mappings <- list()
  for(i in 1:length(x)){
    temp <- sqldf(paste0("select `", x[i], "`, count(`",x[i],"`) as count from data group by `",x[i],"`"))
    names(temp)[2] <- paste0("lazy.",x[i],".",names(temp)[2])
    mappings[[i]] <- temp
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
