#' Numeric feature transformations
#'
#' Applies different transformations to numeric features such as sqrt and log transforms.
#'
#' @param data [required | data.frame] Dataset containing numeric features
#' @param x [required | character] A vector of numeric feature names present in the dataset
#' @param transform.type [optional | character | default="log"] Transform type, options are log or sqrt.
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return Data frame containing transformed features
#' @export
#' @examples
#' res <- numeric.transformers(data = iris, x = "Sepal.Length")
#' @author
#' Xander Horns
numeric.transformers <- function(data, x, transform.type = "log", progress = TRUE){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(x)){
    stop("No categorical features specified in arg 'x'")
  }

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  temp <- as.data.frame(data[, x])
  if(length(x) == 1){
    names(temp) <- x
  }

  for(i in 1:length(x)){

    if(class(temp[,x[i]]) %in% c("numeric", "integer")){
      if(transform.type == "log"){
        temp[, paste0("lazy.log.", x[i])] <- log((temp[, 1] + 1))
      }
      
      if(transform.type == "sqrt"){
        temp[, paste0("lazy.sqrt.", x[i])] <- sqrt(temp[, 1])
      }
      
      if(progress == TRUE){
        setTxtProgressBar(pb, i)
      }
    }
  }

  temp <- temp[,setdiff(names(temp), x)]
  if(progress == TRUE){
    cat(" \n")
  }
return(temp)
}
