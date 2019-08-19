#' Minor feature class changes
#'
#' Alters features in a dataset to change from factors to characters and logicals to numerics
#'
#' @param data [required | data.frame] Dataset containing features
#' @return Formatted dataset
#' @export
#' @examples
#' res <- quick.format(iris)
#' @author
#' Xander Horn
quick.format <- function(data){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  data <- as.data.frame(data)

  for(i in 1:ncol(data)){

    if(class(data[, i]) == "factor"){
      data[, i] <- as.character(data[, i])
    }

    if(class(data[ ,i]) == "logical"){
      data[, i] <- as.numeric(data[, i])
    }
  }

  return(data)
}
