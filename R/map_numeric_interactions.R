#' Numeric feature interactions
#'
#' Creates a data frame containing two-way combinations of all features to be used when creating interactions
#'
#' @param x [required | character] A vector of numeric feature names present in the dataset
#' @return Data frame with combinations of all features to be interacted
#' @export
#' @examples
#' nim <- map.numeric.interactions(x = names(iris)[1:4])
#' @author
#' Xander Horn
map.numeric.interactions <- function(x){
  
  if(missing(x)){
    stop("No numeric features specified in arg 'x'")
  }
  
  if(length(x) < 2){
    stop("Require more than one feature to compute interactions")
  }
  
  comb <- as.data.frame(t(combn(x, 2)), stringsAsFactors = FALSE)
  
  return(comb)
}