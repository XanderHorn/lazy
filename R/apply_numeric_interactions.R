#' Creates numeric interaction features
#' 
#' Creates interaction features off of numeric features by summing, dividing, multiplying and deducting. Interactions are created based off of a data.frame of combinations from function map.numeric.features.
#'
#' @param data [required | data.frame] A data.frame object on which the interactions should be applied to containing the base features used to create the combination of interaction features.
#' @param numeric.combination.frame [required | data.frame] A data.frame object containing a combination of features used to created interactions from.
#' @param verbose [optional | logical] Chatty or silent function output
#' @return A data.frame object containing only interacted features
#' @export
#' @examples
#' nim <- map.numeric.interactions(x=names(iris)[1:4])
#' nints <- apply.numeric.interactions(data = iris, numeric.combination.frame = nim)
#' @author 
#' Xander Horn
apply.numeric.interactions <- function(data, numeric.combination.frame, verbose = TRUE){

  if(missing(data)){
    stop("No data provided to function")
  }
  
  if(missing(numeric.combination.frame)){
    stop("No numeric combination frame provided to function")
  }

  x <- unique(c(numeric.combination.frame[,1], numeric.combination.frame[,2]))
  data <- as.data.frame(data[, x])
  
  if(verbose == TRUE){
    pb <- txtProgressBar(min = 0, max = nrow(numeric.combination.frame), style = 3)
  }
  
  for(i in 1:nrow(numeric.combination.frame)){
    
    data[,paste0("lazy.interact.",paste0(numeric.combination.frame[i,], collapse = "."),".sum")] <- as.numeric(data[, numeric.combination.frame[i, 1]]) + as.numeric(data[, numeric.combination.frame[i, 2]])
    data[,paste0("lazy.interact.",paste0(numeric.combination.frame[i,], collapse = "."),".deduct")] <- as.numeric(data[, numeric.combination.frame[i, 1]]) - as.numeric(data[, numeric.combination.frame[i, 2]])
    data[,paste0("lazy.interact.",paste0(numeric.combination.frame[i,], collapse = "."),".multiply")] <- as.numeric(data[, numeric.combination.frame[i, 1]]) * as.numeric(data[, numeric.combination.frame[i, 2]])
    data[,paste0("lazy.interact.",paste0(numeric.combination.frame[i,], collapse = "."),".divide")] <- as.numeric(data[, numeric.combination.frame[i, 1]]) / as.numeric((data[, numeric.combination.frame[i, 2]] + 1))
    
    if(verbose == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  
  data <- data[,setdiff(names(data), x)]
  
  return(data)
}

