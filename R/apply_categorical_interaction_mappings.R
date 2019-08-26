#' Applies categorical interacion mappins
#' 
#' Creates categorical interaction features by combining categorical features.
#'
#' @param data [required | data.frame] Dataset containing categorical features 
#' @param categorical.interactions.mappings  [ required | list] Mapping list produced by function map.categorical.interactions.
#' @param verbose [optional | logical | default=TRUE] Chatty or silent function output
#' @return A data.frame object with interacted features
#' @export
#' @examples
#' cim <- map.categorical.interactions(x = c("Class","Sex","Age"))
#' cints <- apply.categorical.interaction.mappings(data=as.data.frame(Titanic), categorical.interactions.mappings = cim)
#' @author 
#' Xander Horn
apply.categorical.interaction.mappings <- function(data, categorical.interactions.mappings, verbose = TRUE){

  if(missing(data)){
    stop("No data provided to function")
  }
  
  if(missing(categorical.interactions.mappings)){
    stop("No mapping list provided to function")
  }
  
  if(verbose == TRUE){
    pb <- txtProgressBar(min = 0, max = length(categorical.interactions.mappings), style = 3)
  }
  x <- names(data)
  
  for(i in 1:length(categorical.interactions.mappings)){
    c <- categorical.interactions.mappings[[i]]
    for(j in 1:nrow(c)){
      data[,paste0("interact.",paste0(c[j,], collapse = "."))] <- do.call(paste0, as.data.frame(data[,paste0(c[j,])], stringsAsFactors=FALSE))
    }
    if(verbose == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  
  data <- data[,setdiff(names(data), x)]
  if(verbose == TRUE){
    cat(" \n")
  }

  return(data)
}
