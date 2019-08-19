#' De-duplicate dataframe
#' 
#' Ensures only unique observations are kept according to specific features 
#'
#' @param data [required | data.frame]
#' @param id.feats [required | character] Names of ID features or other features to de-duplicate by
#' @return Data frame containing unique observations
#' @export
#' @examples
#' res <- de.duplicate(iris, id.feats = "Sepal.Length")
#' @author 
#' Xander Horn
de.duplicate <- function(data, id.feats){
  
  if(missing(data)){
    stop("Provide data to function")
  }
  
  if(missing(id.feats)){
    stop("Provide id features to function")
  }
  
  library(sqldf)
  
  id.feats <- paste0(paste0("`",id.feats,"`", collapse = ","))
  qry <- paste0("SELECT * FROM data GROUP BY ",id.feats)
  data <- sqldf(qry)
  return(data)
}
