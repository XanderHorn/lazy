#' Applies kmeans feature mappings
#'
#' Applies mapped kmeans features by calculating the distance to center per observation
#'
#' @param data [required | data.frame] Dataset containing features to apply mappings to
#' @param kmeans.mappings [required | list] Output from function map.kmeans.features
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return Data frame with newly added features and original features
#' @export
#' @examples
#' km <- map.kmeans.features(data = iris, x = setdiff(names(iris), "Species"))
#' new_iris <- apply.kmeans.mappings(data = iris, kmeans.mappings = km)
#' @author 
#' Xander Horn
apply.kmeans.mappings <- function(data, kmeans.mappings, progress = TRUE){
  
if(missing(data)){
  stop("No data provided to function in arg 'data'")
}

if(missing(kmeans.mappings)){
  stop("No mapping list provided to function in arg 'kmeans.mappings'")
}
  
library(sqldf)

data <- as.data.frame(data)
t.data <- as.data.frame(data[,names(kmeans.mappings)])
if(length(kmeans.mappings) == 1){
  names(t.data) <- names(kmeans.mappings)
}

if(progress == TRUE){
  pb <- txtProgressBar(min = 0, max = length(kmeans.mappings), style = 3)
}

for(i in 1:length(kmeans.mappings)){
  temp <- kmeans.mappings[[i]]
  
  if(sum(is.na(temp$min)) == 0 & sum(is.na(temp$max)) == 0){
    qry <- paste0("select `",names(kmeans.mappings)[i],"`,  temp.center from `t.data` left join temp on `t.data`.`",names(kmeans.mappings)[i],"` > temp.min and `t.data`.`",names(kmeans.mappings)[i], "` <= temp.max")
    temp <- sqldf(qry)
    temp$dist <- temp[, 1] - temp[, 2] 
    data[, paste0("lazy.dist.",names(kmeans.mappings)[i])] <- ifelse(is.na(temp$dist) == TRUE, -1, temp$dist)
  }
  
  if(progress == TRUE){
    setTxtProgressBar(pb, i)
  }
}
if(progress == TRUE){
  cat(" \n")
}
return(data)
}
