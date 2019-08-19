#' Imputation mappings
#'
#' Creates imputation mapping values for all features provided
#'
#' @param data [required | data.frame] Dataset containing features
#' @param x [required | character] A vector of feature names present in the dataset
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return List of data frames containing imputation mapping values
#' @export
#' @examples
#' res <- map.impute(data = iris, x = names(iris))
#' @author
#' Xander Horn
map.impute <- function(data, x = NULL, progress = TRUE){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(is.null(x) == TRUE){
    x <- names(data)
  }

  data <- as.data.frame(data)

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  mappings <- list()
  for(i in 1:length(x)){

    tmp <- data.frame(feat = NA,
                      lazy.impute.median.mode = NA,
                      lazy.impute.encode = NA,
                      stringsAsFactors = F)
    names(tmp)[1] <- x[i]

    if(class(data[, x[i]]) %in% c("numeric","integer")){
      tmp[, "lazy.impute.median.mode"] <- median(data[, x[i]], na.rm = TRUE)
      tmp[, "lazy.impute.encode"] <- round(min(data[, x[i]], na.rm = TRUE) - 10)
    }

    if(class(data[, x[i]]) %in% c("character","factor","logical")){
      props <- as.data.frame(prop.table(table(data[, x[i]])))
      tmp[, "lazy.impute.median.mode"] <- as.character(props[which.max(props$Freq), 1])
      tmp[, "lazy.impute.encode"] <- "lazy.unknown"
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
