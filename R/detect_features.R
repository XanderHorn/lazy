#' Detect feature types
#'
#' Detects the type of features present in a data.frame
#'
#' @param data [required | data.frame] Dataset containing categorical features
#' @param x [required | character] A vector of numeric feature names present in the dataset
#' @param text.threshold [optional | integer | default=100] Maximum characters in a feature before it is identified as a text feature
#' @param sample.size [optional | numeric | default=0.3] Percentage to down sample data for decreased computation time
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return Data.frame with categorical, numeric, date, and text feature types
#' @export
#' @examples
#' res <- detect.feats(iris)
#' @author
#' Xander Horn
detect.feats <- function(data, x = NULL, text.threshold = 100, sample.size = 0.3, progress = TRUE){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(sample.size > 1 | sample.size <= 0){
    warning("sample.size restricted between 0 and 1, defaulting to 0.3")
    sample.size <- 0.3
  }

  data <- as.data.frame(data)
  data <- data[sample(nrow(data), sample.size * nrow(data), replace = F), ]

  if(is.null(x) == TRUE){
    x <- names(data)
  }

  feats <- data.frame(feature = x,
                      category = NA,
                      flag = NA,
                      chars = NA)

  if (progress == TRUE) {
    pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)
  }

  for(i in 1:length(x)){
    if(class(data[, x[i]]) %in% c("character")){
      feats[i, "chars"] <- max(nchar(data[, x[i]]))
    }

    feats[i, "category"] <- class(data[, x[i]])
    vals <- unique(data[, x[i]])
    
    if(length(vals) == 2){
      feats[i, "flag"] <- ifelse(sum(tolower(vals) %in% c(0,1,"y","n","1","0","true","false","yes","no"), na.rm = TRUE) == 2, 1, 0)
      feats[i, "flag"] <- ifelse(sum(tolower(vals) %in% c("-1","1","t","f"), na.rm = TRUE) == 2, 1, feats[i, "flag"])
    }

    if (progress == TRUE) {
      setTxtProgressBar(pb, i)
    }
  }
  feats$chars <- ifelse(is.na(feats$chars) == TRUE, 0, feats$chars)
  feats$category <- ifelse(feats$chars >= text.threshold, "text", feats$category)
  feats$category <- ifelse(feats$category %in% c("integer", "numeric", "logical"), "numeric", feats$category)
  feats$category <- ifelse(feats$category %in% c("character", "factor"), "categorical", feats$category)
  feats$category <- ifelse(feats$category %in% c("Date", "POSIXct", "POSIXt"), "date", feats$category)
  feats$category <- ifelse(feats$category %in% c("text", "numeric", "categorical", "date", "indicator"), feats$category, "unknown")
  feats$chars <- NULL
  
  feats[which(feats$flag == 1), "category"] <- "indicator"
  feats$flag <- NULL
  if(progress == TRUE){
    cat(" \n")
  }
  return(feats)
}

