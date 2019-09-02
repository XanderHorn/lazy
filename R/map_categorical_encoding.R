#' Categorical mapping tables
#'
#' Creates a list of mapping tables, one for each categorical feature in the dataset. These tables include engineered features which can then be joined back to the original dataset. Feature engineering techniques include: one hot encoding, ordinal proporitonal encoding, weighted noise target mean encoding given parameter y is provided.
#'
#' @param data [required | data.frame] Dataset containing categorical features
#' @param x [required | character] A vector of categorical feature names present in the dataset
#' @param y [optional | character | default=NULL] The name of the target feature contained in the dataset. If no target is provided mean target encoding will not be calculated.
#' @param max.levels [optional | integer | default=10] The maximum levels allowed for a categorical feature to create one hot encoded features
#' @param min.percent [optional | numeric | default=0.025] The minimum proportion a categorical level is allowed to have before it is flagged as a low proportional level
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @param track.features [optional | logical | default=TRUE] Creates tracking features that records which categories had low proportional values present 
#' @param seed [optional | integer | default=1] The random number seed for reproducable results
#' @return List of data frames containing engineered mapping features
#' @export
#' @examples
#' res <- map.categorical.encoding(data = iris, x = "Species", y = "Sepal.Length")
#' @author
#' Xander Horn
map.categorical.encoding <- function(data, x, y = NULL, max.levels = 10, min.percent = 0.025, track.features = TRUE, seed = 1,
                                     progress = TRUE){

  library(sqldf)

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }
  
  if(missing(x)){
    stop("No categorical features provided to function in arg 'x'")
  }

  set.seed(seed)
  data <- as.data.frame(data[,c(x,y)])
  if(length(x) == 1){
    names(data) <- x
  }

  if(class(data[,y]) %in% c("factor","character")){
    data[,y] <- as.numeric(as.factor(data[,y])) - 1
  }

  mappings <- list()

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  for(i in 1:length(x)){

    if(is.null(y) == FALSE){
      query <- paste0("select `", x[i], "` as level, count(`",x[i],"`) as count, sum(`",y,"`) as sum from data group by `",x[i],"`")
    } else {
      query <- paste0("select `", x[i], "` as level, count(`",x[i],"`) as count from data group by `",x[i],"`")
    }

    temp <- sqldf::sqldf(query)
    temp$proportional.encode <- temp$count / sum(temp$count)
    temp <- temp[order(temp[,"proportional.encode"]),]
    temp$ordinal.encode <- (cumsum(temp$proportional.encode) - 0.5 * temp$proportional.encode) / sum(temp$proportional.encode)
    temp$track.low.prop <- ifelse(temp$count / sum(temp$count) < min.percent, 1, 0)
    temp$report <- ifelse(temp$track.low.prop == 1, "ALL_OTHER", temp$level)
    
    if(is.null(y) == FALSE){
      glb <- sum(temp$sum) / sum(temp$count)
      lambda <- 1/(1 + exp((-1) * (temp$count - 20)/10))
      temp$weighted.target <- ((1 - lambda) * glb) + (lambda * temp$sum/temp$count)
      noise <- runif(temp$level)
      temp$noise.target <- (temp$sum / temp$count) + (noise * 2 * 0.01 - 0.01)
      temp$mean.target <- (temp$weighted.target +temp$noise.target) / 2
    }

    if(nrow(temp) <= max.levels){
      for(j in 1:nrow(temp)){
        val <- temp[j,1]
        temp[,paste0("onehot.all.",temp[j,1])] <- ifelse(temp[,1] == val, 1, 0)
      }
    }
    
    temp$temp <- ifelse(temp$track.low.prop == 1, "other.category", temp$level)
    for(j in 1:nrow(temp)){
      val <- temp[j,"temp"]
      temp[,paste0("onehot.prop.",temp[j,"temp"])] <- ifelse(temp[,"temp"] == val, 1, 0)
    }

    if(track.features == FALSE){
      temp$track.low.prop <- NULL
    }
    
    temp <- temp[,setdiff(names(temp), c("sum","count","noise.target","weighted.target","temp"))]
    names(temp) <- paste0("lazy.",x[i],".",names(temp))
    names(temp)[1] <- x[i]
    names(temp)[2:ncol(temp)] <- make.names(names(temp)[2:ncol(temp)])
    
    
    
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
