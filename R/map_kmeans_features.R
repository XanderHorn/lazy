#' Kmeans distance to center feature mappings
#'
#' Creates mapping tables for each numerical feature containing the center for each feature's min and max value. These tables can then be applied using the function 'apply.kmeans.mappings' to calculate the distance to cluster center for each feature. Each feature is scaled by converting it to a range between 0 and 1 before clustering.
#'
#' @param data [required | data.frame] Dataset containing categorical features
#' @param x [required | character] A vector of categorical feature names present in the dataset
#' @param clusters [optional | integer | default=3] The number of clusters to create in each feature
#' @param seed [optional | integer| default=1] The random number seed for reproducable results
#' @param sample.size [optional | numeric | default=0.3] Percentage to down sample data for decreased computation time
#' @param progress [optional | logical | default=TRUE] Display a progress bar
#' @return List of data frames containing mapping tables
#' @export
#' @examples
#' res <- map.kmeans.features(data = iris, x = setdiff(names(iris), "Species"))
#' @author
#' Xander Horn
map.kmeans.features <- function(data, x, clusters = 3, sample.size = 0.3, seed = 1, progress = TRUE){

  library(sqldf)

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(x)){
    stop("No numerical features specified in arg 'x'")
  }

  if(clusters < 2){
    stop("Clusters need to be 2 or more")
  }

  if(sample.size > 1 | sample.size <= 0){
    warning("sample.size restricted between 0 and 1, defaulting to 0.3")
    sample.size <- 0.3
  }

  set.seed(seed)

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  temp <- as.data.frame(data[, x])
  temp <- temp[sample(nrow(temp), sample.size * nrow(temp), replace = F), ]

  mappings <- list()
  for(i in 1:length(x)){

    if(length(unique(temp[, x[i]])) > 1){
      
      if(clusters > length(unique(temp[, x[i]]))){
        nr.clusters <- length(unique(temp[, x[i]]))
      } else {
        nr.clusters <- clusters
      }
  
      if(max(temp[, x[i]]) == 0){
        toCluster <- temp[, x[i]] / max(temp[, x[i]] + 1, na.rm = TRUE)
      } else {
        toCluster <- temp[, x[i]] / max(temp[, x[i]], na.rm = TRUE)
      }
  
      clst <- suppressWarnings(kmeans(x = toCluster, centers = nr.clusters))
      lookup <- as.data.frame(temp[, x[i]])
      names(lookup) <- x[i]
      lookup$cluster <- clst$cluster
      centers <- data.frame(cluster = row.names(clst$centers),
                            center = clst$centers)
      lookup <- merge(x = lookup,
                      y = centers,
                      by.x = "cluster",
                      all.x = TRUE)
      lookup$center <- lookup$center * max(lookup[, x[i]])
      lookup <- sqldf(paste0("select min(`",x[i],"`) as min, max(`",x[i],"`) as max, center from lookup group by center"))
      
      lookup[2:nrow(lookup), 'min'] <- lookup[1:nrow(lookup)-1, 'max']
      lookup[1, 'min'] <- ifelse(lookup[1, 'min'] == 0, lookup[1, 'min'] - 100, lookup[1, 'min'] * -100)
      lookup[nrow(lookup), 'max'] <- ifelse(lookup[nrow(lookup), 'max'] == 0, lookup[nrow(lookup), 'max'] + 100, lookup[nrow(lookup), 'max'] * 100)
      mappings[[i]] <- lookup
    }
    
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }

  names(mappings) <- x
  mappings <- mappings[lapply(mappings,length)>0]
  if(progress == TRUE){
    cat(" \n")
  }
  return(mappings)
}
