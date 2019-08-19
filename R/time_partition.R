#' Create time sensitive partitions
#'
#' Partitions data according to a time based feature for time sensitive datasets
#'
#' @param data [required | data.frame] Dataset to partition
#' @param time.feature [required | character] Time feature present in dataset
#' @param valid.sample.size [required | numeric | default=0.3]
#' @return List with train and validation row indices
#' @export
#' @examples
#' new.iris <- iris
#' new.iris$date <- seq(ISOdate(2019,1,1), by = "day", length.out = nrow(new.iris))
#' splits <- time.partition(new.iris, "date")
#' @author
#' Xander Horn
time.partition <- function(data, time.feature, valid.sample.size = 0.3){
  data <- data[order(data[,time.feature]), ]
  start <- nrow(data) - round(0.3*nrow(data))
  valid.id <- start:nrow(data)
  train.id <- 1:(start-1)

  split <- list()
  split$train <- train.id
  split$valid <- valid.id
  return(split)
}
