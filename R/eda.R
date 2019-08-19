#' Automated exploratory data analysis
#' 
#' Explores a provided data set and returns a list of plots per feature as well as a summary table.
#'
#' @param data [required | data.frame ] Dataset to visualize.
#' @param x [optional | character | default=NULL] Features to visualize specified as a character vector. If NULL then all features in the dataset will be used except for the target feature.
#' @param y [optional | character | default=NULL] Target feature to visualize. If NULL a univariate visialization will take place.
#' @param sample.size [optional | numeric | default=0.5] Sample size to down sample the data for faster exploration.
#' @param theme [optional | numeric | default=1] Color theme applied to plot, options range from 1 to 4.
#' @param numeric.plot [optional | character | default="histogram"] The type of plot to be produced. For numeric feature types histogram, density, boxplot and violin are available.
#' @param categorical.plot [optional | character | default="bar"] The type of plot to be produced. For categorical bar and stackedbar are available.
#' @param pipeline [optional | list | default=NULL] Pipeline used to pre-process the data for visualization. If NULL then a exploratory pipeline will be produced to pre-process the data.
#' @return List containing plots, tabular summary exploration and the pipeline used to data pre-processing.
#' @export
#' @examples
#' res <- eda(data = iris)
#' res <- eda(data = iris, y = "Species")
#' @author 
#' Xander Horn
eda <- function(data, x = NULL, y = NULL, sample.size = 0.5, theme = 1, numeric.plot = "histogram", 
                categorical.plot = "bar", pipeline = NULL){
  
  if(missing(data)){
    stop("Provide data to function")
  }

  if(is.null(x) == TRUE){
    x <- names(data)
  }
  
  if(is.null(y) == FALSE){
    x <- setdiff(x, y)
  }
  
  if(numeric.plot %in% c("violin","boxplot") & is.null(y) == TRUE){
    warning("numeric.plot of violin/boxplot not applicable when y is null, defaulting to histogram plot type")
    numeric.plot <- "histogram"
  }
  
  if(categorical.plot %in% c("stackedbar") & is.null(y) == TRUE){
    warning("categorical.plot of stackedbar not applicable when y is null, defaulting to bar plot type")
    categorical.plot <- "bar"
  }
  
  if(is.null(pipeline) == TRUE){
    pl <- design.pipeline(pipeline.name = "Data Exploration",
                          remove.data.leakage.feats = FALSE,
                          text.features = FALSE,
                          date.features = FALSE,
                          impute.mode = "encode",
                          outlier.clipping = TRUE,
                          outlier.mode = "percentile",
                          categorical.mode = "report",
                          seed = 1)
  } else {
    pl <- pipeline
  }
  
  set.seed(pl$settings$seed)
  
  data <- quick.format(data)
  
  if(is.null(y) == FALSE){
    data <- data[caret::createDataPartition(y = data[,y], p = sample.size, list = FALSE),]
  } else {
    data <- data[sample(nrow(data), sample.size * nrow(data), replace = F),]
  }
  
  if(is.null(y) == FALSE & length(unique(data[,y])) <= 15){
    data[,y] <- as.character(data[,y])
  } else {
    data[,y] <- as.numeric(data[,y])
  }
  
  plots <- list()
  tbl <- describe(data = data, sample.size = 1, progress = F, seed = pl$settings$seed)
  tmp <- detect.feats(data = data, x = x, progress = FALSE)
  tbl <- merge(tbl, tmp, by.x = "feature", all.x = TRUE)
  tbl <- subset(tbl, is.na(tbl$category) == FALSE)
  
  res <- pre.process(data = data, x = x, y = y, pipeline = pl, verbose = FALSE)
  data <- res$data
  
  nums <- c(res$pipeline$features$numeric.features, res$pipeline$features$indicator.features)
  nums <- nums[which(nums %in% names(data))]
  
  if(length(nums) > 0){
    for(i in 1:length(nums)){
      plots[[i]] <- lazy.plot(data = data, x = nums[i], y = y,type = numeric.plot, theme = theme)
    }
  }
  
  
  cats <- res$pipeline$features$categorical.features
  cats <- cats[which(paste0("lazy.",cats,".report") %in% names(data))]
  
  if(length(cats) > 0){
    for(i in 1:length(cats)){
      plots[[length(plots) + 1]] <- lazy.plot(data = data, x = paste0("lazy.",cats[i],".report"), y = y,type = categorical.plot, theme = theme)
    }
  }
  
  names(plots) <- c(nums, cats)
  
  out <- list()
  out$eda <- tbl
  out$plots <- plots
  out$pipeline <- pl
  return(out)
}
