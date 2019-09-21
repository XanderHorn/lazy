#' Preprocess and predict wrapper
#' 
#' Pre-processes data and predicts. Function to be used in scoring environment. For classification problems the result will contain both the predicted class and probabilities.
#'
#' @param data [required | data.frame] New data to produce predictions for.
#' @param model.object [required | h2o] An h2o model object.
#' @param pipeline [required | list | default=NULL] List object returned either by automl or preprocess, if left as NULL, it is assumed that a prediction is required on an already pre-processed dataset.
#' @param mapping.list [required | list | default=NULL] List object returned either by automl or preprocess, if left as NULL, it is assumed that a prediction is required on an already pre-processed dataset..
#' @return A data.frame object with predictions.
#' @export
#' @examples
#' ## NOT RUN
#' res <- automl(train=iris, valid=iris, y="Species")
#' preds <- lazy.predict(data=iris, model.object=res$models[1], pipeline=res$pipeline, mapping.list=res$mapping.list)
#' @author 
#' Xander Horn
lazy.predict <- function(data, model.object, pipeline = NULL, mapping.list = NULL){

  if(missing(data)){
    stop("No data set to predict on provided to function")
  }
  
  if(missing(model.object)){
    stop("No h2o model object provided to function")
  }

  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  quiet(h2o.init())

  data <- quick.format(as.data.frame(data))
  
  if(is.null(pipeline) == FALSE & is.null(mapping.list) == FALSE){
    data <- quiet(as.h2o(pre.process(data = data, pipeline = pipeline, mapping.list = mapping.list, verbose = F)))
  } else {
    data <- quiet(as.h2o(data))
  }
  
  preds <- as.data.frame(h2o.predict(model.object, newdata = data))
  return(preds)
}
