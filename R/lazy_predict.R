#' Preprocess and predict wrapper
#' 
#' Pre-processes data and predicts. Function to be used in scoring environment. For classification problems the result will contain both the predicted class and probabilities.
#'
#' @param data [required | data.frame] New data to produce predictions for.
#' @param model.object [required | h2o] The h2o model object.
#' @param pipeline [required | list] List object returned either by automl or preprocess.
#' @param mapping.list [required | list] List object returned either by automl or preprocess.
#' @return A data.frame object with predictions.
#' @export
#' @examples
#' ## NOT RUN
#' res <- automl(train=iris, valid=iris, y="Species")
#' preds <- lazy.predict(data=iris, model.object=res$models[1], pipeline=res$pipeline, mapping.list=res$mapping.list)
#' @author 
#' Xander Horn
lazy.predict <- function(data, model.object, pipeline, mapping.list){
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  data <- quick.format(as.data.frame(data))
  
  data <- as.h2o(pre.process(data = data, pipeline = pipeline, mapping.list = mapping.list, verbose = F))
  
  quiet(h2o.init())
  preds <- as.data.frame(h2o.predict(model.object, newdata = data))
  return(preds)
}
