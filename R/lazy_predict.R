#' Preprocess and predict wrapper
#' 
#' Pre-processes data and predicts. Function to be used in scoring environment. For classification problems the result will contain both the predicted class and probabilities.
#'
#' @param data 
#' @param model.object 
#' @param pipeline 
#' @param mapping.list 
#' @return A data.frame object with predictions.
#' @export
#' @examples
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
