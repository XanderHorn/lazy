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