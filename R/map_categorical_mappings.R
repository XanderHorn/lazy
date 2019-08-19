#' Maps categorical interaction combination frames
#' 
#' Creates a list of data.frame for each level of interactions containing a combination of categorical features to interact.
#'
#' @param x [required | character] A vector of categorical feature names present in the dataset
#' @param n.interactions [optional | numeric | Default=2] Number of features to interact, needs to be less than or equal to the number of features provided in x
#' @return List of data.frames containing a combination of features to interact
#' @export
#' @examples
#' cim <- map.categorical.interactions(x = c("Class","Sex","Age"))
#' @author 
#' Xander Horn
map.categorical.interactions <- function(x, n.interactions = 2){

if(missing(x)){
  stop("No categorical features specified")
}

if(n.interactions < 2){
  stop("Interactions require at least 2 levels")
}

n <- seq(from = 2, to = n.interactions, by = 1)
comb <- list()
for(j in 1:length(n)){
  comb[[j]] <- as.data.frame(t(combn(x, n[j])),
                             stringsAsFactors = FALSE)
}

return(comb)
}
