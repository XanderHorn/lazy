#' Creates engineered features from text features
#' 
#' Simple features are created from text features such as punctuation count, character count, number count etc.
#'
#' @param data [required | data.frame] Dataset containing categorical features
#' @param x [required | character] A vector of text feature names present in the dataset
#' @param progress [optional | logical | default=TRUE] Display a progress bar 
#' @return Data frame with engineered text features
#' @export
#' @examples
#' tmp <- iris
#' tmp$Species <- as.character(tmp$Species)
#' tf <- text.features(data = tmp, x = "Species")
text.features <- function(data, x, progress = TRUE){

  library(stringr)
  library(tm)
  
  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }
  
  if(missing(x)){
    stop("No text features specified in arg 'x'")
  }
  
  temp <- as.data.frame(data[, x], stringsAsFactors = F)
  if(length(x) == 1){
    names(temp) <- x
  }
  
  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }
  
  for(i in 1:length(x)){
    temp[, paste0("lazy.text.length.", x[i])] <- nchar(temp[, x[i]])
    temp[, paste0("lazy.space.count.", x[i])] <- str_count(string = temp[, x[i]], pattern = " ")
    temp[, paste0("lazy.capital.count.", x[i])] <- str_count(string = temp[, x[i]], pattern = "[A-Z]")
    temp[, paste0("lazy.number.count.", x[i])] <- str_count(string = temp[, x[i]], pattern = "[0-9]")
    temp[, paste0("lazy.punct.count.", x[i])] <- temp[,paste0("lazy.text.length.",x[i])] - nchar(removePunctuation(temp[, x[i]]))
    temp[, paste0("lazy.excl.count.", x[i])] <- str_count(string = temp[, x[i]], pattern = "!")
    temp[, paste0("lazy.comma.count.", x[i])] <- str_count(string = temp[, x[i]], pattern = ",")
    
    chars <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
    nums <- c("1","2","3","4","5","6","7","8","9","0")
    
    for(j in 1:length(nums)){
      temp[, paste0("lazy.",nums[j],".count.", x[i])] <- str_count(temp[, x[i]], pattern = nums[j])
    }
    
    for(k in 1:length(chars)){
      temp[, paste0("lazy.",chars[k],".count.", x[i])] <- str_count(tolower(temp[, x[i]]), pattern = chars[k])
    }
    
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  
  temp <- temp[, setdiff(names(temp), x)]
  cat(" \n")
  return(temp)
}  
