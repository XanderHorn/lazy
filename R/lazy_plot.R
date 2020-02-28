#' Lazy ggplot plotting
#'
#' Quickly produces specifc plots using the ggplot library. This function is exported but its main purpose is to be used in the eda function.
#'
#' @param data [required | data.frame] Dataset containing predictor and / or target features.
#' @param x [optional | character | default=NULL] A vector of feature names present in the dataset used to predict the target feature. If NULL then all columns in the dataset is used.
#' @param y [required | character | default=NULL] The name of the target feature contained in the dataset.
#' @param type [optional | character | default="histogram"] The type of plot to be produced. For numeric feature types histogram, density, boxplot and violin are available. For categorical bar and stackedbar are available.
#' @param transparency [optional | numeric | default = 1] Transparency applied to plots.
#' @param theme [optional | numeric | default=1] Color theme applied to plot, options range from 1 to 4.
#' @return Plot of ggplot2 type
#' @export
#' @examples
#' lazy.plot(iris, x = "Sepal.Length", y = "Species", type = "density")
#' lazy.plot(iris, x = "Sepal.Length", y = "Species", type = "violin")
#' @author 
#' Xander Horn
lazy.plot <- function(data, x = NULL, y = NULL, type = "histogram", transparency = 1, theme = 1){

  library(ggplot2)
  library(RColorBrewer)
  
  if(missing(data)){
    stop("Provide data to function")
  }
  
  if(is.null(x) == TRUE){
    x <- names(data)
  }
  
  if(is.null(y) == FALSE){
    x <- setdiff(x, y)
    data[,y] <- as.character(data[,y])
  }
  
  tol8qualitative <- c("#332288", "#88CCEE", "#44AA99", "#117733","#999933", "#DDCC77", "#CC6677", "#AA4499")
  set8equal <- c("#66C2A5", "#8DA0CB", "#A6D854", "#B3B3B3", "#E5C494", "#E78AC3", "#FC8D62", "#FFD92F")
  redmono = c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2")
  greenmono = c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#E5F5E0")
  bluemono = c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7")
  greymono = c("#000000", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9")
  
  if(theme == 1){
    theme <- c(brewer.pal(9, "Set1"), brewer.pal(12, "Paired"))
  } else if(theme == 2){
    theme <- c(brewer.pal(12, "Paired"), brewer.pal(8, "Accent"))
  } else if(theme == 3){
    theme <- c(brewer.pal(8, "Dark2"), set8equal, tol8qualitative)
  } else if(theme == 4){
    theme <- c("#4527A0", "#B39DDB", bluemono, redmono, greenmono, greymono)
  }
  
  p <- ggplot(data = data)
  
  if(type == "histogram"){
    if(is.null(y) == TRUE){
      p <- p + 
        aes(x = data[,x]) + 
        geom_histogram(alpha = transparency, fill = theme[2], color = "white", bins = 25) + 
        labs(x = x, y = "Frequency") + 
        theme_light()
    } else {
      p <- p + 
        aes(x = data[,x], fill = data[,y]) + 
        geom_histogram(alpha = transparency, color = "white", bins = 25) + 
        labs(x = x, y = "Frequency") + 
        guides(colour = FALSE, fill = guide_legend(title = y)) + 
        scale_fill_manual(values = theme) + 
        scale_color_manual(values = theme) + 
        theme_light()
    }
  }
  
  if(type == "density"){
   if(is.null(y) == TRUE){
     p <- p +
       aes(x = data[,x]) +
       geom_density(alpha = transparency, fill = theme[2], color = theme[2]) + 
       labs(x = x, y = "Density") + 
       theme_light()
   } else {
     p <- p +
       aes(x = data[,x], fill = data[,y], color = data[,y]) +
       geom_density(alpha = 0.5) + 
       labs(x = x, y = "Density") + 
       guides(colour = FALSE, fill = guide_legend(title = y)) + 
       scale_fill_manual(values = theme) + 
       scale_color_manual(values = theme) + 
       theme_light()
   }
  }
  
  if(type == "boxplot"){
    p <- p +
      aes(x = data[,y], y = data[,x], color = data[,y]) + 
      geom_boxplot(lwd = 0.8, outlier.colour = "black",outlier.shape = 16, outlier.size = 2, alpha = transparency) + 
      labs(x = y, y = x) +
      guides(fill = FALSE, colour = FALSE) +
      scale_color_manual(values = theme) +
      theme_light()
  }
  
  if(type == "violin"){
    p <- p +
      aes(x = data[,y], y = data[,x], fill = data[,y]) + 
      geom_violin(alpha = transparency, color = "white") + 
      labs(x = y, y = x) +
      guides(fill = FALSE, colour = FALSE) +
      scale_fill_manual(values = theme) + 
      theme_light()
  }
  
  if(type == "bar"){
    props <- as.data.frame(prop.table(table(data[,x])))
    props <- props[order(props$Freq), ]
    
    if(is.null(y) == TRUE){
      p <- p +
        aes(x = factor(data[,x], levels = props$Var1)) +
        geom_bar(aes(y = (..count..)/sum(..count..)), fill = theme[2], alpha = transparency) + 
        scale_y_continuous(labels = scales::percent) + 
        labs(x = x, y = "Percentage") + 
        coord_flip() + 
        theme_light()
    } else {
      p <- p + 
        aes(x = factor(data[,x], levels = props$Var1), fill = data[,y]) +
        geom_bar(aes(y = (..count..)/sum(..count..)), alpha = transparency) + 
        scale_y_continuous(labels = scales::percent) + 
        labs(x = x, y = "Percentage") + 
        guides(fill = guide_legend(title = y)) + 
        scale_fill_manual(values = theme) + 
        coord_flip() + 
        theme_light()
    }
  }
  
  if(type == "stackedbar"){
    p <- p + 
      aes(x = data[,y], fill = data[,x]) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill", alpha = transparency) + 
      scale_y_continuous(labels = scales::percent) + 
      labs(x = y, y = "Percentage") + 
      guides(fill = guide_legend(title = x)) + 
      scale_fill_manual(values = theme) + 
      theme_light()
  }

  return(p)
}
