#' Dynamic documentation for pipeline settings
#'
#' Creates dynamic documentation for pipelines.
#'
#' @param pipeline [requied | list] A pipeline list object either returned from automl or design.pipeline.
#' @return List containing documentation
#' @export
#' @examples
#' p <- design.pipeline()
#' doc <- document(p)
#' @author
#' Xander Horn
document <- function(pipeline){

  doc <- list()

  doc$general.information <- paste0("

  Project author: lazy library
  Project created date: ",pipeline$information$created.date,"

  The provided dataset was analysed before any complex modifications and model training commenced. This ensures that models receives a clean and structured dataset before training.

  If any duplicate observations (rows) were found in the training set based on the provided ID feature(s), these duplicated observations were removed before pre-processing and training took place.

  Data formatting was done ensuring that only the following formats are allowed in the dataset:

  1. Numeric
  2. Integer
  3. Date (various types)
  4. Character

  Problematic features were identified and removed if they existed in the dataset such as constant, all missing and duplicate features.

  The random number seed was set to a value of ",pipeline$settings$seed," for reproducibility.
  ")

  if(pipeline$settings$text.features == TRUE){
    if(length(pipeline$features$text.features) > 0){
      doc$text.features <- paste0("
  Text features were identified when a character feature had more than ", pipeline$settings$text.threshold, " characters present.
  After having identified text features, simplistic feature engineering was performed.
  Engineering techniques such as punctuation count, character count, number count, string length etc. were used to derive new features.
  ")
    } else {
      doc$text.features <- "
      No text features were found in the dataset.
      "
    }
  } else {
    doc$text.features <- "
      No text features were engineered in the dataset.
      "
  }

  if(pipeline$settings$date.features == TRUE){
    if(length(pipeline$features$date.features) > 0){
      doc$date.features <- "
    All features formatted as Date, POSIXct or POSIXt were used to apply date feature engineering techniques to.
    New features such as year, month, day of week, hour etc. were derived from the base date features.
    "
    } else {
      doc$date.features <- "
      No date features were found in the dataset.
      "
    }
  } else {
    doc$date.features <- "
      No date features were engineered in the dataset.
      "
  }

  if(pipeline$settings$impute.missing == TRUE){

    if(pipeline$settings$impute.mode == "auto"){
      doc$imputation <- paste0("
      Imputation was done on the feature set provided. All features containing NA (missing) values were imputed using a combination of encoding and the median, mode approach.
      Features containing more than ",pipeline$settings$impute.threshold * 100,"% missing values were imputed using encoding. All other features were imputed using the median value for numerical features and the mode for categorical features. Simply put, encoding involves assigning a new category called “lazy.unknown” for categorical features and using the minimum value for numerical features and subtracting 10 from the minimum value. When using the median and mode approach, for each feature the median or mode is calculated and applied where missing values are encountered.
      ")
    }

    if(pipeline$settings$impute.mode == "encode"){
      doc$imputation <- "
    Imputation was done on the feature set provided. All features containing NA (missing) values were imputed by using an encoding technique for numerical and categorical features.
    Simply put, encoding involves assigning a new category called “lazy.unknown” for categorical features and using the minimum value for numerical features and subtracting 10 from the minimum value.
    "
    }

    if(pipeline$settings$impute.mode == "median.mode"){
      doc$imputation <-"
       Imputation was done on the feature set provided. All features containing NA (missing) values were imputed by using the median value for numerical features and using the mode for categorical features.
       Thus for each feature the median or mode is calculated and applied where missing values are encountered.
      "
    }

    if(pipeline$settings$impute.tracking == TRUE){
      doc$impute.tracking <- "
      For every feature that contained NA (missing) values, a tracking feature was created, where it was recorded which observation had a NA value before the observation was imputed.
      Tracking features typically boost performance in tree based algorithms.
      "
    } else {
      doc$impute.tracking <- "
      Tracking features were not created when imputing features.
      "
    }

  } else {
    doc$imputation <- "
    No imputation was done on the feature set provided.
    "
  }

  if(pipeline$settings$outlier.clipping == TRUE){

    if(pipeline$settings$outlier.mode == "tukey"){
      doc$outlier.clipping <- "
      Outliers were detected in numeric features using the Tukey outlier detection method.
      Outliers were then clipped (winsorized) by replacing the outlying values with the value of the statistic.
      Simply put any value higher than the outlier threshold will be replaced by the outlier threshold.
      "
    }

    if(pipeline$settings$outlier.mode == "percentile"){
      doc$outlier.clipping <- paste0("
      Outliers were detected in numeric features using a percentile based approach.
      Outliers were detected if a feature had a value higher or lower than the ",pipeline$settings$outlier.upper.percentile * 100,"% or ",pipeline$settings$outlier.lower.percentile * 100,"% percentile value. Outliers were then clipped (winsorized) by replacing the outlying values with the value of the statistic.
      Simply put, a value greater than the 99th percentile value will be replaced by the 99th percentile value.
      ")
    }

    if(pipeline$settings$outlier.tracking == TRUE){
      doc.outlier.tracking <- "
      For every feature that contained outlying values, a tracking feature was created, where it was recorded which observation had an outlying value before the observation was clipped.
      Tracking features typically boost performance in tree based algorithms.
      "
    } else {
      doc.outlier.tracking <- "
      Tracking features were not created when detecting and clipping outliers in features.
      "
    }

  } else {
    doc$outlier.clipping <- "
    No outlier clipping was done on the feature set provided.
    "
  }

  if(pipeline$settings$categorical.interactions == TRUE){
    doc$categorical.interactions <- paste0("
    Categorical interaction features were created by combining the categories of ",pipeline$settings$categorical.interactions.levels," pair way interaction categorical features into a new feature.
    Only the ",pipeline$settings$categorical.interaction.feats," most important categorical features were selected to form part of the creation process for interaction features.
    ")
  } else {
    doc$categorical.interactions <- "
    No categorical interaction features were created.
    "
  }

  if(pipeline$settings$categorical.mode == "auto"){
    doc$categorical.encoding <- "
    Automated encoding is a combination between target encoding and one hot encoding. For categorical features that exhibit more than [parameter value] unique categories, target encoding was applied, else one hot encoding was applied.
    Target encoding simply implies that for each unique category in a categorical feature, the average value of the target is used to encode the category.
    To avoid overfitting, the average target value is weighted using a logistic function that will simulate the average of the sample if the frequency for a category is low and random noise is additionally added to further avoid overfitting.
    One hot encoding simply applies a method where each unique category is assigned a value of 1 if a row/observation has that category and 0 otherwise.
    For each unique category in the categorical feature a new feature is created that will have only values of 1s and 0s. If a feature has 10 unique categories, 10 new features will be created.
    "
  }

  if(pipeline$settings$categorical.mode == "target"){
    doc$categorical.encoding <- "
    Categorical features were encoded using a target encoding approach.
    Target encoding simply implies that for each unique category in a categorical feature, the average value of the target is used to encode the category.
    To avoid overfitting, the average target value is weighted using a logistic function that will simulate the average of the sample if the frequency for a category is low and random noise is additionally added to further avoid overfitting.
    "
  }

  if(pipeline$settings$categorical.mode == "onehot.prop"){
    doc$categorical.encoding <- paste0("
    Categorical features were encoded using proportional one hot encoding.
    One hot proportional encoding is a technique that applies one hot encoding after grouping low proportional categories in a categorical feature.
    A good example of this is a categorical feature containing city names for a dataset spanning across regions but with a core operating area in a specific region.
    Most of the city names will have a low proportion relative to the data size and thus they will be grouped into a new category and then only one hot encoding is applied.
    Low proportional categories, are categories where the relative frequency of a category is less than ",pipeline$settings$categorical.min.percent * 100,"%.
    ")
  }

  if(pipeline$settings$categorical.mode == "onehot"){
    doc$categorical.encoding <- "
    Categorical features were encoded using one hot encoding.
    One hot encoding simply applies a method where each unique category is assigned a value of 1 if a row/observation has that category and 0 otherwise.
    For each unique category in the categorical feature a new feature is created that will have only values of 1s and 0s. If a feature has 10 unique categories, 10 new features will be created.
    "
  }

  if(pipeline$settings$categorical.mode == "proportional"){
    doc$categorical.encoding <- "
    Categorical features were encoded using a proportional encoding approach.
    All categories present in a categorical feature is remapped/ encoded/ engineered by calculating the frequency of each category relative to the number of observations/ rows present in the dataset.
    This technique is known as proportional encoding and is a percentage representation of frequency encoding.
    "
  }

  if(pipeline$settings$categorical.mode == "ordinal"){
    doc$categorical.encoding <- "
    Categorical features were encoded using a ordinal encoding approach
    Ordinal encoding is a technique where a categorical feature is encoded by calculating the frequency of each category relative to the number of observations/ rows present in the dataset.
    After calculating each category’s proportion, the proportions are then ordered and the following transformation is applied: cumsum(proportions) - 0.5 * proportion_i / sum(proportions).
    By applying the transformation, a rough probability is derived at the chance that a category might occur.
    "
  }

  if(pipeline$settings$categorical.mode == "report"){
    doc$categorical.encoding <- paste0("
    Categorical features containing many categories were encoded to group all low proportional categories into a group called “ALL_OTHER”.
    This was done to ensure that when visualizing the data, plots can be properly constructed. Low proportional categories, are categories where the relative frequency of a category is less than ",pipeline$settings$categorical.min.percent * 100,"%.
    ")
  }

  if(pipeline$settings$categorical.tracking == TRUE){
    doc$categorical.tracking <- paste0("
    Tracking features were created when low proportional categories were identified.
    Low proportional categories, are categories where the relative frequency of a category is less than ",pipeline$settings$categorical.min.percent * 100,"%.
    For every low proportional category a new feature is created that will contain only 1s and 0s.
    ")
  } else {
    doc$categorical.tracking <- paste0("
    Tracking features were not created when detecting low proportional categories in categorical features.

    ")
  }

  if(pipeline$settings$max.scaling == TRUE){
    doc$max.scaling <- "
    Numerical features were scaled using a max scaling approach.
    This approach results in features having values between 0 and 1.
    Scaling is used to address outlying values and to ensure that time specific features scale over the lifetime of a model.
    "
  } else {
    doc$max.scaling <- "
    No feature scaling was done.
    "
  }

  if(pipeline$settings$numeric.transform == TRUE){
    if(pipeline$settings$transform.mode == "log"){
      doc$numeric.transformations <- paste0("
      Skewed numerical features were transformed to rectify a skewed distribution.
      A log transformation was applied to skewed features.
      Skewed features were identified if they had a skewness value of +/- ",pipeline$settings$transform.cutoff,"
      ")
    }

    if(pipeline$settings$transform.mode == "sqrt"){
      doc$numeric.transformations <- paste0("
      Skewed numerical features were transformed to rectify a skewed distribution.
      A square root transformation was applied to skewed features.
      Skewed features were identified if they had a skewness value of +/- ",pipeline$settings$transform.cutoff,"
      ")
    }
  } else {
    doc$numeric.transformations <- "
    Skewed numerical features were not transformed.
    "
  }

  if(pipeline$settings$kmeans.features == TRUE){
    doc$kmeans.features <- "
    Unsupervised k-means features were derived from base numerical features by clustering per feature, calculating the center of each cluster and then calculating the distance of each value in the numerical feature to the cluster center.
    These features provide some insight as to how closely related observations are in a feature according to some grouping.
    "
  } else {
    doc$kmeans.features <- "
    No unsupervised k-means features were derived.
    "
  }

  if(pipeline$settings$numeric.interactions == TRUE){
    doc$numeric.interactions <- paste0("
    Numerical interaction features were derived by summing, deducting, multiplying and dividing combinations of two features.
    The top ",pipeline$settings$numeric.interaction.feats," most important numeric features were used to create all possible combinations to interact.
    Simply put, if a total of two features were to be interacted, an additional eight features will be derived.
    ")
  } else {
    doc$numeric.interactions <- "
    No numerical interaction features were derived.
    "
  }

  if(pipeline$settings$freq.encode == TRUE){
    doc$frequency.features <- "
    Categorical frequency features were derived by calculating the frequency of a category appearing in a feature.
    These features provide additional information on categorical features after they have been encoded and the information is lost.
    "
  } else {
    doc$frequency.features <- "
    No categorical frequency based features were derived.
    "
  }

  if("ml.time.partitioning" %in% names(pipeline$settings)){
    if(pipeline$settings$ml.time.partitioning == TRUE){
      doc$data.partioning <- "
    Data was partioned in order to create training, validation and testing sets according to a time dependent feature.
    Partioning according to time ensures that when modeling data the model predicts on future data and not random data.
    "
    } else {
      doc$data.partioning <- "
    Simple stratified random sampling was used to create training, validation and testing sets for model validation.
    "
    }
  }

  if("ml.data.leakage" %in% names(pipeline$settings)){
    if(pipeline$settings$ml.data.leakage == TRUE){
      doc$data.leakage <- paste0("
      Data leakage checks were performed by checking on a per feature basis if a feature was able to classify between the training and testing set.
      Pruned decision tree models were used to fit and predict on the training and testing sets.
      Any feature that had an AUC value greater than ",pipeline$settings$ml.data.leakage.cutoff," were removed for data leakage.
      ")
    } else {
      doc$data.leakage <- "
      No data leakage checks were done before model fitting.
      "
    }
  }

  if("ml.feature.importance" %in% names(pipeline$settings)){
    if(pipeline$settings$ml.feature.importance == TRUE){
      doc$feature.importance <- paste0("
      Before fitting models to the pre-processed dataset, any feature that had a relative importance value less than ",pipeline$settings$ml.min.feature.importance * 100,"% were removed before modeling.
      Importance were calculated by fitting a random forest model and lasso glm model to the dataset and computing the permutation feature importance on the validation set, the average of the importance values were then used.
      ")
    } else {
      doc$feature.importance <- "
      No low importance features were removed before model fitting.
      "
    }
  }

  if("ml.balance.classes" %in% names(pipeline$settings)){
    if(pipeline$settings$ml.balance.classes == TRUE){
      doc$balance.classes <- "
      Due to class imbalance, classes were balanced to arrive at a 50/50 distribution.
      Class balancing was done using the automl framework of h2o.
      For more information on this visit their documention page.
      "
    } else {
      doc$balance.classes <- "
      No class balancing was done before fitting models.
      "
    }
  }

  if("ml.cv.folds" %in% names(pipeline$settings)){
    if(pipeline$settings$ml.cv.folds >= 3){
      doc$cross.validation <- paste0("
      Cross validation was done to perform model tuning, validation and to train stacked ensemble models. The number of folds was set to ",pipeline$settings$ml.cv.folds,".
      ")
    } else {
      doc$cross.validation <- "
      No cross validation was done when validating and tuning models.
      "
    }
  }

  if("ml.pipeline.search" %in% names(pipeline$settings)){
    if(pipeline$settings$ml.pipeline.search >= 1){
      doc$pipelines.explored <- paste0("
      Before executing h2o's automl functionality on the dataset, pipelines were explored to optimize the best possible pipeline for the provided dataset.
      A total of ",pipeline$settings$ml.pipeline.search," pipeline(s) were explored for a duration constraint of ",pipeline$settings$ml.pipeline.search.time, " minute(s).
      Random tuning was done to select and optimize the best performing pipeline according to a combined score based on a random forest model and a lasso regression model with performance metrics calculated on the validation set.
      ")
    } else {
      doc$pipelines.explored <- "
      A pre-defined pipeline was used to pre-process the data for model training. No pipelines were explored before model training began.
      "
    }
  }

  if("ml.models.explored" %in% names(pipeline$settings)){
    doc$models.explored <- paste0("
    The top ",pipeline$settings$ml.models.explored," models were selected and kept after validating them.
    A time constraint of ",pipeline$settings$ml.model.search.time," minute(s) was set for model training.
    ")
  }
return(doc)
}
