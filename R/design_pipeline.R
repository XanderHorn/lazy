#' Design a pipeline for data pre-processing
#' 
#' Designs and outputs a pipeline stating which steps will be taken to pre-process data. The output of this function will return the settings specified when designing the pipeline as well as documentation on how the data will be treated based on the specified settings.
#'
#' @param pipeline.name [character | optional | default=NULL] Name of the pipeline, if NULL then a random name will be generated.
#' @param text.features [logical | optional | default=TRUE] Should text features be engineered and new features derived. Simple engineering is applied such as character and number count etc. 
#' @param text.threshold [numeric | optional | default=100] The maximum number of characters counted in a character feature before it is identified as a text feature and assigned to text feature engineering.
#' @param date.features [logical | optional | default=TRUE] Should date features be engineered and new features derived. Simple engineering such as year, month day etc.
#' @param impute.missing [logical | optional | default=TRUE] Should features be imputed.
#' @param impute.mode [character | optional | default="auto"] Imputation mode, options are auto, encode and median.mode. Auto applies a combination bewteen encoding and median.mode imputation based on the na.threshold parameter.
#' @param impute.tracking [logical | optional | default=FALSE] Should tracking features be created which are indicator features that sets a value of 1 to all observations where a missing value was found per feature.
#' @param impute.threshold [optional | numeric | default=0.1] Threshold for auto impute.mode to apply encoding or median.mode imputation. All features containing Na values above the specified percentage threshold will be imputed using encoding.
#' @param categorical.encoding [logical | optional | default=TRUE] Should categorical features be encoded and engineered.
#' @param categorical.mode [optional | character | default="onehot.prop"] Type of mappings to apply. Options are auto, target, proportional, ordinal, onehot, onehot.prop, report, where auto is a combination between onehot and target. Tracking features are created which flags if a feature has a low proportional category in it. Other types of feature engineering includes, weighted mean noise target encoding, proportional encoding, ordinal proportional encoding, one hot encoding and low proportional one hot encoding which flags all low proportional categories as "other". Report cleans up levels so that the data can be represented in reports and charts.
#' @param categorical.tracking [logical | optional | default=FALSE] Should tracking features be created which are indicator features that sets a value of 1 to all observations where a level in a categorical feature was sparse (low proportional).
#' @param categorical.max.levels [optional | integer | default=10] The maximum levels allowed for a categorical feature to create one hot encoded features.
#' @param categorical.min.percent [optional | numeric | default=0.025] The minimum proportion a categorical level is allowed to have before it is flagged as a low proportional level.
#' @param categorical.interactions [logical | optional | default=FALSE] Should interaction features be created for categorical features based on n-way combinations. Categories for different features are combined into a new feature.
#' @param categorical.interactions.levels [optional | numeric | Default=2] Number of features to interact, needs to be less than or equal to the number of features provided in x.
#' @param categorical.interaction.feats [optional | numeric | default=10] The number of top important categorical features to be used when creating categorical interaction features.
#' @param outlier.clipping [logical | optional | default=FALSE] Should outliers be clipped.
#' @param outlier.mode [optional | character | default="tukey"] Mode to identify outliers. Options are tukey or percentile to identify outliers.
#' @param outlier.tracking [optional | logical | default=FALSE] Creates tracking features that record which observations had outliers present.
#' @param outlier.lower.percentile [optional | numeric | default=0.01] The lower percentile value to be used when flagging values as outliers.
#' @param outlier.upper.percentile [optional | numeric | default=0.99] The upper percentile value to be used when flagging values as outliers.
#' @param max.scaling [optional | logical | default=FALSE] Should features be scaled to be between 0  and 1 by dividing by the maximum value.
#' @param kmeans.features [optional | logical | default=FALSE] Should k-means features be created by clustering each feature and calculating the distance to the cluster centre.
#' @param numeric.transform [optional | logical | default=FALSE] Should numerical features with skewed distributions be transformed.
#' @param transform.mode [optional | character | default="log"] Transform type, options are log or sqrt.
#' @param transform.cutoff [optional | numeric | default=7] The skewness statistic cutoff value for features to be transformed.
#' @param numeric.interactions [optional | logical | default=FALSE] Should numerical feature interactions be created by adding, subtracting, dividing and multiplying n-way feature combinations. Only the top n numerical features are used to create interactions features as identified by a random forest permuation based feature importance.
#' @param numeric.interaction.feats [optional | numeric | default=10] The number of top important numerical features to be used when creating numerical interaction features.
#' @param freq.encode [optional | logical | default=FALSE] Should frequency features be created which is simply a count of each unique value present per feature. These are created before any feature engineering is done.
#' @param seed [optional | numeric | default=1] Random seed for reproducable results.
#' @return List containing pipeline information including settings and data pre-processing documentation
#' @export
#' @examples
#' pl <- design.pipeline(kmeans.features = TRUE)
#' @author 
#' Xander Horn
design.pipeline <- function(pipeline.name = NULL,
                            text.features = TRUE,
                            text.threshold = 100,
                            date.features = TRUE,
                            impute.missing = TRUE,
                            impute.mode = "auto",
                            impute.tracking = FALSE,
                            impute.threshold = 0.1,
                            categorical.encoding = TRUE,
                            categorical.mode = "onehot.prop",
                            categorical.tracking = FALSE,
                            categorical.max.levels = 10,
                            categorical.min.percent = 0.025,
                            categorical.interactions = FALSE,
                            categorical.interactions.levels = 2,
                            categorical.interaction.feats = 10,
                            outlier.clipping = FALSE,
                            outlier.mode = "tukey",
                            outlier.tracking = FALSE,
                            outlier.lower.percentile = 0.01,
                            outlier.upper.percentile = 0.99,
                            max.scaling = FALSE,
                            kmeans.features = FALSE,
                            numeric.transform = FALSE,
                            transform.mode = "log",
                            transform.cutoff = 7,
                            numeric.interactions = FALSE,
                            numeric.interaction.feats = 10,
                            freq.encode = FALSE,
                            seed = 1){

  generate.name <- function(n = 1) {
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  }

  pipeline <- list()
  
  if(is.null(pipeline.name) == TRUE){
    pipeline.name <- generate.name(1)
  }

  pipeline$name <- pipeline.name
  pipeline$settings$text.features <- text.features
  pipeline$settings$text.threshold <- text.threshold
  pipeline$settings$date.features <- date.features
  pipeline$settings$impute.missing <- impute.missing
  pipeline$settings$impute.mode <- impute.mode
  pipeline$settings$impute.tracking <- impute.tracking
  pipeline$settings$impute.threshold <- impute.threshold
  pipeline$settings$categorical.encoding <- categorical.encoding
  pipeline$settings$categorical.mode <- categorical.mode
  pipeline$settings$categorical.tracking <- categorical.tracking
  pipeline$settings$categorical.max.levels <- categorical.max.levels
  pipeline$settings$categorical.min.percent <- categorical.min.percent
  pipeline$settings$categorical.interactions <- categorical.interactions
  pipeline$settings$categorical.interactions.levels <- categorical.interactions.levels
  pipeline$settings$categorical.interaction.feats <- categorical.interaction.feats
  pipeline$settings$outlier.clipping <- outlier.clipping
  pipeline$settings$outlier.mode <- outlier.mode
  pipeline$settings$outlier.tracking <- outlier.tracking
  pipeline$settings$outlier.lower.percentile <- outlier.lower.percentile
  pipeline$settings$outlier.upper.percentile  <- outlier.upper.percentile
  pipeline$settings$max.scaling <- max.scaling
  pipeline$settings$kmeans.features <- kmeans.features
  pipeline$settings$numeric.transform <- numeric.transform
  pipeline$settings$transform.mode <- transform.mode
  pipeline$settings$transform.cutoff <- transform.cutoff
  pipeline$settings$numeric.interactions <- numeric.interactions
  pipeline$settings$numeric.interaction.feats <- numeric.interaction.feats
  pipeline$settings$freq.encode <- freq.encode
  pipeline$settings$seed <- seed

  pipeline$description$general.processing <- "The provided dataset was de-duplicated if ID features were provided and detected in the data. This is a necessary step in preparing datasets for machine learning algorithms in order not to create noise for the model to learn from. Quick formatting of the data was applied that simply formats all factor features as character and all logical features as numeric. Next, duplicate features, constant features and all missing features were identified and removed from the dataset (if any were detected). All of the remaining features were then categorised into numeric, categorical, date, text and indicator features in order to apply the correct feature engineering techniques to the relevant and correctly categorised features. Fore more information on the exact mapping values used, see the mapping.list returned after providing the pipeline to the pre.process function."
  
  if(text.features == TRUE){
    pipeline$description$text.features <- paste0("Text features were identified when any observation had a maximum number of ", pipeline$settings$text.threshold ,"characters or more. Engineering such as character count, number count, string length, punctuation count etc. were performed on text features.")
  }
  
  if(date.features == TRUE){
    pipeline$description$date.features <- "Date features formatted as one of the following: Date, POSIXct or POSIXt were detected and engineered accordingly. Engineering such as year, month, day of the week, hour, etc. were performed on date features."
  }
  
  if(impute.missing == FALSE){
    pipeline$description$imputation <- "No imputation was performed on features."
  }
  
  if(impute.mode == "auto" & impute.missing == TRUE){
    pipeline$description$imputation <- paste0("Features containing missing values were imputed using a combination of methods. If a feature had more than ", pipeline$settings$impute.threshold*100,"% missing values present, then encoding was used to impute, which is simply the minimum of the feature -10 and lazy.unknown for categorical features. If a feature had less than 10% missing values present, then the median value was used for numerical features and the mode value was used for categorical features.")
  }
  
  if(impute.mode == "encode" & impute.missing == TRUE){
    pipeline$description$imputation <- "Features containing missing values were imputed using encoding, which is simply the minimum of the feature - 10 and lazy.unknown for categorical features."
  }
  
  if(impute.mode == "median.mode" & impute.missing == TRUE){
    pipeline$description$imputation <- "Features containing missing values were imputed using the median value for numerical features and the mode value for categorical features."
  }
  
  if(impute.tracking == TRUE & impute.missing == TRUE){
    pipeline$description$imputation.tracking <- "Tracking features were created that record which observations per feature that had a missing value with a 1 otherwise if no missing value is present a 0 is recorded."
  }
  
  if(categorical.encoding == TRUE & categorical.mode == "onehot.prop"){
    pipeline$description$categorical.features <- paste0("Categorical features were engineered by identifying per feature which levels were of low proportions i.e. sparse. Low proportional categories were identified if a level had less than or equal to ", pipeline$settings$categorical.min.percent * 100,"% of data represented in the relevant category. These sparse levels are then grouped into a new level / category where after one hot encoding is then applied.")
  }
  
  if(categorical.encoding == TRUE & categorical.mode == "onehot"){
    pipeline$description$categorical.features <- paste0("Categorical features were engineered using one hot encoding. If a categorical feature had more than ",  pipeline$settings$categorical.max.levels, " unique levels/categories, it was not engineered.")
  }
  
  if(categorical.encoding == TRUE & categorical.mode == "auto"){
    pipeline$description$categorical.features <- paste0("Categorical features were engineered using a combination of one hot encoding without correcting low proportional / sparse categories and weighted mean noise target encoding. All features with less than or equal to ",pipeline$settings$categorical.max.levels, " categories were one hot encoded. Mean noise target encoding is calculated by converting the target feature into a numeric feature, weighting the target per category by a logistic function. Another target per category was calculated with the inclusion of random noise. The average of these two values are then taken and forms the target encoding.")
  }
  
  if(categorical.encoding == TRUE & categorical.mode == "target"){
    pipeline$description$categorical.features <- "Categorical features were engineered using weighted mean noise target encoding. Mean noise target encoding is calculated by converting the target feature into a numeric feature, weighting the target per category by a logistic function. Another target per category was calculated with the inclusion of random noise. The average of these two values are then taken and forms the target encoding."
  }
  
  if(categorical.encoding == TRUE & categorical.mode == "proportional"){
    pipeline$description$categorical.features <- "Categorical features were engineered by calculating the proportion of each category. These proportions are then applied to each category to create the numerical representation of the categorical feature."
  }
  
  if(categorical.encoding == TRUE & categorical.mode == "ordinal"){
    pipeline$description$categorical.features <- "Categorical features were engineered using a proportional ordinal encoding approach. The proportions per category are calculated after which the categories are sorted by proportion size. Next a transformation is applied to the ordered proportional levels to simulate the probability of a category occurring. The transformation is the cumulative sum of the ordered proportions - 0.5 * the ordered proportion divided by the sum of all proportions."
  }
  
  if(categorical.encoding == TRUE & categorical.mode == "report"){
    pipeline$description$categorical.features <- paste0("Categorical features were altered for data exploration and visualization for reporting outcomes. Simply put, categorical features with low proportions / sparse categories were combined into a new level / category. Sparse categories were identified as having less than or equal to ",pipeline$settings$categorical.min.percent * 100,"% of data represented in the relevant category. All other categories / levels were kept the same.")
  }
  
  if(categorical.encoding == TRUE & categorical.tracking == TRUE){
    pipeline$description$categorical.tracking <- "Tracking features were created that record which observations had a level corresponding to a sparse / low proportional level with a 1 else a 0 is recorded."
  }
  
  if(categorical.interactions == TRUE){
    pipeline$description$categorical.interactions <- paste0("Categorical interaction features were created by n-way combinations. Simply put, of all the categorical features present in the dataset,", pipeline$settings$categorical.interactions.levels,"-way interactions are created by taking the ", pipeline$settings$categorical.interactions.levels,"-way combination of the top ",pipeline$settings$categorical.interaction.feats," most important categorical features and combining their levels into a new feature.")
  }
  
  if(outlier.clipping == TRUE & outlier.mode == "tukey"){
    pipeline$description$outliers <- "Outliers were clipped / corrected by calculating the tukey statistic per feature and labelling observations as outliers that lie above or below the statistic. These outlying values were then clipped by the using the statistic as the replacement value."
  }
  
  if(outlier.clipping == TRUE & outlier.mode == "percentile"){
    pipeline$description$outliers <- paste0("Outliers were clipped / corrected by calculating the upper ",pipeline$settings$outlier.upper.percentile * 100,"% and lower ",pipeline$settings$outlier.lower.percentile * 100,"% percentile values. Observations with values lying above or below these percentile values were labelled as outliers and were clipped by the percentile values.")
  }
  
  if(outlier.clipping == TRUE & outlier.tracking == TRUE){
    pipeline$description$outlier.tracking <- "Tracking features were created that record which observations had an  outlying value present, lower or upper and recorded it with a 1 else a 0."
  }
  
  if(max.scaling == TRUE){
    pipeline$description$feature.scaling <- "Numerical features were scaled to be between 0 and 1 by dividing all observations in a feature by the maximum value of the feature."
  }
  
  if(kmeans.features == TRUE){
    pipeline$description$kmeans.features <- "K-means unsupervised features were created by clustering each individual feature and calculating the distance to the cluster centre. The number of clusters were set to 3. Cases were there are less than 3 unique values present in a feature, the number of unique values are then used as the number of clusters."
  }
  
  if(numeric.transform == TRUE & transform.mode == "log"){
    pipeline$description$feature.transformations <- paste0("Numerical features detected as having a skewed distribution were transformed by taking the log of the distribution. Features were identified as being skewed if they had a skewness value of + or - ",pipeline$settings$transform.cutoff,".")
  }
  
  if(numeric.transform == TRUE & transform.mode == "sqrt"){
    pipeline$description$feature.transformations <- paste0("Numerical features detected as having a skewed distribution were transformed by taking the square root of the distribution. Features were identified as being skewed if they had a skewness value of + or - ",pipeline$settings$transform.cutoff,".")
  }
  
  if(numeric.interaction.feats == TRUE){
    pipeline$description$numerical.feature.interactions <- paste0("Numerical feature interactions were created by taking the combination of ", pipeline$settings$numeric.interaction.feats," interaction features and creating a +, -, / and * feature for each combination. Only the top ", pipeline$settings$numeric.interaction.feats," features were selected according to a random forest permutation feature importance metric.")
  }
  
  if(freq.encode == TRUE){
    pipeline$description$frequency.features <- "Frequency features were created by counting the number of occurrences for each unique value per feature. These features are created before any engineering is done on the data and were only applied to categorical features."
  }
  
  pipeline$description$random.seed <- paste0("The random seed was set at a value of ", pipeline$settings$seed)
  
  return(pipeline)
}
