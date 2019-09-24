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

  return(pipeline)
}
