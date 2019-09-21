#' Create pipeline grid for optimization
#' 
#' Creates a data.frame of available methods applied in a pipeline to be used in optimizing data towards the best performing pipeline.
#'
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
#' @return All combinations given input parameters in a data.frame object.
#' @export
#' @examples
#' res <- make.pipeline.grid()
#' @author 
#' Xander Horn
make.pipeline.grid <- function(text.features = c(FALSE,TRUE),
                               text.threshold = 100,
                               date.features = c(FALSE,TRUE),
                               impute.missing = c(FALSE,TRUE),
                               impute.mode = c("auto","median.mode","encode"),
                               impute.tracking = c(FALSE,TRUE),
                               impute.threshold = 0.1,
                               categorical.encoding = c(FALSE,TRUE),
                               categorical.mode = c("auto","onehot.prop","target","proportional","ordinal"),
                               categorical.tracking = c(FALSE,TRUE),
                               categorical.max.levels = 10,
                               categorical.min.percent = 0.025,
                               categorical.interactions = c(FALSE,TRUE),
                               categorical.interactions.levels = 2,
                               categorical.interaction.feats = 10,
                               outlier.clipping = c(FALSE,TRUE),
                               outlier.mode = c("tukey","percentile"),
                               outlier.tracking = c(FALSE,TRUE),
                               outlier.lower.percentile = 0.01,
                               outlier.upper.percentile = 0.99,
                               max.scaling = c(FALSE,TRUE),
                               kmeans.features = c(FALSE,TRUE),
                               numeric.transform = c(FALSE,TRUE),
                               transform.mode = c("log","sqrt"),
                               transform.cutoff = 7,
                               numeric.interactions = c(FALSE,TRUE),
                               numeric.interaction.feats = 10,
                               freq.encode = c(FALSE,TRUE)){
  
  
  g <- expand.grid(text.features = text.features,
                   text.threshold = text.threshold,
                   date.features = date.features,
                   impute.missing = impute.missing,
                   impute.mode = impute.mode,
                   impute.tracking = impute.tracking,
                   impute.threshold = impute.threshold,
                   categorical.encoding = categorical.encoding,
                   categorical.mode = categorical.mode,
                   categorical.tracking = categorical.tracking,
                   categorical.max.levels = categorical.max.levels,
                   categorical.min.percent = categorical.min.percent,
                   categorical.interactions = categorical.interactions,
                   categorical.interactions.levels = categorical.interactions.levels,
                   categorical.interaction.feats = categorical.interaction.feats,
                   outlier.clipping = outlier.clipping,
                   outlier.mode = outlier.mode,
                   outlier.tracking = outlier.tracking,
                   outlier.lower.percentile = outlier.lower.percentile,
                   outlier.upper.percentile = outlier.upper.percentile,
                   max.scaling = max.scaling,
                   kmeans.features = kmeans.features,
                   numeric.transform = numeric.transform,
                   transform.mode = transform.mode,
                   transform.cutoff = transform.cutoff,
                   numeric.interactions = numeric.interactions,
                   numeric.interaction.feats = numeric.interaction.feats,
                   freq.encode = freq.encode,
                   stringsAsFactors = FALSE)
  
  return(g)
}