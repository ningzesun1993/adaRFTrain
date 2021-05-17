#' @title grid_search_rf
#' @description This is the function to search the hyper parameter of the random forest
#' @description which returns a total hyper parameter for random forest
#' @param df: total dataframe
#' @param params_range: a list of parameters to tune
#' @param target: the target of the model
#' @param n_fold: number of folds to use to split the train data, default: 3
#' @param scoring: scoring metric used to evaluate the best model, default: c("accuracy", "auc")
#' @param params_: the parameters of random forest, have to be a list with all parameters:
#' @param  ntree: Number of trees to grow, default: 500
#' @param  sampsize: Size(s) of sample to draw, default: total row of train dataset
#' @param  nodesize: Minimum size of terminal nodes, default: NULL
#' @param  maxnodes: Maximum number of terminal nodes trees in the forest can have, default: NULL
#' @param  replace: Should sampling of cases be done with or without replacement?, default: TRUE
#' @param  mtry: Number of variables randomly sampled as candidates at each split,default: floor of square of features
#' @param  strata: A (factor) variable that is used for stratified sampling, default: target
#' @keywords grid search for random forest

#' @examples
#' library(superml)
#' data(iris)
#' df = iris
#' df$Species = as.factor(LabelEncoder$new()$fit_transform(df$Species))
#' target = "Species"
#' params_range = list(n_estimators = seq(50, 200, 10), max_depth = seq(2, 10, 2))
#' params = grid_search_rf(df, params_range, target = target)

params = list(ntree = 500, sampsize = NULL, nodesize = NULL, maxnodes = NULL,
              replace = TRUE, mtry = NULL, Stratify = TRUE)

grid_search_rf <- function(df, params_range, target, n_fold = 3, scoring = c("accuracy", "auc"),
                           params_ = list(ntree = 500, sampsize = NULL, nodesize = NULL,
                                          maxnodes = NULL, replace = TRUE, mtry = NULL,
                                          strata = NULL)) {
  if (!is.null(target)){df[[target]] = as.factor(df[[target]])}
  if (is.null(target)){target = names(df)[[length(names(df))]]}
  if (is.null(params_[['sampsize']])){params_[['sampsize']] = nrow(df)}
  if (is.null(params_[['mtry']])){params_[['mtry']] = floor(sqrt(ncol(df)))}
  if (is.null(params_[['strata']])){params_[['strata']] = target}
  rf = superml::RFTrainer$new()
  rf_grid = superml::GridSearchCV$new(trainer = rf, parameters = params_range,
                         n_folds = n_fold, scoring = scoring)
  rf_grid$fit(df, target)
  result = rf_grid$best_iteration(metric = NULL)
  p_params = list(n_estimaters = "ntree", max_features = "mtry", max_samples = "sampsize",
                  min_samples_leaf = "nodesize", max_leaf_nodes = "maxnodes")
  for (i in names(params_range)){
    if (i %in% names(p_params)){
      params_[[p_params[[i]]]] = result[[i]]
      print(result[[i]])
    }
  }
  params_[['sampsize']] = NULL
  params_[['mtry']] = NULL
  params_[['strata']] = NULL
  return(params_)
}


#' @export
