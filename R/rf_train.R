#' @title dt_train
#' @description This is the function to train and predict the random forest
#' @description which returns a dataframe with test data and result
#' @param df: total dataframe if df_test is not specified else train data frame
#' @param formula: the formula of the model, can be not speicified if target is specified
#' @param df_test: test data frame
#' @param p: the ratio of split, have to be specified if df_test is not, default: 0.8
#' @param params_: the parameters of random forest, have to be a list with all parameters if specified:
#' @param  ntree: Number of trees to grow, default: 500
#' @param  sampsize: Size(s) of sample to draw, default: total row of train dataset
#' @param  nodesize: Minimum size of terminal nodes, default: NULL
#' @param  maxnodes: Maximum number of terminal nodes trees in the forest can have, default: NULL
#' @param  replace: Should sampling of cases be done with or without replacement?, default: TRUE
#' @param  mtry: Number of variables randomly sampled as candidates at each split, default: floor of square of features
#' @param  strata: A (factor) variable that is used for stratified sampling, default: target
#' @param target: the target of the model, have to be specified if formula is not
#' @keywords random forest

#' @examples
#' library(superml)
#' data(iris)
#' df = iris
#' df$Species = as.factor(LabelEncoder$new()$fit_transform(df$Species))
#' formula = "Species ~ ."
#' r_1 = rf_train(df, formula)


rf_train <- function(df, formula = NULL, df_test = NULL, p = 0.8,
                     params_ = list(ntree = 500, sampsize = NULL, nodesize = NULL,
                                    maxnodes = NULL, replace = TRUE, mtry = NULL,
                                    strata = NULL), target = NULL) {
  if (!is.null(target)){df[[target]] = as.factor(df[[target]])}
  if (is.null(df_test)){
    if (is.null(target)){target = names(df)[[length(names(df))]]}
    index = caret::createDataPartition(y = df[[target]], p = p, list = FALSE)
    df_train = df[index,]
    df_test = df[-index,]
    df_t = df_test
  }else{
    df_train = df
  }
  df_t = df_test[,!(names(df_test) %in% c(target))]
  if (is.null(formula)){formula = paste0(target, " ~ .")}
  if (is.null(params_[['sampsize']])){params_[['sampsize']] = nrow(df_train)}
  if (is.null(params_[['mtry']])){params_[['mtry']] = floor(sqrt(ncol(df_train)))}
  if (is.null(params_[['strata']])){params_[['strata']] = target}
  rf = randomForest::randomForest(as.formula(formula), data = df_train, ntree = params_[['ntree']],
                                  sampsize = params_[['sampsize']], nodesize = params_[['nodesize']],
                                  maxnodes = params_[['maxnodes']], replace = params_[['replace']],
                                  mtry = params_[['mtry']], strata = params_[['strata']])
  rf_p = predict(rf, newdata = df_t, type = "class")
  df_test = data.frame(cbind(df_test, rf_p))
  return(df_test)
}


#' @export
