#' @title dt_train
#' @description This is the function to train and perdict the decision tree
#' @description which returns a dataframe with test data and result
#' @param df: total dataframe if df_test is not specified else train data frame
#' @param formula: the formula of the model, can be not speicified if target is specified
#' @param df_test: test data frame
#' @param p: the ratio of split, have to be specified if df_test is not, default: 0.8
#' @param params_: the parameters of decision tree, have to be a list with method and split:
#' @param  method: character string giving the method to use. default: "recursive.partition"
#' @param  split: Splitting criterion to use. default: c("deviance", "gini")
#' @param target: the target of the model, have to be specified if formula is not
#' @keywords decision tree

#' @examples
#' library(superml)
#' data(iris)
#' df = iris
#' df$Species = as.factor(LabelEncoder$new()$fit_transform(df$Species))
#' formula = "Species ~ ."
#' r_1 = dt_train(df, formula)




dt_train <- function(df, formula = NULL, df_test = NULL, p = 0.8,
                     params_ = list(method = "recursive.partition",
                                    split = c("deviance", "gini")),
                     target = NULL) {
  if (!is.null(target)){df[[target]] = as.factor(df[[target]])}
  if (is.null(df_test)){
    if (is.null(target)){target = names(df)[[length(names(df))]]}
    index = caret::createDataPartition(y = df[[target]], p = p, list = FALSE)
    df_train = df[index,]
    df_test = df[-index,]
  }else{
    df_train = df
  }
  df_t = df_test[,!(names(df_test) %in% c(target))]
  if (is.null(formula)){formula = paste0(target, " ~ .")}
  dt = tree::tree(as.formula(formula), data = df_train, method = params_[['method']],
                  split = params_[['split']])
  dt_p = predict(dt, newdata = df_t, type = "class")
  df_test = data.frame(cbind(df_test, dt_p))
  return(df_test)
}


#' @export
