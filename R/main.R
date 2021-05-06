# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

params = list(method = "recursive.partition", split = c("deviance", "gini"))

dt_train <- function(df, formula, df_test = NULL, p = 0.8,
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
  dt = tree::tree(as.formula(formula), data = df_train, method = params_[['method']], split = params_[['split']])
  dt_p = predict(dt, newdata = df_test, type = "class")
  df_test = data.frame(cbind(df_test, dt_p))
  return(df_test)
}
