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

params = list(ntree = 500, sampsize = NULL, nodesize = NULL, maxnodes = NULL,
              replace = TRUE, mtry = NULL, Stratify = TRUE)

rf_train <- function(df, formula, df_test = NULL, p = 0.8,
                     params_ = list(ntree = 500, sampsize = NULL, nodesize = NULL,
                                    maxnodes = NULL, replace = TRUE, mtry = NULL,
                                    Stratify = TRUE), target = NULL) {
  if (!is.null(target)){df[[target]] = as.factor(df[[target]])}
  if (is.null(df_test)){
    if (is.null(target)){target = names(df)[[length(names(df))]]}
    index = caret::createDataPartition(y = df[[target]], p = p, list = FALSE)
    df_train = df[index,]
    df_test = df[-index,]
  }else{
    df_train = df
  }
  if (is.null(params_[['sampsize']])){params_[['sampsize']] = nrow(df_train)}
  if (is.null(params_[['mtry']])){params_[['mtry']] = floor(sqrt(ncol(df_train)))}
  rf = randomForest::randomForest(as.formula(formula), data = df_train, ntree = params_[['ntree']],
                                  sampsize = params_[['sampsize']], nodesize = params_[['nodesize']],
                                  maxnodes = params_[['maxnodes']], replace = params_[['replace']],
                                  mtry = params_[['mtry']], Stratify = params_[['Stratify']])
  rf_p = predict(rf, newdata = df_test, type = "class")
  df_test = data.frame(cbind(df_test, rf_p))
  return(df_test)
}
