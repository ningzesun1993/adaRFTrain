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

grid_search_rf <- function(df, params_range, n_fold = 3, scoring = c("accuracy", "auc"),
                           params_ = list(ntree = 500, sampsize = NULL, nodesize = NULL,
                                          maxnodes = NULL, replace = TRUE, mtry = NULL,
                                          Stratify = TRUE), target = NULL) {
  if (!is.null(target)){df[[target]] = as.factor(df[[target]])}
  if (!is.null(df_test)){
    if (is.null(target)){target = names(df)[[length(names(df))]]}
  }
  rf = superml::RFTrainer$new()
  rf_grid = superml::GridSearchCV$new(trainer = rf, parameters = params_range,
                         n_folds = n_fold, scoring = scoring)
  rf_grid$fit(df, target)
  result = rf_grid$best_iteration(metric = NULL)
  for (i in names(params_range)){
    params_[[i]] = result[[i]]
  }
  return(params_)
}
