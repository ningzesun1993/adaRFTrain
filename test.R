library(devtools)
library(usethis)
devtools::build()
devtools::load_all()
library(adaRFTrain)
library(caret)
library(tree)
library(superml)

data(iris)
df = iris
df$Species = as.factor(LabelEncoder$new()$fit_transform(df$Species))
train_index = createDataPartition(y = df$Species, p = 0.75, list = FALSE)
df_train = df[train_index,]
df_test = df[-train_index,]
formula = "Species ~ ."
target = "Species"
r_1 = dt_train(df, formula)
r_2 = dt_train(df_train, formula, df_test)
r_3 = rf_train(df, formula)
r_4 = rf_train(df_train, formula, df_test)
params_range = list(n_estimators = seq(50, 200, 10),
                    max_depth = seq(2, 10, 2))
params = grid_search_rf(df, params_range, target = target)
usethis::use_package_doc()
devtools::document()
devtools::build()
devtools::load_all()
usethis::use_vignette("adaRF", title="train dt and rf")
devtools::build_vignettes()
devtools::build()
