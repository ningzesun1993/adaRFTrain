## Group Project Presentation

In this repo, we build a R package to train and predict decision tree and random forest, 

meanwhile build a function to use grid search and cross-validation to tune the hyper parameter of random forest

To install this package and read the file of vignettes, please follow this codes:

library(devtools)

install_github("ningzesun1993/adaRFTrain", build_vignettes = TRUE, force = TRUE)

browseVignettes("adaRFTrain")

The repo structure:

doc: vignettes results

Images: images needed in vignettes

man, Meta: automatically generated from the packages

R: main folder for R code including dt_train, grid_search, rf_train

vignettes: codes for vignettes

adaRFTrain_0.1.0.tar: shared tar file for the packages

database.sqilte, Iris.csv: the dataset in this packages