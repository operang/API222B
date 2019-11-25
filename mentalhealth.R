#setwd("./")


# Library -----------------------------------------------------------------

install_or_load_pack <- function(pack){
  new.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

# Packages used

pack <- c("bigrquery","plotly","scales","RColorBrewer","data.table","tidyverse",
          "knitr","corrplot","Hmisc","stats", "janitor", "lubridate", "testthat", "magrittr", 
          "gam", "splines",
          "purrr", "caret", "caretEnsemble", "skimr", "DataExplorer", "knn", "kknn", "MASS", 
          "RANN", "gbm", "glmnet", "fastDummies", "forcats", "MLmetrics", "mice", "ROSE", "lsr",
          "randomForest", "e1071")



# Data Cleaning -----------------------------------------------------------

mental <- as.tibble(read.csv("./Dataset/mental_2014.csv)