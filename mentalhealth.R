#setwd("~/Documents/Bioinformatics/Classes/FALL/API222/Assignments/Group/Dataset")


# Library -----------------------------------------------------------------

install_or_load_pack <- function(pack){
  new.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

# Packages used

pack <- c("data.table","tidyverse",
          "knitr","corrplot","Hmisc","stats", "janitor", "lubridate", "testthat", "magrittr", 
          "gam", "splines",
          "purrr", "caret", "caretEnsemble", "skimr", "DataExplorer", "knn", "kknn", "MASS", 
          "RANN", "gbm", "glmnet", "fastDummies", "forcats", "MLmetrics", "mice", "ROSE", "lsr",
          "randomForest", "e1071")


install_or_load_pack (pack)

# Data Cleaning -----------------------------------------------------------

# Name cleaning done in excel
mental <- read.csv("./mental_2014.csv") 

#  Appropriate gender
table(mental$gender)
mental$gender[grep("^w",mental$gender, ignore.case = TRUE)] <- "F"
mental$gender[grep("^f",mental$gender, ignore.case = TRUE)] <- "F"
mental$gender[grep("^m",mental$gender, ignore.case = TRUE)] <- "M"
mental$gender[-which((mental$gender%in%c("F","M")))] <- "Other"

# Null value identification
mental <- mental%>%na_if('')

# Deleting
mental <- mental%>%dplyr::select(-c("date", "state"))

# Imputation using Mean Matching

mental2 <- mice(mental)
mental <- complete(mental2, 1)

#  Putting variables in appropriate format 
mental<- mental%>%mutate_at("gender", function(x){as.character(x)})
mental<- mental%>%mutate_at("gender", function(x){as.factor(x)})

# Adding regions to countries

install.packages("countrycode")
library(countrycode)
mental$region <- countrycode(mental$country, origin="country.name", destination="region")
mental<- mental%>%mutate_at("region", function(x){as.factor(x)})


# Cleaned dataset

#write.csv(mental, file="mental_clean.csv")







# Loading Cleaned Data
#mental <- read.csv("./mental_clean.csv") 

# Data Analysis -----------------------------------------------------------

# Classifier #1 : Outcome = mi_treatment

# Classifier #2 : Outcome = discussing_supervisor

