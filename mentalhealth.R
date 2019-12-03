setwd("~/Documents/Bioinformatics/Classes/FALL/API222/Assignments/Group/Dataset")


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
          "randomForest", "e1071", "caret")


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

library(mice)
mental2 <- mice(mental)
mental <- complete(mental2, 1)

#  Putting variables in appropriate format 
mental<- mental%>%mutate_at("gender", function(x){as.character(x)})
mental<- mental%>%mutate_at("gender", function(x){as.factor(x)})

# Adding regions to countries

#install.packages("countrycode")
library(countrycode)
mental$region <- countrycode(mental$country, origin="country.name", destination="region")
mental<- mental%>%mutate_at("region", function(x){as.factor(x)})

mental <- mental%>%mutate(
  age = case_when(
    age < 17 ~ 40,
    age > 100 ~ 40,
    TRUE ~ age
  )
)

# Cleaned dataset

write.csv(mental, file="mental_clean.csv")



# Loading Cleaned Data
#mental <- read.csv("./mental_clean.csv") 

# Data Analysis -----------------------------------------------------------

mental <- mental %>% mutate(id = row_number()) 
train <- mental %>% sample_frac(.80)
test  <- anti_join(mental , train, by = "id")


# Classifier #1 : Outcome = mi_treatment



twoClassSummaryCustom = function (data, lev = NULL, model = NULL) 
{
  lvls <- levels(data$obs)
  if (length(lvls) > 2)
    stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
  if (!all(levels(data[, "pred"]) == lvls)) 
    stop("levels of observed and predicted data do not match")
  rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 
                                     1), data[, lvls[1]])
  out <- c(rocAUC,
           sensitivity(data[, "pred"], data[, "obs"], lev[1]),
           specificity(data[, "pred"], data[, "obs"], lev[2]),
           posPredValue(data[, "pred"], data[, "obs"], lev[1]))
  names(out) <- c("ROC", "Sens", "Spec", "Prec")
  out
}


trControl <- trainControl(method  = "cv",
                          number  = 5,
                          summaryFunction =  twoClassSummaryCustom, #Needed for ROC metric
                          savePredictions = "all", #Needed for thresholder
                          classProbs = T #Needed for classification
) 


logistic_mental2 <- caret::train(mi_treatment ~ age + gender + region + self_employed + mi_fhx + employees_number + remote_work + 
                                  tech_company + employer_mi_wellnessprogram + employer_resources + mi_workinterference + mi_benefits + mi_benefits_awareness,
                                method     = "glm",
                                preProcess = c("range"),
                                metric     = "ROC",
                                trControl  = trControl,
                                data = mental)

logistic_mental_no <- caret::train(mi_treatment ~ age + gender + region + self_employed + mi_fhx + employees_number + remote_work + 
                                   tech_company + employer_mi_wellnessprogram + employer_resources + mi_workinterference + mi_benefits + mi_benefits_awareness,
                                 method     = "glm",                                 
                                 metric     = "ROC",
                                 trControl  = trControl,
                                 data = mental)


tg <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2), 
                  interaction.depth = c(1, 3, 7, 10),
                  n.minobsinnode = c(2, 5, 10),
                  n.trees = c(100, 300, 500, 1000))                        



modeldt <- caret::train(mi_treatment ~ age + gender + region + self_employed + mi_fhx + employees_number + remote_work + 
                          tech_company + employer_mi_wellnessprogram + employer_resources + mi_workinterference + mi_benefits + mi_benefits_awareness,
                                method     = "rpart",
                                metric     = "ROC",
                                trControl  = trControl,
                                data = mental)

rpart.plot::rpart.plot(modeldt$finalModel, type = 4, fallen.leaves = TRUE, extra = 2)


gbm_model <- caret::train(mi_treatment ~ age + gender + region + self_employed + mi_fhx + employees_number + remote_work + 
                            tech_company + employer_mi_wellnessprogram + employer_resources + mi_workinterference + mi_benefits + mi_benefits_awareness,
                                method     = "gbm",
                                metric     = "ROC",
                                trControl  = trControl,
                                data = mental)

library(randomForest)
modelrf <- caret::train(mi_treatment ~ age + gender + region + self_employed + mi_fhx + employees_number + remote_work + 
                           tech_company + employer_mi_wellnessprogram + employer_resources + mi_workinterference + mi_benefits + mi_benefits_awareness,
                         method     = "rf",
                         metric     = "ROC",
                         trControl  = trControl,
                         data = mental)



modelSVM <- caret::train(mi_treatment ~ age + gender + region + self_employed + mi_fhx + employees_number + remote_work + 
                           tech_company + employer_mi_wellnessprogram + employer_resources + mi_workinterference + mi_benefits + mi_benefits_awareness,
                        method     = "svmRadial",
                        metric     = "ROC",
                        trControl  = trControl,
                        data = mental)



results2 <- resamples(list("Random Forest"=modelrf, "SVM"=modelSVM, "Logistic Regression"=logistic_mental2, "Decision Tree" = modeldt, "Gradient Boosting" = gbm_model), metric="accuracy")


# summarize the distributions
summary(results2)

dotplot(results2)

plot(varImp(logistic_mental2))

write.csv(as.data.frame(summary(logistic_mental2$finalModel)$coef), file="regression1.csv" )


# Bias

p <-ggplot(mental, aes(x=gender, fill=mi_treatment)) +
  geom_bar(position="stack", alpha=0.5) +
  theme(legend.position="top", axis.text.x = element_text(angle=90, hjust=1))+
  labs(title="Variables distribution in the dataset",x="Gender", y = "Count", fill="Mental Illness treatment")

p

# Accuracy

mental_g <- mental%>%filter(region=="Eastern Europe")

predictions <- predict(logistic_mental_no, mental)
accuracy   <- mean(predictions == mental$mi_treatment, na.rm=F)

# Classifier #2 : Outcome = discussing_supervisor

logistic_mental2_2 <- caret::train(mi_treatment ~ age + gender + region 
                                 + mi_fhx + mi_workinterference + employees_number 
                                 + remote_work + mi_benefits + mi_benefits_awareness
                                 + employer_mi_wellnessprogram 
                                 + employer_resources + anonymity_protection + medical_leave_ease
                                 + discussing_negative + discussing_interview + discussing_physical
                                 + employer_seriousness + observed_negative,
                                 method     = "glm",
                                 preProcess = c("range"),
                                 metric     = "ROC",
                                 trControl  = trControl,
                                 data = mental)

tg_2 <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2), 
                  interaction.depth = c(1, 3, 7, 10),
                  n.minobsinnode = c(2, 5, 10),
                  n.trees = c(100, 300, 500, 1000)) 

modeldt_2 <- caret::train(mi_treatment ~ age + gender + region 
                          + mi_fhx + mi_workinterference + employees_number 
                          + remote_work + mi_benefits + mi_benefits_awareness
                          + employer_mi_wellnessprogram 
                          + employer_resources + anonymity_protection + medical_leave_ease
                          + discussing_negative + discussing_interview + discussing_physical
                          + employer_seriousness + observed_negative,
                        method     = "rpart",
                        metric     = "ROC",
                        trControl  = trControl,
                        data = mental)

modelrf_2 <- caret::train(mi_treatment ~ age + gender + region 
                          + mi_fhx + mi_workinterference + employees_number 
                          + remote_work + mi_benefits + mi_benefits_awareness
                          + employer_mi_wellnessprogram 
                          + employer_resources + anonymity_protection + medical_leave_ease
                          + discussing_negative + discussing_interview + discussing_physical
                          + employer_seriousness + observed_negative,
                        method     = "rf",
                        metric     = "ROC",
                        trControl  = trControl,
                        data = mental)

results2_2 <- resamples(list("Random Forest"=modelrf_2, "Logistic Regression"=logistic_mental2_2, "Decision Tree" = modeldt_2), metric="accuracy")

# summarize the distributions
summary(results2_2)

dotplot(results2_2)

plot(varImp(logistic_mental2_2))

write.csv(as.data.frame(summary(logistic_mental2$finalModel)$coef), file="regression2.csv" )

# Bias
# Accuracy
