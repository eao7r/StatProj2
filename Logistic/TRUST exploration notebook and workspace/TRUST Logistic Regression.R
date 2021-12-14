#' ---
#' title: "TRUST Logistic Regression"
#' author: "Andy Ortiz - eao7r"
#' date: "12/14/2021"
#' output: 
#'   html_document: 
#'     highlight: espresso
#'     theme: flatly
#' ---
## ---- include=FALSE-----------------------------------------------------------
knitr::purl("TRUST Logistic Regression.Rmd", "TRUST Logistic Regression.R", documentation = 2)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(dplyr)
library(sjlabelled)
library(ROCR)
library(leaps)
library(ggthemes)
library(ggplot2)


#' 
#' ***Starting from pre-processed data, further clean and prepare data for logistic regression model search***  
## -----------------------------------------------------------------------------
survey <- haven::read_sav("cleaned_extra.sav")
# survey <- haven::read_sav("dem_only.sav")

# remove SOCTRUST columns for processing and later re-append
surveyA <- subset (survey, select = -c(SOCASSIGN_W32,SOCTRUST_W32,SOCTRUST2_W32,SOCTRUST3_W32,SOCTRUST4_W32,SOCTRUST5_W32))
# also remove DIVISION_BY_MSA
surveyA <- subset (surveyA, select = -c(DIVISION_BY_MSA))
# select 2-level SOCTRUST columns we want to keep and process
surveyB <- subset (survey, select = c(SOCTRUST_W32,SOCTRUST2_W32,SOCTRUST5_W32))

# keep 4-level SOCTRUST4 column for condensing
surveyC <- subset (survey, select = SOCTRUST4_W32)

# condense 1,2 to 1, and 3,4 to 2 in SOCTRUST4
surveyC[surveyC==2] <- 1 
surveyC[surveyC==3] <- 2
surveyC[surveyC==4] <- 2

# condense 10 level SOCTRUST3 to 2
surveyC2<-subset (survey, select = SOCTRUST3_W32)
surveyC2[surveyC2 <= 5] <- 2
surveyC2[surveyC2 > 5] <- 1

surveyBC <- cbind(surveyB,surveyC,surveyC2)

# collapse all columns to one
surveyBC$TRUST <-apply(surveyBC, 1, function(x) paste(x[!is.na(x)]))

# fix class
TRUST <- as.numeric(surveyBC$TRUST)

# put it back together
surveyD <- cbind(surveyA,TRUST)

# exclude THERMs from factors
surveyD <- surveyD %>%
  mutate(across(!contains("THERM"), ~ replace(.x, .x==99, NA))) %>%
  mutate(across(contains("THERM"), ~ replace(.x, .x==999, NA))) %>%
  mutate(across(!contains("THERM"), to_factor))

# surveyD is what we will work with
surveyD <- surveyD[complete.cases(surveyD),]

# unfactor WEIGHT_W32
surveyD$WEIGHT_W32 <- as.numeric(surveyD$WEIGHT_W32)

#' 
#' ***Split data into Train and Test***  
## -----------------------------------------------------------------------------
set.seed(1)
sample<-sample.int(nrow(surveyD), floor(.80*nrow(surveyD)), replace = F)
train<-surveyD[sample, ]
test<-surveyD[-sample, ]

#' ***Create logistic model from entire Training set except THERM vars.***  
#' ***Use this as initial exploration***  
## -----------------------------------------------------------------------------
fullmod <- glm(TRUST ~ . -THERMOBAMA_W32 -THERMPENCE_W32 -THERMTRUMP_W32 -WEIGHT_W32, family = 'binomial', data=train)
summary(fullmod)

#'   
#' ***Do prediction with above full model using Test data for later comparison.***  
## -----------------------------------------------------------------------------
preds<-predict(fullmod,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$TRUST)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for TRUST")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values

##confusion matrix. Actual values in the rows, predicted classification in cols
table(test$TRUST, preds>0.5)
mytab <- table(test$TRUST, preds>0.5)

TP <- mytab[2,2]
TN <- mytab[1,1]
FP <- mytab[1,2]
FN <- mytab[2,1]
Accuracy <- (TP + TN) / (TP+TN+FP+FN)  
print(paste("Accuracy", Accuracy))

print(paste("False Positive Rate:", FP/(TN+FP)))
print(paste("False Negative Rate:", FN/(FN+TP)))
table(train$TRUST)
table(test$TRUST)
TS0<-fullmod$null.deviance-fullmod$deviance
print(paste("p-value = ", 1-pchisq(TS0,197)))

#' ***Remove insignificant predictors, then do logistic regression and prediction.***  
## -----------------------------------------------------------------------------
reduced <- glm(TRUST ~ SATLIFEA_W32+SATLIFEB_W32+SATLIFEE_W32+COMATTACH_W32+FEELB_W32+FEELC_W32+INC_W32+COMMIMPA_W32+COMMIMPD_W32+LOCALPROBI_W32+LOCALPROBJ_W32+NEIGHSAMEC_W32+IMMCULT2_W32+GAYMARR2_W32+F_EDUCCAT_FINAL+F_RACE+F_PARTYSUM_FINAL+F_AGECAT_FINAL+F_INCOME_RECODE_FINAL+F_IDEO_FINAL, family = 'binomial', data=train)
summary(reduced)

#'   
#' ***Do prediction with above reduced model using Test data.***  
## -----------------------------------------------------------------------------
preds<-predict(reduced,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$TRUST)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for TRUST")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values

##confusion matrix. Actual values in the rows, predicted classification in cols
table(test$TRUST, preds>0.5)
mytab <- table(test$TRUST, preds>0.5)

TP <- mytab[2,2]
TN <- mytab[1,1]
FP <- mytab[1,2]
FN <- mytab[2,1]
Accuracy <- (TP + TN) / (TP+TN+FP+FN)  
print(paste("Accuracy", Accuracy))

print(paste("False Positive Rate:", FP/(TN+FP)))
print(paste("False Negative Rate:", FN/(FN+TP)))
table(train$TRUST)
table(test$TRUST)
TS1<-reduced$null.deviance-reduced$deviance
print(paste("p-value = ", 1-pchisq(TS1,55)))
TS1b<-reduced$deviance-fullmod$deviance
print(paste("p-value = ", 1-pchisq(TS1b,55)))

#' ***Do backwards selection on the reduced model.***  
## -----------------------------------------------------------------------------
backwardsred <- step(reduced) # Backwards selection is the default

#' 
#' 
#' ***The above backwards step does not reduce the model any more.***  
#' ***Remove insignificant predictors, then do logistic regression and prediction.***  
## -----------------------------------------------------------------------------

summary(backwardsred)

#'   
#' ***Do prediction with above backwards reduced model using Test data.***  
## -----------------------------------------------------------------------------
preds<-predict(backwardsred,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$TRUST)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for TRUST")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values

##confusion matrix. Actual values in the rows, predicted classification in cols
table(test$TRUST, preds>0.5)
mytab <- table(test$TRUST, preds>0.5)

TP <- mytab[2,2]
TN <- mytab[1,1]
FP <- mytab[1,2]
FN <- mytab[2,1]
Accuracy <- (TP + TN) / (TP+TN+FP+FN)  
print(paste("Accuracy", Accuracy))

print(paste("False Positive Rate:", FP/(TN+FP)))
print(paste("False Negative Rate:", FN/(FN+TP)))
table(train$TRUST)
table(test$TRUST)
TS2<-backwardsred$null.deviance-backwardsred$deviance
print(paste("p-value = ", 1-pchisq(TS2,52)))
TS2b<-backwardsred$deviance-reduced$deviance
print(paste("p-value = ", 1-pchisq(TS2b,3)))

#' ***Remove Demographic predictors from back reduced model.***
#' 
#' 
## -----------------------------------------------------------------------------
backred_noDem<- glm(TRUST ~ SATLIFEB_W32 + SATLIFEE_W32 + COMATTACH_W32 + 
    FEELB_W32 + FEELC_W32 + INC_W32 + COMMIMPA_W32 + COMMIMPD_W32 + 
    LOCALPROBI_W32 + LOCALPROBJ_W32 + NEIGHSAMEC_W32 + IMMCULT2_W32 + 
    GAYMARR2_W32, family = "binomial", 
    data = train)
summary(backred_noDem)

#' 
#'   
#' ***Do prediction with above backwards reduced noDem model using Test data.***  
## -----------------------------------------------------------------------------
preds<-predict(backred_noDem,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$TRUST)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for TRUST")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values

##confusion matrix. Actual values in the rows, predicted classification in cols
table(test$TRUST, preds>0.5)
mytab <- table(test$TRUST, preds>0.5)

TP <- mytab[2,2]
TN <- mytab[1,1]
FP <- mytab[1,2]
FN <- mytab[2,1]
Accuracy <- (TP + TN) / (TP+TN+FP+FN)  
print(paste("Accuracy", Accuracy))

print(paste("False Positive Rate:", FP/(TN+FP)))
print(paste("False Negative Rate:", FN/(FN+TP)))
table(train$TRUST)
table(test$TRUST)
TS3<-backred_noDem$null.deviance-backred_noDem$deviance
print(paste("p-value = ", 1-pchisq(TS3,34)))

#' ***Do back red with no ATTitude***
#' 
## -----------------------------------------------------------------------------
backred_noATT <- glm(TRUST ~ F_EDUCCAT_FINAL + F_RACE + F_PARTYSUM_FINAL + F_AGECAT_FINAL + 
    F_INCOME_RECODE_FINAL + F_IDEO_FINAL, family = "binomial", 
    data = train)
summary(backred_noATT)

#' ***Do prediction with above backwards reduced noATT model using Test data.***  
## -----------------------------------------------------------------------------
preds<-predict(backred_noATT,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$TRUST)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for TRUST")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values

##confusion matrix. Actual values in the rows, predicted classification in cols
table(test$TRUST, preds>0.5)
mytab <- table(test$TRUST, preds>0.5)

TP <- mytab[2,2]
TN <- mytab[1,1]
FP <- mytab[1,2]
FN <- mytab[2,1]
Accuracy <- (TP + TN) / (TP+TN+FP+FN)  
print(paste("Accuracy", Accuracy))

print(paste("False Positive Rate:", FP/(TN+FP)))
print(paste("False Negative Rate:", FN/(FN+TP)))
table(train$TRUST)
table(test$TRUST)
TS4<-backred_noATT$null.deviance-backred_noATT$deviance
print(paste("p-value = ", 1-pchisq(TS4,34)))

#' 
#' 
#' 
