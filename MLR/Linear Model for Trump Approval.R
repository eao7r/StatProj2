library(leaps)
library(tidyverse)
library(MASS)
library(faraway)
library(sjlabelled)
library(haven)

### Read this \/
## Write in your file path to the data set... "dem_only.sav"
survey <- read_sav("dem_only.sav")

survey <- survey %>%
  mutate(across(!contains("THERM"), ~ replace(.x, .x==99, NA))) %>%
  mutate(across(contains("THERM"), ~ replace(.x, .x==999, NA))) %>%
  mutate(across(!contains("THERM"), to_factor))

survey$WEIGHT_W32 <- NULL

# creating a social trust variable
survey$SOCTRUST3_W32 <- ifelse(is.na(survey$SOCTRUST3_W32), NA, 
                               ifelse(survey$SOCTRUST3_W32 %in% 0:5, '2', '1'))

survey$SOCTRUST4_W32 <- ifelse(is.na(survey$SOCTRUST4_W32), NA,
                               ifelse(survey$SOCTRUST4_W32 %in% 1:2, '1', '2'))

survey$F_SOCTRUST <- paste0(survey$SOCTRUST_W32,
                            survey$SOCTRUST2_W32,
                            survey$SOCTRUST3_W32,
                            survey$SOCTRUST4_W32,
                            survey$SOCTRUST5_W32)

survey$F_SOCTRUST <- gsub("NA", "", survey$F_SOCTRUST)

(survey$F_SOCTRUST <- factor(ifelse(survey$F_SOCTRUST == "", NA, survey$F_SOCTRUST),
                             levels = c('1', '2'),
                             labels = c('More Social Trust',
                                        'Less Social Trust')))

survey$SOCTRUST_W32 <- NULL
survey$SOCTRUST2_W32 <- NULL
survey$SOCTRUST3_W32 <- NULL
survey$SOCTRUST4_W32 <- NULL
survey$SOCTRUST5_W32 <- NULL

#removing non-completes
survey <- survey[complete.cases(survey),]

#Setting aside for possible future study
set_aside <- survey[c("THERMOBAMA_W32", "THERMPENCE_W32")]
survey <- survey[ ,!names(survey) %in% c("THERMOBAMA_W32", "THERMPENCE_W32")]

#Recoding a few variables
survey$F_EDUCCAT_FINAL <- ifelse(survey$F_EDUCCAT_FINAL == 1, "College",
                                 "Non-college")

survey$F_AGECAT_FINAL <- ifelse(survey$F_AGECAT_FINAL %in% 1:2, "Under 50", "50+")

survey$F_MARITAL_FINAL <- ifelse(survey$F_MARITAL_FINAL %in% 3:6, "Single", "Non-single")

survey$F_IDEO_FINAL <- ifelse(survey$F_IDEO_FINAL %in% 1:2, "Conservative", 
                              ifelse(survey$F_IDEO_FINAL %in% 4:5, "Liberal",
                                     "Moderate"))

survey$F_RACE <- ifelse(survey$F_RACE == "White non-hispanic", "White", "Non-white")

#remove ideo_final, highly correlated with party 
survey$F_IDEO_FINAL <- NULL

#create the full model
result <- lm(survey$THERMTRUMP_W32 ~ . + survey$F_RACE * survey$F_EDUCCAT_FINAL 
             + survey$F_PARTYSUM_FINAL * survey$F_AGECAT_FINAL + 
               survey$F_COMMUNITY * survey$F_AGECAT_FINAL, data = survey)
summary(result)

#drop community x age, intuser, income, and marital status
reduced <- lm(survey$THERMTRUMP_W32 ~ survey$F_CREGION_FINAL + survey$F_SEX_FINAL +
                survey$F_COMMUNITY + survey$F_VOLSUM_FINAL + survey$F_SOCTRUST + 
                survey$F_RACE * survey$F_EDUCCAT_FINAL + 
                survey$F_PARTYSUM_FINAL * survey$F_AGECAT_FINAL, data = survey)

#run partial f test
anova(reduced, result)
#insignificant, can't reject null so we can drop the terms

#drop social trust
reduced1 <- lm(survey$THERMTRUMP_W32 ~ survey$F_CREGION_FINAL + survey$F_SEX_FINAL +
                 survey$F_COMMUNITY + survey$F_VOLSUM_FINAL + 
                 survey$F_RACE * survey$F_EDUCCAT_FINAL + 
                 survey$F_PARTYSUM_FINAL * survey$F_AGECAT_FINAL, data = survey)
anova(reduced1, reduced)
#p value significant, we reject the null so we can't drop social trust

#drop region
reduced2 <- lm(survey$THERMTRUMP_W32 ~ survey$F_SEX_FINAL +
                 survey$F_COMMUNITY + survey$F_VOLSUM_FINAL + survey$F_SOCTRUST + 
                 survey$F_RACE * survey$F_EDUCCAT_FINAL + 
                 survey$F_PARTYSUM_FINAL * survey$F_AGECAT_FINAL, data = survey)
anova(reduced2, reduced)               
#p value significant, we reject the null so we can't drop region

#final model
#region, sex, community, volsum, soctrust, race x educcat, partysum x agecat 

############################################################################################3
#see what final model allreg picks
allreg <- regsubsets(survey$THERMTRUMP_W32 ~ survey$F_CREGION_FINAL + 
                       survey$F_SEX_FINAL + survey$F_INTUSER_FINAL + survey$F_INCOME_RECODE_FINAL
                     + survey$F_VOLSUM_FINAL + survey$F_MARITAL_FINAL + survey$F_SOCTRUST + 
                       survey$F_RACE * survey$F_EDUCCAT_FINAL 
                     + survey$F_PARTYSUM_FINAL * survey$F_AGECAT_FINAL + 
                       survey$F_COMMUNITY * survey$F_AGECAT_FINAL, data = survey, nbest=1)
summary(allreg)

#best adjr2:
coef(allreg, which.max(summary(allreg)$adjr2))

#best mallow's cp
coef(allreg, which.min(summary(allreg)$cp))

#best bic
coef(allreg, which.min(summary(allreg)$bic))

#try model selection
##intercept only model
regnull <- lm(survey$THERMTRUMP_W32 ~ 1, data = survey)
##model with all predictors
regfull <- lm(survey$THERMTRUMP_W32 ~ survey$F_CREGION_FINAL + 
                survey$F_SEX_FINAL + survey$F_INTUSER_FINAL + survey$F_INCOME_RECODE_FINAL
              + survey$F_VOLSUM_FINAL + survey$F_MARITAL_FINAL + survey$F_SOCTRUST + 
                survey$F_RACE * survey$F_EDUCCAT_FINAL 
              + survey$F_PARTYSUM_FINAL * survey$F_AGECAT_FINAL + 
                survey$F_COMMUNITY * survey$F_AGECAT_FINAL, data = survey)

#forward selection
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")

#backward selection
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")

#stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

#forward selection, backward selection, and stepwise regression all picked the same model

#reduced:
#region, sex, community, volsum, soctrust, race x educcat, partysum x agecat

#based on our original investigation and the model selection we ran, I think the best model contains 
#region, sex, community, volsum, soctrust, race x educcat, partysum x agecat

#this is the model we found at the beginning by taking one predictor out at a time and also the
#model found using forward selection, backward selection, and stepwise regression 

#using the allreg method and using adjr2, mallow's cp, and bic we found volsum, race, parysum x age; 
#this drops region, sex, community, soctrust, race x educcat, all of which we found significant
#during our initial investigation

########################################################################################

#vifs
vif(reduced3)

#check for outliers 
#cook's d
COOKS <- cooks.distance(reduced3)
n <- dim(survey)[1]
p <- 17
COOKS[COOKS>qf(0.5, p, n - p)]

#DFFITs
DFFITS <- dffits(reduced3)
DFFITS[abs(DFFITS) > 2*sqrt(p/n)]


#DFBETAs
DFBETAS <- dfbetas(reduced3)
abs(DFBETAS) > 2/sqrt(n)

#for beta0
DFBETAS[abs(DFBETAS[,1])>2/sqrt(n),1]

#for beta1
DFBETAS[abs(DFBETAS[,2])>2/sqrt(n),2]

#for beta2
DFBETAS[abs(DFBETAS[,3])>2/sqrt(n),3]

##for beta3
DFBETAS[abs(DFBETAS[,4])>2/sqrt(n),4]

#for both DFFITS and DFBETAS there are a couple hundred but compared to the size of our data set
#that is about 6-8% of the data set

summary(reduced3)

#plot residuals 
yhat <- reduced3$fitted.values
res <- reduced3$residuals

residual_plot <- data.frame(yhat, res)

ggplot(residual_plot, aes(x=yhat,y=res))+
  geom_point()+
  geom_hline(yintercept=0, color="red")+
  labs(x="Fitted y",
       y="Residuals",
       title="Residual Plot of Reduced model")

#plot residuals for all categories and compare
yhat2 <- result$fitted.values
res2 <- result$residuals

residual_plot <- data.frame(residual_plot, yhat, res, yhat2, res2)

ggplot(residual_plot, aes(x=yhat2,y=res2))+
  geom_point()+
  geom_hline(yintercept=0, color="red")+
  labs(x="Fitted y",
       y="Residuals",
       title="Residual Plot of Full model")
