library(leaps)
library(tidyverse)
library(sjlabelled)
library(ROCR)

survey <- haven::read_sav("cleaned_extra.sav")
# survey <- haven::read_sav("dem_only.sav")

# remove SOCTRUST columns for processing and later re-append
surveyA <- subset (survey, select = -c(SOCASSIGN_W32,SOCTRUST_W32,SOCTRUST2_W32,SOCTRUST3_W32,SOCTRUST4_W32,SOCTRUST5_W32))
# also remove DIVISION_BY_MSA
surveyA <- subset (surveyA, select = -c(DIVISION_BY_MSA))
# select 2-level SOCTRUST columns we want to keep and process
surveyB <- subset (survey, select = c(SOCTRUST_W32,SOCTRUST2_W32,SOCTRUST5_W32))

# keep 4-level SOCTRUST column for condensing
surveyC <- subset (survey, select = SOCTRUST4_W32)

# condense 1,2 to 1, and 3,4 to 2 in SOCTRUST4
surveyC[surveyC==2] <- 1 
surveyC[surveyC==3] <- 2
surveyC[surveyC==4] <- 2

surveyBC <- cbind(surveyB,surveyC)

# collapse all columns to one
surveyBC$TRUST <-apply(surveyBC, 1, function(x) paste(x[!is.na(x)]))

# fix class
TRUST <- as.numeric(surveyBC$TRUST)

# put it back together
surveyD <- cbind(surveyA,TRUST)

surveyD <- surveyD %>%
  mutate(across(!contains("THERM"), ~ replace(.x, .x==99, NA))) %>%
  mutate(across(contains("THERM"), ~ replace(.x, .x==999, NA))) %>%
  mutate(across(!contains("THERM"), to_factor))

surveyD <- surveyD[complete.cases(surveyD),]
surveyD$WEIGHT_W32 <- as.numeric(surveyD$WEIGHT_W32)

##############

set.seed(1)
sample<-sample.int(nrow(surveyD), floor(.80*nrow(surveyD)), replace = F)
train<-surveyD[sample, ]
test<-surveyD[-sample, ]

############

fullmod <- glm(TRUST ~ . -THERMOBAMA_W32 -THERMPENCE_W32 -THERMTRUMP_W32 -WEIGHT_W32, family = 'binomial', data=train)
summary(fullmod)

backwards <- step(fullmod) # Backwards selection is the default


##############

result <- glm(TRUST ~ SATLIFEB_W32 + SATLIFEE_W32 + COMATTACH_W32 + FEELA_W32 + 
                FEELB_W32 + FEELC_W32 + INC_W32 + COMMIMPC_W32 + FEDSHAREB_W32 + 
                LOCALPROBB_W32 + LOCALPROBD_W32 + LOCALPROBG_W32 + LOCALPROBI_W32 + 
                VALUEURBAN_W32 + NEIGHSAMEB_W32 + NEIGHSAMEC_W32 + IMMCULT2_W32 + 
                WOMENOPPS_W32 + WHADVANT_W32 + GAYMARR2_W32 + F_CREGION_FINAL + 
                F_INTUSER_FINAL + F_RACE + F_AGECAT_FINAL + F_INCOME_RECODE_FINAL + 
                F_VOLSUM_FINAL + F_IDEO_FINAL, family = "binomial", data=train)
summary(result)
