# Title : Predicting Trump Approval
# Author : Alex 
# Date : 9 Dec 2021

library(leaps)
library(tidyverse)
library(MASS)
library(haven)
library(sjlabelled)
library(ggbeeswarm)

### Read this \/
## Write in your file path to the data set... "dem_only.sav"
survey <- read_sav("STAT6021/PRJ2_old/dem_only.sav")

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

#correlation table
corr_table <- cor(survey %>% mutate(across(everything()
                                           ,as.integer, na.rm = T)))

#Recoding a few variables
survey$F_EDUCCAT_FINAL <- ifelse(survey$F_EDUCCAT_FINAL == 1, "College",
                                 "Non-college")

survey$F_AGECAT_FINAL_2 <- ifelse(survey$F_AGECAT_FINAL %in% 1:2, "Under 50", "50+")

survey$F_MARITAL_FINAL <- ifelse(survey$F_MARITAL_FINAL %in% 3:6, "Single", "Non-single")

survey$F_IDEO_FINAL <- ifelse(survey$F_IDEO_FINAL %in% 1:2, "Conservative", 
                              ifelse(survey$F_IDEO_FINAL %in% 4:5, "Liberal",
                                     "Moderate"))

survey$THERMTRUMP_W32 <- as.integer(survey$THERMTRUMP_W32)

# EDA

#plot all the means, then show breaks and possible interactions
names_to_iterate <- names(survey)[-c(8, 12, 13)]

summary_stats <- map(.x = names_to_iterate, function(x){
  survey[,c(x, "THERMTRUMP_W32")] %>%
    mutate(across(1, sjlabelled::as_label)) %>%
    group_by(get(x)) %>%
    summarise(variable = x,
              therm = mean(THERMTRUMP_W32),
              upperlim = therm + (1.96 * sd(THERMTRUMP_W32)/sqrt(n())),
              lowerlm = therm - (1.96 * sd(THERMTRUMP_W32)/sqrt(n())))}) %>% 
  bind_rows() %>% 
  rename("levels" = `get(x)`)

summary_stats$levels <- factor(summary_stats$levels, 
                               levels = summary_stats$levels)

summary_stats %>% 
  ggplot(aes(x = levels, y = therm, color = variable)) +
  geom_point() +
  geom_errorbar(aes(ymax = upperlim, ymin = lowerlm), width = 0.1) +
  theme_minimal() +
  labs(y = "Trump Approval From Cold (0) to Warm (100)", 
       x = "levels of all variables",
       title = "Mean of every variable's levels") +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "none") +
  annotate(
    geom = "curve", x = 16, y = 78, xend = 18.8, yend = 70, #16 78
    curvature = -.3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 15.8, y = 78, 
           label = "Biggest Gap in Party", 
           hjust = "right")

ggsave("fig1.png")


#figures to look at COMMUNITY, RACE, PARTY, AGE, IDEOLOGY, Education


#Party
survey %>% 
  rename("Party" = "F_PARTYSUM_FINAL", "Trump_Approval" = "THERMTRUMP_W32") %>% 
  mutate(Party = as_label(Party)) %>% 
  ggplot() +
  geom_boxplot(aes(x = Party, y = Trump_Approval)) +
  geom_quasirandom(aes(x = Party, y = Trump_Approval, color = Party), alpha = 0.3)+
  labs(title = "Democrats Strongly Unfavorable to Trump",
       y = "Trump Approval From Cold (0) to Warm (100)") +
  theme_classic() +
  scale_color_manual(values = c("firebrick3", "dodgerblue4", "gray10")) +
  theme(legend.position = "none")
ggsave("fig2.png")


# Party By Age
survey %>% 
  rename("Party" = "F_PARTYSUM_FINAL", "Trump_Approval" = "THERMTRUMP_W32") %>% 
  mutate(Party = as_label(Party),
         F_AGECAT_FINAL_2 = factor(F_AGECAT_FINAL_2, levels = c("Under 50",
                                                                "50+"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = Party, y = Trump_Approval)) +
  geom_quasirandom(aes(x = Party, y = Trump_Approval, color = Party), alpha = 0.3)+
  labs(title = "Younger GOP Slightly Less Favorable of Trump Than Older",
       y = "Trump Approval From Cold (0) to Warm (100)") +
  theme_classic() +
  scale_color_manual(values = c("firebrick3", "dodgerblue4", "gray10")) +
  theme(legend.position = "none") +
  facet_grid(~F_AGECAT_FINAL_2)
ggsave("fig3.png")

#race X education
survey %>% 
  rename("Race" = "F_RACE", "Trump_Approval" = "THERMTRUMP_W32") %>% 
  mutate(Race = factor(Race,
                        levels = c("White non-hispanic", 
                                   "Black non-hispanic",
                                   "Hispanic", 
                                   "Asian non-hispanic",
                                   "Other/Refused"),
                        labels = c("White", "Black", "Hispanic", "Other", "Other"))) %>% 
  filter(Race != "Other") %>% 
  ggplot() +
  geom_boxplot(aes(x = Race, y = Trump_Approval)) +
  geom_quasirandom(aes(x = Race, y = Trump_Approval, color = Race), alpha = 0.3)+
  labs(title = "White College Less Supportive Of Trump",
       y = "Trump Approval From Cold (0) to Warm (100)") +
  theme_classic() +
  theme(legend.position = "none") +
  facet_grid(~F_EDUCCAT_FINAL)
ggsave("fig4.png")

#community X age
survey %>% 
  rename("Region" = "F_CREGION_FINAL", "Trump_Approval" = "THERMTRUMP_W32") %>% 
  mutate(Region = as_label(Region)) %>% 
  ggplot() +
  geom_boxplot(aes(x = Region, y = Trump_Approval)) +
  geom_quasirandom(aes(x = Region, y = Trump_Approval, color = Region), alpha = 0.3)+
  labs(title = "Younger Southerners Much Less Supportive Of Trump",
       y = "Trump Approval From Cold (0) to Warm (100)") +
  theme_classic() +
  theme(legend.position = "none") +
  facet_grid(~F_AGECAT_FINAL_2)
ggsave("fig5.png")

#party X ideology
survey %>% 
  ggplot() +
  geom_histogram(aes(x = THERMTRUMP_W32), fill = "firebrick4")+
  labs(x = "Trump Approval From Cold (0) to Warm (100)",
       title = "Lots of 0's, 50's, and 100's") +
  theme_minimal()
ggsave("fig6.png")