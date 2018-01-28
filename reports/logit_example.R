

# Libraries
library(tidyverse)
library(glmnet)
library(knitr)
library(Hmisc)
library(RColorBrewer)
library(plotly)
library(databrew)
library(ggmap)
library(reshape2)
library(memisc)
library(texreg)


# Turn off scientific notation
options(scipen = '999')

# source functions
source('functions.R')

# Get all locationsd
if('preprocess_survey_data.RData' %in% dir('data')){
  load('data/preprocess_survey_data.RData')
} else { 
  source('global.R')
}


# get folder names
path_to_data <-  'data/survey_data'
survey_folders <- list.files(path_to_data)
survey_folders
osduhs <- survey[[10]]


# recode the used cannabis variable to binary
osduhs$used_cannabis_ever <- ifelse(grepl("don't know|never used", osduhs$hw_osduhs_12_months_used_cannabis), 'No', 
                                    ifelse(is.na(osduhs$hw_osduhs_12_months_used_cannabis), 
                                           NA, 'Yes'))
osduhs$used_cannabis_ever <- as.factor(osduhs$used_cannabis_ever)


# create fathervariable off existing variables 
osduhs <- osduhs %>%
  mutate(demo_osduhs_father_type_at_home = ifelse(demo_osduhs_father_home == 'yes',
                                                  'Biological father',
                                                  ifelse(demo_osduhs_adop_father_home == 'yes',
                                                         'Adopted father',
                                                         ifelse(demo_osduhs_step_father_home == 'yes',
                                                                'Step father', 'No father figure at home'))))

osduhs <- osduhs[,!grepl('father_home', names(osduhs))]


# create mother variable off of existing varibales
osduhs <- osduhs %>%
  mutate(demo_osduhs_mother_type_at_home = ifelse(demo_osduhs_mother_home == 'yes',
                                                  'Biological mother',
                                                  ifelse(demo_osduhs_adop_mother_home == 'yes',
                                                         'Adopted mother',
                                                         ifelse(demo_osduhs_step_mother_home == 'yes',
                                                                'Step mother', 'No mother figure at home'))))

osduhs <- osduhs[,!grepl('mother_home', names(osduhs))]

# recode demo_osduhs_father_edu
osduhs$demo_osduhs_father_edu <- ifelse(grepl('attended', osduhs$demo_osduhs_father_edu), 
                                        'Attended college or hs or uni',
                                        ifelse(grepl('graduated', osduhs$demo_osduhs_father_edu),
                                               'graduated college or hs or uni',
                                               ifelse(grepl("don't", osduhs$demo_osduhs_father_edu),
                                                      NA,
                                                      ifelse(is.na(osduhs$demo_osduhs_father_edu),
                                                             NA, 
                                                             ifelse(grepl('did not', osduhs$demo_osduhs_father_edu),
                                                                    'No highschool', 'No father')))))

# recode demo_osduhs_mother_edu
osduhs$demo_osduhs_mother_edu <- ifelse(grepl('attended', osduhs$demo_osduhs_mother_edu), 
                                        'Attended college or hs or uni',
                                        ifelse(grepl('graduated', osduhs$demo_osduhs_mother_edu),
                                               'graduated college or hs or uni',
                                               ifelse(grepl("don't", osduhs$demo_osduhs_mother_edu),
                                                      NA,
                                                      ifelse(is.na(osduhs$demo_osduhs_mother_edu),
                                                             NA, 
                                                             ifelse(grepl('did not', osduhs$demo_osduhs_mother_edu),
                                                                    'No highschool', 'No mother')))))


# reocde demo_osduhs_grade
osduhs$demo_osduhs_grade <- ifelse(grepl('6|11/12', osduhs$demo_osduhs_grade), 
                                   NA, 
                                   ifelse(grepl('7|8|9', osduhs$demo_osduhs_grade),
                                          'Grade 7-9',
                                          ifelse(grepl('10|grade 11|grade 12', osduhs$demo_osduhs_grade),
                                                 'Grade 10-12', osduhs$demo_osduhs_grade)))

# reocde ed_osduhs_avg_marks
osduhs$ed_osduhs_avg_marks <- ifelse(grepl('50%|60%', osduhs$ed_osduhs_avg_marks), 
                                     'below 70%', 
                                     ifelse(is.na(osduhs$ed_osduhs_avg_marks), 
                                            NA, as.character(osduhs$ed_osduhs_avg_marks)))


# relevel factors for the model
# 'father_type_at_home', 'mother_type_at_home', 'demo_osduhs_father_edu', 'demo_osduhs_mother_edu',  'demo_osduhs_grade', 'ed_osduhs_avg_marks', 'ds_osduhs_close_with_people_at_school' , 'cv_osduhs_social_media' , 'hw_osduhs_mental_health', 'demo_osduhs_pop_weight')
osduhs$demo_osduhs_father_type_at_home <- as.factor(osduhs$demo_osduhs_father_type_at_home)
osduhs$demo_osduhs_father_type_at_home <- relevel(osduhs$demo_osduhs_father_type_at_home, ref = 'No father figure at home')

osduhs$demo_osduhs_mother_type_at_home <- as.factor(osduhs$demo_osduhs_mother_type_at_home)
osduhs$demo_osduhs_mother_type_at_home <- relevel(osduhs$demo_osduhs_mother_type_at_home, ref = 'No mother figure at home')

osduhs$demo_osduhs_father_edu <- as.factor(osduhs$demo_osduhs_father_edu)
osduhs$demo_osduhs_father_edu <- relevel(osduhs$demo_osduhs_father_edu, ref = 'No father')

osduhs$demo_osduhs_mother_edu <- as.factor(osduhs$demo_osduhs_mother_edu)
osduhs$demo_osduhs_mother_edu <- relevel(osduhs$demo_osduhs_mother_edu, ref = 'No mother')

osduhs$demo_osduhs_grade <- as.factor(osduhs$demo_osduhs_grade)
osduhs$demo_osduhs_grade <- relevel(osduhs$demo_osduhs_grade, ref = 'Grade 7-9')

osduhs$ds_osduhs_close_with_people_at_school <- as.factor(osduhs$ds_osduhs_close_with_people_at_school)
osduhs$ds_osduhs_close_with_people_at_school <- relevel(osduhs$ds_osduhs_close_with_people_at_school, ref = 'strongly disagree')

osduhs$hw_osduhs_mental_health <- as.factor(osduhs$hw_osduhs_mental_health)
osduhs$hw_osduhs_mental_health<- relevel(osduhs$hw_osduhs_mental_health, ref = 'poor')

osduhs$demo_osduhs_race <- as.factor(osduhs$demo_osduhs_race)
osduhs$demo_osduhs_race <- relevel(osduhs$demo_osduhs_race, ref = 'White')

osduhs$ed_osduhs_avg_marks <- as.factor(osduhs$ed_osduhs_avg_marks)
osduhs$ed_osduhs_avg_marks <- relevel(osduhs$ed_osduhs_avg_marks, ref = 'below 70%')

# avg_marks, race, mental_health, close_with_people_at_school, mother_edu, father_edu, mother_type_at_home, father_type_at_home
# run model
temp <- custom_logit_mod(osduhs, 
                 model_vars = c('demo_osduhs_sex', 'demo_osduhs_grade','demo_osduhs_race','ed_osduhs_avg_marks',
                                'demo_osduhs_father_type_at_home','demo_osduhs_mother_edu','demo_osduhs_pop_weight'),
                 outcome_var = 'used_cannabis_ever', 
                 weights = FALSE, 
                 mod_type = 'binomial')

custom_logit_mod(osduhs, 
                 model_vars = c('demo_osduhs_father_type_at_home','demo_osduhs_pop_weight'),
                 outcome_var = 'used_cannabis_ever', 
                 weights = FALSE, 
                 mod_type = 'binomial')


custom_logit_mod(osduhs, 
                 model_vars = c('ed_osduhs_avg_marks','demo_osduhs_pop_weight'),
                 outcome_var = 'used_cannabis_ever', 
                 weights = FALSE, 
                 mod_type = 'binomial')

custom_logit_mod(osduhs, 
                 model_vars = c('ds_osduhs_close_with_people_at_school', 'demo_osduhs_pop_weight'),
                 outcome_var = 'used_cannabis_ever', 
                 weights = FALSE, 
                 mod_type = 'binomial')

temp <- custom_logit_mod(osduhs, 
                 model_vars = c('demo_osduhs_father_type_at_home', 'demo_osduhs_race',
                                'ed_osduhs_avg_marks', 'demo_osduhs_pop_weight'),
                 outcome_var = 'used_cannabis_ever', 
                 weights = FALSE, 
                 mod_type = 'binomial')







