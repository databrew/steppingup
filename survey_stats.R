##### Script for ad hoc survey analsis
# each model should only have a few predictors and done sequentially
# I'll choose interesting variables. Later will add in programmatic way (fetching relationships with a regularization model)
library(tidyverse)
library(glmnet)
library(reshape2)


# source functions
source('functions.R')

# Get all locations
if('preprocess_survey_data.RData' %in% dir('data')){
  load('data/preprocess_survey_data.RData')
} else { 
  source('global.R')
}

# get folder names
path_to_data <-  'data/survey_data'
survey_folders <- list.files(path_to_data)
survey_folders

# read in var summary 
var_summary <- read_csv('data/survey_data/var_summary.csv')



######################################################################################################################################
# Physical activity 
# hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency" 

# get gss10 
gss12 <- survey[[4]]
summary(as.factor(gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency))
gss12 <- restructure_data_types(gss12, convert_from = 'factor')
gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency <- as.numeric(ifelse(grepl('Not|Don', 
                                                                                                    gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency),
                                                                                              NA, 
                                                                                              ifelse(grepl('Never', gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency,
                                                                                                           gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency), 
                                                                                                     '0', gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency)))

# scatter plot men vs women freq physical activity 
gss12 <- gss12[gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency < 30,]
ggplot(gss12, aes(hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency, fill = demo_gss12_sex)) +
  geom_histogram(alpha = 0.7, bins = 30, position = 'identity') +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('# time participated in moderate to vigorous physical activity in past week') +
  ylab('Frequency') +
  ggtitle('Physical activity by gender') +
  theme_databrew()
  
  
gss12$phys_recode <- ifelse(gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency > 7, 'More than 7', 
                            ifelse(is.na(gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency), NA, 
                                   gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency))
# group by hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency and summarise gender
temp_sex_group <- gss12 %>% 
  filter(!is.na(demo_gss12_sex)) %>%
  group_by(demo_gss12_sex) %>%
  summarise(counts = n(),
            '1' = round((sum(phys_recode == '1', na.rm = T)/counts*100), 2),
            '2' = round((sum(phys_recode == '2', na.rm = T)/counts*100), 2),
            '3' = round((sum(phys_recode == '3', na.rm = T)/counts*100), 2),
            '4' = round((sum(phys_recode == '4', na.rm = T)/counts*100), 2),
            '5' = round((sum(phys_recode == '5', na.rm = T)/counts*100), 2),
            '6' = round((sum(phys_recode == '6', na.rm = T)/counts*100), 2),
            '7' = round((sum(phys_recode == '7', na.rm = T)/counts*100), 2),
            'More than 7' = round((sum(phys_recode == 'More than 7', na.rm = T)/counts*100), 2))

temp_sex_group$counts <- NULL
temp_sex_melt <- melt(temp_sex_group, id.vars = 'demo_gss12_sex')

# bar plot 
ggplot(temp_sex_melt, aes(variable, value, fill = demo_gss12_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('# time participated in moderate to vigorous physical activity in past week') +
  ylab('Percent') +
  ggtitle('Physical activity by gender (%)') +
  theme_databrew()


######################################################################################################################
# self percieved physical health

gss10 <- survey[[2]]
gss11 <- survey[[3]]
gss12 <- survey[[4]]
gss13 <- survey[[6]]
gss14 <- as.data.frame(survey[[9]])

summary(as.factor(gss10$hw_gss10_health))
summary(as.factor(gss11$hw_gss11_self_reported_health))
summary(as.factor(gss12$hw_gss12_self_reported_health))
summary(as.factor(gss13$hw_gss13_general_health))
summary(as.factor(gss14$hw_gss14_selfrated_general_health))


######## gss14
temp_data <- gss14 %>%
  filter(!is.na(demo_gss14_sex)) %>%
  filter(!grepl("Don't|Not", demo_gss14_sex)) %>%
  group_by(demo_gss14_sex) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss14_selfrated_general_health == 'Excellent', na.rm = T)/counts*100), 2),
            very_good = round((sum(hw_gss14_selfrated_general_health == 'Very good', na.rm = T)/counts*100), 2),
            good = round((sum(hw_gss14_selfrated_general_health == 'Good', na.rm = T)/counts*100), 2),
            fair = round((sum(hw_gss14_selfrated_general_health == 'Fair', na.rm = T)/counts*100), 2),
            poor = round((sum(hw_gss14_selfrated_general_health == 'Poor', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = 'demo_gss14_sex')

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_gss14_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('Physical health') +
  ylab('Percent') +
  ggtitle('Self rated physical health by gender 2014') +
  scale_x_discrete(labels=c("excellent" = "Excellent", "very_good" = "Very good", "good" = "Good",
                            "fair" = "Fair", "poor" = "Poor")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()

######## gss13
temp_data <- gss13 %>%
  filter(!is.na(demo_gss13_sex)) %>%
  filter(!grepl("Don't|Not", demo_gss13_sex)) %>%
  group_by(demo_gss13_sex) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss13_self_rated_general_health == 'Excellent', na.rm = T)/counts*100), 2),
            very_good = round((sum(hw_gss13_self_rated_general_health == 'Very good', na.rm = T)/counts*100), 2),
            good = round((sum(hw_gss13_self_rated_general_health == 'Good', na.rm = T)/counts*100), 2),
            fair = round((sum(hw_gss13_self_rated_general_health == 'Fair', na.rm = T)/counts*100), 2),
            poor = round((sum(hw_gss13_self_rated_general_health == 'Poor', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = 'demo_gss13_sex')

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_gss13_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('Physical health') +
  ylab('Percent') +
  ggtitle('Self rate physical health by gender 2013') +
  scale_x_discrete(labels=c("excellent" = "Excellent", "very_good" = "Very good", "good" = "Good",
                            "fair" = "Fair", "poor" = "Poor")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()

######## gss12
temp_data <- gss12 %>%
  filter(!is.na(demo_gss12_sex)) %>%
  filter(!grepl("Don't|Not", demo_gss12_sex)) %>%
  group_by(demo_gss12_sex) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss12_self_reported_health == 'Excellent', na.rm = T)/counts*100), 2),
            very_good = round((sum(hw_gss12_self_reported_health == 'Very good', na.rm = T)/counts*100), 2),
            good = round((sum(hw_gss12_self_reported_health == 'Good', na.rm = T)/counts*100), 2),
            fair = round((sum(hw_gss12_self_reported_health == 'Fair', na.rm = T)/counts*100), 2),
            poor = round((sum(hw_gss12_self_reported_health == 'Poor', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = 'demo_gss12_sex')

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_gss12_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('Physical health') +
  ylab('Percent') +
  ggtitle('Self rate physical health by gender 2012') +
  scale_x_discrete(labels=c("excellent" = "Excellent", "very_good" = "Very good", "good" = "Good",
                            "fair" = "Fair", "poor" = "Poor")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()

######## gss11
temp_data <- gss11 %>%
  filter(!is.na(demo_gss11_sex)) %>%
  filter(!grepl("Don't|Not", demo_gss11_sex)) %>%
  group_by(demo_gss11_sex) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss11_self_reported_health == 'Excellent', na.rm = T)/counts*100), 2),
            very_good = round((sum(hw_gss11_self_reported_health == 'Very good', na.rm = T)/counts*100), 2),
            good = round((sum(hw_gss11_self_reported_health == 'Good', na.rm = T)/counts*100), 2),
            fair = round((sum(hw_gss11_self_reported_health == 'Fair', na.rm = T)/counts*100), 2),
            poor = round((sum(hw_gss11_self_reported_health == 'Poor', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = 'demo_gss11_sex')

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_gss11_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('Physical health') +
  ylab('Percent') +
  ggtitle('Self rate physical health by gender 2011') +
  scale_x_discrete(labels=c("excellent" = "Excellent", "very_good" = "Very good", "good" = "Good",
                            "fair" = "Fair", "poor" = "Poor")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()


######## gss10
temp_data <- gss10 %>%
  filter(!is.na(demo_gss10_sex)) %>%
  filter(!grepl("Don't", demo_gss10_sex)) %>%
  group_by(demo_gss10_sex) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss10_health == 'excellent', na.rm = T)/counts*100), 2),
            very_good = round((sum(hw_gss10_health == 'very good', na.rm = T)/counts*100), 2),
            good = round((sum(hw_gss10_health == 'good', na.rm = T)/counts*100), 2),
            fair = round((sum(hw_gss10_health == 'fair', na.rm = T)/counts*100), 2),
            poor = round((sum(hw_gss10_health == 'poor', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = 'demo_gss10_sex')

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_gss10_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('Physical health') +
  ylab('Percent') +
  ggtitle('Self rate physical health by gender 2010 ') +
  scale_x_discrete(labels=c("excellent" = "Excellent", "very_good" = "Very good", "good" = "Good",
                            "fair" = "Fair", "poor" = "Poor")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()

  
#########################################################################################################################################
# self percieved mental health

######## gss13
temp_data <- gss13 %>%
  filter(!is.na(demo_gss13_sex)) %>%
  filter(!grepl("Don't|Not", demo_gss13_sex)) %>%
  group_by(demo_gss13_sex) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss13_self_rated_mental_health == 'Excellent', na.rm = T)/counts*100), 2),
            very_good = round((sum(hw_gss13_self_rated_mental_health == 'Very good', na.rm = T)/counts*100), 2),
            good = round((sum(hw_gss13_self_rated_mental_health == 'Good', na.rm = T)/counts*100), 2),
            fair = round((sum(hw_gss13_self_rated_mental_health == 'Fair', na.rm = T)/counts*100), 2),
            poor = round((sum(hw_gss13_self_rated_mental_health== 'Poor', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = 'demo_gss13_sex')

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_gss13_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('Mental health') +
  ylab('Percent') +
  ggtitle('Self rated mental health by gender 2013') +
  scale_x_discrete(labels=c("excellent" = "Excellent", "very_good" = "Very good", "good" = "Good",
                            "fair" = "Fair", "poor" = "Poor")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()


######## gss12
temp_data <- gss12 %>%
  filter(!is.na(demo_gss12_sex)) %>%
  filter(!grepl("Don't|Not", demo_gss12_sex)) %>%
  group_by(demo_gss12_sex) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss12_self_reported_mental_health == 'Excellent', na.rm = T)/counts*100), 2),
            very_good = round((sum(hw_gss12_self_reported_mental_health == 'Very good', na.rm = T)/counts*100), 2),
            good = round((sum(hw_gss12_self_reported_mental_health == 'Good', na.rm = T)/counts*100), 2),
            fair = round((sum(hw_gss12_self_reported_mental_health == 'Fair', na.rm = T)/counts*100), 2),
            poor = round((sum(hw_gss12_self_reported_mental_health == 'Poor', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = 'demo_gss12_sex')

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_gss12_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('Mental health') +
  ylab('Percent') +
  ggtitle('Self rated mental health by gender 2012') +
  scale_x_discrete(labels=c("excellent" = "Excellent", "very_good" = "Very good", "good" = "Good",
                            "fair" = "Fair", "poor" = "Poor")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()

######## gss11
temp_data <- gss11 %>%
  filter(!is.na(demo_gss11_sex)) %>%
  filter(!grepl("Don't|Not", demo_gss11_sex)) %>%
  group_by(demo_gss11_sex) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss11_self_reported_mental_health == 'Excellent', na.rm = T)/counts*100), 2),
            very_good = round((sum(hw_gss11_self_reported_mental_health == 'Very good', na.rm = T)/counts*100), 2),
            good = round((sum(hw_gss11_self_reported_mental_health == 'Good', na.rm = T)/counts*100), 2),
            fair = round((sum(hw_gss11_self_reported_mental_health == 'Fair', na.rm = T)/counts*100), 2),
            poor = round((sum(hw_gss11_self_reported_mental_health == 'Poor', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = 'demo_gss11_sex')

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_gss11_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('Mental health') +
  ylab('Percent') +
  ggtitle('Self rated mental health by gender 2011') +
  scale_x_discrete(labels=c("excellent" = "Excellent", "very_good" = "Very good", "good" = "Good",
                            "fair" = "Fair", "poor" = "Poor")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()


####### gss10
temp_data <- gss10 %>%
  filter(!is.na(demo_gss10_sex)) %>%
  filter(!grepl("Don't", demo_gss10_sex)) %>%
  group_by(demo_gss10_sex) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss10_mental_health == 'excellent', na.rm = T)/counts*100), 2),
            very_good = round((sum(hw_gss10_mental_health == 'very good', na.rm = T)/counts*100), 2),
            good = round((sum(hw_gss10_mental_health == 'good', na.rm = T)/counts*100), 2),
            fair = round((sum(hw_gss10_mental_health == 'fair', na.rm = T)/counts*100), 2),
            poor = round((sum(hw_gss10_mental_health == 'poor', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = 'demo_gss10_sex')

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_gss10_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", labels = c('Female', 'Male'), values=c('seagreen', 'lightblue')) +
  xlab('Mental health') +
  ylab('Percent') +
  ggtitle('Self rated mental health by gender 2010 ') +
  scale_x_discrete(labels=c("excellent" = "Excellent", "very_good" = "Very good", "good" = "Good",
                            "fair" = "Fair", "poor" = "Poor")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()


###### osduhs 15 
osduhs <- survey[[10]]

temp_data <- osduhs %>%
  filter(!is.na(demo_osduhs_race)) %>%
  filter(!grepl('not sure', demo_osduhs_race)) %>%
  group_by(demo_osduhs_race) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_osduhs_mental_health == 'excellent', na.rm = T)/counts*100), 2),
            very_good = round((sum(hw_osduhs_mental_health == 'very good', na.rm = T)/counts*100), 2),
            good = round((sum(hw_osduhs_mental_health == 'good', na.rm = T)/counts*100), 2),
            fair = round((sum(hw_osduhs_mental_health == 'fair', na.rm = T)/counts*100), 2),
            poor = round((sum(hw_osduhs_mental_health == 'poor', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = 'demo_osduhs_race')

# bar plot 
cols <- brewer.pal(n = 10, name ='RdBu')
ggplot(temp_melt, aes(variable, value, fill = demo_osduhs_race)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", 
                    breaks = c('aboriginal', 'black', 'chite', 'filipino', 'japanese', 'Korean', 'south asian', 'southeast asian', 'west asian / arab', 'whinese'), 
                    labels = c('Aoriginal', 'Black', 'White', 'Filipino', 'Japanese', 'Korean', 'South asian', 'Southeast asian','West asian / Arab', 'Chinese'),
                    values=cols) +
  xlab('Mental health') +
  ylab('Percent') +
  ggtitle('Self rated mental health by gender 2010 ') +
  scale_x_discrete(labels=c("excellent" = "Excellent", "very_good" = "Very good", "good" = "Good",
                            "fair" = "Fair", "poor" = "Poor")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  theme_databrew()

######################################################################################################################################
# caring for others 
# cc_gss13_favour_for_neighbour_past_month
summary(as.factor(gss13$cc_gss13_favour_for_neighbour_past_month))
summary(as.factor(gss12$cc_gss12_done_favour_for_neighbour_past_month))

######## gss13
temp_data <- gss13 %>%
  filter(!is.na(demo_gss13_sex)) %>%
  filter(!grepl("Don't|Not|Ref|Valid", demo_gss13_sex)) %>%
  group_by(demo_gss13_sex, demo_gss13_age_group) %>%
  summarise(counts = n(),
            Yes = round((sum(cc_gss13_favour_for_neighbour_past_month == 'Yes', na.rm = T)/counts*100), 2))


# bar plot 
ggplot(temp_data, aes(demo_gss13_sex, Yes, fill = demo_gss13_age_group)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) + 
  scale_fill_manual(name="Age Group", values=c('seagreen', 'lightblue')) +
  xlab('Gender') +
  ylab('Percent') +
  ggtitle('Favour for neighbor in past month gss13') +
  geom_text(aes(label=Yes), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()

######## gss12
temp_data <- gss12 %>%
  filter(!is.na(demo_gss12_sex)) %>%
  filter(!grepl("Don't|Not|Ref|Valid", demo_gss12_sex)) %>%
  group_by(demo_gss12_sex, demo_gss12_age_group) %>%
  summarise(counts = n(),
            Yes = round((sum(cc_gss12_done_favour_for_neighbour_past_month == 'Yes', na.rm = T)/counts*100), 2))


# bar plot 
cols <- brewer.pal(n = 4, name ='RdBu')

ggplot(temp_data, aes(demo_gss12_sex, Yes, fill = demo_gss12_age_group)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) + 
  scale_fill_manual(name="Age Group", values=cols)  +
  xlab('Gender') +
  ylab('Percent') +
  ggtitle('Favour for neighbor in past month gss12') +
  geom_text(aes(label=Yes), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()


# em_gss13_main_activity_last_12_months

######## gss13
temp_data <- gss13 %>%
  filter(!is.na(demo_gss13_sex)) %>%
  filter(!grepl("Don't|Not|Ref|Valid", demo_gss13_sex)) %>%
  group_by(demo_gss13_sex, demo_gss13_age_group) %>%
  summarise(counts = n(),
            Yes = round((sum(em_gss13_main_activity_last_12_months == 'Caring for children', na.rm = T)/counts*100), 2))


# bar plot 
ggplot(temp_data, aes(demo_gss13_sex, Yes, fill = demo_gss13_age_group)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) + 
  scale_fill_manual(name="Age Group", values=c('seagreen', 'lightblue')) +
  xlab('Gender') +
  ylab('Percent') +
  ggtitle('Last 12 months main activity caring for children gss13') +
  geom_text(aes(label=Yes), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()

######## gss12
colnames(gss12)[grepl('activity', colnames(gss12))]
summary(as.factor(gss12$demo_gss12_main_activity))
temp_data <- gss12 %>%
  filter(!is.na(demo_gss12_sex)) %>%
  filter(!grepl("Don't|Not|Ref|Valid", demo_gss12_sex)) %>%
  group_by(demo_gss12_sex, demo_gss12_age_group) %>%
  summarise(counts = n(),
            Yes = round((sum(demo_gss12_main_activity == 'Caring for children', na.rm = T)/counts*100), 2))


# bar plot 
cols <- brewer.pal(n = 4, name ='RdBu')

ggplot(temp_data, aes(demo_gss12_sex, Yes, fill = demo_gss12_age_group)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) + 
  scale_fill_manual(name="Age Group", values=cols)  +
  xlab('Gender') +
  ylab('Percent') +
  ggtitle('Last 12 months main activity caring for children gss12') +
  geom_text(aes(label=Yes), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()


######################################################################################################################################
# child services 
# sf_gss14_child_victim_report_child_services
# hw_gss14_child_victim_sexual_assault_no_report
summary(as.factor(gss14$sf_gss14_child_victim_report_child_services))
# summary(as.factor(gss14$hw_gss14_child_victim_sexual_assault_no_report))

######## gss14
temp_data <- gss14 %>%
  filter(!is.na(demo_gss14_sex)) %>%
  filter(!grepl("Don't|Not|Ref|Valid", demo_gss14_sex)) %>%
  group_by(demo_gss14_sex, demo_gss14_age_group) %>%
  summarise(counts = n(),
            Yes = round((sum(sf_gss14_child_victim_report_child_services == 'Yes', na.rm = T)/counts*100), 2))


# bar plot 
ggplot(temp_data, aes(demo_gss14_sex, Yes, fill = demo_gss14_age_group)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) + 
  scale_fill_manual(name="Age Group", values=c('seagreen', 'lightblue')) +
  xlab('Gender') +
  ylab('Percent reported') +
  ggtitle('Reporting childhood victimization to child services') +
  geom_text(aes(label=Yes), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()



######################################################################################################################################
# homelessness 
lapply(survey, function(x) colnames(x)[grepl('main', colnames(x))])

# sf_gss14_homeless_ever_been_homeless
# sf_gss14_homeless_longest_period_of_time_for_which_you_have_been_homeless
# sf_gss14_homeless_had_to_temporarily_live_with_familyfriendscaretc    
# sf_gss14_homeless_longest_period_living_with_familyfriendscaretc

# homeless by sex and age 

######## gss14
temp_data <- gss14 %>%
  filter(!is.na(demo_gss14_sex)) %>%
  filter(!grepl("Don't|Not", demo_gss14_sex)) %>%
  group_by(demo_gss14_sex, demo_gss14_age_group) %>%
  summarise(counts = n(),
            Yes = round((sum(sf_gss14_homeless_ever_been_homeless == 'Yes', na.rm = T)/counts*100), 2))


# bar plot 
ggplot(temp_data, aes(demo_gss14_sex, Yes, fill = demo_gss14_age_group)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) + 
  scale_fill_manual(name="Age Group", values=c('seagreen', 'lightblue')) +
  xlab('Gender') +
  ylab('Percent ever been homeless') +
  ggtitle('Homelessness GSS14') +
  geom_text(aes(label=Yes), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_databrew()





######################################################################################################################################
# financial knowledge
# demo_cfc_rate_self_on_level_of_financ_knowledge
# demo_cfc_financially_preparing_for_retirement"
cfc <- survey[[7]]
lapply(survey, function(x) colnames(x)[grepl('bank', colnames(x))])

summary(as.factor(cfc$demo_cfc_rate_self_on_level_of_financ_knowledge))
summary(as.factor(cfc$demo_cfc_aboriginal_status))
summary(as.factor(cfc$demo_cfc_age_of_respondent_grouped))


# demo_cfc_rate_self_on_level_of_financ_knowledge

temp_data <- cfc %>%
  filter(demo_cfc_age_of_respondent_grouped == '18 to 24') %>%
  group_by(demo_cfc_sex) %>%
  summarise(counts = n(),
            very_knowledgeable = round((sum(demo_cfc_rate_self_on_level_of_financ_knowledge == 'Very knowledgeable', na.rm = T)/counts*100), 2),
            knowledgeable = round((sum(demo_cfc_rate_self_on_level_of_financ_knowledge == 'Knowledgeable', na.rm = T)/counts*100), 2),
            fairly_knowledgeable = round((sum(demo_cfc_rate_self_on_level_of_financ_knowledge== 'Fairly knowledgeable', na.rm = T)/counts*100), 2),
            not_very_knowledgeable = round((sum(demo_cfc_rate_self_on_level_of_financ_knowledge == 'Not very knowledgeab', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = c('demo_cfc_sex'))

# bar plot 
cols <- brewer.pal(n = 10, name ='RdBu')
ggplot(temp_melt, aes(variable, value, fill = demo_cfc_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", 
                    values=c("#67001F", "#D1E5F0")) +
  xlab('Financial knowledge') +
  ylab('Percent') +
  ggtitle('Self rated knowledge of finance for ages 18-24 (CFCS 2014)') +
  scale_x_discrete(labels=c("very_knowledgeable" = "Very knowledgeable", 
                            "knowledgeable" = "Knowledgeable", 
                            "fairly_knowledgeable" = "Fairly knowledgeable",
                            "not_very_knowledgeable" = "Not very knowledgeable")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  theme_databrew()
######

temp_data <- cfc %>%
  filter(demo_cfc_age_of_respondent_grouped == '18 to 24') %>%
  group_by(demo_cfc_aboriginal_status) %>%
  summarise(counts = n(),
            very_knowledgeable = round((sum(demo_cfc_rate_self_on_level_of_financ_knowledge == 'Very knowledgeable', na.rm = T)/counts*100), 2),
            knowledgeable = round((sum(demo_cfc_rate_self_on_level_of_financ_knowledge == 'Knowledgeable', na.rm = T)/counts*100), 2),
            fairly_knowledgeable = round((sum(demo_cfc_rate_self_on_level_of_financ_knowledge== 'Fairly knowledgeable', na.rm = T)/counts*100), 2),
            not_very_knowedgeable = round((sum(demo_cfc_rate_self_on_level_of_financ_knowledge == 'Not very knowledgeab', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = c('demo_cfc_aboriginal_status'))

# bar plot 
cols <- brewer.pal(n = 10, name ='RdBu')
ggplot(temp_melt, aes(variable, value, fill = demo_cfc_aboriginal_status)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Aboriginal status", 
                    values=c("#67001F", "#FDDBC7",  "#D1E5F0")) +
  xlab('Financial knowledge') +
  ylab('Percent') +
  ggtitle('Self rate knowledge of finance (CFCS 2014)') +
  scale_x_discrete(labels=c("very_knowledgeable" = "Very knowledgeable",
                            "knowledgeable" = "Knowledgeable", 
                            "fairly_knowledgeable" = "Fairly knowledgeable",
                            "not_very_knowledgeable" = "Not very knowledgeable")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  theme_databrew()


####################################################################################################################
# demo_cfc_number_total_bank_accounts
summary(as.factor(cfc$demo_cfc_number_total_bank_accounts))


temp_data <- cfc %>%
  filter(demo_cfc_age_of_respondent_grouped == '18 to 24') %>%
  group_by(demo_cfc_sex) %>%
  summarise(counts = n(),
            one = round((sum(demo_cfc_number_total_bank_accounts == '1 personal and joint account', na.rm = T)/counts*100), 2),
            two = round((sum(demo_cfc_number_total_bank_accounts == '2 personal and joint accounts', na.rm = T)/counts*100), 2),
            three = round((sum(demo_cfc_number_total_bank_accounts == '3 personal and joint accounts', na.rm = T)/counts*100), 2),
            four = round((sum(demo_cfc_number_total_bank_accounts == '4 personal and joint accounts', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = c('demo_cfc_sex'))

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_cfc_sex)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Sex", 
                    values=c("#67001F", "#FDDBC7")) +
  xlab('Total bank accounts') +
  ylab('Percent') +
  ggtitle('Total joint and personal bank accounts (CFCS 2014)') +
  scale_x_discrete(labels=c("one" = "One",
                            "two" = "Two", 
                            "three" = "Three",
                            "four" = "Four")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  theme_databrew()



temp_data <- cfc %>%
  filter(demo_cfc_age_of_respondent_grouped == '18 to 24') %>%
  group_by(demo_cfc_aboriginal_status) %>%
  summarise(counts = n(),
            one = round((sum(demo_cfc_number_total_bank_accounts == '1 personal and joint account', na.rm = T)/counts*100), 2),
            two = round((sum(demo_cfc_number_total_bank_accounts == '2 personal and joint accounts', na.rm = T)/counts*100), 2),
            three = round((sum(demo_cfc_number_total_bank_accounts == '3 personal and joint accounts', na.rm = T)/counts*100), 2),
            four = round((sum(demo_cfc_number_total_bank_accounts == '4 personal and joint accounts', na.rm = T)/counts*100), 2))

temp_data$counts <- NULL
temp_melt <- melt(temp_data, id.vars = c('demo_cfc_aboriginal_status'))

# bar plot 
ggplot(temp_melt, aes(variable, value, fill = demo_cfc_aboriginal_status)) + 
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
  scale_fill_manual(name="Aboriginal status", 
                    values=c("#67001F", "#FDDBC7", "black")) +
  xlab('Total bank accounts') +
  ylab('Percent') +
  ggtitle('Total joint and personal bank accounts (CFCS 2014)') +
  scale_x_discrete(labels=c("one" = "One",
                            "two" = "Two", 
                            "three" = "Three",
                            "four" = "Four")) +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25, size = 3) +
  theme_databrew()

# # function that takes an outcome (any variable by demo) and regresses on all demo variables
# # default estimates a lasso
# get_glm <- function(model_data = dat, 
#                     model_type, 
#                     weights = NULL) {
#   
#   # get list to store dta
#   result_list <- list()
#   
#   # remove NAS
#   model_data <- as.data.frame(model_data[complete.cases(model_data),])
#   
#   # get weight vector
#   weight_index <- grepl('demo_osduhs_pop_weight', colnames(model_data))
#   weight_vector <-model_data[, weight_index]
#   
#   if(!is.null(weights)) {
#     weights <- weight_vector
#   }
#   # remove weight from data 
#   model_data <- model_data[, !grepl('demo_osduhs_pop_weight', colnames(model_data))]
#   
#   # for the time being remove any factors that have more than 10 levels (ill clean them later, just rushed to      get this to xing)
#   model_data <- model_data[, apply(model_data, 2, function(x) length(unique(x)) < 12)]
#   
#   # get outcome and feature data (for time being anything that is demo is feature)
#   outcome_data <- model_data[, !grepl('^demo', colnames(model_data))]
#   
#   # add in head_injury, school_days_breakfast, , energy_drinks, soft_drinks, tv_video_games, phy_activity_60
#   # get_along (includeds father and mother), avg_marks, close_with_people_at_school, safe_at_school. 
#   predictor_variables <- '^demo|head_injury|school_days_breakfast|energy_drinks|soft_drinks|tv_video_games|phy_activity_60|get_along|avg_marks|close_with_people_at_school|safe_at_school'
#   demo_data <- model_data[, grepl(predictor_variables, colnames(model_data))]
#   
#   # only keep outcome variable that have two levels
#   outcome_data <- outcome_data[, apply(outcome_data, 2, function(x) length(unique(x)) < 3)]
#   
#   # loop through outcome_dat and bind each outcome variable with feature dat
#   for(i in 1:ncol(outcome_data)){
#     
#     # get outcome variable and the name (to store later)
#     y_outcome <- outcome_data[,i]
#     y_outcome_name <- names(outcome_data)[i]
#     
#     # get target class (always second in alphabetical order so you can store this as well)
#     target_class <- sort(unique(y_outcome))[2]
#     
#     # set the alpha parameter based on model type
#     if(model_type == 'lasso') {
#       alpha_val <- 1
#     } 
#     if(model_type =='ridge'){
#       alpha_val <- 0
#     }
#     
#     # do ridge or lasso
#     if(grepl('lasso|ridge', model_type)){
#       
#       if(!is.null(weights)) {
#         weights <- weight_vector
#       } else {
#         weights <- rep.int(1, nrow(demo_data))
#       }
#       
#       # runs model with 5 fold cv and we keep the lambda with minimum cv error
#       cv_model = cv.glmnet(model.matrix(~.,demo_data), 
#                            nfolds = 5,
#                            y_outcome,
#                            alpha = alpha_val, 
#                            family = 'binomial',
#                            weights = weights,
#                            parallel = TRUE)
#       
#       lambda_index = which(cv_model$lambda == cv_model$lambda.min) 
#       
#       # creat result table with odds ratios tha correspond to the best lamnd (minimum)
#       model_result <- data.frame(outcome_name = y_outcome_name, target_class = target_class, coef.name = dimnames(coef(cv_model))[[1]], coef.value = matrix(exp(coef(cv_model, s = "lambda.min"))))
#       
#     } else {
#       
#       # do normal logit with no regularization
#       temp_mod_data <- as.data.frame(cbind(y_outcome = y_outcome, demo_data))
#       
#       # loop though colnames (without demo) and estimate logit
#       model_result <- glm(y_outcome~. , family = 'binomial', weights = weights, data = temp_mod_data)
#       model_result <- cbind(tidy(model_result), odds_ratio = exp(model_result$coefficients), target_class = target_class)
#       model_result$odds_ratio.names <- NULL
#       model_result$outcome_var <- y_outcome_name
#       model_result$sig <- ifelse(model_result$p.value < 0.05, 'significant', 'not_statistically_significant')
#       
#     }
#     
#     result_list[[i]] <- model_result
#     print(i)
#   }
#   
#   logit_results <- do.call(rbind, result_list)
#   
#   return(logit_results)
# }
# 
# # # get all variable levels for xing so she can interpret
# # predictor_variables <- '^demo|head_injury|school_days_breakfast|energy_drinks|soft_drinks|tv_video_games|phy_activity_60|get_along|avg_marks|close_with_people_at_school|safe_at_school'
# # temp_demo <- dat[, grepl(predictor_variables, colnames(dat))]
# # temp_demo <- temp_demo[, apply(temp_demo, 2, function(x) length(unique(x)) < 15)]
# # temp_demo <- apply(temp_demo, 2, function(x) unique(x))
# # temp_demo <- as.data.frame(do.call(rbind, temp_demo))
# # temp_demo$var <- rownames(temp_demo)
# # write_csv(temp_demo, '~/Desktop/temp_demo.csv')
# 
# # logistic regression
# logit_results <- get_glm(model_data = dat, model_type = 'no_regularization', weights = NULL)
# write_csv(logit_results, '~/Desktop/logit_results.csv')
# 
# 
# # maybe dont use these
# lasso_results <- get_glm(model_data = dat, model_type = 'lasso', weights = NULL)
# write_csv(lasso_results, '~/Desktop/lasso_results.csv')
# ridge_results <- get_glm(model_data = dat, model_type = 'ridge', weights = NULL)
# write_csv(ridge_results, '~/Desktop/ridge_results.csv')
