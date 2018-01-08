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
library(ggthemes)

# Turn off scientific notation
options(scipen = '999')

# Basic knitr options
knitr::opts_chunk$set(comment = NA, 
                      echo = FALSE, 
                      warning = FALSE, 
                      message = FALSE, 
                      error = TRUE, # Render report, even with errors
                      cache = F)

# source functions
source('functions.R')

# Get all locations
if('preprocess_survey_data.RData' %in% dir('data')){
  load('data/preprocess_survey_data.RData')
} else { 
  source('global.R')
}


# General Social Survey 2010
# get lfs data from survey list 
osduhs <- survey[[10]]
gss <- survey[[2]]

# make all characters
gss <- restructure_data_types(gss, convert_from  = 'character')
osduhs <- restructure_data_types(osduhs, convert_from  = 'character')


#############
# gss
#############

# Do a summary of the variables you want to look at so you now what levels to keep and throw awa
summary(gss$demo_gss10_aboriginal_status)
summary(gss$hw_gss10_mental_health)

# This one does the percent (the one below is raw numbers)
# Line by line:
# filter out levels that have "Not" or "Don't" in them
# filter NA out of that column as well
# group by the variable of interest
# the first line in summarise (counts = n()) gets total counts for each level (not for the levels we already removed thought)
# dividing by the counts, times 100 and then rounding to get exact percentages 
group_abor <- gss %>%
  filter(!grepl("Not|Don't", demo_gss10_aboriginal_status)) %>%
  filter(!is.na(demo_gss10_aboriginal_status)) %>%
  group_by(demo_gss10_aboriginal_status) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss10_mental_health == 'excellent')/counts)*100, 2),
            fair = round((sum(hw_gss10_mental_health == 'fair')/counts)*100, 2),
            poor = round((sum(hw_gss10_mental_health == 'poor')/counts)*100, 2),
            good  = round((sum(hw_gss10_mental_health == 'good')/counts)*100, 2),
            very_good  = round((sum(hw_gss10_mental_health == 'very good')/counts)*100, 2))


# # Same as above, but just preserves the raw numbers
# group_abor <- gss %>%
#   filter(!grepl("Not|Don't", demo_gss10_aboriginal_status)) %>%
#   filter(!is.na(demo_gss10_aboriginal_status)) %>%
#   group_by(demo_gss10_aboriginal_status) %>%
#   summarise(counts = n(),
#             excellent = sum(hw_gss10_mental_health == 'excellent'),
#             fair = sum(hw_gss10_mental_health == 'fair'),
#             poor = sum(hw_gss10_mental_health == 'poor'),
#             good  = sum(hw_gss10_mental_health == 'good'),
#             very_good  = sum(hw_gss10_mental_health == 'very good'))

# melt data set for plotting
# remove counts 
group_abor$counts <- NULL
abor_melt <- melt(group_abor, id.vars = 'demo_gss10_aboriginal_status')

# bar plot- if you want to stack
abor_melt$variable <- factor(abor_melt$variable, levels = c("excellent", "very_good", 
                             "good", "fair", "poor"))

ggplot(abor_melt, aes(demo_gss10_aboriginal_status, value, group = variable, fill = variable)) +
  geom_bar(stat= 'identity', position = 'dodge', alpha = 0.7) +
  xlab('Aboriginal status') +
  ylab('Percent') + # or counts
  scale_fill_manual(name = 'Mental health',
                    breaks = c('excellent', 'very_good', 'good', 'fair', 'poor'),
                    labels = c('Excellent', 'Very Good', 'Good', 'Fair', 'Poor'),
                    values = c('#999999', '#E69F00', '#56B4E9', '#009E72', '#0072B2')) +
  ggtitle('Mental Health by Aboriginal Status') +
  theme_bw() +
  theme(text = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))


#### Health by visible minority group

group_vismin <- gss %>%
  filter(!grepl("Don't", demo_gss10_aboriginal_vismin)) %>%
  filter(!is.na(demo_gss10_aboriginal_vismin)) %>%
  group_by(demo_gss10_aboriginal_vismin) %>%
  summarise(counts = n(),
            excellent = round((sum(hw_gss10_health == 'excellent')/counts)*100, 2),
            fair = round((sum(hw_gss10_health == 'fair')/counts)*100, 2),
            poor = round((sum(hw_gss10_health == 'poor')/counts)*100, 2),
            good  = round((sum(hw_gss10_health == 'good')/counts)*100, 2),
            very_good  = round((sum(hw_gss10_health == 'very good')/counts)*100, 2))

group_vismin$counts <- NULL
vismin_melt <- melt(group_vismin, id.vars = 'demo_gss10_aboriginal_vismin')

# bar plot- if you want to stack
vismin_melt$variable <- factor(vismin_melt$variable, levels = c("excellent", "very_good", 
                                                            "good", "fair", "poor"))

ggplot(vismin_melt, aes(variable, value, group = demo_gss10_aboriginal_vismin, fill = demo_gss10_aboriginal_vismin)) +
  geom_bar(stat= 'identity', position = 'dodge', alpha = 0.7) +
  xlab('Visible Minority status') +
  ylab('Percent') + # or counts
  scale_fill_manual(name = 'Visible Minority Status',
                    breaks = c('Not a visible minority', 'Visible minority'),
                    labels = c('Not a visible minority', 'Visible minority'),
                    values = c('#009E72', '#0072B2')) +
  ggtitle('Self-reported Health by Visible Minority Status') +
  theme_bw() +
  theme(text = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))

#####################################################################

## Plot ed_gss10_highest_level_edu vs. personal income

summary(gss$ed_gss10_highest_level_edu)
summary(gss$demo_gss10_personal_income)

# 5-15, 15-30, 30-60, 60-100, 100+, none
gss$income_groups <- ifelse(grepl('5,|14,', gss$demo_gss10_personal_income),
                            'five_15', 
                            ifelse(grepl('15,|20,', gss$demo_gss10_personal_income),
                                   'fifteen_30',
                                   ifelse(grepl('30,|40,|50,', gss$demo_gss10_personal_income),
                                          'thirty_60',
                                          ifelse(grepl('60,|80,', gss$demo_gss10_personal_income),
                                                 'sixty_100',
                                                 ifelse(grepl('more', gss$demo_gss10_personal_income),
                                                              'hundo_plus', 
                                                              ifelse(grepl('No', gss$demo_gss10_personal_income),
                                                                     'no_income', 
                                                                     ifelse(grepl("Don't", gss$demo_gss10_personal_income),
                                                                            NA, gss$demo_gss10_personal_income)))))))

ed <- gss %>%
    filter(!grepl("Don't", ed_gss10_highest_level_edu)) %>%
    filter(!grepl("Don't", income_groups)) %>%
    filter(!is.na(ed_gss10_highest_level_edu)) %>%
    filter(!is.na(income_groups)) %>%
    group_by(ed_gss10_highest_level_edu) %>%
    summarise(counts = n(),
            none = round((sum(income_groups == 'no_income')/counts)*100, 2),
            five_14999 = round((sum(income_groups == 'five_15')/counts)*100, 2),
            fifteen_29999 = round((sum(income_groups == 'fifteen_30')/counts)*100, 2),
            thirty_59999 = round((sum(income_groups == 'thirty_60')/counts)*100, 2),
            sixty_99999 = round((sum(income_groups == 'sixty_100')/counts)*100, 2),
            hundred_plus = round((sum(income_groups == 'hundo_plus')/counts)*100, 2))
          

ed$counts <- NULL         
ed_melt <- melt(ed, id.vars = 'ed_gss10_highest_level_edu')

ed_melt$ed_gss10_highest_level_edu <- factor(ed_melt$ed_gss10_highest_level_edu, 
                                             levels = c('Some secondary/elementary or no school',
                                                        'High school diploma',
                                                        'Some university or community college',
                                                        'Dip/cert from trade/technical school or college',
                                                        "Doctorate/masters/bach's"))

ggplot (ed_melt, aes(ed_gss10_highest_level_edu, value, group = variable, fill = variable)) +
  geom_bar(stat = 'identity') +
  xlab('Highest Level of Education') +
  ylab('Percent') +
  ggtitle('Education Level vs. Personal Income') +
  theme_minimal() + 
  scale_fill_manual(name = 'Income Groups',
                    breaks = c('none','five_14999','fifteen_29999', 'thirty_59999','sixty_99999','hundred_plus'),
                    labels = c('No Income', '$5,000-$14,999', '$15,000-$29,999', '$30,000-$59,999', '$60,000-$99,999',
                               '$100,000 and more'),
                    values = c('#e0e0e0', '#999999', '#E69F00', '#56B4E9', '#009E72', '#0072B2')) 


#####################################################################

# give this function a real column name for x and y
# and then a renamed name for the x and y labs
correlate_numeric <- function(x_column, 
                              y_column,
                              x_lab,
                              y_lab,
                              cutoff_zero) {
  
  
  if(cutoff_zero) {
    gss <- gss[gss[, x_column ] > 0, ]
    gss <- gss[gss[, y_column ] > 0, ]
    
  }
  
  title <- paste0(x_lab, ' by ', y_lab)
  
  # plot 
  ggplot(gss, aes_string(x_column, y_column)) + 
    geom_point(size = 2, alpha = 0.5) +
    xlab(x_lab) +
    ylab(y_lab) +
    ggtitle(title) +
    theme_bw() +
    theme(text = element_text(size = 13),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13))
    
}

# this just gets you the names of the numeric columns for your own convenience
colnames(gss)[sapply(gss, is.numeric)]

# if you want to you opinion of life, its scaled 1-10, but has 99 and 98 which mean not responded or not asked
# so I'll remove those 
gss$hw_gss10_life <- ifelse(gss$hw_gss10_life > 10, NA, gss$hw_gss10_life)

# plots minutes alone vs minutes watching tv
correlate_numeric(x_column = 'hw_gss10_mins_tv',
                  y_column = 'sf_gss10_mins_at_someone_else_home',
                  x_lab = 'Minutes TV',
                  y_lab = 'Minutes other home',
                  cutoff_zero = FALSE)




