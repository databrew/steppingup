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

# get all data 
survey_folders
lfs <- survey[[1]]
gss10 <- survey[[2]]
gss11 <- survey[[3]]
gss12 <- survey[[4]]
gss13 <- survey[[6]]
gss14 <- survey[[9]]
cfc <- survey[[7]]
osduhs <- survey[[10]]


######################################################################################################################

group_by_plot_factor <- function(var_1, 
                                 var_2, 
                                 dat, 
                                 remove_string, 
                                 title) {
  dat <- dat[,c(var_1, var_2)]
  temp_data <- dat %>%
    filter_all(all_vars(!is.na(.))) %>%
    group_by_(var_1, var_2) %>%
    summarise(counts = n())
  
  # remove unneeded strings
  temp_data <- as.data.frame(temp_data)
  temp_data <- temp_data[!grepl(remove_string, as.character(temp_data[,var_2])),]
  
  # join them
  temp_totals <- as.data.frame(tapply(temp_data$counts,temp_data[, var_1] , FUN=sum))
  temp_totals$var_1 <- rownames(temp_totals)
  names(temp_totals) <- c('total', 'var_1')
  colnames(temp_data)[1] <- 'var_1'
  colnames(temp_data)[2] <- 'var_2'
  temp_new <- left_join(temp_data, temp_totals, by = 'var_1')
  
  # get percentage column
  temp_new$per <- round((temp_new$counts/temp_new$total)*100, 2)
  
  temp_new$counts <- NULL
  temp_new$total <- NULL
  
  # bar plot 
  levels_of_var_1 <- length(unique(temp_new$var_1))
  if(levels_of_var_1 == '2') {
    cols <- c("#67001F", "#D1E5F0")
  } else {
    cols <- brewer.pal(levels_of_var_1, name = 'RdBu')
  }
  ggplot(temp_new, aes(var_2, per, fill = var_1)) + 
    geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
    scale_fill_manual(name="", values = cols) +
    xlab('') +
    ylab('Percent') +
    ggtitle(title) +
    geom_text(aes(label=per), position=position_dodge(width=0.9), vjust=-0.25) +
    theme_databrew()
  
}



group_by_plot_factor_2 <- function(var_1, 
                                   var_2, 
                                   var_3,
                                   dat, 
                                   remove_string, 
                                   title) {
  
  dat <- dat[,c(var_1, var_2, var_3)]
  temp_data <- dat %>%
    filter_all(all_vars(!is.na(.))) %>%
    group_by_(var_1, var_2, var_3) %>%
    summarise(counts = n())
  
  # remove unneeded strings
  temp_data <- as.data.frame(temp_data)
  temp_data <- temp_data[!grepl(remove_string, as.character(temp_data[,var_2])),]
  temp_data <- temp_data[!grepl(remove_string, as.character(temp_data[,var_3])),]
  
  # join them
  temp_totals <- as.data.frame(tapply(temp_data$counts,temp_data[, 1:2] , FUN=sum))
  temp_totals$var_1 <- rownames(temp_totals)
  temp_melt <- melt(temp_totals, id.vars = 'var_1')
  temp_melt <- temp_melt[complete.cases(temp_melt),]
  names(temp_melt) <- c('var_1', 'var_2', 'value')
  colnames(temp_data)[1] <- 'var_1'
  colnames(temp_data)[2] <- 'var_2'
  colnames(temp_data)[3] <- 'var_3'
  temp_new <- left_join(temp_data, temp_melt)
  
  # get percentage column
  temp_new$per <- round((temp_new$counts/temp_new$value)*100, 2)
  
  temp_new$counts <- NULL
  temp_new$value <- NULL
  
  # bar plot 
  levels_of_var_1 <- length(unique(temp_new$var_1))
  if(levels_of_var_1 == '2') {
    cols <- c("#67001F", "#D1E5F0")
  } else {
    cols <- brewer.pal(levels_of_var_1, name = 'RdBu')
  }
  ggplot(temp_new, aes(var_2, per, group = interaction(var_1, var_3), fill = var_1)) + 
    geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
    scale_fill_manual(name="", values = cols) +
    xlab('') +
    ylab('Percent') +
    ggtitle(title) +
    geom_text(aes(label=per), position=position_dodge(width=0.9), vjust=-0.25) +
    theme_databrew()
  
}



group_by_plot_numeric_1 <- function(var_1, var_2, dat,title) {
  
  dat <- dat[, c(var_1, var_2)]
  dat[, var_2] <- as.numeric(as.character(dat[, var_2]))
  temp_data <- dat %>%
    filter_all(all_vars(!is.na(.))) %>%
    group_by_(var_1) %>%
    summarise_all(funs(mean))
  
  
  temp_data$counts <- NULL
  
  colnames(temp_data)[1] <- 'var_1'
  colnames(temp_data)[2] <- 'var_2'
  
  
  # bar plot 
  ggplot(temp_data, aes(var_1, var_2)) + 
    geom_bar(stat = 'identity', alpha = 0.7, fill = 'black', colour = 'lightblue') +
    xlab('') +
    ylab('Mean #') +
    ggtitle(title) +
    geom_text(aes(label=round(var_2, 3)), position=position_dodge(width=0.9), vjust=-0.25) +
    theme_databrew()
  
}


group_by_plot_numeric_2 <- function(var_1, var_2, var_3, dat, remove_string, title) {
  
  dat <- dat[, c(var_1, var_2, var_3)]
  dat[, var_3] <- as.numeric(as.character(dat[, var_3]))
  temp_data <- dat %>%
    filter_all(all_vars(!is.na(.))) %>%
    group_by_(var_1, var_2) %>%
    summarise_all(funs(mean))
  
  
  colnames(temp_data)[1] <- 'var_1'
  colnames(temp_data)[2] <- 'var_2'
  colnames(temp_data)[3] <- 'var_3'
  
  levels_of_var_1 <- length(unique(temp_data$var_1))
  if(levels_of_var_1 == '2') {
    cols <- c("#67001F", "#D1E5F0")
  } else {
    cols <- brewer.pal(levels_of_var_1, name = 'RdBu')
  }
  # bar plot 
  ggplot(temp_data, aes(var_2, var_3, fill = var_1)) + 
    geom_bar(stat = 'identity', position = 'dodge', alpha = 0.8) +
    scale_fill_manual(name="", values=cols) +
    xlab('') +
    ylab('Mean #') +
    ggtitle(title) +
    geom_text(aes(label=round(var_3, 2)), position=position_dodge(width=0.9), vjust=-0.25) +
    theme_databrew()
  
}





######################################################################################################################################
# Physical activity 
# hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency" 

gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency <- 
  as.numeric(as.character(gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency))

gss12$phys_recode <- ifelse(gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency > 7, 'More than 7',
                            ifelse(is.na(gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency), NA,
                                   gss12$hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency))

group_by_plot_factor(var_1 = 'demo_gss12_sex',
                     var_2 = 'phys_recode' , 
                     remove_string = 'Not|Don',
                     title = '# of moderate to vigorous activites prior week',
                     dat = gss12)

group_by_plot_numeric_1(var_1 = 'demo_gss12_sex',
                        var_2 = 'hw_gss12_participated_moderate_vigorous_physical_activity_past_week_frequency' , 
                        title = '# of moderate to vigorous activites prior week',
                        dat = gss12)


##################################################################################################################
# closeness to friends and relatives 

group_by_plot_numeric_1(var_1 = 'demo_gss13_sex' , 
                        var_2 = 'sf_gss13_relatives_feel_close',
                        title = 'Relatives',
                        dat = gss13)

group_by_plot_numeric_2(var_1 = 'demo_gss13_sex' , 
                        var_2 = 'demo_gss13_age_group',
                        var_3 = 'sf_gss13_relatives_feel_close',
                        title = 'Avg # of relatives respondent feels close to',
                        dat = gss13)

group_by_plot_factor(var_1 = 'demo_gss14_sex', 
                     var_2 = 'hw_gss14_selfrated_general_health', 
                     remove_string = 'Ref|Don|Vali|Not', 
                     title = 'Self rated general health by sex', 
                     dat = gss14)



######################################################################################################################
# self percieved physical health


group_by_plot_factor(var_1 = 'demo_gss14_sex', 
                     var_2 = 'hw_gss14_selfrated_general_health', 
                     dat = gss14, 
                     remove_string = 'Don|Not', 
                     title = 'Self rated general health by sex 2014')

group_by_plot_factor(var_1 = 'demo_gss13_sex', var_2 = 'hw_gss13_self_rated_general_health', dat = gss13, remove_string = 'Don|Not', 
                     title = 'Self rated general health by sex 2013')


group_by_plot_factor(var_1 = 'demo_gss12_sex', var_2 = 'hw_gss12_self_reported_health', dat = gss12, remove_string = 'Don|Not', 
                     title = 'Self rated general health by sex 2012')

group_by_plot_factor(var_1 = 'demo_gss11_sex', var_2 = 'hw_gss11_self_reported_health', dat = gss11, remove_string = 'Don|Not', 
                     title = 'Self rated general health by sex 2011')

group_by_plot_factor(var_1 = 'demo_gss10_sex', var_2 = 'hw_gss10_health', dat = gss10, remove_string = 'Don|Not', 
                     title = 'Self rated general health by sex 2010')



#########################################################################################################################################
# self percieved mental health

group_by_plot_factor(var_1 = 'demo_gss14_sex', var_2 = 'hw_gss14_selfrated_mental_health', dat = gss14, remove_string = 'Ref|Don|Not', 
                     title = 'Self rated mental health by sex 2014')

group_by_plot_factor(var_1 = 'demo_gss13_sex', var_2 = 'hw_gss13_self_rated_mental_health', dat = gss13, remove_string = 'Ref|Don|Not', 
                     title = 'Self rated mental health by sex 2013')

group_by_plot_factor(var_1 = 'demo_gss12_sex', var_2 = 'hw_gss12_self_reported_mental_health', dat = gss12, remove_string = 'Ref|Don|Not', 
                     title = 'Self rated mental health by sex 2012')

group_by_plot_factor(var_1 = 'demo_gss11_sex', var_2 = 'hw_gss11_self_reported_mental_health', dat = gss11, remove_string = 'Ref|Don|Not', 
                     title = 'Self rated mental health by sex 2011')

summary(as.factor(gss10$hw_gss10_mental_health))
gss10$hw_gss10_mental_health <- factor(gss10$hw_gss10_mental_health, levels = c('excellent', 'very good', 'good', 'fair', 'poor', "Don't know"))
group_by_plot_factor(var_1 = 'demo_gss10_sex', var_2 = 'hw_gss10_mental_health', dat = gss10, remove_string = 'Ref|Don|Not', 
                     title = 'Self rated mental health by sex 2010')


summary(as.factor(osduhs$demo_osduhs_race))
group_by_plot_factor(var_1 = 'demo_osduhs_race', var_2 = 'hw_osduhs_mental_health', dat = osduhs, remove_string = 'not sure', 
                     title = 'Self rated mental health by sex 2010')


######################################################################################################################################
# caring for others 
# cc_gss13_favour_for_neighbour_past_month
summary(as.factor(gss13$cc_gss13_favour_for_neighbour_past_month))
summary(as.factor(gss12$cc_gss12_done_favour_for_neighbour_past_month))

group_by_plot_factor(var_1 = 'demo_gss13_sex', 
                     var_2 = 'cc_gss13_favour_for_neighbour_past_month',
                     dat = gss13,
                     remove_string = 'Just|Valid|Don|Refus|Not',
                     title = 'Favour for neightbor in past month 2013')

group_by_plot_factor(var_1 = 'demo_gss12_sex', 
                     var_2 = 'cc_gss12_done_favour_for_neighbour_past_month',
                     dat = gss12,
                     remove_string = 'Just|Valid|Don|Refus|Not',
                     title = 'Favour for neightbor in past month 2012')





######################################################################################################################################
# child services 
# sf_gss14_child_victim_report_child_services
# hw_gss14_child_victim_sexual_assault_no_report
summary(as.factor(gss14$sf_gss14_child_victim_report_child_services))
# summary(as.factor(gss14$hw_gss14_child_victim_sexual_assault_no_report))

group_by_plot_factor(var_1 = 'demo_gss14_sex', 
                     var_2 = 'sf_gss14_child_victim_report_child_services',
                     dat = gss14,
                     remove_string = 'Just|Valid|Don|Refus|Not',
                     title = '% of childhood abuses reported to child services')

group_by_plot_factor(var_1 = 'demo_gss14_age_group', 
                     var_2 = 'sf_gss14_child_victim_report_child_services',
                     dat = gss14,
                     remove_string = 'Just|Valid|Don|Refus|Not',
                     title = '% of childhood abuses reported to child services')



######################################################################################################################################
# homelessness 
lapply(survey, function(x) colnames(x)[grepl('main', colnames(x))])

# sf_gss14_homeless_ever_been_homeless
# sf_gss14_homeless_longest_period_of_time_for_which_you_have_been_homeless
# sf_gss14_homeless_had_to_temporarily_live_with_familyfriendscaretc    
# sf_gss14_homeless_longest_period_living_with_familyfriendscaretc

# homeless by sex and age 
group_by_plot_factor_2(var_1 = 'demo_gss14_sex', 
                       var_2 = 'demo_gss14_age_group',
                       var_3 = 'sf_gss14_homeless_ever_been_homeless',
                       dat = gss14,
                       remove_string = 'Just|Valid|Don|Refus|Not',
                       title = '% of been homeless')

# homeless by sex and age 
group_by_plot_factor(var_1 = 'demo_gss14_age_group', 
                       var_2 = 'sf_gss14_homeless_ever_been_homeless',
                       dat = gss14,
                       remove_string = 'Just|Valid|Don|Refus|Not',
                       title = '% of been homeless')


######################################################################################################################################
# financial knowledge
# demo_cfc_total_assets
# demo_cfc_estimate_value_of_debts_and_liabil_grpd

summary(as.factor(cfc$demo_cfc_estimate_value_of_debts_and_liabil_grpd))
summary(as.factor(cfc$demo_cfc_total_assets))
summary(as.factor(cfc$demo_cfc_aboriginal_status))

# homeless by sex and age 
group_by_plot_factor_2(var_1 = 'demo_cfc_sex', 
                     var_2 = 'demo_cfc_age_of_respondent_grouped',
                     var_3 = 'demo_cfc_estimate_value_of_debts_and_liabil_grpd',
                     dat = cfc,
                     remove_string = 'Just|Valid|Don|Refus|Not',
                     title = 'Estimate of value of debts and liabilities')

# homeless by sex and age 
group_by_plot_factor(var_1 = 'demo_cfc_age_of_respondent_grouped', 
                     var_2 = 'demo_cfc_estimate_value_of_debts_and_liabil_grpd',
                     dat = cfc,
                     remove_string = 'Just|Valid|Don|Refus|Not',
                     title = 'Estimate of value of debts and liabilities')




##########################################################################################
# ASSETS
summary(as.factor(cfc$demo_cfc_total_assets))

# homeless by sex and age 
group_by_plot_factor_2(var_1 = 'demo_cfc_sex', 
                       var_2 = 'demo_cfc_age_of_respondent_grouped',
                       var_3 = 'demo_cfc_total_assets',
                       dat = cfc,
                       remove_string = 'Just|Valid|Don|Refus|Not',
                       title = 'Estimate of value of total assets')

# homeless by sex and age 
group_by_plot_factor(var_1 = 'demo_cfc_age_of_respondent_grouped', 
                     var_2 = 'demo_cfc_total_assets',
                     dat = cfc,
                     remove_string = 'Just|Valid|Don|Refus|Not',
                     title = 'Estimate of value of total assets')








#
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
