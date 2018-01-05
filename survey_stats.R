##### Script for ad hoc survey analsis
# each model should only have a few predictors and done sequentially
library(tidyverse)
library(glmnet)

# load in survey_data
load('data/processed_survey_data.RData')

# get folder names
path_to_data <-  'data/survey_data'
survey_folders <- list.files(path_to_data)
survey_folders

#########
# Labour force survey
#########












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
