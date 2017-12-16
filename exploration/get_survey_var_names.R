library(tidyverse)

# read in data from desktop
survey_data <- readRDS('~/Desktop/temp_survey_data.rda')

# function that grabs the column names from each dataset 
folder_names <- list.files('data/survey_data')[!grepl('csv', list.files('data/survey_data/'))]

get_survey_vars <- function(survey_data = survey_data){
  
# get list to store loop output
  temp_list <- list()
  for (i in 1:length(survey_data)){
    temp_name <- folder_names[i]
    temp_dat <- as.data.frame(survey_data[[i]])
    temp_list[[i]] <- cbind(variable_name = colnames(temp_dat), data_name = temp_name)
  }
  
  temp_results <- as.data.frame(do.call(rbind, temp_list), stringsAsFactors = F)
}

var_data <- get_survey_vars(survey_data)

write_csv(var_data, '~/Desktop/survey_variables.csv')
