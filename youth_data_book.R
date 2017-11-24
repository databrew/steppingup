library(dplyr)
library(tidyr)
library(broom)
library(feather)
library(foreign)
library(sas7bdat)

##########
# datasets
##########
# 1) "1987_2015_labour_force_survey"
# 2) "2010_general_social_survey"
# 3) "2010_imdb_tax_file"
# 4) "2011_general_social_survey"
# 5) "2012_general_social_survey"
# 6) "2012_program_for_international_assessment_of_adult_comptencies"
# 7)  "2013_general_social_survey"
# 8) "2014_cananda_financial_capabilities_survey"
# 9) "2014_employment_insurance_coverage_survey"
# 10) "2014_general_social_survey"
# 11) "2015_ontario_student_drug_use_and_health_survey"
#12) "year_ontario_social_assistance_database"
##########################################################################

##########
# function will loop through files and get the attributes of the columns names as well
# as the summary for that column (and number of NAs)
##########

path_to_data = "data/youth_data_book"
i= j = 1
get_youth_data <-
  function(path_to_data) {

  # get folders in path
  youth_data_folders <- list.files(path_to_data)
  # get list to store objects
  data_folders <- list()

  for (i in 1:length(youth_data_folders)) {
    temp_folder <- youth_data_folders[i]
    # get list of data in given data folder
    data_names <- list.files(paste(path_to_data, temp_folder, sep = '/'))
    # get list to store data
    data_list <- list()
    for (j in 1:length(data_names)) {
      temp_data <- data_names[j]
      if(grepl('.sav', temp_data)) {
         temp_dat <- read.spss(file = paste(path_to_data,
                                            temp_folder,
                                            temp_data, sep = '/'),
                                      use.value.labels = T,
                                      to.data.frame = T,
                                      trim.factor.names = T,
                                      trim_values = F,
                                      use.missings = T)

         # get column names
         var_names <- as.data.frame(attr(temp_dat,"variable.labels"))
         var_names$var_short <- rownames(var_names)
         names(var_names)[1] <- 'var_long'
         age_names <- var_names$var_short[grepl('AGE|age ', var_names$var_short)]
         summary_age_list <- list()

         if(length(age_names) == 0) {

           var_names$age_summary <- 'no_age_variable'
         } else {
           for(a in 1:length(age_names)) {
             sub_age <- as.character(age_names[a])
             summary_age_list[[a]] <- paste(unique(temp_dat[, sub_age]), collapse = ', ')
           }
           summary_age <- do.call(rbind, summary_age_list)
           if(length(age_names) > 1) {
             summary_age <- paste(summary_age, collapse = ' ||||| ')
             var_names$age_summary <- summary_age
           } else {
             var_names$age_summary <- as.character(summary_age)
           }
         }
         # missing values per column
         na_index <- as.data.frame(apply(temp_dat, 2, function(x) length(which(is.na(x)))))
         na_index$var_short <- rownames(na_index)
         names(na_index)[1] <- 'na_counts'

         # join the two
         var_data <- inner_join(var_names, na_index, by = 'var_short')
         var_data$num_rows <- as.numeric(nrow(temp_dat))
         var_data$num_cols <- as.numeric(ncol(temp_dat))
         names(var_data) <- c('long_name','short_name', 'age_summary', 'na_count', 'num_rows', 'num_cols')
         var_data$per_missing <- round((var_data$na_count/var_data$num_rows)*100, 2)
         var_data$folder_name <- temp_folder
         var_data$data_name <- temp_data

         data_list[[j]] <- var_data

      } else {

        temp_dat <- as.data.frame(read.sas7bdat(file = paste(path_to_data,
                                                             temp_folder,
                                                             temp_data,
                                                             sep = '/')),
                                  stringsAsFactors = F)



        # get column names
        var_names <- as.data.frame(colnames(temp_dat))
        names(var_names) <- 'var_short'
        age_names <- var_names$var_short[grepl('AGE|age ', var_names$var_short)]
        summary_age_list <- list()
        for(a in 1:length(age_names)) {
          sub_age <- as.character(age_names[a])
          summary_age_list[[a]] <- paste(unique(temp_dat[, sub_age]), collapse = ', ')
        }

        summary_age <- do.call(rbind, summary_age_list)
        if(length(age_names) > 1) {
          summary_age <- paste(summary_age, collapse = ' ||||| ')
          var_names$age_summary <- summary_age
        } else {
          var_names$age_summary <- as.character(summary_age)

        }
        # missing values per column
        na_index <- as.data.frame(apply(temp_dat, 2, function(x) length(which(is.na(x)))))
        na_index$var_short <- rownames(na_index)
        # join the two
        var_data <- inner_join(var_names, na_index, by = 'var_short')
        var_data$num_rows <- as.numeric(nrow(temp_dat))
        var_data$num_cols <- as.numeric(ncol(temp_dat))
        names(var_data) <- c('short_name', 'age_summary', 'num_rows', 'num_cols', 'na_count')
        var_data$per_missing <- round((var_data$na_count/var_data$num_rows)*100, 2)
        var_data$long_name <- var_data$short_name
        var_data$folder_name <- temp_folder
        var_data$data_name <- temp_data
        data_list[[j]] <- var_data
      }
    }
    data_folders[[i]] <- do.call(rbind, data_list)
    print(youth_data_folders[i])
  }
  temp_final <- do.call(rbind, data_folders)
  return(temp_final)
}

temp_results <- get_youth_data(path_to_data = "data/youth_data_book")
saveRDS(temp_results, '~/Desktop/temp_results.rda')

# temp_results <- readRDS('~/Desktop/temp_results.rda')
# temp_results$age_summary <- NULL
#
# # remove duplicates in long name
# temp_results <- temp_results[!duplicated(temp_results$long_name),]
#
# # remove rows that have over 40% missing
# temp_results <- temp_results[temp_results$per_missing < 40,]
#
# temp_results <- as.data.frame(apply(temp_results, 2, function(x) trimws(x)),stringsAsFactors = F)
# temp_results <- temp_results[order(temp_results$long_name, decreasing = T),]
#
# temp_results <- temp_results[!grepl("Bootstrap", temp_results$long_name),]
#
# temp_results <- temp_results[order(temp_results$folder_name),]
#
# temp_results$per_missing <- as.numeric(temp_results$per_missing)
# temp <- temp_results %>%
#   group_by(folder_name, data_name) %>%
#   summarise(mean_na_per_column = mean(per_missing, na.rm = T))
#
# write.csv(temp, '~/Desktop/youth_data_book_data_summary.csv')
#

##########################################################################

##########
# Employment Insurance Coverage Survey
##########
# one sub folder:  cfcs-5159-E-2014
# 5 files

# 1) cfcs-5159-E-2014.pdf - this is the survey report with all the data books and info
# 2) CFCS-Youth-YDB-Indicators.sps - data
# 3) CFCS-Tables.xlsx - this has cross-tabulated data tables for, what im assuming is from .sps file
# 4) cfcs-5159-E-2014_F1.sav - spss file

# basic info: need to add year (2014) and subset by age and geography.
# also when use.missing = F, NA's will have real non response values.

# First read in cfcs-5159-E-2014_F1.sav
temp_dat <- as.data.frame(attr(read.spss('cananda_financial_capabilities_survey/cfcs_5159E_2014/cfcs-5159-E-2014_F1.sav',
                                         use.value.labels = T, to.data.frame = T, trim.factor.names = T, trim_values = F,
                                         use.missings = F), "variable.labels"))


