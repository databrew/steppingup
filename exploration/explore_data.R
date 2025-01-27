library(tidyr)
library(broom)
library(feather)
library(foreign)
library(sas7bdat)

# GO THROUGH EACH ONE AND FINISH FILLING OUT DATA NOTES AND CLEAN AND JOIN

survey_data <- T
if(!survey_data) {
  source('global.R')
}

##########
# datasets - survey
##########
# survey_data folder
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

# read in variable list that xing and i chose
var_list <- read.csv('data/survey_data/var_summary.csv')
var_names <- as.character(var_list$long_name)

# get id indicators

#########
# read in survey data, subset by our variables, and write to csv
#########
path_to_data <- 'data/survey_data'
survey_folders <- list.files(path_to_data)


result_list <- list()
for(i in 1:length(survey_folders)) {
  temp_folder <- survey_folders[i]
  survey_data <- list.files(paste(path_to_data, temp_folder, sep = '/'))
  data_list <- list()
  for(j in 1:length(survey_data)) {
    temp_data <- survey_data[j]
    if (grepl('.sav', temp_data)) {
        temp_dat <- read.spss(file = paste(path_to_data,
                                           temp_folder,
                                           temp_data, sep = '/'),
                              use.value.labels = T,
                              to.data.frame = T,
                              trim.factor.names = T,
                              trim_values = F,
                              use.missings = T)


        # get column names
        colnames(temp_dat) <- attr(temp_dat,"variable.labels")
        colnames(temp_dat) <- gsub('.', '', as.character(colnames(temp_dat)), fixed = T)
        temp_sub <-  temp_dat[, colnames(temp_dat)[colnames(temp_dat) %in% var_names]]
        colnames(temp_sub) <- tolower(colnames(temp_sub))
        temp_sub <- as.data.frame(temp_sub, stringsAsFactors = F)
        data_list[[j]] <- temp_sub
        } else {

          temp_dat <- as.data.frame(read.sas7bdat(file = paste(path_to_data,
                                                               temp_folder,
                                                               temp_data,
                                                               sep = '/')),
                                    stringsAsFactors = F)
          # get column names
          colnames(temp_dat) <- tolower(colnames(temp_dat))
          temp_sub <- temp_dat
          temp_sub <- as.data.frame(temp_sub, stringsAsFactors = F)
          data_list[[j]] <- temp_sub
      }

  }

  if(length(data_list) > 1) {
    temp_1 <- data_list[[1]]
    temp_2 <- data_list[[2]]
    temp_3 <- data_list[[3]]


    # make colnames the same and join
    join_key <- Reduce(intersect, list(colnames(temp_1),
                                       colnames(temp_2),
                                       colnames(temp_3)))[1]

    # outer join temp1 and temp2
    temp <- full_join(temp_1, temp_2, by = join_key)
    temp_full <- full_join(temp, temp_3, by = join_key)

    result_list[[i]] <- temp_full

  } else {
    result_list[[i]] <- data_list

  }

  print(temp_folder)
}

length(result_list)

save.image('~/Desktop/survey_temp_readin.RData')
load('~/Desktop/survey_temp_readin.RData')

survey_folders[1]
temp_1 <- as.data.frame(result_list[[1]])
apply(temp_1, 2, function(x) any(is.na(x)))
length(which(is.na(temp_1$self.employed)))
dim(temp_1)
colnames(temp_1)

survey_folders[2]
temp_2 <- as.data.frame(result_list[[2]])

survey_folders[3]
temp_3 <- as.data.frame(result_list[[3]])

survey_folders[4]
temp_4 <- as.data.frame(result_list[[4]])

survey_folders[5]
temp_5 <- as.data.frame(result_list[[5]])

survey_folders[6]
temp_6 <- as.data.frame(result_list[[6]])

survey_folders[7]
temp_7 <- as.data.frame(result_list[[7]])

survey_folders[8]
temp_8 <- as.data.frame(result_list[[8]])

survey_folders[9]
temp_9 <- as.data.frame(result_list[[9]])

survey_folders[10]
temp_10 <- as.data.frame(result_list[[10]])

survey_folders[11]
temp_11 <- as.data.frame(result_list[[11]])



# # census data - group by year, geography, and get total population
# "Total - Population"
#

# MAKE TEMP TABLE FOR XING, NO NEED TO REVIEW THIS
temp <- census %>%
  filter(grepl('2006|2016', year))
temp <- temp %>%
  group_by(year, Geography) %>%
  summarise(tot_pop = sum(`Total - Population`, na.rm = T))
#
temp_16 <- temp %>%
  filter(year == '2016')

temp_06 <- temp %>%
  filter(year == '2006')
#
temp <- inner_join(temp_16, temp_06, by = 'Geography')

colnames(temp)
temp <- temp[ , c('Geography', 'tot_pop.x', 'tot_pop.y')]
colnames(temp) <- c('Geography', 'pop_2016', 'pop_2006')

temp$percent <- round(((temp$pop_2016 - temp$pop_2006)/temp$pop_2016)*100, 2)

temp <- temp[order(temp$percent, decreasing = TRUE),]

write_csv(temp, '~/Desktop/pop.csv')



#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# get_data <-
#   function(path_to_data) {
#
#     # get folders in path
#     data_folder <- list.files(path_to_data)
#     # get list to store objects
#     data_folders <- list()
#
#     for (i in 1:length(data_folder)) {
#       # nhs only has data, no sub folders
#       if (grepl('nhs', data_folder)) {
#         data_names <- data_folder
#       } else {
#         temp_folder <- data_folder[i]
#         # get list of data in given data folder
#         data_names <- list.files(paste(path_to_data, temp_folder, sep = '/'))
#       }
#       # get list to store data
#       data_list <- list()
#       for (j in 1:length(data_names)) {
#         temp_data <- data_names[j]
#         if(grepl('.csv', temp_data)){
#         temp_dat <- read.csv(file = paste(path_to_data,
#                                           temp_data, sep = '/'), header = F, stringsAsFactors = F)
#
#         colnames(temp_dat) <- as.character(temp_dat[1,])
#         temp_dat <- temp_dat[-1,]
#
#         # get column names
#         var_names <- as.data.frame(colnames(temp_dat))
#         names(var_names) <- 'var_long'
#
#         # missing values per column
#         na_index <- as.data.frame(apply(temp_dat, 2, function(x) length(which(is.na(x)))))
#         names(na_index)[1] <- 'na_counts'
#
#         # join the two
#         var_data <- cbind(var_names, na_index)
#         var_data$num_rows <- as.numeric(nrow(temp_dat))
#         var_data$num_cols <- as.numeric(ncol(temp_dat))
#         names(var_data) <- c('long_name', 'na_count', 'num_rows', 'num_cols')
#         var_data$per_missing <- round((var_data$na_count/var_data$num_rows)*100, 2)
#         var_data$folder_name <- temp_data
#         var_data$data_name <- temp_data
#
#         data_list[[j]] <- var_data
#
#
#         } else if (grepl('.sav', temp_data)) {
#           temp_dat <- read.spss(file = paste(path_to_data,
#                                              temp_folder,
#                                              temp_data, sep = '/'),
#                                 use.value.labels = T,
#                                 to.data.frame = T,
#                                 trim.factor.names = T,
#                                 trim_values = F,
#                                 use.missings = T)
#
#
#           # get column names
#           var_names <- as.data.frame(attr(temp_dat,"variable.labels"))
#           var_names$var_short <- rownames(var_names)
#           names(var_names)[1] <- 'var_long'
#           age_names <- var_names$var_short[grepl('AGE|age ', var_names$var_short)]
#           summary_age_list <- list()
#
#           # missing values per column
#           na_index <- as.data.frame(apply(temp_dat, 2, function(x) length(which(is.na(x)))))
#           na_index$var_short <- rownames(na_index)
#           names(na_index)[1] <- 'na_counts'
#
#           # join the two
#           var_data <- inner_join(var_names, na_index, by = 'var_short')
#           var_data$num_rows <- as.numeric(nrow(temp_dat))
#           var_data$num_cols <- as.numeric(ncol(temp_dat))
#           names(var_data) <- c('long_name','short_name', 'na_count', 'num_rows', 'num_cols')
#           var_data$per_missing <- round((var_data$na_count/var_data$num_rows)*100, 2)
#           var_data$folder_name <- temp_folder
#           var_data$data_name <- temp_data
#
#           data_list[[j]] <- var_data
#
#
#         } else {
#
#           temp_dat <- as.data.frame(read.sas7bdat(file = paste(path_to_data,
#                                                                temp_folder,
#                                                                temp_data,
#                                                                sep = '/')),
#                                     stringsAsFactors = F)
#
#           # get column names
#           var_names <- as.data.frame(colnames(temp_dat))
#           names(var_names) <- 'var_short'
#           # missing values per column
#           na_index <- as.data.frame(apply(temp_dat, 2, function(x) length(which(is.na(x)))))
#           na_index$var_short <- rownames(na_index)
#           # join the two
#           var_data <- inner_join(var_names, na_index, by = 'var_short')
#           var_data$num_rows <- as.numeric(nrow(temp_dat))
#           var_data$num_cols <- as.numeric(ncol(temp_dat))
#           names(var_data) <- c('short_name', 'num_rows', 'num_cols', 'na_count')
#           var_data$per_missing <- round((var_data$na_count/var_data$num_rows)*100, 2)
#           var_data$long_name <- var_data$short_name
#           var_data$folder_name <- temp_folder
#           var_data$data_name <- temp_data
#           data_list[[j]] <- var_data
#         }
#       }
#       data_folders[[i]] <- do.call(rbind, data_list)
#       print(data_folder[i])
#     }
#     temp_final <- do.call(rbind, data_folders)
#     return(temp_final)
#   }
#
# # get the nhs and survey data
# temp_nhs <- get_data(path_to_data = "data/nhs_data")
# temp_survey <- get_data(path_to_data = "data/survey_data")
#
#
# # add "short_name" to temp_nhs so you can rbind them togther
# temp_nhs$short_name <- temp_nhs$long_name
#
# # combine temp_nhs and temp_survey
# temp_results <- rbind(temp_nhs, temp_survey)
#
# # remove duplicates in long name
# temp_results <- temp_results[!duplicated(temp_results$long_name),]
# temp_results <- temp_results[!duplicated(temp_results$short_name),]
#
# # remove rows that have over 40% missing
# temp_results <- temp_results[temp_results$per_missing < 40,]
#
# # remove bootstrap  and save
# temp_results <- temp_results[!grepl("Bootstrap", temp_results$long_name),]
# temp_results <- as.data.frame(apply(temp_results, 2, function(x) trimws(x)),stringsAsFactors = F)
# write.csv(temp_results, '~/Desktop/var_summary.csv')
#
