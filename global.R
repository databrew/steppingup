library(shinydashboard)
library(sp)
library(raster)
library(maptools)
library(googledrive)
library(yaml)
library(stringr)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(broom)
library(feather)
library(foreign)
library(sas7bdat)
library(rmapshaper)


##########
# Source databrew package files
##########
db_files <- dir('R')
for (i in 1:length(db_files)){
  source(paste0('R/', db_files[i]))
}

# Source helper functions
source('functions.R')
if('processed_survey_data.RData' %in% dir('data')){
  load('data/processed_survey_data.RData')
} else {
  survey_list <- get_survey_data()
  survey <- survey_list[[1]]
  removals <- survey_list[[2]] # removals due to some columns which appear in dict being all NA in data
  # Make the dictionary smaller (don't include those variables which are all NA)
  path_to_data <- 'data/survey_data'
  var_summary <- read_csv(paste0(path_to_data, '/var_summary.csv'))
  var_summary <- var_summary %>% 
    filter(!new_variable %in% removals)
  save(survey,
       var_summary,
       file = 'data/processed_survey_data.RData')
}



##########
# Get Canadian shapefile
##########
# https://drive.google.com/file/d/1vew1zjUFJwR6sQJY0OxaczcddW_fJ1fH/view

if('map_data.RData' %in% dir('data/geo')){
  load('data/geo/map_data.RData')
} else {
  # Get a map of canada
  can2 <- raster::getData(name = 'GADM', country = 'CAN', level = 2)
  # Subset to just Toronto
  ont2 <- can2[can2@data$NAME_1 == 'Ontario',]
  ont2 <- rmapshaper::ms_simplify(ont2)
  # Create a crazy-ass looking version
  ont_crazy <- thinnedSpatialPoly(SP = ont2,
                                  minarea = 0,
                                  tolerance = 5,
                                  topologyPreserve = TRUE)
  # Save for later
  save(ont2, ont_crazy,
       file = 'data/geo/map_data.RData')
}

# Get a fortified version of ont
ont_fortified <- broom::tidy(ont2)
ont_fortified <- ont_fortified %>% left_join(ont2@data %>%
                                               mutate(OBJECTID = as.character(OBJECTID)) %>%
                                               dplyr::select(OBJECTID, CCA_2) %>%
                                               dplyr::rename(id = OBJECTID,
                                                             geography = CCA_2)) %>%
  mutate(geography = as.character(geography))

##########
# all census data from census_data folder in data folder
##########
# first 3 are short form census data. usually they accompany the short form with
# the long form, but conservative govt took over in 2011 and they did the
# National Household survey (nhs), completely voluntary.
# Currently we have 9 csvs.
# "2001_census.csv"
# "2006_census.csv"
# "2011_census.csv"
# "2011_nhs_16_19.csv"
# "2011_nhs_20_24.csv"
# "2011_nhs_25_29.csv"
# "2011_nhs_employment.csv"
# "2011_nhs_employment_toronto.csv"

##########
# all survey data
##########
# survey_data folder
# 1) "1987_2015_labour_force_survey"
# 2) "2010_general_social_survey"
# 3) "2011_general_social_survey"
# 4) "2012_general_social_survey"
# 5) "2012_program_for_international_assessment_of_adult_comptencies"
# 6) "2013_general_social_survey"
# 7) "2014_cananda_financial_capabilities_survey"
# 8) "2014_employment_insurance_coverage_survey"
# 9) "2014_general_social_survey"
# 10) "2015_ontario_student_drug_use_and_health_survey"

##########
# This function will be used in the get_data function to clean columns and make long
##########

get_census_data <- function() {
  
  # first get vector of data set names to loop through later
  data_names <- list.files('data/census_data')
  # cread empty list to store data
  data_list <- list()
  total_list <- list()
  # get data type
  sub_names <- data_names[grepl('census', data_names)]
  # function that loops through each name in for census data and read into a list
  for (i in 1: length( sub_names)) {
    name <- sub_names[i]
    temp_data <- read_csv(paste0('data/census_data/', name))
    # Declare that the encoding is all screwed up for this file
    Encoding(temp_data$Geography) <- "latin1"
    # Address the weirdness with "New Credit (Part)"
    temp_data$Geography <- gsub('(Part) ', '', temp_data$Geography, fixed = TRUE)
    # Keep only the first part of the name (what is with the %?)
    temp_data$Geography <- paste0(unlist(lapply(strsplit(temp_data$Geography, ')', fixed = TRUE),
                                                function(x){x[1]})), ')')
    
    # give Ontario four digit number to subset by.
    temp_data$Geography <- ifelse(grepl('Ontario', temp_data$Geography), 'Ontario', temp_data$Geography)
    
    #subset to Ontarios and 4 digit geo codes
    geo_codes <- unlist(lapply(strsplit(temp_data$Geography,
                                        '(', fixed = TRUE),
                               function(x){
                                 gsub(')', '', x[2], fixed = TRUE)}))
    
    # (those wih NA for the geo_code are all ontario) - give it 3500 so we can subset
    # entirely by 4 digit geo_code
    temp_data$geo_code <- geo_codes
    temp_data$geo_code[is.na(temp_data$geo_code)] <- '3500'
    
    # keep only rows that have 4 number
    temp_data <- temp_data[nchar(temp_data$geo_code) == 4,]
    
    # add year
    year <- as.numeric(substr(name, 1, 4))
    temp_data$year <- year
    
    # Throw away variables depending on the year (since not available in other years)
    # We've checked that the "different" names between 2001 and 2006 are just due to spelling, etc.
    # Therefore, we force the names from 2001 onto 2006
    names(temp_data)[names(temp_data) == 'Total - Low income status (LICO thresholds revised to be comparable to 2006)'] <- 'Total - Income status (LICO)'
    if(year == 2001){
      temp_data <- temp_data[,!grepl('school part time|school full time', names(temp_data))]
      names_2001 <- names(temp_data)
    } else if (year == 2006){
      temp_data <- temp_data[,!grepl('after tax', names(temp_data))]
      names(temp_data) <- names_2001
    } else if (year == 2011){
      temp_data <- temp_data[,!grepl('after tax', names(temp_data))]
      # Keep those columns which are shared
      shared <- temp_data[,names(temp_data) %in% names_2001]
      not_shared <- temp_data[,!names(temp_data) %in% names_2001]
      # Get rid of subsidized data
      not_shared <- not_shared[,!grepl('subsidized', tolower(names(not_shared)))]
      # Get rid of employment rate
      not_shared <- not_shared[,!grepl('Employment rate %', names(not_shared), fixed = TRUE)]
      not_shared <- not_shared[,!grepl('Employee', names(not_shared), fixed = TRUE)]
      # Rename total diploma to match with other years
      names(not_shared)[names(not_shared) == 'Total - Highest certificate, diploma or degree'] <- 'Total - Population by highest certificate, diploma or degree'
      
      library(stringdist)
      fuzzy <- stringdistmatrix(a = names(not_shared),
                                b = names_2001)
      best_matches <- apply(fuzzy, 1, which.min)
      best_names <- names_2001[best_matches]
      names(not_shared) <- best_names
      temp_data <- bind_cols(shared, not_shared)
    }
    
    # store in list
    data_list[[i]] <- temp_data
  }
  census <- bind_rows(data_list)
  # clean column names
  names(census)[2:3] <- c('Age group', 'Sex')
  names(census)[5] <- c('Visible minority')
  
  # remove Total - 15 and up from age group
  census  <- census[!grepl('15 to 24 years', census$`Age group`),]
  # clean sex
  census$Sex <- ifelse(grepl('fem', tolower(census$Sex)), 'Female', 
                       ifelse(grepl('Male', census$Sex), 'Male', 
                              ifelse(grepl('Total', census$Sex), 'Total',NA)))
  # clean place of birth
  census$`Place of birth` <- ifelse(grepl('Total', census$`Place of birth`), 'Total - Place of birth',
                                    ifelse(grepl(' in',census$`Place of birth`), 'Born in Canada',
                                           ifelse(grepl('out', census$`Place of birth`), 'Born outside of Canada',
                                                  NA)))
  
  # fix multiple vismin
  census$`Visible minority` <- gsub('minorities', 'minority', census$`Visible minority`)
  census$`Visible minority` <- ifelse(census$`Visible minority` == "Total visible minority population",
                                      'All visible minorities',
                                      census$`Visible minority`)
  census$`Visible minority` <- ifelse(grepl('Total', census$`Visible minority`), 'Total - Population by visible minority', census$`Visible minority`)
  
  # fix geography
  geo_dictionary <- census %>% 
    dplyr::select(geo_code, Geography) %>% 
    filter(!duplicated(geo_code))
  
  # join dictionary to census
  census <- census %>% 
    dplyr::select(-Geography) %>% 
    left_join(geo_dictionary, by = 'geo_code')
  
  # remove duplicates 
  census <- census %>%
    distinct(geo_code, year, `Age group`, Sex, `Place of birth`, `Visible minority`, .keep_all = TRUE)
  
  census <- census[, unique(c('Geography', 'geo_code', 'year', 'Age group', 'Sex', 'Place of birth', names(census)))]
  
  # Fix column names to make sure that denominators are correctly called "Total"
  names(census)[names(census) == 'Population 15 years and over by Place of Residence 5 years ago'] <-
    paste0('Total - ', 'Population 15 years and over by Place of Residence 5 years ago')
  names(census)[names(census) == 'Population 15 years and over by labour force activity'] <-
    paste0('Total - ', 'Population 15 years and over by labour force activity')
  names(census)[names(census) == 'Population 15 and over by hours of unpaid housework'] <- 
    paste0('Total - ', 'Population 15 and over by hours of unpaid housework')
  names(census)[names(census) == 'Population 15 and over by hours of unpaid childcare'] <- 
    paste0('Total - ', 'Population 15 and over by hours of unpaid childcare')
  names(census)[names(census) == 'Population 15 and over by hours of unpaid care to seniors'] <-
    paste0('Total - ', 'Population 15 and over by hours of unpaid care to seniors')
  census$`Population - concept not applicable` <- NULL
  census$`Population for the low income status variable` <- NULL
  census$`Standard error of average household income $` <- NULL
  
  # Create a new total youth only for ages 15 to 29
  census <- census %>% filter(`Age group` != 'Total - 15 years and over')
  total_rows <- census %>% dplyr::select(-`Age group`) %>% group_by(Geography, geo_code, year, Sex, `Place of birth`, `Visible minority`) %>% summarise_all(.funs = sum) %>% mutate(`Age group` = 'Total - 15 to 29 years')
  census <- bind_rows(census,
                      total_rows)
  # Clean up geography
  census$Geography <- unlist(lapply(strsplit(census$Geography, ','), function(x){x[1]}))
  
  # Add a population column
  census <- census %>%
    dplyr::mutate(Population = `Total - School attendance`) %>%
    dplyr::mutate(`Total - Population` = Population) 
  
  # Add an aboriginal identity column
  new_rows <- 
    census %>%
    filter(`Visible minority` %in% c('Aboriginal identity',
                                     'Non-Aboriginal identity')) %>%
    dplyr::rename(`Aboriginal identity` = `Visible minority`) %>%
    mutate(`Visible minority` = "Total - Population by visible minority") 
  ordered_columns <- unique(c('Geography', 'geo_code',
                              'year', 'Age group',
                              'Sex', 'Place of birth',
                              'Visible minority',
                              'Aboriginal identity',
                              names(new_rows)))
  new_rows <- new_rows[,ordered_columns]
  old_rows <- census %>%
    filter(!`Visible minority` %in% c('Aboriginal identity',
                                      'Non-Aboriginal identity')) %>%
    mutate(`Aboriginal identity` = 'Total - Population by aboriginal identity')
  old_rows <- old_rows[,ordered_columns]
  census <- bind_rows(new_rows, old_rows)
  
  # Remove the word "identity" from the `Aboriginal identity` column
  census <- census %>%
    mutate(`Aboriginal identity` = 
             gsub(' identity', '', `Aboriginal identity`))

  # Change "All others" in vm to "white"
  census <- 
    census %>%
    mutate(`Visible minority` = ifelse(`Visible minority` == 'All others', 'White', `Visible minority`))
  return(census)
}

# # Generate dictionary
# write_csv(data_frame(variable = names(census),
#                      category = NA,
#                      sub_category = NA),
#           'dictionaries/census_dictionary.csv')


##########
# function that will take on argument "data_type" which is either census or nhs
# for the time being only use data_type ='census' and this will read in and clean
# and aggregate census data from 2001, 2006, 2011 and combine them into one large data set
# this will be the base data set for the app and my further analysis
# if you do data_type == 'nhs', then it will return a list the 5 nhs data sets we have, cleaned
# and all for 2011. They do not have geo coding census tracks so for now lets stick with just census.
##########

# data_type = 'nhs'
get_data <- function(data_type) {

  if(data_type == 'census'){
    # first get vector of data set names to loop through later
    data_names <- list.files('data/census_data')
    # cread empty list to store data
    data_list <- list()
    total_list <- list()
    # get data type
    sub_names <- data_names[grepl(data_type, data_names)]
    # function that loops through each name in for census data and read into a list
    for (name in sub_names) {
      temp_data <- read_csv(paste0('data/census_data/', name))
      # Declare that the encoding is all screwed up for this file
      Encoding(temp_data$Geography) <- "latin1"
      # Address the weirdness with "New Credit (Part)"
      temp_data$Geography <- gsub('(Part) ', '', temp_data$Geography, fixed = TRUE)
      # Keep only the first part of the name (what is with the %?)
      temp_data$Geography <- paste0(unlist(lapply(strsplit(temp_data$Geography, ')', fixed = TRUE),
                                                  function(x){x[1]})), ')')

      # give Ontario four digit number to subset by.
      temp_data$Geography <- ifelse(grepl('Ontario', temp_data$Geography), 'Ontario', temp_data$Geography)

      #subset to Ontarios and 4 digit geo codes
      geo_codes <- unlist(lapply(strsplit(temp_data$Geography,
                                          '(', fixed = TRUE),
                                 function(x){
                                   gsub(')', '', x[2], fixed = TRUE)}))

      # (those wih NA for the geo_code are all ontario) - give it 3500 so we can subset
      # entirely by 4 digit geo_code
      temp_data$geo_code <- geo_codes
      temp_data$geo_code[is.na(temp_data$geo_code)] <- '3500'

      # keep only rows that have 4 number
      temp_data <- temp_data[nchar(temp_data$geo_code) == 4,]

      # new stuff 
    
      # create a total able 
      temp_data <- as.data.frame(temp_data[, !grepl('Total', colnames(temp_data))], stringsAsFactors = F)
      
      
      temp_total <- temp_data %>%
        dplyr::select(Geography, `Age groups (5)`, `Sex (3)`, `Place of birth`, `Visible minorit`)
      keep <- apply(temp_total, 1, function(x){any(grepl('Total', x))})
      
      temp_total <- temp_total[keep,]
      
      # remove any row that has total by looping through columns
      remove_total <- function(data_frame) {
        # get an indicator for column name
        variable_names <- as.character(colnames(data_frame))[1:5]
        # loop through variables and remove rows with 'Total'
        for(v in variable_names) {
          data_frame <- data_frame[!grepl('Total', data_frame[, v]),]
          print(v)
        }
        return(data_frame)
      }

      # remove total from all rows
      temp_data <- remove_total(temp_data)

      # Make long
      temp_data_long <- tidyr::gather(temp_data,
                                      key,
                                      value,
                                      `Never married (single) 15 years and over`:`Living in band housing`)

      # Clean up names
      names(temp_data_long) <- c('geo',
                                 'age',
                                 'sex',
                                 'pob',
                                 'vm',
                                 'geo_code',
                                 'special_indicators',
                                 'value')

      # temp_11 <- temp_no_
      # recode sex
      temp_data_long$sex <- gsub('Females', 'Female', temp_data_long$sex)
      temp_data_long$sex <- gsub('Males', 'Male', temp_data_long$sex)

      # recod pob
      temp_data_long$pob <- gsub('birth', 'Birth', temp_data_long$pob)
      temp_data_long$pob <- gsub('inside', 'in', temp_data_long$pob)

      # Clean up sex
      temp_data_long$sex <- ifelse(temp_data_long$sex == 'Total - Sex', 'Total', temp_data_long$sex)

      # Clean up pob
      temp_data_long$pob <-
        ifelse(temp_data_long$pob == 'Total - Place of Birth', 'Total', temp_data_long$pob)

      # Add year
      temp_data_long$year <- as.numeric(substr(name, 1, 4))

      # Remove duplicate columns
      temp_data_long <- temp_data_long[,!duplicated(names(temp_data_long))]
      temp_data_long <- temp_data_long[,!is.na(names(temp_data_long))]

      # Clean up age group
      temp_data_long$age <-
        ifelse(temp_data_long$age == '15 to 2', '15 to 24 years',
               ifelse(temp_data_long$age == 'Total - 15 years and over', '15 +',
                      ifelse(temp_data_long$age == 'Total - 15 year', '15 +',
                             temp_data_long$age)))

    }
    data_list[[name]] <- temp_data_long
  } else {
    # get the survey folder names in data
    path_to_data <- 'data/survey_data'
    var_summary <- read_csv(paste0(path_to_data, '/var_summary.csv'))
    var_names <- as.character(var_summary$long_name)
    survey_folders <- list.files(path_to_data)
    # remove var_summary.csv from the list so that there are 10 unique folders pertaining to each survey
    survey_folders <- survey_folders[!grepl('var_summary', survey_folders)]
    # create list to store results
    result_list <- list()
    # loop through each folder and read in all data in that folder (either 1 or 3)
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

          if(grepl('gss|piaac|cfcs|sduhs', temp_data)) {
            get_year = T
          } else {
            get_year = F
          }

          # get long for variable names
          colnames(temp_dat) <- attr(temp_dat,"variable.labels")
          # get the column names we want from are varibale list
          temp_sub <-  temp_dat[, colnames(temp_dat)[colnames(temp_dat) %in% var_names]]
          temp_sub <- clean_subset_survey(temp_sub, get_year = get_year, folder = temp_folder)
          data_list[[j]] <- as.data.frame(temp_sub)
        }
      }

      if(length(data_list) > 1) {

        list_length = length(data_list)

        if(list_length == 2) {
          temp_1 <- data_list[[1]]
          temp_2 <- data_list[[2]]
          # make colnames the same and join
          join_key <- Reduce(intersect, list(colnames(temp_1),
                                             colnames(temp_2)))[1]
          # outer join temp1 and temp2
          data_frame <- full_join(temp_1, temp_2, by = join_key)
          result_list[[i]] <- data_frame
        } else {
          temp_1 <- data_list[[1]]
          temp_2 <- data_list[[2]]
          temp_3 <- data_list[[3]]
          # make colnames the same and join
          join_key <- Reduce(intersect, list(colnames(temp_1),
                                             colnames(temp_2),
                                             colnames(temp_3)))[1]
          # outer join temp1 and temp2
          temp <- full_join(temp_1, temp_2, by = join_key)
          data_frame <- full_join(temp, temp_3, by = join_key)
          result_list[[i]] <- data_frame
        }

      } else {
        result_list[[i]] <- data_list
      }
      print(temp_folder)
    }
    length(result_list)
  }

  if(data_type == 'census'){
    dat <- bind_rows(data_list)
    return(dat)
  } else {
    return(result_list)
  }
}

# Get surey and census data
# If the aggregated/cleaned file already exists (ie, this script has already been run)
# load it
# CURRENTLY COMMENTING OUT IN ORDER TO SPEED UP
# if('survey_list.rda' %in% dir('data')) {
#   survey_list <- readRDS('data/survey_list.rda')
# } else {
#   survey_list <- get_data(data_type = 'survey')
#   saveRDS(survey_list, 'data/survey_list.rda')
# }

# read in dictionary 
census_dict <- read_csv('dictionaries/census_dictionary.csv')

if('census.feather' %in% dir('data')){
  census <- read_feather('data/census.feather')
} else {
  census <- get_census_data()
  # and then save data to to "data" folder for faster retrieval in subsequent runs
  # save(census, file = 'data/census.RData')
  write_feather(census, 'data/census.feather')
}

# define input cateogry choices
category_choices <- sort(unique(census_dict$category))
category_choices <- category_choices[!category_choices %in% c('demographic', 'geo_code', 'year')]
names(category_choices) <- Hmisc::capitalize(category_choices)

head_vector <- c('Geography', 'geo_code', 'year', 'Age group', 'Sex', 'Place of birth','Visible minority', 'Aboriginal identity', 'Total')

# Eliminate everywhere references to 15 and over
names(census) <- gsub(' 15 and over', '', names(census))
names(census) <- gsub(' 15 years and over', '', names(census))
census_dict$variable <- names(census)

# Make a theme dictionary
theme_dictionary <- 
  data_frame(long_name = c('Supportive families',
                           'Education',
                           'Employment',
                           'Civic engagement',
                           'Diversity',
                           'Communities',
                           'Health and wellness'),
             short_name = c('sf',
                            'ed',
                            'em',
                            'ce',
                            'ds',
                            'cc',
                            'hw'))

# Make a dictionary to associate data set names in the survey list 
# with the code in the var_summary
dataset_dictionary <- 
  data_frame(short_name = c('cfc',
                            'eic',
                            'gss10',
                            'gss11',
                            'gss12',
                            'gss13',
                            'gss14',
                            'lfs',
                            'osduhs',
                            'pisa'),
             long_name = c('2014_cananda_financial_capabilities_survey',
                           '2014_employment_insurance_coverage_survey',
                           '2010_general_social_survey',
                           '2011_general_social_survey',
                           '2012_general_social_survey',
                           '2013_general_social_survey',
                           '2014_general_social_survey',
                           '1987_2015_labour_force_survey',
                           '2015_ontario_student_drug_use_and_health_survey',
                           '2012_program_for_international_assessment_of_adult_comptencies'))

# Clean up the var_summary (survey_dict) to make more usable
survey_dictionary <-
  var_summary %>%
  mutate(data_set = unlist(lapply(strsplit(new_variable, '_'), function(x) x[2]))) %>%
  mutate(theme_name = unlist(lapply(strsplit(new_variable, '_'), function(x) x[1]))) %>%
    mutate(display_name = Hmisc::capitalize(gsub('_', ' ', variable_name))) %>%
    dplyr::rename(short_name = data_set) %>%
    left_join(dataset_dictionary, by = 'short_name') %>%
  mutate(display_name = paste0(display_name, 
                               ' (',
                               gsub('_', ' ', long_name),
                               ')'))
  
# Choices for survey download
survey_download_choices <- names(survey)
names(survey_download_choices) <- Hmisc::capitalize(gsub('_', ' ', survey_download_choices))

# Read in genderrace dictionary
race_gender_dictionary <- read_csv('dictionaries/race_gender_dictionary.csv')
race_gender_dictionary <-
  race_gender_dictionary %>%
  mutate(variable_name = ifelse(variable_name == 'na', NA, variable_name))

# Loop through each survey dataset and add a race / gender var
for(i in 1:nrow(race_gender_dictionary)){
  this_variable <- race_gender_dictionary$variable_name[i]
  if(!is.na(this_variable)){
    this_data_name <- race_gender_dictionary$data_folder[i]
    this_category <- race_gender_dictionary$category[i]
    this_data <- survey[[which(names(survey) == this_data_name)]]
    if(!this_category %in% names(this_data)){
      this_data[,this_category] <- this_data[,this_variable, drop = TRUE]
      survey[[which(names(survey) == this_data_name)]] <- this_data
    }
  }
  print(i)
}

# # Perform a check to ensure that all race/gender variables are in their respective datasets
# out <- rep(NA, nrow(race_gender_dictionary))
# for(i in 1:nrow(race_gender_dictionary)){
#   this_variable <- race_gender_dictionary$variable_name[i]
#   if(!is.na(this_variable)){
#     this_data_name <- race_gender_dictionary$data_folder[i]
#     this_data <- survey[[which(names(survey) == this_data_name)]]
#     out[i] <- this_variable %in% names(this_data)
#   }
# }

# Define a list of variables from the themes
theme_variables <- survey_dictionary %>%
  dplyr::select(-long_name) %>%
  left_join(theme_dictionary, by = c('theme_name' = 'short_name')) %>%
  filter(!grepl('demo_', new_variable),
         !is.na(long_name)) %>%
  .$display_name
