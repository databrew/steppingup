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


##########
# Source databrew package files
##########
db_files <- dir('R')
for (i in 1:length(db_files)){
  source(paste0('R/', db_files[i]))
}

# Source helper functions
source('functions.R')

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

clean_cols_long <- function(x, wide_column_start, dups, name){
  colnames(x) <- tolower(colnames(x))
  colnames(x) <- gsub('x', replacement = '', colnames(x))
  for(i in 1:ncol(x)){
    temp.name <- colnames(x)[i]
    if(grepl('.', temp.name, fixed = T)){
      temp.name_1 <- unlist(strsplit(temp.name, '.', fixed = T))
      temp.name_2 <- paste0(temp.name_1[temp.name_1 != ""], collapse = '_')
      temp.name <- temp.name_2
    }
    colnames(x)[i] <- temp.name
  }
  # make long format
  # find 6 column name and last colum name
  column_start <- colnames(x)[wide_column_start]
  last_column <- colnames(x)[ncol(x)]
  if(dups) {
    x <-  x[,!duplicated(colnames(x))]
    column_start <- colnames(x)[wide_column_start]
    last_column <- colnames(x)[ncol(x)]
  }
  x_long <- gather(x, key, value, column_start:last_column)
  #add a year column
  x_long$year <- as.character(unlist(lapply(strsplit(name, '_'), function(x) x[1])))
  return(x_long)
}


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

      # # remove any 'Total' from columns
      temp_data <- as.data.frame(temp_data[, !grepl('Total', colnames(temp_data))], stringsAsFactors = F)

      # remove any row that has total by looping through columns
      remove_total <- function(data_frame) {
        # group by get an indicator for column name
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

# read in temporary data, before organizing into theme, returns a list of 10 data sets,
# corresponding to the 10 folder (multiple data sets per folder were combined with full_join,
# creating NAs)
survey_data <- get_data(data_type = 'survey')

# Get census data
# If the aggregated/cleaned file already exists (ie, this script has already been run)
# load it
if('census_all.feather' %in% dir('data')){
  census_all <- read_feather('data/census_all.feather')
} else {
  # Otherwise (ie, the aggregated/cleaned flie does not already exist)
  # Read the files from csvs, clean, and aggregate
  census_all <- get_data(data_type = "census")

  # Clean up geography
  geo_dictionary <- census_all %>% group_by(geo, geo_code) %>% tally %>% dplyr::select(-n) %>% ungroup
  geo_dictionary$geo <- unlist(lapply(strsplit(geo_dictionary$geo, ' (', fixed = TRUE), function(x){x[1]}))
  geo_dictionary <- geo_dictionary %>%
    arrange(geo_code, geo)
  geo_dictionary <- geo_dictionary %>%
    filter(!duplicated(geo_code))
  census_all <-
    census_all %>%
    dplyr::select(-geo) %>%
    left_join(geo_dictionary, by = 'geo_code') %>%
    dplyr::select(geo_code, geo, year, age, sex, pob, vm, special_indicators, value)
  census_all <-
    census_all %>%
    rename(si = special_indicators)

  # Remove the 15 to 24 age group (since it overlaps with others)
  census_all <- census_all %>%
    filter(age != '15 to 24 years')


  # Rename geo code to geography
  census_all <-
    census_all %>%
    rename(geography = geo_code)

  # and then save data to to "data" folder for faster retrieval in subsequent runs
  # save(census_all, file = 'data/census_all.RData')
  write_feather(census_all, 'data/census_all.feather')
}
