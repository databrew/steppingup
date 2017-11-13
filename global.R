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

# Define whether we want to fetch new data or not
# (useful after a data update, but slow, so generally set to false)
# (set to TRUE only for the first run)
fetch_new <- FALSE

# new data just added by xing
# 2011_special_indicators_by_vismin
# 2011_birthplace_by_vismin
# 2011_age_sex_by_vismin

# Get Canadian shapefile
if('map_data.RData' %in% dir('data/geo') & !fetch_new){
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


# Download each file
if('all_drive_data.RData' %in% dir('data/drive') & !fetch_new){
  load('data/drive/all_drive_data.RData')
} else {
  g_files <- drive_find(pattern = 'databrew_youth_compass',
                        n_max = 100)
  # use .httr-oauth
  # Download each file
  for (i in 1:nrow(g_files)){
    file_name <- g_files$name[i]
    file_id <- g_files$id[i]
    message('Fetching ', file_name, ' from google drive.')
    drive_download(file = as_id(file_id),
                   path = paste0('data/drive/', file_name),
                   overwrite = TRUE)
  }
  # Load in each file
  object_names <- c()
  for (i in 1:nrow(g_files)){
    # Get the file name
    this_file <- g_files$name[i]
    # Get the year (if applicable)
    year <- stringr::str_extract(this_file, "[[:digit:]]+")
    # If a csv, clean up the name and then read it in
    if(grepl('.csv', this_file, fixed = TRUE)){
      this_path <- paste0('data/drive/', this_file)
      this_name <- gsub('databrew_youth_compass', '', this_file)
      has_year <- !is.na(year)
      if(has_year){
        this_name <- gsub(paste0('_', year, '_'), '', this_name)
      }
      file_type <- unlist(strsplit(this_name, '.', fixed = TRUE))[2]
      this_name <- gsub(paste0('.', file_type),
                        '',
                        this_name,
                        fixed = TRUE)
      # Add the year back ot the name
      if(has_year){
        this_name <- paste0(this_name, '_', year)
      }

      # Get the column names only
      if(grepl('special|birth|by_vismin', this_path)){
        skipper <- 3
      } else {
        skipper <- 2
      }
      # if(grepl('birth', this_path) &
      #    grepl('2011', this_path)){
      #    skipper <- 2
      # }
      column_names <- read_csv(this_path,
                               col_names = FALSE,
                               n_max = skipper)
      # Generate better column names
      new_column_names <- c()
      for(j in 1:ncol(column_names)){
        unlisted <- unlist(column_names[,j])
        unlisted <- unlisted[!is.na(unlisted)]
        new_column_name <- paste0(unlisted, collapse = ' ')
        new_column_names[j] <- new_column_name
      }
      new_column_names <- gsub(' - Place of birth',
                               '_total',
                               new_column_names)
      # Read in the full data
      if(grepl('by_vismin', this_path)) {

        full_data <- read_csv(this_path,
                              col_names = TRUE,
                              skip = 0)
      } else {
        full_data <- read_csv(this_path,
                              col_names = TRUE,
                              skip = skipper -1)
      }

      # Replace the column names
      names(full_data) <- new_column_names
      # Assign to the global environment
      message('Reading in ',
              this_file,
              ' as ',
              this_name)

      assign(this_name, full_data)
      object_names[i] <- this_name
    }
  }
  object_names <- object_names[!is.na(object_names)]
  save(list = object_names,
       file = 'data/drive/all_drive_data.RData')
}

# Combine into more meaningful datafames
clean_columns_age_sex <- function(x){
  x <- tolower(x)
  x <- gsub(' ', '_', x)
  x <- gsub('-', '', x)
  x <- gsub('__', '_', x)
  x <- gsub('_to_', '_', x)
  x <- gsub('_sex_', '_', x)
  sex <- ifelse(grepl('female', x), 'female',
                ifelse(grepl('male', x), 'male',
                       ifelse(grepl('total', x), 'total',
                              NA)))
  age <- ifelse(grepl('15_19', x), '15_19',
                ifelse(grepl('15_24', x), '15_24',
                       ifelse(grepl('20_24', x), '20_24',
                              ifelse(grepl('25_29', x), '25_29',
                                     ifelse(grepl('15_years_and_over', x), '15_up',
                                            NA)))))
  new_name <- paste0(sex, '_', age)
  new_name <- ifelse(is.na(age), x, new_name)
  return(new_name)
}


# Clean up column names and combine
names(age_sex_2001) <- clean_columns_age_sex(names(age_sex_2001))
names(age_sex_2006) <- clean_columns_age_sex(names(age_sex_2006))
names(age_sex_2011) <- clean_columns_age_sex(names(age_sex_2011))

# homogenize all age_sex data
clean_columns_age_sex_vismin <- function(x) {
  x <- tolower(x)
  x <- gsub(' ', '_', x)
  x <- gsub('-', '_', x)
  x <- gsub('f', 'female', x)
  x <- gsub('m', 'male', x)

  return(x)
}
# apply to column names
names(age_sex_by_vismin_2011) <- clean_columns_age_sex_vismin(names(age_sex_by_vismin_2011))

#bind other age_sex data
age_sex <-
  bind_rows(
    age_sex_2001 %>% mutate(year = 2011),
    age_sex_2006 %>% mutate(year = 2006),
    age_sex_2011 %>% mutate(year = 2011)
  )

# Make both age_sex and age_sex_by_vismin
age_sex <-
  age_sex %>%
  gather(age_sex, value, total_15_up:female_25_29)

age_sex_by_vismin_2011 <-
  age_sex_by_vismin_2011 %>%
  gather(age_sex, value, total_15_24_1702345_502670:male_25_29_396485_116590)


create_age_gender <- function(x) {
  # Separate age and sex
  x$sex <- ifelse(grepl('female', x$age_sex), 'female',
                  ifelse(grepl('male', x$age_sex), 'male', 'total'))
  x$age_group <- unlist(lapply(strsplit(x$age_sex, split = '_'), function(x){paste0(x[2], '_', x[3])}))
  return(x)
}

# apply to age sex data
age_sex <- create_age_gender(age_sex)
age_sex_by_vismin_2011 <- create_age_gender(age_sex_by_vismin_2011)

# clean key for geo codes
clean_geo_key <- function(x) {
  # create new column based on number ids in geography
  temp_geo_key <- unlist(lapply(strsplit(x$geography, '(', fixed = T), function(x) x[length(x)]))
  # remove ) and X
  temp_geo_key <- gsub(')', replacement = '', temp_geo_key, fixed = T)
  temp_geo_key <- gsub('X', replacement = '', temp_geo_key, fixed = T)
  # put in data frame
  x$geography <- temp_geo_key
  # keep only 4 digit and ontario
  x <- subset(x, grepl('On', geography) | nchar(geography) == '4')
  return(x)
}

# get subset age_sex - with 4 digit codes and ontario
age_sex <- clean_geo_key(age_sex)

# Get rid of the no longer necessary files
rm(age_sex_2001, age_sex_2006, age_sex_2011)

# Clean up the birthplace data
# save.image('~/Desktop/temp_step_up.RData')

# (have to define special function for birthplace 2011, different format)
clean_columns_birthplace <- function(x){
  sex <- ifelse(grepl('Female', x), 'female',
                ifelse(grepl('Male', x), 'male',
                       ifelse(grepl('Total', x), 'total',
                              ifelse(grepl('Geography', x), NA,
                                     'total'))))
  age <- ifelse(grepl('15 to 24', x), '15_24',
                ifelse(grepl('15 to 19', x), '15_19',
                       ifelse(grepl('20 to 24', x), '20_24',
                              ifelse(grepl('25 to 29', x), '25_29',
                                     ifelse(grepl('15 years and', x), '15_up',
                                            NA)))))
  place <- ifelse(grepl('in', x), 'Canada',
                  ifelse(grepl('out', x), 'Abroad', NA))
  place <- ifelse(is.na(place) & x != 'Geography', 'Anywhere', place)
  new_name <- paste0(sex, '_', age, ifelse(!is.na(place), '_', ''), ifelse(!is.na(place), place, ''))
  x <- tolower(x)
  new_name <- ifelse(is.na(age), x, new_name)
  new_name <- tolower(new_name)
  return(new_name)
}

names(birthplace_2001) <- clean_columns_birthplace(names(birthplace_2001))
names(birthplace_2006) <- clean_columns_birthplace(names(birthplace_2006))
names(birthplace_2011) <- clean_columns_birthplace(names(birthplace_2011))

# homogenize all age_sex data
clean_columns_age_sex_vismin <- function(x) {
  x <- tolower(x)
  x <- gsub(' ', '_', x)
  x <- gsub('-', '_', x)
  x <- gsub('f', 'female', x)
  x <- gsub('m', 'male', x)

  return(x)
}

# apply to column names
names(birthplace_by_vismin_2011) <- clean_columns_age_sex_vismin(names(birthplace_by_vismin_2011))

# Combine
birthplace_2011$total_15_up_anywhere <- as.numeric(birthplace_2011$total_15_up_anywhere)
birthplace <-
  bind_rows(birthplace_2001 %>% mutate(year = 2001),
            birthplace_2006 %>% mutate(year = 2006),
            birthplace_2011 %>% mutate(year = 2011))

# Make long
birthplace <- birthplace %>%
  gather(key, value, total_15_up_anywhere:female_25_29_abroad)

birthplace_by_vismin_2011 <- birthplace_by_vismin_2011 %>%
  gather(key, value,  total_15_19_856955_253390:outside_15_29_percent_22.7_58.9)


# function to creat sex, age_group, and place columns
create_age_gender_birthplace <- function(x, has_sex) {
  # Separate age and sex
  if(has_sex) {
    x$sex <- ifelse(grepl('female', x$key), 'female',
                    ifelse(grepl('male', x$key), 'male', 'total'))
  }

  x$age_group <- unlist(lapply(strsplit(x$key, split = '_'), function(x){paste0(x[2], '_', x[3])}))
  x$place <-   ifelse(grepl('canada', x$key), 'canada',
                      ifelse(grepl('abroad', x$key), 'abroad', 'anywhere'))
  return(x)
}

# apply to birthplace data, normal and vismin
birthplace <- create_age_gender_birthplace(birthplace, has_sex = T)
birthplace_by_vismin_2011 <- create_age_gender_birthplace(birthplace_by_vismin_2011, has_sex = F)

# get subset age_sex - with 4 digit codes and ontario
birthplace <- clean_geo_key(birthplace)

# remove the other birthplace stuff
rm(birthplace_2001, birthplace_2006, birthplace_2011)

# remove key from data
birthplace$key <- NULL
birthplace_by_vismin_2011$key <- NULL

rm(column_names, full_data, special_indicators_2001, special_indicators_2006, special_indicators_2011,
   special_indicators_by_vismin_2011)
rm(vismin_2006, vismin_2011)

# save.image('~/Desktop/temp_global.RData')
# load('~/Desktop/temp_global.RData')


# # special indicators
# clean_up_special_indicators <- function(df){
#   out <- df[, !duplicated(colnames(df))]
#   out <- out %>%
#     gather(key, value, -Geography)
#   colnames(out) <- tolower(colnames(out))
#
#   return(out)
# }
#
# # get new long format data
# special_indicators_2001 <- clean_up_special_indicators(special_indicators_2001)
# special_indicators_2006 <- clean_up_special_indicators(special_indicators_2006)
# special_indicators_2011 <- clean_up_special_indicators(special_indicators_2011)
#
# # combine special indicator data and creat year variable
# special_indicators <-
#   bind_rows(
#     special_indicators_2001 %>% mutate(year = 2011),
#     special_indicators_2006 %>% mutate(year = 2006),
#     special_indicators_2011 %>% mutate(year = 2011)
#   )
#
# # get subset age_sex - with 4 digit codes and ontario
# special_indicators <- clean_geo_key(special_indicators)
#
# rm(special_indicators_2001, special_indicators_2006, special_indicators_2011)
# # # create new columns based on key variable
# # names(special_indicators)
# # temp <- as.data.frame(unique(special_indicators$key))
#
# # special inidcators has quite a bit of info, so im just gona choose one category for the purpose of the template.
# # background - possible sub cateogries to divide data set into
# # look at temp to find sub categories
# # 1) population, 2) marriage (widowed, divorced, never married), 3) language, 4) population,
# # 5) migration (movers, migrators, non movers),
# # 6) Total - Census family status, persons in census, family (spouses, kids, parents, single parents, living with relatives),
# # 7) Education - university, certificates,
# # 8) Employment -
# # 9) worker class
# # 10) household income - std err, low income status,
# # 11) housing deatails - owner occupied dwelling,
#
# # how many duplicates
# length(which(duplicated(special_indicators$geography)))
# length(which(duplicated(paste0(special_indicators$geography, special_indicators$key, special_indicators$year))))
#
# ###### CONTINUE
# # get information on household income and marriage status
# # column names with marriage and employment
# married_employment_strings <- 'marital|married|Divorced|Widowed|Employed|Unemployed|Unemployment|Participation'
# temp_dat <- special_indicators[grepl(married_employment_strings, special_indicators$key),]
#
# # creat new columns off of key variable
# temp <- as.data.frame(unique(temp_dat$key))
#
# # ages range from 15-19, 15-24, 20-24, 25-29, 15 years and over
#
# # for gender - Male, Female, Total, Male Total, Female Total.
# temp_dat$gender <- ifelse(grepl('Male|male', temp_dat$key), 'M',
#                           ifelse(grepl('Female|female', temp_dat$key), 'F', 'Total'))
#
# # for age range
# temp_dat$age <-  ifelse(grepl('15 years and', temp_dat$key), '15_up',
#                         ifelse(grepl('15 to 24', temp_dat$key), '15_24',
#                                ifelse(grepl('15 to 19', temp_dat$key), '15_19',
#                                       ifelse(grepl('20 to 24', temp_dat$key), '20_24',
#                                              ifelse(grepl('25 to 29', temp_dat$key), '25_29', 'no_age')))))
#
# # look at unique places


# Define a function for creating a crazy looking map
crazy_map <- function(){
  # Add some colors
  lots_of_colors <- c(rainbow(100), grey(seq(0.001, 0.999, length = 100)))
  ont_crazy_colors <- sample(lots_of_colors, nrow(ont_crazy))
  plot(ont_crazy, col = ont_crazy_colors)
}
