

# data_type = 'nhs'
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
  return(data_list)
}

temp_census <- get_census_data()

census <- bind_rows(temp_census)

# read in dictionary 
census_dict <- read_csv('dictionaries/census_dictionary.csv')
