

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
    if(year == 2001){
      temp_data <- temp_data[,!grepl('school part time|school full time', names(temp_data))]
      names_2001 <- names(temp_data)
    } else if (year == 2006){
      temp_data <- temp_data[,!grepl('after tax', names(temp_data))]
      names(temp_data) <- names_2001
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
