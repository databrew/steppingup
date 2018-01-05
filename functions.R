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
library(memisc)

# data <- as.data.set(spss.system.file('filename.sav'))


# Define function for reading survey data
get_survey_data <- function() {
  path_to_data <- 'data/survey_data'
  var_summary <- read_csv(paste0(path_to_data, '/var_summary.csv'))
  
  removals <- c()
  var_names <- as.character(var_summary$variable_name)
  survey_folders <- list.files(path_to_data)
  # remove var_summary.csv from the list so that there are 10 unique folders pertaining to each survey
  survey_folders <- survey_folders[!grepl('var_summary', survey_folders)]
  # create list to store results
  result_list <- list()

  # loop through each folder and read in all data in that folder (either 1 or 3)
  for(i in 1:length(survey_folders)) {
    message('Starting ', i, ': ', survey_folders[i])
    temp_folder <- survey_folders[i]
    survey_data <- list.files(paste(path_to_data, temp_folder, sep = '/'))
    data_list <- list()
    lsd <- length(survey_data)
    message('--- There are ', lsd, ' sub datasets')
    for(j in 1:lsd) {
      message('------ Working on ', j, ' of ', lsd)
      temp_data <- survey_data[j]
      if (grepl('.sav', temp_data)) {
        temp_dat <- read.spss(file = paste(path_to_data,
                                           temp_folder,
                                           temp_data, sep = '/'),
                              use.value.labels = T,
                              to.data.frame = T,
                              trim.factor.names = T,
                              trim_values = F,
                              use.missings = FALSE)
        
        if(grepl('gss|piaac|cfcs', temp_data)) {
          get_year = T
        } else {
          get_year = F
        }
       
        # get long for variable names
        colnames(temp_dat) <- attr(temp_dat,"variable.labels")
        
        # get the column names we want from are varibale list
        temp_sub <- clean_subset_survey(temp_dat, get_year = get_year, folder = temp_folder)
        
        # get subsetted by variables names
        temp_sub <- data.frame(temp_sub[, colnames(temp_sub)[colnames(temp_sub) %in% var_names]])
        
        # remove in column name that has .1 in it because its a duplicate 
        temp_sub <- temp_sub[,!grepl('.1', colnames(temp_sub), fixed = TRUE)]
        
        # clean data - don't recode variable names because the current ones are linked to a data dictionary 
        # clean by recoding factors or numerics (bare minimum right now)
        if(grepl('lfs', temp_data)) {
          
          temp_sub <- clean_lfs(temp_sub)
          
        } else if (grepl('gss_2010', temp_data)) {
          
          temp_sub <- clean_gss10(temp_sub)
          
        } else if (grepl('gss_2010_1|gss_2012_1', temp_data)) {

          temp_sub <- temp_sub[grepl('15 to 17|18 to 19|20 to 24|25 to 29', 
                                      temp_sub$age_group_of_the_respondent_groups_of_5),]
          
        } else if (grepl('gss_2010_2', temp_data)) {
          
          temp_sub <- temp_sub[grepl('15 to 17|18 to 19|20 to 24|25 to 29', 
                                      temp_sub$age_group_of_the_respondent),]
          
        } else if (grepl('gss_2011', temp_data)) {
          temp_sub <- temp_sub[grepl('15 to 17|18 to 19|20 to 24|25 to 29', 
                                     temp_sub$age_group_of_r_grps_of_5),]
          
        } else if (grepl('piaac', temp_data)) {
          temp_sub <- temp_sub[grepl('24 or less|25-34', 
                                      temp_sub$age_in_10_year_bands_derived),]
          
        } else if (grepl('gss_2013|gss_2014', temp_data)) {
          
          # make weight numeric
          temp_sub$person_weight <- as.numeric(as.character(temp_sub$person_weight))
          
          temp_sub <- temp_sub[grepl('15 to 24|25 to 34', 
                                      temp_sub$age_group_of_respondent_groups_of_10),]

        } else if (grepl('cfcs_1', temp_data)) {
          
          temp_sub <- temp_sub[grepl('18 to 24|25 to 34', 
                                      temp_sub$age_of_respondent_grouped),]
          
        } else if (grepl('eics_1', temp_data)) {
          
          temp_sub <- temp_sub[!grepl('15-24 years', 
                                      temp_sub$age_of_respondent_groups),]
        } else if (grepl('sduhs', temp_data)) {
          
          # restructure data
          temp_sub <- restructure_data_types(temp_sub)
          
          # make first letter capital
          # temp_sub <- get_capital_osduhs(temp_sub)
          
          # get date, and date and time of start and finish
          temp_sub <- get_date_and_time_osduhs(temp_sub)
          
          # recode body weight variable
          temp_sub <- get_body_weight_osduhs(temp_sub)
          
          # Need to combine all race variables into one
          temp_sub <- temp_sub %>%
            mutate(race = ifelse(white_which_of_the_following_best_describes_your_background == 'yes',
                                 'chite',
                                 ifelse(chinese_which_of_the_following_best_describes_your_background == 'yes',
                                        'whinese',
                                        ifelse(south_asian_which_of_the_following_best_describes_your_background == 'yes',
                                               'south asian',
                                               ifelse(black_which_of_the_following_best_describes_your_background == 'yes',
                                                      'black',
                                                      ifelse(aboriginalfirst_nations_which_of_the_following_best_describes_your_background == 'yes',
                                                             'aboriginal',
                                                             ifelse(filipino_which_of_the_following_best_describes_your_background == 'yes',
                                                                    'filipino',
                                                                    ifelse(southeast_asian_which_of_the_following_best_describes_your_background == 'yes',
                                                                           'southeast asian',
                                                                           ifelse(west_asian_or_arab_which_of_the_following_best_describes_your_background == 'yes',
                                                                                  'west asian / arab',
                                                                                  ifelse(korean_which_of_the_following_best_describes_your_background == 'yes',
                                                                                         'Korean',
                                                                                         ifelse(japanese_which_of_the_following_best_describes_your_background == 'yes',
                                                                                                'japanese',
                                                                                                ifelse(not_sure_which_of_the_following_best_describes_your_background == 'yes',
                                                                                                       'not sure', 
                                                                                                       NA))))))))))))
 
          temp_sub <- temp_sub[,!grepl('which_of_the_following_best_describes_your_background', names(temp_sub))]
        }
        
        new_names <- data.frame(variable_name = names(temp_sub),
                                data_name = temp_folder)
        new_names <- left_join(new_names, var_summary)
        names(temp_sub) <- new_names$new_variable
        data_list[[j]] <-  temp_sub
      }
    }
    
    if(length(data_list) > 1) {
      the_data <- left_join(data_list[[2]],
                          data_list[[1]])
    } else {
      the_data <- data_list[[1]]
    }
    
    # Remove any variables which are 100% NA
    flags <- rep(FALSE, ncol(the_data))
    for(j in 1:length(flags)){
      all_na <- length(which(is.na(the_data[,j]))) == nrow(the_data)
      flags[j] <- all_na
    }
    how_many_flags <- length(which(flags))
    if(how_many_flags > 0){
      message('Removing the following ', how_many_flags, ' variables since they are all NA:\n')
      these_removals <- names(the_data)[flags]
      message(paste0('---', these_removals, collapse = '\n'))
      removals <- c(removals, these_removals)
    }
    the_data <- the_data[,which(!flags)]
    
    result_list[[i]] <- the_data
    names(result_list)[i] <- temp_folder
    message('Done with ', survey_folders[i])
  }
  return(list(result_list, removals))
}


# Define a function for creating a crazy looking map
crazy_map <- function(){
  # Add some colors
  lots_of_colors <- c(rainbow(100), grey(seq(0.001, 0.999, length = 100)))
  ont_crazy_colors <- sample(lots_of_colors, nrow(ont_crazy))
  plot(ont_crazy, col = ont_crazy_colors)
}

# Function for a time chart
time_chart <- function(x,y,
                       ylab = '',
                       fill = TRUE){
  require(ggplot2)
  df <- data.frame(x,y)
  g <-
    ggplot(data = df,
           aes(x = x,
               y = y)) +
    geom_line(alpha = 0.6,
              color = '#0d63c4') +
    geom_point(alpha = 0.6,
               color = '#0d63c4') +
    theme_databrew() +
    labs(x = 'Date',
         y = ylab)
  if(fill){
    g <-
      g +
      geom_area(fill = '#0d63c4',
                alpha = 0.3)
  }
  return(g)
}

# Function for map
ontario_map <- function(x){
  require(dplyr)
  require(ggplot2)
  # This function expects "x" to be a dataframe with a column named "geography"
  # and another named "value"
  # Keep only the numbered values
  right <- x %>%
    filter(!is.na(as.numeric(geography))) %>%
    mutate(geography_name = geography) %>%
    mutate(geography = substr(geography, 3,4))
  # join to ont fortified
  shp <- ont_fortified
  shp <- shp %>%
    left_join(right,
              by = 'geography')
  # Make a plot
  g <-
    ggplot(data = shp,
           aes(x = long,
               y = lat,
               group = group,
               fill = value)) +
    geom_polygon() +
    coord_map() +
    theme_databrew() +
    scale_fill_continuous(low = 'lightblue', high = 'darkorange', name = '', na.value = 'white') +
    theme(legend.text = element_text(size = 7),
          legend.position = 'right') +
    labs(x = '',
         y = '')
  return(g)
}
# # Example
# df <- census_all %>%
#   filter(year == 2011,
#          age == '15 to 19 years',
#          sex == 'Male',
#          pob == 'Born in Canada',
#          vm == 'Chinese',
#          si == 'Never married (single) 15 years and over') %>%
#   group_by(geography) %>%
#   summarise(value = sum(value))
# ontario_map(x = df)

# Define function for generating a leaflet plot

# Create a basic leaflet with nothing else on it
leaf_basic <- function(shp = ont2){
  tile = 'OpenStreetMap.Mapnik'
  palette = 'Purples'
  l <- leaflet(data = shp) %>%
    addPolylines(color = NA, opacity = 0.5, weight = 0.2) %>%
    addProviderTiles(tile,
                     options = providerTileOptions(minZoom = 4, maxZoom = 10)) 
  return(l)
}

leaf <- function(x, 
                 tile = 'OpenStreetMap.Mapnik', 
                 palette = 'Purples',
                 show_legend = TRUE,
                 title = NULL){
  require(dplyr)
  require(leaflet)
  require(RColorBrewer)
  # This function expects "x" to be a dataframe with a column named "geo_code" (a 4 character string)
  # and another named "value"
  # Keep only the numbered values
  right <- x %>%
    filter(!is.na(as.numeric(geo_code))) %>%
    mutate(geography = substr(geo_code, 3,4))
  # join to ont shapefile
  shp <- ont2
  shp@data <- shp@data %>%
    mutate(geography = CCA_2) %>%
    left_join(right,
              by = 'geography')

  # Create a color palette
  # pal <- colorQuantile("Blues", NULL, n = 9)
  # bins <- round(c(quantile(shp@data$value, na.rm = TRUE), Inf))
  bins <- unique(round(c(quantile(shp@data$value, na.rm = TRUE, c(seq(0, 1, 0.15), 1)))))
  pal <- colorBin(palette, domain = shp@data$value, bins = bins,
                  na.color = NA)

  # Create a popup
  popper <- paste0(shp@data$NAME_2, ': ',
                   round(shp@data$value, 2))

  # Create map
  l <- leaf_basic(shp = shp)
  # l <- leaflet(data = shp) %>%
  #   addProviderTiles(tile)
  if(show_legend){
    l <- l %>%
      addLegend(pal = pal, values = ~value, opacity = 0.7, 
                position = "bottomleft",
                title = title)
  }
  l <- l %>%
    addPolygons(fillColor = ~pal(value),
                fillOpacity = 0.8,
                color = "#BDBDC3",
                weight = 1,
                # popup = popper,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = popper,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", 
                               padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))
  return(l)
}

# Define function for printing nice html tables
prettify <- function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE,
                      cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y",
                      round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE,
                      data_table = TRUE, nrows = 5, download_options = FALSE, no_scroll = TRUE){
  column_names <- names(the_table)
  the_table <- data.frame(the_table)
  names(the_table) <- column_names
  classes <- lapply(the_table, function(x) {
    unlist(class(x))[1]
  })
  if (cap_columns) {
    names(the_table) <- Hmisc::capitalize(names(the_table))
  }
  if (remove_underscores_columns) {
    names(the_table) <- gsub("_", " ", names(the_table))
  }
  for (j in 1:ncol(the_table)) {
    the_column <- the_table[, j]
    the_class <- classes[j][1]
    if (the_class %in% c("character", "factor")) {
      if (cap_characters) {
        the_column <- as.character(the_column)
        the_column <- Hmisc::capitalize(the_column)
      }
      if (remove_line_breaks) {
        the_column <- gsub("\n", " ", the_column)
      }
    }
    else if (the_class %in% c("POSIXct", "Date")) {
      the_column <- format(the_column, format = date_format)
    }
    else if (the_class %in% c("numeric", "integer")) {
      the_column <- round(the_column, digits = round_digits)
      if (comma_numbers) {
        if(!grepl('year', tolower(names(the_table)[j]))){
          the_column <- scales::comma(the_column)
        }
      }
    }
    the_table[, j] <- the_column
  }
  if (remove_row_names) {
    row.names(the_table) <- NULL
  }
  if (data_table) {
    if (download_options) {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          dom = "Bfrtip", buttons = list("copy", "print",
                                         list(extend = "collection", buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
          # scrollY = '300px', paging = FALSE,
          dom = "Bfrtip", buttons = list("copy", "print",
                                         list(extend = "collection", buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      }
      
    }
    else {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
                                                             columnDefs = list(list(className = "dt-right",
                                                                                    targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             columnDefs = list(list(className = "dt-right",
                                                                                    targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      }
    }
  }
  return(the_table)
}


# Define function for printing nice html tables
prettify_scroll <- function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE,
                      cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y",
                      round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE,
                      data_table = TRUE, nrows = 5, download_options = FALSE, no_scroll = TRUE,
                      scroll_x = TRUE){
  column_names <- names(the_table)
  the_table <- data.frame(the_table)
  names(the_table) <- column_names
  classes <- lapply(the_table, function(x) {
    unlist(class(x))[1]
  })
  if (cap_columns) {
    names(the_table) <- Hmisc::capitalize(names(the_table))
  }
  if (remove_underscores_columns) {
    names(the_table) <- gsub("_", " ", names(the_table))
  }
  for (j in 1:ncol(the_table)) {
    the_column <- the_table[, j]
    the_class <- classes[j][1]
    if (the_class %in% c("character", "factor")) {
      if (cap_characters) {
        the_column <- as.character(the_column)
        the_column <- Hmisc::capitalize(the_column)
      }
      if (remove_line_breaks) {
        the_column <- gsub("\n", " ", the_column)
      }
    }
    else if (the_class %in% c("POSIXct", "Date")) {
      the_column <- format(the_column, format = date_format)
    }
    else if (the_class %in% c("numeric", "integer")) {
      the_column <- round(the_column, digits = round_digits)
      if (comma_numbers) {
        if(!grepl('year', tolower(names(the_table)[j]))){
          the_column <- scales::comma(the_column)
        }
      }
    }
    the_table[, j] <- the_column
  }
  if (remove_row_names) {
    row.names(the_table) <- NULL
  }
  if (data_table) {
    if (download_options) {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          scrollX = scroll_x,
          dom = "Bfrtip", buttons = list("copy", "print",
                                         list(extend = "collection", buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             scrollX = scroll_x,
                                                             # scrollY = '300px', paging = FALSE,
                                                             dom = "Bfrtip", buttons = list("copy", "print",
                                                                                            list(extend = "collection", buttons = "csv",
                                                                                                 text = "Download"))), rownames = FALSE, extensions = "Buttons")
      }
      
    }
    else {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          scrollX = scroll_x,
          columnDefs = list(list(className = "dt-right",
                                 targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             scrollX = scroll_x,
                                                             columnDefs = list(list(className = "dt-right",
                                                                                    targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      }
    }
  }
  return(the_table)
}

# Define function for subsetting tables
subset_table <- function(data = census_all,
                        geo_code = NULL,
                        year = NULL,
                        age = NULL,
                        sex = NULL,
                        pob = NULL,
                        vm = NULL,
                        si = NULL){
  # Create data
  sub_data <- data

  # Modify param names
  the_geo_code = geo_code
  the_year = year
  the_age = age
  the_sex = sex
  the_pob = pob
  the_vm = vm
  the_si = si

  # Empty vector of groupers
  groupers <- c()

  if(!is.null(geo_code)){
    sub_data <- sub_data %>% dplyr::filter(geo_code == the_geo_code)
  } else {
    groupers <- c(groupers, 'geo_code')
  }
  if(!is.null(year)){
    sub_data <- sub_data %>% dplyr::filter(year == the_year)
  } else {
    groupers <- c(groupers, 'year')
  }
  if(!is.null(age)){
    sub_data <- sub_data %>% dplyr::filter(age == the_age)
  } else {
    groupers <- c(groupers, 'age')
  }
  if(!is.null(sex)){
    sub_data <- sub_data %>% dplyr::filter(sex == the_sex)
  } else {
    groupers <- c(groupers, 'sex')
  }
  if(!is.null(pob)){
    sub_data <- sub_data %>% dplyr::filter(pob == the_pob)
  } else {
    groupers <- c(groupers, 'pob')
  }
  if(!is.null(vm)){
    sub_data <- sub_data %>% dplyr::filter(vm == the_vm)
  } else {
    groupers <- c(groupers, 'vm')
  }
  if(!is.null(si)){
    sub_data <- sub_data %>% dplyr::filter(si == the_si)
  } else {
    groupers <- c(groupers, 'si')
  }

  # Apply groupers
  if(length(groupers) > 0){
    sub_data <-
      sub_data %>%
      group_by_(groupers) %>%
      summarise(value = sum(value))
  }

  return(sub_data)
}



# temp <- temp_dat
clean_subset_survey <- function(temp, get_year, folder) {

  Encoding(colnames(temp)) <- 'latin1'
  colnames(temp) <- tolower(colnames(temp))
  # clean cols
  colnames(temp) <- gsub("[[:punct:]]", '', as.character(colnames(temp)))
  colnames(temp) <- trimws(colnames(temp), which = 'both')
  colnames(temp) <- gsub("[[:space:]]", '_', as.character(colnames(temp)))
  colnames(temp) <- gsub("__", '_', as.character(colnames(temp)), fixed = T)

  # remove any whitespaces left over
  temp <- as.data.frame(temp, stringsAsFactors = F)

  if(get_year) {
    year <- unlist(lapply(strsplit(folder, split = '_',  fixed = T), function(x) x[1]))
    temp$year <- year
  }

  return(temp)
}



# define function for filtering data by inputs
# year Numeric vector of length 1 or 3. For example, c(2001, 2006) to keep data from both years
censify <- function(df = census,
                    dict = census_dict,
                    age = FALSE, 
                    sex = FALSE,
                    pob = FALSE,
                    vm = FALSE, 
                    ai = FALSE,
                    geo_code = FALSE,
                    years = 2001,
                    sc = NULL,
                    percent = 'Percentage') { # one of Percentage, Raw Numbers, or Both
  # category
  if(!is.null(sc)) {
    if(!sc %in% unique(dict$sub_category)) {
      stop("sc must be one of ", paste0(unique(dict$sub_category), collapse = ", "))
    }
    # extract columns corresponding to category from census dict
    keep_columns <- dict %>%
      filter(sub_category %in% c('demographic', 'geo_code', 'year', sc)) %>%
      .$variable
    df <- df[, keep_columns]
  }
  
  # age 
  if(age) {
    df <- df %>%
      filter(!grepl('Total', `Age group`))
  } else {
    df <- df %>%
      filter(grepl('Total', `Age group`))
    df$`Age group` <- NULL
  }
  
  # sex
  if(sex){
    df <- df %>%
      filter(!grepl('Total', Sex))
  } else {
    df <- df %>%
      filter(grepl('Total', Sex))
    df$Sex <- NULL
  }
  
  # pob
  if(pob){
    df <- df %>%
      filter(!grepl('Total', `Place of birth`))
  } else {
    df <- df %>%
      filter(grepl('Total', `Place of birth`))
    df$`Place of birth` <- NULL
  }
  
  # vm
  if(vm){
    df <- df %>%
      filter(!grepl('Total', `Visible minority`))
  } else {
    df <- df %>%
      filter(grepl('Total',  `Visible minority`))
    df$`Visible minority` <- NULL
  }
  
  # ai
  if(ai){
    df <- df %>%
      filter(!grepl('Total', `Aboriginal identity`))
  } else {
    df <- df %>%
      filter(grepl('Total',  `Aboriginal identity`))
    df$`Aboriginal identity` <- NULL
  }
  
  # geo_code
  if(geo_code){
    df <- df %>%
      filter(geo_code != '3500')
  } else {
    df <- df %>%
      filter(geo_code == '3500')
    df$geo_code <- NULL
    df$Geography  <- NULL
  }
  
  # filter for year
  df <- df %>%
    filter(year %in% years)
  # if(length(years) == 1){
  #   df$year <- NULL
  # }
  
  # make percentages 
  if(percent %in% c('Percentage', 'Both')) {
    if(is.null(sc)) {
      warning("Choose a sub category to generate percentages")
    } else {
      if(!sc %in% c('income', 'population')){
        denom_column <- which(grepl('Total', names(df)))
        denom <- df[, denom_column]
        names(denom) <- 'x'
        denom <- denom$x
        
        # identify indices of numerator columns
        ni <- (denom_column + 1):ncol(df)
        
        # make temp data frame 
        col_names <- names(df)
        
        df <- as.data.frame(df)
        
        if(percent == 'Percentage'){
          for(j in ni) {
            df[,j] <- (df[, j]/denom)*100
          }
        } else if(percent == 'Both'){
          for(j in ni) {
            n <- df[,j]
            p <- (df[, j]/denom)*100
            df[,j] <- paste0(prettyNum(n, big.mark = ','), ' (',
                             round(p, 2), '%)')
          }
        }
        
        colnames(df) <- col_names
      }
    }
  }
  total_columns <- grepl('Total', names(df))
  if(length(which(total_columns)) == 1){
    names(df)[total_columns] <- 'Total' 
  }
  return(df)
}

# Define function for plotting between 1 and 3 variables
plotter <- function(df, variable = NULL, show_labels = TRUE){
  # Skim down the columns to only keep the grouper ones and the plotting variable
  df <- df[,names(df) %in% c(head_vector, variable)]
  no_year <- FALSE
  if(length(variable) != 1){
    too_many_variables <- TRUE
  } else {
    too_many_variables <- FALSE
  }
  
  all_nas <- FALSE
  if(length(variable) == 1){
    which_var <- which(names(df) == variable)
    val <- df[, which_var]
    if(all(is.na(val))){
      all_nas <- TRUE
    }
  }
  if(all_nas){
    the_title <- paste0('There is no data available for ', variable, ' for the year(s) selected.')
    return(ggplot() +
             theme_databrew() +
             labs(title = the_title))
  } else {

    one_year <- FALSE
    if ('year' %in% names(df)) {
      df$year <- as.factor(df$year)
      if(length(unique(df$year)) == 1){
        one_year <- TRUE
      }
    } 
    if(nrow(df) == 0){
      no_year <- TRUE
    }
    df <- df[,names(df) != 'geo_code']
    total_column <- which(grepl('Total', names(df)))[1]
    df <- df[,-total_column]
    # Identify the column to be plotted
    plot_column <- ncol(df)#  total_column + 1
    original_y <- names(df)[plot_column]
    names(df)[plot_column] <- 'y'
    # Only keep necessary columns
    df <- df[,1:plot_column]
    if(
      (!one_year & ncol(df) > 4) |
      (one_year & ncol(df) > 5) | 
      no_year | 
      too_many_variables){
      st <- ''
      if(no_year){
        st <- 'You must select at least one year'
      } else {
        st <- 'Variables can be Age group, Sex, Place of birth, Visible minority, Geography and Year(s)'
      }
      if(too_many_variables){
        the_title <- 'Pick only one variable in the upper right to view a plot'
      } else {
        the_title <- 'Pick between 0 and 3 sub-groups to view a plot'
      }
      
      return(ggplot() +
               theme_databrew() +
               labs(title = the_title,
                    subtitle = st))
    } else {
      if(one_year & ncol(df) > 2){
        df$year <- NULL
        plot_column <- plot_column - 1
      }
      original_var_names <- names(df)[1:(plot_column - 1)]
      # if('year' %in% original_var_names){
      #   if(length(unique(df$year)) == 1){
      #     df <- df[,names(df) != 'year']
      #     original_var_names <- original_var_names[original_var_names != 'year']
      #   }
      # }
      names(df)[1:(plot_column - 1)] <- paste0('var', 1:(plot_column - 1))
      # Define how many plot variables there are
      plot_variables <- plot_column - 1
      # Make a plot 
      g <- ggplot(data = df,
                  aes(x = var1,
                      y = y)) +
        geom_bar(stat = 'identity',
                 fill = 'darkorange',
                 alpha = 0.6) 
      if(show_labels){
        g <- g +
          geom_text(aes(label = round(y, digits = 2)), alpha = 0.4,
                    position = position_dodge(width = 1))
      }
      if(plot_variables == 2){
        cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(df$var2)))
        if(length(cols) == 2){
          cols <- c('darkorange', 'lightblue')
        }
        g <- ggplot(data = df,
                    aes(x = var1,
                        y = y, 
                        group = var2,
                        fill = var2)) +
          geom_bar(stat = 'identity', position = 'dodge')  +
          scale_fill_manual(name = '',
                            values = cols) 
        if(show_labels){
          g <- g +
            geom_text(aes(label = round(y, digits = 2)), alpha = 0.4,
                      position = position_dodge(width = 1))
        }
      }
      if(plot_variables == 3){
        cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(df$var3)))
        if(length(cols) == 2){
          cols <- c('darkorange', 'lightblue')
        }
        g <- ggplot(data = df,
                    aes(x = var1,
                        y = y, 
                        group = var3,
                        fill = var3)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          scale_fill_manual(name = '',
                            values = cols) +
          facet_wrap(~var2)
        if(show_labels){
          g <- g +
            geom_text(aes(label = round(y, digits = 2)), alpha = 0.4,
                      position = position_dodge(width = 1))
        }
      }
    }
    g <- g +
      theme_databrew() +
      theme(axis.text.x = element_text(angle = 45,
                                       hjust = 1)) 
    title <- paste0(original_y, ' by ', original_var_names[1])
    if(plot_variables == 2){
      title <- paste0(title, ' and ', original_var_names[2])
    }
    if(plot_variables == 3) {
      title <- paste0(title, ', ', original_var_names[2], ', and ', original_var_names[3])
    }
    
    g <- g +
      labs(x = '',
           title = title, 
           y = '')
    if('Geography' %in% original_var_names){
      which_geo <- which(grepl('eograph', original_var_names))
      geo_var <- paste0('var', which_geo)
      lots <- length(unique(df[,geo_var])) > 10
      if(lots){
        if(plot_variables == 3){
          g <- 
            g + theme(axis.text.x = element_text(size = 6))
        } else {
          g <- 
            g + theme(axis.text.x = element_text(size = 8))
        }
        
      }
    }
    return(g)
  }
}

# taking factors with one level in osduhs survey data and imputing 'NO' onto it
relevel_factor_one_lfs <- function(dat_var) {
  dat_var <- as.character(dat_var)
  dat_var[is.na(dat_var)] <- 'NO'
  return(dat_var)
}

# restructure data types
restructure_data_types <- function(temp) {
  for(i in 1:ncol(temp)) {
    if(grepl('factor', class(temp[, i]))) {
      temp[, i] <- as.character(temp[, i])
    } else {
      temp[, i] <- as.numeric(temp[, i])
    }
  }
  return(temp)
}

clean_lfs <- function(temp_clean) {
  
  # turn factors to characters, and everything else numeric
  temp_clean <- restructure_data_types(temp_clean)

  # if the level of a factor is 1, then that factor only has "yes" coded and should replace NA with "NO"
  temp_clean$job_seeker_checked_wemployers_directly <- relevel_factor_one_lfs(temp_clean$job_seeker_checked_wemployers_directly)
  temp_clean$job_seeker_checked_wemployment_agency <- relevel_factor_one_lfs(temp_clean$job_seeker_checked_wemployment_agency)
  temp_clean$jobseeker_contacted_relatives <- relevel_factor_one_lfs(temp_clean$jobseeker_contacted_relatives)
  temp_clean$jobseeker_looked_at_ads <- relevel_factor_one_lfs(temp_clean$jobseeker_looked_at_ads)
  temp_clean$jobseeker_placed_or_answered_ads <- relevel_factor_one_lfs(temp_clean$jobseeker_placed_or_answered_ads)
  temp_clean$jobseeker_other_methods <- relevel_factor_one_lfs(temp_clean$jobseeker_other_methods)
  
  # filter to get ontario (should have done this earlier, but missed it)
  temp_clean <- temp_clean %>% filter(province == 'Ontario')
  
  # recode variables that have sloppy coding - We checked and NAs literally mean the question was not appicable to that person. 
  # Real NAs - actual missing values - were deleted prior to them giving us this data (from carlton university). This is documented in 
  # data documentation under the Labour Force Survey.
  temp_clean$not_currently_employed_worked_in_past <- ifelse(grepl('within', temp_clean$not_currently_employed_worked_in_past), 
                                                             'Yes within last year',
                                                             ifelse(grepl('>1', temp_clean$not_currently_employed_worked_in_past), 
                                                                    'Yes greater than 1 year', 
                                                                    ifelse(grepl('never', temp_clean$not_currently_employed_worked_in_past),
                                                                           'No never worked', 'Not applicable')))
  
  temp_clean$full_or_parttime_status_of_last_job <- ifelse(grepl('Part', temp_clean$full_or_parttime_status_of_last_job),
                                                           'Part time (1 to 29 hours)',
                                                           ifelse(grepl('Full', temp_clean$full_or_parttime_status_of_last_job), 
                                                                  'Full time (30+)', 'Not applicable'))
  
  # recode variables with too many levels or ones that can be summarized with less information
  temp_clean$class_of_worker_main_job <- ifelse(grepl('unpaid', temp_clean$class_of_worker_main_job), 
                                                'Unpaid family work', 
                                                ifelse(grepl('0|no', temp_clean$class_of_worker_main_job),
                                                       'Self employed, no employees',
                                                       ifelse(grepl('/w/', temp_clean$class_of_worker_main_job),
                                                              'Self employed, with employees',
                                                              ifelse(grepl('Private', temp_clean$class_of_worker_main_job), 
                                                                     'Private employee', 
                                                                     ifelse(grepl('Public', temp_clean$class_of_worker_main_job),
                                                                            'Publice employee', 'Not applicable')))))
  
  temp_clean$industry_of_main_job_naics_200718 <- ifelse(grepl('Utilities|Construction|Manufact', as.character(temp_clean$industry_of_main_job_naics_200718)),
                                                         'Utilities/Construction/Manufacturing',
                                                         ifelse(grepl('Trade', as.character(temp_clean$industry_of_main_job_naics_200718)), 
                                                                'Trade Retail/Whosale',
                                                                ifelse(grepl('Accomm/Food Services|Other Services', as.character(temp_clean$industry_of_main_job_naics_200718)),
                                                                       'Accommodation/Food/Other Services',
                                                                       ifelse(grepl('Agric|Forest', as.character(temp_clean$industry_of_main_job_naics_200718)),
                                                                              'Agriculture/Forest/Fish/Mine/Oil&Gas', as.character(temp_clean$industry_of_main_job_naics_200718)))))
  
  temp_clean$fulltime_or_parttime_main_or_only_job <- ifelse(grepl('Full', temp_clean$fulltime_or_parttime_main_or_only_job), 
                                                             'Full time (30+)',
                                                             ifelse(grepl('Part', temp_clean$fulltime_or_parttime_main_or_only_job), 
                                                                    'Part time (1 to 29 hours)','Not applicable'))
  
  # is it better to recode them all or create new variables that are just true of false for type of couple, employeds, kids etc?
  temp_clean$type_of_economic_family <- as.character(temp_clean$type_of_economic_family)
  temp_clean$type_of_economic_family <- ifelse(temp_clean$type_of_economic_family == 'H-W:2earn,0 kids<25',
                                               'Husband Wife: both employeds, no kids < 25',
                                               ifelse(temp_clean$type_of_economic_family == 'H-W:2earn, kids<18',
                                                      'Husband Wife: both employeds, kids < 18',
                                                      ifelse(temp_clean$type_of_economic_family == 'H-W:2earn,kids18-24',
                                                             'Husband Wife: both employeds, kids 18 - 24',
                                                             ifelse(temp_clean$type_of_economic_family == 'H-W:H empl,0 kids<25',
                                                                    'Husband Wife: husband employed, no kids < 25',
                                                                    ifelse(temp_clean$type_of_economic_family == 'H-W:H empl,kids<18', 
                                                                           'Husband Wife: husband employed, kids < 18',
                                                                           ifelse(temp_clean$type_of_economic_family == 'H-W:H empl,kids18-24',
                                                                                  'Husband Wife: husband employed, kids 18 - 24',
                                                                                  ifelse(temp_clean$type_of_economic_family == 'H-W:W empl,0 kids<25',
                                                                                         'Husband Wife: wife employed, no kids < 25',
                                                                                         ifelse(temp_clean$type_of_economic_family == 'H-W:W empl,kids<18', 
                                                                                                'Husband Wife: wife employed, kids < 18',
                                                                                                ifelse(temp_clean$type_of_economic_family == 'H-W:W empl,kids18-24',
                                                                                                       'Husband Wife: wife employed, kids 18 - 24',
                                                                                                       ifelse(temp_clean$type_of_economic_family == 'H-W:non-earn,0kid<25',
                                                                                                              'Husband Wife: no employed, no kids < 25',
                                                                                                              ifelse(temp_clean$type_of_economic_family == 'H-W:non-earn,kids<18', 
                                                                                                                     'Husband Wife: no employed, kids < 18',
                                                                                                                     ifelse(temp_clean$type_of_economic_family == 'H-W:no-earn,kid18-24',
                                                                                                                            'Husband Wife:no employed, kids 18 - 24',
                                                                                                                            ifelse(temp_clean$type_of_economic_family == '1parent:empl,kids<18',
                                                                                                                                   '1 Parent: employed, kids < 18',
                                                                                                                                   ifelse(temp_clean$type_of_economic_family == '1parent:emp,kid18-24',
                                                                                                                                          '1 Parent: employed, kids 18 - 24', 
                                                                                                                                          ifelse(temp_clean$type_of_economic_family == '1par:no-empl,kids<18',
                                                                                                                                                 '1 Parent: not employed, kids < 18', 
                                                                                                                                                 ifelse(temp_clean$type_of_economic_family == '1par:no-emp,kid18-24',
                                                                                                                                                        '1 Parent: not employed, kids 18 - 24', temp_clean$type_of_economic_family))))))))))))))))
  
  
  
  # just replace with NA with Not applicable (need to make character first)
  temp_clean$union_membership_status_employees_only <- as.character(temp_clean$union_membership_status_employees_only)
  temp_clean$union_membership_status_employees_only[is.na(temp_clean$union_membership_status_employees_only)] <- 'Not applicable'
  
  # same here as above
  temp_clean$age_of_spouse_if_applicable <- as.character(temp_clean$age_of_spouse_if_applicable)
  temp_clean$age_of_spouse_if_applicable[is.na(temp_clean$age_of_spouse_if_applicable)] <- 'Not applicable'
  
  # same as above, but also fix out of scope value in labour force status
  temp_clean$spouse_labour_force_status <- as.character(temp_clean$spouse_labour_force_status)
  temp_clean$spouse_labour_force_status[is.na(temp_clean$spouse_labour_force_status)] <- 'Not applicable'
  temp_clean$spouse_labour_force_status <- ifelse(grepl('military', temp_clean$spouse_labour_force_status),
                                                  'Out of scope (military)', temp_clean$spouse_labour_force_status)
  
  # same here as above
  temp_clean$spouses_usual_hours_at_main_job <- as.character(temp_clean$spouses_usual_hours_at_main_job)
  temp_clean$spouses_usual_hours_at_main_job[is.na(temp_clean$spouses_usual_hours_at_main_job)] <- 'Not applicable'
  
  # same as above and recode some levels in spouses main job
  temp_clean$spouses_class_of_worker_at_main_job <- as.character(temp_clean$spouses_class_of_worker_at_main_job)
  temp_clean$spouses_class_of_worker_at_main_job[is.na(temp_clean$spouses_class_of_worker_at_main_job)] <- 'Not applicable'
  temp_clean$spouses_class_of_worker_at_main_job <- ifelse(grepl('-w/', temp_clean$spouses_class_of_worker_at_main_job),
                                                           'Self-employed paid',
                                                           ifelse(grepl('no ', temp_clean$spouses_class_of_worker_at_main_job),
                                                                  'Self-employed not paid', temp_clean$spouses_class_of_worker_at_main_job))
  # remove the NA from the level "Spouse present,NA" 
  temp_clean$spouses_class_of_worker_at_main_job <- gsub(',NA', '', temp_clean$spouses_class_of_worker_at_main_job)
  
  
  return(temp_clean)
  
}


##########
# gss10 functions
##########
# no NAs in this data set
# if variable has "Not asked" level then it should pertain to the number of people in which that question was not applicable - they are negative of that question.
# for example: "how much does your spouse make". The not asked people are those without a spouse, or in survey jargon, not on that questions path. 
# not stated and/or not answered should be filled with NAs in all levels. This is done numeric data because its coded with all numbers except the "Not stated" or 
# "Not answered" level
# which turns to NA when converted to numbers.
clean_gss10 <- function(temp_clean) {
  
  # first restructure so factors are characters, else numeric
  temp_clean <- restructure_data_types(temp_clean)
  # this function will fill any variable that has "Not stated" or "Not answered" with NA, but for time being, I'll keep "Not asked" levels as they are.
  temp_clean <- get_na_gss10(temp_clean)
  
  # make columns that have mins or occur all numeric
  temp_clean <- cols_numeric_gss10(temp_clean, keyword = 'mins')
  
  # remove age 
  temp_clean<- temp_clean[grepl('15 to 17|18 to 19|20 to 24|25 to 29', 
                            temp_clean$age_group_of_the_respondent_groups_of_5),]
  
  # recode young child age - can conver to numeric and fill Nas with zero since only 
  # characters will become NAs, and all character represent zero (i checked this)
  temp_clean$age_of_respndnts_youngest_child_in_hhld <- 
    numeric_add_zero_gss10(temp_clean$age_of_respndnts_youngest_child_in_hhld)
  
  # same thing 
  temp_clean$age_of_youngest_member_in_respdnts_hhld <- 
    numeric_add_zero_gss10(temp_clean$age_of_youngest_member_in_respdnts_hhld)
  
  # remove extra white spaces  
  temp_clean$household_size_of_r <- remove_extra_white_spaces_gss10(temp_clean$household_size_of_r)
  
  # remove elipses, ?, and make first letters capital
  temp_clean$how_often_not_know_what_to_do_with_your_free_time <-
    remove_and_capitalize_gss10(temp_clean$how_often_not_know_what_to_do_with_your_free_time)
  
  temp_clean$in_general_would_you_say_your_health_is <-
    remove_and_capitalize_gss10(temp_clean$in_general_would_you_say_your_health_is)
  
  temp_clean$in_general_would_you_say_your_mental_health_is <-
    remove_and_capitalize_gss10(temp_clean$in_general_would_you_say_your_mental_health_is)
  
  temp_clean$in_past_month_how_often_use_the_internet_to_buy_goods_or_services <- 
    remove_and_capitalize_gss10(temp_clean$in_past_month_how_often_use_the_internet_to_buy_goods_or_services)
  
  # clean "hour" variables - "not stated" level should be filled with NA, Not answered as well.
  # so all you have to do is make these variables numeric and it will generate NA for you
  temp_clean$last_week_hrs_unpaid_care_to1_seniors_not_in_hhld <- 
    as.numeric(as.character(temp_clean$last_week_hrs_unpaid_care_to1_seniors_not_in_hhld))
  
  # recoed variables that are on scale from 1-10 as happiness level - 98 (dont know) or 99 (not stated) sho
  temp_clean$how_feel_about_your_life_as_whole <-
    recode_scale_vars_gss10(temp_clean$how_feel_about_your_life_as_whole)
  
  # recode last_week_was_your_main_activity 
  temp_clean$last_week_was_your_main_activity <- gsub("Working at a paid job or busi", 'Working at a paid job', temp_clean$last_week_was_your_main_activity)
  
  # recode labour_force_status_of_the_r
  temp_clean$labour_force_status_of_the_r <- gsub( " *\\(.*?\\) *", "", temp_clean$labour_force_status_of_the_r)
  temp_clean$labour_force_status_of_the_r <- gsub( "emplmnt", "employment", temp_clean$labour_force_status_of_the_r)
  
  # clean  or busi from main_activity_of_r_in_the_last_12_months
  temp_clean$main_activity_of_the_r_in_the_last_12_months <- 
    gsub(' or busi', '', temp_clean$main_activity_of_the_r_in_the_last_12_months)
  
  # clean education variables
  temp_clean$highest_level_of_edu_obtained_by_the_r_5_groups <-
    clean_education_var_gss10(temp_clean$highest_level_of_edu_obtained_by_the_r_5_groups, spouse = FALSE)
  temp_clean$highest_level_of_educ_obtained_by_the_rs_spousepartner_5_groups <-
    clean_education_var_gss10(temp_clean$highest_level_of_educ_obtained_by_the_rs_spousepartner_5_groups, spouse = TRUE)
  temp_clean$highest_level_of_educ_obtained_by_the_rs_mother_5_groups <-
    clean_education_var_gss10(temp_clean$highest_level_of_educ_obtained_by_the_rs_mother_5_groups, spouse = FALSE)
  
  # clean would_you_say_that_you_know_the_ppl_in_your_neighbhood
  temp_clean$would_you_say_that_you_know_the_ppl_in_your_neighbhood <-
    gsub(' in your n', '', temp_clean$would_you_say_that_you_know_the_ppl_in_your_neighbhood)
  
  # clean main_source_of_inc_during_the_yr_ending_dec_31_2009
  temp_clean$main_source_of_inc_during_the_yr_ending_dec_31_2009 <- ifelse(grepl('Employment-sal', temp_clean$main_source_of_inc_during_the_yr_ending_dec_31_2009), 
                                                                         'Employment salary, commission, tips', 
                                                                         ifelse(grepl('Self-employ',temp_clean$main_source_of_inc_during_the_yr_ending_dec_31_2009),
                                                                                'Self employed, unincorporated',
                                                                                ifelse(grepl('Invest', temp_clean$main_source_of_inc_during_the_yr_ending_dec_31_2009),
                                                                                       'Investment income, interest, or net rents',
                                                                                       ifelse(grepl('Retiremnt', temp_clean$main_source_of_inc_during_the_yr_ending_dec_31_2009),
                                                                                              'Retirement pension or annuitites',
                                                                                              ifelse(grepl('Guaranteed Inc', temp_clean$main_source_of_inc_during_the_yr_ending_dec_31_2009),
                                                                                                     'Guaranteed income supplement',
                                                                                                     ifelse(grepl('Prov Territ', temp_clean$main_source_of_inc_during_the_yr_ending_dec_31_2009),
                                                                                                            'Provincial/municipal social assistance', temp_clean$main_source_of_inc_during_the_yr_ending_dec_31_2009))))))
  
  
  return(temp_clean)
  # 
}

# sub functions
cols_numeric_gss10 <- function(temp_clean, keyword) {
  
  for(col_num in 1:ncol(temp_clean)) {
    
    temp_col_name <-colnames(temp_clean)[col_num]
    temp_col <- temp_clean[, col_num]
    
    if(grepl(keyword, temp_col_name)) {
      
      # remove "No time spent for this" by making the column numeric - that will make 
      # that level an NA and then fill that with zero, because no NAs in dataset.
      temp_col <- as.numeric(temp_col)
      
      # fill NA with 0
      temp_col[is.na(temp_col)] <- 0
      
    }
    temp_clean[, col_num] <- temp_col
  }
  return(temp_clean)
}


numeric_add_zero_gss10 <- function(temp_clean_column){
  temp_clean_column <- as.numeric(temp_clean_column)
  temp_clean_column[is.na(temp_clean_column)] <- 0
  return(temp_clean_column)
}

remove_extra_white_spaces_gss10 <- function(temp_clean_column){
  temp_clean_column <- gsub('\\s+', " ", temp_clean_column)
  return(temp_clean_column)
}


# make_first_captial <- function(x) {
#   s <- strsplit(x, " ")[[1]]
#   paste(toupper(substring(s, 1,1)), substring(s, 2),
#         sep="", collapse=" ")
# }


recode_scale_vars_gss10 <- function(temp_clean_column) {
  temp_clean_column <- gsub("Very dissatisfied", "1", temp_clean_column)
  # make numeric
  temp_clean_column <- as.numeric(as.character(temp_clean_column))
  return(temp_clean_column)
}


remove_and_capitalize_gss10 <- function(temp_clean_column) {
  temp_clean_column <- gsub('... ', '', temp_clean_column , fixed = TRUE)
  temp_clean_column <- gsub('?', '', temp_clean_column , fixed = TRUE)
  # temp_clean_column <- 
  #   sapply(temp_clean_column, make_first_captial)
  return(temp_clean_column)
  
}

# clean levels of education
clean_education_var_gss10 <- function(temp_clean_column, spouse) {
  
  temp_clean_column <- gsub('Dipl/certif from com coll or trade/technica' ,
                            'Dip/cert from trade/technical school or college', 
                            temp_clean_column)
  
  temp_clean_column <- gsub('Some uni/comm coll' ,
                            'Some university or community college', 
                            temp_clean_column)
  
  temp_clean_column <- gsub('Some sec/elem/no schl' ,
                            'Some secondary/elementary or no school', 
                            temp_clean_column)
  
  if(spouse) {
    temp_clean_column <- gsub('7', 'No spouse', temp_clean_column)
  }
  
  return(temp_clean_column)
}

get_na_gss10 <- function(temp_clean) {
  for(col_num in 1:ncol(temp_clean)){
    temp_col <- temp_clean[, col_num]
    if(any(grepl('not stated|not answered', temp_col, ignore.case = TRUE))) {
      temp_col <- gsub('not stated', NA, temp_col, ignore.case = TRUE)
      temp_col <- gsub('not answered', NA, temp_col, ignore.case = TRUE)
    }
    temp_clean[, col_num] <- temp_col
  }
  return(temp_clean)
}


##########
# osduhs functions
##########

get_date_and_time_osduhs <- function(temp_clean) {
  # get survey data, and start and finsih time
  survey_date <- trimws(temp_clean$date_of_survey_administration, 'both')
  survey_begin_time <- trimws(temp_clean$time_started_survey_written_by_student, 'both')
  survey_end_time <- trimws(temp_clean$time_ended_survey_written_by_student, 'both')
  
  for(i in 1:nrow(temp_clean)){
    temp_survey_date <- survey_date[i]
    temp_survey_begin_time <- survey_begin_time[i]
    temp_survey_end_time <- survey_end_time[i]
    
    if(is.na(temp_survey_date)) {
      temp_survey_date <- NA
    } else if (nchar(temp_survey_date) == 7) {
      temp_survey_date <- gsub('^(.{1})(.*)$', '\\1/\\2', temp_survey_date)
      temp_survey_date <- gsub('^(.{4})(.*)$', '\\1/\\2', temp_survey_date)
    } else {
      temp_survey_date <- gsub('^(.{2})(.*)$', '\\1/\\2', temp_survey_date)
      temp_survey_date <- gsub('^(.{5})(.*)$', '\\1/\\2', temp_survey_date)
    }
    
    if(is.na(temp_survey_begin_time)) {
      temp_survey_begin_time <- NA
    } else if(nchar(temp_survey_begin_time) == 3) {
      temp_survey_begin_time <- gsub('^(.{1})(.*)$', '\\1:\\2', temp_survey_begin_time)
    } else {
      temp_survey_begin_time <- gsub('^(.{2})(.*)$', '\\1:\\2', temp_survey_begin_time)
    }

    if(is.na(temp_survey_end_time)){
      temp_survey_end_time <- NA
    } else if(nchar(temp_survey_end_time) == 3) {
      temp_survey_end_time <- gsub('^(.{1})(.*)$', '\\1:\\2', temp_survey_end_time)
    } else {
      temp_survey_end_time <- gsub('^(.{2})(.*)$', '\\1:\\2', temp_survey_end_time)
    }
   
    survey_date[i] <- temp_survey_date
    survey_begin_time[i] <- temp_survey_begin_time
    survey_end_time[i] <- temp_survey_end_time
    
    print(i)
  }
  
  temp_clean$date_of_survey_administration <- as.Date(survey_date, format = '%d/%m/%Y')
  temp_clean$time_started_survey_written_by_student<- as.POSIXct(paste(survey_date, survey_begin_time, sep = ' '), format="%d/%m/%Y %H:%M")
  temp_clean$time_ended_survey_written_by_student <- as.POSIXct(paste(survey_date, survey_end_time, sep = ' '), format="%d/%m/%Y %H:%M")

  return(temp_clean)
}

# # make first letter capital 
# get_capital_osduhs <- function(temp_clean) {
#   
#   for(num_col in 1:ncol(temp_clean)) {
#     temp_col <- temp_clean[, num_col]
#     if(grepl('character', class(temp_col))) {
#       temp_col <- sapply(temp_col, make_first_captial)
#     }
#     temp_clean[, num_col] <- temp_col
#     temp_clean[, num_col] <- gsub('NANA', NA, temp_clean[, num_col])
#   }
#   
#   temp_clean[grepl('NANA', temp_clean)] <- NA
#   return(temp_clean)
#   
# }


get_body_weight_osduhs <- function(temp_clean){
  # temp <- unlist(lapply(strsplit(as.character(dat$hw_osduhs_weight), 
  #                         '/', 
  #                         fixed = TRUE), function(x){
  #                           x[1]
  #                         }))
  unique_pounds <- as.character(unique(sort(temp_clean$what_is_your_current_weight_without_shoes)))
  
  # take bottom to put back on top
  bottom_5 <- unique_pounds[38:length(unique_pounds)]
  unique_pounds <- unique_pounds[-c(38:length(unique_pounds))]
  unique_pounds <- c(bottom_5, unique_pounds)
  
  
  temp <- as.data.frame(cbind(pounds = unique_pounds, 
                              sequence = seq(1, length(unique_pounds), 1)))
  temp$sequence <- as.numeric(sort(temp$sequence))
  
  # split into 5 categories using sequence 
  temp$new_weight <- ifelse(temp$sequence > 0 & temp$sequence <=6, '80_105',
                            ifelse(temp$sequence > 6 & temp$sequence <=12, '106_135', 
                                   ifelse(temp$sequence > 12 & temp$sequence <= 18, '136_165',
                                          ifelse(temp$sequence > 18 & temp$sequence <=24, '166_195',
                                                 ifelse(temp$sequence > 24 & temp$sequence <= 30, '196_225',
                                                        ifelse(temp$sequence > 30 & temp$sequence <=36, '226_255', '255_up'))))))
  
  temp$sequence <- NULL
  temp_clean <- left_join(temp_clean, temp, by = c('what_is_your_current_weight_without_shoes' = 'pounds'))
  temp_clean$what_is_your_current_weight_without_shoes <- NULL
  colnames(temp_clean)[ncol(temp_clean)] <- 'what_is_your_current_weight_without_shoes'
  return(temp_clean)
  
}

