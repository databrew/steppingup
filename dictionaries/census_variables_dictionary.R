# This script will generate a spreadsheet that will then be manually edited to create a data dictrionary for the 
# census variables, enabling us to keep the union of the set of all census variables
library(rowr)

# we'll use 2006 data as our reference year and coerce all other data sets to that
# load data
temp_2001 <- read_csv(paste0('data/census_data/2001_census.csv'))
temp_2006 <- read_csv(paste0('data/census_data/2006_census.csv'))
temp_2011 <- read_csv(paste0('data/census_data/2011_census.csv'))
temp_2016 <- read_csv(paste0('data/census_data/2016_census/census_2016_1.csv'))

# get 2001 data first
temp_2001 <- as.data.frame(cbind.fill(colnames(temp_2001),
                                      colnames(temp_2006),
                                      fill = NA))
colnames(temp_2001) <- c('old_2001', 'old_2006')
write.csv(temp_2001, 'dictionaries/temp_2001.csv')

# get 2011 data first
temp_2011 <- as.data.frame(cbind.fill(colnames(temp_2011),
                                      colnames(temp_2006),
                                      fill = NA))
colnames(temp_2011) <- c('old_2011', 'old_2006')
write.csv(temp_2011, 'dictionaries/temp_2011.csv')


# get 2016 data first
temp_2016 <- as.data.frame(cbind.fill(colnames(temp_2016),
                                      colnames(temp_2006),
                                      fill = NA))
colnames(temp_2016) <- c('old_2016', 'old_2006')
write.csv(temp_2016, 'dictionaries/temp_2016.csv')

