# explore environex data
library(foreign)

#millenial survey 

# read in data 
temp_dat <- read.spss(file = 'data/millenial_survey.sav',
                      use.value.labels = T,
                      to.data.frame = T,
                      trim.factor.names = T,
                      trim_values = F,
                      use.missings = FALSE)

# get long for variable names
colnames(temp_dat) <- attr(temp_dat,"variable.labels")

first_100 <- temp_dat[, 1:100]
second_100 <- temp_dat[, 101:200]
third_100 <- temp_dat[, 201:300]
fourth_100 <- temp_dat[, 301:400]
fifth_100 <- temp_dat[, 401:500]
sixth_100 <- temp_dat[, 501:600]
seventh_100 <- temp_dat[, 601:648]



# Environics Muslim Survey 2016

# read in data 
temp_dat <- read.spss(file = 'data/Environics Institute -  Survey of Muslims in Canada 2016 - Data File v2.sav',
                      use.value.labels = T,
                      to.data.frame = T,
                      trim.factor.names = T,
                      trim_values = F,
                      use.missings = FALSE)

# get long for variable names
colnames(temp_dat) <- attr(temp_dat,"variable.labels")

# first 167 columns no good 
temp <- temp_dat[, 1:100]




# Muslims in Canada 2016 - Youth Study - Data File v1 .sav

# read in data 
temp_dat <- read.spss(file = 'data/Muslims in Canada 2016 - Youth Study - Data File v1 .sav',
                      use.value.labels = T,
                      to.data.frame = T,
                      trim.factor.names = T,
                      trim_values = F,
                      use.missings = FALSE)

# get long for variable names
colnames(temp_dat) <- attr(temp_dat,"variable.labels")

# first 167 columns no good 
temp <- temp_dat[, 1:100]



# UAPS - Main Study - SPSS Datafile - FOR LICENSE.sav

# read in data 
temp_dat <- read.spss(file = "data/UAPS - Main Study - SPSS Datafile - FOR LICENSE.sav",
                      use.value.labels = T,
                      to.data.frame = T,
                      trim.factor.names = T,
                      trim_values = F,
                      use.missings = FALSE)

# get long for variable names
colnames(temp_dat) <- attr(temp_dat,"variable.labels")

# first 167 columns no good 
temp <- temp_dat[, 1:100]




