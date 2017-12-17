# steppingup

Youth Compass Stepping Up Web Application

## Data sources

### Geographic data

- Census division shapefile was retrieved from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm on 2017-11-03. Its filename is `gcd_000b11a_e`.  
- Census sub-division shapefile was retrivied from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm on 2017-11-03. Its filename is `gcsd_000b11a_e`. 
- Province and sub-province boundaries were retrieved via the raster package, from the GADM database, on 2017-10-25.  

### Census data  

Customized Canadian census data for 2001, 2006, and 2011 has been purchased by YouthREX and is stored (in a modified form) privately in the `databrew/Conulting Projects/york university/census_data` google drive folder. The files were originally saved in .ivt format, and were opened by downloading the Statistics Canada's Beyond 20/20 Professional Browser Software on a Windows machine. The Beyond 20/20 Browser can be downloaded here: https://www.statcan.gc.ca/eng/public/beyond20-20

### Survey data

Survey data is stored privately at https://drive.google.com/drive/u/0/folders/1JgFxWSjvsCgX9_QS2Si3eMjRKpCkMw4s (`databrew/Consulting Projects/york university/survey_data`) in google drive. The files were originally provided to databrew by York University via a shared Box account.

## The application

### Location

The live application is at http://databrew.cc/youthrex.

### Use

For a demonstration of the application's use, see [https://www.youtube.com/watch?v=940TuOpuJWY](https://www.youtube.com/watch?v=940TuOpuJWY) (video to be updated in future).

## Developer's guide 

### Task 1: Get data

Download this entire folder from [https://drive.google.com/drive/folders/1xjJVvybmhH6RUk3Ic-6fSCEuCkxmqJH9](https://drive.google.com/drive/folders/1xjJVvybmhH6RUk3Ic-6fSCEuCkxmqJH9) and save it to your data folder so it looks like this: "data/census_data/(put the 8 .`csv`s here)"

### Task 2: Run global.R, save data (optional)

Run global.R and it will save the data to your data folder (outside of census_data). Alternatively, the first time you run the app, `global.R` will automatically be called.

### Task 3: Start app

Now that you have the final data set with all info start a new branch to work on your app off of `global.R`. 

