# steppingup

Youth Compass Stepping Up Web Application

## Data sources

### Geographic data

- Census division shapefile was retrieved from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm on 2017-11-03. Its filename is `gcd_000b11a_e`.  
- Census sub-division shapefile was retrivied from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm on 2017-11-03. Its filename is `gcsd_000b11a_e`. 
- Province and sub-province boundaries were retrieved via the raster package, from the GADM database, on 2017-10-25.  

### Census data  

Customized Canadian census data for 2001, 2006, and 2011 has been purchased by YouthREX and is stored (in a modified form) privately in the `databrew/Conulting Projects/york university/census_data` google drive folder. The files were originally saved in .ivt format, and were opened by downloading the Statistics Canada's Beyond 20/20 Professional Browser Software on a Windows machine. The Beyond 20/20 Browser can be downloaded here: https://www.statcan.gc.ca/eng/public/beyond20-20

Once the .ivt files are opened in Beyond 20/20, they can be manipulated to display data for specific parameters. The tables we downloaded all have the 'geography' parameter (broken down by Canada, provinces, census divisions, and census subdivisions) as rows and selected variables as columns. This is meant to facilitate ultimately mapping the data by census division.

## Use

### Task 1: Get data

Download this entire folder from [https://drive.google.com/drive/folders/1xjJVvybmhH6RUk3Ic-6fSCEuCkxmqJH9](https://drive.google.com/drive/folders/1xjJVvybmhH6RUk3Ic-6fSCEuCkxmqJH9) and save it to your data folder so it looks like this: "data/census_data/(put the 8 .`csv`s here)"

### Task 2: Run global.R, save data (optional)

Run global.R and it will save the data to your data folder (outside of census_data). Alternatively, the first time you run the app, `global.R` will automatically be called.

### Task 3: Start app

Now that you have the final data set with all info start a new branch to work on your app off of `global.R`. 

