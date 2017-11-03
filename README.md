# steppingup

Youth Compass Stepping Up Web Application

## Data sources

### Geographic data

- Census division shapefile was retrieved from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm on 2017-11-03. Its filename is `gcd_000b11a_e`.  
- Census sub-division shapefile was retrivied from http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm on 2017-11-03. Its filename is `gcsd_000b11a_e`. 
- Province and sub-province boundaries were retrieved via the raster package, from the GADM database, on 2017-10-25.  

### Census data  

Customized Canadian census data for 2001, 2006, and 2011 has been purchased by YouthREX and is stored in the 'databrew/Canada census data' google drive folder. The files are saved in .ivt format and can be opened by downloading the Statistics Canada's Beyond 20/20 Professional Browser Software on a Windows machine. The Beyond 20/20 Browser can be downloaded here: https://www.statcan.gc.ca/eng/public/beyond20-20

A summary of the census data that was purchased from StatCan can be viewed here: https://drive.google.com/open?id=1y04s9gVvCPZtGmy4MoWToW5_ap61vOrL

Once the .ivt files are opened in Beyond 20/20, they can be manipulated to display data for specific parameters. The tables we downloaded all have the 'geography' parameter (broken down into census divisions and subdivisions) as rows and selected variables as columns. This is meant to facilitate ultimately mapping the data by census division.

All census data files are in the `databrew/Youth Compass data` google drive folder, and contain the following prefix: `databrew_youth_compass_`. It is important that this prefix be included, as files without this prefix are not automatically downloaded for the apps use.
