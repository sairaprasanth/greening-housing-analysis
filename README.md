# About
This repository contains the data download, cleaning, and analysis code for the research project, "Associations between greenness change and mental distress prevalence change by
housing tenure: an analysis of 109 U.S. metropolitan areas" by Saira Prasanth, Joshua Warren, Daniel Carrión, Gregg Gonsalves, and Danya Keene (in preparation).

# Installation and data downloads
To reproduce this analysis, first, install R version 4.3.2. R is open-source and available for free from the R Project for Statistical Computing: https://www.r-project.org/

Next, download a copy of this repository to your device and open the R project ("greening-housing-analysis.Rproj") in R or R Studio. The renv package will be installed, if it is not currently installed on your device. To load package versions identical to those used in this analysis (as recorded in the lockfile, "renv.lock"), run `renv::restore()`. This will change versions of packages loaded within this project only, and package versions for any other projects will remain unchanged.

Below are instructions to reproduce the datasets used in this analysis.

1.  To retrieve population data from the U.S. Census Bureau using the `tidycensus` package, you will need to create a Census API key. You can request a key by following the instructions at this link: https://api.census.gov/data/key_signup.html

2.  To compute population-weighted exposure estimates, you will need to manually download the Gridded Population of the World (GPW) Version 4: Population Count, Revision 11 dataset for the year 2020 with 30-second resolution from the Columbia University Center for International Earth Science Information Network (CIESIN) and NASA Socioeconomic Data and Applications Center (SEDAC). This will require a free NASA Earthdata account. If you do not already have an account, you can create one here: https://urs.earthdata.nasa.gov/users/new (additional information about Earthdata Login and detailed instructions for registering are available here: https://urs.earthdata.nasa.gov/documentation/what_do_i_need_to_know).

3.  Once you have created an account, visit the following URL to immediately begin the download: https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-count-rev11/gpw-v4-population-count-rev11_2020_30_sec_tif.zip

4.  You may be prompted to log in with your NASA Earthdata account. Once you log in, the download will begin. The downloaded .zip folder should be named: "gpw-v4-population-count-rev11_2020_30_sec_tif"; place this in the "data" folder.

The "data" folder contains monthly 1-km x 1-km normalized difference vegetation index (NDVI) in each year from 2011 through 2019, retrieved from NASA's Terra Moderate Resolution Imaging Spectroradiometer (MODIS) Vegetation Indices Monthly (MOD13A3) Version 6.1 product (available here: https://doi.org/10.5067/MODIS/MOD13A3.061). There are 109 total GeoTIFF files containing NDVI data in this folder (1 for each month in 2011-2019).
  
