# About
This repository contains the data download, cleaning, and analysis code for the study, "Associations between greenness change and mental distress prevalence change by housing tenure: an analysis of 109 U.S. metropolitan areas" by Saira Prasanth, Joshua Warren, Daniel Carrión, Gregg Gonsalves, and Danya Keene (in preparation).

# Installation and data downloads
To reproduce this analysis, first, install R version 4.5.1. R is open-source and available for free from the R Project for Statistical Computing: https://www.r-project.org/

Next, download a copy of this repository to your device and open the R project ("greening-housing-analysis.Rproj") in R or R Studio. The renv package will be installed, if it is not currently installed on your device. To load package versions identical to those used in this analysis (as recorded in the lockfile, "renv.lock"), run `renv::restore()`. This will change versions of packages loaded within this project only, and package versions for any other projects will remain unchanged.

Below are instructions to reproduce the datasets used in this analysis.

1.  To retrieve population data from the U.S. Census Bureau using the `tidycensus` package, you will need to create a Census API key. You can request a key by following the instructions at this link: https://api.census.gov/data/key_signup.html

2.  To compute population-weighted exposure estimates, you will need to manually download the Gridded Population of the World (GPW) Version 4: Population Count, Revision 11 dataset for the year 2020 with 30-second resolution from the Columbia University Center for International Earth Science Information Network (CIESIN) and NASA Socioeconomic Data and Applications Center (SEDAC). This will require a free NASA Earthdata account. If you do not already have an account, you can create one here: https://urs.earthdata.nasa.gov/users/new (additional information about Earthdata Login and detailed instructions for registering are available here: https://urs.earthdata.nasa.gov/documentation/what_do_i_need_to_know).

3.  Once you have created an account, visit the following URL to immediately begin the download: https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-count-rev11/gpw-v4-population-count-rev11_2020_30_sec_tif.zip

4.  You may be prompted to log in with your NASA Earthdata account. Once you log in, the download will begin. The downloaded .zip folder should be named: "gpw-v4-population-count-rev11_2020_30_sec_tif.zip"; place this in the "data" folder.

5.  To download the 500 Cities: Census Tract-level Data (GIS Friendly Format), 2016 release required for the tract-level secondary analysis, visit https://chronicdata.cdc.gov/500-Cities/500-Cities-Census-Tract-level-Data-GIS-Friendly-Fo/5mtz-k78d

6. Click export, ensure that "Download file" is selected and "CSV" is selected for the export format, and click download. The downloaded .csv file should be named: "500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2016_release_20250622.csv"; place this in the "data" folder.

7. To download the corresponding shapefile for the 500 Cities, 2016 release, visit https://chronicdata.cdc.gov/500-Cities/500-Cities-Census-Tract-Boundaries/x7zy-2xmx and click "Download" under the "Download this Resource" section. The downloaded .zip folder should be named: "500Cities_Tracts_11082016"; place this in the "data" folder.

8. To download the PLACES: Census Tract Data (GIS Friendly Format), 2021 release required for the tract-level secondary analysis, visit https://data.cdc.gov/500-Cities-Places/PLACES-Census-Tract-Data-GIS-Friendly-Format-2021-/mb5y-ytti

9. Click export, ensure that "Download file" is selected and "CSV" is selected for the export format, and click download. The downloaded .csv file should be named: "PLACES__Census_Tract_Data__GIS_Friendly_Format___2021_release_20250622.csv"; place this in the "data" folder.

The "data" folder contains the folder "NDVI" with monthly 1-km x 1-km normalized difference vegetation index (NDVI) in each year from 2011 through 2019, retrieved from NASA's Terra Moderate Resolution Imaging Spectroradiometer (MODIS) Vegetation Indices Monthly (MOD13A3) Version 6.1 product (available here: https://doi.org/10.5067/MODIS/MOD13A3.061). There are 109 total GeoTIFF files containing NDVI data in this folder (1 for each month in 2011-2019). The "data" folder also contains the files "feb_2013_cbsa.xls" and "metro_area_history_1950_2020.xls", which list U.S. core based statistical areas, metropolitan divisions, and component counties by year (downloads and more information are available here: https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/historical-delineation-files.html). These do not need to be manually downloaded.

The Behavioral Risk Factor Surveillance System Selected Metropolitan/Micropolitan Area Risk Trends (BRFSS SMART) 2011 and 2019 datasets used in this analysis will be retrieved via URL when running the code (data and documentation are available here: https://www.cdc.gov/brfss/smart/Smart_data.htm). MMSA, county, and additional census tract shapefiles will be retrieved via API. These do not need to be manually downloaded.

# Running the analysis
Open the `_targets.R` and `Functions.R` scripts. Then, paste your Census API key between the quotation marks in line 14 of `_targets.R`, as indicated in the script.

Then, in the R console, run the following lines (also located at the beginning of `_targets.R`) to load the packages required to define and execute the `targets` pipeline:
```
library(targets)
library(tarchetypes)
library(future)
```
Finally, to execute the entire pipeline, run `tar_make()` in the R console. You can cancel the execution at any point by pressing the escape key.

# Viewing results

Each table and figure has a corresponding "target," which can be read using `tar_read()` and loaded into your R environment using `tar_load()`. For example, Table 1, Table 2, and Figure S1 can be viewed in R by running the following lines:
```
tar_read(table_1)
tar_read(table_2)
tar_read(figure_S1)
```
Additionally, after running the pipeline, all main or supplementary figures and tables will be saved in the "output" folder created in your local repository.
