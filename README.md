# About
This repository contains the data download, cleaning, and analysis code for the research project, "Associations between greenness change and mental distress prevalence change by
housing tenure: an analysis of 109 U.S. metropolitan areas" by Saira Prasanth, Joshua Warren, Daniel Carrión, Gregg Gonsalves, and Danya Keene (in preparation).

# Installation and data downloads
To reproduce this analysis, first, install R version 4.3.2. R is open-source and available for free from the R Project for Statistical Computing: https://www.r-project.org/

Next, download a copy of this repository to your device and open the R project ("greening-housing-analysis.Rproj") in R or R Studio. The renv package will be installed, if it is not currently installed on your device. To load package versions identical to those used in this analysis (as recorded in the lockfile, "renv.lock"), run `r renv::restore()`. This will change versions of packages loaded within this project only, and package versions for any other projects will remain unchanged.

Below are instructions to reproduce the datasets used in this analysis.

To retrieve population data from the U.S. Census Bureau using the tidycensus package, you will need to create a Census API key. You can request a key by following the instructions at this link: https://api.census.gov/data/key_signup.html
