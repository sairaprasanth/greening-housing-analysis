
get_brfss <- function() {
  
  # download BRFSS SMART datasets, 2011-2019
  if(!file.exists(here("data", "main_MMSA2011.xpt"))){download.file("https://www.cdc.gov/brfss/smart/2011/mmsa11xpt.zip", destfile = here("data", "MMSA2011.xpt"))}
  if(!file.exists(here("data", "main_MMSA2019.xpt"))){download.file("https://www.cdc.gov/brfss/annual_data/2019/files/MMSA2019_XPT.zip", destfile = here("data", "MMSA2019.xpt"))}
  
  # list files to be unzipped
  mmsa_filenames <- list.files(here("data"), pattern = "MMSA", full.names = TRUE)
  
  clean_brfss <- function(mmsa_filename) {
    
    # assign year
    year <- mmsa_filename %>% 
      str_sub(start = nchar(mmsa_filename)-7, end = nchar(mmsa_filename)-4)
    
    # load data
    clean <- import(mmsa_filename) %>% 
      # select variables of interest
      dplyr::select(MMSANAME = contains("MMSANAM"), MMSA = `_MMSA`, MENTHLTH, RENTHOM1, STSTR = `_STSTR`, MMSAWT = `_MMSAWT`) %>% 
      # dichotomize mental distress (less than 14 days vs. 14+ days)
      mutate(mh2 = case_when(MENTHLTH == 88 ~ 0, MENTHLTH %in% c(77,99) ~ NA, 
                             MENTHLTH %in% 1:13 ~ 0, MENTHLTH %in% 14:30 ~ 1)) %>% 
      # replace MMSA names with accent marks to avoid error
      mutate(MMSANAME = case_when(MMSA == 41980 ~ "San Juan-Bayamon-Caguas, PR, Metropolitan Statistical Area",
                                  MMSA != 41980 ~ MMSANAME)) %>% 
      # clean MMSA name to join later with shapefile
      mutate(NAME = sub(regex(" (Metropolitan|Micropolitan) (Statistical Area|Division)"), 
                        "   ", MMSANAME)) %>%
      mutate(NAME = sub(regex("(,|)\\s\\s+"), "", NAME), YEAR = as.numeric(year)) %>% 
      # dichotomize homeowner status
      mutate(owner = case_when(RENTHOM1 == 1 ~ 1, RENTHOM1 == 2 ~ 0, RENTHOM1 %in% c(3,7,9,NA) ~ NA)) %>%
      # recode mh2, MMSA, stratum, and owner as factor variables
      mutate(mh2 = factor(mh2), owner = factor(owner), MMSA = factor(MMSA), STSTR = factor(STSTR))
    
    return(clean)
  }
  
  # load and prepare data for each year
  brfss <- lapply(mmsa_filenames, clean_brfss)
  
  return(brfss)
  
}

get_mmsa <- function(brfss) {
  
  sample_names <- unique(brfss[[1]][c("MMSA", "MMSANAME")]) %>%
    # join 2011 BRFSS MMSAs to 2019 BRFSS MMSAs by MMSA ID
    inner_join(unique(brfss[[2]][c("MMSA", "MMSANAME")]), by = "MMSA") %>%
    # filter for contiguous US
    filter(!str_detect(MMSANAME.y, ", HI"), !str_detect(MMSANAME.y, ", PR"), !str_detect(MMSANAME.y, ", AK")) %>% 
    # clean names if extra comma
    mutate(MMSANAME = sub(regex(", Metropolitan"), " Metropolitan", MMSANAME.x)) %>% 
    mutate(MMSANAME = sub(regex(", Micropolitan"), " Micropolitan", MMSANAME))

  sample_mmsa <- sample_names %>% 
    # filter for MMSA names
    filter(str_detect(MMSANAME, "Statistical Area")) 
  
  sample_division <- sample_names %>% 
    # filter for division names
    filter(str_detect(MMSANAME, "Division"))
  
  # get county polygons and poverty rate, ethnoracial group for summarizing over metro divisions
  counties_pre <- get_acs(geography = "county", survey = "acs5", year = 2011, 
                          variables = c(
                                        # poverty
                                        "B06012_001", "B06012_002",
                                        # ethnoracial group
                                        "B03002_001", "B03002_003"
                                        ),
                          output = "wide", geometry = FALSE)
  
  # get metro division geometries
  divisions_pre <- metro_divisions(year = 2011) %>%
    inner_join(sample_division, by = c("METDIVFP" = "MMSA"))
  
  # get metro division delineation
  div_13 <- read_excel(here("data", "feb_2013_cbsa.xls"), skip = 2) %>% 
    # filter for metro divisions
    filter(!is.na(`Metropolitan Division Title`)) %>% 
    dplyr::select(NAME = `Metropolitan Division Title`, `County/County Equivalent`, 
                  `Metro Division Code`, `FIPS State Code`, `FIPS County Code`)
  
  divisions_fill <- divisions_pre %>% 
    # join counties to metro divisions using 2013 (earliest available) delineation file
    left_join(div_13, by = c("METDIVFP" = "Metro Division Code")) %>% 
    # get county GEOID to join with estimates
    mutate(GEOID = paste0(`FIPS State Code`, `FIPS County Code`)) %>% 
    # join county estimates
    left_join(counties_pre, by = "GEOID") %>% 
    # summarize population counts across all counties in metro division
    group_by(METDIVFP, NAME.x) %>% 
    summarize(B06012_002E = sum(B06012_002E), B06012_001E = sum(B06012_001E), 
              B03002_003E = sum(B03002_003E), B03002_001E = sum(B03002_001E)) %>% 
    # compute proportion under poverty level
    mutate(prop_poverty = B06012_002E/B06012_001E) %>% 
    # compute proportion non-Hispanic White
    mutate(prop_nhw = B03002_003E/B03002_001E) %>% 
    # # join BRFSS area titles
    # left_join(sample_names, by = c("METDIVFP" = "MMSA")) %>% 
    # # clean names
    # mutate(MMSANAME = paste0(MMSANAME, " Metropolitan Division")) %>% 
    dplyr::select(MMSA = METDIVFP, prop_poverty, prop_nhw, geometry)
  
  # get baseline ACS poverty rate, ethnoracial group and CBSA (MMSA) geometry
  mmsa_pre <- get_acs(geography = "cbsa", 
                      variables = c(# poverty
                                    "B06012_001", "B06012_002",
                                    # ethnoracial group
                                    "B03002_001", "B03002_003"),
                      year = 2011, survey = "acs5", output = "wide", geometry = TRUE) %>% 
    # compute proportion under poverty level
    mutate(prop_poverty = B06012_002E/B06012_001E) %>% 
    # compute proportion non-Hispanic White
    mutate(prop_nhw = B03002_003E/B03002_001E) %>% 
    # filter for sample MMSAs
    filter(GEOID %in% sample_mmsa$MMSA) %>% 
    # select columns
    dplyr::select(MMSA = GEOID, prop_poverty, prop_nhw, geometry)
  
  # combine MMSAs and metro divisions
  mmsa <- mmsa_pre %>%
    bind_rows(divisions_fill)
  
  return(mmsa)
}

get_ndvi_summary <- function(sf, analysis) {
  
  # load monthly NDVI for 2011-2019
  ndvi_files <- list.files(path = here("data", "NDVI"))
  
  if(analysis == "tracts") {
    
    # for tract-level secondary analysis, subset list to only 2014-2019 months
    ndvi_files <- ndvi_files[37:108]
    
  }
  
  # create raster for each NDVI file
  ndvi <- purrr::map(ndvi_files, ~ rast(here("data", "NDVI", .x)))

  # unzip GPW data
  unzip(here("data", "gpw-v4-population-count-rev11_2020_30_sec_tif.zip"), files = "gpw_v4_population_count_rev11_2020_30_sec.tif", exdir = here("data"))
  
  # load and reproject Gridded Population of the World for population weighting
  gpw <- rast(here("data", "gpw_v4_population_count_rev11_2020_30_sec.tif")) %>%
    # change CRS
    project(crs(sf)) %>%
    # match extent
    crop(sf)
  
  # reproject NDVI raster to shapefile CRS
  ndvi_proj <- lapply(ndvi, project, y = crs(sf)) %>% 
    # crop to match extent
    lapply(., crop, sf)
  
  # resample gpw to match NDVI grid cells
  gpw_ndvi <- resample(gpw, ndvi_proj[[1]]) %>% 
    # replace NA with 0
    classify(., cbind(NA, 0))
  
  # extract NDVI weighted average over each polygon  
  ndvi_vals <- purrr::map(ndvi_proj, ~ exact_extract(.x, sf, fun = "weighted_mean", weights = gpw_ndvi))
  
  # initialize empty list
  ndvi_sf <- list()
  
  # append average NDVI values to shapefile
  for(i in 1:length(ndvi_vals)) {
    
    ndvi_sf[[i]] <- sf %>% 
      mutate(ndvi = ndvi_vals[[i]], year = str_extract(ndvi_files[i], regex("\\d{4}")))
  }
  
  ndvi_summary <- ndvi_sf %>%
    # bind rows for NDVI by unit across all months in 2011-2019
    bind_rows() %>%
    group_by(MMSA) %>% 
    # compute standard deviation of monthly NDVI values
    mutate(ndvi_sd = sd(ndvi)) %>% 
    group_by(MMSA, year) %>%
    # average NDVI across all months in each year
    summarize(ndvi_mean = mean(ndvi), ndvi_sd = first(ndvi_sd)) %>% 
    # drop geometry
    st_drop_geometry() %>% 
    # remove grouping
    ungroup() %>% 
    # pivot to wide format
    pivot_wider(names_from = year, values_from = ndvi_mean)
  
  return(ndvi_summary)
  
}

get_full_data_strat <- function(brfss, ndvi_summary, mmsa) {
    
  svy_pre <- brfss[[1]] %>%
    # create survey object with weights
    as_survey(strata = STSTR, weights = MMSAWT) %>% 
    # summarize counts stratified by housing tenure
    svyby(~mh2, by = ~MMSA+MMSANAME+owner, design = ., svytotal, na.rm = TRUE) %>% 
    # specify year for inner join
    rename_with(.cols = c("MMSANAME", "mh20", "mh21"), ~ paste0(.x, "_pre")) %>% 
    # clean name
    mutate(MMSANAME_pre = sub(", Metro", " Metro", MMSANAME_pre), 
           MMSANAME_pre = sub(", Micro", " Micro", MMSANAME_pre))
  
  svy_post <- brfss[[2]] %>%
    # create survey object with weights
    as_survey(strata = STSTR, weights = MMSAWT) %>% 
    # summarize counts stratified by housing tenure
    svyby(~mh2, by = ~MMSA+MMSANAME+owner, design = ., svytotal, na.rm = TRUE) %>% 
    # specify year for inner join
    rename_with(.cols = c("MMSANAME", "mh20", "mh21"), ~ paste0(.x, "_post"))
  
  # to assign regions to MMSAs
  regions <- states(year = 2019) %>% 
    dplyr::select(REGION, DIVISION, STUSPS) %>% 
    st_drop_geometry()
  
  # rename start year NDVI column
  ndvi_summary_1 <- ndvi_summary %>% 
    rename(ndvi_pre = `2011`)
  
  # join datasets
  full_data_strat <- inner_join(svy_pre, svy_post, by = c("MMSA", "owner")) %>%
    # sum weighted counts for mental distress prevalence denominator and compute prevalence
    mutate(total_pre = mh20_pre+mh21_pre, mh_prop_pre = mh21_pre/total_pre,
           total_post = mh20_post+mh21_post, mh_prop_post = mh21_post/total_post) %>% 
    # select columns
    dplyr::select(MMSA, MMSANAME_pre, MMSANAME_post, owner, mh_prop_pre, mh_prop_post) %>% 
    # extract state abbreviation (first state if multiple states hyphenated)
    mutate(STUSPS = str_match(MMSANAME_pre, regex(", (.{2})"))[,2]) %>% 
    # join region
    left_join(regions, by = "STUSPS") %>%
    # join NDVI to BRFSS counts for all sample MMSAs
    inner_join(ndvi_summary_1[,c("MMSA", "ndvi_pre", "2019", "ndvi_sd")], by = c("MMSA")) %>%
    # join ACS estimates
    left_join(mmsa, by = "MMSA") %>%
    # compute change in NDVI (scaled by factor of 10 to get 0.1-unit increases) and mental distress, 2011-2019
    mutate(ndvi_post_x10 = `2019`*10, ndvi_pre_x10 = ndvi_pre*10, diff_ndvi = ndvi_post_x10 - ndvi_pre_x10, diff_mh = mh_prop_post-mh_prop_pre)
  
  return(full_data_strat)
  
}

get_mmsa_to_remove <- function(brfss) {
  
  sample_names <- unique(brfss[[1]][c("MMSA", "MMSANAME")]) %>%
    # join 2011 BRFSS MMSAs to 2019 BRFSS MMSAs by MMSA ID
    inner_join(unique(brfss[[2]][c("MMSA", "MMSANAME")]), by = "MMSA") %>%
    # filter for contiguous US
    filter(!str_detect(MMSANAME.y, ", HI"), !str_detect(MMSANAME.y, ", PR"), !str_detect(MMSANAME.y, ", AK")) %>% 
    # clean names to match delineation file
    mutate(MMSANAME = sub(regex(" (Metro|Micro)politan Statistical Area"), "", MMSANAME.x))
  
  # # download Census historical MMSA delineation file - data lost when downloading
  # if(!file.exists(here("data", "metro_area_history_1950_2020.xls"))){download.file("https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/historical-delineation-files/metro_area_history_1950_2020.xls", destfile = here("data", "metro_area_history_1950_2020.xls"))}
  
  mmsa_history <- read_excel(here("data", "metro_area_history_1950_2020.xls")) %>%
    # select columns
    dplyr::select(-`March 2020 Component`) %>% 
    # fill MMSA names downward
    fill(Title, .direction = "down") %>% 
    # group by MMSA
    group_by(Title) %>% 
    # fill county, GEOID, and state downward within each MMSA
    fill(GEOID, County, State, .direction = "down") %>% 
    # filter to rows with changes in 2012-2019
    filter(str_detect(Date, regex(str_flatten(2012:2019, collapse = "|")))) %>% 
    # clean name to match sample names
    mutate(Title = sub(regex(" (Metro|Micro)politan Statistical Area"), "", Title)) %>% 
    # filter for areas in sample and actions beside title/status changes
    filter(Title %in% sample_names$MMSANAME, !str_detect(Action, regex("title", ignore_case = TRUE)), !str_detect(Action, regex("Status changed", ignore_case = TRUE))) %>% 
    # indicator variables for county added vs. deleted
    mutate(added = if_else(str_detect(Action, regex("added", ignore_case = TRUE)), 1, 0)) %>% 
    mutate(deleted = if_else(str_detect(Action, regex("deleted", ignore_case = TRUE)), 1, 0))
  
  boundaries_changed <- mmsa_history %>% 
    # group by MMSA and county
    group_by(Title, GEOID, County) %>% 
    # count number of times a county is added or deleted from an MMSA
    summarize(times_added = sum(added), times_deleted = sum(deleted)) %>% 
    # filter out rows in each MMSA where a county is added then deleted
    filter(times_added != times_deleted)
  
  # get 2019 MMSA names and GEOIDs
  mmsa_19 <- get_acs(geography = "cbsa", variables = "B01003_001", year = 2019, survey = "acs5") %>% 
    dplyr::select(GEOID, NAME)
  
  # get population counts by MMSA by year
  mmsa_pop_12 <- get_acs(geography = "cbsa", variables = "B01003_001", year = 2012, survey = "acs5") %>% 
    # join 2019 names
    left_join(mmsa_19, by = "GEOID") %>% 
    # clean title
    mutate(Title = sub(regex(" (Metro|Micro).+"), "", NAME.y)) %>% 
    # select vars
    dplyr::select(Title, mmsa_pop = estimate)
  
  mmsa_pop_17 <- get_acs(geography = "cbsa", variables = "B01003_001", year = 2017, survey = "acs5") %>% 
    # join 2019 names
    left_join(mmsa_19, by = "GEOID") %>% 
    # clean title
    mutate(Title = sub(regex(" (Metro|Micro).+"), "", NAME.y)) %>% 
    # select vars
    dplyr::select(Title, mmsa_pop = estimate)
  
  # get population counts by county by year
  co_pop_12 <- get_acs(geography = "county", variables = "B01003_001", year = 2012, survey = "acs5")
  co_pop_17 <- get_acs(geography = "county", variables = "B01003_001", year = 2017, survey = "acs5")
  
  # # get metro division names
  # sample_div <- sample_names %>%
  #   filter(str_detect(MMSANAME, "Division")) %>%
  #   # clean name
  #   mutate(name = sub(" Metropolitan Division", "", MMSANAME)) %>%
  #   mutate(name = if_else(name == "Nassau-Suffolk, NY", "Nassau County-Suffolk County, NY", name))
  # 
  # # get metro division history and filter for sample division names
  # div_13 <- read_excel(here("data", "feb_2013_cbsa.xls"), skip = 2) %>%
  #   filter(`Metropolitan Division Title` %in% sample_div$name) %>%
  #   dplyr::select(`Metropolitan Division Title`, `County/County Equivalent`, `FIPS State Code`, `FIPS County Code`)
  # div_15 <- read_excel(here("data", "jul_2015_cbsa.xls"), skip = 2) %>%
  #   filter(`Metropolitan Division Title` %in% sample_div$name) %>%
  #   dplyr::select(`Metropolitan Division Title`, `County/County Equivalent`, `FIPS State Code`, `FIPS County Code`)
  # div_17 <- read_excel(here("data", "aug_2017_cbsa.xls"), skip = 2) %>%
  #   filter(`Metropolitan Division Title` %in% sample_div$name) %>%
  #   dplyr::select(`Metropolitan Division Title`, `County/County Equivalent`, `FIPS State Code`, `FIPS County Code`)
  # div_18_apr <- read_excel(here("data", "apr_2018_cbsa.xls"), skip = 2) %>%
  #   filter(`Metropolitan Division Title` %in% sample_div$name) %>%
  #   dplyr::select(`Metropolitan Division Title`, `County/County Equivalent`, `FIPS State Code`, `FIPS County Code`)
  # div_18_sep <- read_excel(here("data", "list1_Sep_2018.xls"), skip = 2) %>%
  #   filter(`Metropolitan Division Title` %in% sample_div$name | `Metropolitan Division Title` %in% c("Seattle-Bellevue-Kent, WA", "Fort Worth-Arlington-Grapevine, TX")) %>%
  #   dplyr::select(`Metropolitan Division Title`, `County/County Equivalent`, `FIPS State Code`, `FIPS County Code`) %>%
  #   # rename divisions
  #   mutate(`Metropolitan Division Title` = case_match(`Metropolitan Division Title`,
  #                                                     "Seattle-Bellevue-Kent, WA" ~ "Seattle-Bellevue-Everett, WA",
  #                                                     "Fort Worth-Arlington-Grapevine, TX" ~ "Fort Worth-Arlington, TX",
  #                                                     .default = `Metropolitan Division Title`))
  # 
  # # check if data frames identical
  # identical(div_13, div_15) # identical
  # identical(div_13, div_17) # identical
  # identical(div_13, div_18_apr) # identical
  # identical(div_13, div_18_sep) # not identical
  # 
  # check_divs <- div_13 %>%
  #   full_join(div_18_sep, by = c("FIPS State Code", "FIPS County Code")) %>%
  #   # filter for counties present in one year and not other
  #   filter(is.na(`Metropolitan Division Title.x`)|is.na(`Metropolitan Division Title.y`)) %>%
  #   mutate(GEOID = paste0(`FIPS State Code`, `FIPS County Code`)) %>%
  #   left_join(co_pop_17, by = "GEOID") %>%
  #   mutate(`Metropolitan Division Title` = coalesce(`Metropolitan Division Title.x`, `Metropolitan Division Title.y`))
  # 
  # div_eval <- div_18_apr %>%
  #   mutate(GEOID = paste0(`FIPS State Code`, `FIPS County Code`)) %>%
  #   left_join(co_pop_17, by = "GEOID") %>%
  #   filter(`Metropolitan Division Title` %in% c(check_divs$`Metropolitan Division Title.x`, check_divs$`Metropolitan Division Title.y`)) %>%
  #   group_by(`Metropolitan Division Title`) %>%
  #   summarize(pop_total = sum(estimate)) %>%
  #   right_join(check_divs, by = "Metropolitan Division Title") %>%
  #   mutate(prop_change = estimate/pop_total) %>%
  #   filter(prop_change >= 0.1) # 0 divisions to remove
    
  mmsa_eval <- mmsa_history %>% 
    # filter for counties with boundary changes that weren't reversed for 2012-2019
    filter(GEOID %in% boundaries_changed$GEOID) %>% 
    # extract year
    mutate(year = str_extract(Date, regex("\\d{4}")), year_prev = as.numeric(year)-1)
  
  eval_13 <- mmsa_eval %>% 
    # split off 2013 changes
    filter(year == "2013") %>% 
    # join MMSA pop counts
    left_join(mmsa_pop_12, by = "Title") %>% 
    # join county pop counts
    left_join(co_pop_12, by = "GEOID")
  
  eval_18 <- mmsa_eval %>% 
    # split off 2018 changes
    filter(year == "2018") %>% 
    # join MMSA pop counts
    left_join(mmsa_pop_17, by = "Title") %>% 
    # join county pop counts
    left_join(co_pop_17, by = "GEOID")
  
  # bind rows
  mmsa_to_remove <- bind_rows(eval_13, eval_18) %>% 
    # calculate proportion, county pop / MMSA pop, before merger/separation
    mutate(prop_change = estimate/mmsa_pop) %>% 
    # filter to relative changes of 10% or more - these will be removed from sample
    filter(prop_change >= 0.1) %>% 
    # join MMSA IDs
    left_join(sample_names[,c("MMSA", "MMSANAME")], by = c("Title" = "MMSANAME"))
  
  return(mmsa_to_remove)
  
}

get_full_data_overall <- function(brfss, ndvi_summary, mmsa) {
  
  # get summary data overall
  svy_pre_overall <- brfss[[1]] %>%
    # create survey object with weights
    as_survey(strata = STSTR, weights = MMSAWT) %>% 
    # summarize counts stratified by housing tenure
    svyby(~mh2, by = ~MMSA+MMSANAME, design = ., svytotal, na.rm = TRUE) %>% 
    # specify year for inner join
    rename_with(.cols = c("MMSANAME", "mh20", "mh21"), ~ paste0(.x, "_pre"))
  
  svy_post_overall <- brfss[[2]] %>%
    # create survey object with weights
    as_survey(strata = STSTR, weights = MMSAWT) %>% 
    # summarize counts stratified by housing tenure
    svyby(~mh2, by = ~MMSA+MMSANAME, design = ., svytotal, na.rm = TRUE) %>% 
    # specify year for inner join
    rename_with(.cols = c("MMSANAME", "mh20", "mh21"), ~ paste0(.x, "_post"))
  
  # to assign regions to MMSAs
  regions <- states(year = 2019) %>% 
    dplyr::select(REGION, DIVISION, STUSPS) %>% 
    st_drop_geometry()
  
  # rename start year NDVI column
  ndvi_summary_1 <- ndvi_summary %>% 
    rename(ndvi_pre = `2011`)
  
  # join datasets
  full_data_overall <- inner_join(svy_pre_overall, svy_post_overall, by = "MMSA") %>%
    # sum weighted counts for mental distress prevalence denominator and compute prevalence
    mutate(total_pre = mh20_pre+mh21_pre, mh_prop_pre = mh21_pre/total_pre,
           total_post = mh20_post+mh21_post, mh_prop_post = mh21_post/total_post) %>% 
    # select columns
    dplyr::select(MMSA, MMSANAME_pre, MMSANAME_post, mh_prop_pre, mh_prop_post) %>% 
    # extract state abbreviation (first state if multiple states hyphenated)
    mutate(STUSPS = str_match(MMSANAME_pre, regex(", (.{2})"))[,2]) %>% 
    # join region
    left_join(regions, by = "STUSPS") %>%
    # join NDVI to BRFSS counts for all sample MMSAs
    inner_join(ndvi_summary_1[,c("MMSA", "ndvi_pre", "2019", "ndvi_sd")], by = "MMSA") %>%
    # join ACS estimates
    left_join(mmsa, by = "MMSA") %>%
    # compute change in NDVI (scaled to get 0.1-unit increases) and mental distress, 2011-2019
    mutate(ndvi_post_x10 = `2019`*10, ndvi_pre_x10 = ndvi_pre*10, diff_ndvi = ndvi_post_x10 - ndvi_pre_x10, diff_mh = mh_prop_post-mh_prop_pre)
  
  return(full_data_overall)
}

get_models <- function(full_data_strat, full_data_overall) {
  
  data_owner1 <- full_data_strat %>% 
    # filter for homeowner prevalence estimates
    filter(owner == 1)
  
  data_owner0 <- full_data_strat %>% 
    # filter for non-homeowner prevalence estimates
    filter(owner == 0)
  
  # build model - overall
  model_overall <- lm(diff_mh ~ diff_ndvi + ndvi_sd + ndvi_pre_x10 + mh_prop_pre + REGION + REGION*diff_ndvi + prop_poverty + prop_nhw, data = full_data_overall)

  # build model - homeowners
  model_owner1 <- lm(diff_mh ~ diff_ndvi + ndvi_sd + ndvi_pre_x10 + mh_prop_pre + REGION + REGION*diff_ndvi + prop_poverty + prop_nhw, data = data_owner1)
  
  # build model - non-homeowners
  model_owner0 <- lm(diff_mh ~ diff_ndvi + ndvi_sd + ndvi_pre_x10 + mh_prop_pre + REGION + REGION*diff_ndvi + prop_poverty + prop_nhw, data = data_owner0)
  
  models <- list(model_overall, model_owner1, model_owner0)
  
  return(models)
  
}

get_models_s2 <- function(full_data_strat, full_data_overall, mmsa_to_remove) {
  
  full_data_overall_s2 <- full_data_overall %>% 
    # filter out MMSAs with substantial boundary changes
    filter(!MMSA %in% mmsa_to_remove$MMSA)
  
  full_data_strat_s2 <- full_data_strat %>% 
    # filter out MMSAs with substantial boundary changes
    filter(!MMSA %in% mmsa_to_remove$MMSA)

  data_owner1 <- full_data_strat_s2 %>% 
    # filter for homeowner prevalence estimates
    filter(owner == 1) 
  
  data_owner0 <- full_data_strat_s2 %>% 
    # filter for non-homeowner prevalence estimates
    filter(owner == 0)
  
  # build model - overall
  model_overall <- lm(diff_mh ~ diff_ndvi + ndvi_sd + ndvi_pre_x10 + mh_prop_pre + REGION + REGION*diff_ndvi + prop_poverty + prop_nhw, data = full_data_overall_s2)
  
  # build model - homeowners
  model_owner1 <- lm(diff_mh ~ diff_ndvi + ndvi_sd + ndvi_pre_x10 + mh_prop_pre + REGION + REGION*diff_ndvi + prop_poverty + prop_nhw, data = data_owner1)
  
  # build model - non-homeowners
  model_owner0 <- lm(diff_mh ~ diff_ndvi + ndvi_sd + ndvi_pre_x10 + mh_prop_pre + REGION + REGION*diff_ndvi + prop_poverty + prop_nhw, data = data_owner0)
  
  models_s2 <- list(model_overall, model_owner1, model_owner0)
  
  return(models_s2)
  
}

get_ndvi_summer <- function(sf) {
  
  # load monthly NDVI for 2011-2019, keep only May through Aug of each year
  ndvi_files <- list.files(path = here("data", "NDVI")) %>% 
    .[c(5:8,17:20,29:32,41:44,53:56,65:68,77:80,89:92,101:104)]
  
  # create raster for each NDVI file
  ndvi <- purrr::map(ndvi_files, ~ rast(here("data", "NDVI", .x)))
  
  # unzip GPW data
  unzip(here("data", "gpw-v4-population-count-rev11_2020_30_sec_tif.zip"), files = "gpw_v4_population_count_rev11_2020_30_sec.tif", exdir = here("data"))
  
  # load and reproject Gridded Population of the World for population weighting
  gpw <- rast(here("data", "gpw_v4_population_count_rev11_2020_30_sec.tif")) %>%
    # change CRS
    project(crs(sf)) %>%
    # match extent
    crop(sf)
  
  # reproject NDVI raster to shapefile CRS
  ndvi_proj <- lapply(ndvi, project, y = crs(sf)) %>% 
    # crop to match extent
    lapply(., crop, sf)
  
  # resample gpw to match NDVI grid cells
  gpw_ndvi <- resample(gpw, ndvi_proj[[1]]) %>% 
    # replace NA with 0
    classify(., cbind(NA, 0))
  
  # extract NDVI weighted average over each polygon  
  ndvi_vals <- purrr::map(ndvi_proj, ~ exact_extract(.x, sf, fun = "weighted_mean", weights = gpw_ndvi))
  
  # initialize empty list
  ndvi_sf <- list()
  
  # append average NDVI values to shapefile
  for(i in 1:length(ndvi_vals)) {
    
    ndvi_sf[[i]] <- sf %>% 
      mutate(ndvi = ndvi_vals[[i]], year = str_extract(ndvi_files[i], regex("\\d{4}")))
  }
  
  ndvi_summer <- ndvi_sf %>%
    # bind rows for NDVI by area across all months
    bind_rows() %>%
    group_by(MMSA) %>% 
    # compute standard deviation of monthly NDVI values
    mutate(ndvi_sd = sd(ndvi)) %>% 
    group_by(MMSA, year) %>%
    # average NDVI across all months in each year
    summarize(ndvi_mean = mean(ndvi), ndvi_sd = first(ndvi_sd)) %>% 
    # drop geometry
    st_drop_geometry() %>% 
    # remove grouping
    ungroup() %>% 
    # pivot to wide format
    pivot_wider(names_from = year, values_from = ndvi_mean)
  
  return(ndvi_summer)
  
}

get_tracts_brfss <- function() {
  
  # list contiguous state names + DC
  sample_states <- c(state.abb[!state.abb %in% c("AK","HI")], "DC")
  
  # load 500 cities mental distress prevalence estimates for 2014 (2016 release)
  tracts_14_500cities <- read.csv(here("data", "500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2016_release_20250622.csv")) 
  
  tracts_14 <- tracts_14_500cities %>% 
    # select variables
    dplyr::select(StateAbbr, TractFIPS, Place_TractID, Population2010, MHLTH_CrudePrev) %>% 
    mutate(GEOID = str_pad(TractFIPS, width = 11, side = "left", pad = "0")) %>% 
    # filter for contiguous states
    filter(StateAbbr %in% sample_states) %>% 
    # compute weighted mean of tract portions for each split tract
    group_by(GEOID) %>% 
    summarize(MDP_14 = weighted.mean(MHLTH_CrudePrev, w = Population2010)) %>% 
    ungroup()
  
  # download ZIP file for modified 2010 tract polygons for 2016 BRFSS 500 Cities release
  if(!file.exists(here("data", "500Cities_Tracts_11082016.zip"))){download.file("https://chronicdata.cdc.gov/download/x7zy-2xmx/application%2Fzip", destfile = here("data", "500Cities_Tracts_11082016.zip"))}
  
  # unzip file
  unzip(here("data", "500Cities_Tracts_11082016.zip"), exdir = here("data", "500Cities_Tracts_11082016"))
  
  # # load shapefile for 2016 release
  # tracts_14_sf <- st_read(here("data", "500Cities_Tracts_11082016")) %>% 
  #   # join 2016 release (with 2014 mental distress estimates) to appropriate shapefile
  #   full_join(tracts_14, by = c("plctract10" = "Place_TractID"))
  
  # load PLACES mental distress prevalence estimates for 2019 (2021 release)
  tracts_19_PLACES <- read.csv(here("data", "PLACES__Census_Tract_Data__GIS_Friendly_Format___2021_release_20250622.csv"))
  
  tracts_19 <- tracts_19_PLACES %>% 
    # select variables
    dplyr::select(StateAbbr, TractFIPS, MHLTH_CrudePrev) %>% 
    mutate(GEOID = str_pad(TractFIPS, width = 11, side = "left", pad = "0")) %>% 
    # filter for contiguous states 
    filter(StateAbbr %in% sample_states)
  
  # load 2015 census tract boundaries (used for 2021 release)
  tracts_sf <- get_acs(geography = "tract", survey = "acs5", variables = "B00001_001",
                       state = sample_states, year = 2015, geometry = TRUE) %>%
    # join BRFSS
    right_join(tracts_19, by = "GEOID") %>% # tract with GEOID 06037930401 (in LA county) does not have polygon match in 2015 shapefile
    # join both datasets by GEOID, keep only 2015 boundary polygons
    right_join(tracts_14, by = "GEOID")
    
  # # identify tracts with multiple polygons/estimates in 2016 release but only one estimate in 2021 release
  # drop_tracts <- tracts_sf %>% 
  #   group_by(tract2010) %>% 
  #   summarize(count = n()) %>% 
  #   filter(count>1) %>% 
  #   pull(tract2010)
  # 
  # tracts_sample <- tracts_sf %>% 
  #   # filter out tracts identified above
  #   filter(!tract2010 %in% drop_tracts)
  
  # pause execution before next API request - for 60 seconds
  Sys.sleep(60)
  
  # load ACS covariate estimates for 2014
  tracts_acs <- get_acs(geography = "tract", survey = "acs5", year = 2014, 
                        # in renter-occupied unit
                        variables = c("B07013_001", "B07013_003",
                                      # poverty
                                      "B06012_001", "B06012_002",
                                      # ethnoracial group
                                      "B03002_001", "B03002_003"),
                        state = sample_states,
                        output = "wide", geometry = FALSE)
  
  # identify sample tracts missing geometry
  missing_geo <- tracts_sf %>% 
    filter(st_is_empty(geometry)) %>% 
    pull(GEOID)
  
  # find 2010 tract boundary for GEOID 06037930401 in LA county (missing from 2015 tract shapefile)
  tract_add <- tracts(state = str_sub(missing_geo, start = 1L, end = 2L), county = str_sub(missing_geo, start = 3L, end = 5L), year = 2010) %>% 
    filter(GEOID10 == missing_geo)
  
  tracts_sf$geometry[st_is_empty(tracts_sf$geometry)] <- tract_add$geometry
  
  tracts_brfss <- tracts_sf %>% 
    # join covariates
    left_join(tracts_acs, by = "GEOID") %>% 
    # compute proportion of householders living in renter-occupied units
    mutate(prop_renter = B07013_003E/B07013_001E) %>% 
    # compute proportion under poverty level
    mutate(prop_poverty = B06012_002E/B06012_001E) %>% 
    # compute proportion non-Hispanic White
    mutate(prop_nhw = B03002_003E/B03002_001E) %>% 
    # compute mental distress prevalence change
    mutate(mh_diff = MHLTH_CrudePrev-MDP_14) %>% 
    # rename GEOID to MMSA for get_ndvi_summary
    mutate(MMSA = GEOID)
  
  return(tracts_brfss)
  
}

get_model_tracts <- function(tracts_brfss, ndvi_summary_tracts) {
  
  # to assign regions to tracts
  regions <- states(year = 2019) %>% 
    dplyr::select(REGION, DIVISION, STUSPS) %>% 
    st_drop_geometry()
  
  # join data
  full_data_tracts <- tracts_brfss %>% 
    full_join(ndvi_summary_tracts, by = c("GEOID" = "MMSA")) %>% 
    # compute change in NDVI (scaled by factor of 10 to get 0.1-unit increases)
    mutate(ndvi_post_x10 = `2019`*10, ndvi_pre_x10 = `2014`*10, diff_ndvi = ndvi_post_x10 - ndvi_pre_x10) %>% 
    # identify region
    left_join(regions, by = c("StateAbbr" = "STUSPS")) %>% 
    # scale mental distress prevalence to match primary analysis
    mutate(MDP_14 = MDP_14/100, MHLTH_CrudePrev = MHLTH_CrudePrev/100) %>% 
    mutate(mh_diff = MHLTH_CrudePrev - MDP_14)
  
  # build model
  model_tracts <- lm(mh_diff ~ diff_ndvi + ndvi_sd + ndvi_pre_x10 + MDP_14 + 
                      REGION + REGION*diff_ndvi + prop_poverty + prop_nhw + prop_renter + 
                      prop_renter*diff_ndvi, data = full_data_tracts)

  ## check missingness
  # missing 2019 BRFSS MDP
  missing_brfss_19 <- full_data_tracts %>% 
    filter(is.na(MHLTH_CrudePrev)) %>% 
    nrow()
  
  # complete BRFSS data in 2014 and 2019
  complete_brfss <- full_data_tracts %>%
    filter(!is.na(mh_diff))
  
  # complete BRFSS data but missing one or more covariates
  complete_brfss_but_missing_covars <- complete_brfss %>% 
    filter(is.na(prop_poverty)|is.na(prop_nhw)|is.na(prop_renter)|is.na(ndvi_sd)) %>% 
    nrow()
  
  complete_brfss_but_missing_poverty <- complete_brfss %>% 
    filter(is.na(prop_poverty)) %>% 
    nrow()
  
  complete_brfss_but_missing_nhw <- complete_brfss %>% 
    filter(is.na(prop_nhw)) %>% 
    nrow()
  
  complete_brfss_but_missing_renter <- complete_brfss %>% 
    filter(is.na(prop_renter)) %>% 
    nrow()
  
  complete_brfss_but_missing_ndvi <- complete_brfss %>% 
    filter(is.na(ndvi_sd)) %>% 
    nrow()
  
  final_sample <- complete_brfss %>% 
    filter(!is.na(prop_poverty)&!is.na(prop_nhw)&!is.na(prop_renter)&!is.na(ndvi_sd)) %>% 
    nrow()
  
  missing_vars_tbl <- tibble(descrip = c("missing_brfss_19",
                                         "complete_brfss_but_missing_covars", 
                                         "complete_brfss_but_missing_poverty",
                                         "complete_brfss_but_missing_nhw",
                                         "complete_brfss_but_missing_renter",
                                         "complete_brfss_but_missing_ndvi",
                                         "final_sample"),
                             missing_rows = c(missing_brfss_19, 
                                            complete_brfss_but_missing_covars,
                                            complete_brfss_but_missing_poverty, 
                                            complete_brfss_but_missing_nhw,
                                            complete_brfss_but_missing_renter,
                                            complete_brfss_but_missing_ndvi,
                                            final_sample)
                             )
  
  write.csv(missing_vars_tbl, here("output", "missing_vars_tbl.csv"))
  
  return(model_tracts)
  
}

get_table_1 <- function(full_data_overall) {
  
  full_data_overall_summary <- full_data_overall %>% 
    # select vars
    dplyr::select(mh_prop_pre, mh_prop_post, diff_mh, ndvi_11 = ndvi_pre, ndvi_19 = `2019`, diff_ndvi, REGION, prop_poverty, prop_nhw) %>% 
    # recode region names
    mutate(REGION = case_match(REGION, "1" ~ "Northeast", "2" ~ "Midwest", "3" ~ "South", "4" ~ "West")) %>% 
    # rescale NDVI change back to get absolute change unscaled
    mutate(diff_ndvi = diff_ndvi/10)
  
  labels_2 <- list(mh_prop_pre ~ "Mental distress prevalence, 2011",
                   mh_prop_post ~ "Mental distress prevalence, 2019",
                   diff_mh ~ "Change in mental distress prevalence, 2011-2019",
                   ndvi_11 ~ "NDVI, 2011",
                   ndvi_19 ~ "NDVI, 2019",
                   diff_ndvi ~ "Change in NDVI, 2011-2019",
                   REGION ~ "Region",
                   prop_poverty ~ "Poverty rate, 2011",
                   prop_nhw ~ "Proportion of residents who are non-Hispanic White, 2011"
                   )
  
  overall_tbl <- tbl_summary(data = full_data_overall_summary,
                           statistic = list(all_continuous() ~ "{mean} ({sd})"),
                           label = labels_2,
                           digits = list(all_continuous() ~ c(3, 3),
                                         all_categorical() ~ c(0,1))) %>% 
    # convert to tibble for combining
    as_tibble()
  
  table_1 <- overall_tbl %>% 
    # clean names
    rename("Measure" = "**Characteristic**", "Overall" = "**N = 109**")
  
  write.csv(table_1, here("output", "table_1.csv"))
  
  # find lowest and highest mental distress prevalence in 2011 and 2019
  lowest_2011 <- full_data_overall %>% 
    filter(mh_prop_pre == min(mh_prop_pre)) %>% 
    dplyr::select(name = MMSANAME_pre, prop = mh_prop_pre) %>% 
    mutate(year = 2011)
  
  highest_2011 <- full_data_overall %>% 
    filter(mh_prop_pre == max(mh_prop_pre)) %>% 
    dplyr::select(name = MMSANAME_pre, prop = mh_prop_pre) %>% 
    mutate(year = 2011)
  
  lowest_2019 <- full_data_overall %>% 
    filter(mh_prop_post == min(mh_prop_post)) %>% 
    dplyr::select(name = MMSANAME_post, prop = mh_prop_post) %>% 
    mutate(year = 2019)
  
  highest_2019 <- full_data_overall %>% 
    filter(mh_prop_post == max(mh_prop_post)) %>% 
    dplyr::select(name = MMSANAME_post, prop = mh_prop_post) %>% 
    mutate(year = 2019)
  
  # bind rows
  mental_distress_min_max_2011_2019 <- bind_rows(lowest_2011, highest_2011, lowest_2019, highest_2019)
  
  # save output
  write.csv(mental_distress_min_max_2011_2019, here("output", "mental_distress_min_max_2011_2019.csv"))
  
  return(table_1)
}

get_table_2 <- function(full_data_strat) {
  
  # get summary data stratified by housing tenure
  full_data_strat_summary <- full_data_strat %>% 
    as_tibble() %>% 
    dplyr::select(owner, mh_prop_pre, mh_prop_post, diff_mh) %>% 
    mutate(owner = case_match(owner, "0" ~ "Non-homeowner", "1" ~ "Homeowner"))
  
  labels_1 <- list(mh_prop_pre ~ "Mental distress prevalence, 2011",
                   mh_prop_post ~ "Mental distress prevalence, 2019",
                   diff_mh ~ "Change in mental distress prevalence, 2011-2019")
  
  table_2 <- tbl_summary(by = owner, data = full_data_strat_summary,
                           statistic = list(all_continuous() ~ "{mean} ({sd})"),
                           label = labels_1,
                           digits = list(all_continuous() ~ c(3, 3))) %>% 
    # convert to tibble for combining
    as_tibble()
  
  write.csv(table_2, here("output", "table_2.csv"))
  
  return(table_2)
  
}

get_figure_S1 <- function(mmsa) {
  
  # load MMSA polygons
  mmsa_map <- core_based_statistical_areas(year = 2011) %>% 
    # filter for conterminous US
    filter(!str_detect(NAME, regex(", (HI|PR|AK)"))) %>% 
    # indicator if area present in sample
    mutate(Sample = if_else(CBSAFP %in% mmsa$MMSA, "Yes", "No"))
  
  # load metro division polygons
  division_map <- metro_divisions(year = 2011) %>% 
    # filter for conterminous US
    filter(!str_detect(NAME, regex(", (HI|PR|AK)"))) %>% 
    # indicator if area present in sample
    mutate(Sample = if_else(METDIVFP %in% mmsa$MMSA | CBSAFP %in% mmsa$MMSA, "Yes", "No"))
  
  # color palette
  fills <- c("Yes" = "darkgreen", "No" = "grey", "None" = "white")
  
  # US map outline
  us <- states(year = "2019") %>% 
    # filter for conterminous U.S.
    filter(STUSPS %in% state.abb, !STUSPS %in% c("AK", "HI"))
    
  nation <- nation(year = "2019") %>% 
    # conterminous U.S.
    st_intersection(us)
  
  # plot figure S1
  figure_S1 <- ggplot() +
    geom_sf(data = nation, alpha = 0) +
    geom_sf(data = mmsa_map, aes(fill = Sample)) +
    geom_sf(data = division_map, aes(fill = Sample)) + 
    theme_minimal() + 
    scale_fill_manual(values = fills)
  
  # save figure S1
  ggsave(here("output", "figure_S1.png"), figure_S1, width = 10, height = 6, dpi = 400)
  
  return(figure_S1)
  
}

get_figure_S2 <- function(full_data_overall) {
 
  full_data_overall_1 <- full_data_overall %>% 
    # reverse scaling of NDVI
    mutate(diff_ndvi = diff_ndvi/10)
  
  ndvi_change_summary <- full_data_overall_1 %>% 
    pull(diff_ndvi) %>% 
    # get summary of area-level NDVI difference
    summary() %>% 
    c() %>% 
    as_tibble(rownames = "measure")
    
  # save output
  write.csv(ndvi_change_summary, here("output", "ndvi_change_summary.csv"))
   
  # plot figure S2
  figure_S2 <- ggplot() + 
    geom_histogram(data = full_data_overall_1, aes(x = diff_ndvi), fill = "#00a926", color = "black", binwidth = 0.005) +
    theme_minimal() +
    labs(x = "Change in Average Annual NDVI, 2011-2019", y = "Number of Metropolitan Areas") + 
    scale_x_continuous(breaks = seq(-0.04, 0.09, 0.01))
  
  # save figure S2
  ggsave(here("output", "figure_S2.png"), figure_S2, width = 10, height = 6, dpi = 400)
    
  return(figure_S2)
  
}

get_figure_S3 <- function(full_data_overall, full_data_strat) {
  
  data_owner1 <- full_data_strat %>% 
    # filter for homeowner prevalence estimates
    filter(owner == 1)
  
  data_owner0 <- full_data_strat %>% 
    # filter for non-homeowner prevalence estimates
    filter(owner == 0)
  
  figure_S3 <- list()
  
  # plot figure S3a - overall
  figure_S3[[1]] <- ggplot() +
    geom_histogram(data = full_data_overall, aes(x = diff_mh), fill = "purple", color = "black") +
    theme_minimal() +
    labs(x = "Change in Prevalence of 14+ Poor Mental Health Days, 2011-2019", y = "Number of Metropolitan Areas") + 
    scale_x_continuous(breaks = seq(-0.25, 0.25, 0.05))
  
  # plot figure S3b - owners
  figure_S3[[2]] <- ggplot() +
    geom_histogram(data = data_owner1, aes(x = diff_mh), fill = "purple", color = "black") +
    theme_minimal() +
    labs(x = "Change in Prevalence of 14+ Poor Mental Health Days, 2011-2019", y = "Number of Metropolitan Areas") + 
    scale_x_continuous(breaks = seq(-0.25, 0.25, 0.05))
  
  # plot figure S3c - renters
  figure_S3[[3]] <- ggplot() +
    geom_histogram(data = data_owner0, aes(x = diff_mh), fill = "purple", color = "black") +
    theme_minimal() +
    labs(x = "Change in Prevalence of 14+ Poor Mental Health Days, 2011-2019", y = "Number of Metropolitan Areas") + 
    scale_x_continuous(breaks = seq(-0.25, 0.25, 0.05))
  
  # save figure S3
  ggsave(here("output", "figure_S3a.png"), figure_S3[[1]], height = 6, width = 10, dpi = 400)
  ggsave(here("output", "figure_S3b.png"), figure_S3[[2]], height = 6, width = 10, dpi = 400)
  ggsave(here("output", "figure_S3c.png"), figure_S3[[3]], height = 6, width = 10, dpi = 400)
  
  mh_change_summary <- full_data_overall %>% 
    pull(diff_mh) %>% 
    summary() %>% 
    c() %>% 
    as_tibble(rownames = "measure")
  
  # save output
  write.csv(mh_change_summary, here("output", "mh_change_summary.csv"))
  
  return(figure_S3)
  
}

get_table_3 <- function(models, analysis) {
  
  # set variable labels
  labels_t2 <- list(diff_ndvi ~ "NDVI change, 2011-2019",
                    ndvi_sd ~ "Standard deviation in monthly average NDVI",
                    ndvi_pre_x10 ~ "Average NDVI, 2011",
                    mh_prop_pre ~ "Mental distress prevalence, 2011",
                    REGION ~ "Region",
                    `diff_ndvi:REGION` ~ "NDVI change * Region",
                    prop_poverty ~ "Poverty rate, 2011",
                    prop_nhw ~ "Proportion of residents who are non-Hispanic White, 2011"
                    )
  
  overall_tbl <- tbl_regression(models[[1]], label = labels_t2, estimate_fun = style_ratio, 
                            pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>% 
    as_tibble()
  
  owner1_tbl <- tbl_regression(models[[2]], label = labels_t2, estimate_fun = style_ratio, 
                               pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>% 
    as_tibble()
    
  owner0_tbl <- tbl_regression(models[[3]], label = labels_t2, estimate_fun = style_ratio, 
                               pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>% 
    as_tibble()
  
  table_3 <- overall_tbl %>% 
    full_join(owner1_tbl, by = "**Characteristic**") %>% 
    full_join(owner0_tbl, by = "**Characteristic**") %>% 
    # clean names
    rename(beta_overall = 2, CI_overall = 3, p_overall = 4, beta_owner1 = 5, CI_owner1 = 6, p_owner1 = 7,
           beta_owner0 = 8, CI_owner0 = 9, p_owner0 = 10)
  
  # save output
  write.csv(table_3, here("output", paste0("table_", analysis, ".csv")))
  
  return(table_3)
  
}

get_missing_tbl <- function(brfss, mmsa) {
  
  respondents_2011_sample <- brfss[[1]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    nrow()
  
  respondents_2019_sample <- brfss[[2]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    nrow()
  
  # missing values for mental distress
  missing_mh_11 <- brfss[[1]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(is.na(MENTHLTH) | (MENTHLTH %in% c(77,99))) %>%
    nrow()
  
  missing_mh_19 <- brfss[[2]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(is.na(MENTHLTH) | (MENTHLTH %in% c(77,99))) %>%
    nrow()
  
  # missing values for housing tenure
  missing_owner_11 <- brfss[[1]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(is.na(RENTHOM1) | (RENTHOM1 %in% c(7,9))) %>%
    nrow()
  
  missing_owner_19 <- brfss[[2]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(is.na(RENTHOM1) | (RENTHOM1 %in% c(7,9))) %>%
    nrow()
  
  # included in final sample for unstratified prevalence estimates
  included_unstrat_11 <- brfss[[1]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(!is.na(MENTHLTH) & !(MENTHLTH %in% c(77,99))) %>%
    nrow()
  
  included_unstrat_19 <- brfss[[2]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(!is.na(MENTHLTH) & !(MENTHLTH %in% c(77,99))) %>%
    nrow()
  
  # included in final sample for calculating subgroup prevalence estimates (not calculated in group reporting "other arrangement")
  included_strat_11 <- brfss[[1]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(!is.na(RENTHOM1) & !is.na(MENTHLTH) & !(RENTHOM1 %in% c(7,9)) & !(MENTHLTH %in% c(77,99))) %>%
    nrow()
  
  included_strat_19 <- brfss[[2]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(!is.na(RENTHOM1) & !is.na(MENTHLTH) & !(RENTHOM1 %in% c(7,9)) & !(MENTHLTH %in% c(77,99))) %>%
    nrow()
  
  # summary tibble
  missing_tbl <- tibble(variable = c("respondents_2011_sample", "respondents_2019_sample",
                                    "missing_mh_11", "missing_mh_19",
                                    "missing_owner_11", "missing_owner_19",
                                    "included_unstrat_11", "included_unstrat_19",
                                    "included_strat_11", "included_strat_19"),
                        value = c(respondents_2011_sample, respondents_2019_sample,
                                  missing_mh_11, missing_mh_19,
                                  missing_owner_11, missing_owner_19,
                                  included_unstrat_11, included_unstrat_19,
                                  included_strat_11, included_strat_19))

  # save output
  write.csv(missing_tbl, here("output", "missing_or_included_values.csv"))
  
  return(missing_tbl)
  
}

get_tenure_tbl <- function(brfss, mmsa) {
  
  # summarize respondents by housing tenure stratum
  homeowners_11 <- brfss[[1]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(RENTHOM1 == 1) %>%
    nrow()
  
  homeowners_19 <- brfss[[2]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(RENTHOM1 == 1) %>%
    nrow()
  
  renters_11 <- brfss[[1]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(RENTHOM1 == 2) %>%
    nrow()
  
  renters_19 <- brfss[[2]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(RENTHOM1 == 2) %>%
    nrow()
  
  other_arrangement_11 <- brfss[[1]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(RENTHOM1 == 3) %>%
    nrow()
  
  other_arrangement_19 <- brfss[[2]] %>% 
    filter(MMSA %in% mmsa$MMSA) %>% 
    filter(RENTHOM1 == 3) %>%
    nrow()
  
  # summary tibble for housing tenure
  tenure_tbl <- tibble(variable = c("homeowners_11", "homeowners_19",
                                       "renters_11", "renters_19",
                                       "other_arrangement_11", "other_arrangement_19"),
                          value = c(homeowners_11, homeowners_19,
                                    renters_11, renters_19,
                                    other_arrangement_11, other_arrangement_19))
  
  # save output
  write.csv(tenure_tbl, here("output", "respondents_by_tenure.csv"))
  
  return(tenure_tbl)
  
}

get_table_S1 <- function(full_data_strat, full_data_overall) {
  
  data_owner1 <- full_data_strat %>% 
    # filter for homeowner prevalence estimates
    filter(owner == 1)
  
  data_owner0 <- full_data_strat %>% 
    # filter for non-homeowner prevalence estimates
    filter(owner == 0)
  
  # initialize lists
  uni <- list()
  uni_1 <- list()
  uni_0 <- list()
  
  # univariate analyses - overall
  uni[[1]] <- lm(diff_mh ~ diff_ndvi, data = full_data_overall)
  uni[[2]] <- lm(diff_mh ~ ndvi_sd, data = full_data_overall)
  uni[[3]] <- lm(diff_mh ~ ndvi_pre_x10, data = full_data_overall)
  uni[[4]] <- lm(diff_mh ~ mh_prop_pre, data = full_data_overall)
  uni[[5]] <- lm(diff_mh ~ REGION, data = full_data_overall)
  uni[[6]] <- lm(diff_mh ~ prop_poverty, data = full_data_overall)
  uni[[7]] <- lm(diff_mh ~ prop_nhw, data = full_data_overall)

  # univariate analyses - homeowners
  uni_1[[1]] <- lm(diff_mh ~ diff_ndvi, data = data_owner1)
  uni_1[[2]] <- lm(diff_mh ~ ndvi_sd, data = data_owner1)
  uni_1[[3]] <- lm(diff_mh ~ ndvi_pre_x10, data = data_owner1)
  uni_1[[4]] <- lm(diff_mh ~ mh_prop_pre, data = data_owner1)
  uni_1[[5]] <- lm(diff_mh ~ REGION, data = data_owner1)
  uni_1[[6]] <- lm(diff_mh ~ prop_poverty, data = data_owner1)
  uni_1[[7]] <- lm(diff_mh ~ prop_nhw, data = data_owner1)

  # univariate analyses - non-homeowners
  uni_0[[1]] <- lm(diff_mh ~ diff_ndvi, data = data_owner0)
  uni_0[[2]] <- lm(diff_mh ~ ndvi_sd, data = data_owner0)
  uni_0[[3]] <- lm(diff_mh ~ ndvi_pre_x10, data = data_owner0)
  uni_0[[4]] <- lm(diff_mh ~ mh_prop_pre, data = data_owner0)
  uni_0[[5]] <- lm(diff_mh ~ REGION, data = data_owner0)
  uni_0[[6]] <- lm(diff_mh ~ prop_poverty, data = data_owner0)
  uni_0[[7]] <- lm(diff_mh ~ prop_nhw, data = data_owner0)
  
  # set variable labels
  labels_t2 <- list(diff_ndvi ~ "NDVI change, 2011-2019",
                    ndvi_sd ~ "Standard deviation in monthly average NDVI",
                    ndvi_pre_x10 ~ "Average NDVI, 2011",
                    mh_prop_pre ~ "Mental distress prevalence, 2011",
                    REGION ~ "Region",
                    `diff_ndvi:REGION` ~ "NDVI change * Region",
                    prop_poverty ~ "Poverty rate, 2011",
                    prop_nhw ~ "Proportion of residents who are non-Hispanic White, 2011")
  
  # combine univariate regression summary tables for each dataset (overall and stratified)
  tbl <- purrr::map(uni, ~ as_tibble(tbl_regression(.x))) %>% 
    bind_rows()
  
  tbl_1 <- purrr::map(uni_1, ~ as_tibble(tbl_regression(.x))) %>% 
    bind_rows()
  
  tbl_0 <- purrr::map(uni_0, ~ as_tibble(tbl_regression(.x))) %>% 
    bind_rows()
  
  # combine datasets
  table_S1 <- tbl %>% 
    full_join(tbl_1, by = "**Characteristic**") %>% 
    full_join(tbl_0, by = "**Characteristic**")
  
  write.csv(table_S1, here("output", "table_S1.csv"))
  
  return(table_S1)
  
}

get_table_S4 <- function(model_tracts) {
  
  # set variable labels
  labels_S4 <- list(diff_ndvi ~ "NDVI change, 2014-2019",
                    ndvi_sd ~ "Standard deviation in monthly average NDVI",
                    ndvi_pre_x10 ~ "Average NDVI, 2014",
                    MDP_14 ~ "Mental distress prevalence, 2014",
                    REGION ~ "Region",
                    `diff_ndvi:REGION` ~ "NDVI change * Region",
                    prop_poverty ~ "Poverty rate, 2014",
                    prop_nhw ~ "Proportion of residents who are non-Hispanic White, 2014",
                    prop_renter ~ "Proportion of residents in renter-occupied unit, 2014",
                    `diff_ndvi:prop_renter` ~ "NDVI change * Proportion renters"
  )
  
  table_S4 <- tbl_regression(model_tracts, label = labels_S4, estimate_fun = style_ratio, 
                                pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>% 
    as_tibble()
  
  # save output
  write.csv(table_S4, here("output", "table_S4.csv"))
  
  return(table_S4)
  
}


