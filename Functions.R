
get_brfss <- function() {

  # # unzip data
  # unzip(here("data", "MMSA11ASC.zip"), exdir = here("data"))
  # unzip(here("data", "MMSA12ASC.zip"), exdir = here("data"))
  # unzip(here("data", "MMSA2013_ASC.zip"), exdir = here("data"))
  # unzip(here("data", "MMSA2014_ASC.zip"), exdir = here("data"))
  # unzip(here("data", "mmsa2015_asc.zip"), exdir = here("data"))
  # unzip(here("data", "MMSA2016_ASC.zip"), exdir = here("data"))
  # unzip(here("data", "MMSA2017_ASC.zip"), exdir = here("data"))
  # unzip(here("data", "MMSA2018_ASC.zip"), exdir = here("data"))
  # unzip(here("data", "MMSA2019_ASC.zip"), exdir = here("data"))
  
  # load variable layout for each year of data
  get_brfss_1 <- function(year) {
    
    # list filenames
    filenames <- list.files(here("data"))
    
    # extract filename for variable layout corresponding to year
    var_file <- filenames[str_detect(filenames, regex(paste0("varlayout_", substr(year, nchar(year)-1,nchar(year))), ignore_case = TRUE))]
    
    # extract filename for data corresponding to year
    data_file <- filenames[str_detect(filenames, regex(paste0(year, "\\.asc"), ignore_case = TRUE))]
    
    columns <- pdf_text(here("data", var_file)) %>%
      strsplit(split = "\n") %>%
      unlist() %>%
      tibble(text = .) %>%
      filter(!str_detect(text, "Starting Column")) %>%
      mutate(`Starting Column` = as.numeric(str_extract(text, regex("\\d+\\s+\\b")))) %>%
      mutate(`Variable Name` = str_extract(text, regex("_?[A-Z]+\\d*"))) %>%
      mutate(`Field Length` = str_extract(text, regex("\\w\\s+\\d+"))) %>%
      mutate(`Field Length` = as.numeric(sub("\\w\\s+", "", x = `Field Length`))) %>%
      mutate(year = year, text = NULL) %>% 
      filter(!is.na(`Starting Column`), `Variable Name` != "W", `Variable Name` != "M", `Variable Name` != "T") #%>% 
      # mutate(`Variable Name` = case_when(`Variable Name` == "_MMSANAM" ~ "MMSANAME", `Variable Name` != "_MMSANAM" ~ `Variable Name`))
    
    brfss <- read.fwf(here("data", data_file), widths = columns$`Field Length`, col.names = columns$`Variable Name`, fileEncoding = "latin1") %>%
      rename(MMSANAME = `X_MMSANAM`) %>% 
      mutate(NAME = sub(regex(", (Metropolitan|Micropolitan) (Statistical Area|Division)"), "", MMSANAME)) %>%
      mutate(NAME = sub(regex("\\s\\s+"), "", NAME), YEAR = year)
    
    return(brfss)
  }
  
  get_brfss_from_html <- function(year, filename) {
    
    columns <- read_html(here("data", filename)) %>%
      html_table(fill = TRUE) %>% 
      as.data.frame() %>%
      mutate(year = year)
    
    brfss <- read.fwf(here("data", paste0("MMSA", year, ".ASC")), widths = columns$`Field.Length`, col.names = columns$`Variable.Name`, fileEncoding = "latin1") %>% 
      mutate(NAME = sub(regex(", (Metropolitan|Micropolitan) (Statistical Area|Division)"), "", MMSANAME)) %>%
      mutate(NAME = sub(regex("\\s\\s+"), "", NAME), YEAR = year)
    
    return(brfss)
  }
  
  # brfss_11 <- get_brfss_1(year = 11)
  # brfss_12 <- get_brfss_1(year = 12)
  # brfss_13 <- get_brfss_1(year = 2013)
  # brfss_14 <- get_brfss_1(year = 2014)
  # brfss_15 <- get_brfss_1(year = 2015)
  # brfss_16 <- get_brfss_1(year = 16)
  # brfss_17 <- get_brfss_from_html(year = 17, filename = "VarLayout_Table_17.html")
  # brfss_18 <- get_brfss_1(year = 18)
  # brfss_19 <- get_brfss_1(year = 19)
  
  # read in cleaned BRFSS CSV files for ease of loading
  # brfss_11 <- read.csv(here("data_clean", "brfss_11.csv")) # 2011 data not written/read in correctly
  brfss_12 <- read.csv(here("data_clean", "brfss_12.csv"))
  brfss_13 <- read.csv(here("data_clean", "brfss_13.csv"))
  brfss_14 <- read.csv(here("data_clean", "brfss_14.csv"))
  brfss_15 <- read.csv(here("data_clean", "brfss_15.csv"))
  brfss_16 <- read.csv(here("data_clean", "brfss_16.csv"))
  brfss_17 <- read.csv(here("data_clean", "brfss_17.csv"))
  brfss_18 <- read.csv(here("data_clean", "brfss_18.csv"))
  brfss_19 <- read.csv(here("data_clean", "brfss_19.csv"))
  
  brfss_list <- list(#brfss_11, 
                     brfss_12, brfss_13, brfss_14, brfss_15, brfss_16, brfss_17, brfss_18, brfss_19)
  
  # eventually move these mutate functions up into original function
  brfss <- purrr::map(brfss_list, ~ mutate(.x, mh2 = case_when(MENTHLTH == 88 ~ 0, 
     MENTHLTH %in% c(77,99) ~ NA, MENTHLTH %in% 1:13 ~ 0, MENTHLTH %in% 14:30 ~ 1)) %>% 
     dplyr::select(YEAR, NAME, X_MMSA, mh2, RENTHOM1, X_STSTR, X_MMSAWT))
  
  return(brfss)
}

get_mmsa <- function(brfss) {
  
  mmsa <- purrr::map(1:8, ~ core_based_statistical_areas(year = .x+2011)) %>% 
    bind_rows() %>%
    group_by(NAME) %>% 
    slice_head() %>% 
    ungroup()
  
  return(mmsa)
}

get_ndvi_summary <- function(mmsa) {

  if (!dir.exists(here("NDVI"))) {
    dir.create(here("NDVI"))
  }
  
  # unzip NDVI folder
  unzip(here("NDVI.zip"), exdir = here("NDVI"))
  
  # load monthly NDVI for 2011-2021
  ndvi_files <- list.files(path = here("NDVI"))
  
  # create raster for each NDVI file
  ndvi <- purrr::map(ndvi_files, ~ rast(here("NDVI", .x)))

  # unzip GPW data
  unzip(here("gpw-v4-population-count-rev11_2020_30_sec_tif.zip"), files = "gpw_v4_population_count_rev11_2020_30_sec.tif", exdir = here())
  
  # load and reproject Gridded Population of the World for population weighting
  gpw <- rast(here("gpw_v4_population_count_rev11_2020_30_sec.tif")) %>%
    # change CRS
    project(crs(ndvi[[1]])) %>%
    # match extent
    crop(ext(ndvi[[1]]))
  
  # resample gpw to match TCC grid cells
  gpw_ndvi <- resample(gpw, ndvi[[1]]) %>% 
    # replace NA with 0
    classify(., cbind(NA, 0))
  
  # reproject MMSA shapefile
  mmsa_proj <- st_transform(mmsa, crs = st_crs(ndvi[[1]]))
  
  # extract NDVI weighted average over each MMSA area  
  ndvi_vals <- purrr::map(ndvi, ~ exact_extract(.x, mmsa_proj, fun = "weighted_mean", weights = gpw_ndvi))
  
  # initialize empty list
  ndvi_mmsa <- list()
  
  # append average NDVI values to reprojected MMSA shapefile
  for(i in 1:length(ndvi_vals)) {
    
    ndvi_mmsa[[i]] <- mmsa_proj %>% 
      mutate(ndvi = ndvi_vals[[i]], year = str_extract(ndvi_files[i], regex("\\d{4}")))
    # mmsa_proj[,13+i] <- ndvi_vals[[i]]
  }
  
  ndvi_summary <- ndvi_mmsa %>%
    # bind rows for NDVI by MMSA across all months in 2010-2021
    bind_rows() %>% 
    group_by(NAME, year) %>%
    # average NDVI across all months in each year
    summarize(ndvi_mean = mean(ndvi)) %>% 
    # convert year to numeric
    mutate(year = as.numeric(year))
  
  return(ndvi_summary)
  
}

remove <- function(brfss) {
  
  # list unique MMSA's with data across all years
  mmsa_in_all_years <- brfss %>%
    bind_rows() %>%
    group_by(NAME, YEAR) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(NAME) %>%
    summarize(count = n()) %>%
    # filter for MMSAs appearing in all study years (change to 9 after including 2011)
    filter(count == 8) %>%
    pull(NAME)
  
  # randomly select MMSAs to exclude - temporary because as_survey not working with entire set of MMSAs
  set.seed(1649)
  remove <- sample(mmsa_in_all_years, 1) # don't remove Huntington-Ashland
  remove <- sample(mmsa_in_all_years, 2)
  
  return(remove)
}

merge_data <- function(brfss, ndvi_summary, remove) {
  
  # list unique MMSA's with data across all years
  mmsa_in_all_years <- brfss %>%
    bind_rows() %>%
    group_by(NAME, YEAR) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(NAME) %>%
    summarize(count = n()) %>%
    # filter for MMSAs appearing in all study years (change to 9 after including 2011)
    filter(count == 8) %>%
    pull(NAME)
  
  brfss_summary <- brfss %>%
    # bind rows across all years
    bind_rows() %>% 
    # filter for MMSAs with data for all years
    filter(NAME %in% mmsa_in_all_years) %>% 
    # clean year variable for merging
    mutate(year = str_pad(YEAR, 3, side = "left", pad = "0")) %>% 
    mutate(year = str_pad(year, 4, side = "left", pad = "2")) %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(mh2 = as.integer(mh2)) %>% 
    # recode homeowner variable
    mutate(owner = case_when(RENTHOM1 == 1 ~ 1, RENTHOM1 == 2 ~ 0, RENTHOM1 == 3 ~ 0, RENTHOM1 %in% c(7,9,NA) ~ NA)) %>% 
    # recode mh2, MMSA, stratum, and owner as factor variables
    mutate(mh2 = factor(mh2), owner = factor(owner), X_MMSA = factor(X_MMSA), X_STSTR = factor(X_STSTR))
  
  ndvi_tbl <- ndvi_summary %>% 
    st_drop_geometry() %>% 
    filter(NAME %in% mmsa_in_all_years) %>% 
    mutate(lag1 = year + 1, lag2 = year + 2, lag3 = year + 3, lag4 = year + 4, lag5 = year + 5)
  
  full_data_lags_1 <- ndvi_summary %>%
    # join BRFSS to NDVI with lag 0 years
    right_join(brfss_summary, by = c("NAME", "year")) %>%
    # join NDVI with lag 1 year
    rename(lag1 = year, ndvi_lag0 = ndvi_mean) %>% 
    left_join(ndvi_tbl[,c("NAME", "ndvi_mean", "lag1")], by = c("NAME", "lag1")) %>% 
    # join NDVI with lag 2 years
    rename(lag2 = lag1, ndvi_lag1 = ndvi_mean) %>% 
    left_join(ndvi_tbl[,c("NAME", "ndvi_mean", "lag2")], by = c("NAME", "lag2")) %>%
    # join NDVI with lag 3 years
    rename(lag3 = lag2, ndvi_lag2 = ndvi_mean) %>% 
    left_join(ndvi_tbl[,c("NAME", "ndvi_mean", "lag3")], by = c("NAME", "lag3")) %>% 
    # join NDVI with lag 4 years
    rename(lag4 = lag3, ndvi_lag3 = ndvi_mean) %>% 
    left_join(ndvi_tbl[,c("NAME", "ndvi_mean", "lag4")], by = c("NAME", "lag4")) %>% 
    # join NDVI with lag 5 years
    rename(lag5 = lag4, ndvi_lag4 = ndvi_mean) %>% 
    left_join(ndvi_tbl[,c("NAME", "ndvi_mean", "lag5")], by = c("NAME", "lag5")) %>% 
    # rename year variable
    rename(year = lag5, ndvi_lag5 = ndvi_mean) %>% 
    st_drop_geometry() %>% 
    ungroup() %>% 
    dplyr::select(-YEAR) 
    
  # # check which MMSAs have lowest sample sizes
  # data_summary <- full_data_lags_1 %>% 
  #   group_by(NAME) %>% 
  #   summarize(count = n())
  
  full_data_lags <- full_data_lags_1 %>% 
    # for now, exclude some MMSAs to avoid survey design error - 76 works
    filter(!str_detect(NAME, regex("HI|PR|AK")), !NAME %in% remove)
  
  return(full_data_lags)
}

get_full_svy <- function(full_data_lags) {
  
  # # using CDC example code:
  # brfssdsgn <- svydesign(
  #   id=~1, # indicates no clusters - same as leaving ids argument empty in as_survey()
  #   strata = ~X_STSTR,
  #   weights = ~X_MMSAWT,
  #   data = full_data_lags) # does this still work correctly if combining all years?
  
  full_svy <- full_data_lags %>%
    # create survey object with weights
    as_survey(strata = X_STSTR, weights = X_MMSAWT)

  return(full_svy)

}

build_model <- function(full_svy) {

  # build multivariable logistic regression
  model <- svyglm(mh2 ~ ns(ndvi_lag0 + ndvi_lag1 + ndvi_lag2 + ndvi_lag3 + ndvi_lag4 + 
    ndvi_lag5) + owner + ndvi_lag0*owner + ndvi_lag1*owner + ndvi_lag2*owner + 
    ndvi_lag3*owner + ndvi_lag4*owner + ndvi_lag5*owner + X_MMSA, design = full_svy, 
    family = quasibinomial)
 
  # summary(model)
  # extractAIC(model)
  
  return(model)

}

build_model_owner0 <- function(full_svy) {
  
  full_svy_owner0 <- full_svy %>% 
    # filter to non-homeowner subgroup
    filter(owner == 0)
  
  # build multivariable logistic regression for non-homeowner subgroup
  model_owner0 <- svyglm(mh2 ~ ns(ndvi_lag0 + ndvi_lag1 + ndvi_lag2 + ndvi_lag3 + 
    ndvi_lag4 + ndvi_lag5) + X_MMSA, design = full_svy_owner0, family = quasibinomial)
  
  # summary(model_owner0)
  # extractAIC(model_owner0)
  
  return(model_owner0)
}

build_model_owner1 <- function(full_svy) {
  
  full_svy_owner1 <- full_svy %>% 
    # filter to homeowner subgroup
    filter(owner == 1)
  
  # build multivariable logistic regression for homeowner subgroup
  model_owner1 <- svyglm(mh2 ~ ns(ndvi_lag0 + ndvi_lag1 + ndvi_lag2 + ndvi_lag3 + 
    ndvi_lag4 + ndvi_lag5) + X_MMSA, design = full_svy_owner1, family = quasibinomial)
  
  # summary(model_owner1)
  # extractAIC(model_owner1)
  
  return(model_owner1)
}

get_table1 <- function(model) {
  
  # fully adjusted model summary
  table1_coeffs <- tbl_regression(model, exponentiate = FALSE)
  table1 <- tbl_regression(model, exponentiate = TRUE)
  
  return(table1)
  
}

get_figure1 <- function(brfss, ndvi_summary, remove) {
  
  # list unique MMSA's with data across all years
  mmsa_in_sample <- brfss %>%
    bind_rows() %>%
    group_by(NAME, YEAR) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(NAME) %>%
    summarize(count = n()) %>%
    filter(!str_detect(NAME, regex("HI|PR|AK")), !NAME %in% remove) %>% 
    # filter for MMSAs appearing in all study years (change to 9 after including 2011)
    filter(count == 8) %>%
    pull(NAME)
  
  # summarize average NDVI by year across all MMSAs
  ndvi_overall <- ndvi_summary %>% 
    filter(NAME %in% mmsa_in_sample) %>% 
    group_by(year) %>% 
    summarize(ndvi_mean = mean(ndvi_mean, na.rm = TRUE))
  
  # # plot NDVI over time by MMSA
  # ggplot() + geom_line(data = ndvi_overall, aes(x = year, y = ndvi_mean, group = NAME)) +
    # theme(legend.position = NULL) + ggtitle("Mean annual NDVI by MMSA, 2007-2019") +
    # labs(x = "Year", y = "NDVI")
  
  # plot of average NDVI over time across all MMSAs
  ndvi_all_plot <- ggplot() + geom_line(data = ndvi_overall, aes(x = year, y = ndvi_mean)) +
    theme(legend.position = NULL) + labs(x = "Year", y = "NDVI") + theme_minimal()
  
  return(ndvi_all_plot)
  
  ggsave(ndvi_all_plot, filename = here("ndvi_all_plot.png"))
  
}

get_figure2 <- function(full_svy, remove) {
  
  # summarize excess poor mental health prevalence by MMSA by year
  mh2_summary <- full_svy %>% 
    group_by(NAME, year) %>% 
    summarize(mh2_prop = survey_mean(as.numeric(mh2), na.rm = TRUE))
  
  # summarize excess poor mental health prevalence by MMSA averaged across all years
  mh2_summary_allyears <- full_svy %>% 
    group_by(NAME, year) %>% 
    summarize(mh2_prop = survey_mean(as.numeric(mh2), na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(NAME) %>% 
    summarize(mh2_avg = mean(mh2_prop, na.rm = TRUE))
  
  # # summarize excess poor mental health prevalence overall by year - can we combine across MMSAs??
  # mh2_all_summary <- full_svy %>% 
  #   group_by(year) %>% 
  #   summarize(mh2_prop = survey_mean(as.numeric(mh2), na.rm = TRUE))
  
  mh2_mmsa_plot <- ggplot() + geom_histogram(data = mh2_summary_allyears, aes(x = mh2_avg-1), fill = "purple", color = "white") +
    labs(x = "Prevalence of 14+ Poor Mental Health Days", y = "Number of MMSAs") + theme_minimal()

  return(mh2_mmsa_plot)
  
  ggsave(mh2_mmsa_plot, filename = "brfss_histogram.png")
}

get_map1 <- function(brfss, mmsa, remove) {
  
  # list MMSAs in sample
  mmsa_in_sample <- brfss %>%
    bind_rows() %>%
    group_by(NAME, YEAR) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    group_by(NAME) %>%
    summarize(count = n()) %>%
    # filter for MMSAs appearing in all study years (change to 9 after including 2011)
    filter(count == 8) %>%
    filter(!NAME %in% remove) %>% 
    pull(NAME)
  
  mmsa_sample <- mmsa %>% 
    mutate(in_sample = if_else(NAME %in% mmsa_in_sample, "Yes", "No")) %>% 
    filter(!str_detect(NAME, regex(", (AK|HI|PR)")))
    
  mmsa_map <- ggplot() + geom_sf(data = mmsa_sample, aes(fill = in_sample)) +
    theme_minimal() + labs(fill = "In sample?") + scale_colour_brewer(type = "qual")
  
  ggsave(mmsa_map, filename = here("mmsa_map.png"))
}

# run_model_diagnostics <- function(full_data_lags, model) {
#   
#   # other
#   # plot(roc(full_data_lags$mh2, model), print.auc = TRUE)
#   
#   X <- model.matrix(model)
#   svycollinear(mod = X, w = full_data_lags$X_MMSAWT, Vcov = vcov(model), svyglm.obj = FALSE)
#   
# }
