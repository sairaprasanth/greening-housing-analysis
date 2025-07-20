
# run these 3 lines 
library(targets)
library(tarchetypes)
library(future)

source("Functions.R")

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
options(tigris_use_cache = TRUE)

# required - define your Census API key here
census_key <- ""

tar_option_set(
  packages = c(
    "tidyverse",
    "here",
    "rio",
    "sf",
    "tigris",
    "terra",
    "stars",
    "exactextractr",
    "readxl",
    "pdftools",
    "rvest",
    "survey",
    "srvyr",
    "gtsummary",
    "svydiags",
    "tidycensus"
  ),
  format = 'rds', 
  workspace_on_error = TRUE
)

list(
  tar_target(install_census_key, {
    census_api_key(key = census_key)
  }),
  tar_target(brfss, {
    get_brfss()
  }),
  tar_target(mmsa, {
    get_mmsa(brfss = brfss)
  }),
  tar_target(ndvi_summary, {
    get_ndvi_summary(sf = mmsa, analysis = "main")
  }),
  tar_target(full_data_overall, {
    get_full_data_overall(brfss = brfss, ndvi_summary = ndvi_summary, mmsa = mmsa)
  }),
  tar_target(full_data_strat, {
    get_full_data_strat(brfss = brfss, ndvi_summary = ndvi_summary, mmsa = mmsa)
  }),
  tar_target(models, {
    get_models(full_data_strat = full_data_strat, full_data_overall = full_data_overall)
  }),
  # sensitivity analysis 1 - subset to areas without large boundary changes
  tar_target(mmsa_to_remove, {
    get_mmsa_to_remove(brfss = brfss)
  }),
  tar_target(models_s2, {
    get_models_s2(full_data_strat = full_data_strat, full_data_overall = full_data_overall, mmsa_to_remove = mmsa_to_remove)
  }),
  # sensitivity analysis - summertime NDVI
  tar_target(ndvi_summer, {
    get_ndvi_summer(sf = mmsa)
  }),
  tar_target(full_data_strat_summer, {
    get_full_data_strat(brfss = brfss, ndvi_summary = ndvi_summer, mmsa = mmsa)
  }),
  tar_target(full_data_overall_summer, {
    get_full_data_overall(brfss = brfss, ndvi_summary = ndvi_summer, mmsa = mmsa)
  }),
  tar_target(models_summer, {
    get_models(full_data_strat = full_data_strat_summer, full_data_overall = full_data_overall_summer)
  }),
  # secondary analysis - tracts
  tar_target(tracts_brfss, {
    get_tracts_brfss()
  }),
  tar_target(ndvi_summary_tracts, {
    get_ndvi_summary(sf = tracts_brfss, analysis = "tracts")
  }),
  tar_target(model_tracts, {
    get_model_tracts(tracts_brfss = tracts_brfss, ndvi_summary_tracts = ndvi_summary_tracts)
  }),
  # tables and figures
  tar_target(table_1, {
    get_table_1(full_data_overall = full_data_overall)
  }),
  tar_target(table_2, {
    get_table_2(full_data_strat = full_data_strat)
  }),
  tar_target(figure_S1, {
    get_figure_S1(mmsa = mmsa)
  }),
  tar_target(figure_S2, {
    get_figure_S2(full_data_overall = full_data_overall)
  }),
  tar_target(figure_S3, {
    get_figure_S3(full_data_overall = full_data_overall, full_data_strat = full_data_strat)
  }),
  tar_target(table_S1, {
    get_table_S1(full_data_overall = full_data_overall, full_data_strat = full_data_strat)
  }),
  tar_target(table_3, {
    get_table_3(models = models, analysis = "main")
  }),
  tar_target(table_S2, {
    get_table_3(models = models_s2, analysis = "boundaries")
  }),
  tar_target(table_S3, {
    get_table_3(models = models_summer, analysis = "summer")
  }),
  tar_target(table_S4, {
    get_table_S4(model_tracts = model_tracts)
  }),
  tar_target(missing_tbl, {
    get_missing_tbl(brfss = brfss, mmsa = mmsa)
  }),
  tar_target(tenure_tbl, {
    get_tenure_tbl(brfss = brfss, mmsa = mmsa)
  })
)
