
library(targets)
library(tarchetypes)
library(future)
library(here)

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
    "lme4",
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
    get_mmsa(brfss = brfss, start_year = 2011)
  }),
  tar_target(ndvi_summary, {
    get_ndvi_summary(mmsa = mmsa, start_year = 2011)
  }),
  tar_target(full_data_overall, {
    get_full_data_overall(brfss = brfss, ndvi_summary = ndvi_summary, mmsa = mmsa, start_year = 2011)
  }),
  tar_target(full_data_strat, {
    get_full_data_strat(brfss = brfss, ndvi_summary = ndvi_summary, mmsa = mmsa, start_year = 2011)
  }),
  tar_target(models, {
    get_models(full_data_strat = full_data_strat, full_data_overall = full_data_overall)
  }),
  # sensitivity analysis 1 - 2012 start year
  tar_target(mmsa_2012, {
    get_mmsa(brfss = brfss, start_year = 2012)
  }),
  tar_target(ndvi_summary_2012, {
    get_ndvi_summary(mmsa = mmsa_2012, start_year = 2012)
  }),
  tar_target(full_data_strat_2012, {
    get_full_data_strat(brfss = brfss, ndvi_summary = ndvi_summary_2012, mmsa = mmsa_2012, start_year = 2012)
  }),
  tar_target(full_data_overall_2012, {
    get_full_data_overall(brfss = brfss, ndvi_summary = ndvi_summary_2012, mmsa = mmsa_2012, start_year = 2012)
  }),
  tar_target(models_s1, {
    get_models(full_data_strat = full_data_strat_2012, full_data_overall = full_data_overall_2012)
  }),
  # sensitivity analysis 2 - subset to areas without large boundary changes
  tar_target(mmsa_to_remove, {
    get_mmsa_to_remove(brfss = brfss)
  }),
  tar_target(models_s2, {
    get_models_s2(full_data_strat = full_data_strat, full_data_overall = full_data_overall, mmsa_to_remove = mmsa_to_remove)
  }),
  # sensitivity analysis 3 - summer only
  tar_target(ndvi_summer, {
    get_ndvi_summer(mmsa = mmsa)
  }),
  tar_target(full_data_strat_summer, {
    get_full_data_strat(brfss = brfss, ndvi_summary = ndvi_summer, mmsa = mmsa, start_year = 2011)
  }),
  tar_target(full_data_overall_summer, {
    get_full_data_overall(brfss = brfss, ndvi_summary = ndvi_summer, mmsa = mmsa, start_year = 2011)
  }),
  tar_target(models_summer, {
    get_models(full_data_strat = full_data_strat_summer, full_data_overall = full_data_overall_summer)
  }),
  # tables and figures
  tar_target(table_1, {
    get_table_1(full_data_strat = full_data_strat, full_data_overall = full_data_overall)
  }),
  tar_target(figure_A1, {
    get_figure_A1(mmsa = mmsa)
  }),
  tar_target(figure_A2, {
    get_figure_A2(full_data_overall = full_data_overall)
  }),
  tar_target(figure_A3, {
    get_figure_A3(full_data_overall = full_data_overall)
  }),
  tar_target(table_A1, {
    get_table_A1(full_data_overall = full_data_overall, full_data_strat = full_data_strat)
  }),
  tar_target(table_2, {
    get_table_2(models = models, analysis = "main")
  }),
  tar_target(table_3, {
    get_table_2(models = models_s1, analysis = "s1")
  }),
  tar_target(table_4, {
    get_table_2(models = models_s2, analysis = "s2")
  }),
  tar_target(table_5, {
    get_table_2(models = models_summer, analysis = "s3")
  })
)
