
library(targets)
library(tarchetypes)
library(future)
library(here)

source("Functions.R")

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

tar_option_set(
  packages = c(
    "tidyverse",
    "here",
    "sf",
    "tigris",
    "terra",
    "stars",
    "exactextractr",
    "readxl",
    "hudr",
    "pdftools",
    "rvest",
    "survey",
    "srvyr",
    "dlnm",
    "splines",
    "gtsummary",
    "svydiags"
  ),
  format = 'rds', 
  workspace_on_error = TRUE
)

list(
  tar_target(brfss, {
    get_brfss()
  }),
  tar_target(mmsa, {
    get_mmsa(brfss = brfss)
  }),
  tar_target(ndvi_summary, {
    get_ndvi_summary(mmsa = mmsa)
  }),
  tar_target(remove, {
    remove(brfss = brfss)
  }),
  tar_target(full_data_lags, {
    merge_data(brfss = brfss, ndvi_summary = ndvi_summary, remove = remove)
  }),
  tar_target(full_svy, {
    get_full_svy(full_data_lags = full_data_lags)
  }),
  tar_target(model, {
    build_model(full_svy = full_svy)
  }),
  tar_target(model_owner0, {
    build_model_owner0(full_svy = full_svy)
  }),
  tar_target(model_owner1, {
    build_model_owner1(full_svy = full_svy)
  }),
  tar_target(table1, {
    get_table1(model = model)
  }),
  tar_target(figure1, {
    get_figure1(brfss = brfss, ndvi_summary = ndvi_summary, remove = remove)
  }),
  tar_target(figure2, {
    get_figure2(full_svy = full_svy, remove = remove)
  }),
  tar_target(map1, {
    get_map1(brfss = brfss, mmsa = mmsa, remove = remove)
  })
)




