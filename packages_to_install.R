# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-19
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) check if package is installed
# 2) install missing packages
# 3) load libraries
# Notes: This script is part of an RProject

################################################################################
# install packages
################################################################################

packages_to_install <- c(
  "cowplot",
  "dplyr",
  "DT",
  "ggplot2",
  "gridExtra",
  #"httr",
  "jsonlite",
  "purrr",
  "rjson",
  "shiny",
  "shinydashboard",
  "tidyr",
  "tidyverse" #,

)

new.packages <- packages_to_install[!(packages_to_install %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

################################################################################
# load libraries
################################################################################

lapply(packages_to_install, library, character.only = TRUE) |>
  #suppress output
  invisible()

################################################################################
# detach libraries
################################################################################

#detach("package:bs4Dash", unload = TRUE)