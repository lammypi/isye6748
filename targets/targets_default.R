#targets_default.R for pipeline

# Library
library(targets)
library(tarchetypes)

# tar_source - Uncomment once scripts are cleaned up, then remove # Shared Code section.
#tar_source("scripts")

# packages
tar_option_set(packages = c(
  "tidyverse", 
  "qgraph", 
  "bootnet", 
  "psychonetrics", 
  "polycor", 
  "NetworkComparisonTest", 
  "readxl", 
  "rlang", 
  "gt", 
  "effsize",
  "cowplot",
  "fmsb",
  "ggpubr",
  "roxygen2"
))

# Shared code
source("scripts/full_data_prep.R")
source("scripts/ggm_networks.R")
source("scripts/charts.R")


