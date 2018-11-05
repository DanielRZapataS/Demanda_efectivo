## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999)
# Change prompt
options(prompt="Demanda_efectivo> ", continue=" ") 

# Load utilities functions (change wd, auxiliary scripts...)
source("Scripts/utilities.R")
# Set up paths
set_environment() 
# Load configuration file and values
config <- fromJSON("settings.json")
# Runing forecasting and results process
# forecast_qcajeros()
# results_maker()
