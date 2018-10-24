## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999)
# Change prompt
options(prompt="Aprovisionamiento1.1> ", continue=" ") 

# Load utilities functions (change wd, auxiliary scripts...)
source("Scripts/utilities.R")
# Set up paths
set_environment() 
# Load configuration file and values
config <- fromJSON("settings.json")
# Runing historical, forecasting and results process
#hist_maker()
#forecast_qcajeros()
results_maker()
