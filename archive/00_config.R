# 00_config: load necessary libraries, create the directory tree, source basic functions, and loading all input data into object
# Sourcing functions that I will use
source('~/R/setup.R')
source('~/R/basic_functions.R')
source('~/R/log_functions.R')

# Manual input: creating objects for input data if it does not already exist
if (!(exists("INPUT_DATA"))) {
        
        # Path of downloaded the data in native xlsx file downloaded to ~/INPUT directory as csv
        INPUT_FN <- "INPUT/MSK Data Quality Test - Data.csv"
        
        # Loading data into object while also logging the activity to ~/project_log.csv
        INPUT_DATA <- my_read_csv(INPUT_FN)
        
        # Removing `INPUT_FN` object
        rm(INPUT_FN)
        
}

