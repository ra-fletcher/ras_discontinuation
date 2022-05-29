#*******************************************************************************
#
# Project: Canagliflozin and discontinuation of RAS inhibitors in the CREDENCE
#          trial
# Date:    06-May-2022
# Author:  Rob Fletcher
# Purpose: Load SAS data and save as CSV
#
#*******************************************************************************


# Load libraries ----------------------------------------------------------

source(here::here("src", "initialise_workspace.R"))


# Specify variables -------------------------------------------------------

# Data directories
credence_dir = "C:/SASDATA/CREDENCE_Shared/Data/Derived"

# Raw data file names
credence_files = credence_dir %>% 
  list.files(full.names = FALSE) %>% 
  str_remove_all("[.]sas7bdat$")

# Raw data file paths
credence_filepaths = credence_dir %>% list.files(full.names = TRUE)

# Output directories
credence_output = "data/credence"


# Specify function to read and write files --------------------------------

read_and_write_files = function(x, y, output) {
  
  assign(y, read_sas({{ x }}), envir = .GlobalEnv) %>%
    write_csv(str_glue("{output}/{y}.csv"))
  
  # Remove objects from the Global Environment
  rm(list = y, envir = .GlobalEnv)
  gc()
}


# Read and write data -----------------------------------------------------

# CREDENCE
purrr::walk2(
    credence_filepaths,
    credence_files,
    ~ read_and_write_files(.x, .y, output = credence_output)
  )

# Read and write Brendon Neuen's pre-prepared file
bneuen_data = 
  haven::read_sas(here::here("data", "bneuen", "ras_discontinuation.sas7bdat"))

write_csv(bneuen_data, here::here("output", "ras_discontinuation.csv"))
