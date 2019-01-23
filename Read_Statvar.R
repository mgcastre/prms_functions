# Function to read PRMS/GSFLOW statvar file (output_statvar.dat)
# Created By: R. Niswonger/M. G. Castrellon

# Reads a statvar file (main output from PRMS) and returns a data frame

# Required Library
library(dplyr)
library(lubridate)

# Function
read.statvar <- function(file_location) {
  # Number of elements file (columns with output data)
  elements <- read.table(file_location, nrows = 1)$V1
  
  # Read values from PRMS statvar file, skip headers
  statvar <- read.table(file_location, sep=" ", skip=elements+1)
  
  # Remove NA values
  statvar <- statvar[,colSums(is.na(statvar))<nrow(statvar)]
  
  # Read headers from statvar and create column with full 
  read.table(file_location, skip=1, sep=" ", nrows = elements, row.names = NULL, stringsAsFactors = FALSE) %>% 
    mutate(full_header = paste(V1,V2,sep="_")) %>% 
    select(full_header) -> statvar_header
  
  # Add headers for the row number and dat-time stamp
  newnames <- c("rownum","Year","Month","Day","hour","min","sec",statvar_header$full_header)
  
  # Assigns column header names to the DF that has values
  colnames(statvar) <- newnames
  
  # Clean final table
  statvar %>% mutate(Date = ymd(paste(Year,Month,Day, sep="-"))) %>% 
    select(Date,everything(),-rownum,-hour,-min,-sec) -> final_statvar 
  
  # Return final table
  return(final_statvar)
}
                   