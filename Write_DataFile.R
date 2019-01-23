# Functions to Write PRMS Climate Data File (input_climate.data)
# Created By: M. G. Castrellon

# The climate data is required as a table (data frame) with the following format:
##         Date Station Sensor Value
## 1 1975-06-19  122006 PRECIP  18.2
## 2 1975-06-20  122006 PRECIP   4.3
## 3 1975-06-21  122006 PRECIP   5.2
## 4 1975-06-22  122006 PRECIP   2.5
## 5 1975-06-23  122006 PRECIP   1.1
## 6 1975-06-24  122006 PRECIP   1.9
# Where "Station" is a unique ID for the met station and "Sensor" refers to the type of measurment (PRECIP, TMAX, TMIN, FLOW)
# Units: Temperature = Celsius, Precip = in/day, Streamflow = cfs
# Reference: https://pubs.usgs.gov/tm/6b7/

# Required Libraries
library(dplyr)
library(tidyr)

# Generic Functions
CreateDate <- function(Data_Table) {
  Data_Table %>% 
    mutate(Date = paste(Year,Month,Day,sep="-")) %>% 
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
    mutate(Year = as.integer(Year), Month = as.integer(Month), Day = as.integer(Day)) %>% 
    select(Date, everything()) 
}

SeparateDate <- function(Data_Table) {
  Data_Table %>% 
    separate(Date, c("Year","Month","Day")) %>% 
    CreateDate()
}

SpreadVariables <- function(Data_Table, SS) {
  Variable <- Data_Table %>% 
    filter(Sensor == SS) %>% 
    select(-Sensor) %>% 
    mutate(Date = as.Date(Date)) %>% 
    distinct() %>%
    spread(Station, Value)
  return(Variable)
}

# Data File Function
Write_DataFile <- function(df, ff, date_start, date_end) {
  
  # Individual Tables for Different Sensors
  TMax <- SpreadVariables(df, "TMAX")
  TMin <- SpreadVariables(df, "TMIN")
  Precipitation <- SpreadVariables(df, "PRECIP")
  Runoff <- SpreadVariables(df, "FLOW")
  
  # Building Data File
  DataFile <- data.frame(Date = seq(as.Date(date_start),as.Date(date_end), by = '1 day')) %>% 
    SeparateDate() %>% mutate(Hour = 0, Minute = 0, Second = 0)
  
  # Adding Meteorological Data
  DataFile <- DataFile %>% 
    left_join(TMax, by = 'Date') %>% 
    left_join(TMin, by = 'Date') %>% 
    left_join(Precipitation, by = 'Date') %>% 
    left_join(Runoff, by = 'Date') %>% 
    select(-Date)
  
  # Replacing NAs with -999
  DataFile[is.na(DataFile)] <- -999
  
  # Number of Stations
  ntmax <- ncol(TMax) - 1
  ntmin <- ncol(TMin) - 1
  nprecip <- ncol(Precipitation) - 1
  nrunoff <- ncol(Runoff) - 1
  
  # Saving Data to a File
  cat("PRMS Data File", file = ff, sep = "\n")
  cat(paste("tmax", ntmax), file = ff, append = TRUE, sep = "\n")
  cat(paste("tmin", ntmin), file = ff, append = TRUE, sep = "\n")
  cat(paste("precip", nprecip), file = ff, append = TRUE, sep = "\n")
  cat(paste("runoff", nrunoff), file = ff, append = TRUE, sep = "\n")
  cat("// Units: Temperature = Celsius, Precipitation = in/day, Runoff = cfs", 
      file = ff, append = TRUE, sep = "\n")
  cat("##############################################################################################", 
      file = ff, append = TRUE, sep = "\n")
  write.table(DataFile, ff, sep=" ", row.names = FALSE, col.names = FALSE, append = TRUE)
  
}