# Functions to Write PRMS Climate Paramneter File (input_climate.params)
# Created By: M. G. Castrellon

# This set of functions calculate the parameters for the xyz_dist module in PRMS/GSFLOW.
# This module (xyz_dist) deals with the interpolation and/or distribution of climate data (precipitation and temperature) to the different HRUs.
# Units: Temperature = Celsius, Precip = in/day
# Reference: https://pubs.usgs.gov/tm/6b7/

# Inputs
#
## dfdata: data frame similar to the one used to create the data file (see below)
##         Date Station Sensor Value
## 1 1975-06-19  122006 PRECIP  18.2
## 2 1975-06-20  122006 PRECIP   4.3
## 3 1975-06-21  122006 PRECIP   5.2
## 4 1975-06-22  122006 PRECIP   2.5
## 5 1975-06-23  122006 PRECIP   1.1
## 6 1975-06-24  122006 PRECIP   1.9
## Where "Station" is a unique ID for the met station and "Sensor" refers to the type of measurment (PRECIP, TMAX, TMIN, FLOW)
## Units: Temperature = Celsius, Precip = in/day, Streamflow = cfs
#
## dfstations: data frame with station ID and its coordinates (see below) 
##    Station      X        Y     Z
## 1  122006 519603.8 854176.8   680
## 2  124006 542288.4 834568.8   220
## 3  124007 558033.3 839836.5   377
## 4  126001 579022.3 858569.4    43
## 5  126003 569855.1 843813.9   260
## 6  126007 562474.8 864072.2    84
## Where "Station" is a unique ID for the met station and X, Y, Z are the station's coordinates in meters
#
## parameters_excel: file path to an excel table (see provided example) with the format for other required parameters
#
## filename: name of the file to save parameters (e.g. input_climate.params)

# Required Libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(broom)

# Generic Functions ================================================================================

CreateDate <- function(Data_Table) {
  Data_Table %>% 
    mutate(Date = paste(Year,Month,Day,sep="-")) %>% 
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
    mutate(Year = as.integer(Year), Month = as.integer(Month), Day = as.integer(Day)) %>% 
    dplyr::select(Date, everything()) 
}

SeparateDate <- function(Data_Table) {
  Data_Table %>% 
    separate(Date, c("Year","Month","Day")) %>% 
    CreateDate()
}

SelectVariable <- function(df, ss) {
  df %>% 
    filter(Sensor == ss) %>% 
    SeparateDate()
}

SelectStations <- function(dfdata, dfstations, ss){
  # Function to select all the stations from a given sensor
  Out_Stations <- dfdata %>% 
    filter(Sensor == ss) %>% 
    merge(dfstations, by = "Station") %>% 
    dplyr::select(Station, Name, Latitude, Longitude, Elevation) %>% 
    unique() %>% arrange(Station)
  return(Out_Stations)
}

# Summary Functions ================================================================================

MonthlyMeanValues <- function(df, sensor) {
  # Calculates monthly average measurement from the specified sensor for all stations
  # Returns a table of stations and months with their respective mean monthly value
  Result <- filter(df, Sensor == sensor) %>% 
    SeparateDate() %>% 
    group_by(Station, Month) %>%
    summarise(Value = mean(Value, na.rm = TRUE)) %>% 
    dplyr::select(Station, Month, Value) %>% 
    arrange(Station, Month)
  return(Result)
}

PlotMonthlyMeanValues <- function(df, sensor, title, y) {
  # Plots all the monthly values and the calculated monthly average
  Figure <- df %>% filter(Sensor == sensor) %>% 
    SeparateDate() %>% 
    group_by(Station, Month) %>%
    mutate(MonthlyValue = mean(Value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot() + ggtitle(title) +
    geom_point(aes(x = Month, y = Value)) +
    geom_point(aes(x = Month, y = MonthlyValue), color = 'red', size = 1.5) +
    scale_x_continuous(breaks = seq(1,12,1)) + ylab(y) +
    theme_light() + facet_wrap(~Station)
  return(Figure)
}

IndependentVariables <- function(dfdata, dfstations) {
  # Met Stations Coordinates
  DataStations <- dfdata %>% dplyr::select(Station) %>% distinct() %>% merge(dfstations)
  # Create Output Table
  IV <- list()
  # Mean 
  IV$x_add <- mean(DataStations$X)
  IV$y_add <- mean(DataStations$Y)
  IV$z_add <- mean(DataStations$Z)
  # Standard Deviation
  IV$x_div <- sd(DataStations$X)
  IV$y_div <- sd(DataStations$Y)
  IV$z_div <- sd(DataStations$Z)
  return(IV)
}

DependentVariables <- function(dfdata, dfstations){
  # Create Output Table
  DV <- list()
  # Precipitation
  Precipitation <- SelectVariable(dfdata, "PRECIP")
  DV$ppt_add <- mean(Precipitation$Value, na.rm = TRUE)
  DV$ppt_div <- sd(Precipitation$Value, na.rm = TRUE)
  # TMax (degres Celsius)
  TMax <- SelectVariable(dfdata, "TMAX")
  DV$tmax_add <- mean(TMax$Value, na.rm = TRUE)
  DV$tmax_div <- sd(TMax$Value, na.rm = TRUE)
  # TMin (degres Celsius)
  TMin <- SelectVariable(dfdata, "TMIN")
  DV$tmin_add <- mean(TMin$Value, na.rm = TRUE)
  DV$tmin_div <- sd(TMin$Value, na.rm = TRUE)
  return(DV)
}

# Linear Regression Functions =========================================================================

Monthly_MLR <- function(Data1, Data2, SS){
  # Data1 is a table with daily values. It will be transformed to monthly values
  # Data2 is a table with the locations of the respectives observations (containing x,y,z coordinates)
  # SS is the sensor to be calculated (PRECIP, TMAX, TMIN)
  
  # Independent Variable Calculation
  IV <- IndependentVariables(Data1, Data2)
  
  # Dependent Variable Calculation
  Variable <- SelectVariable(Data1, SS)
  m <- mean(Variable$Value, na.rm = TRUE)
  s <- sd(Variable$Value, na.rm = TRUE)
  
  # Transforming Dependent Variable
  New_Var <- Variable %>% 
    group_by(Month, Station) %>% 
    mutate(Value = (Value - m)/s) %>% 
    summarise(Value = mean(Value, na.rm = TRUE))
  
  # Default Regression Equation
  Equation <- Value ~ X + Y + Z
  
  # Regression Coefficients Calculation
  TibbleTable <- merge(New_Var, Data2, by = "Station") %>% 
    dplyr::select(Month,Value,X,Y,Z) %>% 
    mutate(X = (X - IV$x_add)/IV$x_div,
           Y = (Y - IV$y_add)/IV$y_div,
           Z = (Z - IV$z_add)/IV$z_div) %>% 
    nest(-Month) %>% arrange(Month) %>% 
    mutate(mlr = map(data, ~lm(Equation, data = .)),
           results = map(mlr, augment),
           coeffs = map(mlr, broom::tidy),
           glance = map(mlr, broom::glance))
  
  # Return Tibble
  return(TibbleTable)
}

# Plotting Functions ==================================================================================

ggplotScatter <- function(TibbleTable) {
  require(ggplot2)
  Data_Table <- TibbleTable %>% unnest(results)
  fit <- lm(.fitted ~ Value, data = Data_Table)
  r2 <- signif(summary(fit)$adj.r.squared,5)
  Plot <- ggplot(Data_Table) + theme_grey() +
    geom_point(aes(x = Value, y = .fitted)) + 
    geom_abline(color = "red", linetype = "dashed", size = 0.8) +
    theme_grey() + labs(x = "Measured", y = "Fitted") +
    labs(subtitle = paste("R^2=",r2))
  return(Plot)
}

ModelCoeffs <- function(TibbleTable) {
  Model_Coeffs <- TibbleTable %>% 
    unnest(coeffs) %>% dplyr::select(term,Month,estimate) %>% 
    plyr::rename(c("term"="nlapse","Month"="nmonths","estimate"="parameter")) %>% 
    filter(nlapse != '(Intercept)') %>% arrange(nlapse, nmonths)
  return(Model_Coeffs)
}

ModelQuality <- function(TibbleTable) {
  Plot <- TibbleTable %>% 
    unnest(glance) %>% arrange(Month) %>% 
    ggplot() + geom_col(aes(Month, adj.r.squared), width = 0.5) + theme_light() +
    scale_y_continuous(name = "Adj. R Squared", limits = c(0,1), breaks = seq(-1,1,0.2)) +
    scale_x_discrete(name = "Months", limits = seq(1,12,1)) +
    ggtitle("Monthly Adjusted R.Sqaured")
  return(Plot)
}

ggplotScatterMonthly <- function(TibbleTable) {
  require(ggplot2)
  Plot <- TibbleTable %>% unnest(results) %>% 
    ggplot() + theme_grey() +
    geom_point(aes(x = Value, y = .fitted)) + 
    geom_abline(color = "red", linetype = "dashed", size = 0.5) +
    theme_grey() + labs(x = "Measured", y = "Fitted") + 
    facet_wrap(~Month)
  return(Plot)
}

# Functions to Write Parameter File ==================================================================

write.parameter <- function(parameter_name, num_dim, dimension_name, type, values, filename) {
  # This function is useful for writing parameters with only one dimension number
  # Values must be a one dimensional array (a column of values)
  cat("####", file = filename, append = TRUE, sep = "\n")
  cat(parameter_name, file = filename, append = TRUE, sep = "\n")
  cat(num_dim, file = filename, append = TRUE, sep = "\n")
  cat(dimension_name, file = filename, append = TRUE, sep = "\n")
  cat(length(values), file = filename, append = TRUE, sep = "\n")
  cat(type, file = filename, append = TRUE, sep = "\n")
  write.table(values, file = filename, sep=" ", row.names = FALSE, col.names = FALSE, append = TRUE)
}

excel.parameters <- function(tt, ff){
  for (j in seq(2,ncol(tt),1)) {
    cat("####", file = ff, append = TRUE, sep = "\n")
    write.table(tt[,j], quote = FALSE, row.names = FALSE, col.names = FALSE, file = ff, append = TRUE, sep = " ")
  }
}

Write_Parameters_XYZDist <- function(dfdata, dfstations, parameters_excel, filename){
  
  # Calculating Parameters --------------------------------------------------------
  
  ## Rain Station Coordinates
  nrain <- SelectStations(dfdata, dfstations, "PRECIP") %>% 
    transmute(psta_elev = as.numeric(Elevation),
              psta_x = as.numeric(Longitude),
              psta_y = as.numeric(Latitude),
              psta_freq_nuse = as.integer(1),
              psta_nuse = as.integer(1))
  
  ## Temperature Station Coordinates
  ntemp <- SelectStations(dfdata, dfstations, "TMAX") %>% 
    transmute(tsta_elev = as.numeric(Elevation),
              tsta_x = as.numeric(Longitude),
              tsta_y = as.numeric(Latitude),
              tsta_nuse = as.integer(1))
  
  ## Mean Monthly Climate Parameters
  psta_month_ppt <- MonthlyMeanValues(dfdata, "PRECIP")
  tsta_month_max <- MonthlyMeanValues(dfdata, "TMAX")
  tsta_month_min <- MonthlyMeanValues(dfdata, "TMIN")
  
  ## Transformation Parameters for Dependent & Independent Variables
  IV <- IndependentVariables(dfdata, dfstations)
  DV <- DependentVariables(dfdata, dfstations)
  
  ## Multiple Linear Regression Coefficients
  ### Precipitation
  model_precip <- Monthly_MLR(dfdata, dfstations, "PRECIP")
  ppt_lapse <- ModelCoeffs(model_precip)
  ### TMAX
  model_tmax <- Monthly_MLR(dfdata, dfstations, "TMAX")
  max_lapse <- ModelCoeffs(model_tmax)
  ### TMIN
  model_tmin <- Monthly_MLR(dfdata, dfstations, "TMIN")
  min_lapse <- ModelCoeffs(model_tmin)
  
  ## Read Other Required Parameters from Excel
  Par_one <- read_excel(parameters_excel, sheet = "one", col_names = FALSE)
  Par_nmonths <- read_excel(parameters_excel, sheet = "nmonths", col_names = FALSE)
  
  # Writing Parameter File -----------------------------------------------------------
  
  ## Opening File
  cat("// PRMS Parameter File Part: XYZ Climate Distribution Module", file = filename, sep = "\n")
  
  ## Writing Parameters with Dimension "one"
  excel.parameters(Par_one, filename)
  write.parameter("x_add",1,"one",2,IV$x_add,filename)
  write.parameter("y_add",1,"one",2,IV$y_add,filename)
  write.parameter("z_add",1,"one",2,IV$z_add,filename)
  write.parameter("x_div",1,"one",2,IV$x_div,filename)
  write.parameter("y_div",1,"one",2,IV$y_div,filename)
  write.parameter("z_div",1,"one",2,IV$z_div,filename)
  write.parameter("ppt_add",1,"one",2,DV$ppt_add,filename)
  write.parameter("ppt_div",1,"one",2,DV$ppt_div,filename)
  write.parameter("tmax_add",1,"one",2,DV$tmax_add,filename)
  write.parameter("tmax_div",1,"one",2,DV$tmax_div,filename)
  write.parameter("tmin_add",1,"one",2,DV$tmin_add,filename)
  write.parameter("tmin_div",1,"one",2,DV$tmin_div,filename)
  
  ## Writing Parameters with Dimension "nrain"
  write.parameter("psta_elev",1,"nrain",2,nrain$psta_elev,filename)
  write.parameter("psta_x",1,"nrain",2,nrain$psta_x,filename)
  write.parameter("psta_y",1,"nrain",2,nrain$psta_y,filename)
  write.parameter("psta_elev",1,"nrain",2,nrain$psta_elev,filename)
  write.parameter("psta_freq_nuse",1,"nrain",1,nrain$psta_freq_nuse,filename)
  write.parameter("psta_nuse",1,"nrain",1,nrain$psta_nuse,filename)
  
  ## Writing Parameters with Dimension "ntemp"
  write.parameter("tsta_elev",1,"ntemp",2,ntemp$tsta_elev,filename)
  write.parameter("tsta_x",1,"ntemp",2,ntemp$tsta_x,filename)
  write.parameter("tsta_y",1,"ntemp",2,ntemp$tsta_y,filename)
  write.parameter("tsta_elev",1,"ntemp",2,ntemp$tsta_elev,filename)
  write.parameter("tsta_nuse",1,"ntemp",1,ntemp$tsta_nuse,filename)
  
  ## Writing Parameters with Dimension "nmonths"
  excel.parameters(Par_nmonths, filename)
  
  # Writing mean monthly climate parameters
  write.parameter("psta_month_ppt",2,c("nrain","nmonths"),2,psta_month_ppt$Value,filename)
  write.parameter("tsta_month_max",2,c("ntemp","nmonths"),2,tsta_month_max$Value,filename)
  write.parameter("tsta_month_min",2,c("ntemp","nmonths"),2,tsta_month_min$Value,filename)
  
  # Writing multiple linear regression parameters
  write.parameter("ppt_lapse",2,c("nlapse","nmonths"),2,ppt_lapse$parameter,filename)
  write.parameter("max_lapse",2,c("nlapse","nmonths"),2,max_lapse$parameter,filename)
  write.parameter("min_lapse",2,c("nlapse","nmonths"),2,min_lapse$parameter,filename)
}
