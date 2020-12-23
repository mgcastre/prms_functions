# Functions to create plots based on PRMS output (output_statvar.dat)
# Created By: M. G. Castrellon

# Required Libraries
library(tmaptools)
library(tidyverse)
library(lubridate)
library(stringr)
library(rgdal)
library(tmap)

# Generic Functions
SeparateDate <- function(Data_Table) {
  Data_Table %>% 
    mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>% 
    arrange(Date) %>% select(Date, Year, Month, Day, everything())
}

# Plotting Functions ------------------------------------------------------------------------------

HRU_Plot_Col <- function(statvar, plotting_variable, yyyy, name_unit, cc) {
  # Plots a year of a data of a specified output variable using columns
  # statvar is the table created with the read.statvar function
  # plotting_variable is the desired variable to plot
  # yyyy is the year to plot
  # name_unit is the name of the variable and its unit for the y axis
  # cc is the color for the features in the plot
  SeparateDate(statvar) %>% 
    filter(Year == yyyy) %>% 
    gather(-Date, -Year, -Month, -Day, key="Key", value="Values") %>% 
    filter(str_detect(Key, plotting_variable)) %>% 
    ggplot() + geom_col(mapping = aes(x = Date, y = Values), fill = cc) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b%y") +
    theme_light() + labs(x = 'Date', y = name_unit) + facet_wrap(~Key)
}

HRU_Plot_Line <- function(statvar, plotting_variable, yyyy, name_unit, cc) {
  # Plots a year of a data of a specified output variable using lines
  # statvar is the table created with the read.statvar function
  # plotting_variable is the desired variable to plot
  # yyyy is the year to plot
  # name_unit is the name of the variable and its unit for the y axis
  # cc is the color for the features in the plot
  SeparateDate(statvar) %>% 
    filter(Year == yyyy) %>% 
    gather(-Date, -Year, -Month, -Day, key="Key", value="Values") %>% 
    filter(str_detect(Key, plotting_variable)) %>% 
    ggplot() + geom_line(mapping = aes(x = Date, y = Values, color = Key)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b%y") +
    scale_color_brewer(palette = "Set1") +
    theme_light() + labs(x = 'Date', y = name_unit)
}

PlotHydrographPRMS <- function(DataTable, Name, Sim, Obs, y1y2) {
  # Plots simulated and observed streamflow values at a specified stream gage station
  # DataTable is the data from the statvar file
  # Name is the name of the stream gage station
  # Sim is the name of the PRMS output variable corresponding to the simulated flow
  # Obs is the name of the PRMS output variable corresponding to the observed flow
  # y1y2 is a vector of the years to plot (year1:year2)
  # It is recommended to plot a period of 5 years
  select(DataTable, Date, Sim, Obs) %>% 
    filter(year(Date) %in% y1y2) %>% 
    gather(-Date, key = "Key", value = "Values") %>% 
    mutate(Key = replace(Key, str_detect(Key, Obs), "Observed"), size = 0.8) %>%
    mutate(Key = replace(Key, str_detect(Key, Sim), "Simulated"), size = 0.2) %>% 
    ggplot() + geom_line(mapping = aes(x = Date, y = Values, color = Key)) +
    scale_color_manual(name = paste("Streamflow at ",Name,":",sep = ""), values = c("Simulated"="blue","Observed"="black")) +
    theme_light() + labs(x = 'Date', y = 'Q (cfs)') +
    theme(legend.position = 'top', legend.text = element_text(size=12), legend.title = element_text(size=12,face='bold'), 
          plot.title = element_text(hjust=0.5,vjust=0.5,size=14,face='bold'),
          axis.title = element_text(size=11,face='bold')) -> Figure1
  return(Figure1)
}

## Note: Instead of using PlorHydrographPRMS, try ggof from the hydroGOF package

# Mapping Functions ------------------------------------------------------------------------------

MonthlyTotalAverage <- function(statvar){
  # Averages monthly total of the selected output variable and organized it by HRU
  # statvar is the table created from the statvar data file
  monthly_vars <- statvar %>% 
    gather(-Date, -Year, -Month, -Day, key = "Key", value = "Value") %>% 
    group_by(Year, Month, Key) %>% 
    summarise(Value = sum(Value)) %>% 
    group_by(Month, Key) %>% 
    summarise(Value = mean(Value)) %>% 
    separate(Key, c("a","b","hru"), sep = "_") %>% 
    mutate(Variable = paste(a,b,sep="_")) %>% 
    select(Month, hru, Variable, Value) %>% 
    ungroup()
  return(monthly_vars)
}

select.outputvar <- function(Table, Name){
  # This function  selects the desired monthly variable and creates new data frame with it
  # Example Output:
  ##    Month    hru   hru_ppt
  ## 1     1      1 0.5355551
  ## 2     1     10 0.7135882
  ## 3     1     11 0.7702726
  ## 4     1     12 0.8036605
  ## 5     1     13 0.7973785
  ## 6     1     14 0.8229955
  Result <- Table %>%
    filter(Variable == Name) %>% 
    select(-Variable)
  colnames(Result) <- c("Month","hru",Name)
  return(Result)
}

spread.outputvar <- function(Table, Key){
  # This function spreads the selected montly variables so that each month is a column and the hrus are in the rows
  # Example Output:
  ##     hru hru_ppt_1 hru_ppt_10 hru_ppt_11 hru_ppt_12 hru_ppt_2 hru_ppt_3 hru_ppt_4 hru_ppt_5 hru_ppt_6 hru_ppt_7 hru_ppt_8
  ## 1      1 0.5355551   12.21067   8.494248   1.753241 0.3305901 0.8263914  2.299758  7.678567  7.588027  6.000678  8.565233
  ## 2     10 0.7135882   12.58821  10.153763   2.059454 1.0215688 0.3591967  4.786995  7.628758  6.960519  5.991037  9.426981
  ## 3     11 0.7702726   12.40075   9.989138   2.061985 1.5068127 0.4765890  5.291592  7.617272  7.064118  6.183643  9.683624
  ## 4     12 0.8036605   12.43175  10.231855   2.178013 1.6233903 0.4110165  5.622522  7.600093  6.977039  6.177982  9.757211
  ## 5     13 0.7973785   11.66208   8.785026   2.251580 2.0732053 1.1473847  4.927610  7.558785  7.630326  6.620905  9.506695
  ## 6     14 0.8229955   11.23583   8.044168   2.221019 2.5648938 1.7850372  4.895478  7.542843  8.071937  6.953187  9.574388
  Result <- Table %>% 
    filter(str_detect(Variable, Key)) %>%
    mutate(MonthlyVar = paste(Variable,Month,sep="_")) %>% 
    select(-Month, -Variable) %>% 
    spread(key = MonthlyVar, value = Value)
  return(Result)
}

PlotMap <- function(shp, df, variable, title, plt, u){
  # This function takes a polygon with HRUs (shapefile format) joins it with the output (df) and plots it by month
  # shp is the HRU polygon shapefile
  # df is the dataframe containing the data to be plotted in the format similar to the output from the function select.outputvars
  # variable is the nam of the variable to be plotted
  # title is the title of the plot whih is the name of the variable
  # plt is the palette of color for the plot
  # u is the unit of the selected variable (e.g. mm/month for hru_ppt)
  ## Merging
  shp@data <- shp@data %>% left_join(df, by = "hru")
  ## Months String
  mnths <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
  ## Plot Map
  tm_shape(shp) +
    tm_fill(variable, palette = plt, title = u) +
    tm_facets(by = "Month", ncol = 4) +
    tm_shape(polygon) +
    tm_borders("black", lwd = .2) +
    tm_legend(title = title,
              scale = 1.5,
              panel.label.size = 1.5,
              panel.labels = mnths) +
    tm_layout(asp = 4/3)
}