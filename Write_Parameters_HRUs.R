# Functions to Write PRMS HRU/SSR Paramneter File (input_climate.params)
# Created By: M. G. Castrellon

# Required Libraries
library(readxl)

# Function to write parameters
excel.parameters <- function(tt, ff){
  for (j in seq(2,ncol(tt),1)) {
    cat("####", file = ff, append = TRUE, sep = "\n")
    write.table(tt[,j], quote = FALSE, row.names = FALSE, col.names = FALSE, file = ff, append = TRUE, sep = " ")
  }
}

Write_Parameters_HRUs <- function(excel_table, filename){
  # Loading Data (from excel tables)
  HRU_Params <- read_excel(excel_table, sheet = "nhru", col_names = FALSE)
  SSR_Params <- read_excel(excel_table, sheet = "nssr", col_names = FALSE)
  # Writing Parameter File
  ## Opening File
  cat("// La Villa PRMS Parameter File Part III: HRU Parameters from GIS", file = filename, sep = "\n")
  ## Writing Parameters
  excel.parameters(HRU_Params, filename)
  excel.parameters(SSR_Params, filename)
}

