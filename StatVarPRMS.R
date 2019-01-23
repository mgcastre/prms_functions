### Variables for PRMS/GSFLOW StatVar File
## Maria Gabriela Castrellon
## March 7th, 2018

# Creating StatVariables
statVar_names <- c(rep("net_rain",4),
                   rep("hru_actet",4),
                   rep("potet",4),
                   rep("tavgf",4),
                   rep("swrad",4),
                   rep("orad_hru",4),
                   rep("transp_on",4),
                   rep("sroff",4),
                   rep("seg_inflow",6),
                   rep("hru_rain",6),
                   rep("runoff",5),
                   "orad",
                   "basin_horad",
                   "basin_potsw",
                   "basin_orad",
                   "basin_cloud_cover",
                   "basin_swrad",
                   rep("hru_ppt",47),
                   rep("gwres_in",47),
                   rep("gwres_sink",47))

statVar_element <- c(rep(c(1,12,36,46),8),rep(c(9,20,34,46,47,29),2),1:5,rep(1,6),rep(1:47,3))

# statVar_names <- c("seg_inflow","seg_outflow","seg_outflow","seg_inflow","seg_inflow","seg_outflow","seg_outflow")
# 
# statVar_element <- c(47,46,45,20,34,29,9)

nvalues <- length(statVar_names)

# Writing Variables to File
filename <- "control/Statistic_Variables.txt"

cat("####", file = filename, sep = "\n")
cat("nstatVars", file = filename, sep = "\n", append = TRUE)        # Control Parameter Name
cat(1, file = filename, sep = "\n", append = TRUE)                  # Number of Values
cat(1, file = filename, sep = "\n", append = TRUE)                  # Data Type (1=integer, 2=real, 3=double, 4=character)
cat(nvalues, file = filename, sep = "\n", append = TRUE)            # Value(s)

cat("####", file = filename, sep = "\n", append = TRUE)
cat("statVar_element", file = filename, sep = "\n", append = TRUE)
cat(length(statVar_element), file = filename, sep = "\n", append = TRUE)
cat(4, file = filename, sep = "\n", append = TRUE)
cat(statVar_element, file = filename, sep = "\n", append = TRUE)

cat("####", file = filename, sep = "\n", append = TRUE)
cat("statVar_names", file = filename, sep = "\n", append = TRUE)
cat(length(statVar_names), file = filename, sep = "\n", append = TRUE)
cat(4, file = filename, sep = "\n", append = TRUE)
cat(statVar_names, file = filename, sep = "\n", append = TRUE)