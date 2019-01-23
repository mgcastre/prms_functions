### Variables for PRMS/GSFLOW StatVar File
## Maria Gabriela Castrellon
## March 7th, 2018

# Creating StatVariables
statVar_names <- c(rep("runoff",5),
                   rep("hru_rain",6),
                   rep("subinc_precip",4),
                   rep("subinc_actet",4),
                   rep("sub_sroff",4),
                   rep("sub_cfs",4),
                   rep("sub_gwflow",4))

statVar_element <- c(1:5,c(9,20,34,46,47,29),rep(1:4,5))

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