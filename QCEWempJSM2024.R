# This is the R Script to reproduce the linked micromaps for the QCEW data
# as presented in the JSM talk: Using Linked Micromaps for Evidence-Based Policy
#
# It uses the data from the QCEW, found here.
# https://data.bls.gov/maps/cew/us
#
# Data values/columns were extracted from the QCEW site and placed in .csv files
# so we did not have to load the entire data set.

# Created separate spreadsheets for the Time Series Data to build
# the 3-D array in R. Saved each as a CSV file that are read in.

library(micromapST)

# We first need to create the 3-D array for the time series in micromapST.

# Read in the years/quarters for the horizontal axis of the series.
# For each state.
TSyears <- as.matrix(read.csv("QCEW_emp_X.csv", row.names = 1))

# Same for Y, which is the Change Emp rate.
TSrate <- as.matrix(read.csv("QCEW_emp_Y.csv", row.names = 1))

# Get the row names for the data frame input to micromapST
rown <- row.names(TSrate)

###############  Time Series #########################

# Put TS data into the 3-D array required by micromapST

# First create a 3-D array of zeros that we fill in with values.
# We have 51 rows (states), 9 years, and 2 pages (see below)

TSd <- array(0,c(51,9,2))
rownames(TSd) <- rown   # Assign row names, required by micromapST

# Fill in with the data.
# TSd[,,1] is a page containing a matrix of years, in our example
# TSd[,,2] is a page containing a matrix of change emp rates

# Use 2020 Q1 to 2022 Q1 - just look at these
TSd[,,1] <- TSyears[,3:11]    # Horizontal values of time series
TSd[,,2] <- TSrate[,3:11]     # Vertical values of time series

# Examine TSd - just so you can see what is going on and if it looks right.
dim(TSd)
TSd[1,,]
head(TSd[,,1])
head(TSd[,,2])


# Get just the employment rates that will be the data frame
# required by micromapST.
temprates <- data.frame(TSd[,,2])

# First data col is time series, second is dots of 2020 Q1 rate
# Note that panelData contains the name of the data object
# containing the values for the time series.
# It must be a 3-D array.

# NOT USED IN JSM PRESENTATION  
# We did not use this graphic in the presentation. We are providing it
# in case users are interested in it. 

# This specifies the structure of the micromap.
panelDesc <- data.frame(
  type=c("maptail","id","ts","dot"),    
  lab1=c("","","QCEW Change in Employment","2020 Q1 Change in Employment"),
  lab2=c("","","2020 Q1 to 2022 Q1","(Sort Variable)"),
  lab3=c("", "","Year.Quarter","% Change Over One Year"),
  lab4=c("", "","% Change",""),
  col1=c(NA,NA,NA,1),
  panelData = c(NA, NA, "TSd", NA)
) 

ExTitle <- c("Time Series of QCEW Change in One-Year Employment", 
             "Leasure & Hospitatlity 2020 Q1 to 2022 Q1")

# The micromapST output cannot be displayed in the R Studio plot pane.
# Send output to a graphics device, in this case a .png file.
# Open the file to see the micromap. 
grDevices::png(file="QCEWTimeSeriesB.png",
               width=7.5,height=10, units = "in", res = 600)

micromapST(temprates, panelDesc, sortVar=1, ascend=FALSE,
           title=ExTitle
) 

x <- grDevices::dev.off()

######################################
#   Try doing an arrow from start to finish
######################################

panelDesc <- data.frame(
  type=c("maptail","id","ts","dot","arrow"),    
  lab1=c("","","Over-the-Year Change","Over-the-Year Change","Over-the-year Change"),
  lab2=c("","","2020 to 2022","2020 Q1","2020 to 2022"),
  lab3=c("", "","Year.Quarter","% Change","Percentage Point Change"),
  lab4=c("", "","% Change","",""),
  col1=c(NA,NA,NA,1,1),
  col2=c(NA,NA,NA,NA,9),
  refVals=c(NA,NA,NA,0,NA),
  panelData = c(NA, NA, "TSd", NA,NA)
) 

ExTitle <- c("Effects of COVID: QCEW % Change in One-Year Employment", 
             "Leasure & Hospitatlity 2020 Q1 to 2022 Q1")

grDevices::png(file="QCEWTimeSeriesArrow.png",
               width=7.5,height=10, units = "in", res = 600)

micromapST(temprates, panelDesc, sortVar=1, ascend=FALSE,
           title=ExTitle
) 

x <- grDevices::dev.off()





