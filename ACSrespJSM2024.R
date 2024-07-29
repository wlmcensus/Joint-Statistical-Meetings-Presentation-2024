# This is the R Script to reproduce the linked micromaps for the ACS data
# as presented in the JSM talk: Using Linked Micromaps for Evidence-Based Policy
#
# It uses the data from the ACS Response Rates, found here.
# www.census.gov/acs/www/methodology/sample-size-and-data-quality/response-rates/
#
# and the PEW Religious Landscape Survey found here
# www.pewresearch.org/religious-landscape-study/database/ 
#
# Data values/columns were extracted and placed in .csv files
# so we did not have to load the entire data set.

# It uses prepared spreadsheets, as follows.

# GovSentCom.csv contains Government Dissatisfaction as a percent for 2007 and
# 2014. The proxy used is the percentage wanting Small Government in the
# Pew Religious Landscape Survey. Assuming Small Govt ~ Anti-Govt

# Created separate spreadsheets for the Time Series Data to build
# the 3-D array in R. Saved each tab as a CSV file that are read in.

library(micromapST)

# Read in the Gov Sentiment File
GovSent <- as.matrix(read.csv("GovSentComb.csv", row.names = 1))

# Read in the years for the horizontal axis of the series.
# For each state.
TSyears <- as.matrix(read.csv("TSdataX.csv", row.names = 1))

# Same for Y, which is the RESPONSE rate.
TSresp <- as.matrix(read.csv("TSdataY.csv", row.names = 1))

# Get the row names for the data frame input to micromapST
rown <- row.names(TSresp)


###############  Time Series #########################

# Put TS data into the 3-D array required by micromapST
# Only use 2010 (P11) thru to the end 2022 (P23) in the plots. 
# Earlier years are rather consistent and uninteresting.

# First create a 3-D array of zeros
TSd <- array(0,c(51,13,2))
rownames(TSd) <- rown   # Assign row names, required by micromapST

# Fill in with the data.
# TSd[,,1] is a page containing a matrix of years, in our example
# TSd[,,2] is a page containing a matrix of response rates

TSd[,,1] <- TSyears[,11:23]    # Horizontal values of time series
TSd[,,2] <- TSresp[,11:23]     # Vertical values of time series

# Examine TSd for understanding and verifying structure
dim(TSd)
TSd[1,,]
head(TSd[,,1])
head(TSd[,,2])

# Get the Response Rates part of the array as data frame
# which is referenced in the function call; e.g. sort variable
temprates <- data.frame(TSd[,,2])

# First micromaps data col is time series, second is dots of 2022 rate
# Note that panelData contains the name of the data object
# containing the values for the time series.
# It must be a 3-D array, in our example. 

panelDesc <- data.frame(
  type=c("maptail","id","ts","dot"),    
  lab1=c("","","ACS Response Rate","Response Rate 2022"),
  lab2=c("","","2010 to 2022","(Sort Variable)"),
  lab3=c("", "","Years","Response Rate 2022"),
  lab4=c("", "","Rate",""),
  col1=c(NA,NA,NA,13),
  panelData = c(NA, NA, "TSd", NA)
) 

ExTitle <- c("Time Series of ACS Response Rates", "2010 to 2022")

# Micromap plots are not shown in the R Studio plot panel.
# Graphical output is sent to a file. We use .png, but there are other options.

grDevices::png(file="ACSwithTimeSeries.png",
               width=7.5,height=10, units = "in", res = 600)

micromapST(temprates, panelDesc, sortVar=13, ascend=FALSE,
           title=ExTitle
) 

x <- grDevices::dev.off()

###############  Arrow  ##########################
# Now do an Arrow plot in the third col and the
# sort variable in the middle one.

# Do a sorted dot of the response rate in 2022
# Do an arrow plot showing the change from 2010 to 2022

# Refer to table in JSS article for values in col1 and col2 for arrow.

#  THIS WAS NOT INCLUDED IN THE JSM TALK
#  Included here as extra example

panelDesc <- data.frame(
  type=c("mapcum","id","dot","arrow"),    
  lab1=c("","","ACS Response Rate","ACS Response Rate"),
  lab2=c("","","2022","2010 to 2022"),
  lab3=c("", "","Rate in 2022","Response Rates"),
  col1=c(NA,NA,13,1),
  col2=c(NA,NA,NA,13)
) 

ExTitle <- c("ACS Response Rates 2010 to 2022")

# Create the garphics device to send the output to. 
grDevices::png(file="ACSwithArrow.png",
               width=7.5,height=10, units = "in", res = 600)

micromapST(temprates, panelDesc, sortVar=13, ascend=FALSE,
           title=ExTitle
) 

x <- grDevices::dev.off()

###########  Dot and Bar  ##########################
# Calculate the difference between 2010 and 2022
# Just take the straight difference, which is the
# Decrease in percentage points, because it was a decrease everywhere

#  THIS WAS NOT INCLUDED IN THE JSM TALK
#  Included here as extra example

# Find the difference 2010 minus 2022
# Remember... this is the data frame with data to be shown in micromaps
# This is needed for the bars.

temprates[,14] <- temprates[,1] - temprates[,13]

# Do a sorted dot of the response rate in 2022
# Do a bar plot showing the percentage point change from 2010 to 2022

panelDesc <- data.frame(
  type=c("mapmedian","id","dot","bar"),    
  lab1=c("","","ACS Response Rate","Decrease in"),
  lab2=c("","","2022","Percentage Points"),
  lab3=c("", "","Rate in 2022","Percentage Point Change"),
  col1=c(NA,NA,13,14)
) 

ExTitle <- c("ACS Response Rates",
             "Decrease in Percentage Points 2010 to 2022")

# Create graphics device. The micromap is in this file.
grDevices::png(file="ACSwithBar.png",
               width=7.5,height=10, units = "in", res = 600)

micromapST(temprates, panelDesc, sortVar=13, ascend=FALSE,
           title=ExTitle
) 

x <- grDevices::dev.off()

##############   Anti-Govt Sentiment   #####################
# Add a third column showing a proxy for anti-govt sentiment
# Note that all examples in JSS article have a max of 2 data columns
# One can add columns, but good idea to expand the width of the graphical
# output in the graphics device call.

# Measured as % for small government. We will use this as proxy
# for anti-government

# Do a sorted dot of the response rate in 2022
# Do a bar plot showing the percentage point change from 2010 to 2022

# Add another component to temprates, since that contains the data.
# Adding Pro Small Govt (~ anti-govt sentiment)

temprates[,15] <- GovSent[,1]   # 2014 values

panelDesc <- data.frame(
  type=c("mapmedian","id","dot","bar","dot"),    
  lab1=c("","","ACS Response Rate","Decrease in","Percent"),
  lab2=c("","","2022","Percentage Points","Pro Small Govt 2014"),
  lab3=c("", "","Rate in 2022","Percentage Point Change","Percent"),
  col1=c(NA,NA,13,14,15)
) 

ExTitle <- c("ACS Response Rates",
             "Decrease in Percentage Points 2010 to 2022")

# Notice we make the width bigger to better include an additional column.
# Set up the graphics device to capture the micromap. 
grDevices::png(file="ACSwithBarAntiG.png",
               width=9.5,height=10, units = "in", res = 600)

micromapST(temprates, panelDesc, sortVar=13, ascend=FALSE,
           title=ExTitle
) 

x <- grDevices::dev.off()

