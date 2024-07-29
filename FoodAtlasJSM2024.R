# This is the R Script to reproduce the linked micromaps for the ERS data
# as presented in the JSM talk: Using Linked Micromaps for Evidence-Based Policy
#
# It uses the data from the ERS FoodAtlas, found here.
# https://www.ers.usda.gov/data-products/food-environment-atlas/go-to-the-atlas/
#
# Data values/columns were extracted from the ERS site and placed in .csv files
# so we did not have to load the entire data set.

library(micromapST)
library(DescTools)    

# Read in the following data. We ended up not using it for JSM.
X <- read.csv("Health.csv")

# These are Percent Obesity - County values are all the same within each state.
# So, just take the mean (or median) as the value for the state.
Ob12 <- aggregate(X$PCT_OBESE_ADULTS12, list(X$State), FUN= mean)
Ob17 <- aggregate(X$PCT_OBESE_ADULTS17, list(X$State), FUN= mean)

# Now for Assistance - SNAP benefits
X <- read.csv("Assist.csv")
SNAPcng <- aggregate(X$PCH_SNAP_12_17, list(X$State), FUN=mean )

# Now for Food Insecurity
X <- read.csv("Insecurity.csv")
Insec <- aggregate(X$CH_FOODINSEC_14_17, list(X$State), FUN = mean)

# State rownames for data frame.
# These are needed for micromapST.
rown <- Ob12$Group.1

# Calculate difference between obesity ob17-ob12
# We ended up not using this for the JSM talk.
ob17m12 <- Ob17$x - Ob12$x

# Create the data frame for micromaps
FoodData <- data.frame(Ob12$x,Ob17$x,SNAPcng$x,Insec$x,ob17m12)
# Add the state names needed with micromapST
rownames(FoodData) <- rown

######################################
#   SNAP vs Food Insecurity - First plot
######################################

# This is the micromap showing the ordered dots for SNAP
# and bars for the change in Food Insecurity

panelDesc <- data.frame(
  type=c("maptail","id","dot","bar"),    
  lab1=c("","","% Change in SNAP","Change in Household Food Insecurity"),
  lab2=c("","","2012 to 2017","2012/14 to 2015/17"),
  lab3=c("", "","Percent Change","Percentage Points"),
  refVals=c(NA,NA,0,NA),
  col1=c(NA,NA,3,4)
) 

ExTitle <- c("SNAP Participation and Food Insecurity")

grDevices::png(file="SNAPinsec.png",
               width=7.5,height=10, units = "in", res = 600)

micromapST(FoodData, panelDesc, sortVar=3, ascend=FALSE,
           title=ExTitle
) 

x <- grDevices::dev.off()


###########################################
#   Now explore the
#   Relationship between Food Insec and Access
#   Second linked micromaps for food data
###########################################


# Now for Low Access to Store, we need to load another data set
# county level data
X <- read.csv("Access.csv")

# This is Population, low access to store (% change), 2010 -15, % Change
# We are going to use this county level data for boxplots in one column.
# There are many outliers that make it harder to see what is going on with
# the distribution. 

# The following gets rid of the outliers.
X$Access <- Winsorize(X$PCH_LACCESS_POP_10_15,na.rm=TRUE)

# The following gets the information for the boxplots in micromapST
Accboxlist <- boxplot(split(X$Access,X$State), plot=FALSE,
                      outline=FALSE)

###################################################################
### Another col on last one: Access 2015 vs Insecurity 2017
#   This is for the scatterplot.
###################################################################

# We need to get more data. 
# Insecurity is state level, Access to Stores (%) is county level
# FOODINSEC_15_17 (%) (State) (3-year avg)
# PCT_LACCESS_POP15 (%) Population, low access to store (%) (County)
# Take the MEDIAN of Low Access to Stores to get a state value.

Xaccess15 <- read.csv("Access15.csv")
Xinsec15 <- read.csv("Insec15.csv")

A15 <- aggregate(Xaccess15$PCT_LACCESS_POP15, list(Xaccess15$State), FUN= median)
I15 <- aggregate(Xinsec15$FOODINSEC_15_17, list(Xinsec15$State), FUN= median)

# Add to FooData
FoodData$Insec15 <- I15$x
FoodData$Acc15 <- A15$x

# Boxplot is Low Access % Change

panelDesc <- data.frame(
  type=c("maptail","id","boxplot","dot","scatdot"),    
  lab1=c("","","% Change Low Access to Store","Change in Food Insecurity","2015 Food Insecurity"),
  lab2=c("","","Counties 2010 to 2015","2012/14 to 2015/17","vs 2015 Low Access to Stores"),
  lab3=c("", "","Percent Change","Percentage Points","% Low Access"),
  lab4=c("","","","","% Insecurity"),
  col1=c(NA,NA,NA,4,7),
  col2=c(NA,NA,NA,NA,6),
  refVals=c(NA,NA,NA,0,NA),
  panelData=c("","","Accboxlist","","")
) 

ExTitle <- c("Household Low Access to Stores and Food Insecurity")

grDevices::png(file="AccessInsecBoxScat.png",
               width=9.5,height=10, units = "in", res = 600)

micromapST(FoodData, panelDesc, sortVar=4, ascend=FALSE,
           title=ExTitle
) 

x <- grDevices::dev.off()












