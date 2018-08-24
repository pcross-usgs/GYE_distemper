# Script to get the data ready for the JAGS models
rm(list = ls())
setwd("D:/My Documents/Research/Active Papers/GYE distemper")

library(data.table)
# read in the data
cutoff <- 24
if(cutoff == 12){
	Bear_Data <- read.csv("./Data/Bear_Data_12.csv", header = T, stringsAsFactors=F)
	Wolf_Data <- read.csv("./Data/Wolf_Data_12.csv", header = T, stringsAsFactors=F)
}


if(cutoff == 16){
  Bear_Data <- read.csv("./Data/Bear_Data_16.csv", header = T, stringsAsFactors=F)
  Wolf_Data <- read.csv("./Data/Wolf_Data_16.csv", header = T, stringsAsFactors=F)
}

if(cutoff == 24){
  Bear_Data <- read.csv("./Data/Bear_Data_24.csv", header = T, stringsAsFactors=F)
  Wolf_Data <- read.csv("./Data/Wolf_Data_24.csv", header = T, stringsAsFactors=F)
}

# add left and right months to each data.frame
MonthAssignFxn <- function(left, right, min_year){
	out.data<-data.frame(left.months =
	                       (as.numeric(substring(left, 1,4)) - min_year) *
	                       12 + as.numeric(substring(left, 6,7)),
						 right.months = (as.numeric(substring(right, 1,4)) - min_year) *
						   12 + as.numeric(substring(right, 6,7)),
						 stringsAsFactors=F)
	return(out.data)
}

min_year <- min(c(as.numeric(substring(Bear_Data$left, 1, 4)),
                as.numeric(substring(Wolf_Data$left, 1, 4))))
Bear_Data <- cbind(Bear_Data, MonthAssignFxn(Bear_Data$left,
                                           Bear_Data$right, min_year))
Wolf_Data <- cbind(Wolf_Data, MonthAssignFxn(Wolf_Data$left,
                                           Wolf_Data$right, min_year))
m.adjust <- min(c(Bear_Data$left.months, Wolf_Data$left.months)) - 1

Bear_Data$left.months <- Bear_Data$left.months - m.adjust
Bear_Data$right.months <- Bear_Data$right.months - m.adjust
Wolf_Data$left.months <- Wolf_Data$left.months - m.adjust
Wolf_Data$right.months <- Wolf_Data$right.months - m.adjust

Bear_Data$left.date <- as.Date(Bear_Data$left)
Bear_Data$right.date <- as.Date(Bear_Data$right)
Wolf_Data$left.date <- as.Date(Wolf_Data$left)
Wolf_Data$right.date <- as.Date(Wolf_Data$right)

# Need to index the biological year with a monthly timestep.
total.months = max(c(max(Bear_Data$right.months), max(Wolf_Data$right.months)))
start.month = 3 #start bioyear in march
first.month = as.numeric(substring(min(c(min(Bear_Data$left),
                                         min(Wolf_Data$left))), 6, 7)) # what is month 1 in data

if (first.month<start.month){
	month2year <- c(rep(1, (start.month-first.month)),
	              rep(1:floor((total.months - (start.month-first.month)) / 12) + 1, each = 12),
								rep(floor((total.months- (start.month-first.month))/12) + 2,
								    12*(((total.months- (start.month-first.month) )/12) -
								          floor((total.months- (start.month-first.month) )/12)))
	)
}

if (first.month==start.month){
	month2year <- c(rep(1:floor(total.months / 12), each=12),
	               rep(floor(total.months / 12) + 1,total.months -
	                     (floor(total.months / 12) * 12)))
}

if (first.month>start.month) {
	month2year<- c(rep(1, 13 - first.month),
	               rep(1:floor((total.months - (start.month-first.month) ) / 12) + 1,
	                   each=12))[1:total.months]
}

###################################################
# Prepare individual variables for JAGS models.
###################################################
lookup = month2year
n_year <- max(month2year)
left = c(Bear_Data$left.months, Wolf_Data$left.months)
right = c(Bear_Data$right.months, Wolf_Data$right.months)
censored = c(as.numeric(Bear_Data$TestR=="N"), as.numeric(Wolf_Data$TestR=="N")) # not infected
infected = c(as.numeric(Bear_Data$TestR=="P"), as.numeric(Wolf_Data$TestR=="P")) # infected
male = c(as.numeric(Bear_Data$Sex=="M"), as.numeric(Wolf_Data$Sex=="M"))

#binary variable = 1 for wolves or bears.
wolf = c(rep(0, length(Bear_Data$left.months)),
         rep(1,length(Wolf_Data$left.months)))
bear = c(rep(1, length(Bear_Data$left.months)),
         rep(0,length(Wolf_Data$left.months)))
###################################################

# year variable by interval
Bear_Data <- as.data.table(Bear_Data)
Wolf_Data <- as.data.table(Wolf_Data)
bioYear.left <- c(month2year[Bear_Data[,left.months]], month2year[Wolf_Data[,left.months]])
bioYear.right <- c(month2year[Bear_Data[,right.months]], month2year[Wolf_Data[,right.months]])

###################################################
# Create a combined data.frame for plotting purposes.
###################################################

Bear_Data <- Bear_Data[,c(2:3, 6:10)]
names(Bear_Data) <- c("id", "sex", "distemper", "left.months", "right.months",
											"left.date", "right.date")
Wolf_Data <- Wolf_Data[,c(2:3, 6:10)]
names(Wolf_Data) <- c("id", "sex", "distemper", "left.months", "right.months",
											"left.date", "right.date")

CDV <- rbind(Bear_Data, Wolf_Data)
CDV$spp <- "wolf"
CDV$spp[which(bear==1)] <- "bear"

CDV$distemper <- as.factor(CDV$distemper)
CDV$sex <- as.factor(CDV$sex)
CDV$spp <- as.factor(CDV$spp)

head(Bear_Data)
#Create a sequential ID for plotting purposes
CDV <- CDV[order(CDV$spp, CDV$left.date),] #order the data by spp and start date

j <- 1 # counter
CDV$id2 <- 1 #pre-populated the new ID (to be overwritten)
for (i in 2:dim(CDV)[1]){
  # is this a new individual?
  if (length(which(CDV$id[i] == CDV$id[1:(i-1)])) == 0) {#if == 0, then new individual
    j <- j + 1 # new individual, so new ID
    CDV$id2[i] <- j
  } else { # not a new individual, fill in previous ID
    tmp <- which(CDV$id[i] == CDV$id[1:(i-1)])
    CDV$id2[i] <- CDV$id2[tmp[1]]
  }
}
###################################################

# remove things not needed
rm(Bear_Data, Wolf_Data, start.month, first.month, m.adjust, min_year, total.months)

# save it all
if(cutoff == 12){save.image("./Data/Wolf_Bear_12.RData")}
if(cutoff == 16){save.image("./Data/Wolf_Bear_16.RData")}
if(cutoff == 24){save.image("./Data/Wolf_Bear_24.RData")}

