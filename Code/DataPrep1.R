# Data preparation script to Recreate Bear_Data.csv and Wolf_Data.csv from the BearTiters and WolfTiters.csv in the working directory
setwd("D:/My Documents/Research/Active Papers/GYE distemper")
library(lubridate)

bear <- read.csv("./Data/Working Data/BearTiters.csv", header = T)

#remove columns
bear <- bear[, c(-1,-6, -10,-11,-12, -13,-14)] 
#create Unique individual ID
bear$UID <- as.factor(sapply(strsplit(as.character(bear$AID), split = "_"), '[',1))
bear <- bear[,-1]
summary(bear)
bear$Cap.Type[bear$Cap.Type == "mgt"] <- "MGT"
bear$Cap.Type[bear$Cap.Type == "res"] <- "RES"
bear$Cap.Type <- as.factor(as.character(bear$Cap.Type))

# remove rows w/o titer data
bear  <- subset(bear, bear$Titer != "Inf"  )
bear$Trap.Date <- mdy(bear$Trap.Date)
summary(bear)
 
bear<- bear[order(bear$UID, bear$Trap.Date),]
names(bear) <- c("sex", "birth.year", "age", "trap.date", "cap.type", "titer", "UID")
 
write.csv(bear, file = "./Data/Working Data/BearTiters2.csv")

#Wolf section
wolf <- read.csv("./Data/Working Data/Wolf_titers.csv", header = T)

wolf<- wolf[,c(3,4,6,11,13,14)]
names(wolf) <- c("UID", "trap.date", "sex", "nr", "vaccine", "titer")
wolf$vaccine <- as.factor(wolf$vaccine)
wolf$trap.date <- as.Date(wolf$trap.date, "%m/%d/%Y")
wolf$nr <- as.factor(wolf$nr)
summary(wolf)

wolf <- subset(wolf, wolf$vaccine == 0)
wolf <- wolf[,-5]
wolf$sex <- as.character(wolf$sex)
wolf$sex[wolf$sex == "FEMALE"] <- "F"
wolf$sex[wolf$sex == "Female"] <- "F"
wolf$sex[wolf$sex == "MALE"] <- "M"
wolf$sex[wolf$sex == "Male"] <- "M"
wolf$sex <- as.factor(wolf$sex)
wolf <- wolf[order(wolf$UID, wolf$trap.date),]
summary(wolf)

wolf$UID <- as.character(wolf$UID)
wolf$UID[wolf$UID == "217FB"] <- "217F"
wolf$UID[wolf$UID == "380FB"] <- "380F"
wolf$UID[wolf$UID == "527FB"] <- "527F"
wolf$UID <- as.factor(wolf$UID)
table(wolf$UID)
summary(wolf)

#remove the 1 record w/o a date: 
wolf <- wolf[is.na(wolf$trap.date) == F, ]

entry <- read.csv("./Data/Working Data/EntryExit_Wolf.csv", header = T)
wolf <- merge(wolf, entry, by.y = "Wolf", by.x = "UID", all.x = T)
wolf$DOB <- as.Date(wolf$DOB, "%m/%d/%Y")
#remove the 1 record w/o a birthday: 
wolf[is.na(wolf$DOB)==T,]
#remove the 1 record w/o a birthday: 
wolf <- wolf[is.na(wolf$DOB)==F,]

wolf$birth.year <- year(wolf$DOB)
wolf <- subset(wolf, select = -DOB)
trap.year <- year(wolf$trap.date)
wolf$age <- trap.year-wolf$birth.year
write.csv(wolf, file = "./Data/Working Data/WolfTiters2.csv")

# Create a merged dataframe that includes all the tests
# This should be used for some of the raw plots and maps as it doesn't exclude
# individuals that tested positive multiple times. 

wolf <- subset(wolf, select = -nr)
wolf$spp <- "wolf"
# reorder bears
bear <- bear[,c(7, 4, 1, 6, 2, 3)]
bear$spp <- "bear"
names(bear) <- names(wolf)
CDV_all <- rbind(wolf, bear)
CDV_all$spp <- as.factor(CDV_all$spp)
save(CDV_all, file = "./Data/CDV_all.Rdata")
write.csv(CDV_all, file = "./Data/CDV_all.csv")

