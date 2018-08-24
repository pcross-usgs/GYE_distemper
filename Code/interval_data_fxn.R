interval_data <- function(data, cutoff){
	# Function to turn testing data with a row per test into interval data
	# with a row for each interval going from neg to positive. 
	# INPUT: 
	# Data must have the following columns: 
	# UID = unique individual identifier
	# trap.date
	# titer
	# DOB = Date of birth
	# 
	# Cutoff is the CDV titer required to be called a positive
	# 
	# OUTPUT: 
	# data2 = restructured data
	# back.data = dataframe of backconversions
	
	data$sex <- as.character(data$sex)
	data$TestR <- "N"
	data$TestR[data$titer >= cutoff] <- "P"
	data <- data[order(data$UID,data$trap.date),]
	
	# split out the individuals that were tested multiple times
	t <- as.data.frame(table(data$UID))
	one.test <- t[t$Freq == 1, 1]
	mul.test <- t[t$Freq > 1, 1]
	rm(t)
	
	data.one <- subset(data, data$UID %in% one.test)
	data.mul <- subset(data, data$UID %in% mul.test)
	
	data2 <- data.one
	data2$left <- data2$DOB
	data2$right <- data2$trap.date
	data2 <- subset(data2, select = c("UID", "sex", "left", "right", "TestR"))
	
	data2$sex <- as.character(data2$sex)
	#add in the rows from multiple tested individuals
	#loop over all individuals
	mul.test <- as.character(mul.test)
	backconvert <- data.frame(UID = NA,  
														sex = NA, 
														left = as.Date(NA), 
														right = as.Date(NA), 
														TestR = NA, 
														test1 = NA)
	
	for(i in 1:length(mul.test)){
		ind <- mul.test[i]
		ind.dat <- data.mul[data.mul$UID == ind,]
		
		temp <- data.frame(UID = rep(NA,10),   
											 sex = rep(NA,10), 
											 left = as.Date(rep(NA,10)), 
											 right = as.Date(rep(NA,10)), 
											 TestR = rep(NA,10), 
											 test1 = rep(NA,10))
		
		# write in the first interval
		temp$UID[1] <- ind
		temp$sex[1] <- ind.dat$sex[1]
		temp$left[1] <- ind.dat$DOB[1]
		temp$right[1] <- ind.dat$trap.date[1]
		temp$TestR[1] <- ind.dat$TestR[1]
		temp$test1[1] <- "N"

				# construct the other intervals
		for(j in 2:dim(ind.dat)[1]){
			temp$UID[j] <- ind
			temp$sex[j] <- ind.dat$sex[j]
			temp$left[j] <- ind.dat$trap.date[j-1]+1
			temp$right[j] <- ind.dat$trap.date[j]
			temp$test1[j] <- ind.dat$TestR[j-1]
			temp$TestR[j] <- ind.dat$TestR[j]
		}
		
		temp <- temp[is.na(temp$UID) == F, ] # remove the extra rows. 
		
		#store records that back-convert
		temp2 <- subset(temp, temp$test1 == "P" & temp$TestR == "N")
		if(dim(temp2)[1] > 0){
			backconvert <- rbind(backconvert, temp2)
		}
		temp <- subset(temp, temp$test1 != "P") #remove intervals that start positive
		temp <- temp[,-6] # remove test1
		# append temp to the data
		data2 <- rbind(data2, temp)
		rm(temp, temp2, ind.dat, ind)
	}
	
	# remove anything starting prior to 1970
	data2 <- subset(data2, data2$left > as.Date("1970-01-01"))
	#keep only complete cases
	data2 <- data2[complete.cases(data2),]
	
	back.data <- data[data$UID %in% backconvert$UID,]
	output <- list(data2, back.data)
	return(output)
}