backconversion <- function(d){
	# function to create a data frame of information about individuals
	# that initially test positive and later are test negative. 
	# requires a dataframe with the following columns: 
	# UID  = unique individual id
	# DOB = date of birth
	# trap.date = date of the capture/testing event. 
	# titer = the antibody titer dilution
	
	backconvert <- data.frame(UID = NA,  left = as.Date(NA), 
														right = as.Date(NA), titer1 = NA, 
														titer2 = NA)
	
	ind <- unique(d$UID)
	for(i in 1:length(ind)){
		tempind <- ind[i]
		ind.dat <- d[d$UID == tempind,]
		
		temp <- data.frame(UID = rep(NA,10),  left = as.Date(rep(NA,10)), 
											 right = as.Date(rep(NA,10)), titer1 = rep(NA,10), 
											 titer2 = rep(NA,10))
		
		# write in the first interval
		temp$UID[1] <- tempind
		temp$left[1] <- ind.dat$DOB[1]
		temp$right[1] <- ind.dat$trap.date[1]
		temp$titer2[1] <- ind.dat$titer[1]
		temp$titer1[1] <- "N"
		
		# construct the other intervals
		for(j in 2:dim(ind.dat)[1]){
			temp$UID[j] <- tempind
			temp$left[j] <- ind.dat$trap.date[j-1]+1
			temp$right[j] <- ind.dat$trap.date[j]
			temp$titer1[j] <- ind.dat$titer[j-1]
			temp$titer2[j] <- ind.dat$titer[j]
		}
		
		temp <- temp[is.na(temp$UID) == F, ] # remove the extra rows. 
		temp <- temp[temp$titer1 != "N", ]
		#store records that back-convert
		backconvert <- rbind(backconvert, temp)
		rm(temp, ind.dat, tempind)
	}
	backconvert <- backconvert[-1,]
	return(backconvert)	
}
