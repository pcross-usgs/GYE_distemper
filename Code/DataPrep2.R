# Data preparation script 2
# Going from data with a row for every test to a row for every interval
# implements a titer cut-off for pos/neg
# removes multiple pos retests
setwd("D:/My Documents/Research/Active Papers/GYE distemper")

bear2 <- read.csv("./Data/Working Data/BearTiters2.csv", header = T)
wolf2 <- read.csv("./Data/Working Data/WolfTiters2.csv", header = T)

# assuming bears come out of the den in mid april, same time as wolf births
bear2$DOB <- as.Date(paste("4/15/", bear2$birth.year, sep = ""), "%m/%d/%Y")
wolf2$DOB <- as.Date(paste("4/15/", wolf2$birth.year, sep = ""), "%m/%d/%Y")
bear2$trap.date <- as.Date(bear2$trap.date)
wolf2$trap.date <- as.Date(wolf2$trap.date)

source("./Code/interval_data_fxn.R")

bear_6 <- interval_data(bear2, cutoff = 6)
bear_8 <- interval_data(bear2, cutoff = 8)
bear_12 <- interval_data(bear2, cutoff = 8)
bear_16 <- interval_data(bear2, cutoff = 16)
bear_24 <- interval_data(bear2, cutoff = 24)

wolf_6 <- interval_data(wolf2, cutoff = 6)
wolf_8 <- interval_data(wolf2, cutoff = 8)
wolf_12 <- interval_data(wolf2, cutoff = 8)
wolf_16 <- interval_data(wolf2, cutoff = 16)
wolf_24 <- interval_data(wolf2, cutoff = 24)

# write the interval data to a file. 
write.csv(bear_12[[1]], file = "./Data/Bear_Data_12.csv")
write.csv(bear_16[[1]], file = "./Data/Bear_Data_16.csv")
write.csv(bear_24[[1]], file = "./Data/Bear_Data_24.csv")

write.csv(wolf_12[[1]], file = "./Data/Wolf_Data_12.csv")
write.csv(wolf_16[[1]], file = "./Data/Wolf_Data_16.csv")
write.csv(wolf_24[[1]], file = "./Data/Wolf_Data_24.csv")

#grab the back conversions
source("./Code/backconversion_fxn.R")
bear_back <- backconversion(d = bear_12[[2]])
wolf_back <- backconversion(d = wolf_12[[2]])

#plot the results
bear_back$datedif <- as.numeric(bear_back$right - bear_back$left)/365
wolf_back$datedif <- as.numeric(wolf_back$right - wolf_back$left)/365

par(mfrow = c(1,2))
plot(bear_back$datedif, bear_back$titer1, 
		 type = "n", xlim = c(0,5), ylim = c(0, 40),
		 xlab = "time lag", ylab = "Titer", main = "bears")
for(i in 1:dim(bear_back)[1]){
	lines(c(0,bear_back$datedif[i]), c(bear_back$titer1[i], bear_back$titer2[i]),
				col = "grey")
}

plot(wolf_back$datedif, wolf_back$titer1, 
		 type = "n", xlim = c(0,5), ylim = c(0, 40),
		 xlab = "time lag", ylab = "Titer", main = "wolves")
for(i in 1:dim(wolf_back)[1]){
	lines(c(0,wolf_back$datedif[i]), c(wolf_back$titer1[i], wolf_back$titer2[i]),
				col = "grey")
}

