# function to plot the interval censored testing data
plot_interval <- function(Data, LWD){
  if(missing(LWD)){LWD <- 2}

  n <- length(unique(Data$id))
  nrows <- dim(Data)[1]

  m <- seq.Date(as.Date("1970/1/1"), as.Date("2016/1/1"), by = "5 years")
  labs <- seq(1970, 2015,5)
  par(mar = c(3,4,1,1))
  plot(Data$left.date, seq(1:nrows), type = 'n',
       xlim = c(min(Data$left.date), as.Date("2016/1/1")),
       xlab = '', ylab = 'Individual bear or wolf',
       bty = 'n', tck = -0.02)

  for(i in 1:nrows){ #loop over every row
    j <- Data$id2[i] # plotting row = sequential ID
    if(Data$distemper[i] == "P"){
      lines(c(Data$left.date[i], Data$right.date[i]), c(j,j), lwd = LWD, col = 'red')
      }

    if(Data$distemper[i] == "N"){
      lines(c(Data$left.date[i], Data$right.date[i]), c(j,j), lwd = LWD, col = 'blue')
    }
  }
   legend('topleft', c('negative', 'positive'),
          lwd = c(2,2,2), col = c('blue', 'red'), bty = 'n')

   axis(1, at = m, labels = labs, tck = -0.02)

   #plot a spp. divider line
   nbears <- length(unique(Data$id2[Data$spp=="bear"]))
   abline(h = nbears+0.5, lty = 2)
   text(as.Date("1980/1/1"), nbears + 30, "wolves")
   text(as.Date("1980/1/1"), nbears - 30, "bears")
}

# function to plot wolves only
plot_wolf_interval <- function(Data, LWD){
  if(missing(LWD)){LWD <- 2}

  n <- length(unique(Data$id))
  nrows <- dim(Data)[1]

  #need a new plotting ID
  Data$plot.id <- Data$id2 - min(Data$id2) + 1

  m <- seq.Date(as.Date("1995/1/1"), as.Date("2016/1/1"), by = "5 years")
  labs <- seq(1995, 2015, 5)
  par(mar = c(3,4,1,1))
  plot(Data$left.date, seq(1:nrows), type = 'n',
       xlim = c(min(Data$left.date), as.Date("2016/1/1")),
       xlab = '', ylab = 'Individual wolf',
       bty = 'n', tck = -0.02)

  polygon(as.Date(c("1999/3/1", "1999/3/1", "2000/3/1", "2000/3/1")), c(0, 300, 300, 0), col = "pink", lty = 2, border = "red")
  polygon(as.Date(c("2005/3/1", "2005/3/1", "2006/3/1", "2006/3/1")), c(0, 300, 300, 0), col = "pink", lty = 2, border = "red")
  polygon(as.Date(c("2008/3/1", "2008/3/1", "2009/3/1", "2009/3/1")), c(0, 300, 300, 0), col = "pink", lty = 2, border = "red")

  for(i in 1:nrows){ #loop over every row
    j <- Data$plot.id[i]
    if(Data$distemper[i] == "P"){
      lines(c(Data$left.date[i], Data$right.date[i]), c(j,j), lwd = LWD, col = 'red')
    }

    if(Data$distemper[i] == "N"){
      lines(c(Data$left.date[i], Data$right.date[i]), c(j,j), lwd = LWD, col = 'blue')
    }
  }
  legend('topleft', c('negative', 'positive'),
         lwd = c(2,2,2), col = c('blue', 'red'), bty = 'n')

  axis(1, at = m, labels = labs, tck = -0.02)
}

# Black and White function to plot wolves only
plot_wolf_interval_bw <- function(Data, LWD){
	if(missing(LWD)){LWD <- 2}
	
	n <- length(unique(Data$id))
	nrows <- dim(Data)[1]
	
	#need a new plotting ID
	Data$plot.id <- Data$id2 - min(Data$id2) + 1
	
	m <- seq.Date(as.Date("1995/1/1"), as.Date("2016/1/1"), by = "5 years")
	labs <- seq(1995, 2015, 5)
	par(mar = c(3,5,1,1))
	plot(Data$left.date, seq(1:nrows), type = 'n',
			 ylim = c(0,300),
			 xlim = c(min(Data$left.date), as.Date("2016/1/1")),
			 xlab = '', 
			 ylab = 'Individual wolves',
			 bty = 'n', 
			 cex.axis = 1.5, 
			 cex.lab = 2, 
			 tck = -0.02)
	
	polygon(as.Date(c("1999/3/1", "1999/3/1", "2000/3/1", "2000/3/1")), 
					c(0, 290, 290, 0), col = "light grey", lty = 2, border = "grey")
  polygon(as.Date(c("2005/3/1", "2005/3/1", "2006/3/1", "2006/3/1")), 
					c(0, 290, 290, 0), col = "light grey", lty = 2, border = "grey")
	polygon(as.Date(c("2008/3/1", "2008/3/1", "2009/3/1", "2009/3/1")), 
					c(0, 290, 290, 0), col = "light grey", lty = 2, border = "grey")
	
	for(i in 1:nrows){ #loop over every row
		j <- Data$plot.id[i]
		if(Data$distemper[i] == "P"){
			lines(c(Data$left.date[i], Data$right.date[i]), c(j,j), lwd = LWD, col = 'black')
		}
		
		if(Data$distemper[i] == "N"){
			lines(c(Data$left.date[i], Data$right.date[i]), c(j,j), lwd = LWD, col = 'grey')
		}
	}
	legend(x = as.Date("1994/7/1"), y = 290, c('negative', 'positive'),
				 lwd = c(2,2,2), col = c('grey', 'black'), bty = 'n', xjust = 0)
	axis(1, at = m, labels = rep(NA), tck = -0.02)
	text(x = as.Date("1998/1/1"), y = 300, labels = "major outbreaks")
	polygon(as.Date(c("1995/1/1", "1995/1/1", "1996/1/1", "1996/1/1")), 
					c(290, 310, 310, 290), col = "light grey", lty = 2, border = "grey")
	
}


# Black and White function to plot bears only
plot_bear_interval_bw <- function(Data, LWD){
	if(missing(LWD)){LWD <- 2}
	
	n <- length(unique(Data$id))
	nrows <- dim(Data)[1]
	
	#need a new plotting ID
	Data$plot.id <- Data$id2 - min(Data$id2) + 1
	
	m <- seq.Date(as.Date("1970/1/1"), as.Date("2016/1/1"), by = "5 years")
	labs <- seq(1970, 2015,5)
	
	par(mar = c(3,5,1,1))
	plot(Data$left.date, seq(1:nrows), type = 'n',
			 xlim = c(min(Data$left.date), as.Date("2016/1/1")),
			 ylim = c(0, n),
			 xlab = '', ylab = 'Individual bear',
			 bty = 'n', 
			 cex.axis = 1.5,
			 cex.lab = 2,
			 tck = -0.02)

		for(i in 1:nrows){ #loop over every row
		j <- Data$plot.id[i]
	
			if(Data$distemper[i] == "P"){
			lines(c(Data$left.date[i], Data$right.date[i]), c(j,j), lwd = LWD, col = 'black')
				}
		
			if(Data$distemper[i] == "N"){
				lines(c(Data$left.date[i], Data$right.date[i]), c(j,j), lwd = LWD, col = 'grey')
			}
		}
	
	axis(1, at = m, labels = rep(NA), tck = -0.02)
}