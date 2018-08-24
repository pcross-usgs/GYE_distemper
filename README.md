# Canine distemper virus infection hazards for Grizzly bears and wolves in Yellowstone

This repo has the code to analyze the serological data for bears and wolves from the Greater Yellowstone region. The wolf data come from the Yellowstone Wolf Project and are predominantly from winter captures on the northern range. The bear data come from the Interagency Grizzly Bear Study Team and are from around the GYE. We use an bayesian statistical approach that accounts for the interval-censoring, whereby the timing of infection is not known exactly, but occurs between birth and the time of sampling or between two sampling events for individuals that were sampled multiple times. We also allow for diagnostic testing error. All the code is written in R and the MCMC sampling is done with JAGS.

##Folders:   
1. Code: holds the code  
2. Data: holds the working serological data  
3. Figures: Folder to hold generated figures. 
4. Output: These files are created by running the RunJagsModels_paper.R file  

##Main Code Files:   
DataPrep 1,2 and 3.R: files need to be run first to prep the data for the model runs. They convert the data from a row per capture to a row per interval, where an interval is from birth to first test, or from 1st test to 2nd test.   

WriteJAGSmodels_paper.R: script to write the BUGS models  
RunJAGSmodels_paper.R: script to run the models.   
LabNotebook.Rmd: Rmarkdown file doing some initial investigation of the data and the prior distributions.  

Plot_Fig3_etal.R: Script to plot figure 3 of the manuscript

CompareCutoffs.RMD: initial code to look at some of the consequences of different cutoff values.

###Function files
plot_interval_fxn: some functions to plot the raw intervals of the testing data  
plot_haz_fxn: some functions to plot the model results on the probability of infection over time  
plot_bear_wolf_haz_fxn.r: function to plot the inf. probability for bears vs. wolves. 
plot_bear_wolf_gamma_fxn.r: function to plot the log hazards of bears vs. wolves.  

##Data
This folder includes csv files for the bear and wolf testing data.  

./Data/Working Data/BearTiters.csv = working file for the bear serology data.  
./Data/Working Data/Wolf_titers.csv = working file for the wolf serology data.  
./Data/Working Data/EntryExit_Wolf.csv = working file for the Date of Birth for known wolves.  

The ./Code/DataPrep1.R code pulls in these files and joins them together to create 
./Data/Working Data/BearTiters2.csv and  
./Data/Working Data/WolfTiters2.csv and
./Data/CDV_all.csv which is both of them combined together. 

./Code/DataPrep2.R pulls in BearTiters2.csv and WolfTiters2.csv and implements the different cuttoff values for positive and negative individuals. Creating:   
./Data/Bear_Data_12.csv which assumes a titer of 12 cuttoff for bears
./Data/Wolf_Data_12.csv which assumes a titer of 12 cuttoff for wolves. 

We do this again for cuttoff values of 16 and 24. 

.Code/DataPrep3.R combines the wolf and bear data together to create:  
.Data/Wolf_Bear_12.RData for the JAGS runs. This script was re-run with a cutoff of 16 and 24 to create those RData files as well.  

