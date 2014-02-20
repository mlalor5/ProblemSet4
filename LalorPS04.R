##Maggie Lalor
##PS4625 Applied Statistical Programming Spring 2014
##Problem Set 04 Due 02/20/2014

##Housekeeping

# Load relevant libraries
rm(list=ls()) #after cleaning space
library(plyr)
set.seed(25) #Set seed for reproduceability 
#Set working directory for later function that will create directories and files
setwd("/home/maggie/Documents/Spring_2014/Montgomery/PS04") 


### PART I: Reading in data without a clean format ###

## 1) Write a function that takes a generic NetLogo file as an input and writes out a directory
#where the various components of the output are written in an organized format (See #2).

netfile <- "~/Documents/Spring_2014/Montgomery/PS04/NetLogo.csv"

###FUNCTION###
netdirectory <- function(netfile, directory=getwd()){ #Takes file as input, optional to change directory for base
  
  
  ###ROOT MODEL DIRECTORY###
  
  
  #Make model name 2nd line of file (appears to be either model or file name)
  ModelTime <- scan(netfile, skip=1, nlines=2, what=" ", sep=",") #Only need first item in 2 lines
  ModelName <- gsub(pattern="nlogo", replacement="", x=ModelTime[1]) #get rid of extension
  Dates <- strsplit(ModelTime[85], split=" ") #get separation of dates/times
  Day <- as.Date(Dates[[1]][1], format ='%m/%d/%Y') #convert time to useable format: YEAR-MN-DY
  Time <- strsplit(Dates[[1]][2], split=":") #Time separated by :, second item in Dates
  ModelDat <- paste(ModelName, Day, ".", Time[[1]][1], Time[[1]][2], sep="") #Make one string, separate by periods

  #ModelName root
  dir.create(ModelDat)
  #list.dirs() #Can see that ModelName is now a directory in working directory
  #Change working directory to ModelName, since that's root of described tree
  setwd(ModelDat)
  #getwd() #Now it's the working directory
    
  
  ###Globals DIRECTORY###
  
  
  Globals <- scan(netfile, skip=8, nlines=2, what=" ", sep="\n")
  GlobalNames <- strsplit(Globals[1], split=",")
  GlobalVals <- strsplit(Globals[2], split=",")

  Global <-rep(list(), length(GlobalNames[[1]]))  #create list of length 84 aka number of global variables
  for (i in 1:length(GlobalNames[[1]])){ #Assign name and value
    Global[[i]] = GlobalVals[[1]][i] #Value for ith Global Variable
    names(Global)[i] = GlobalNames[[1]][i] #Name for ith Global Variable  
  } #end for loop
  
  #Create directory and output
  dir.create("Globals")
  #From facebook "Global.r is supposed to be a list of the globals. Use the dump command."
  dump(list="Global", file="Globals/Globals.R") #write R file

  
  ### TURTLES DIRECTORY###
  
  
  #Turtles (Districts, Voters, Activists, Parties, Candidates breeds as .csv)
  dir.create("Turtles")
  
  #Column names should be meaningful.
  # Vectors should be broken down into separate columns.
  # Drop information that is constant or missing for all agents of relevant breed.
  
  #Cleanup data
  Turtles <- scan(netfile, skip=12, nlines=4787, what=" ", sep="\n") #Turtles Data, last one row 4799
  TurtleVarNames <- strsplit(Turtles[1], split=",") #Get Names
  TurtlesData <- strsplit(Turtles[-1], split=",") #Get Turtles Data
  #Remove empties from line with variable names to get # of variables
  TurtleVarNames[[1]] <- TurtleVarNames[[1]][TurtleVarNames[[1]]!=""] 
  numvars <- length(TurtleVarNames[[1]]) #38 variables
  Turtlematrix<- do.call(rbind, TurtlesData) #Combine by rows
  #Remove all columns after 38th since empty, then name
  Turtlematrix <- Turtlematrix[,(1:38)] 
  colnames(Turtlematrix) <- TurtleVarNames[[1]]
  #Remove punctuation characters {,},[,], ",' ,
  Turtleclean <- aaply(Turtlematrix, .margins=1 , .fun=gsub, pattern="\\[|\\]|\\{|\\}|\"|\'", replacement="")
  #Get rid of redundant "breed" and white space in breed data
  Turtleclean[,9] <- gsub(pattern="breed|[[:blank:]]", replace="", x=Turtleclean[,9])
  
  ## 10) - extra data (about plot display) - remove so only relevant information
  # remove color, heading, shape, label, label-color, (2-8) size, pen-size, pen-mode, (11-13) 
  Turtleclean <- Turtleclean[,-c(2:8,11:13)]
  #  colnames(Turtleclean) #Remanining names
  Turtledf <- data.frame(Turtleclean, stringsAsFactors=F)
  #dim(Turtledf) #28 variables after removing ones that appear graphing related
  
  ## 5)
  
  #Divide into District, Voters, Activists, Parties, Candidates
  turtletypes <- unique(Turtledf$breed) #1 districts, 2 voters, 3 activits, 4 parties, 5 cands
  districts <- subset(Turtledf, breed=="districts")
  voters <- subset(Turtledf, breed=="voters")
  activists <- subset(Turtledf, breed=="activists")
  parties <- subset(Turtledf, breed=="parties")
  candidates <- subset(Turtledf, breed=="cands")
  
  # Divide vectors in districs data
  #Break down vectors into separate columns: "district-prefs" and "my-cands-district"
  newdistpref<- strsplit(x=districts[,26], split=" ") #split district prefs
  newcandist <- strsplit(x=districts[,27], split=" ") #split my-cands-dist
  dists <- matrix(unlist(newdistpref), ncol=3, byrow=T)
  colnames(dists) <- c("district.prefs1", "district.prefs2", "district.prefs3")
  candist <-  matrix(unlist(newcandist), ncol=3, byrow=T)
  candist <- candist[,2:3]
  colnames(candist) <- c("my.cands.district1", "my.cands.district2") #first value always "turtle"
  #New dataset with vectors separated
  districts <- cbind(districts[,-c(26,27)], dists, candist) 
  
  #Divide vectors in voters data
  #break down "prefs" and "this.voter.sal" - 15 and 17
  newprefs<- strsplit(x=voters[,15], split=" ") #split prefs
  prefs <- matrix(unlist(newprefs), ncol=3, byrow=T)
  colnames(prefs) <- c("prefs1", "prefs2", "prefs3")
  newthisvotersal<- strsplit(x=voters[,17], split=" ") #split this.voter.sal
  votersal <-  matrix(unlist(newthisvotersal), ncol=3, byrow=T)
  colnames(votersal) <- c("this.voter.sal1", "this.voter.sal2", "this.voter.sal3")
  #New dataset with vectors separated
  voters <- cbind(voters[,-c(15,17)], prefs, votersal) 
  
  #Divide vectors in activists data
  #colnames(activists)
  #break down "prefs" and "this.act.sal" - 15 and 18
  newprefs<- strsplit(x=activists[,15], split=" ") #split prefs
  prefs <- matrix(unlist(newprefs), ncol=3, byrow=T)
  colnames(prefs) <- c("prefs1", "prefs2", "prefs3")
  newthisactsal<- strsplit(x=activists[,18], split=" ") #split this.voter.sal
  actsal <-  matrix(unlist(newthisactsal), ncol=3, byrow=T)
  colnames(actsal) <- c("this.act.sal1", "this.act.sal2", "this.act.sal3")
  #New dataset with vectors separated
  activists <- cbind(activists[,-c(15,18)], prefs, actsal) 
  
  #Divide vectors in parties data
  #colnames(parties)
  #break down "mean.position", "my.cands.party", and "enforcement.point" - 20, 22, 23
  meanpos<- strsplit(x=parties[,20], split=" ") #split mean position
  prefs <- matrix(unlist(meanpos), nrow=2, byrow=T)
  colnames(prefs) <- c("mean.position1", "mean.position2", "mean.position3")
  newcandsparty<- strsplit(x=parties[,22], split=" ") #split my.cands.party
  candsparty <-  matrix(unlist(newcandsparty), nrow=2, byrow=T)
  colnames(candsparty) <- paste("my.cands.party", 1:209, sep="")
  enforce<- strsplit(x=parties[,23], split=" ") #split enforcement.point
  enfpoint <- matrix(unlist(enforce), nrow=2, byrow=T)
  colnames(enfpoint) <- c("enforcement.point1", "enforcement.point2", "enforcement.point3")
  #New dataset with vectors separated
  parties <- cbind(parties[,-c(20,22,23)], prefs, candsparty,enfpoint) 
  
  #Divide vectors in candidates data
  #colnames(candidates)
  #break down "positions.obs", "positions.obs.last" - 4, 12
  posobs<- strsplit(x=candidates[,4], split=" ") #split positions.obs
  prefs <- matrix(unlist(posobs), ncol=3, byrow=T)
  colnames(prefs) <- c("positions.obs1", "positions.obs2", "positions.obs3")
  lastposobs<- strsplit(x=candidates[,12], split=" ") #split positions.obs.last
  lastpos <-  matrix(unlist(lastposobs), ncol=3, byrow=T)
  colnames(lastpos) <- c("positions.obs.last1", "positions.obs.last2", "positions.obs.last3")
  #New dataset with vectors separated
  candidates <- cbind(candidates[,-c(4,12)], prefs, lastpos) 
  
  
  ## QUICK function to find heterogeneous variables
  #If there's no variation in column value FALSE, else TRUE
  uniquefunction <- function(x) {
    val <- ifelse(length(unique(x))==1,F,T)
  }

  #For districts
  removecol <- sapply(districts, FUN=uniquefunction) #returns false for each column with no variation in variable
  districtunique <- districts[,removecol] #remove homogenous variables
  #dim(districtunique) #Only 6 heterogeneous variables in districts
  #For Voters
  removecol <- sapply(voters, FUN=uniquefunction) #returns false for each column with no variation in variable
  votersunique <- voters[,removecol] #remove homogenous variables
  #dim(votersunique) #Only 9 heterogeneous variables 
  # For Activists
  removecol <- sapply(activists, FUN=uniquefunction) #returns false for each column with no variation in variable
  activistsunique <- activists[,removecol] #remove homogenous variables
  #dim(activistsunique) #Only 10 heterogeneous variables
  #For Parties
  removecol <- sapply(parties, FUN=uniquefunction) #returns false for each column with no variation in variable
  partyunique <- parties[,removecol] #remove homogenous variables
 # dim(partyunique) #219 heterogeneous variables in districts
  #For Candidates
  removecol <- sapply(candidates, FUN=uniquefunction) #returns false for each column with no variation in variable
  candidatesunique <- candidates[,removecol] #remove homogenous variables
  #dim(candidatesunique) #Only 12 heterogeneous variables 
  
  # Create CSVs
  write.csv(x=districtunique, file="Turtles/Districts.csv")
  write.csv(x=votersunique, file="Turtles/Voters.csv")
  write.csv(x=activistsunique, file="Turtles/Activists.csv")
  write.csv(x=partyunique, file="Turtles/Parties.csv")
  write.csv(x=candidatesunique, file="Turtles/Candidates.csv")

  
  ## PLOTS DIRECTORY ##  
  
  dir.create("Plots")
  setwd("Plots") #Save typing since rest of work in Plots directory
  
  ## 6 Position Plot
  dir.create("PositionPlot")
  
  #The first “Plot” records the average position of incumbent candidates (Red, Blue), activists, and
  #voters along each of six dimensions.  (D1, D2, D3, etc.) Only the first three are relevant here.
  
  #PLOTS at row 8531, D1: 8533 (8545-8715), D2: 8717 (8729-8899), D3: 8901 (8913-9083)
  Dim1 <- scan(netfile, skip=8544, nlines=1, what=" ", sep="\n") #Dim 1, first 2 lines are headers
  
  # Red Blue, Activists, Voters in first row (get 4 columns until next in lines below)
  Groups <- strsplit(x=Dim1[[1]], split=",") # 24 items before blanks
  ColorAndType <- gsub(pattern="[[:punct:]]", replace="", x=Groups[[1]])
  ColorType <- ColorAndType[seq(1,24,by=4)] #every 4 new type, only 24 columns of actual data
  #Make label that combines x or y with name in appropriate order
  positionlabels <- paste(rep(c("x","y"), 6), rep(ColorType, each=2), sep=".")

  #Extract Data
  Dim1dat <- scan(netfile, skip=8546, nlines=169, what=" ", sep="\n") #Dim 1, data
  Dim1Data <- strsplit(x=Dim1dat, split=",")
  Dim1matrix <-  matrix(unlist(Dim1Data), nrow=169, byrow=T)
  Dim1matrix <- Dim1matrix[, c(1,2,5,6,9,10,13,14,17,18,21,22)] #only x,y values
  colnames(Dim1matrix) <- positionlabels

  Dim2dat <- scan(netfile, skip=8730, nlines=169, what=" ", sep="\n") #Dimension 2 data
  Dim2Data <- strsplit(x=Dim2dat, split=",")
  Dim2matrix <-  matrix(unlist(Dim2Data), nrow=169, byrow=T)
  Dim2matrix <- Dim2matrix[, c(1,2,5,6,9,10,13,14,17,18,21,22)] #only x,y values
  colnames(Dim2matrix) <- positionlabels
  
  Dim3dat <- scan(netfile, skip=8914, nlines=169, what=" ", sep="\n") #Dimension 3 data
  Dim3Data <- strsplit(x=Dim3dat, split=",")
  Dim3matrix <-  matrix(unlist(Dim3Data), nrow=169, byrow=T)
  Dim3matrix <- Dim3matrix[, c(1,2,5,6,9,10,13,14,17,18,21,22)] #only x,y values
  colnames(Dim3matrix) <- positionlabels
  
  
  #CSV files for each of the three dimensions
  write.csv(x=Dim1matrix, file="PositionPlot/D1.csv")
  write.csv(x=Dim2matrix, file="PositionPlot/D2.csv")
  write.csv(x=Dim3matrix, file="PositionPlot/D3.csv")
  
  #Include a PDF file plotting (in some creative, meaningful, and labeled way) 
  #how these quantities varied across the simulation.
  
  pdf("PositionPlot/Positions.pdf", width=7, height=7) 
  par(mfrow=c(3,1))
  
  #Have 3 plots per page of each dimension, 
  #DIMENSION 1
  plot(Dim1matrix[,2]~Dim1matrix[,1], #Red Candidates D1
       xlab="Simulation", ylab="Dimension 1", ylim=c(-6,6), type="l", col="red", main="Candidates-Dim1") 
  lines(Dim1matrix[,3], Dim1matrix[,4], type="l", col="blue")
  plot(Dim1matrix[,5], Dim1matrix[,6], #Red Activists D1
       xlab="Simulation", ylab="Dimension 1", ylim=c(-6,6), type="l", col="red", main="Activists-Dim1") 
  lines(Dim1matrix[,11], Dim1matrix[,12], type="l", col="blue")
  plot(Dim1matrix[,7], Dim1matrix[,8], #Red Voters D1
       xlab="Simulation", ylab="Dimension 1", ylim=c(-6,6), type="l", col="red", main="Voters-Dim1") 
  lines(Dim1matrix[,9], Dim1matrix[,10], type="l", col="blue")
  
  #DIMENSION 2
  plot(Dim2matrix[,2]~Dim2matrix[,1], #Red Candidates
       xlab="Simulation", ylab="Dimension 2", ylim=c(-11,11), type="l", col="red", main="Candidates-Dim2") 
  lines(Dim2matrix[,3], Dim2matrix[,4], type="l", col="blue")
  plot(Dim2matrix[,5], Dim2matrix[,6], #Red Activists
       xlab="Simulation", ylab="Dimension 2", ylim=c(-11,11), type="l", col="red", main="Activists-Dim2") 
  lines(Dim2matrix[,11], Dim2matrix[,12], type="l", col="blue")
  plot(Dim2matrix[,7], Dim2matrix[,8], #Red Voters
       xlab="Simulation", ylab="Dimension 2", ylim=c(-11,11), type="l", col="red", main="Voters-Dim2") 
  lines(Dim2matrix[,9], Dim2matrix[,10], type="l", col="blue")
  
  #DIMENSION 3
  plot(Dim1matrix[,2]~Dim1matrix[,1], #Red Candidates
       xlab="Simulation", ylab="Dimension 3", ylim=c(-6,6), type="l", col="red", main="Candidates-Dim3") 
  lines(Dim1matrix[,3], Dim1matrix[,4], type="l", col="blue")
  plot(Dim1matrix[,5], Dim1matrix[,6], #Red Activists
       xlab="Simulation", ylab="Dimension 3", ylim=c(-6,6), type="l", col="red", main="Activists-Dim3") 
  lines(Dim1matrix[,11], Dim1matrix[,12], type="l", col="blue")
  plot(Dim1matrix[,7], Dim1matrix[,8], #Red Voters
       xlab="Simulation", ylab="Dimension 3", ylim=c(-6,6), type="l", col="red", main="Voters-Dim3") 
  lines(Dim1matrix[,9], Dim1matrix[,10], type="l", col="blue")
  
  par(mfrow=c(1,1))
  
  
  dev.off()
  
  ## 7 Winners Plot
  dir.create("WinnersPlot")
  
  #information about what percentage of candidates from each party “Won” in each cycle. 
  #The numbers may not add to 100. 
  #The “y” column shows the percentage and the “x” column shows the time period.
  
  #Extract Data - "WINNERS at 9130, x,y 9141-9309
  Winnersdat <- scan(netfile, skip=9140, nlines=169, what=" ", sep="\n") #Dim 1, data
  WinData <- strsplit(x=Winnersdat, split=",")
  Winmatrix <-  matrix(unlist(WinData), nrow=169, byrow=T)
  Winmatrix <- Winmatrix[, c(1,2,5,6,9,10)] #only x,y values for "BLUE", "FIFTY", and "RED"
  colnames(Winmatrix) <- paste(rep(c("x","y"), 3), rep(c("BLUE", "FIFTY", "RED"), each=2), sep=".")
  head(Winmatrix)
  
  #CSV file
  write.csv(x=Winmatrix, file="WinnersPlot/Winner.csv")
  
  #MAKE PDF
  pdf("WinnersPlot/Winner.pdf", width=7, height=7)
  
  par(mfrow=c(1,1))
  plot(Winmatrix[,4]~Winmatrix[,3], #Middle
       xlab="Simulation", ylab="Percent Win", ylim=c(0,100), type="l", main="Percentage Winners by Color") 
  lines(Winmatrix[,2]~Winmatrix[,1], type="l", col="blue") #BLUE PARTY
  lines(Winmatrix[,6]~Winmatrix[,5], type="l", col="red")  #RED PARTY
  
  dev.off()
  
  ## 8 Polariation Plot
  dir.create("PolarizationPlot")
  
  #Euclidean distance between the mean position of the candidates (TOTAL), voters, 
  #and activists associated with the two parties in each time period.
  
  #Extract Data - "Polarization at 9311, x,y 9322-9490
  Polardat <- scan(netfile, skip=9321, nlines=169, what=" ", sep="\n") #Polarization data
  PolarData <- strsplit(x=Polardat, split=",")
  Polarmatrix <-  matrix(unlist(PolarData), nrow=169, byrow=T)
  Polarmatrix <- Polarmatrix[, c(1,2,5,6,9,10)] #only x,y values for "TOTAL", "VOTERS", and "ACTIVISTS"
  colnames(Polarmatrix) <- paste(rep(c("x","y"), 3), rep(c("TOTAL", "VOTERS", "ACTIVISTS"), each=2), sep=".")
  
  #CSV file
  write.csv(x=Polarmatrix, file="PolarizationPlot/Polarization.csv")
  
  #MAKE PDF
  pdf("PolarizationPlot/PolariationPlot.pdf", width=7, height=7)
  
  plot(Polarmatrix[,2]~Polarmatrix[,1], #Candidates
       xlab="Simulation", ylab="Euclidian Distance", ylim=c(0,10), type="p", pch="c", cex=.6, main="Distance between Red and Blue") 
  lines(Polarmatrix[,4]~Polarmatrix[,3], type="p", pch="v", cex=.6) #Voters
  lines(Polarmatrix[,6]~Polarmatrix[,5], type="p", pch="a", cex=.6) #Activists
  
  dev.off()
  
  ## 9 Incumbent Percentage Plot
  dir.create("IncumbentPercentagePlot")
  
  #the percentage of incumbent candidates in each party that are “winning” in each time period
  
  #Extract Data - Incumbent at 9492, data start 9501
  Incumbdat <- scan(netfile, skip=9500, nlines=169, what=" ", sep="\n") #Incumbent Percentage data
  IncumbData <- strsplit(x=Incumbdat, split=",")
  Incumbmatrix <-  matrix(unlist(IncumbData), nrow=169, byrow=T)
  Incumbmatrix <- Incumbmatrix[, c(1,2)] #only x,y values for "PERCENT"
  colnames(Incumbmatrix) <- paste(rep(c("x","y"), 1), rep(c("PERCENT"), each=2), sep=".")
  head(Incumbmatrix)
  
  #CSV file
  write.csv(x=Incumbmatrix, file="IncumbentPercentagePlot/IncumbentWins.csv")
  
  #Make PDF
  pdf("IncumbentPercentagePlot/IncumbentWins.pdf", width=7, height=7)
  
  plot(Incumbmatrix[,2]~Incumbmatrix[,1], #Candidates
       xlab="Simulation", ylab="Percentage Winning", ylim=c(0,100), 
       type="l", main="Percentage of Incumbents Winning") 
  
  dev.off()
  
} # end function

## 2) The final directory should be structured as written in the assignment.
#  Run the function and examine the output - looked correct
netdirectory(netfile, directory=getwd())


### Part II: Extra Problems ###

## 1) JMR Chapter 4, Problems 3 and 4


# 3) Devise a program that outputs a table of squares and cubes of the numbers 1 to n.
  
sqrcube <- function(n=7) { # n is top value, default for example
  #Check value for n 
  if (n < 1) { #Check n
    print("N should be a real number greater than 0")
    break
  }
  title <- cat(c(format("number", justify="right", width=9), format("square", justify="right", width=9),format("cube", justify="right", width=9), "\n"), sep="")
  i=1
  for (i in 1:n) {
    cat(c(format(i, justify="right", width=9), format(i^2, justify="right", width=9),format(i^2, justify="right", width=9),"\n"), sep="")   
  } #end for loop to
} # End function 
sqrcube(7)

# Make into a program so it could be read from source, if desired
# dump(list="sqrcube", file="sqrcube.R")


# 4) Write an R program that prints out the standard multiplication table:
# Hint: generate a matrix mtable that contains the table, then use show(mtable)

multitable <- function(m,k) { #Takes inputs for table size m by k
  mtable <- matrix(nrow=m, ncol=k)
  for (i in 1:m) {
    mtable[i,] <- i*(1:k)
  }
  show(mtable)
  return(mtable)
}
#Prints out table, as well as returning matrix
m <- k <- 9
mtable <- multitable(m,k)


## 2) JMR Chapter 7, problems 3 and 4
library(lattice)

#3) Regression to the mean for population heights

#Below given in question:
# Create population
pop <- data.frame(m = rnorm(100, 160, 20), f = rnorm(100, 160, 20))
# Randomly computes heights for the next generation
next.gen <- function(pop) {
  pop$m <- sample(pop$m)
  pop$m <- apply(pop, 1, mean)
  pop$f <- pop$m
  return(pop)
}

#Use the function next.gen to generate nine generations
gens <- data.frame(height=pop$m, generation=factor(rep(1,100), levels=c(1:9)))
nextgen <- prevgen <- pop
for (i in 2:9) {
  nextgen <- next.gen(prevgen) #Get data for next generation
  gens <- rbind(gens, data.frame(height=nextgen$m, generation=factor(rep(i,100), levels=c(1:9))))
  prevgen <- nextgen # Setup for next iteration
}
head(gens)

#use histogram to plot the distribution of male heights, as in Fig 7.7 (3x3)
histogram(~height | generation, data=gens, 
          xlab="Height", main= "Distribution of Male Heights by Generation") 


#4 Reproduce Figure 6.1 using lattice graphics (pg 119 for origm says 106)
#Tree data - package for book
library(spuRs)
data(treeg)

#Each tree is a variable (tree.ID), each line a variable of tree's height (height.ft) over age (age)

xyplot(height.ft ~ age, group= tree.ID, data=treeg, 
       type="l", ylab="height (feet)", xlab="age (years)", col="black", tck=c(1,0)) #tck length 2 vector controls length of left/bottom and right/top



