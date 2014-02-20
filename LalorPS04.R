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
  
  #Make model name 2nd line of file (appears to be either model or file name)
  ModelTime <- scan(netfile, skip=1, nlines=2, what=" ", sep=",") #Only need first item in 2 lines
  ModelName <- gsub(pattern="nlogo", replacement="", x=ModelTime[1]) #get rid of extension
  Dates <- strsplit(ModelTime[85], split=" ") #get separation of dates/times
  Day <- as.Date(Dates[[1]][1], format ='%m/%d/%Y') #convert time to useable format: YEAR-MN-DY
  Time <- strsplit(Dates[[1]][2], split=":") #Time separated by :, second item in Dates
  ModelDat <- paste(ModelName, Day, ".", Time[[1]][1], Time[[1]][2], sep="") #Make one string, separate by periods

  
  #ModelName root
  dir.create(ModelDat)
  list.dirs() #Can see that ModelName is now a directory in working directory
  #Change working directory to ModelName, since that's root of described tree
  setwd(ModelDat)
  getwd() #Now it's the working directory

  
  # List of Globals
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
  
  
  ## 10 - extra data (about plot display) - remove so only relevant information
  # remove color, heading, shape, label, label-color, (2-8) size, pen-size, pen-mode, (11-13)
  # 
  Turtleclean <- Turtleclean[,-c(2:8,11:13)]
  #  colnames(Turtleclean) #Remanining names
  Turtledf <- data.frame(Turtleclean, stringsAsFactors=F)
  #dim(Turtledf) #28 variables after removing ones that appear graphing related
  
  
  ##5 Turtle directory ##
  
  
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
  colnames(parties)
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
  colnames(candidates)
  #break down "positions.obs", "positions.obs.last" - 4, 12
  posobs<- strsplit(x=candidates[,4], split=" ") #split positions.obs
  prefs <- matrix(unlist(posobs), ncol=3, byrow=T)
  colnames(prefs) <- c("positions.obs1", "positions.obs2", "positions.obs3")
  lastposobs<- strsplit(x=candidates[,12], split=" ") #split positions.obs.last
  lastpos <-  matrix(unlist(lastposobs), ncol=3, byrow=T)
  colnames(lastpos) <- c("positions.obs.last1", "positions.obs.last2", "positions.obs.last3")
  #New dataset with vectors separated
  candidates <- cbind(candidates[,-c(4,12)], prefs, lastpos) 
  
  
  
  #If there's no variation in column value remove
  uniquefunction <- function(x) {
    val <- ifelse(length(unique(x))==1,F,T)
  }

  #For districts
  removecol <- sapply(districts, FUN=uniquefunction) #returns false for each column with no variation in variable
  districtunique <- districts[,removecol] #remove homogenous variables
  dim(districtunique) #Only 6 heterogeneous variables in districts
  #For Voters
  removecol <- sapply(voters, FUN=uniquefunction) #returns false for each column with no variation in variable
  votersunique <- voters[,removecol] #remove homogenous variables
  dim(votersunique) #Only 9 heterogeneous variables 
  # For Activists
  removecol <- sapply(activists, FUN=uniquefunction) #returns false for each column with no variation in variable
  activistsunique <- activists[,removecol] #remove homogenous variables
  dim(activistsunique) #Only 10 heterogeneous variables
  #For Parties
  removecol <- sapply(parties, FUN=uniquefunction) #returns false for each column with no variation in variable
  partyunique <- parties[,removecol] #remove homogenous variables
  dim(partyunique) #219 heterogeneous variables in districts
  #For Candidates
  removecol <- sapply(candidates, FUN=uniquefunction) #returns false for each column with no variation in variable
  candidatesunique <- candidates[,removecol] #remove homogenous variables
  dim(candidatesunique) #Only 12 heterogeneous variables 
  
  # Create CSVs
  write.csv(x=districtunique, file="Turtles/Districts.csv")
  write.csv(x=votersunique, file="Turtles/Voters.csv")
  write.csv(x=activistsunique, file="Turtles/Activists.csv")
  write.csv(x=partyunique, file="Turtles/Parties.csv")
  write.csv(x=candidatesunique, file="Turtles/Candidates.csv")
  
  ## END TURTLE ##
  
  ## PLOTS ##
  
  } # end function

## 2) The final directory should be structured as written in the assignment.
# - Run the function and examine the output

## 3) The final model output has three sections of interest

# The first item are the “Globals”, which are either parameter settings used to initiate the model, 
# or aggregate quantities of value as recorded in the final period of the simulation

# Traits are associated with the various agents (Turtles) in the simulation as
# they were in the final period of the simulation

# various “plots” that recorded quantities of interest (e.g., the mean position 
# of “Blue” candidates) periodically throughout the simulation.


## 4) The Top level directory should be named such that it incorporates the name of the file used as well
# as date and time of simulation


## 5) Turtles directory should contain separate csv files containing the relevant information for
# districts, voters, activists, candidates, and parties

## 6) The first “Plot” records the average position of incumbent candidates (Red, Blue), activists, 
# and voters along each of six dimensions. (D1, D2, D3, etc.) Only the first three are relevant here

# This directory should contain CSV files for each of the separate dimensions

# also include a PDF file plotting (in a meaningful and labeled way) how these quantities varied across the simulation


## 7) The “Winners” Plot contains information about what percentage of candidates from each party
# “Won” in each cycle. The numbers may not add to 100. The “y” column shows the percentage,
# and the “x” column shows the time period. You should again make a CSV and PDF file for this information.


## 8) The polarization plot shows the Euclidean distance between the mean position of the candidates
#(TOTAL), voters, and activists associated with the two parties in each time period. The task is the same as before.


## 9) The “Incumbent” output shows the percentage of incumbent candidates in each party that are
# “winning” in each time period. The assignment is the same as above.


## 10) Throughout the file there is extra information (mostly specifying how specific plots are to be dis-
# played, the color they should be plotted etc. in the NetLogo interface).



### Part II: Extra Problems ###

## 1) JMR Chapter 4, Problems 3 and 4


# 3) Devise a program that outputs a table of squares and cubes of the numbers 1 to n.
  
sqrcube <- function(n) { # n is top value 
  #Check value for n 
  if (n < 1) { #Check n
    print("N should be a real number greater than 0")
    break
  }
  title <- c("number", "square", "cube")
  sprintf(title, justify="right") #alt: format()
  for (i in 1:n) {
    cat(i, i^2, i^3, "\n", sep="\t")
    format()
  } #end for loop to
} # End function - says program, may want to make a script
sqrcube(3)


# 4) Write an R program that prints out the standard multiplication table:
# Hint: generate a matrix mtable that contains the table, then use show(mtable)



## 2) JMR Chapter 7, problems 3 and 4

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

#Use the function next.gen to generate nine generations, then use histogram to plot the distribution of male heights in each gen
gens <- vector("list", 9)
gen1 <- next.gen(pop)
gen2 <- next.gen(gen1)
i=1
prevgen <- pop
for (i in 1:9) {
  gens[i] = next.gen(prevgen)
  prevgen <- gens[i]
}

name <- paste("gen",1, sep="")


#4 Reproduce Figure 6.1 using lattice graphics (pg 102 for orig)



