##########
##########
##Testing GitHub Fork of tool to download and import data for ndx graphing: 
##  Sam Clark's getUsaMortalityData-v0.1.R (https://github.com/sinafala/usa-mortality/blob/master/getUsaMortalityData-v0.1.R)
##
setwd("")
##########
##########

#####Begin getUsaMortalityData-v0.1.R

# getUsaMortalityData.R Version 0.1

# Sam Clark
# License GPL3
# 2018-12-31

# clear things out
rm(list=ls())

# install required packages that may not be installed now
list.of.packages <- c(
  "readr",
  "stringr",
  "httr"
)
# identify required packages that are not already installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install required packages that are not already installed
if(length(new.packages)) install.packages(new.packages)

# load necessary packages
library(readr)
library(stringr)
library(httr)

##########################################################
# function to download zip file containing USA life tables
##########################################################
download.usa <- function (output.file,unzip.dir,usa.user,usa.pass) {
  
  # output.file is name of file to be created when the zip file is unzipped
  # unzip.dir is the directory where you want to put that file
  # usa.user is your usa.mortality.org user name
  # usa.pass is you usa.mortality.org password
  
  # return: a list describing various aspects of an attempted download
  
  url <- "https://usa.mortality.org/uploads/lifetables/lifetables.zip"
  print("Downloading USA ...")
  usa.zip <- try(GET(
    url,authenticate(usa.user,usa.pass)
    ,write_disk(output.file,overwrite=TRUE)
    ,progress(),show.error.messages=FALSE
  ))
  if(class(usa.zip)=="try-error") {
    return(usa.zip)
    stop(attributes(usa.zip)$condition)
  } else if (http_status(usa.zip)$category=="Client error") {
    if(http_status(usa.zip)$message=="Client error: (401) Unauthorized") {
      return(usa.zip)
      stop("Check user name and password and try again.")    
    } else {
      return(usa.zip)
      stop(http_status(usa.zip)$message)
    }
  } 
  unzip(zipfile=output.file,exdir=unzip.dir)
  return(usa.zip)
}

##########################################
# function to read life tables into a list
##########################################
readUsaLts <- function(age.period,data.dir) {
  
  # age.period is the agexperiod aggregation you want, e.g. '1x1' or '5x5'
  # data.dir is the directory where you unzipped the zip file downloaded from usa.mortality.org
  
  # return: a list containing
  #   lts, the life tables in a list
  #   age, the age aggregation you input, e.g. '1' or '5'
  #   lts.read, the total life tables read
  
  # extract the age identifier from the age.period argument
  age <- str_sub(age.period,1,1)
  # set the number of age groups
  if (age=="1") {
    ages <- 111
  } else if (age=="5") {
    ages <- 24
  } else {
    # stop age not '1' or '5'
    stop("Invalid 'age.period' argument")
  }
  
  # get the regions in the data directory
  regions <- Sys.glob(paste(eval(data.dir),"/*",sep=""))
  # initialize life tables list
  lts <- list()
  # initialize total life tables count
  lts.tot <- 0
  # loop over the regions
  for (region in regions) {
    # get the subregions
    subregions <- Sys.glob(paste(eval(region),"/*",sep=""))
    # loop over subregions
    for (subregion in subregions) {
      # get the names of the CSV files
      csvs <- Sys.glob(paste(eval(subregion),"/*_",age.period,".csv",sep=""))
      # loop over the CSV files    
      for (csv in csvs) {
        # save the region, subregion, and sex
        region <- basename(eval(region))
        sub.region <- basename(eval(subregion))
        # retrieve the sex from the CSV file name
        sex <- str_sub(str_split(eval(csv),"_")[[1]][2],1,1)
        # read the life tables from the CSV, all of them
        lts.long <- read_csv(eval(csv),col_types=cols())
        # calculate how many life tables there are
        lts.num <- dim(lts.long)[1]/ages
        # loop ver the life tables
        for (lt in 1:lts.num) {
          # store the begin and end rows for this life table
          row.begin <- (lt*ages)-(ages-1)
          row.end <- lt*ages
          # store the period for this life table
          period <- paste("per.",lts.long[row.begin,3],sep="")
          # finally read this life table and store it in the list
          lts[[region]][[sub.region]][[sex]][[period]] <- lts.long[(row.begin:row.end),]
          # increment total life tables count
          lts.tot <- lts.tot+1
        }
      }
    }
  }
  return <- list(
    lts = lts,
    age = age,
    lts.read = lts.tot
  )
}

#############################################################
# function to extract a specified column from all life tables 
#   and store the results in a single numeric matrix
#############################################################
extractLtCol <- function (lts.list,sex,col.name) {
  
  # lts.list is a list object returned by the function readUsaLts()
  # sex is sex to be retrieved:
  #   'b'
  #   'f'
  #   'm'
  # colname name of column to be retrieved and stored in a matrix:
  #   'popname'
  #   'sex'
  #   'year'
  #   'age'
  #   'mx'
  #   'qx'
  #   'ax'
  #   'lx'
  #   'dx'
  #   'Lx'
  #   'Tx'
  #   'ex'
  
  # return: a numeric age by life table matrix containing values
  #   from the desired column, e.g. a matrix containing the qx 
  #   columns from all the life tables of the specified sex

  # set the number of age groups
  if (lts.list$age == 1) {
    ages <- 111
  } else if (lts.list$age == 5) {
    ages <- 24
  } else {
    # stop if invalid age
    stop("Invalid 'age' argument")
  }
  
  # set the column number of the desired column
  if (col.name == "popname") {
    col <- 1  
  } else if (col.name == "sex") {
    col <- 2
  } else if (col.name == "year") {
    col <- 3
  } else if (col.name == "age") {
    col <- 4
  } else if (col.name == "mx") {
    col <- 5
  } else if (col.name == "qx") {
    col <- 6
  } else if (col.name == "ax") {
    col <- 7
  } else if (col.name == "lx") {
    col <- 8
  } else if (col.name == "dx") {
    col <- 9
  } else if (col.name == "Lx") {
    col <- 10
  } else if (col.name == "Tx") {
    col <- 11
  } else if (col.name == "ex") {
    col <- 12
  } else {
    col <- NA
  }
  
  # extract the life tables from the input list
  lts <- lts.list$lts
  
  # store the number of life tables for one sex (sexes={"f","m","b"})
  lts.count <- lts.list$lts.read/3
  
  # initilize the output matrix
  if (col > 4) {
    # zeroes for numeric columns
    lts.colmat <- matrix(data=rep(0,ages*lts.count),nrow=ages,ncol=lts.count)
  } else {
    # empty strings for character columns
    lts.colmat <- matrix(data=rep("",ages*lts.count),nrow=ages,ncol=lts.count)
  }
  
  # initialize the output matrix column names vector
  col.names <- rep("",lts.count)
  # initialize output matrix column counter
  col.index <- 1
  # loop over regions
  for (region in 1:length(lts)) {
    # loop over subregions
    for (subregion in 1:length(lts[[region]])) {
      # loop over periods
      for (period in 1:length(lts[[region]][[subregion]][[sex]])) {
        # store the desired column in the output matrix
        if (col > 4) {
          # numeric columns
          lts.colmat[,col.index] <- as.numeric(unlist(lts[[region]][[subregion]][[sex]][[period]][,col]))
        } else {
          # character columns
          lts.colmat[,col.index] <- as.character(unlist(lts[[region]][[subregion]][[sex]][[period]][,col]))
        }
        # store the region, subregion, sex, and period 
        region.chr <- names(lts)[region]
        subregion.chr <- names(lts[[region]])[subregion]
        # keep just the period part of the period name
        period.chr <- str_sub(names(lts[[region]][[subregion]][[sex]])[period],5)
        # add the column identifier for the output matrix to the list vector of column names
        col.names[col.index] <- paste(region.chr,subregion.chr,sex,period.chr,sep=".")
        # increment the output matrix column counter
        col.index <- col.index + 1          
      }
      # }
    }
  }
  # name the columns of the output matrix
  colnames(lts.colmat) <- col.names
  # set the output matrix row names to the age groups
  rownames(lts.colmat) <- unlist(lts[[1]][[1]][[1]][[1]][,4])
  # return the output matrix
  return(lts.colmat)
}

###############
# Example usage - MODIFIED FROM HER BY EDDIEH - MOST EXAMPLES INFO REMOVED - TO JUST GET DATA FOR THE ndx GRAPHING PURPOSE
###############

# WARNING: contents of the directory './example-data' in your working directory 
#   will be removed
# delete directory './example-data'
unlink("./example-data",recursive=TRUE)
# create empty directory './example-data'
dir.create("./example-data")

# download the usa mortality zip file 
output.file <- "./example-data/USA-lifetables.zip"
unzip.dir <- "./example-data"
usa.user <- "" #"<usa.mortality.org user name>"
usa.pass <- "" #"<usa.mortality.org password>"
usa.lts.download <- download.usa(output.file,unzip.dir,usa.user,usa.pass)

# parse the raw 1x10 (single year of age and ten calendar year) life tables into a list
age.period <- "1x10"
data.dir <- "./example-data/lifetables"
usa.lts <- readUsaLts(age.period,data.dir)
# explore resulting list a little
# number of life tables read
usa.lts$lts.read
# age aggregation, 1 -> single year in age
usa.lts$age
# extract the life tables list
lts <- usa.lts$lts
# explore the life tables list a little
names(lts)
names(lts$States)
names(lts$States$AK)
names(lts$States$AK$b)
names(lts$States$AK$b$per.1960)

#####End getUsaMortalityData-v0.1.R

##########
##########
##Graphing 10yr life table ndx data for Alaska
##Data from the United States Mortality Database (usa.mortality.org)
##
##Eddie Hunsinger, January 2019
##########
##########

##Pyramid plots (Using Carl Mason's great pyramid function: https://lab.demog.berkeley.edu/Docs/Faq/faq/fq.shtml#SECTION00421000000000000000)
##And also thanks to http://www.shizukalab.com/toolkits/overlapping-histograms
poppyr3<-function(female,male,cat,dog){
par(new=TRUE)
split.screen(figs=rbind(c(0,.58,0,1),c(.43,1,0,1)))
screen(1)
barplot(female,horiz=T,names=cat,cex.names=.8,space=0,las=2,axes=FALSE,
xlim=c(5000,0),col=dog)
title("",line=-3,cex.main=1)
screen(2)
barplot(male,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,
xlim=c(0,5000),col=dog)
title("",line=-3,cex.main=1)
close.screen(all=T)}

cat<-read.table(file="https://raw.githubusercontent.com/edyhsgr/DeathsDistributionGraphing/master/agelabels.csv",sep=",",header=TRUE)
cat<-cat$x

##1970s - I use 1x10 (age by years) data - called 1970 after selected
female<-lts$States$AK$f$per.1970$dx
male<-lts$States$AK$m$per.1970$dx
dog<-rgb(1,1,0,0.35)
poppyr3(female,male,cat,dog)

female<-lts$States$AK$f$per.2000$dx
male<-lts$States$AK$m$per.2000$dx
dog<-rgb(0,1,1,0.35)
poppyr3(female,male,cat,dog)

mtext(side=3,line=1,text=expression("Period Life Table Deaths (" * ""[n] * d[x] *") by Sex, Alaska, 1970s and 2000s"),cex=1)
mtext(side=3,line=.2,text="(From the United States Mortality Database - usa.mortality.org - January 2019.)",font=1,cex=.75)
mtext(side=1,line=1,adj=.175,text=expression("Female"),font=1,cex=1)
mtext(side=1,line=1,adj=.75,text=expression("Male"),font=1,cex=1)
mtext(side=1,line=-24.5,adj=-.275,text="Age",font=1,cex=1)
legend(0,.5, legend=c("2000s","Overlap","1970s"), col=c(rgb(0,1,1,0.35), 
     rgb(1,1,0,0.35),rgb(1,1,0,0.35)), pt.cex=2, pch=15, cex=1, bty ="n", y.intersp=1.25)
legend(0,.5, legend=c("","",""), col=c(rgb(1,1,0,0), 
     rgb(0,1,1,0.35),rgb(0,1,1,0)), pt.cex=2, pch=15, cex=1, bty ="n", y.intersp=1.25)
legend(0,.5, legend=c("","",""), col=c(rgb(0,0,0), 
     rgb(0,0,0),rgb(0,0,0)), pt.cex=2, pch=0, cex=1, bty ="n", y.intersp=1.25)

