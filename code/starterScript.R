#!/usr/bin/R
# Copyright (c)2016 Accenture and/or its affiliates.  All Rights Reserved.  
# You may not use, copy, modify, and/or distribute this code and/or its documentation without permission from Accenture.
# Please contact the Advanced Analytics-Operations Analytics team and/or Frode Huse Gjendem (lead) with any questions.

# brief: This is the starter script for the Accenture Datathon 2016 Competition.

# version 1.0
# date: 2016/05/20

rm(list = ls()) # Clear workspace.


# -----1. Set configuration & Data Import.----- 
# Sys.setlocale("LC_TIME", "English")
Sys.setlocale("LC_TIME", "en_US")

# ----- Attach packages -----
usePackage <- function(p) {
  if ( !is.element(p, installed.packages()[,1]) ) {
    install.packages(p, dep = TRUE)}
  require(p, character.only = TRUE)}

packages <- c("dplyr","reshape2","lubridate", "ggplot2", "ggmap", "caret", "ROCR", 
              "doParallel", "xgboost","rvest","stringr","foreach","doParallel", 
              "RCurl", "leaflet","rgdal", "caret")


for (p in packages) { usePackage(p) }


# ----- AUC function -----
#' It computes the AUC for 64bit numbers.
#' @param actual is the actual output (i.e., gound truth).
#' @param predicted is the prediction itself.
#' @param decimals are the number of decimals to compute AUC.
#' @return the AUC of the prediction.
my.AUC <- function (actual, predicted, decimals = 6) {
  predicted <- round(predicted, decimals)
  r <- as.numeric(rank(predicted))
  n_pos <- as.numeric(sum(actual == 1))
  n_neg <- as.numeric(length(actual) - n_pos)
  auc <- (sum(r[actual == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos *  n_neg)
  return(auc)
}


# Enter your input data and output data paths below.
PATH = getwd() # Otherwise use your own path
OUTPATH = getwd() 
# Set the input data folder as default path.
setwd(PATH)

# ----- Data Import -----
# Read the input files.
accidents  <- read.csv("accidents.csv", header=T, nrows=-1, sep=",", stringsAsFactors = F)
type.cause <- read.csv("type-cause.csv", header=T, nrows=-1, sep=",", stringsAsFactors = F)
grid       <- read.csv("city-grid.csv", header=T, nrows=-1, sep=",", stringsAsFactors = F)
test       <- read.csv("test.csv", header=T, nrows=-1, sep=",", stringsAsFactors = F)




# -----2. Data Transformation.----- 

# Join accidents and type.cause data.
data <- accidents %>% left_join(type.cause, by="ID")

# Format the dates.
data$date <- as.Date(data$date, "%Y-%B-%d")
data$month <- month(data$date)

# Group at prediction level.
data <- data %>%
  group_by(date, month, Shift, GridID) %>%
  summarise(minor  = sum(AccidentType=="Minor"),
            severe = sum(AccidentType=="Severe")) %>%
  na.omit()

# Clasify type of accident.
data$Accident <- TRUE

# Generate the no accidents data.
dates <- data.frame(date=seq(as.Date("2010-01-01"), as.Date("2014-12-31"), "days"))

noAccidents <- expand.grid(date=dates$date,
                           Shift=unique(data$Shift),
                           GridID=na.omit(unique(grid$GridID)),
                           stringsAsFactors = F)

# Generate the Month Variable.
noAccidents$month <- month(noAccidents$date)

# Create train data.
train <- data %>%
  full_join(noAccidents, by=c("date","month","Shift","GridID")) %>%
  left_join(grid, by="GridID")

# Accident type format.
train$Accident[is.na(train$Accident)] <- FALSE



class(train$date)
head(train)
summary(train)
table(train$month)
table(train$Shift)
table(train$severe)

table(train$minor)
table(train$severe)

train$minor[is.na(train$minor)]<-0
train$severe[is.na(train$severe)]<-0
table(train$minor)
table(train$severe)
test = train$minor
table(test)
test[train$minor>0]
test2 = test
test2[train$severe==1]<-1


save(test,file="test.rdata")
save(train,file="train.rdata")


library(xgboost)

summary(train$severe)
# -----3. Run Naive Model.----- 
naive.model <- train %>%
  group_by(month, Shift, GridID) %>%
  summarise(obs=n(),
            NumberAccidents=sum(ifelse(Accident=="TRUE",1,0)),
            AccidentLikelihood = NumberAccidents/obs)


# -----4. Perform the prediction.----- 
# Format test dates.
test$date <- as.Date(test$date, "%Y-%m-%d")
test$month <- month(test$date)

# Use train ratio.
test <- test %>%
  left_join(naive.model, by=c("month", "Shift", "GridID")) 


# -----5. Save the submission.----- 
submission <- test %>%
  select(date, Shift, GridID, AccidentLikelihood)

# Write the final CSV file.
write.csv(submission, file=paste0(OUTPATH,'/sample-submission.csv'), row.names = F)
# Please, remember than in order to make the submission you need to create a .zip file with the csv

# Clear memory.
rm(list = ls()) 

(res = my.AUC(test,submission))


sapply(submission$AccidentLikelihood,sum, submission$GridID)
