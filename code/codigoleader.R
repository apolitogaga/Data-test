#Mi código (Sandra):
    
    #!/usr/bin/R
    # Copyright (c)2016 Accenture and/or its affiliates.  All Rights Reserved.  
    # You may not use, copy, modify, and/or distribute this code and/or its documentation without permission from Accenture.
    # Please contact the Advanced Analytics-Operations Analytics team and/or Frode Huse Gjendem (lead) with any questions.
    
    # brief: This is the starter script for the Accenture Datathon 2016 Competition.
    
    # version 1.0
    # date: 2016/05/20
    
rm(list = ls()) # Clear workspace.
#memory.limit(size = 40000)

# -----1. Set configuration & Data Import.----- 
#Sys.setlocale("LC_TIME", "English")
Sys.setlocale("LC_TIME", "en_US")

# ----- Attach packages -----
usePackage <- function(p) {
    if ( !is.element(p, installed.packages()[,1]) ) {
        install.packages(p, dep = TRUE)}
    require(p, character.only = TRUE)}

packages <- c("dplyr","reshape2","lubridate", "ggplot2", "ggmap", "ROCR", 
              "doParallel", "xgboost","rvest","stringr","foreach","doParallel", 
              "RCurl", "leaflet","rgdal", "nnet")

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
weather  <- read.csv("BCNweather-201001-201601.csv", header=T, nrows=-1, sep=",", stringsAsFactors = F)
holidays  <- read.csv("holidays.csv", header=T, nrows=-1, sep=",", stringsAsFactors = F, fileEncoding="latin1")


# -----2. Data Transformation.----- 

# Join accidents and type.cause data.
data <- accidents %>% left_join(type.cause, by="ID")

# Format the dates.
data$date <- as.Date(data$date, "%Y-%B-%d")
data$month <- month(data$date)

# Day of week
data$weekday<-weekdays(data$date)


# Remove accidents with missing date
data<-data[!is.na(data$date),]

# Group at prediction level.
data <- data %>%
    group_by(date, month, Shift, GridID, District, weekday) %>%
    summarise(minor  = sum(AccidentType=="Minor"),
              severe = sum(AccidentType=="Severe")) %>%
    na.omit()

# Clasify type of accident.
data$Accident <- 1

# Generate the no accidents data.
dates <- data.frame(date=seq(as.Date("2010-01-01"), as.Date("2014-12-31"), "days"))

noAccidents <- expand.grid(date=dates$date,
                           Shift=unique(data$Shift),
                           GridID=na.omit(unique(grid$GridID)),
                           stringsAsFactors = F)

# Generate the Month Variable.
noAccidents$month <- month(noAccidents$date)

# Create train data.
all <- data %>%
    full_join(noAccidents, by=c("date","month","Shift","GridID")) %>%
    left_join(grid, by="GridID")

# Accident type format.
all$Accident[is.na(all$Accident)] <- 0

#Variable: Number of accidents per node
n_accidents<-all %>%
    group_by(month, Shift, GridID) %>%
    summarise(obs=n(),
              NumberAccidents=sum(ifelse(Accident==1,1,0)),
              AccidentLikelihood = NumberAccidents/obs)
rm(all)

#Undersampling
n.ctrl<-nrow(noAccidents)
noAccidents<-noAccidents[sample(row.names(noAccidents),
                                nrow(data)*4,replace=F),]

# Adding District to noAccidents
districts<-aggregate(data[!is.na(data$District), c("date")], 
                     by=data[!is.na(data$District), c("GridID", "District")], 
                     FUN=length)
colnames(districts)<-c("GridID", "District", "n")

#There are several districts for the same Grid, so we keep the most common for each grid
#districts<-districts[order(districts[, c("GridID")], -districts[, c("n")]),]

districts<-districts %>% group_by(GridID) %>% filter(min_rank(n) == 1)
districts<-as.data.frame(districts)
districts<-districts[,c("GridID", "District")]

#noAccidents<-merge(noAccidents, districts, by="GridID", all.x=T)
noAccidents$District <- districts$District[match(noAccidents$GridID,districts$GridID)]
noAccidents[is.na(noAccidents$District),"District"]<-"No"


# Day of week
noAccidents$weekday<-weekdays(noAccidents$date)

# Create train data.
train <- data %>%
    full_join(noAccidents, by=c("date","month","Shift","GridID", "District")) %>%
    left_join(grid, by="GridID")
rm(noAccidents)

# Accident type format.
train$Accident[is.na(train$Accident)] <- 0

#Variables: Holidays
#Cleaning
holidays<-holidays[holidays$Holiday.type=="National holiday" | 
                       holidays$Where.it.is.observed %in% c("16 states", "CT, LO, NA, PV, VC",
                                                            "Catalonia", "Barcelona", 
                                                            "All except AN, AR, CE, CL, EX, LO, M, ML, MU, O, VC",
                                                            "CT, LO, NA, PV, S, VC", "All except AN, AR, CL, CN, EX, M, ML, O, S",
                                                            "Catalonia, Galicia", "All except AN, AR, CE, EX, LO, ML, MU, O, VC",
                                                            "B, CT, IB, MU"),]
holidays<-holidays[!(holidays$Date=="Mar 28" & holidays$Holiday.type=="Local holiday"),]

train$dateJoin<-paste(format(train$date,"%b"), day(train$date) ) 
train$year<-year(train$date)

train$dateJoin[1]==holidays$Date[1]
train$year[1]==holidays$year[1]

holidays$Weekday<-NULL
train<-merge(train, holidays, by.x=c("dateJoin", "year"), 
             by.y=c("Date", "year"), all.x=T)
train[is.na(train$Holiday.type),"Holiday.type"]<-"No"


#Variables:Weather

#Variable: accidents
train <- train %>%
    left_join(n_accidents[, c("month", "Shift", "GridID", "AccidentLikelihood")], by=c("month", "Shift", "GridID")) 


# # -----2.1 Plots of relation between variables and accidents
# # This part should be commented for scoring
# variables<-c("month", "Shift", "GridID", "District", "weekday", "AccidentLikelihood",
#"Holiday.type")
# pdf(file="C:/Datathon/plots/Distributions.pdf")
# average_global<-mean(train$Accident)
# for(i in variables){
#   #We plot the target average and the target average per class of the variable
#   data_plot<-aggregate(train$Accident, by=train[i], FUN=mean)
#   colnames(data_plot)<-c("variable", "average_prob")
#   
#   # Histogram overlaid with kernel density curve
#   print(ggplot(data_plot, aes(x=variable, y=average_prob, group=1)) + 
#           geom_bar(stat="identity", fill="white", colour="darkgrey") +
#           geom_line(y=average_global) + ggtitle(i))
#   
# }
# dev.off()

# #Plot the accidents
# # map<-get_map(location = 'Catalunya', zoom = 7, maptype = "terrain")
# # save(map, file="C:/Datathon/plots/Catalunyamapa.RData")
# load("C:/Datathon/plots/Catalunyamapa.RData")
# print(ggmap(map) + geom_point(data=data_site_i, aes(x=x, y=y, size=n_accounts), color="darkgreen", stroke = 0,
#                               show.legend = TRUE, alpha=1) + 
#         ggtitle(i) )


#Split into train and test
#Another test: try 2014 as test
set.seed(38277)
train_id<-round(runif(n = round(0.80*nrow(train)), 1, nrow(train)),0)
train_train<-train[train_id,]
train_test<-train[-train_id,]
rm(train)

# # -----3. Run Naive Model.-----
# naive.model <- rbind(train_train,train_test) %>%
#   group_by(month, Shift, GridID) %>%
#   summarise(obs=n(),
#             NumberAccidents=sum(ifelse(Accident==1,1,0)),
#             AccidentLikelihood = NumberAccidents/obs)
# 
# predict_train <- train_train %>%
#   left_join(naive.model, by=c("month", "Shift", "GridID")) 
# predict_train<-predict_train[,"AccidentLikelihood"]
# predict_test <- train_test %>%
#   left_join(naive.model, by=c("month", "Shift", "GridID")) 
# predict_test<-predict_test[,"AccidentLikelihood"]
# 
# #Print AUC
# cat("Train:", my.AUC(train_train$Accident,predict_train$AccidentLikelihood))
# cat("Test:", my.AUC(train_test$Accident,predict_test$AccidentLikelihood))
# #Train: 0.9028931
# #Test: 0.866971
# 
# #0.68

# # -----3. Train Model.----- 
#Training
variables<-c("month", "Shift", "GridID", "District", "weekday", "AccidentLikelihood",
             "Holiday.type")
target<-"Accident"
formula<-as.formula(paste0(target, " ~ ", paste0(variables, collapse=" + ")))
# nnet.model<-nnet( formula, train_train, size = 2, rang = 0.5, maxit = 100)
# save(nnet.model, file="C:/Datathon/output/nnet.model2.RData")
# 
# #Prediction for train_train & train_test
# predict_train<-predict(nnet.model, train_train )
# predict_test<-predict(nnet.model, train_test )
# 
# #Print AUC
# cat("Train:", my.AUC(train_train$Accident,predict_train))
# cat("Test:", my.AUC(train_test$Accident,predict_test))

#Arreglo de la variable de weekday
train_train[is.na(train_train$weekday.y),"weekday.y"]<-train_train[is.na(train_train$weekday.y),"weekday.x"]
train_train$weekday.x<-NULL
colnames(train_train)<-c("dateJoin",             "year",                 "date",                 "month",                "Shift",               
                         "GridID",               "District",           "minor",                "severe",              
                         "Accident",             "weekday",            "Longitude.grid",       "Latitude.grid",        "Holiday.name",        
                         "Holiday.type",         "Where.it.is.observed", "AccidentLikelihood"  )

train_test[is.na(train_test$weekday.y),"weekday.y"]<-train_test[is.na(train_test$weekday.y),"weekday.x"]
train_test$weekday.x<-NULL
colnames(train_test)<-c("dateJoin",             "year",                 "date",                 "month",                "Shift",               
                        "GridID",               "District",           "minor",                "severe",              
                        "Accident",             "weekday",            "Longitude.grid",       "Latitude.grid",        "Holiday.name",        
                        "Holiday.type",         "Where.it.is.observed", "AccidentLikelihood"  )


#Logistic regression
glm.model<-glm(formula, data=train_train)

rf.model <- randomForest(formula,data=train_train,ntree=100,proximity=false)

ksv.model <- ksvm(formula,data=train_train)

save(glm.model, file="glm.model.RData")

?glm
#Prediction for train_train & train_test
predict_train<-predict(glm.model, train_train )
predict_test<-predict(glm.model, train_test )

#Print AUC
cat("Train:", my.AUC(train_train$Accident,predict_train))
cat("Test:", my.AUC(train_test$Accident,predict_test))


#Prueba con el mes como caracter
train_train$month_car<-as.character(train_train$month)
train_test$month_car<-as.character(train_test$month)
#Logistic regression

variables<-c("month_car", "Shift", "GridID", "District", "weekday", "AccidentLikelihood",
             "Holiday.type")
formula<-as.formula(paste0(target, " ~ ", paste0(variables, collapse=" + ")))
names(train_train)
glm.model<-glm(formula, data=train_train)
save(glm.model, file="glm.model_mescar.RData")

#Prediction for train_train & train_test
predict_train<-predict(glm.model, train_train )
predict_test<-predict(glm.model, train_test )

#Print AUC
cat("Train:", my.AUC(train_train$Accident,predict_train))
cat("Test:", my.AUC(train_test$Accident,predict_test))

#Un modelo por tipo de accidente?



# -----4. Perform the prediction.----- 
# Format test dates.
test$date <- as.Date(test$date, "%Y-%m-%d")
test$month <- month(test$date)

#Adding the rest of the variables
#District
test$District <- districts$District[match(test$GridID,districts$GridID)]
test[is.na(test$District),"District"]<-"No"

#weekday
test$weekday<-weekdays(test$date)

#AccidentLikelihood
test <- test %>%
    left_join(n_accidents[, c("month", "Shift", "GridID", "AccidentLikelihood")], by=c("month", "Shift", "GridID")) 


#Holiday.type
test$dateJoin<-paste(format(test$date,"%b"), day(test$date) ) 
test$year<-year(test$date)

test$dateJoin[1]==holidays$Date[1]
test$year[1]==holidays$year[1]
test<-merge(test, holidays, by.x=c("dateJoin", "year"), 
            by.y=c("Date", "year"), all.x=T)
test[is.na(test$Holiday.type),"Holiday.type"]<-"No"


# # Use train ratio.
# test <- test %>%
#   left_join(naive.model, by=c("month", "Shift", "GridID")) 

#Prediction
load("glm.model.RData")
test$predict<-predict(glm.model, test )

#Pasamos los negativos a cero
test[test$predict<0,"predict"]<-0
test[test$predict>1,"predict"]<-1

# -----5. Save the submission.----- 
submission <- test %>%
    select(date, Shift, GridID, predict)
colnames(submission)<-c("date", "Shift", "GridID", "AccidentLikelihood")
# Write the final CSV file.
write.csv(submission, file=paste0(OUTPATH,'/sample-submission.csv'), row.names = F)
# Please, remember than in order to make the submission you need to create a .zip file with the csv

# Clear memory.
rm(list = ls()) 


