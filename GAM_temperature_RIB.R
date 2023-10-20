###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################
library(lme4)
library(ggplot2)
library(nlme)
library(MuMIn)
library(performance)


######################################################
#Bolstad
######################################################
tracking_bolstad <-readRDS("./data/modified_data/fever_bolstad.RDS")
str(tracking_bolstad)
summary(as.factor(tracking_bolstad$sensor))
#only temp data in this dataset - all good. Also latlong
summary(as.factor(tracking_bolstad$Transmitter)) #comment - all transmitters are only temp sensors here
tracking_bolstad$temperature <- tracking_bolstad$Data/10
tracking_bolstad$receiverID <- tracking_bolstad$Receiver
str(tracking_bolstad)
tracking_bolstad <- tracking_bolstad[c("transmitterID", "receiverID", "datetime", "temperature", "gill_sample", "fjord", "tagging_date")]
tracking_bolstad$fishID <- tracking_bolstad$transmitterID
tracking_bolstad$datetime <- as.POSIXct(tracking_bolstad$datetime)
tracking_bolstad$day <- as.numeric(tracking_bolstad$datetime-tracking_bolstad$tagging_date)
summary(tracking_bolstad$day) #Note - we have only included 15 days here - a bit too little?



######################################################
#Aurland
######################################################
tracking_aurland <- readRDS("./data/raw_data/tracking_data_aurland.RDS")
str(tracking_aurland)
summary(as.factor(tracking_aurland$sensor))
tracking_aurland <- tracking_aurland[tracking_aurland$sensor=='temp', ]
tracking_aurland <- tracking_aurland[!is.na(tracking_aurland$sensor), ]

#only temp data in this dataset - all good. Also latlong
summary(as.factor(tracking_aurland$oid)) #comment - all transmitters are only temp sensors here
tracking_aurland$temperature <- tracking_aurland$Data/10
tracking_aurland$receiverID <- tracking_aurland$Receiver
str(tracking_aurland)
tracking_aurland <- tracking_aurland[c("transmitterID", "receiverID", "datetime", "temperature", "gill_sample", "fjord", "tagging_date")]
tracking_aurland$fishID <- tracking_aurland$transmitterID
tracking_aurland$datetime <- as.POSIXct(tracking_aurland$datetime)
tracking_aurland$day <- as.numeric(tracking_aurland$datetime-tracking_aurland$tagging_date)
summary(tracking_aurland$day)



######################################################
#Beiarfjorden
######################################################
tracking_beiarn <- readRDS("./data/modified_data/fever_beiarfjorden.RDS")  
str(tracking_beiarn)
fishdata_beiarn <- readRDS("./data/modified_data/fishdata.RDS")
tracking_beiarn <- merge(tracking_beiarn, fishdata_beiarn[c("transmitterID", "gill_ID")], by = "transmitterID")
str(tracking_beiarn)
tracking_beiarn$gill_sample <- tracking_beiarn$gill_ID
tracking_beiarn <- tracking_beiarn[c("fishID","transmitterID", "receiverID", "datetime", "temperature", "gill_sample", "fjord", "tagging_date")]
tracking_beiarn$day <- as.numeric(tracking_beiarn$datetime-tracking_beiarn$tagging_date)
head(tracking_beiarn$day)
summary(tracking_beiarn$day)


######################################################
#Stjordal
######################################################
str(tracking_stjordal)
tracking_stjordal <- readRDS("./data/modified_data/fever_stjordal.RDS")
fishdata_stjordal <- readRDS("./data/modified_data/fishdata_stjordal.RDS")
str(fishdata_stjordal)
export <- fishdata_stjordal
export <- export[export$gill_sample!='',]
write.csv(export, "./data/modified_data/samples_stjordal_140623.csv")
tracking_stjordal <- merge(tracking_stjordal, fishdata_stjordal[c("transmitterID", "gill_sample")], by = "transmitterID")
str(tracking_stjordal)
tracking_stjordal <- tracking_stjordal[c("fishID","transmitterID", "receiverID", "datetime", "temperature", "gill_sample", "fjord", "tagging_date")]
tracking_stjordal$day <- as.numeric(tracking_stjordal$datetime-tracking_stjordal$tagging_date)
head(tracking_stjordal$day)
summary(tracking_stjordal$day)

tracking_data <- rbind(tracking_bolstad, tracking_beiarn, tracking_stjordal)

tmp <- tracking_data



library(dplyr)
str(tracking_data)
tracking_data$temperature <- as.numeric(tracking_data$temperature)
tracking_data$fjord <- as.factor(tracking_data$fjord)
tracking_data$fishID <- as.factor(tracking_data$fishID)
tracking_data$receiverID <- as.factor(tracking_data$receiver)
tracking_data$transmitterID <- as.factor(tracking_data$transmitterID)
tracking_data <- tracking_data[!is.na(tracking_data$temperature), ]
#tracking_data$day <- floor(tracking_data$day)

str(tracking_beiarn)
str(tracking_data)

tracking_data <- tracking_data[tracking_data$fishID!=128,]
tracking_data <- tracking_data[tracking_data$fishID!=145,]