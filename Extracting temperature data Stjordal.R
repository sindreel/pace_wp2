-###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################


library(dplyr)
library(lubridate)

tracking_data <- readRDS( "./data/modified_data/filtered_tracking_data_stjordal.rds")

str(tracking_data)
stationID <- readRDS("./data/modified_data/stationID_stjordal.rds")
fishdata <- readRDS("./data/modified_data/fishdata_stjordal.rds")
str(fishdata)
summary(as.factor(fishdata$temp_sensor))
fishdata <- fishdata[fishdata$temp_sensor=='x', ]
fishdata$tagging_date
fishdata$tagging_date <- dmy(fishdata$tagging_date)

fishdata$tagging_date <- as.POSIXct(fishdata$tagging_date)
str(fishdata)
summary(as.factor(fishdata$species))
fishdata <- fishdata[fishdata$species == 'Salmo trutta',]
fishdata <- fishdata[fishdata$tot_length>270, ]
fishdata <- fishdata[fishdata$temp_sensor=='x',]
fishdata$tagging_date
fishdata <- fishdata[fishdata$tag_type!='V13A-1x',]
tracking_data  <- merge(tracking_data, fishdata[c("fishID", "tagging_date")], by="fishID")
tracking_data$fjord <- "Stjordal"
str(tracking_data)
str(stationID)
stationID <- stationID[!duplicated(stationID$stationID), ]
tracking_data <- merge(tracking_data, stationID[c("stationID", "DEPLOY_LAT", "DEPLOY_LONG")], by = "stationID")
saveRDS(tracking_data, "./data/modified_data/fever_stjordal.RDS")
