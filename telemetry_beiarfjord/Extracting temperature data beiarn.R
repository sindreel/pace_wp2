-###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################


library(dplyr)

tracking_data <- readRDS( "./data/modified_data/beiarn_dets_filtered.rds")

str(tracking_data)
stationID <- readRDS("./data/modified_data/stationID.rds")
fishdata <- readRDS("./data/modified_data/fishdata.rds")
fishdata$tagging_date <- as.POSIXct(fishdata$tagging_date)
str(fishdata)
fishdata <- fishdata[fishdata$tagging_date>'2020-08-01 00:00:00',]
#fishdata$end_time <- fishdata$tagging_date + 20160*60+86400
tracking_data  <- merge(tracking_data, fishdata[c("fishID", "tagging_date")], by="fishID")
tracking_data$fjord <- "Beiarfjorden"
stationID <- stationID[!duplicated(stationID$stationID), ]
str(stationID)
str(tracking_data)
tracking_data <- merge(tracking_data, stationID[c("stationID", "DEPLOY_LAT", "DEPLOY_LONG")],  by="stationID")
summary(tracking_data$temperature)
saveRDS(tracking_data, "./data/modified_data/fever_beiarfjorden.RDS")
