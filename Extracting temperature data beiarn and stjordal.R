-###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################


library(dplyr)

tracking_data <- readRDS( "./data/modified_data/filtered_tracking_data.rds")

str(tracking_data)
stationID <- readRDS("./data/modified_data/stationID.rds")
fishdata <- readRDS("./data/modified_data/fishdata.rds")
fishdata$tagging_date <- as.POSIXct(fishdata$tagging_date)
str(fishdata)
fishdata <- fishdata[fishdata$tagging_date>'2020-08-01 00:00:00',]
fishdata$end_time <- fishdata$tagging_date + 20160*60+86400
tracking_data  <- merge(tracking_data, fishdata[c("fishID", "tagging_date", "end_time")], by="fishID")
tracking_data <- tracking_data[tracking_data$datetime>tracking_data$tagging_date+86400 & tracking_data$datetime<tracking_data$end_time,]
tracking_data$fjord <- "Beiarfjorden"

saveRDS(tracking_data, "./data/modified_data/fever_beiarfjorden.RDS")
