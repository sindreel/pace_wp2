###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################


library(dplyr)

tracking_data <- read.csv( "./data/modified_data/bolstad/filtered_detections.csv")
str(tracking_data)
stationID <- read.csv("./data/modified_data/bolstad/receiver_locations.csv")
fishdata <- read.csv("./data/modified_data/bolstad/fishdata_kristie.csv")
str(fishdata)
summary(as.factor(fishdata$Spp))
fishdata <- fishdata[fishdata$Spp == "s_trout",]
fishdata$tagging_date <- as.POSIXct(fishdata$Date, format= "%d.%m.%Y")
#fishdata$end_time <- fishdata$tagging_date + 20160*60+86400
str(fishdata)
str(tracking_data)
tracking_data$transmitterID <- tracking_data$ID
tracking_data  <- merge(tracking_data, fishdata[c("transmitterID", "gill_sample", "tagging_date")], by="transmitterID")
str(tracking_data)
tracking_data$datetime <- tracking_data$dth
#tracking_data <- tracking_data[tracking_data$datetime>tracking_data$tagging_date+86400 & tracking_data$datetime<tracking_data$end_time,]
tracking_data$fjord <- "Bolstad"
str(tracking_data)
saveRDS(tracking_data, "./data/modified_data/fever_bolstad.RDS")
