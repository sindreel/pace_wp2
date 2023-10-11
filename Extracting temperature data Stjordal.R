-###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################


library(dplyr)

tracking_data <- readRDS( "./data/modified_data/filtered_tracking_data_stjordal.rds")

str(tracking_data)
stationID <- readRDS("./data/modified_data/stationID_stjordal.rds")
fishdata <- readRDS("./data/modified_data/fishdata_stjordal.rds")
fishdata$tagging_date <- as.POSIXct(fishdata$tagging_date)
str(fishdata)
summary(as.factor(fishdata$species))
fishdata <- fishdata[fishdata$species == 'Salmo trutta',]
fishdata <- fishdata[fishdata$tot_length>270, ]
fishdata <- fishdata[fishdata$temp_sensor=='x',]
fishdata$tagging_date
fishdata <- fishdata[fishdata$tag_type!='V13A-1x',]
fishdata$tagging_date <- as.POSIXct(fishdata$tagging_date, format= "%d.%m.%Y")
fishdata$end_time <- fishdata$tagging_date + 20160*60+86400
tracking_data  <- merge(tracking_data, fishdata[c("fishID", "tagging_date", "end_time")], by="fishID")
tracking_data <- tracking_data[tracking_data$datetime>tracking_data$tagging_date+86400 & tracking_data$datetime<tracking_data$end_time,]
tracking_data$fjord <- "Stjordal"
str(tracking_data)
saveRDS(tracking_data, "./data/modified_data/fever_stjordal.RDS")
