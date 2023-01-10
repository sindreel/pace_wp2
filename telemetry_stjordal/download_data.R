###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################

library(stringr)

#########################################################################
#download Stjordal telemetry-data
#########################################################################

#Last updated telemetry data file - 17.10.2022

#-----------------------------------------------------------------------
# Download data from external source. This is best practice for making 
# your code transportable among different computers.. 
#-----------------------------------------------------------------------

# This script downloads and merges the data needed for further analyses for the Greenland tracking project.


# first create new folder (./data/raw_data) to store the files 
# locally (cache down the data) - remember to update gitignore folder (add 
# the line */data/raw_data to the .gitignore file)
dir.create("data/raw_data", showWarnings = FALSE, recursive = TRUE)
dir.create("data/modified_data", showWarnings = FALSE, recursive = TRUE)

#change timeout option to fix problem with large files
options(timeout = max(999, getOption("timeout")))


URL_tracking_data_vemco <- "https://ntnu.box.com/shared/static/2u3r2gocrdeaotnajnn96n964yk36jw7.rds" #Uploaded new version 29.11.22
URL_tracking_data_thelma <- "https://ntnu.box.com/shared/static/v7aj60674wyvdfe2b7d04lrh2mcovjfa.rds" #Uploaded new version 29.11.22
URL_tagging_data <- "https://ntnu.box.com/shared/static/cz3mpe0uulccb9pl1gaa8w692pevq7x4.csv" #Updated 29.09.2021
URL_receiver_deployment <- "https://ntnu.box.com/shared/static/4g8rjxknweej65ymdoz0znynuy95fj4e.csv" #Updated 29.09.2021


#download data
download.file(url=URL_tracking_data_vemco,destfile="./data/raw_data/tracking_data_vemco_stjordal.rds")  
download.file(url=URL_tracking_data_thelma,destfile="./data/raw_data/tracking_data_thelma_stjordal.rds")  

download.file(url=URL_tagging_data,destfile="./data/raw_data/tagging_data_stjordal.csv")
download.file(url=URL_receiver_deployment,destfile="./data/raw_data/receiver_deployment_stjordal.csv")

#combine tracking_data
tracking_data_vemco <- readRDS("./data/raw_data/tracking_data_vemco_stjordal.rds")
str(tracking_data_vemco)


#Remove all lines with wrong codesets 
tmp1 <- dplyr::filter(tracking_data_vemco, grepl("A69-9007", tracking_data_vemco$transmitterID))
tmp2 <- dplyr::filter(tracking_data_vemco, grepl("A69-9006", tracking_data_vemco$transmitterID))
tmp3 <- dplyr::filter(tracking_data_vemco, grepl("A69-1303", tracking_data_vemco$transmitterID))

tracking_data_vemco <- rbind(tmp1, tmp2,tmp3)

rm(tmp1)
rm(tmp2)
rm(tmp3)
tracking_data_vemco$transmitterID <- str_replace(tracking_data_vemco$transmitterID, "A69-9007-", "")
tracking_data_vemco$transmitterID <- str_replace(tracking_data_vemco$transmitterID, "A69-9006-", "")
tracking_data_vemco$transmitterID <- str_replace(tracking_data_vemco$transmitterID, "A69-1303-", "")

summary(as.factor(tracking_data_vemco$receiverID), maxsum=999)
tracking_data_vemco$receiverID <- str_replace(tracking_data_vemco$receiverID, "VR2W-", "")
tracking_data_vemco$receiverID <- str_replace(tracking_data_vemco$receiverID, "VR2AR-", "")

str(tracking_data_vemco)
tracking_data_vemco$receiverID <- as.integer(tracking_data_vemco$receiverID)
tracking_data_vemco$transmitterID <- as.integer(tracking_data_vemco$transmitterID)
tracking_data_vemco$datetime <- as.POSIXct(tracking_data_vemco$datetime, format="%Y-%m-%d %H:%M:%S")
#check for duplicated rows
tracking_data_vemco <- tracking_data_vemco[!duplicated(tracking_data_vemco), ]
str(tracking_data_vemco)


tracking_data_thelma <- readRDS("./data/raw_data/tracking_data_thelma_stjordal.rds")
str(tracking_data_thelma)
names(tracking_data_thelma) <- c("datetime", "unix", "transmitterID", "sensor_value", "protocol", "SNR", "receiverID")
tracking_data_thelma <- tracking_data_thelma[c("datetime", "receiverID", "transmitterID", "sensor_value")]
tracking_data_thelma <- tracking_data_thelma[!duplicated(tracking_data_thelma), ]

tracking_data <- rbind(tracking_data_vemco, tracking_data_thelma)
rm(tracking_data_vemco)
rm(tracking_data_thelma)

fishdata <- read.csv("./data/raw_data/tagging_data_stjordal.csv", sep = ";", header=T)
names(fishdata)
str(fishdata)
duplicated(fishdata$Transmitter.ID)
fishdata <- fishdata[c("Fish.ID", "Transmitter.ID", "Spp","TL..mm.","Weight..g.", "Colour", "capture.site", "Sample.vial.ID..Gjelleprove.", "sex..DNA.", "Capture.method", "DEPTHsensor", "ACCELRATIONsensor", "TEMPsensor", "dmy", "tag.family")]
fishdata <- fishdata[!duplicated(fishdata$Transmitter.ID), ]
names(fishdata)<-c("fishID","transmitterID","species", "tot_length","mass_g", "color", "capture_site", "gill_sample", "sex_dna", "capture_method", "depthsensor", "acceleration_sensor", "temp_sensor", "tagging_date", "tag_type")
str(fishdata)
summary(as.factor(fishdata$transmitterID), maxsum = 999)
tracking_data <- merge(tracking_data, fishdata, by = "transmitterID", all.x = TRUE)

library(dplyr)
test <- tracking_data %>%
  group_by(fishID, transmitterID)%>%
  summarize(count = n())

###########################################################
#format tracking data
str(tracking_data)
tracking_data$transmitterID <- as.factor(tracking_data$transmitterID)
tracking_data$receiverID <- as.factor(tracking_data$receiverID)
tracking_data$sensor_value <- as.numeric(tracking_data$sensor_value)
tracking_data$fishID <- as.factor(tracking_data$fishID)
tracking_data$species <- as.factor(tracking_data$species)
tracking_data$sex_dna <- as.factor(tracking_data$sex_dna)
tracking_data$capture_method <- as.factor(tracking_data$capture_method)
tracking_data$depthsensor <- as.factor(tracking_data$depthsensor)
tracking_data$acceleration_sensor <- as.factor(tracking_data$acceleration_sensor)
tracking_data$temp_sensor <- as.factor(tracking_data$temp_sensor)

str(tracking_data)


###########################################################
#RENAME receivers THAT HAVE BEEN RESUSED IN DIFFERENT LOCATION BY ADDING "_2" TO REICEIVERID
###########################################################
tracking_data$receiverID <- as.character(tracking_data$receiverID)
tracking_data$receiverID[tracking_data$receiverID=='142' & tracking_data$datetime>'2021-06-11 14:00:00'] <- '142_2' #trenger ikke denne nå, siden den nye lokaliteten ikke er lastet ned enda.
tracking_data$receiverID <- as.factor(tracking_data$receiverID)



###########################################################
#Add coulum - this will be the Uniqe record ID that will be used to exclude false detections from the filtered dataset
tracking_data$row_ref <- seq.int(nrow(tracking_data))
tracking_data$rowID <- seq.int(nrow(tracking_data))
###########################################################


###########################################################
#ADD STATIONID
stationID <- read.csv("./data/raw_data/receiver_deployment_stjordal.csv", sep = ";")
str(stationID)
stationID <- stationID[c(2,4,5,6,11,20)]
str(stationID)

stationID$stationID <- stationID$STATION_NO
stationID$stationIDTo <- stationID$stationID
stationID$stationIDFrom <- stationID$stationID
stationID$receiverID <- stationID$INS_SERIAL_NO
str(stationID)
stationID$DEPLOY_DATE <- stationID$DEPLOY_DATE_TIME....yyyy.mm.ddThh.mm.ss.
str(stationID)
stationID$DEPLOY_DATE <- str_replace(stationID$DEPLOY_DATE, "T", " ")
stationID$DEPLOY_DATE <- str_replace(stationID$DEPLOY_DATE, "Z", "")
stationID$DEPLOY_DATE <- as.POSIXct(stationID$DEPLOY_DATE, format="%Y.%m.%d %H:%M:%S")
stationID$receiverID[stationID$receiverID=='142' & stationID$stationID=='34'] <- '142_2'
stationID$deploy_date <- stationID$DEPLOY_DATE
stationID$recover_date <- stationID$RECOVER_DATE_TIME..yyyy.mm.ddThh.mm.ss.

tracking_data <- merge(x = tracking_data, y = stationID[c("stationID","receiverID")], by = "receiverID")
tracking_data <- tracking_data[!duplicated(tracking_data$row_ref), ]

tmp <- test[!(is.na(test$fishID)) | test$count<1000, ]
potential_fish <- test[(is.na(test$fishID)) & test$count>999, ]
potential_fish <- merge(tracking_data, potential_fish[c("transmitterID")], by = "transmitterID")
saveRDS(potential_fish, "./data/modified_data/potential_fish_stjordal.RDS")
tracking_data <- merge(tracking_data, tmp[c("transmitterID")], by = "transmitterID")

tracking_data$fishID <- as.character(tracking_data$fishID)
tracking_data$fishID[is.na(tracking_data$fishID)] <- paste("false_ID-", tracking_data$transmitterID[is.na(tracking_data$fishID)], sep = '')
tracking_data$fishID <- as.factor(tracking_data$fishID)
summary(as.factor(tracking_data$fishID), maxsum = 999)


tracking_data <- tracking_data[tracking_data$datetime>'2020-01-01 00:00:00', ]

###########################################################
#ADD REVEIVERIDTO AND REVEIVERIDFROM
summary(as.factor(tracking_data$receiverID))
summary(as.factor(tracking_data$stationID))

tracking_data <- tracking_data[order(tracking_data$fishID, tracking_data$datetime), ]
tracking_data$rowID <- seq.int(nrow(tracking_data))
tracking_data$rowIDTo <- tracking_data$rowID+1
tracking_data$rowIDFrom <- tracking_data$rowID-1
tracking_data_tmp <- tracking_data
tracking_data_tmp$fishIDFrom <- tracking_data_tmp$fishID
tracking_data_tmp$receiverIDFrom <- tracking_data_tmp$receiverID
tracking_data_tmp$fishIDTo <- tracking_data_tmp$fishID
tracking_data_tmp$receiverIDTo <- tracking_data_tmp$receiverID
str(tracking_data_tmp)
tracking_data <- merge(x = tracking_data, y = tracking_data_tmp[c("rowID","receiverIDFrom","fishIDFrom")], by.x = "rowIDFrom", by.y = "rowID", all.x = TRUE, allow.cartesian=TRUE)
str(tracking_data)

tracking_data <- merge(x = tracking_data, y = tracking_data_tmp[c("rowID","receiverIDTo","fishIDTo")], by.x = "rowIDTo", by.y = "rowID", all.x = TRUE, allow.cartesian=TRUE)
str(tracking_data)

tracking_data$receiverIDFrom[tracking_data$fishID != tracking_data$fishIDFrom] <- NA
tracking_data$receiverIDTo[tracking_data$fishID != tracking_data$fishIDTo] <- NA
rm(tracking_data_tmp)
###########################################################


###########################################################
#ADD STATIONIDTo and stationIDFrom
tracking_data <- merge(x = tracking_data, y = stationID[c("stationIDTo", "receiverID")], by.x = "receiverIDTo", by.y = "receiverID", all.x = TRUE, all.y=FALSE,  allow.cartesian=TRUE)
tracking_data <- tracking_data[!duplicated(tracking_data$row_ref), ]
tracking_data <- merge(x = tracking_data, y = stationID[c("stationIDFrom","receiverID")], by.x = "receiverIDFrom", by.y = "receiverID", all.x = TRUE,  allow.cartesian=TRUE)
tracking_data <- tracking_data[!duplicated(tracking_data$row_ref), ]


###########################################################
#CALCULATING TIMETO AND TIMEFROM
tracking_data$category <- interaction(tracking_data$transmitterID, tracking_data$receiverID)
tracking_data <- tracking_data[order(tracking_data$category, tracking_data$datetime), ]
tracking_data$timeFrom <- unlist(tapply(tracking_data$datetime, INDEX = tracking_data$category,
                                        FUN = function(x) c(0, `units<-`(diff(x), "mins"))))
tracking_data <- tracking_data[order (-tracking_data$rowID),]
tracking_data <- tracking_data[order(tracking_data$category, -xtfrm(tracking_data$datetime)), ]
tracking_data$timeTo <- unlist(tapply(tracking_data$datetime, INDEX = tracking_data$category,
                                      FUN = function(x) c(0, `units<-`(diff(x), "mins"))))
tracking_data$timeTo <- tracking_data$timeTo*-1
head(tracking_data)

tracking_data_unfiltered <- tracking_data



#check number of detections per fish

tmp <- tracking_data %>%
  group_by(transmitterID) %>%
  dplyr::summarize(n_dets = n())


summary(tracking_data$fishID, maxsum = 999)

false_fishID <- tracking_data[grepl("false_ID", tracking_data$fishID), ]
#tmp <- false_fishID[grepl("A69-1601-", false_fishID$transmitterID), ]
#false_fishID <- anti_join(false_fishID, tmp, by="row_ref")


#How to evaluate what to filter
#Just filter stations in hours where there are false fishID's on repective receivers.
#Make summary table where we count number of false fishID on each receiver 
library(lubridate)

str(tracking_data)
#tracking_data <- tracking_data[grepl("A69-9007-", tracking_data$transmitterID), ]
str(false_fishID)

false_fishID_1 <- false_fishID %>%
  mutate(dth=round_date(datetime, "1 day")) %>% #
  group_by(receiverID, dth) %>%
  dplyr::summarize(n_false_fishid = n()) %>%
  mutate(interaction=interaction(receiverID, dth)) %>%
  group_by(interaction) %>%
  dplyr::select(interaction, n_false_fishid)


str(false_fishID_1)

detection_numbers <- tracking_data %>%
  mutate(dth=round_date(datetime, "1 day")) %>% #
  group_by(receiverID, dth) %>%
  dplyr::summarize(daily_detections = n()) %>%
  mutate(interaction=interaction(receiverID, dth)) %>%
  group_by(interaction) %>%
  dplyr::select(interaction, daily_detections)

str(tracking_data)
summary(tracking_data$fishID)

rm(tmp)
tmp <- tracking_data %>%
  filter(fishID!='false_ID') %>%
  mutate(dth=round_date(datetime, "1 day")) %>%
  group_by(receiverID, dth) %>%
  filter(!duplicated(fishID)) %>%
  group_by(receiverID, dth) %>%
  dplyr::summarize(n_fish = n()) %>%
  mutate(interaction=interaction(receiverID, dth))

tmp <- full_join(tmp, false_fishID_1, by="interaction")
tmp <- full_join(tmp, detection_numbers, by="interaction")

str(tmp)
tmp$n_fish[is.na(tmp$n_fish)] <- 0
tmp$n_false_fishid[is.na(tmp$n_false_fishid)] <- 0

library(ggplot2)

test <- ggplot(tmp, aes(x=n_fish, y=n_false_fishid)) + ggtitle("") + theme_classic(base_size = 18) + geom_point() + geom_smooth() + xlab("fish_detected_per_day") + ylab("n_false_fish_ID's")#+  scale_x_datetime(date_breaks = "2 months")
test
ggsave("./data/modified_data/falsedets_nfish.tiff", test, units="cm", width=35, height=30, dpi=600, compression = 'lzw')


str(tmp)
test <- ggplot(tmp[tmp$n_false_fishid>0,], aes(x=daily_detections, y=n_false_fishid, col=receiverID)) + ggtitle("") + theme_classic(base_size = 18) + geom_point() +  xlab("daily_detections") + ylab("n_false_fishID")#+  scale_x_datetime(date_breaks = "2 months")
test
ggsave("./data/modified_data/falsedets_ndets.tiff", test, units="cm", width=35, height=30, dpi=600, compression = 'lzw')


str(false_fishID_1)
#false_fishID <- 2544087


library(ggplot2)
str(tmp)
str(false_fishID_1)
test <- ggplot(tmp, aes(x=dth, y=receiverID, col=n_false_fishid)) + ggtitle("False fishID's") + theme_classic(base_size = 10) + geom_point() + xlab("Date") + ylab("ReceiverID")#+  scale_x_datetime(date_breaks = "2 months")
test
#hist(false_fishID_1$n_detections)
#false_fishID_1 <- false_fishID_1[which (false_fishID_1$n_detections>30), ]

#test <- ggplot(false_fishID_1, aes(x=dth, y=n_detections, col=receiverID)) + ggtitle("False fishID's") + theme_classic(base_size = 10) + geom_point() + xlab("Date") + ylab("ReceiverID")#+  scale_x_datetime(date_breaks = "2 months")
#test

tracking_data <- tracking_data %>%
  mutate(dth=round_date(datetime, "1 day")) #

str(tracking_data)
tracking_data$interaction <- interaction(tracking_data$receiverID, tracking_data$dth)

#str(false_fishID_1)
#false_fishID_1$n_falseid <- false_fishID_1$n_detections
#false_fishID_1$interaction <- interaction(false_fishID_1$receiverID, false_fishID_1$dth)

#Decide to filter days of each station where they have either more than 1000 detections in a day, or 3 or more false fish'IDs (surely false detections)
filter <- tmp[tmp$n_false_fishid>2|tmp$daily_detections>1000, ]
str(filter)
#plot filter choice
test <- ggplot(filter, aes(x=dth, y=receiverID, col=n_false_fishid)) + ggtitle("Applied filter to data") + theme_classic(base_size = 18) + geom_point() +  xlab("daily_detections") + ylab("n_false_fishID")#+  scale_x_datetime(date_breaks = "2 months")
test
ggsave("./data/modified_data/filter_applied_to_receivers_dates.tiff", test, units="cm", width=35, height=30, dpi=600, compression = 'lzw')

filter$filter <- '1'

str(tracking_data)
str(filter)
tracking_data <- merge(tracking_data, filter[c("interaction", "filter")], by="interaction", all.x = TRUE)
summary(as.factor(tracking_data$filter))


#######################################################################################
#ADD 24h-FILTER ON STATION selected for filtering
#tracking_data <- tracking_data_unfiltered
str(tracking_data)
stations_hour_filter <- tracking_data[which(tracking_data$filter==1), ]

stations_hour_filter_60_60 <- stations_hour_filter[which (stations_hour_filter$timeFrom>60 & stations_hour_filter$timeTo>60), ]
stations_hour_filter_60_0 <- stations_hour_filter[which (stations_hour_filter$timeFrom>60 & stations_hour_filter$timeTo==0), ]
stations_hour_filter_0_60 <- stations_hour_filter[which (stations_hour_filter$timeFrom==0 & stations_hour_filter$timeTo>60), ]
stations_hour_filter_0_0 <- stations_hour_filter[which (stations_hour_filter$timeFrom==0 & stations_hour_filter$timeTo==0), ]
stations_hour_filter <- rbind(stations_hour_filter_60_60,stations_hour_filter_60_0,stations_hour_filter_0_60,stations_hour_filter_0_0)
rm(stations_hour_filter_60_60)
rm(stations_hour_filter_60_0)
rm(stations_hour_filter_0_60)
rm(stations_hour_filter_0_0)
false_regs<- stations_hour_filter
summary(false_regs$fishID)
rm(stations_hour_filter)
false_regs$false <- 1
str(false_regs)

tracking_data <- merge(tracking_data, false_regs[c("row_ref", "false")], by = "row_ref", all.x = TRUE)
str(tracking_data)


#######################################################################################
#REMOVE RECEIVERIDFROM AND -TO, STATIONID, TIMETO AND TIMEFROM
str(tracking_data)
tracking_data <- tracking_data[c("transmitterID", "stationID", "receiverID", "datetime", "sensor_value", "row_ref", "fishID", "filter", "false")]
#######################################################################################
#Transform sensor-values
tracking_data_false_detections <- tracking_data[which (tracking_data$false==1),]
tracking_data <- tracking_data[is.na(tracking_data$false), ]
str(tracking_data)


###########################################################
#ADD REVEIVERIDTO AND REVEIVERIDFROM, STATIONIDTO AND FROM, AND TIMETO AND TIMEFROM.
tracking_data <- tracking_data[order(tracking_data$transmitterID, tracking_data$datetime), ]
tracking_data$rowID <- seq.int(nrow(tracking_data))
tracking_data$rowIDTo <- tracking_data$rowID+1
tracking_data$rowIDFrom <- tracking_data$rowID-1
tracking_data_tmp <- tracking_data
tracking_data_tmp$transmitterIDFrom <- tracking_data_tmp$transmitterID
tracking_data_tmp$receiverIDFrom <- tracking_data_tmp$receiverID
tracking_data_tmp$transmitterIDTo <- tracking_data_tmp$transmitterID
tracking_data_tmp$receiverIDTo <- tracking_data_tmp$receiverID
str(tracking_data_tmp)
tracking_data <- merge(x = tracking_data, y = tracking_data_tmp[c("rowID","receiverIDFrom","transmitterIDFrom")], by.x = "rowIDFrom", by.y = "rowID", all.x = TRUE)
tracking_data <- tracking_data[!duplicated(tracking_data$row_ref), ]
tracking_data <- merge(x = tracking_data, y = tracking_data_tmp[c("rowID","receiverIDTo","transmitterIDTo")], by.x = "rowIDTo", by.y = "rowID", all.x = TRUE)
tracking_data <- tracking_data[!duplicated(tracking_data$row_ref), ]
rm(tracking_data_tmp)
tracking_data$receiverIDFrom[tracking_data$transmitterID != tracking_data$transmitterIDFrom] <- NA
tracking_data$receiverIDTo[tracking_data$transmitterID != tracking_data$transmitterIDTo] <- NA
str(tracking_data)

str(stationID)
tracking_data <- merge(x = tracking_data, y = stationID[c("stationIDTo", "receiverID")], by.x = "receiverIDTo", by.y = "receiverID", all.x = TRUE, all.y=FALSE)
tracking_data <- tracking_data[!duplicated(tracking_data$row_ref), ]
tracking_data <- merge(x = tracking_data, y = stationID[c("stationIDFrom","receiverID")], by.x = "receiverIDFrom", by.y = "receiverID", all.x = TRUE)
tracking_data <- tracking_data[!duplicated(tracking_data$row_ref), ]

###########################################################
#CALCULATING TIMETO AND TIMEFROM
#tracking_data$category <- interaction(tracking_data$transmitterID, tracking_data$receiverID)
tracking_data <- tracking_data[order(tracking_data$fishID, tracking_data$datetime), ]
tracking_data$timeFrom <- unlist(tapply(tracking_data$datetime, INDEX = tracking_data$fishID,
                                        FUN = function(x) c(0, `units<-`(diff(x), "mins"))))
tracking_data$rowID <- seq.int(nrow(tracking_data))
tracking_data <- tracking_data[order (-tracking_data$rowID),]
tracking_data <- tracking_data[order(tracking_data$fishID, -xtfrm(tracking_data$datetime)), ]
tracking_data$timeTo <- unlist(tapply(tracking_data$datetime, INDEX = tracking_data$fishID,
                                      FUN = function(x) c(0, `units<-`(diff(x), "mins"))))
tracking_data$timeTo <- tracking_data$timeTo*-1
head(tracking_data)                                         
str(tracking_data)
tracking_data$filter <- as.factor(tracking_data$filter)



#######################################################################################

#Transform sensor-values

head(tracking_data)
str(fishdata)

tracking_data <- merge(tracking_data, fishdata[c("transmitterID", "tag_type", "depthsensor", "acceleration_sensor", "temp_sensor")], by = "transmitterID")

summary(as.factor(fishdata$tag_type[fishdata$temp_sensor=='x']))

tracking_data$swimming_depth <- ''
summary(as.factor(tracking_data$depthsensor))
tracking_data$swimming_depth[tracking_data$depthsensor=='x'] <- tracking_data$sensor_value[tracking_data$depthsensor=='x']*0.2
summary(as.numeric(tracking_data$swimming_depth))
tracking_data$swimming_depth <- as.numeric(tracking_data$swimming_depth)
summary(as.factor(tracking_data$temp_sensor))
tracking_data$temperature <- ''
tracking_data$temperature[tracking_data$temp_sensor=='x'] <- tracking_data$sensor_value[tracking_data$temp_sensor=='x']*0.1
summary(as.numeric(tracking_data$temperature))

str(tracking_data)
3.465/255
3.456/255
summary(as.factor(tracking_data$acceleration_sensor))
tracking_data$swimming_accelleration[tracking_data$acceleration_sensor=='x'] <- tracking_data$sensor_value[tracking_data$acceleration_sensor=='x']*0.01355294
summary(as.numeric(tracking_data$swimming_accelleration))

#transform sensor data
tracking_data$swimming_accelleration[tracking_data$tag_type=='V13A-1x'] <- tracking_data$sensor_value[tracking_data$tag_type=='V13A-1x']*0.01922
summary(tracking_data$swimming_accelleration)

#plot(tracking_data$swimming_accelleration~tracking_data$datetime)

str(tracking_data)
#tracking_data <- tracking_data[!duplicated(tracking_data), ]





#write.csv(tracking_data, "./data/modified_data/greenland_filtered.csv", row.names = FALSE)
saveRDS(tracking_data, "./data/modified_data/filtered_tracking_data_stjordal.rds")
saveRDS(tracking_data_false_detections, "./data/modified_data/tracking_data_false_detections_stjordal.rds")
saveRDS(stationID, "./data/modified_data/stationID_stjordal.rds")
saveRDS(fishdata, "./data/modified_data/fishdata_stjordal.rds")


