###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################


library(dplyr)

#Read telemetry data
tracking_data <- readRDS("./data/raw_data/tracking_data.rds")
deployment_data <- readRDS("./data/raw_data/receiver_deployment.rds")
deployment_data$TRANSMITTER[deployment_data$INS_SERIAL_NO=='547128'] <- 'A69-1601-61592'
fishdata <- readRDS("./data/raw_data/tagging_data.rds")

###########################################################
#Add row-ref
tracking_data$row_ref <- seq.int(nrow(tracking_data))
str(tracking_data)

###########################################################
#Remove pinger tags
# tmp <- tracking_data[grepl("A69-1601-", tracking_data$transmitterID), ]
# tracking_data <- anti_join(tracking_data, tmp, by="row_ref")
# rm(tmp)

fishdata$transmitterID <- as.factor(fishdata$transmitterID)

#add pinger tags
str(deployment_data)
pinger_tags <- deployment_data[c("INS_SERIAL_NO", "TRANSMITTER")]
names(pinger_tags) <- c("receiverID", "transmitterID")
#pinger_tags$transmitterID <- paste("A69-1601-", pinger_tags$transmitterID, sep = '')
pinger_tags$fishID <- paste("VR2AR-", pinger_tags$receiverID, sep = '')
pinger_tags <- pinger_tags[pinger_tags$fishID!='VR2AR-NA', ]
pinger_tags <- pinger_tags[!duplicated(pinger_tags$receiverID), ]
pinger_tags <- pinger_tags[pinger_tags$transmitterID!="", ]


tmp <- rbind(fishdata[c("transmitterID", "fishID")], pinger_tags[c("transmitterID", "fishID")])
str(tracking_data)
tracking_data <- merge(tracking_data, tmp[c("transmitterID", "fishID")], by = "transmitterID", all.x = TRUE)
tracking_data$fishID <- as.character(tracking_data$fishID)
summary(as.factor(tracking_data$fishID), maxsum = 999)


tracking_data$fishID[is.na(tracking_data$fishID)] <- paste("false_ID-", tracking_data$transmitterID[is.na(tracking_data$fishID)], sep = '')
tracking_data$fishID <- as.factor(tracking_data$fishID)
summary(as.factor(tracking_data$fishID), maxsum = 999)

summary(as.factor(tracking_data$receiverID[tracking_data$fishID=='false_ID-A69-1601-60202'] ))
summary(as.factor(tracking_data$receiverID[tracking_data$fishID=='false_ID-A69-1601-61592'] ))
tracking_data <- tracking_data[tracking_data$fishID!='false_ID-A69-1601-61592', ]
tmp <- tracking_data[tracking_data$fishID=='false_ID-A69-9007-3',]
tracking_data <- tracking_data[tracking_data$datetime<'2022-07-01 00:00:00',]
tracking_data <- tracking_data[tracking_data$fishID!='false_ID-A69-9007-3',]
tracking_data <- tracking_data[tracking_data$datetime<'2022-01-01 00:00:00' | tracking_data$receiverID=='WR2AR-547125',]


###########################################################
#ADD STATIONID
str(deployment_data)

stationID <- deployment_data
stationID$stationID <- stationID$STATION_NO
stationID$stationIDTo <- stationID$stationID
stationID$stationIDFrom <- stationID$stationID
stationID$receiverID <- stationID$INS_SERIAL_NO
str(stationID)
stationID <- stationID[c(1:201),]

stationID$receiverID[stationID$INS_SERIAL_NO>500000] <- paste("VR2AR-", stationID$receiverID[stationID$INS_SERIAL_NO>500000], sep = '')
stationID$receiverID[stationID$INS_SERIAL_NO<200000] <- paste("VR2W-", stationID$receiverID[stationID$INS_SERIAL_NO<200000], sep = '')
stationID$receiverID <- as.factor(stationID$receiverID)
summary(stationID$receiverID)
stationID <- stationID[!duplicated(stationID$receiverID),]
str(tracking_data)
str(stationID)
tracking_data <- merge(tracking_data, stationID[c("stationID", "receiverID")], by = "receiverID", all.y = FALSE, allow.cartesian=TRUE)
tracking_data <- tracking_data[!duplicated(tracking_data$row_ref), ]

#check if there is detections before tagging deployment time
str(tracking_data)
#tmp <- tracking_data[which (tracking_data$deployment_time<tracking_data$datetime), ]
#rm(tmp)

summary(as.factor(tracking_data$fishID), maxsum = 999)


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


###############################################################################
##Remove dead fish etc. Maually checked by Jan 20.10.2021
##NOTE: Deleate all tacking data (N=8+1): 1 død laks fra høsten pluss smolt som har kun en registrering på lyttestasjon ved Hellstranda. Hadde fisk vært her burde den ha minst to registeringer på samme eller ulike stasjoner.Har ikke brukt reglen på smolt registrert i elva da de på enkeltstasjoner godt kan ha kun en registrering.
##NOTE: Deleate data before date (N=5): data registrert på lyttestasjoner før fisken ble merket
##NOTE: Deleate data after date: Data fra perioder hvor fisk antas å være død eller merket har blitt utstøtt.
##NOTE: Voksen merker med akselerasjonssensor (3 stk) har blitt definert som død mår akselerasjon viser at merket konstant er i ro.
###############################################################################
# str(tracking_data_17)
# tracking_data_17_2 <- tracking_data_17[which (tracking_data_17$fishID=='118'|
#                                                 tracking_data_17$fishID=='H2021 - 211'|
#                                                 tracking_data_17$fishID=='H2021 - 220'|
#                                                 tracking_data_17$fishID=='H2021 - 223'|
#                                                 tracking_data_17$fishID=='H2021 - 245'|
#                                                 tracking_data_17$fishID=='H2021 - 314'|
#                                                 tracking_data_17$fishID=='H2021 - 344'|
#                                                 tracking_data_17$fishID=='H2021 - 360'|
#                                                 tracking_data_17$fishID=='H2021 - 370'),]
# 
# tracking_data_17 <- dplyr::anti_join(tracking_data_17, tracking_data_17_2, by="row_ref")
# 
# 
# tracking_data_17 <- tracking_data_17[!(tracking_data_17$fishID=='H2021 - 291'& tracking_data_17$datetime<'2021-04-01 23:00:00'), ]
# 
# 
# 
# summary(tracking_data_17_2$fishID)


#######################################################################################
#REMOVE RECEIVERIDFROM AND -TO, STATIONID, TIMETO AND TIMEFROM
str(tracking_data)
tracking_data <- tracking_data[c("transmitterID", "stationID", "receiverID", "datetime", "sensor_value", "row_ref", "fishID", "filter", "false")]
#######################################################################################
#Transform sensor-values
tracking_data$temperature <- ''
tracking_data$temperature <- (tracking_data$sensor_value*0.1575)-5.157
tracking_data_false_detections <- tracking_data[which (tracking_data$false==1),]
tracking_data <- tracking_data[is.na(tracking_data$false), ]
str(tracking_data)


temp_table <- tracking_data[which (!is.na(tracking_data$temperature)), ]
#test <- ggplot(tracking_data, aes(x=datetime, y=temperature, col=fishID)) + ggtitle("Applied filter to data") + theme_classic(base_size = 18) + geom_point() +  xlab("daily_detections") + ylab("n_false_fishID")#+  scale_x_datetime(date_breaks = "2 months")
#test
#ggsave("./data/modified_data/filter_applied_to_receivers_dates.tiff", test, units="cm", width=35, height=30, dpi=600, compression = 'lzw')
summary(as.numeric(tracking_data$temperature))
temp_table$fishID <- as.character(temp_table$fishID)
summary(as.factor(temp_table$fishID))
temp_table$fishID <- as.integer(temp_table$fishID)
temp_table <- temp_table[which (!is.na(temp_table$fishID)), ]
temp_table$fishID <- as.character(temp_table$fishID)
temp_table$fishID <- as.factor(temp_table$fishID)
str(temp_table)

tmp <- temp_table %>%
  filter(is.na(false)) %>%
  mutate(dth=round_date(datetime, "7 days")) %>%
  group_by(fishID, dth) %>%
  dplyr::summarize(mean_temp = mean(temperature))
str(tmp)
rm(temp_table)

str(tmp)
test <- ggplot(tmp, aes(x=dth, y=mean_temp, col=fishID)) + ggtitle("Temperature use") + theme_classic(base_size = 18) + geom_point() +  xlab("date") + ylab("avg_weekly_temperature_celcius")+  scale_x_datetime(date_breaks = "1 months")
test
ggsave("./data/modified_data/avg_weekly_temperature.tiff", test, units="cm", width=70, height=30, dpi=600, compression = 'lzw')


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
#tracking_data$category <- interaction(tracking_data_18$transmitterID, tracking_data_18$receiverID)
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


#write.csv(tracking_data, "./data/modified_data/greenland_filtered.csv", row.names = FALSE)
saveRDS(tracking_data, "./data/modified_data/filtered_tracking_data.rds")
saveRDS(tracking_data_false_detections, "./data/modified_data/tracking_data_false_detections.rds")
saveRDS(stationID, "./data/modified_data/stationID.rds")
saveRDS(fishdata, "./data/modified_data/fishdata.rds")
