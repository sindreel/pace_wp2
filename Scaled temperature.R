###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################

library(ggplot2)
#library(nlme)
#library(MuMIn)
#library(performance)
library(lubridate)
library(dplyr)
fishdata <- readRDS("./data/modified_data/fishdata_PACE_NORCE.RDS")


######################################################
#Aurland
######################################################
#tracking_aurland_old <- readRDS("./data/raw_data/tracking_data_aurland.RDS")
tracking_aurland <- readRDS("./data/modified_data/detections_aurland_vosso.RDS")
str(tracking_aurland)
summary(as.factor(tracking_aurland$sensor))
tracking_aurland <- tracking_aurland[tracking_aurland$sensor=='temp', ]
tracking_aurland <- tracking_aurland[!is.na(tracking_aurland$sensor), ]

#only temp data in this dataset - all good. Also latlong
summary(as.factor(tracking_aurland$oid)) #comment - all transmitters are only temp sensors here
tracking_aurland$temperature <- tracking_aurland$Data/10
tracking_aurland$receiverID <- tracking_aurland$Receiver
str(tracking_aurland)
tracking_aurland$transmitterID <- tracking_aurland$oid
tracking_aurland$receiverID <- ''
str(fishdata)

fishdata_aurland <- fishdata[fishdata$System=='Aurland'|fishdata$System=='Vosso', ]
tracking_aurland <- merge(tracking_aurland, fishdata_aurland[c("ID", "dmy")], by.x="oid", by.y="ID")
tracking_aurland$datetime <- tracking_aurland$dt
tracking_aurland$fjord[tracking_aurland$System=='Aurland'] <- "Aurland"
tracking_aurland$fjord[tracking_aurland$System=='Vosso'] <- "Vosso"
tracking_aurland$tagging_date <- dmy(tracking_aurland$dmy)
tracking_aurland$tagging_date <- as.POSIXct(tracking_aurland$tagging_date)

tracking_aurland <- tracking_aurland[c("transmitterID", "receiverID", "datetime", "temperature", "vial", "fjord", "tagging_date", "lat", "lon")]
tracking_aurland$datetime <- as.POSIXct(tracking_aurland$datetime)
summary(as.factor(tracking_aurland$fjord))


#Remove fish with too litle data detections in up to 4 seperate hours
tracking_aurland <- tracking_aurland[tracking_aurland$vial!='AUR10',]
tracking_aurland <- tracking_aurland[tracking_aurland$vial!='BF35',]
tracking_aurland <- tracking_aurland[tracking_aurland$vial!='BF41',]
tracking_aurland <- tracking_aurland[tracking_aurland$vial!='BF64',]
tracking_aurland <- tracking_aurland[tracking_aurland$vial!='PA30',]

#add scaling based on average temp for each individual
names(tracking_aurland)
tracking_aurland$date <- floor_date(tracking_aurland$datetime, unit="days")
tracking_aurland$hour <- floor_date(tracking_aurland$datetime, unit="hours")
tracking_aurland$day <- tracking_aurland$date-tracking_aurland$tagging_date
units(tracking_aurland$day) <- "days"
tracking_aurland$day <- floor(tracking_aurland$day)
str(tracking_aurland)

library(dplyr)
tracking_aurland <-  tracking_aurland %>%
  group_by(vial, fjord, hour, date, day) %>%
  dplyr::summarize(temperature = mean(temperature))

tracking_aurland <- tracking_aurland %>%
  group_by(hour, fjord) %>%
  mutate(scale_temp = scale(temperature), min_temp = min(temperature), max_temp = max(temperature), avg_temp = mean(temperature))

tracking_aurland$diff_avg <- tracking_aurland$temperature-tracking_aurland$avg_temp
tracking_aurland$diff_min <- tracking_aurland$min_temp-tracking_aurland$avg_temp
tracking_aurland$diff_max <- tracking_aurland$max_temp-tracking_aurland$avg_temp

tracking_aurland <- tracking_aurland[tracking_aurland$day>2 & tracking_aurland$day<17,]
str(tracking_aurland)
hist(tracking_aurland$scale_temp)
summary(as.numeric(tracking_aurland$day))


p0 <- ggplot(tracking_aurland, aes(x=scale_temp, fill = fjord)) +
  geom_histogram()+
  facet_wrap(~vial,ncol=10)+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_vline(xintercept = 0, linetype="dashed", size=1)

p0

p0 <- ggplot(tracking_aurland, aes(x=diff_avg, fill = fjord)) +
  geom_histogram()+
  facet_wrap(~vial,ncol=10)+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_vline(xintercept = 0, linetype="dashed", size=1)

p0


p0 <- ggplot(tracking_aurland, aes(x=hour, y = scale_temp, col = fjord)) +
  geom_point()+ geom_line()+ 
  facet_wrap(~vial,ncol=10, scales = "free_x")+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_hline(yintercept = 0, linetype="dashed", size=1)

p0

str(tracking_aurland)
p0 <- ggplot(tracking_aurland, aes(x=hour, y = temperature, col = fjord)) +
  geom_point()+ geom_line()+ 
  facet_wrap(~vial,ncol=10, scales = "free")+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_line(aes(x= hour, y = min_temp), col="blue", linetype="dashed", size=0.5)+
  geom_line(aes(x= hour, y = avg_temp), col="black", linetype="dashed", size=0.5) +geom_line(aes(x= hour, y = max_temp), col="red", linetype="dashed", size=0.5)

p0


p0 <- ggplot(tracking_aurland, aes(x=hour, y = diff_avg, col = fjord)) +
  geom_point()+ geom_line()+ 
  facet_wrap(~vial,ncol=10, scales = "free")+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_line(aes(x= hour, y = diff_min), col="blue", linetype="dashed", size=0.5)+
  geom_line(aes(x= hour, y = diff_max), col="red", linetype="dashed", size=0.5) + geom_hline(yintercept = 0, linetype="dashed", size=1)

p0


######################################################
#Beiarfjorden
######################################################
tracking_beiarn <- readRDS("./data/modified_data/fever_beiarfjorden.RDS")  
str(tracking_beiarn)
fishdata_beiarn <- readRDS("./data/modified_data/fishdata.RDS")
tracking_beiarn <- merge(tracking_beiarn, fishdata_beiarn[c("transmitterID", "gill_ID")], by = "transmitterID")
str(tracking_beiarn)
tracking_beiarn$vial <- tracking_beiarn$gill_ID
tracking_beiarn$lat <- tracking_beiarn$DEPLOY_LAT
tracking_beiarn$lon <- tracking_beiarn$DEPLOY_LONG
tracking_beiarn <- tracking_beiarn[c("transmitterID", "receiverID", "datetime", "temperature", "vial", "fjord", "tagging_date", "lat", "lon")]
tracking_beiarn$day <- tracking_beiarn$datetime-tracking_beiarn$tagging_date
str(tracking_beiarn)
tracking_beiarn$date <- floor_date(tracking_beiarn$datetime, unit="days")

# tracking_beiarn <- tracking_beiarn %>%
#   group_by(vial, date) %>%
#   mutate(scale_temp = scale(temperature))


units(tracking_beiarn$day) <- "days"
#tracking_beiarn <- tracking_beiarn[tracking_beiarn$day>2 & tracking_beiarn$day<17,]
summary(as.numeric(tracking_beiarn$day))


######################################################
#Stjordal
######################################################
tracking_stjordal <- readRDS("./data/modified_data/fever_stjordal.RDS")
fishdata_stjordal <- readRDS("./data/modified_data/fishdata_stjordal.RDS")
str(fishdata_stjordal)
export <- fishdata_stjordal
export <- export[export$gill_sample!='',]
write.csv(export, "./data/modified_data/samples_stjordal_140623.csv")
tracking_stjordal <- merge(tracking_stjordal, fishdata_stjordal[c("transmitterID", "gill_sample")], by = "transmitterID")
str(tracking_stjordal)
tracking_stjordal$vial <- tracking_stjordal$gill_sample
tracking_stjordal$lat <- tracking_stjordal$DEPLOY_LAT
tracking_stjordal$lon <- tracking_stjordal$DEPLOY_LONG
tracking_stjordal <- tracking_stjordal[c("transmitterID", "receiverID", "datetime", "temperature", "vial", "fjord", "tagging_date", "lat", "lon")]
tracking_stjordal$day <- tracking_stjordal$datetime-tracking_stjordal$tagging_date
str(tracking_stjordal)
tracking_stjordal$date <- floor_date(tracking_stjordal$datetime, unit="days")
tracking_stjordal$temperature <- as.numeric(tracking_stjordal$temperature)
tracking_stjordal <- tracking_stjordal[!is.na(tracking_stjordal$temperature), ]
# tracking_stjordal <- tracking_stjordal %>%
#   group_by(vial, date) %>%
#   mutate(scale_temp = scale(temperature))
# 
# units(tracking_stjordal$day) <- "days"
# tracking_stjordal <- tracking_stjordal[tracking_stjordal$day>2 & tracking_stjordal$day<17,]
# summary(as.numeric(tracking_stjordal$day))
# str(tracking_stjordal)

#tracking_aurland$receiverID <- as.character(tracking_aurland$receiverID)
#tracking_bolstad$receiverID <- as.character(tracking_bolstad$receiverID)
tracking_beiarn$receiverID <- as.character(tracking_beiarn$receiverID)
tracking_stjordal$receiverID <- as.character(tracking_stjordal$receiverID)

#tracking_aurland$transmitterID <- as.character(tracking_aurland$transmitterID)
#tracking_bolstad$transmitterID <- as.character(tracking_bolstad$transmitterID)
tracking_beiarn$transmitterID <- as.character(tracking_beiarn$transmitterID)
tracking_stjordal$transmitterID <- as.character(tracking_stjordal$transmitterID)

#tracking_aurland$lat <- as.numeric(tracking_aurland$lat)
#tracking_bolstad$lat <- as.numeric(tracking_bolstad$lat)
tracking_beiarn$lat <- as.numeric(tracking_beiarn$lat)
tracking_stjordal$lat <- as.numeric(tracking_stjordal$lat)
#tracking_aurland$lon <- as.numeric(tracking_aurland$lon)
#tracking_bolstad$lon <- as.numeric(tracking_bolstad$lon)
tracking_beiarn$lon <- as.numeric(tracking_beiarn$lon)
tracking_stjordal$lon <- as.numeric(tracking_stjordal$lon)

names(tracking_aurland)
names(tracking_beiarn)
names(tracking_stjordal)



################################################################################
################################################################################
#merge beiarn and Stjordal data
################################################################################
################################################################################

tracking_data <- rbind(tracking_beiarn, tracking_stjordal)
summary(as.factor(tracking_data$fjord))


#add scaling based on average temp for each individual
names(tracking_data)
tracking_data$date <- floor_date(tracking_data$datetime, unit="days")
tracking_data$hour <- floor_date(tracking_data$datetime, unit="hours")
tracking_data$day <- tracking_data$date-tracking_data$tagging_date
units(tracking_data$day) <- "days"
tracking_data$day <- floor(tracking_data$day)
str(tracking_data)


#filter out unrelevant fish
fishdata <- fishdata[fishdata$System=='Stjordal'|fishdata$System=='Beiarelva', ]
summary(as.factor(fishdata$System))
names(fishdata)
tracking_data <- merge(tracking_data, fishdata[c("vial")], by="vial")



#remove S20 and S37
tracking_data <- tracking_data[tracking_data$vial!='S20', ]
tracking_data <- tracking_data[tracking_data$vial!='S37', ]


#remove tracking data over 25 celciuscelci
tracking_data <- tracking_data[tracking_data$temperature<25, ]



library(dplyr)
tracking_data <-  tracking_data %>%
  group_by(vial, fjord, hour, date, day) %>%
  dplyr::summarize(temperature = mean(temperature))


tracking_data <- tracking_data %>%
  group_by(hour, fjord) %>%
  mutate(scale_temp = scale(temperature), min_temp = min(temperature), max_temp = max(temperature), avg_temp = mean(temperature))

tracking_data$diff_avg <- tracking_data$temperature-tracking_data$avg_temp
tracking_data$diff_min <- tracking_data$min_temp-tracking_data$avg_temp
tracking_data$diff_max <- tracking_data$max_temp-tracking_data$avg_temp

tracking_data <- tracking_data[tracking_data$day>2 & tracking_data$day<17,]
str(tracking_data)
hist(tracking_data$scale_temp)
summary(as.numeric(tracking_data$day))


p0 <- ggplot(tracking_data, aes(x=scale_temp, fill = fjord)) +
  geom_histogram()+
  facet_wrap(~vial,ncol=10)+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_vline(xintercept = 0, linetype="dashed", size=1)

p0

p0 <- ggplot(tracking_data, aes(x=diff_avg, fill = fjord)) +
  geom_histogram()+
  facet_wrap(~vial,ncol=10)+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_vline(xintercept = 0, linetype="dashed", size=1)

p0


p0 <- ggplot(tracking_data, aes(x=hour, y = scale_temp, col = fjord)) +
  geom_point()+ geom_line()+ 
  facet_wrap(~vial,ncol=20, scales = "free_x")+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_hline(yintercept = 0, linetype="dashed", size=1)

p0

str(tracking_data)
p0 <- ggplot(tracking_data, aes(x=hour, y = temperature, col = fjord)) +
  geom_point()+ geom_line()+ 
  facet_wrap(~vial,ncol=20, scales = "free")+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_line(aes(x= hour, y = min_temp), col="blue", linetype="dashed", size=0.5)+
  geom_line(aes(x= hour, y = avg_temp), col="black", linetype="dashed", size=0.5) +geom_line(aes(x= hour, y = max_temp), col="red", linetype="dashed", size=0.5)

p0


p0 <- ggplot(tracking_data, aes(x=hour, y = diff_avg, col = fjord)) +
  geom_point()+ geom_line()+ 
  facet_wrap(~vial,ncol=20, scales = "free")+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_line(aes(x= hour, y = diff_min), col="blue", linetype="dashed", size=0.5)+
  geom_line(aes(x= hour, y = diff_max), col="red", linetype="dashed", size=0.5) + geom_hline(yintercept = 0, linetype="dashed", size=1)

p0




###############################################################################
###############################################################################
#Merge Norce and NTNU datasets
###############################################################################
###############################################################################
tracking_data <- rbind(tracking_data, tracking_aurland)
saveRDS(tracking_data, "./data/modified_data/tracking_data_scaled_191223.RDS")

############################################################################################################





###############################################################################
###############################################################################
#Combine with pathogens
###############################################################################
###############################################################################

pathogens <- readRDS("./data/modified_data/fish_metadata_PACE_temp_paper.RDS")
str(pathogens)
summary(as.factor(pathogens$System))
summary(as.factor(tracking_data$fjord[!duplicated(tracking_data$vial)]))
summary(as.factor(pathogens$Spp))

#pathogens <- pathogens[!duplicated(pathogens$vial), ]
unique(pathogens$vial[pathogens$System=='Vosso'])
unique(tracking_data$vial[tracking_data$fjord=='Bolstad'])

library(tidyr)
str(pathogens)
names(pathogens)
pathogens <- pathogens %>% dplyr::select (-c(value, LOD_Copy_95., LOD_Copy_75., passed_75, passed_95, log_value, max_path, ran_in_2021, ran_in_2023))
pathogens <- pivot_wider(pathogens, names_from = "assay", values_from = "rib", id_expand = FALSE)
pathogens[is.na(pathogens)] <- 0

names(pathogens)
#Add warm and cold RIBs
pathogens$warm_rib <- pathogens$c_b_cys + pathogens$ic_mul + pathogens$pa_ther + pathogens$te_mar + pathogens$te_bry 
pathogens$cold_rib <- pathogens$fl_psy + pathogens$pa_pse + pathogens$`prv-1` + pathogens$`prv-3`

tracking_data <- merge(tracking_data, pathogens, by = "vial")
summary(as.factor(tracking_data$fjord))


p0 <- ggplot(tracking_data, aes(x=scale_temp, fill = fjord)) +
  geom_histogram()+
  facet_wrap(~vial,ncol=10)+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_vline(xintercept = 0, linetype="dashed", size=1)

p0

p0 <- ggplot(tracking_data, aes(x=diff_avg, fill = fjord)) +
  geom_histogram()+
  facet_wrap(~vial,ncol=10)+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_vline(xintercept = 0, linetype="dashed", size=1)

p0


p0 <- ggplot(tracking_data, aes(x=hour, y = scale_temp, col = fjord)) +
  geom_point()+ geom_line()+ 
  facet_wrap(~vial,ncol=20, scales = "free_x")+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_hline(yintercept = 0, linetype="dashed", size=1)

p0

str(tracking_data)
p0 <- ggplot(tracking_data, aes(x=hour, y = temperature, col = fjord)) +
  geom_point()+ geom_line()+ 
  facet_wrap(~vial,ncol=20, scales = "free")+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_line(aes(x= hour, y = min_temp), col="blue", linetype="dashed", size=0.5)+
  geom_line(aes(x= hour, y = avg_temp), col="black", linetype="dashed", size=0.5) +geom_line(aes(x= hour, y = max_temp), col="red", linetype="dashed", size=0.5)

p0


p0 <- ggplot(tracking_data, aes(x=hour, y = diff_avg, col = fjord)) +
  geom_point()+ geom_line()+ 
  facet_wrap(~vial,ncol=25, scales = "free")+  theme_classic(base_size = 12) + xlab("Scaled temperature") + geom_line(aes(x= hour, y = diff_min), col="blue", linetype="dashed", size=0.5)+
  geom_line(aes(x= hour, y = diff_max), col="red", linetype="dashed", size=0.5) + geom_hline(yintercept = 0, linetype="dashed", size=1)

p0

names(tracking_data)
summary_table <- tracking_data%>%
  group_by(vial, fjord, tot_rib, pathogen_count, c_b_cys, pch_sal, ic_spp, `prv-3`, fl_psy, sch, `prv-1`, pa_ther, te_mar, te_bry, pa_pse, ascv, ic_mul, warm_rib, cold_rib)%>%
  dplyr::summarize(temperature = mean(temperature), scale_temp=mean(scale_temp), diff_avg=mean(diff_avg))

p0 <- ggplot(summary_table, aes(x=tot_rib, y = diff_avg)) +
  geom_point()+ facet_wrap(~fjord,ncol=4, scales = "free")+  theme_classic(base_size = 12) + xlab("Total RIB") + ylab("Temperature deviation (celcius)")+
  geom_hline(yintercept = 0, linetype="dashed", size=1) + geom_smooth(method = "lm")

p0

names(summary_table)
p0 <- ggplot(summary_table, aes(x=tot_rib, y = scale_temp)) +
  geom_point()+ 
  facet_wrap(~fjord,ncol=4, scales = "free")+
  theme_classic(base_size = 12) + xlab("Total RIB") + ylab("Temperature deviation (celcius)")+
  geom_hline(yintercept = 0, linetype="dashed", size=1) + geom_smooth(method = "lm")

p0

names(summary_table)
p0 <- ggplot(summary_table, aes(x=tot_rib, y = temperature)) +
  geom_point()+ 
  facet_wrap(~fjord,ncol=4, scales = "free")+
  theme_classic(base_size = 12) + xlab("Total RIB") + ylab("Temperature deviation (celcius)")+
  #geom_hline(yintercept = 0, linetype="dashed", size=1) +
  geom_smooth(method = "lm")

p0


summary(lm(summary_table$scale_temp ~summary_table$tot_rib))
summary(lm(summary_table$diff_avg ~summary_table$tot_rib))


p0 <- ggplot(summary_table, aes(x=warm_rib, y = diff_avg)) +
  geom_point()+ facet_wrap(~fjord,ncol=4, scales = "free")+  theme_classic(base_size = 12) + xlab("Warm RIB") + ylab("Temperature deviation (celcius)")+
  geom_hline(yintercept = 0, linetype="dashed", size=1) + geom_smooth(method = "lm")

p0


p0 <- ggplot(summary_table, aes(x=cold_rib, y = diff_avg)) +
  geom_point()+ facet_wrap(~fjord,ncol=4, scales = "free")+  theme_classic(base_size = 12) + xlab("Cold RIB") + ylab("Temperature deviation (celcius)")+
  geom_hline(yintercept = 0, linetype="dashed", size=1) + geom_smooth(method = "lm")

p0

p0 <- ggplot(summary_table, aes(x=`prv-3`, y = diff_avg)) +
  geom_point()+ facet_wrap(~fjord,ncol=4, scales = "free")+  theme_classic(base_size = 12) + xlab("PRV-3") + ylab("Temperature deviation (celcius)")+
  geom_hline(yintercept = 0, linetype="dashed", size=1) + geom_smooth(method = "lm")

p0


p0 <- ggplot(summary_table, aes(x=`prv-3`, y = diff_avg, col=fjord)) +
  geom_point()+  theme_classic(base_size = 12) + xlab("PRV-3") + ylab("Temperature deviation (celcius)")+
  geom_hline(yintercept = 0, linetype="dashed", size=1) + geom_smooth(method = "lm")

p0

p0 <- ggplot(summary_table, aes(x=tot_rib, y = temperature, col=fjord)) +
  geom_point()+  theme_classic(base_size = 12) + xlab("PRV-3") + ylab("Temperature deviation (celcius)")+
  geom_smooth(method = "lm")

p0
names(summary_table)

p0 <- ggplot(summary_table, aes(x=pathogen_count, y = temperature, col=fjord)) +
  geom_point()+  theme_classic(base_size = 12) + xlab("PRV-3") + ylab("Temperature deviation (celcius)")+
  geom_smooth(method = "lm")

p0


saveRDS(summary_table, "./data/modified_data/summary_table_191223.RDS")

library(dplyr)
str(tracking_data)
tracking_data$temperature <- as.numeric(tracking_data$temperature)
tracking_data$fjord <- as.factor(tracking_data$fjord)
tracking_data$transmitterID <- as.factor(tracking_data$transmitterID)
tracking_data$receiverID <- as.factor(tracking_data$receiver)
tracking_data <- tracking_data[!is.na(tracking_data$temperature), ]
#tracking_data$day <- floor(tracking_data$day)
str(tracking_data)

tracking_data$datetime <- floor_date(tracking_data$datetime, "hours")
tracking_data <- tracking_data%>%
  summarize(temperature = mean(temperature))

str(tracking_data)

p <- ggplot(tracking_data, aes(x=day, y=temperature, group = vial)) + geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5))+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p

p <- ggplot(tracking_data, aes(x=day, y=temperature, group = vial)) + geom_line() + geom_point() + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p


#identify the fish at 6 degrees

tmp <- tracking_data[tracking_data$temperature<7,]
tmp <- tmp %>%
  group_by(vial)%>%
  dplyr::summarize(n = n())

p <- ggplot(tracking_data[tracking_data$vial=='S20',], aes(x=day, y=temperature, group = vial)) + geom_line() + geom_point() + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p

p <- ggplot(tracking_data[tracking_data$vial=='S37',], aes(x=day, y=temperature, group = vial)) + geom_line() + geom_point() + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p


p <- ggplot(tracking_data[tracking_data$vial=='S26',], aes(x=day, y=temperature, group = vial)) + geom_line() + geom_point() + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p


p <- ggplot(tracking_data[tracking_data$vial=='S08',], aes(x=day, y=temperature, group = vial)) + geom_line() + geom_point() + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p


p <- ggplot(tracking_data[tracking_data$vial=='S36',], aes(x=day, y=temperature, group = vial)) + geom_line() + geom_point() + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p



p <- ggplot(tracking_data, aes(x=day, y=temperature, group = vial)) + geom_line() + geom_point() + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p


tracking_data$year <- format(as.Date(tracking_data$datetime, format="%d/%m/%Y"),"%Y")

library(scales)
str(tracking_data)

p <- ggplot(tracking_data, aes(x=day, y=temperature, group = vial)) + geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5))+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p


p <- ggplot(tracking_data, aes(x=day, y=temperature, group = vial, col=year)) + geom_line() + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Day of tracking") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p


#ggsave("./data/modified_data/temperature_lines_14days_081223_new.tiff", p, units="cm", width=30, height=15, dpi=600, compression = 'lzw', limitsize = FALSE)


p <- ggplot(tracking_data, aes(x=day, y=scale_temp, group = vial, col=year)) + geom_line() + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p

str(tracking_data)


summary(as.factor(tracking_data))

tmp <- tracking_data %>%
  group_by(vial, fjord, year) %>%
  dplyr::summarize(scale_temp = mean(scale_temp))

tmp <- merge(tmp, pathogens, by = "vial")
str(tmp)

str(pathogens)

summary(tmp)
tmp <- tmp[!is.na(tmp$scale_temp),]


p <- ggplot(tmp, aes(x=rib, y=scale_temp, col=assay)) + geom_point() + geom_smooth() + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius")# + facet_wrap(~fjord, scales = "free_y", ncol=4)

p


p <- ggplot(tmp[tmp$assay=='prv-3',], aes(x=rib, y=scale_temp)) + geom_point() +geom_smooth(method='lm', formula= y~x) + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") #+ facet_wrap(~fjord, scales = "free_y", ncol=4)

p




p <- ggplot(tmp, aes(x=rib, y=scale_temp, col=path_therm_niche)) + geom_point() + geom_smooth(method = 'lm') + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius")# + facet_wrap(~fjord, scales = "free_y", ncol=4)

p


p <- ggplot(tmp[tmp$assay=='prv-3',], aes(x=rib, y=scale_temp)) + geom_point() +geom_smooth(method='lm', formula= y~x) + theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") #+ facet_wrap(~fjord, scales = "free_y", ncol=4)

p



#ggsave("./data/modified_data/temperature_lines_14days.tiff", p, units="cm", width=30, height=15, dpi=600, compression = 'lzw', limitsize = FALSE)


avg_temp <- tracking_data %>%
  group_by(vial)%>%
  summarize(average_temp = mean(temperature))






########
#GAM
library(mgcv)
library(gratia)
library(dplyr)
library(tidyr)
library(ggplot2)


data1 <- tracking_data
str(fishdata)
data1 <- merge(tracking_data, fishdata[c("vial", "TL")], by="vial")

data1 <- droplevels(data1)
summary(data1$day)
summary(data1)
summary(data1$temperature)


# plt_labs <- labs(y = 'Temperature',
#                  x = 'day',
#                  colour = 'Fjord')
# ggplot(data1, aes(x = day, y = temperature,
#                   group = vial, colour = Fjord)) +
#   geom_line() +
#   facet_wrap(~ Fjord, ncol = 4) +
#   plt_labs

str(data1)
str(data1)
data1$day <- as.numeric(data1$day)
data1$vial <- as.factor(data1$vial)
data1$lat <- as.numeric(data1$lat)
data1$lon <- as.numeric(data1$lon)
data1$hour <- as.POSIXct(data1$datetime, format="%H:%M:%S","%H")

data1$hour = format(as.POSIXct(data1$datetime,format="%H:%M:%S"),"%H")

head(data1)


str(data1)
str(data1)

names(data1)
data1$hour <- as.numeric(data1$hour)
head(data1)
data1$doy <- strftime(data1$tagging_date, format = "%j")
data1$doy <- as.integer(data1$doy)
str(data1)
summary(data1$datetime)
data1$day_of_year <- data1$datetime-as.POSIXct('2020-01-01 00:00:00', units = "days")
data1$day_of_year[data1$day_of_year>366] <- data1$day_of_year[data1$day_of_year>366]-366
data1$day_of_year <- as.numeric(data1$day_of_year)

summary(data1$day_of_year)
# gam1 <- bam(temperature~s(day, bs="tp", k=) + s(hour, bs="cc", k=5) + s(fjord, vial, bs="re", k=97) +
#               s(lat, lon, bs="re", k=10) +  s(tot_rib, bs="tp", k=5), 
#             data= data1,
#             method = "fREML")
# 

str(data1)

data1 %>% 
  as_tibble %>% 
  dplyr::select(dt=datetime, vial, fjord, temperature, tot_rib) %>% 
  group_by(vial, dt=round_date(dt, "6 hours"), tot_rib, fjord) %>% 
  dplyr::summarise(temperature=mean(temperature)) %>% 
  dplyr::filter(month(dt)<10) %>% 
  arrange(dt) %>% 
  distinct %>% 
  ggplot(aes(tot_rib, temperature, colour=fjord))+
  geom_point()+
  geom_smooth(method="lm")

gam1 <- bam(temperature~s(day_of_year, by=fjord, bs="tp", k=14) +
              #s(hour, bs="cc", k=5) + 
              s(vial, bs="re") +
              #s(lat, lon, bs="re", k=10) +
              tot_rib+
              pathogen_count+
              TL+
              fjord, 
            data= data1,
            method = "fREML",
            discrete = T)

gam1 <- bam(scale_temp~s(day, by=fjord, bs="tp", k=14) +
              #s(hour, bs="cc", k=5) + 
              s(vial, bs="re") +
              #s(lat, lon, bs="re", k=10) +
              tot_rib*fjord,
            #pathogen_count+
            #TL+
            #fjord, 
            data= data1,
            method = "fREML",
            discrete = T)

summary(gam1)
p1 <- draw(gam1, parametric = TRUE)

plot(gam1)

d <- gratia::draw(gam1)
ggsave("./data/modified_data/temp_model_081223.tiff", p1, units="cm", width=30, height=30, dpi=200, compression = 'lzw')

