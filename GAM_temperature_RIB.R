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

fishdata <- readRDS("./data/modified_data/fishdata_PACE_NORCE.RDS")

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
tracking_bolstad$vial <- tracking_bolstad$gill_sample
tracking_bolstad <- tracking_bolstad[c("transmitterID", "receiverID", "datetime", "temperature", "vial", "fjord", "tagging_date", "lat", "lon")]
tracking_bolstad$datetime <- as.POSIXct(tracking_bolstad$datetime)
str(tracking_bolstad)
tracking_bolstad$day <- tracking_bolstad$datetime-tracking_bolstad$tagging_date
units(tracking_bolstad$day) <- "days"
tracking_bolstad <- tracking_bolstad[tracking_bolstad$day>2 & tracking_bolstad$day<17,]
#tracking_bolstad <- tracking_bolstad[tracking_bolstad$datetime>tracking_bolstad$tagging_date+(2) & tracking_bolstad$datetime<tracking_bolstad$tagging_date+(16),]
summary(as.numeric(tracking_bolstad$day)) #Note - we have only included 15 days here - a bit too little?
summary(tracking_bolstad$datetime)
max(tracking_bolstad$datetime)-min(tracking_bolstad$datetime)
str(tracking_bolstad)



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
tracking_aurland$transmitterID <- tracking_aurland$oid
tracking_aurland$receiverID <- ''
str(fishdata)
fishdata_aurland <- fishdata[fishdata$System=='Aurland', ]
tracking_aurland <- merge(tracking_aurland, fishdata_aurland[c("ID", "dmy", "vial")], by.x="oid", by.y="ID")
tracking_aurland$datetime <- tracking_aurland$dt
tracking_aurland$fjord <- "Aurland"
tracking_aurland$tagging_date <- dmy(tracking_aurland$dmy)
tracking_aurland$tagging_date <- as.POSIXct(tracking_aurland$tagging_date)

tracking_aurland <- tracking_aurland[c("transmitterID", "receiverID", "datetime", "temperature", "vial", "fjord", "tagging_date", "lat", "lon")]
tracking_aurland$datetime <- as.POSIXct(tracking_aurland$datetime)

tracking_aurland$day <- tracking_aurland$datetime-tracking_aurland$tagging_date
units(tracking_aurland$day) <- "days"
tracking_aurland <- tracking_aurland[tracking_aurland$day>2 & tracking_aurland$day<17,]
str(tracking_aurland)
summary(as.numeric(tracking_aurland$day))



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
units(tracking_beiarn$day) <- "days"
tracking_beiarn <- tracking_beiarn[tracking_beiarn$day>2 & tracking_beiarn$day<17,]
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
units(tracking_stjordal$day) <- "days"
tracking_stjordal <- tracking_stjordal[tracking_stjordal$day>2 & tracking_stjordal$day<17,]
summary(as.numeric(tracking_stjordal$day))
str(tracking_stjordal)


tracking_data <- rbind(tracking_aurland, tracking_bolstad, tracking_beiarn, tracking_stjordal)

pathogens <- readRDS("./data/modified_data/fish_metadata_PACE_temp_paper.RDS")
pathogens <- pathogens[!duplicated(pathogens$vial), ]

tracking_data <- merge(tracking_data, pathogens, by = "vial")


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
  group_by(transmitterID, datetime, day, vial, fjord, tagging_date, lat, lon, tot_rib)%>%
  summarize(temperature = first(temperature))

str(tracking_data)
library(scales)

p <- ggplot(tracking_data, aes(x=day, y=temperature, group_by = transmitterID)) + geom_line()+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius") + facet_wrap(~fjord, scales = "free_y", ncol=4)

p

ggsave("./data/modified_data/temperature_lines_14days.tiff", p, units="cm", width=30, height=15, dpi=600, compression = 'lzw', limitsize = FALSE)









########
#GAM
library(mgcv)
library(gratia)
library(dplyr)
library(tidyr)
library(ggplot2)


data1 <- tracking_data

data1 <- droplevels(data1)
summary(data1$day)
summary(data1)
summary(data1$temperature)
data1$mean_temp[data1$mean_temp<0.001] <- 0.001


plt_labs <- labs(y = 'Temperature',
                 x = 'day',
                 colour = 'Fjord')
ggplot(data1, aes(x = day, y = temperature,
                  group = vial, colour = Fjord)) +
  geom_line() +
  facet_wrap(~ Fjord, ncol = 4) +
  plt_labs

str(data1)
str(data1)
data1$day <- as.numeric(data1$day)
data1$vial <- as.factor(data1$vial)
data1$lat <- as.numeric(data1$lat)
data1$lon <- as.numeric(data1$lon)



str(data1)

gam1 <- bam(temperature~s(day, bs="tp", k=28) + s(vial, day, bs="re", k=28) + s(lat, lon, bs="re", k=10) + fjord + tot_rib, 
            data= data1,
            method = "fREML")


summary(gam1)




gam3 <- bam(temperature  ~s(day, bs="tp", k=28) + s(vial, bs="re", k=80) + lat + fjord,
            data= data2,
            AR.start=data2$start.event, rho=valRho,
            discrete=TRUE,nthreads=3,
            method = "fREML")




summary(gam1)
draw(gam1)

data1 <- data1 %>% arrange(fishID, datetime)
data1 <- data1[data1$sex=='F'|data1$sex=='M',]
summary(data1$lenght_mm)
data1 <- data1[!is.na(data1$lenght_mm>0),]
data2 <- start_event(data1, column="mean_temp", event="fishID")
(valRho <- acf(resid(gam1), plot=FALSE)$acf[2])


str(data2)


gam2 <- bam(mean_temp~s(day, bs="tp", k=240) + s(fishID, bs="re", k=80) + s(DEPLOY_LAT, bs="tp", k=5),
            data= data2,
            AR.start=data2$start.event, rho=valRho,
            discrete=TRUE,nthreads=3,
            method = "REML")

#data2$lat <- data2$DEPLOY_LAT*(maxlat-minlat)
summary(data2$day)
gam3 <- bam(mean_temp~s(day, bs="tp", k=320) + s(fishID, bs="re", k=80) + DEPLOY_LAT + lenght_mm + sex,
            data= data2,
            AR.start=data2$start.event, rho=valRho,
            discrete=TRUE,nthreads=3,
            method = "fREML")


draw(gam3)

compareML(gam2, gam3)

summary(data)

summary(gam2)
summary(gam3)


predict_gam(gam3, values = list(f1 = c(0.5, 1, 1.5))) %>%
  ggplot(aes(x2, fit)) +
  geom_smooth_ci(f1)


maxlat <- max(data2$DEPLOY_LAT)
minlat <- min(data2$DEPLOY_LAT)
difflat <- maxlat-minlat
medlong <- median(data2$DEPLOY_LONG)
4.381e+00*(maxlat-minlat)/40.59599
7.352e-02*(maxlat-minlat)/40.59599
4.381e+00*(maxlat-minlat)
7.357e-02*(maxlat-minlat)
str(data2)
summary(data2$sex)
library(geosphere)
distm(c(medlong, minlat), c(medlong, maxlat), fun = distHaversine)
#draw(gam3)

library(mgcv)
library(tidymv)
model_p <- predict_gam(gam3)
model_p

predict_gam(gam3, values = list(f1 = c(minlat, (((minlat+maxlat)/2)), maxlat))) %>%
  ggplot(aes(day, fit)) +
  geom_smooth_ci(f1)



maxsize <- max(data2$lenght_mm)
minsize <- min(data2$lenght_mm)
-2.202e-03*(maxsize-minsize)
1.116e-03*(maxsize-minsize)

str(data2)
gam.check(gam3)



#calculate the average experienced temp during first week og migration

data3 <- data2
avg_temp <- data3%>%
  group_by(fishID)%>%
  summarize(out = first(day))

avg_temp$weekend <- avg_temp$out+7
head(avg_temp)

str(data3)
data3 <- merge(data3, avg_temp, by="fishID")
data3 <- data3[which (data3$day<data3$weekend),]
head(data3)



avg_temp <- data3%>%
  group_by(fishID)%>%
  summarize(mean_temp = mean(mean_temp))

mean(avg_temp$mean_temp)
sd(avg_temp$mean_temp)
min(avg_temp$mean_temp)
max(avg_temp$mean_temp)


p <- ggplot(data3, aes(x=datetime, y=mean_temp, col=fishID)) + geom_line()+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + 
  xlab("Month") + ylab("Celcius")+scale_x_datetime(date_breaks="1 month", labels = date_format("%b"))

p



new_data <- tidyr::expand(data2, nesting(fishID, DEPLOY_LAT,sex, lenght_mm, mean_temp),
                          date = unique(day))
head(new_data)
m1_pred <- bind_cols(data2,
                     as.data.frame(predict(gam3, data2 = data2,
                                           se.fit = TRUE)))

ggplot(m1_pred, aes(x = day, y = fit, group = fishID,
                    colour = sex)) +
  geom_line() +
  facet_wrap(~ sex) +
  plt_labs

ggplot(m1_pred, aes(x = day, y = fit, group = fishID,
                    colour = DEPLOY_LAT)) +
  geom_line() +
  geom_point(data = data2, aes(y = mean_temp)) +
  facet_wrap(~ fishID) +
  plt_labs

plot(gam3, page = 1)

gam.check(gam3, page = 1)










str(data1)
tempmod <- data1[c("fishID", "day", "mean_temp", "lenght_mm", "DEPLOY_LAT", "stationID")]
tempmod <- tempmod[complete.cases(tempmod),]




model_temp_k10 <- gam(mean_temp ~  date + lenght_mm, k = 10, bs="tp",
                      data=tempmod,
                      method="REML")

summary(model_temp_k10)
plot(model_temp_k10)

# We can also use the gratia package
draw(model_temp_k10)

# is it flexible enough?
summary(model_temp_k10)
str(tempmod)
tempmod$day <- tempmod$date - min(tempmod$date)
tempmod$day <- as.numeric(tempmod$day)/(60*60*24)
model_temp_k30 <- gam(mean_temp ~  s(day, k=10) + s(lenght_mm, k=10)+ s(DEPLOY_LAT, k=10)+cluster + s(fishID, bs="re"),
                      data=tempmod,
                      method="REML")

?gam
draw(model_temp_k30)
plot(model_temp_k30)
summary(model_temp_k30)
model_temp_k30$sp
str(tempmod)
tempmod <- droplevels(tempmod)
?bam
speed_mod2_bam2 <- bam(mean_temp~s(day, fishID, bs="fs") + s(DEPLOY_LAT, k=15)+cluster,
                       data= filter(tempmod),
                       family = Gamma(link="log"),
                       discrete = TRUE,
                       method = "REML")

plot(speed_mod2_bam2, page = 1)
str()
gam.check(speed_mod2_bam2)
draw(speed_mod2_bam2)
summary(speed_mod2_bam2)
AIC(speed_mod2_bam2)
summary(tempmod$cluster)
speed_mod2_bam2$sp

tempmod$fjord_zone <- 'mid'
tempmod$fjord_zone[tempmod$DEPLOY_LAT<60.15] <- 'outer'
tempmod$fjord_zone[tempmod$DEPLOY_LAT>60.25] <- 'inner'
tempmod$fjord_zone[tempmod$stationID>4 & tempmod$stationID<17] <- 'estuary'
tempmod$fjord_zone <- as.factor(tempmod$fjord_zone)
summary(tempmod$fjord_zone)

p <- ggplot(tempmod, aes(x=day, y=mean_temp, col= fjord_zone)) + geom_point()+ geom_smooth()+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("Fjord zones") + 
  xlab("Day of tracking") + ylab("Celsius")
p