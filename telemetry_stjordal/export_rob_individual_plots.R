#Making individual plots

###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################

library(plyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(OpenStreetMap)
library(ggpubr)
library(scales)


#Read telemetry data
tracking_data <- readRDS("./data/modified_data/filtered_tracking_data_stjordal.rds")
#tracking_data$fishID <- as.character(tracking_data$fishID)
#tracking_data$fishID <- as.integer(tracking_data$fishID)

deployment_data <- readRDS("./data/modified_data/stationID_stjordal.rds")
fishdata <- readRDS("./data/modified_data/fishdata_stjordal.rds")
false_detections <- readRDS("./data/modified_data/tracking_data_false_detections_stjordal.rds")

export_marc <- deployment_data[c("STATION_NO", "DEPLOY_LAT", "DEPLOY_LONG")]
export_marc <- export_marc[!duplicated(export_marc$STATION_NO),]
write.csv(export_marc, "./data/modified_data/receiver_locations_stjordal_marc_030123.csv")

tracking_data <- rbind.fill(false_detections, tracking_data)
tracking_data$false <- as.factor(tracking_data$false)
summary(tracking_data$false)
tracking_data$filtered_as_false <- ''
tracking_data$filtered_as_false[tracking_data$false=='1'] <- 'yes'
tracking_data$filtered_as_false[is.na(tracking_data$false)] <- 'no'

tracking_data$filtered_as_false <- as.factor(tracking_data$filtered_as_false)
summary(tracking_data$filtered_as_false)
#make a test plot
tracking_data <- merge(tracking_data, fishdata[c("fishID")], by= "fishID")
tracking_data$stationID <- as.integer(tracking_data$stationID)
#tracking_data <- tracking_data[(tracking_data$stationID>24&tracking_data$stationID<32)|tracking_data$stationID>49,]
deployment_data$stationID <- as.integer(deployment_data$stationID)
#deployment_data <- deployment_data[(deployment_data$stationID>24&deployment_data$stationID<32)|deployment_data$stationID>49,]


#Export to Robert
str(tracking_data)
tracking_data <- tracking_data[!duplicated(tracking_data$row_ref),]
tracking_data$fishID <- as.character(tracking_data$fishID)
tracking_data$fishID <- as.factor(tracking_data$fishID)
summary(as.factor(tracking_data$fishID), maxsum = 10000)
str(tracking_data)
detections <- tracking_data[c("fishID", "transmitterID", "stationID", "receiverID", "datetime", "sensor_value", "temperature", "swimming_depth", "swimming_accelleration", "filtered_as_false")]
names(detections) <- c("fishID", "transmitterID", "stationID", "receiverID", "datetime", "sensor_value", "temperature", "depth", "acceleration", "filtered_as_false")
str(detections)
str(deployment_data)
deployment_data$lat <- deployment_data$DEPLOY_LAT
deployment_data$long <- deployment_data$DEPLOY_LONG
deployment_data$deploy_datetime <- deployment_data$DEPLOY_DATE_TIME....yyyy.mm.ddThh.mm.ss.
deployment_data$recover_datetime <- deployment_data$RECOVER_DATE_TIME..yyyy.mm.ddThh.mm.ss.
str(deployment_data)
deployments <- deployment_data[c("stationID", "receiverID", "deploy_datetime", "recover_datetime", "lat", "long")]
deployments <- deployment_data[c("stationID", "receiverID", "deploy_datetime", "recover_datetime", "lat", "long")]
pathogens <- readRDS("./data/modified_data/pathogens.RDS")
pathogens$transmitterID <- pathogens$Transmitter.ID
str(pathogens)
summary(pathogens$set_location)
pathogens <- pathogens[pathogens$set_location=='Stjordal',]
pathogens <- merge(pathogens, fishdata[c("transmitterID", "fishID")], by = "transmitterID")
str(pathogens)
summary(pathogens$measurement)
pathogens$inverse_ct <- pathogens$measurement
pathogens <- pathogens[c("fishID", "transmitterID", "set_location", "hkg_alert", "fluidigm_num", "alternate_num", "common_name", "pathogen", "inverse_ct")]




pca_gene_expression <- readRDS("./data/modified_data/gene_expression_pca_export.rds")
str(pca_gene_expression)
pca_gene_expression <- merge(fishdata[c("fishID", "transmitterID")], pca_gene_expression,  by="transmitterID")
str(pca_gene_expression)
library(tidyr)
names(pca_gene_expression)
pca_gene_expression <- pca_gene_expression[-c(1)]
pca_gene_expression <- gather(pca_gene_expression, panel, comp1, comp1_CL,comp1_IF,comp1_IM,comp1_IS,comp1_MRS,comp1_OS,comp1_TM,comp1_VDD)
summary(as.factor(pca_gene_expression$panel))
pca_gene_expression$panel<-gsub("comp1_","",as.character(pca_gene_expression$panel))
summary(pca_gene_expression$comp1)
pca_gene_expression <- pca_gene_expression %>% group_by(panel) %>% mutate(comp1 = scale(comp1))


fitchip <- readRDS("./data/modified_data/fitchip_long.RDS")
str(fitchip)
fitchip <- fitchip[fitchip$set_location=='Stjordal', ]
fitchip <- merge(fitchip, fishdata[c("transmitterID", "fishID")], by = "transmitterID")
fitchip <- fitchip[c("fishID", "transmitterID", "gill_id", "fitchip_panel", "gene_marker", "measurement")]
str(fitchip)
export_rob <- list(detections=detections,deployments=deployments, fishdata=fishdata, pathogens=pathogens, fitchip=fitchip)
#saveRDS(export_rob, "./data/modified_data/export_rob_stjordal_020123.rds")

#################################################################
str(deployment_data)
deployment_data$DEPLOY_LAT <- as.numeric(deployment_data$DEPLOY_LAT)
deployment_data$DEPLOY_LONG <- as.numeric(deployment_data$DEPLOY_LONG)
deployment_data <- deployment_data[!is.na(deployment_data$DEPLOY_LAT),]
deployment_data <- deployment_data[!is.na(deployment_data$DEPLOY_LONG),]

lat1 <- min(deployment_data$DEPLOY_LAT-0.02)
lat2 <- max(deployment_data$DEPLOY_LAT+0.02)
lon1 <- min(deployment_data$DEPLOY_LONG-0.01)
lon2 <- max(deployment_data$DEPLOY_LONG+0.01)


# other 'type' options are "osm", "maptoolkit-topo", "bing", "stamen-toner",
# "stamen-watercolor", "esri", "esri-topo", "nps", "apple-iphoto", "skobbler";
# play around with 'zoom' to see what happens; 10 seems just right to me
getMapInfo()

sa_map <- openmap(c(lat2, lon1), c(lat1, lon2), type = "osm", minNumTiles = 20, mergeTiles = TRUE)
plot(sa_map)

sa_map <- openmap(c(lat2, lon1), c(lat1, lon2), type = "bing", minNumTiles = 80, mergeTiles = TRUE)
#increase t\to 80 tiles when ok
plot(sa_map)

# reproject onto WGS84
sa_map2 <- openproj(sa_map)

#plot study area with receiver locations
deployment_data$STATION_NO <- as.factor(deployment_data$STATION_NO)
lab <- deployment_data[c("STATION_NO", "DEPLOY_LAT", "DEPLOY_LONG")]
lab <- lab[!duplicated(lab$STATION_NO),]
lab$STATION_NO <- as.character(lab$STATION_NO)
sa_map2_plt <- OpenStreetMap::autoplot.OpenStreetMap(sa_map2)
sa_map2_plt <- sa_map2_plt + geom_point(data = deployment_data, aes(x = DEPLOY_LONG, y = DEPLOY_LAT), colour = "green", size =  2.5) + xlab("Longitude") + ylab("Latitude")+ ggtitle("Receiver locations") + theme_classic(base_size = 18) #+ 
sa_map2_plt <- sa_map2_plt + geom_label_repel(data = lab, aes(x = DEPLOY_LONG, y = DEPLOY_LAT, label = STATION_NO), colour = "black", size =  2.5, nudge_x = 0.005, nudge_y = 0.005, max.overlaps = Inf)
sa_map2_plt

ggsave("./data/modified_data/study_area_map_stjordal.tiff", sa_map2_plt, units="cm", width=60, height=30, dpi=400, compression = 'lzw')


str(tracking_data)

#lims <- as.POSIXct(strptime(c("2020-08-01","2020-12-01"), format = "%Y-%m-%d"))    #endret tidspunkt
str(tracking_data)
tracking_data$stationID <- as.factor(tracking_data$stationID)
library(ggplot2)
library(lubridate)
str(tracking_data)
#ggplot(subset(tracking_data, tracking_data$fishID=='101'), aes(x=datetime, y=stationID, col=filtered_as_false)) + geom_point()+ theme_classic(base_size = 18)+ theme(legend.position = "top") + ggtitle("101") + labs(fill =element_blank())+ xlab("Month")+  scale_y_discrete(drop = FALSE)+
#  geom_rect(aes(ymin = '22', ymax = '35', xmin = '2021-08-01', xmax= '2022-10-01', fill = "red"), alpha = 0.4)
###################     LOOPS FOR INDIVIDUAL PLOTS (creating a single pdf)    ####################
str(tracking_data)
summary(as.factor(tracking_data$tag_type))
ID_list <- as.character(sort(unique(tracking_data$fishID[tracking_data$tag_type=="V13A-1x"]))) 
ID_list

#lims <- as.POSIXct(strptime(c("2020-08-01","2022-01-01"), format = "%Y-%m-%d"))    #endret tidspunkt


det_count <- tracking_data %>%
  filter(is.na(false)) %>%
  filter(!is.na(fishID)) %>%
  group_by(stationID, fishID) %>%
  dplyr::summarize(n_fish = n())

det_count <- merge(det_count, deployment_data[c("stationID", "DEPLOY_LAT", "DEPLOY_LONG")], by="stationID", all.y=TRUE)
det_count <- det_count[det_count$stationID!=36,]

str(tracking_data)

pt <- list()
ps <- list()
pa <- list()
pr <- list()
pc <- list()
pz <- list()
pb <- list()
pd <- list()
pf <- list()

str(tracking_data)
str(det_count)
str(tracking_data)
tracking_data <- as.data.frame(tracking_data)
str(tracking_data)
tracking_data$temperature <- as.numeric(tracking_data$temperature)



det_all <- tracking_data %>%
  filter(is.na(false)) %>%
  filter(!is.na(fishID)) %>%
  group_by(stationID) %>%
  dplyr::summarize(n_all = n())

str(det_all)
det_all <- merge(det_all, deployment_data[c("stationID", "DEPLOY_LAT", "DEPLOY_LONG")], by="stationID", all.y=TRUE)
#det_all <- det_all[det_all$stationID!=36,]
det_all <- det_all[!is.na(det_all$n_all),]
det_all <- det_all[!duplicated(det_all$stationID),]
summary(tracking_data$datetime)


all_fish <- sa_map2_plt <- OpenStreetMap::autoplot.OpenStreetMap(sa_map2)+
  geom_point(data = deployment_data, aes(x = DEPLOY_LONG, y = DEPLOY_LAT), colour = "green", size =  2.5) + xlab("Longitude") + ylab("Latitude")+ ggtitle("Number of detections fish ") + theme_classic(base_size = 18)+
  geom_label_repel(data = det_all, aes(x = DEPLOY_LONG, y = DEPLOY_LAT, label = n_all), colour = "black", size =  2.5, nudge_x = 0.005, nudge_y = 0.005, max.overlaps = Inf)


dir.create("data/modified_data/individual_plots", showWarnings = FALSE, recursive = TRUE)
rm(i)

for (i in seq_along(ID_list)) {
  #ps[[i]] <- ggplot(subset(daily_avg, daily_avg$transmitterID == ID_list[i]), aes(x = datetime2)) + labs(title = paste(ID_list[i])) + theme_classic()
  pa[[i]] <- sa_map2_plt <- OpenStreetMap::autoplot.OpenStreetMap(sa_map2)+
    geom_point(data = deployment_data, aes(x = DEPLOY_LONG, y = DEPLOY_LAT), colour = "green", size =  2.5) + xlab("Longitude") + ylab("Latitude")+ ggtitle(paste0("Number of detections fish ", as.character(ID_list[i]))) + theme_classic(base_size = 18)+
    geom_label_repel(data = det_count[det_count$fishID==ID_list[i],], aes(x = DEPLOY_LONG, y = DEPLOY_LAT, label = n_fish), colour = "black", size =  2.5, nudge_x = 0.005, nudge_y = 0.005, max.overlaps = Inf)
  
  #  img <- readPNG(paste0("./data/raw_data/fish_pictures/", as.character(ID_list[i]),".png"))
  
  #  pa[[i]] <- ggdraw() +
  #    draw_plot(ps[[i]])+
  #    draw_image(img, x = -0.12, y = 0.23, scale = 0.47)
  
  #  ggsave(pa[[i]], file=paste0("./data/modified_data/ndetections_fish_", as.character(ID_list[i]),".tiff"), units="cm", width=40, height=30, dpi=200, compression = 'lzw', limitsize = FALSE)
  
  
  pt[[i]] <- ggplot(tracking_data[tracking_data$fishID==ID_list[i],], aes(x=datetime, y=temperature, col=filtered_as_false)) + theme_classic() + geom_point()+  xlab("Date") + ylab("Temperature (C)") + theme_classic(base_size = 18)+ ylim(-2, 25)+  scale_x_datetime(breaks = seq(from = min(tracking_data$datetime[tracking_data$fishID==ID_list[i]]), to = max(tracking_data$datetime[tracking_data$fishID==ID_list[i]]), by = "1 month"), labels = date_format("%b-%y"))
  
  pr[[i]] <- ggplot(tracking_data[tracking_data$fishID==ID_list[i],], aes(x=datetime, y=stationID, col=filtered_as_false)) + theme_classic() + geom_point()+  xlab("Date") + ylab("Station ID") + theme_classic(base_size = 18)+  scale_x_datetime(breaks = seq(from = min(tracking_data$datetime[tracking_data$fishID==ID_list[i]]), to = max(tracking_data$datetime[tracking_data$fishID==ID_list[i]]), by = "1 month"), labels = date_format("%b-%y"))
  
  pb[[i]] <- ggplot(tracking_data[tracking_data$fishID==ID_list[i],], aes(x=datetime, y=swimming_depth, col=filtered_as_false)) + theme_classic() + geom_point()+  xlab("date") + ylab("swimming_depth") + theme_classic(base_size = 16)+  scale_x_datetime(breaks = seq(from = min(tracking_data$datetime[tracking_data$fishID==ID_list[i]]), to = max(tracking_data$datetime[tracking_data$fishID==ID_list[i]]), by = "1 month"), labels = date_format("%b-%y"))
  
  pd[[i]] <- ggplot(tracking_data[tracking_data$fishID==ID_list[i],], aes(x=datetime, y=swimming_accelleration, col=filtered_as_false)) + theme_classic() + geom_point()+  xlab("date") + ylab("acceleration") + theme_classic(base_size = 16)+  scale_x_datetime(breaks = seq(from = min(tracking_data$datetime[tracking_data$fishID==ID_list[i]]), to = max(tracking_data$datetime[tracking_data$fishID==ID_list[i]]), by = "1 month"), labels = date_format("%b-%y"))
  
  
  
  pc[[i]] <- ggarrange(pt[[i]],pr[[i]],pb[[i]],pd[[i]], ncol = 4, nrow = 1)
  #  pf[[i]] <- ggarrange(pb[[i]],pd[[i]], ncol = 1, nrow = 2)
  
  #pf[[i]] <- ggarrange(pt[[i]],pr[[i]],pb[[i]],pd[[i]], ncol = 2, nrow = 2)
  
  pz[[i]] <- ggarrange(pa[[i]],pc[[i]], ncol = 1, nrow = 2, heights = c(1, 1))
  
  #  ps[[i]]
  ggsave(pz[[i]], file=paste0("./data/modified_data/stjordal_accelleration_fishid_",  as.character(ID_list[i]),".tiff"), units="cm", width=120, height=30, dpi=200, compression = 'lzw', limitsize = FALSE)
  
}

# Exporting in a single PDF

pdf(pz, file='./data/modified_data/individual_adult_tracks.pdf', width=8.66, height=6.3)
ps
dev.off()

