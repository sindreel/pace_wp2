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


#Read telemetry data
tracking_data <- readRDS("./data/modified_data/filtered_tracking_data.rds")
tracking_data$fishID <- as.character(tracking_data$fishID)
tracking_data$fishID <- as.integer(tracking_data$fishID)

deployment_data <- readRDS("./data/modified_data/stationID.rds")
fishdata <- readRDS("./data/modified_data/fishdata.rds")
fishdata <- fishdata[!fishdata$gill_ID=="",]
false_detections <- readRDS("./data/modified_data/tracking_data_false_detections.rds")
str(false_detections)
false_detections$fishID <- as.character(false_detections$fishID)
summary(as.factor(false_detections$fishID))
#false_detections$fishID <- as.integer(false_detections$fishID)
false_detections <- false_detections[which (!is.na(false_detections$fishID)), ]
false_detections$fishID <- as.factor(false_detections$fishID)

tracking_data <- rbind.fill(false_detections, tracking_data)
str(tracking_data)
tracking_data$false <- as.factor(tracking_data$false)
summary(tracking_data$false)
tracking_data$filtered_as_false <- ''
tracking_data$filtered_as_false[tracking_data$false=='1'] <- 'yes'
tracking_data$filtered_as_false[is.na(tracking_data$false)] <- 'no'
#tracking_data <- tracking_data[tracking_data$datetime<'2021-01-01 00:00:00',]

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
summary(as.factor(tracking_data$fishID))
detections <- tracking_data[c("fishID", "transmitterID", "stationID", "receiverID", "datetime", "sensor_value", "temperature", "filtered_as_false")]
detections <- detections[!duplicated(detections),]
str(deployment_data)
deployment_data$lat <- deployment_data$DEPLOY_LAT
deployment_data$long <- deployment_data$DEPLOY_LONG
deployments <- deployment_data[c("stationID", "receiverID", "deploy_datetime", "recover_datetime", "lat", "long")]
pathogens <- readRDS("./data/modified_data/pathogens.RDS")
pathogens$transmitterID <- pathogens$Transmitter.ID
pathogens <- merge(pathogens, fishdata[c("transmitterID", "fishID")], by = "transmitterID")
str(pathogens)
summary(pathogens$measurement)
pathogens$inverse_ct <- pathogens$measurement
pathogens <- pathogens[c("fishID", "transmitterID", "set_location", "hkg_alert", "fluidigm_num", "alternate_num", "common_name", "pathogen", "inverse_ct")]



  
pca_gene_expression <- readRDS("./data/modified_data/gene_expression_pca_export.rds")
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

pathogens <- readRDS("./data/modified_data/pathogens.RDS")
str(pathogens) 
pathogens$transmitterID <- pathogens$Transmitter.ID
pathogens <- merge(pathogens, fishdata[c("transmitterID", "fishID")], by = "transmitterID")
fitchip <- readRDS("./data/modified_data/fitchip_long.RDS")
str(fitchip)
fitchip <- merge(fitchip, fishdata[c("transmitterID", "fishID")], by = "transmitterID")
fitchip <- fitchip[c("fishID", "transmitterID", "gill_id", "fitchip_panel", "gene_marker", "measurement")]
str(fishdata)
export_rob <- list(detections=detections,deployments=deployments, fishdata=fishdata, pathogens=pathogens, fitchip=fitchip)
#saveRDS(export_rob, "./data/modified_data/export_rob_beiarfjord_081222.rds")

str(tracking_data)
str(fishdata)
summary(as.factor(fishdata$tag_type))
tracking_data <- merge(tracking_data, fishdata[c("transmitterID", "tag_type")], by="transmitterID")
summary(as.factor(tracking_data$tag_type))


#################################################################
lat1 <- min(deployment_data$DEPLOY_LAT-0.01)
lat2 <- max(deployment_data$DEPLOY_LAT+0.01)
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
lab$STATION_NO <- as.character(lab$STATION_NO)
sa_map2_plt <- OpenStreetMap::autoplot.OpenStreetMap(sa_map2)
sa_map2_plt <- sa_map2_plt + geom_point(data = deployment_data, aes(x = DEPLOY_LONG, y = DEPLOY_LAT), colour = "green", size =  2.5) + xlab("Longitude") + ylab("Latitude")+ ggtitle("Receiver locations") + theme_classic(base_size = 18) #+ 
sa_map2_plt <- sa_map2_plt + geom_label_repel(data = lab, aes(x = DEPLOY_LONG, y = DEPLOY_LAT, label = STATION_NO), colour = "black", size =  2.5, nudge_x = 0.005, nudge_y = 0.005, max.overlaps = Inf)
sa_map2_plt

ggsave("./data/modified_data/study_area_map.tiff", sa_map2_plt, units="cm", width=40, height=30, dpi=200, compression = 'lzw')


str(tracking_data)

lims <- as.POSIXct(strptime(c("2020-08-01","2020-12-01"), format = "%Y-%m-%d"))    #endret tidspunkt
str(tracking_data)
tracking_data$stationID <- as.factor(tracking_data$stationID)
library(ggplot2)
library(lubridate)
str(tracking_data)
#ggplot(subset(tracking_data, tracking_data$fishID=='101'), aes(x=datetime, y=stationID, col=filtered_as_false)) + geom_point()+ theme_classic(base_size = 18)+ theme(legend.position = "top") + ggtitle("101") + labs(fill =element_blank())+ xlab("Month")+  scale_y_discrete(drop = FALSE)+
#  geom_rect(aes(ymin = '22', ymax = '35', xmin = '2021-08-01', xmax= '2022-10-01', fill = "red"), alpha = 0.4)
###################     LOOPS FOR INDIVIDUAL PLOTS (creating a single pdf)    ####################
ID_list <- sort(unique(tracking_data$fishID)) 
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
temp <- tracking_data %>%
  filter(is.na(false)) %>%
  filter(!is.na(fishID)) %>%
  mutate(timeslot=round_date(datetime, "1 day"))%>%
  group_by(fishID, timeslot) %>%
  dplyr::summarize(daily_temp = mean(temperature))

str(temp)
avg_temp <- temp %>%
  group_by(timeslot) %>%
  dplyr::summarize(avg_temp = mean(daily_temp))

temp <- merge(temp, avg_temp, by = "timeslot")
temp$temp_diff <- temp$daily_temp-temp$avg_temp

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

test1 <- ggplot(pca_gene_expression[pca_gene_expression$fishID=='115',], aes(x=panel, y=comp1)) + theme_classic() + geom_col()+  xlab("Panel") + ylab("standardized comp1") + theme_classic(base_size = 18) + ylim(-2,2)

test2 <- ggplot(pathogens[pathogens$fishID=='115',], aes(x=pathogen, y=measurement)) + theme_classic() + geom_col()+  xlab("Pathogen") + ylab("Inverse CT-value (0-45)") + theme_classic(base_size = 18) + ylim(0, 45)

summary(as.factor(tracking_data$fishID))

det_all <- tracking_data %>%
  filter(is.na(false)) %>%
  filter(!is.na(fishID)) %>%
  group_by(stationID) %>%
  dplyr::summarize(n_all = n())

str(det_all)
det_all <- merge(det_all, deployment_data[c("stationID", "DEPLOY_LAT", "DEPLOY_LONG")], by="stationID", all.y=TRUE)
det_all <- det_all[det_all$stationID!=36,]
det_all <- det_all[!is.na(det_all$n_all),]

summary(tracking_data$datetime)

all_fish <- sa_map2_plt <- OpenStreetMap::autoplot.OpenStreetMap(sa_map2)+
  geom_point(data = deployment_data, aes(x = DEPLOY_LONG, y = DEPLOY_LAT), colour = "green", size =  2.5) + xlab("Longitude") + ylab("Latitude")+ ggtitle(paste0("Number of detections fish ", as.character(ID_list[i]))) + theme_classic(base_size = 18)+
  geom_label_repel(data = det_count[det_count$fishID==ID_list[i],], aes(x = DEPLOY_LONG, y = DEPLOY_LAT, label = n_fish), colour = "black", size =  2.5, nudge_x = 0.005, nudge_y = 0.005, max.overlaps = Inf)




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
  
  
  pt[[i]] <- ggplot(temp[temp$fishID==ID_list[i],], aes(x=timeslot, y=temp_diff)) + theme_classic() + geom_point()+  xlab("Date") + ylab("Temperature difference (C)") + theme_classic(base_size = 18)+ scale_x_datetime(limits =lims)+ geom_hline(yintercept=0, linetype="dashed", color = "black")
  
  pr[[i]] <- ggplot(tracking_data[tracking_data$fishID==ID_list[i],], aes(x=datetime, y=stationID, col=filtered_as_false)) + theme_classic() + geom_point()+  xlab("Date") + ylab("Station ID") + theme_classic(base_size = 18)+ scale_x_datetime(limits =lims)
  

  pb[[i]] <- ggplot(pca_gene_expression[pca_gene_expression$fishID==ID_list[i],], aes(x=panel, y=comp1)) + theme_classic() + geom_col()+  xlab("Panel") + ylab("standardized comp1") + theme_classic(base_size = 16) + ylim(-4,4)
  
  pd[[i]] <- ggplot(pathogens[pathogens$fishID==ID_list[i],], aes(x=pathogen, y=measurement)) + theme_classic() + geom_col()+  xlab("Pathogen") + ylab("Inverse CT-value (0-45)") + theme_classic(base_size = 16) + ylim(0, 45) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
   
  pc[[i]] <- ggarrange(pt[[i]],pr[[i]],pb[[i]],pd[[i]], ncol = 2, nrow = 2)
#  pf[[i]] <- ggarrange(pb[[i]],pd[[i]], ncol = 1, nrow = 2)

#pf[[i]] <- ggarrange(pt[[i]],pr[[i]],pb[[i]],pd[[i]], ncol = 2, nrow = 2)
  
pz[[i]] <- ggarrange(pa[[i]],pc[[i]], ncol = 2, nrow = 1, heights = c(2, 1))
  
  #  ps[[i]]
  ggsave(pz[[i]], file=paste0("./data/modified_data/individual_plots/summary_fishid_",  as.character(ID_list[i]),".tiff"), units="cm", width=80, height=30, dpi=600, compression = 'lzw', limitsize = FALSE)
  
}

# Exporting in a single PDF

#pdf(pz, file='./data/modified_data/individual_adult_tracks.pdf', width=8.66, height=6.3)
#ps
#dev.off()

