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



URL_tracking_data <- "https://ntnu.box.com/shared/static/9p5djdvhwn8lpw369itforit7juxg864.rds" #Uploaded new version 29.11.22
download.file(url=URL_tracking_data,destfile="./data/raw_data/tracking_data_aurland.RDS")

#setwd("C:/Users/role/OneDrive - NORCE/BTN/detections")

getwd()
detections <- readRDS("./data/raw_data/tracking_data_aurland.RDS")
str(detections)


URL_tracking_data <- "https://ntnu.box.com/shared/static/r2jv8xk27igozt0a3pnb6yvcxa21303v.rds" #Uploaded new version 29.11.22
download.file(url=URL_tracking_data,destfile="./data/raw_data/tracking_data_vosso_081223.RDS")

detections_vosso <- readRDS("./data/raw_data/tracking_data_vosso_081223.RDS")

str(detections)
str(detections_vosso)
nam <- names(detections)
detections_vosso <- detections_vosso[c(nam)]
detections <- rbind(detections, detections_vosso)


library(dplyr)

require(tidyverse); require(lubridate); require(rgdal)
library(tidyverse)
library(lubridate)
#library(rgdal)

# pull receiver metadata

library(gsheet)
rec<-gsheet2tbl('https://docs.google.com/spreadsheets/d/18mUpHQkSBs5PKN2XqoOZEUXXjzUTJwyDx7qvU-0kgqs/edit#gid=1414140311') %>% 
  as_tibble %>% 
  dplyr::filter(!is.na(lon))
str(rec)
summary(as.factor(rec$Project))
#rec <- rec[rec$Project=='LAKES Aurland'|rec$Project=='LaKES Aurland',]

saveRDS(rec, "./data/modified_data/stationID_aurland.RDS")

meta<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1ht4dk480HDm5a6Eop1eGKOnQNeGXJnKb7FnFBc9dMGA/edit#gid=0') 
str(meta)
meta <- meta[c(2:55)]
meta_trout <- meta[!is.na(meta$vial), ]
summary(as.factor(meta_trout$Spp))
str(meta_trout)
meta_trout <- meta_trout[meta_trout$Spp=='Salmo trutta', ]
meta_trout <- meta_trout[meta_trout$Year>2019, ]
summary(as.factor(meta_trout$System))
str(meta_trout)
meta_trout <- as.data.frame(meta_trout)
meta_trout <- meta_trout[meta_trout$System=='Aurland'|meta_trout$System=='Beiarelva'|meta_trout$System=='Stjordal'|meta_trout$System=='Vosso', ]
summary(as.factor(meta$ID))
saveRDS(meta_trout, "./data/modified_data/fishdata_PACE_NORCE.RDS")

summary(detections)
summary(as.factor(detections$sensor))
names(meta_trout)
names(detections)
summary(as.factor(detections$oid))
summary(as.factor(meta_trout$ID))
str(detections)
str(meta_trout)
detections <- detections[detections$sensor=='temp', ]
detections <- detections[!is.na(detections$sensor), ]
tmp <- meta_trout[c("ID", "vial", "System")]
str(tmp)
summary(as.factor(tmp$System))

detections <- merge(detections, tmp, by.x = "oid", by.y="ID")
str(detections)
summary(as.factor(detections$System))
detections <- detections[detections$System=='Aurland', ]
detections <- detections[!duplicated(detections), ]
str(detections)

saveRDS(detections, "./data/modified_data/detections_aurland_vosso.RDS")
