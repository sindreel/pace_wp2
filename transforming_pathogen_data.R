#Peliminary analyses Pathogen data received 04.12.2021

###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################
options(scipen=999)
#test

#########################################################################
#download Pathogen data
#########################################################################

#-----------------------------------------------------------------------
# Download data from external source. This is best practice for making 
# your code transportable among different computers.. 
#-----------------------------------------------------------------------

# This script downloads and merges the data needed for further analyses for the CHASES WP2 (Marine migration and physiology) paper.
# End product of this script is a .csv file containing fish info and key migratory variables (timing of marine entry, timing of marine exit, marine residence duration and categorized migratory distance)


# first create new folder (./data/raw_data) to store the files 
# locally (cache down the data) - remember to update gitignore folder (add 
# the line */data/raw_data to the .gitignore file)
dir.create("data/raw_data", showWarnings = FALSE, recursive = TRUE)
dir.create("data/modified_data", showWarnings = FALSE, recursive = TRUE)


#Pahtogen list 11-2021:
pathogen_list <- "https://ntnu.box.com/shared/static/9di50n6g2f2iutu2xpwrbg6f9phemqmr.csv" #Uploaded new version 18.12.20
download.file(url=pathogen_list,destfile="./data/raw_data/pathogen_list.csv")  


#Pahtogen prevalence analyses 11-2021:
pathogens_NOV21 <- "https://ntnu.box.com/shared/static/8spgc94zm6wqk2etfuynyxhrlrg34akf.csv" #Uploaded new version 18.12.20
download.file(url=pathogens_NOV21,destfile="./data/raw_data/pathogens_NOV21.csv")  


library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

#Read pathogen list
pathogen_list <- read.csv("./data/raw_data/pathogen_list.csv", sep = ";")
pathogen_list$agent_name[pathogen_list$agent_name=='Piscine orthoreovirus'] <- 'Piscine orthoreovirus -3'
str(pathogen_list)
summary(as.factor(pathogen_list$agent_name))

pathogens_NOV21 <- read.csv("./data/raw_data/pathogens_NOV21.csv", sep = ";")
str(pathogens_NOV21)
summary(pathogens_NOV21)

str(pathogens_NOV21)
fishid <- pathogens_NOV21[c("alternate_num", "set_location", "common_name")]
fishid <- fishid[!duplicated(fishid$alternate_num), ]
summary(as.factor(fishid$common_name))
fishid$common_name[fishid$common_name=='Brown trout'] <- 'Sea trout'
fishid$common_name[fishid$common_name=='Atlantic'] <- 'Atlantic Salmon'


#transform
colnames(pathogens_NOV21)
#pathogens_NOV21 <-  pathogens_NOV21 %>%           # Applying functions of dplyr
#  mutate_at(c("ascv","c_b_cys","fl_psy","ic_mul","IcD","my_sp","pa_pse","pa_ther","pch_sal","pisck_sal","prv1","prv3","sch","te_dic","te_fin","te_mar"), ~(45-.))
str(pathogens_NOV21)
#pathogens <- pathogens[c(19:76)<0] <- 0
#pathogens_NOV21[,-c(1:18)][pathogens_NOV21[, -c(1:18)] < 0] <- 0
#pathogens_NOV21[,-c(1:18)][pathogens_NOV21[, -c(1:18)] == 999] <- 0
#pathogen_NOV21_raw <- pathogens_NOV21

pathogens_NOV21$common_name[pathogens_NOV21$common_name=='Brown trout'] <- 'Sea trout'
pathogens_NOV21$common_name[pathogens_NOV21$common_name=='Atlantic'] <- 'Atlantic Salmon'

pathogens_NOV21$hkg_alert <- as.factor(pathogens_NOV21$hkg_alert)
pathogens_NOV21$common_name <- as.factor(pathogens_NOV21$common_name)
pathogens_NOV21$dna_id <- as.factor(pathogens_NOV21$dna_id)
pathogens_NOV21$hatchery_wild <- as.factor(pathogens_NOV21$hatchery_wild)
pathogens_NOV21$set_location <- as.factor(pathogens_NOV21$set_location)
pathogens_NOV21$station <- as.factor(pathogens_NOV21$station)
pathogens_NOV21$Transmitter.ID <- as.factor(pathogens_NOV21$Transmitter.ID)
pathogens_NOV21$transmitterID <- pathogens_NOV21$Transmitter.ID
#pathogens_NOV21$pathogen <- as.factor(pathogens_NOV21$pathogen)
#pathogens_NOV21$measurement <- as.numeric(pathogens_NOV21$measurement)
str(pathogens_NOV21)

pathogens_NOV21 <- pathogens_NOV21[c("hkg_alert", "unique_id", "fluidigm_num", "alternate_num", "common_name", "dna_id", "hatchery_wild", "fork_length..mm.", "set_location", "station", "transmitterID", "capture_date",
                                     "ascv", "c_b_cys", "fl_psy", "ic_mul", "IcD", "my_sp", "pa_pse", "pa_ther", "pch_sal", "pisck_sal", "prv1", "prv3", "sch","te_mar")]


count_groups <- pathogens_NOV21[c("set_location", "common_name", "alternate_num")]
count_groups <- count_groups[!duplicated(count_groups$alternate_num), ]
count_groups <- count_groups %>%
  group_by(set_location, common_name) %>%
  dplyr::summarise(pathogen_precence = n())


#Add fish info and behavioral data


pathogens_NOV21 <- gather(pathogens_NOV21, pathogen, copy_n, ascv:te_mar, factor_key=TRUE)

pathogens_NOV21$copy_n <- as.numeric(pathogens_NOV21$copy_n)

pathogens_NOV21 <-  pathogens_NOV21 %>%           # Applying functions of dplyr
  mutate(ct_value =45-copy_n)
pathogens_NOV21$ct_value[pathogens_NOV21$ct_value<0] <- 0
pathogens_NOV21$ct_value[pathogens_NOV21$ct_value==999] <- 0

saveRDS(pathogens_NOV21, "./data/modified_data/pathogens_copyn_and_inverse_ct.RDS")

