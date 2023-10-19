#Sorting pathogen data that was received September 2023

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
pathogen_list <- "https://ntnu.box.com/shared/static/0gcqxbw2662n4tcd2lf8e44m697pxjxg.csv" #Uploaded new version 18.12.20
download.file(url=pathogen_list,destfile="./data/raw_data/pathogen_list_2023.csv")  


#download pathogen exports:
norce_aug_8_2023_ct_copy_norm_24Aug2023_microbes_only_URL <- "https://ntnu.box.com/shared/static/wmmtgtluqzbnyxxz7isyg2kw01nor5in.csv" #Uploaded new version 18.12.20
download.file(url=norce_aug_8_2023_ct_copy_norm_24Aug2023_microbes_only_URL,destfile="./data/raw_data/norce_aug_8_2023_ct_copy_norm_24Aug2023_microbes_only.csv")  


Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised_URL <- "https://ntnu.box.com/shared/static/atqt39wpexon7ge9v420l5fbeazw031e.csv" #Uploaded new version 18.12.20
download.file(url=Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised_URL,destfile="./data/raw_data/Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised.csv")  


NORCE_ct_copy_reexport_aug_23_2023_URL <- "https://ntnu.box.com/shared/static/03ieqhh883v93tlvqg0ibee5og11vytq.csv" #Uploaded new version 18.12.20
download.file(url=NORCE_ct_copy_reexport_aug_23_2023_URL,destfile="./data/raw_data/NORCE_ct_copy_reexport_aug_23_2023.csv")  


Norway_Satr_Gill_4468_Fitchip_21June2023_Table_Microbes_only_URL <- "https://ntnu.box.com/shared/static/8fiadkmcx4vjg9e5of8md7pcasdsadcl.csv" #Uploaded new version 18.12.20
download.file(url=Norway_Satr_Gill_4468_Fitchip_21June2023_Table_Microbes_only_URL,destfile="./data/raw_data/Norway_Satr_Gill_4468_Fitchip_21June2023_Table_Microbes_only.csv")  

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

#Read pathogen list
pathogen_list <- read.csv("./data/raw_data/pathogen_list_2023.csv", sep = ";")
#pathogen_list$agent_name[pathogen_list$agent_name=='Piscine orthoreovirus'] <- 'Piscine orthoreovirus -3'
str(pathogen_list)
summary(as.factor(pathogen_list$agent_name))


#Read assays
norce_aug_8_2023_ct_copy_norm_24Aug2023_microbes_only <- read.csv("./data/raw_data/norce_aug_8_2023_ct_copy_norm_24Aug2023_microbes_only.csv", sep = ",")
names(norce_aug_8_2023_ct_copy_norm_24Aug2023_microbes_only)

Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised <- read.csv("./data/raw_data/Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised.csv", sep = ",")
names(Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised)

NORCE_ct_copy_reexport_aug_23_2023 <- read.csv("./data/raw_data/NORCE_ct_copy_reexport_aug_23_2023.csv", sep = ",")
names(NORCE_ct_copy_reexport_aug_23_2023)

Norway_Satr_Gill_4468_Fitchip_21June2023_Table_Microbes_only <- read.csv("./data/raw_data/Norway_Satr_Gill_4468_Fitchip_21June2023_Table_Microbes_only.csv", sep = ",")
names(Norway_Satr_Gill_4468_Fitchip_21June2023_Table_Microbes_only)

summary(as.factor(Norway_Satr_Gill_4468_Fitchip_21June2023_Table_Microbes_only$alternate_num))
summary(as.factor(Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised$alternate_num))

Norway_Satr_Gill_4468_4469_Fitchips <- rbind(Norway_Satr_Gill_4468_Fitchip_21June2023_Table_Microbes_only, Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised)
names(Norway_Satr_Gill_4468_4469_Fitchips)
unique(norce_aug_8_2023_ct_copy_norm_24Aug2023_microbes_only$alternate_num)
unique(Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised$alternate_num)


Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised$latitude <- as.character(Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised$latitude)
Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised$longitude <- as.character(Norway_Satr_Gill_4469_Fitchip_23June2023_Table_microbes_only_revised$longitude)


full_list <- bind_rows(norce_aug_8_2023_ct_copy_norm_24Aug2023_microbes_only, NORCE_ct_copy_reexport_aug_23_2023)
names(full_list)

names(Norway_Satr_Gill_4468_4469_Fitchips)

colnames_full_list <- names(full_list)

full_list <- merge(full_list, Norway_Satr_Gill_4468_4469_Fitchips[c(2, 39:52)], by = "fish_num", all.x = TRUE, all.y = TRUE)
names(full_list)


tempcols <- names(Norway_Satr_Gill_4468_4469_Fitchips[c(39:52)])

tempcols1 <- paste(tempcols, ".x", sep="")
str(tempcols1)
tempcols2 <- paste(tempcols, ".y", sep="")
tempcols <- c(tempcols1, tempcols2)
tempcols_0 <- tempcols
tempcols <- c("fish_num", tempcols)

tmp <- full_list[c(tempcols)]
tmp <- tmp[ , order(names(tmp))]
str(tmp)
full_list$CTV_node2_2940F_Gill_copy.x
full_list$CTV_node2_2940F_Gill_copy.y
full_list$CTV_node2_2940F_Gill_copy.x[is.na(full_list$CTV_node2_2940F_Gill_copy.x)] <- full_list$CTV_node2_2940F_Gill_copy.y[is.na(full_list$CTV_node2_2940F_Gill_copy.x)]
full_list$CTV_node2_2940F_Gill_copy <- full_list$CTV_node2_2940F_Gill_copy.x

full_list$CTV_node2_2940F_Gill_ct.x
full_list$CTV_node2_2940F_Gill_ct.y
full_list$CTV_node2_2940F_Gill_ct.x[is.na(full_list$CTV_node2_2940F_Gill_ct.x)] <- full_list$CTV_node2_2940F_Gill_ct.y[is.na(full_list$CTV_node2_2940F_Gill_ct.x)]
full_list$CTV_node2_2940F_Gill_ct <- full_list$CTV_node2_2940F_Gill_ct.x

full_list$Lesa_COI_Gill_copy.x
full_list$Lesa_COI_Gill_copy.y
full_list$Lesa_COI_Gill_copy.x[is.na(full_list$Lesa_COI_Gill_copy.x)] <- full_list$Lesa_COI_Gill_copy.y[is.na(full_list$Lesa_COI_Gill_copy.x)]
full_list$Lesa_COI_Gill_copy <- full_list$Lesa_COI_Gill_copy.x

full_list$Lesa_COI_Gill_ct.x
full_list$Lesa_COI_Gill_ct.y
full_list$Lesa_COI_Gill_ct.x[is.na(full_list$Lesa_COI_Gill_ct.x)] <- full_list$Lesa_COI_Gill_ct.y[is.na(full_list$Lesa_COI_Gill_ct.x)]
full_list$Lesa_COI_Gill_ct <- full_list$Lesa_COI_Gill_ct.x

full_list$ascv_141_Gill_copy.x
full_list$ascv_141_Gill_copy.y
full_list$ascv_141_Gill_copy.x[is.na(full_list$ascv_141_Gill_copy.x)] <- full_list$ascv_141_Gill_copy.y[is.na(full_list$ascv_141_Gill_copy.x)]
full_list$ascv_141_Gill_copy <- full_list$ascv_141_Gill_copy.x

full_list$ascv_141_Gill_ct.x
full_list$ascv_141_Gill_ct.y
full_list$ascv_141_Gill_ct.x[is.na(full_list$ascv_141_Gill_ct.x)] <- full_list$ascv_141_Gill_ct.y[is.na(full_list$ascv_141_Gill_ct.x)]
full_list$ascv_141_Gill_ct <- full_list$ascv_141_Gill_ct.x

full_list$ca_cl_Gill_copy.x
full_list$ca_cl_Gill_copy.y
full_list$ca_cl_Gill_copy.x[is.na(full_list$ca_cl_Gill_copy.x)] <- full_list$ca_cl_Gill_copy.y[is.na(full_list$ca_cl_Gill_copy.x)]
full_list$ca_cl_Gill_copy <- full_list$ca_cl_Gill_copy.x

full_list$ca_cl_Gill_ct.x
full_list$ca_cl_Gill_ct.y
full_list$ca_cl_Gill_ct.x[is.na(full_list$ca_cl_Gill_ct.x)] <- full_list$ca_cl_Gill_ct.y[is.na(full_list$ca_cl_Gill_ct.x)]
full_list$ca_cl_Gill_ct <- full_list$ca_cl_Gill_ct.x

full_list$cov_Gill_copy.x
full_list$cov_Gill_copy.y
full_list$cov_Gill_copy.x[is.na(full_list$cov_Gill_copy.x)] <- full_list$cov_Gill_copy.y[is.na(full_list$cov_Gill_copy.x)]
full_list$cov_Gill_copy <- full_list$cov_Gill_copy.x

full_list$cov_Gill_ct.x
full_list$cov_Gill_ct.y
full_list$cov_Gill_ct.x[is.na(full_list$cov_Gill_ct.x)] <- full_list$cov_Gill_ct.y[is.na(full_list$cov_Gill_ct.x)]
full_list$cov_Gill_ct <- full_list$cov_Gill_ct.x

full_list$psnv_2_b_Gill_copy.x
full_list$psnv_2_b_Gill_copy.y
full_list$psnv_2_b_Gill_copy.x[is.na(full_list$psnv_2_b_Gill_copy.x)] <- full_list$psnv_2_b_Gill_copy.y[is.na(full_list$psnv_2_b_Gill_copy.x)]
full_list$psnv_2_b_Gill_copy <- full_list$psnv_2_b_Gill_copy.x

full_list$psnv_2_b_Gill_ct.x
full_list$psnv_2_b_Gill_ct.y
full_list$psnv_2_b_Gill_ct.x[is.na(full_list$psnv_2_b_Gill_ct.x)] <- full_list$psnv_2_b_Gill_ct.y[is.na(full_list$psnv_2_b_Gill_ct.x)]
full_list$psnv_2_b_Gill_ct <- full_list$psnv_2_b_Gill_ct.x

names(full_list)
colnames_full_list
tempcols_0

full_list <- full_list %>%
  select(-one_of(tempcols_0))

names(full_list)
tmp <- full_list[c("fish_num", "specimen_unique_id", "alternate_num", "stock_origin", "scientific_name", "transmitter_id", "Norway.number")]
full_list$alternate_num[!is.na(full_list$Norway.number)] <- full_list$Norway.number[!is.na(full_list$Norway.number)]

saveRDS(full_list, "./data/modified_data.RDS")

#Make some data inspection figures.
#You need to reshape your data to long form with tidyr::gather, so you have key and value columns like such:
library(tidyr)
library(ggplot2)

#Example
mtcars %>% gather() %>% head()
#>   key value
#> 1 mpg  21.0
#> 2 mpg  21.0
#> 3 mpg  22.8
#> 4 mpg  21.4
#> 5 mpg  18.7
#> 6 mpg  18.1
?gather

names(full_list)

full_list_long_copy <- full_list %>%
  select(c(39:668, "fish_num", "alternate_num", "stock_origin", "scientific_name", "transmitter_id", "Norway.number"))%>%
  select("fish_num", "alternate_num", "stock_origin", "scientific_name", "transmitter_id", "Norway.number", contains('copy')) %>%
  group_by(fish_num,alternate_num, stock_origin, transmitter_id, Norway.number) %>%
  gather(key = "assay", value = value, -fish_num, -alternate_num, -stock_origin, -scientific_name, -transmitter_id, -Norway.number) %>%
  filter(!is.na(value))%>%
  mutate_at(c(1:5), as.factor)

full_list_long_copy$vial <- full_list_long_copy$alternate_num

#Inspect the number of assays for each fish
tmp <- full_list_long_copy %>%
  group_by(fish_num, stock_origin, assay) %>%
  summarize(n = n())
#Comment - duplicates are not a problem


str(full_list_long_copy)

#Using this as our data, we can map value as our x variable, and use facet_wrap to separate by the key column:
  
p1 <- ggplot(full_list_long_copy, aes(value)) + 
  geom_histogram() +#bins = 10 
  facet_wrap(~assay, scales = 'free')

ggsave("./data/modified_data/histogram_copy_numbers.tiff", p1, units="cm", width=80, height=50, dpi=600, compression = 'lzw', limitsize = FALSE)

saveRDS(full_list_long_copy,"./data/modified_data/full_assay_list_long_copy_101023.RDS")

# make similar for CT values
full_list_long_ct <- full_list %>%
  select(c(39:668, "fish_num", "alternate_num", "stock_origin", "scientific_name", "transmitter_id", "Norway.number"))%>%
  select("fish_num", "alternate_num", "stock_origin", "scientific_name", "transmitter_id", "Norway.number", contains('_ct')) %>%
  group_by(fish_num, stock_origin, transmitter_id, Norway.number) %>%
  gather(key = "assay", value = value, -fish_num, -alternate_num, -stock_origin, -scientific_name, -transmitter_id, -Norway.number) %>%
  filter(!is.na(value))%>%
  mutate_at(c(1:5), as.factor)
names(full_list_long_ct)

full_list_long_ct$vial <- full_list_long_ct$alternate_num

p1 <- ggplot(full_list_long_ct, aes(value, col=stock_origin)) + 
  geom_histogram() +#bins = 10 
  facet_wrap(~assay, scales = 'free')

ggsave("./data/modified_data/histogram_ct_values.tiff", p1, units="cm", width=200, height=180, dpi=200, compression = 'lzw', limitsize = FALSE)

saveRDS(full_list_long_ct,"./data/modified_data/full_assay_list_long_ct_101023.RDS")


str(full_list_long_ct)
str(full_list_long_copy)
head(full_list_long_ct)
head(full_list_long_copy)
hist(full_list_long_ct$value)
hist(full_list_long_copy$value)
summary(full_list_long_copy$value)
hist(full_list_long_copy$value[full_list_long_copy$value<1000000 & full_list_long_copy$value>0])

#According to angela we should use the copy numbers for the pathogens.
#Lets bring in the fishdata to try to replicate the RIB from Robs paper.

library(gsheet)
meta<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1ht4dk480HDm5a6Eop1eGKOnQNeGXJnKb7FnFBc9dMGA/edit#gid=0') 
str(meta)
meta_trout <- meta[!is.na(meta$vial), ]
summary(as.factor(meta_trout$Spp))
meta_trout <- meta_trout[meta_trout$Spp=='Salmo trutta', ]
meta_trout <- meta_trout[meta_trout$Year>2019, ]
summary(as.factor(meta_trout$System))
summary(as.factor(meta$ID))
str(meta_trout)
meta_trout <- as.data.frame(meta_trout)
library(tidyverse)
str(meta_trout)

summary(as.factor(meta_trout$Transmitter))
str(full_list_long_copy)
tmp <- full_list_long_copy[!duplicated(full_list_long_copy$vial), ]

#keep only vials analysed
meta_trout <- merge(meta_trout, tmp[c("vial")], by = "vial")
summary(as.factor(meta_trout$System))
summary(as.factor(meta_trout$Transmitter))


accel_trout <- meta_trout[meta_trout$Transmitter=='LP13-ADT' |meta_trout$Transmitter=='LP13-AT' |meta_trout$Transmitter=='V13A-1x-BL', ]
summary(as.factor(accel_trout$System))

#max pathogen values
accel_pathogens <- merge(full_list_long_copy, accel_trout, by = "vial")
hist(accel_pathogens$value[accel_pathogens$value>0 & accel_pathogens$value<1.5], breaks = 15)
?hist
#what was the cutoff again?
accel_pathogens <- accel_pathogens[accel_pathogens$value>1, ]

max_path <- accel_pathogens%>%
  group_by(assay) %>%
  summarize(max_path = max(value))
accel_pathogens <- merge(accel_pathogens, max_path, by = "assay")
accel_pathogens$rib <- accel_pathogens$value/accel_pathogens$max_path
summary(accel_pathogens$rib)


tot_rib <- accel_pathogens %>%
  group_by(vial)%>%
  summarize(tot_rib = sum(rib))
accel_pathogens <- merge(accel_pathogens, tot_rib, by="vial")
levels = unique(accel_pathogens$vial[order(accel_pathogens$tot_rib)])

accel_pathogens$vial <- factor(accel_pathogens$vial,                                    # Change ordering manually
                  levels = levels)

str(accel_pathogens)
ggplot(accel_pathogens, aes(y=value, x=assay, col=vial)) + geom_point()


# Grouped
ggplot(accel_pathogens, aes(fill=assay, x=rib, y=vial)) + 
  geom_bar(stat="identity")+
  facet_wrap(~System, scales = "free")


summary(as.factor(meta_trout$Transmitter))
###########################################################
#Fish included in temperature studies
###########################################################
temp_trout <- meta_trout[meta_trout$Transmitter=='LP13-ADT' |meta_trout$Transmitter=='LP13-AT' |meta_trout$Transmitter=='V13-T-1x-BLU-1'|meta_trout$Transmitter=='V9T-2x', ]
summary(as.factor(temp_trout$System))

#max pathogen values
temp_pathogens <- merge(full_list_long_copy, temp_trout, by = "vial")
hist(temp_pathogens$value[temp_pathogens$value>0 & temp_pathogens$value<1.5], breaks = 15)
?hist
#what was the cutoff again?
temp_pathogens <- temp_pathogens[temp_pathogens$value>1, ] #make them zero

max_path <- temp_pathogens%>%
  group_by(assay) %>%
  summarize(max_path = max(value))
temp_pathogens <- merge(temp_pathogens, max_path, by = "assay")
temp_pathogens$rib <- temp_pathogens$value/temp_pathogens$max_path
summary(temp_pathogens$rib)


tot_rib <- temp_pathogens %>%
  group_by(vial)%>%
  summarize(tot_rib = sum(rib))
temp_pathogens <- merge(temp_pathogens, tot_rib, by="vial")
levels = unique(temp_pathogens$vial[order(temp_pathogens$tot_rib)])

temp_pathogens$vial <- factor(temp_pathogens$vial,                                    # Change ordering manually
                               levels = levels)

str(temp_pathogens)
ggplot(temp_pathogens, aes(y=value, x=assay, col=vial)) + geom_point()


# Grouped
p1 <-ggplot(temp_pathogens, aes(fill=assay, x=rib, y=vial)) + 
  geom_bar(stat="identity")+
  facet_wrap(~System, scales = "free_y", ncol=4)

p1
str(temp_pathogens)


ggsave("./data/modified_data/Pace_temperature_RDS.tiff", p1, units="cm", width=35, height=15, dpi=600, compression = 'lzw')


#saveRDS(temp_pathogens, "./data/modified_data/fish_metadata_PACE_temp_paper.RDS")
