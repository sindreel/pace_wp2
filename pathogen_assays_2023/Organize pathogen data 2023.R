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

