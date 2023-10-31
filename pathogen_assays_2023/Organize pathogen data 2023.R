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
library(stringr)

#Read pathogen list
pathogen_list <- read.csv("./data/raw_data/pathogen_list_2023.csv", sep = ";")
#pathogen_list$agent_name[pathogen_list$agent_name=='Piscine orthoreovirus'] <- 'Piscine orthoreovirus -3'
pathogen_list <- pathogen_list[pathogen_list$assay_id!="81", ]
str(pathogen_list)
summary(as.factor(pathogen_list$agent_name))
str(pathogen_list)
pathogen_list$ran_in_2020 <- pathogen_list$Ran.in.Norway.2020_SL..outmigrating.
summary(droplevels(as.factor(pathogen_list$ran_in_2020)))
pathogen_list$ran_in_2020 <- str_replace(pathogen_list$ran_in_2020, "ran isav7", "yes")
pathogen_list$ran_in_2020 <- str_replace(pathogen_list$ran_in_2020, "46", "")

pathogen_list$ran_in_2021 <- paste(pathogen_list$Ran.in.Norway_2021_Microbe_runs_AS..retun.migrating..4089.4090..48x2., pathogen_list$Ran.in.Norway_2021_Fitchip_runs_AS..retun.migrating..4186.4187..1x2..1x9., sep="_")
summary(droplevels(as.factor(pathogen_list$ran_in_2021)))
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "yes_", "yes")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "_yessingleton", "yes_singleton")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "yes-singleton-4187 only", "singleton_only_4187")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "yes-4090 onlyyes in duplicate-4186 only", "yes")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "_", "")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "NANA", "")
pathogen_list$ran_in_2021 <- str_replace(pathogen_list$ran_in_2021, "yes-4090 onlyyes in duplicate-4186 only", "yes")

pathogen_list$ran_in_2023 <- paste(pathogen_list$Run.in.Norway_2023_Microbe.chip..48x2., pathogen_list$Run.on.Norway_2003_.Fit.chip..4x2..3x1., sep="_")
summary(droplevels(as.factor(pathogen_list$ran_in_2023)))
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "NA_NA", "")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "_", "")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "_yes", "yes")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "_yes", "yes")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "_yes-singleton", "yes_singleton")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "yes-isav8", "yes")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "yes_", "yes")
pathogen_list$ran_in_2023 <- str_replace(pathogen_list$ran_in_2023, "NA", "")
pathogen_list$ran_in_2023[pathogen_list$Run.on.Norway_203_.Fit.chip..4x2..3x1.=='yes'] <- 'yes'
pathogen_list$ran_in_2023[pathogen_list$Run.on.Norway_203_.Fit.chip..4x2..3x1.=='yes-singleton'] <- 'yes_singleton'


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

#ggsave("./data/modified_data/histogram_copy_numbers.tiff", p1, units="cm", width=80, height=50, dpi=600, compression = 'lzw', limitsize = FALSE)

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

#ggsave("./data/modified_data/histogram_ct_values.tiff", p1, units="cm", width=200, height=180, dpi=200, compression = 'lzw', limitsize = FALSE)

saveRDS(full_list_long_ct,"./data/modified_data/full_assay_list_long_ct_101023.RDS")


str(full_list_long_ct)
str(full_list_long_copy)
head(full_list_long_ct)
head(full_list_long_copy)
hist(full_list_long_ct$value)
hist(full_list_long_copy$value)
summary(full_list_long_copy$value)
hist(full_list_long_copy$value[full_list_long_copy$value<1000000 & full_list_long_copy$value>0])

summary(pathogen_list)

summary(as.factor(full_list_long_copy$assay))
full_list_long_copy <- full_list_long_copy[grepl("Gill", full_list_long_copy$assay), ]
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "PRV3_L1", "prv-3")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "prv_3", "prv-3")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "prv", "prv-1")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "prv-1-3", "prv-3")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_1", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_18", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_20", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_21", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_27", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_29", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_37", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_38", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_39", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_41", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_47", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_50", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_51", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "psy8", "psy")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "_Gill_copy", "")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "isav8", "isav")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "c_b_cys3", "c_b_cys")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "Lesa_COI", "le_sa")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "Ichy_Costia", "ic_spp")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "ihnv_22", "ihnv")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "nu_sal_33", "nu_sal")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "pa_min_36", "pa_min")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "pa_min_36", "pa_min")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "ascv_a", "ascv")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "ascv41", "ascv")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "myco_sp", "my_sp")
full_list_long_copy$assay <- str_replace(full_list_long_copy$assay, "rlo_45", "rlo")




pathogen_list$December.2021.Onward.Names <- str_replace(pathogen_list$December.2021.Onward.Names, "Ic_spp ", "ic_spp")


pathogen_list$December.2021.Onward.Names

unique(full_list_long_copy$vial)
summary(as.factor(full_list_long_copy$scientific_name))

library(gsheet)
meta<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1ht4dk480HDm5a6Eop1eGKOnQNeGXJnKb7FnFBc9dMGA/edit#gid=0') 
str(meta)
meta_trout <- meta[!is.na(meta$vial), ]
summary(as.factor(meta_trout$Spp))
meta_trout <- meta_trout[meta_trout$Spp=='Salmo trutta' | meta_trout$Spp=='Salmo salar' |meta_trout$Spp=='Salmo salar hatchery', ]
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

str(meta_trout)
pathogens_long_trout_salmon <- merge(full_list_long_copy, meta_trout, by = "vial")
str(pathogens_long_trout_salmon)
pathogens_long_trout_salmon <- pathogens_long_trout_salmon[c("vial", "System", "Year", "Transmitter", "Full_ID", "Spp", "assay", "value")]
head(pathogens_long_trout_salmon)


#merge LOD copy columns and evauate number of pathogens that get cut off:
unique(pathogens_long_trout_salmon$assay[pathogens_long_trout_salmon$value>0])
pathogen_list$December.2021.Onward.Names
pathogen_list$assay <- pathogen_list$December.2021.Onward.Names
str(pathogen_list)
pathogen_list$LOD_Copy_95. <- as.numeric(pathogen_list$LOD_Copy_95.)
pathogen_list$LOD_Copy_75. <- as.numeric(pathogen_list$LOD_Copy_75.)
pathogens_long_trout_salmon <- merge(pathogens_long_trout_salmon, pathogen_list[c("assay", "LOD_Copy_95.", "LOD_Copy_75.")], by = "assay", all.x = TRUE)
summary(pathogens_long_trout_salmon$value[is.na(pathogens_long_trout_salmon$LOD_Copy_75.)])
#find which assays have not been merged properly
str(pathogens_long_trout_salmon)
str(pathogen_list)
pathogens_long_trout_salmon$value[is.na(pathogens_long_trout_salmon$LOD_Copy_75.)] <- 0
pathogens_long_trout_salmon$passed_75 <- '' 
pathogens_long_trout_salmon$passed_75[pathogens_long_trout_salmon$value>=pathogens_long_trout_salmon$LOD_Copy_75.] <- 'yes'
pathogens_long_trout_salmon$passed_95 <- '' 
pathogens_long_trout_salmon$passed_95[pathogens_long_trout_salmon$value>=pathogens_long_trout_salmon$LOD_Copy_95.] <- 'yes'
summary(as.factor(pathogens_long_trout_salmon$passed_75))
summary(as.factor(pathogens_long_trout_salmon$passed_95))
tmp <- pathogens_long_trout_salmon$value[pathogens_long_trout_salmon$value>0]

#According to angela we should use the copy numbers for the pathogens.
#Lets bring in the fishdata to try to replicate the RIB from Robs paper.
pathogens_long_trout_salmon$log_value <- log(pathogens_long_trout_salmon$value) # Make log value
pathogens_long_trout_salmon$log_value[pathogens_long_trout_salmon$log_value<0] <- 0
pathogens_long_trout_salmon$log_value[is.na(pathogens_long_trout_salmon$log_value)] <- 0
pathogens_long_trout_salmon$log_value[pathogens_long_trout_salmon$log_value==-Inf] <- 0
summary(pathogens_long_trout_salmon$log_value)

summary(as.factor(pathogens_long_trout_salmon$vial), maxsum = 999)


#check vial 144
t1 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='144',]
t2 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='144_N4078',]#Comment: Identical values, dont need to do anything except removing N_samples 
pathogens_long_trout_salmon <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$vial!='144_N4078', ]

#check vial 153
t1 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='153',]
t2 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='153_N4082',]#Comment: Identical values, dont need to do anything except removing N_samples 
pathogens_long_trout_salmon <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$vial!='153_N4082', ]

#Check BF42
t1 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='BF42',]
t2 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='BF42_2',]#Comment: Duplicated vials with various pathogen results. Remove this vial ID from analyses
pathogens_long_trout_salmon <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$vial!='BF42', ]
pathogens_long_trout_salmon <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$vial!='BF42_2', ]

#Check BF49
t1 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='BF49',]
t2 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='BF49_N4139',]#Comment: Identical values, dont need to do anything except removing N_samples 
pathogens_long_trout_salmon <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$vial!='BF49_N4139', ]

#Check BF56
t1 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='BF56',]
t2 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='BF56_N4146',]#Comment: Identical values, dont need to do anything except removing N_samples 
pathogens_long_trout_salmon <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$vial!='BF56_N4146', ]

#Check S33
t1 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='S33',]
t2 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='S33_N4031',]#Comment: Identical values, dont need to do anything except removing N_samples 
pathogens_long_trout_salmon <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$vial!='S33_N4031', ]

#Check S36
t1 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='S36',]
t2 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='S36_N4034',]#Comment: Identical values, dont need to do anything except removing N_samples 
pathogens_long_trout_salmon <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$vial!='S36_N4034', ]

#Check S40
t1 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='S40',]
t2 <- pathogens_long_trout_salmon[c("vial", "assay", "log_value")][pathogens_long_trout_salmon$vial=='S40_2',]#Comment: Identical values, dont need to do anything except removing N_samples 
pathogens_long_trout_salmon <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$vial!='S36_N4034', ]



saveRDS(pathogens_long_trout_salmon, "./data/modified_data/all_pathogens_long.RDS")
write.csv(pathogens_long_trout_salmon, "./data/modified_data/all_pathogens_long.csv", row.names = FALSE)
head(pathogens_long_trout_salmon)



#################################################################################
#All_trout
#################################################################################
meta_trout <- meta_trout[meta_trout$Spp=='Salmo trutta', ]
all_trout <- meta_trout

str(pathogens_long_trout_salmon)
summary(as.factor(pathogens_long_trout_salmon$Spp))
trout_pathogens <- pathogens_long_trout_salmon[pathogens_long_trout_salmon$Spp=='Salmo trutta', ]
head(trout_pathogens)

#Test with 75% LOD
str(trout_pathogens)
trout_75 <- trout_pathogens[trout_pathogens$passed_75=='yes', ]
max_path <- trout_75%>%
  group_by(assay) %>%
  summarize(max_path = max(log_value))
summary(max_path)
trout_75 <- merge(trout_75, max_path, by = "assay")
trout_75$rib <- trout_75$log_value/trout_75$max_path
summary(trout_75$rib)
trout_75$rib[is.nan(trout_75$rib)] <- 0

tot_rib <- trout_75 %>%
  group_by(vial)%>%
  summarize(tot_rib = sum(rib))
trout_75 <- merge(trout_75, tot_rib, by="vial")
levels = unique(trout_75$vial[order(trout_75$tot_rib)])

trout_75$vial <- factor(trout_75$vial,                                    # Change ordering manually
                               levels = levels)
head(trout_75)
str(trout_75)
ggplot(trout_75, aes(y=value, x=assay, col=vial)) + geom_point()
summary(trout_75$rib)

# Grouped
p0 <- ggplot(trout_75, aes(fill=assay, x=rib, y=vial)) + 
  geom_bar(stat="identity")+
  facet_wrap(~System, scales = "free", ncol=4)

p0

ggsave("./data/modified_data/Pace_all_trout_log_transformed_LOD75.tiff", p0, units="cm", width=35, height=30, dpi=600, compression = 'lzw')

str(trout_pathogens)

head(trout_75)
summary(as.factor(meta_trout$Transmitter))
trout_75 <- subset(trout_75, select=-c(tot_rib))
saveRDS(trout_75, "./data/modified_data/PACE_WP2_WP3_trout_pathogens_LOD75.RDS")



#Test with 95% LOD
str(trout_pathogens)
trout_95 <- trout_pathogens[trout_pathogens$passed_95=='yes', ]
max_path <- trout_95%>%
  group_by(assay) %>%
  summarize(max_path = max(log_value))
summary(max_path)
trout_95 <- merge(trout_95, max_path, by = "assay")
trout_95$rib <- trout_95$log_value/trout_95$max_path
summary(trout_95$rib)
trout_95$rib[is.nan(trout_95$rib)] <- 0

tot_rib <- trout_95 %>%
  group_by(vial)%>%
  summarize(tot_rib = sum(rib))
trout_95 <- merge(trout_95, tot_rib, by="vial")
levels = unique(trout_95$vial[order(trout_95$tot_rib)])

trout_95$vial <- factor(trout_95$vial,                                    # Change ordering manually
                        levels = levels)
head(trout_95)
str(trout_95)
ggplot(trout_95, aes(y=value, x=assay, col=vial)) + geom_point()
summary(trout_95$rib)

# Grouped
p0 <- ggplot(trout_95, aes(fill=assay, x=rib, y=vial)) + 
  geom_bar(stat="identity")+
  facet_wrap(~System, scales = "free", ncol=4)

p0

ggsave("./data/modified_data/Pace_all_trout_log_transformed_LOD95.tiff", p0, units="cm", width=35, height=30, dpi=600, compression = 'lzw')

str(trout_pathogens)

head(trout_95)
summary(as.factor(meta_trout$Transmitter))
trout_95 <- subset(trout_95, select=-c(tot_rib))
saveRDS(trout_95, "./data/modified_data/PACE_WP2_WP3_trout_pathogens_LOD95.RDS")


#################################################################################
#################################################################################



accel_trout <- meta_trout[meta_trout$Transmitter=='LP13-ADT' |meta_trout$Transmitter=='LP13-AT' |meta_trout$Transmitter=='V13A-1x-BL',]
summary(as.factor(accel_trout$System))

accel_pathogens <- merge(trout_pathogens, accel_trout[c("vial")], by = "vial")
str(accel_pathogens)
accel_pathogens <- subset(accel_pathogens, select=-c(max_path, rib, tot_rib))


max_path <- accel_pathogens%>%
  group_by(assay) %>%
  summarize(max_path = max(log_value))
accel_pathogens <- merge(accel_pathogens, max_path, by = "assay")
accel_pathogens$rib <- accel_pathogens$log_value/accel_pathogens$max_path
summary(accel_pathogens$rib)


tot_rib <- accel_pathogens %>%
  group_by(vial)%>%
  summarize(tot_rib = sum(rib))
accel_pathogens <- merge(accel_pathogens, tot_rib, by="vial")
levels = unique(accel_pathogens$vial[order(accel_pathogens$tot_rib)])

accel_pathogens$vial <- factor(accel_pathogens$vial,                                    # Change ordering manually
                  levels = levels)

str(accel_pathogens)

#remove ca_cl (sea lice ran in signleton)
accel_pathogens <- accel_pathogens[accel_pathogens$assay!='ca_cl', ]

ggplot(accel_pathogens, aes(y=value, x=assay, col=vial)) + geom_point()

str(pathogen_list)
accel_pathogen_list <- pathogen_list[pathogen_list$ran_in_2023=='yes'|pathogen_list$ran_in_2023=='yes_singleton', ]
accel_pathogen_list$December.2021.Onward.Names
accel_pathogen_list$agent_name

# Grouped
p0 <- ggplot(accel_pathogens, aes(fill=assay, x=rib, y=vial)) + 
  geom_bar(stat="identity")+
  facet_wrap(~System, scales = "free", ncol=3)

p0

ggsave("./data/modified_data/Pace_accleleration_log_transformed.tiff", p0, units="cm", width=35, height=15, dpi=600, compression = 'lzw')


summary(as.factor(meta_trout$Transmitter))
###########################################################
#Fish included in temperature studies
###########################################################
temp_trout <- meta_trout[meta_trout$Transmitter=='LP13-ADT' |meta_trout$Transmitter=='LP13-AT' |meta_trout$Transmitter=='V13-T-1x-BLU-1'|meta_trout$Transmitter=='V9T-2x' |meta_trout$Transmitter=='LP13-T',]
summary(as.factor(temp_trout$System))

temp_pathogen_list <- pathogen_list[pathogen_list$ran_in_2021=='yes'&pathogen_list$ran_in_2023=='yes', ]
temp_pathogen_list$December.2021.Onward.Names
temp_pathogen_list$agent_name
temp_pathogen_list$December.2021.Onward.Names[temp_pathogen_list$December.2021.Onward.Names=='Ic_spp '] <- 'ic_spp'
temp_pathogen_list$assay <- temp_pathogen_list$December.2021.Onward.Names
temp_pathogen_list$assay

#max pathogen values
temp_pathogens <- merge(trout_pathogens, temp_trout[c("vial")], by = "vial")
temp_pathogens <- subset(temp_pathogens, select=-c(max_path, rib, tot_rib))
summary(as.factor(temp_pathogens$assay))
temp_pathogens$assay[temp_pathogens$assay=='Ichy_Costia'] <- 'ic_spp'
temp_pathogens$assay[temp_pathogens$assay=='prv_3'] <- 'prv-3'
summary(as.factor(temp_pathogens$assay))
temp_pathogens <- merge(temp_pathogens, temp_pathogen_list[c("assay")], by= "assay")


#temp_pathogens <- merge(full_list_long_copy, temp_trout, by = "vial")
hist(temp_pathogens$value[temp_pathogens$value>0 & temp_pathogens$value<1.5], breaks = 15)
?hist
#what was the cutoff again?
#temp_pathogens <- temp_pathogens[temp_pathogens$value>1, ] #make them zero

max_path <- temp_pathogens%>%
  group_by(assay) %>%
  summarize(max_path = max(log_value))
temp_pathogens <- merge(temp_pathogens, max_path, by = "assay")
temp_pathogens$rib <- temp_pathogens$log_value/temp_pathogens$max_path
summary(temp_pathogens$rib)
temp_pathogens$assay <- as.character(temp_pathogens$assay)
str(temp_pathogens)


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
  facet_wrap(~System, scales = "free", ncol=4)

p1
str(temp_pathogens)




ggsave("./data/modified_data/Pace_temperature.tiff", p1, units="cm", width=35, height=15, dpi=600, compression = 'lzw')


#saveRDS(temp_pathogens, "./data/modified_data/fish_metadata_PACE_temp_paper.RDS")
saveRDS(pathogen_list, "./data/modified_data/pathogen_list.RDS")
write.csv(pathogen_list, "./data/modified_data/pathogen_list.csv", row.names = FALSE)
pathogen_list$December.2021.Onward.Names

