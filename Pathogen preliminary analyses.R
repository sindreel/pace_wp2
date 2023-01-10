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
pathogens_NOV21 <-  pathogens_NOV21 %>%           # Applying functions of dplyr
  mutate_at(c("ascv","c_b_cys","fl_psy","ic_mul","IcD","my_sp","pa_pse","pa_ther","pch_sal","pisck_sal","prv1","prv3","sch","te_dic","te_fin","te_mar"), ~(45-.))
str(pathogens_NOV21)
#pathogens <- pathogens[c(19:76)<0] <- 0
pathogens_NOV21[,-c(1:18)][pathogens_NOV21[, -c(1:18)] < 0] <- 0
pathogens_NOV21[,-c(1:18)][pathogens_NOV21[, -c(1:18)] == 999] <- 0
pathogen_NOV21_raw <- pathogens_NOV21

pathogens_NOV21$common_name[pathogens_NOV21$common_name=='Brown trout'] <- 'Sea trout'
pathogens_NOV21$common_name[pathogens_NOV21$common_name=='Atlantic'] <- 'Atlantic Salmon'

pathogens_NOV21$hkg_alert <- as.factor(pathogens_NOV21$hkg_alert)
pathogens_NOV21$common_name <- as.factor(pathogens_NOV21$common_name)
pathogens_NOV21$dna_id <- as.factor(pathogens_NOV21$dna_id)
pathogens_NOV21$hatchery_wild <- as.factor(pathogens_NOV21$hatchery_wild)
pathogens_NOV21$set_location <- as.factor(pathogens_NOV21$set_location)
pathogens_NOV21$station <- as.factor(pathogens_NOV21$station)
pathogens_NOV21$Transmitter.ID <- as.factor(pathogens_NOV21$Transmitter.ID)
#pathogens_NOV21$pathogen <- as.factor(pathogens_NOV21$pathogen)
#pathogens_NOV21$measurement <- as.numeric(pathogens_NOV21$measurement)
str(pathogens_NOV21)

pathogens_NOV21 <- pathogens_NOV21[c("hkg_alert", "unique_id", "fluidigm_num", "alternate_num", "common_name", "dna_id", "hatchery_wild", "fork_length..mm.", "set_location", "station", "Transmitter.ID", "capture_date",
                                     "ascv", "c_b_cys", "fl_psy", "ic_mul", "IcD", "my_sp", "pa_pse", "pa_ther", "pch_sal", "pisck_sal", "prv1", "prv3", "sch","te_mar")]
pathogens_NOV21_0 <- pathogens_NOV21
table_kristi <- pathogens_NOV21[c("unique_id", "fluidigm_num", "alternate_num", "common_name", "dna_id", "hatchery_wild", "fork_length..mm.", "set_location", "station", "Transmitter.ID",  "capture_date")]
write.csv(table_kristi,"./data/modified_data/table_kristi.csv", row.names = FALSE)

pathogens_NOV21 <- pathogens_NOV21[c("hkg_alert", "fluidigm_num", "alternate_num", "common_name", "dna_id", "set_location", "Transmitter.ID", "capture_date",
                                      "ascv", "c_b_cys", "fl_psy", "ic_mul", "IcD", "my_sp", "pa_pse", "pa_ther", "pch_sal", "pisck_sal", "prv1", "prv3", "sch","te_mar")]
colnames(pathogens_NOV21)
str(pathogens_NOV21)

count_groups <- pathogens_NOV21[c("set_location", "common_name", "alternate_num")]
count_groups <- count_groups[!duplicated(count_groups$alternate_num), ]
count_groups <- count_groups %>%
  group_by(set_location, common_name) %>%
  dplyr::summarise(pathogen_precence = n())


#Add fish info and behavioral data


pathogens_NOV21 <- gather(pathogens_NOV21, pathogen, measurement, ascv:te_mar, factor_key=TRUE)

summary(pathogens_NOV21$common_name)

str(pathogens_NOV21)

summary(pathogens_NOV21$measurement)

saveRDS(pathogens_NOV21, "./data/modified_data/pathogens.RDS")


pathogens_NOV21 <- pathogens_NOV21[which (!is.na(pathogens_NOV21$measurement)), ]
pathogens_NOV21 <- pathogens_NOV21[which (pathogens_NOV21$measurement!=0), ]

write.csv(pathogens_NOV21, "./data/modified_data/pathogen_prevalence_NOV21.csv", row.names = FALSE)


###########################################################################
#Plotting proportional detected prevalence
###########################################################################
tmp <- dplyr::select(pathogens_NOV21, common_name, set_location, hatchery_wild, pathogen, measurement)

tmp <- pathogens_NOV21 %>%
  group_by(set_location, common_name, pathogen) %>%
  dplyr::summarise(pathogen_precence = n())
tmp1 <- tmp[tmp$common_name=='Sea trout', ]
str(tmp)
str(pathogen_list)
tmp <- merge(tmp, pathogen_list, by.x = "pathogen", by.y= "assay_name", all.x = TRUE, all.y = FALSE)
tmp$prop_positive[tmp$set_location=='Beiarfjorden'& tmp$common_name=='Sea trout'] <- tmp$pathogen_precence[tmp$set_location=='Beiarfjorden'& tmp$common_name=='Sea trout']/50
tmp$prop_positive[tmp$set_location=='Bolstadfjorden'& tmp$common_name=='Sea trout'] <- tmp$pathogen_precence[tmp$set_location=='Bolstadfjorden'& tmp$common_name=='Sea trout']/32
tmp$prop_positive[tmp$set_location=='Bolstadfjorden'& tmp$common_name=='Atlantic Salmon'] <- tmp$pathogen_precence[tmp$set_location=='Bolstadfjorden'& tmp$common_name=='Atlantic Salmon']/37
tmp$prop_positive[tmp$set_location=='Stjordal'& tmp$common_name=='Atlantic Salmon'] <- tmp$pathogen_precence[tmp$set_location=='Stjordal'& tmp$common_name=='Atlantic Salmon']/3
tmp$prop_positive[tmp$set_location=='Stjordal'& tmp$common_name=='Sea trout'] <- tmp$pathogen_precence[tmp$set_location=='Stjordal'& tmp$common_name=='Sea trout']/35
count_groups


############################################################################
#SEA TROUT
############################################################################

#Number with atleast one pathogen - SEA TROUT
tmp1 <- pathogens_NOV21[pathogens_NOV21$common_name=='Sea trout', ] %>%
    group_by(alternate_num)%>%
    dplyr::summarise(pathogen_precence = n(), proportion = n()/117)


#Summary data total all fjords - SEA TROUT
tmp1 <- pathogens_NOV21[pathogens_NOV21$common_name=='Sea trout', ] %>%
  group_by(pathogen) %>%
  dplyr::summarise(pathogen_precence = n(), proportion = n()/117)

#Summary data total all fjords - Salmon
salmon_pathogens_detections <- pathogens_NOV21[pathogens_NOV21$common_name=='Atlantic Salmon', ] %>%
  group_by(pathogen) %>%
  dplyr::summarise(pathogen_precence = n(), proportion = n()/40)


summary(tmp$set_location)

trout <- tmp[tmp$common_name=='Sea trout', ]
salmon <- tmp[tmp$common_name=='Atlantic Salmon', ]

#Sea trout
p1 <- ggplot(trout, aes(y = agent_name, x = prop_positive*100, fill=set_location))+ theme_classic(base_size = 18) + geom_col(position = position_dodge(width = 0.9)) + theme(axis.text.x=element_text(angle=90, hjust=1))+ ggtitle("Pathogen prevalence") +xlab("Proportion prevalence (%)")+ ylab(element_blank())
p1
ggsave("./data/modified_data/pathogen_prevalence_trout_FEB-22.tiff", p1, units="cm", width=30, height=30, dpi=300, compression = 'lzw')

str(trout)

str(pathogen_list)
str(pathogens_NOV21)
pathogens_NOV21 <- merge(pathogens_NOV21, pathogen_list[c("assay_name", "agent_name")], by.x = "pathogen", by.y = "assay_name")


#prevalence level
p3 <- 
  ggplot(pathogens_NOV21[pathogens_NOV21$common_name=='Sea trout', ], aes(x=agent_name, y=measurement, color=set_location)) +
  geom_boxplot()+
  geom_jitter(width=0.15, alpha=0.5)+
  labs(x="Pathogen name", y="CT value (reversed)") +
  theme(legend.position="none")+
  ylim(0, 45)+
  coord_flip()+
  theme_classic(base_size = 18)
ggsave("./data/modified_data/pathogen_prevalence_level_trout_FEB-22.tiff", p3, units="cm", width=30, height=30, dpi=300, compression = 'lzw')

p3
##################################################################
#Salmon
p4 <- ggplot(salmon, aes(y = agent_name, x = prop_positive*100, fill=set_location))+ theme_classic(base_size = 18) + geom_col(position = position_dodge(width = 0.9)) + theme(axis.text.x=element_text(angle=90, hjust=1))+ ggtitle("Pathogen prevalence") +xlab("Proportion prevalence (%)")+ ylab(element_blank())
p4
ggsave("./data/modified_data/pathogen_prevalence_trout_FEB-22.tiff", p1, units="cm", width=30, height=30, dpi=300, compression = 'lzw')

str(trout)

str(pathogen_list)
str(pathogens_NOV21)
pathogens_NOV21 <- merge(pathogens_NOV21, pathogen_list[c("assay_name", "agent_name")], by.x = "pathogen", by.y = "assay_name")


#prevalence level
p5 <- 
  ggplot(pathogens_NOV21[pathogens_NOV21$common_name=='Atlantic Salmon', ], aes(x=agent_name, y=measurement, color=set_location)) +
  geom_boxplot()+
  geom_jitter(width=0.15, alpha=0.5)+
  labs(x="Pathogen name", y="CT value (reversed)") +
  theme(legend.position="none")+
  ylim(0, 45)+
  coord_flip()+
  theme_classic(base_size = 18)
#ggsave("./data/modified_data/pathogen_prevalence_level_trout_FEB-22.tiff", p3, units="cm", width=30, height=30, dpi=300, compression = 'lzw')

p5


#Shannon diversity
library(vegan)

#Summary data total all fjords - SEA TROUT
str(pathogen_NOV21_raw)
tmp_2 <- pathogens_NOV21
str(tmp_2)
#tmp_2 <- pathogen_NOV21_raw[c(7, 13, 24:76)]
#tmp_2 <- tmp_2[tmp_2$common_name=='Sea trout', ]

#the following script canges all positive calues to 1
tmp_3 <- tmp_2 %>% mutate_if(is.numeric, ~1 * (. > 0))
str(tmp_2)
tmp_3 <- tmp_3 %>%
  group_by(alternate_num, pathogen) %>%
  dplyr::summarise(set_location, common_name, count = n())
tmp_3 <- tmp_3 %>%
  group_by(set_location, common_name, alternate_num) %>%
  dplyr::summarise(pathogen_count= sum(count))

str(tmp_2)
sea_trout <- tmp_2
sea_trout

str(tmp_2)
tmp_2 <- tmp_2 %>%
  group_by(alternate_num, pathogen) %>%
  dplyr::summarise(set_location, common_name, ct_value_inverse = measurement)

str(tmp_3)
tmp_2 <- merge(tmp_2, tmp_3[c("alternate_num", "pathogen_count")], by = "alternate_num", all.x = TRUE)

# combined_shan
# str(combined_shan)
# shapiro.test(shan_stjordal$shan_stjordal)
# hist(shan_stjordal$shan_stjordal)
# hist(shan_bolstad$shan_div)
# hist(shan_beiarn$shan_div)
# kruskal.test(combined_shan$shan_div, combined_shan$fjord)
# kruskal.test(list(shan_stjordal$shan_stjordal, shan_bolstad$shan_div, shan_beiarn$shan_div))
# 
# boxplot(combined_shan$shan_div~combined_shan$fjord)
# str(combined_shan)

#rejoin fish with no pathogens

#all
str(tmp_2)
shan_all <- tmp_2
str(shan_all)
library(tidyr)
shan_all <- spread(shan_all, key = pathogen, value = ct_value_inverse)
shan_all

str(fishid)
#fishid_trout <- fishid[c("alternate_num", "set_location", "common_name")]
shan_all <- merge(shan_all, fishid, by=c("alternate_num", "set_location", "common_name"), all.y = TRUE)

?vegan::diversity
str(shan_all)
#shan_all <- shan_all[c(1, 3:11)]
shan_all[is.na(shan_all)] <- 0
shan_all_div <- vegan::diversity(shan_all[5:11], index = "shannon")
shan_all$shan_div <- shan_all_div
str(shan_all)
sea_trout <- shan_all[shan_all$common_name=='Sea trout', ]
summary(as.factor(sea_trout$set_location))
hist(sea_trout$shan_div[sea_trout$set_location=='Beiarfjorden'])
boxplot(sea_trout$shan_div~sea_trout$set_location)
kruskal.test(sea_trout$shan_div, sea_trout$set_location)


#library(indicspecies)
#data("wetland")
#groups = c(rep(1, 17), rep(2, 14), rep(3,10))
#groups
#str(wetland)

library(indicspecies)
str(sea_trout)
#indicator species sites for sea trout
tmp <-  multipatt(sea_trout[c(4:18)], sea_trout$set_location, control = how(nperm=999))
summary(tmp)



#salmon
summary(shan_all$common_name)
salmon <- shan_all[shan_all$common_name=='Atlantic Salmon',]
summary(salmon$set_location)
kruskal.test(shan_all$shan_div[shan_all$set_location=='Bolstadfjorden'], shan_all$common_name[shan_all$set_location=='Bolstadfjorden'])


shan_bolstadfjorden <- shan_all[shan_all$set_location=='Bolstadfjorden',]
wilcox.test(shan_bolstadfjorden$shan_div ~ shan_bolstadfjorden$common_name)
kruskal.test(shan_bolstadfjorden$shan_div, shan_bolstadfjorden$common_name)
boxplot(shan_bolstadfjorden$shan_div~shan_bolstadfjorden$common_name)

#indicator pathogens among salmo species in Bolstadfjord
tmp <-  multipatt(shan_bolstadfjorden[c(4:18)], shan_bolstadfjorden$common_name, control = how(nperm=999))
summary(tmp)




#Non-metric multidimensional scaling
str(shan_all)
positive_fish <- shan_all[shan_all$pathogen_count>0, ]
tmp_pos <- positive_fish[c(5:18)]
tmp_pos[tmp_pos == 0] <- NA

nmms <- metaMDS(positive_fish[c(5:18)], maxit = 100, permutations = 999, k = 2, autotransform = FALSE, engine = "isoMDS")
plot(nmms, type = "t")

data.scores <- as.data.frame(scores(nmms))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores <- cbind(data.scores, positive_fish)  #  add the grp variable created earlier
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(nmms, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

#plot ordination species
ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(
    data=data.scores, 
    aes(
      x=NMDS1, 
      y=NMDS2,
      colour=common_name), 
    size=3,
    alpha = 0.6)+
  geom_point(
    data=data.scores,
    aes(
      x=NMDS1,
      y=NMDS2,
      colour=common_name,
      shape = common_name),
    size=3.5,
    alpha = 0.6) +
  theme_light()+
  ylab("NMDS2")+
  xlab("NMDS1")+
  stat_ellipse(data=data.scores, aes(x=NMDS1, y=NMDS2,color=common_name, group=common_name),type = "norm")


#plot ordination species
ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(
    data=data.scores[data.scores$common_name=='Sea trout',], 
    aes(
      x=NMDS1, 
      y=NMDS2,
      colour=set_location), 
    size=3,
    alpha = 0.6)+
  geom_point(
    data=data.scores[data.scores$common_name=='Sea trout',],
    aes(
      x=NMDS1,
      y=NMDS2,
      colour=set_location,
      shape = set_location),
    size=3.5,
    alpha = 0.6) +
  theme_light()+
  ylab("NMDS2")+
  xlab("NMDS1")+
  stat_ellipse(data=data.scores, aes(x=NMDS1, y=NMDS2,color=set_location, group=set_location),type = "norm")




#run envfit to ordination 
positive_trout <- positive_fish[positive_fish$common_name=='Sea trout', ]
str(positive_trout)
#positive_trout[c(5:18)] <- positive_trout[c(5:18)] %>% mutate_if(is.numeric, ~1 * (. > 0))#changes all positive calues to 1

nmms <- metaMDS(positive_trout[c(5:18)], maxit = 100, permutations = 999, k = 2, autotransform = FALSE, engine = "isoMDS")
data.scores <- as.data.frame(scores(nmms))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores <- cbind(data.scores, positive_trout)  #  add the grp variable created earlier
head(data.scores)  #look at the data

fish_metadata <- read.csv("./data/modified_data/summary_table_metadata_PACE_WP2_270122.csv")
str(fish_metadata)
str(data.scores)
fish_metadata <- merge (fish_metadata, data.scores["alternate_num"])
fish_metadata$daysurv <- as.numeric(fish_metadata$daysurv)

species.scores <- as.data.frame(scores(nmms, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

str(trout_metadata)
trout_metadata <- fish_metadata[fish_metadata$common_name=='Sea trout',]
trout_metadata <- merge(trout_metadata, shan_all[c("alternate_num", "shan_div")], by = "alternate_num")
gene_expression <- read.csv("./data/modified_data/thermal_stress_expression.csv", sep = ",")
str(gene_expression)

trout_metadata <- merge(trout_metadata, gene_expression, by.x = "dna_id", by.y = "gill_id", all.x = TRUE)

#Export trout_metadata
write.csv(trout_metadata, "./data/modified_data/trout_metadata.csv", row.names = FALSE)

names(gene_expression)

envfit_0 <- (envfit(nmms, trout_metadata[c("CL_H2EB1_672","CL_ICLP2_674","CL_PSMB7_686",
                                           "IF_ES1_668","IF_txn_683","IM_ARRDC2_663","IM_EPD_667","IM_GLUL_670","IM_napepld_676",
                                           "IM_NUPR1_677","IM_ODC1_678","IM_TAGLN3_681","IM_tgfb_682","IS_B2M_182","IS_C5aR_577",
                                           "IS_CD83_579", "IS_IL1B_295" ,"IS_RIG1_361" ,"MRS_ATP5G3_181","MRS_C7_189","MRS_FYB_241", 
                                           "MRS_HTATIP_272","MRS_NKAB2_328","OS_CCL4_195","OS_CFTR_I_206","OS_HBA_254","OS_NDUFB2_322",
                                           "OS_UBA1_605","TM_FKBP10_4_583","TM_HSP70_267","TM_Hsp90a_15_269","TM_HSP90a_6_271","TM_SERPIN_9_380",
                                           "TM_SERPIN20_379","VDD_HERC6_77","VDD_IFI44A_81","VDD_IFIT5_2_83","VDD_MX_86","VDD_NFX_87")], permutations = 999, na.rm = TRUE))
envfit_0

#NOTE: No significant effect of single genes

str(trout_metadata)
envfit_1 <- (envfit(nmms, trout_metadata[c("fork_length..mm.","skin_colour", "daysurv", "shan_div", "thermal_stress_comp1")], permutations = 999, na.rm = TRUE))
envfit_1
en_coord_cont = as.data.frame(scores(envfit_1, "vectors")) * ordiArrowMul(envfit_1)
en_coord_cat = as.data.frame(scores(envfit_1, "factors")) * ordiArrowMul(envfit_1)
envfit_2 <- (envfit(nmms, trout_metadata[c("freshwater_entry_days")], permutations = 999, na.rm = TRUE))
envfit_2
en_coord_cont2 = as.data.frame(scores(envfit_2, "vectors")) * ordiArrowMul(envfit_2)
trout_metadata[c("fork_length..mm.")]
str(data.scores)

gg = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(colour = set_location), size = 3, alpha = 0.5) + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  scale_colour_manual(values = c("orange", "steelblue","purple")) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) +
  labs(colour = "Season")+
  stat_ellipse(data=data.scores, aes(x=NMDS1, y=NMDS2,color=set_location, group=set_location),type = "norm")

gg


gg = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(colour = set_location), size = 3, alpha = 0.5) + 

  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  scale_colour_manual(values = c("orange", "steelblue", "pink"))  + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
              data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
              data = en_coord_cont2, size =1, alpha = 0.5, colour = "grey30") +
  geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
             shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
  geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2+0.04), 
            label = row.names(en_coord_cat), colour = "navy", fontface = "bold") + 
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont)) + 
  geom_text(data = en_coord_cont2, aes(x = NMDS1, y = NMDS2+0.05), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont2)) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Season")+
  stat_ellipse(data=data.scores, aes(x=NMDS1, y=NMDS2,color=set_location, group=set_location),type = "norm")


gg















str(positive_trout)
str(fish_metadata)
str(shan_all)
tmp <- merge(fish_metadata, shan_all, by = "alternate_num")
ggplot(tmp, aes(x=tmp$fork_length..mm., y=tmp$pathogen_count)) + geom_smooth(method="lm") + theme_classic(base_size = 18) + geom_point() + xlab("Body size (mm)") + ylab("Number of detected pathogens")  +ylim (0,5) + ggtitle("d") +xlab(element_blank())
ggplot(tmp, aes(x=tmp$fork_length..mm., y=tmp$pathogen_count, fill= common_name.x)) + geom_smooth(method="lm") + theme_classic(base_size = 18) + geom_point() + xlab("Body size (mm)") + ylab("Number of detected pathogens")  +ylim (0,5) + ggtitle("d") +xlab(element_blank())
ggplot(tmp, aes(x=tmp$fork_length..mm., y=tmp$pathogen_count, fill= set_location.x)) + geom_smooth(method="lm") + theme_classic(base_size = 18) + geom_point() + xlab("Body size (mm)") + ylab("Number of detected pathogens")  +ylim (0,5) + ggtitle("d") +xlab(element_blank())
#Note ingen effekt av kroppslengde
tmp2 <- tmp[which (tmp$shan_div>0),]
ggplot(tmp2, aes(x=tmp2$fork_length..mm., y=tmp2$shan_div, fill= set_location.x)) + geom_smooth(method="lm") + theme_classic(base_size = 18) + geom_point() + xlab("Body size (mm)") + ylab("Shannon diversity")  +ylim (0.6,0.7) + ggtitle("d") +xlab(element_blank())
ggplot(tmp, aes(x=tmp$daysurv, y=tmp$shan_div, fill= set_location.x)) + geom_smooth(method="lm") + theme_classic(base_size = 18) + geom_point() + xlab("Body size (mm)") + ylab("Shannon diversity")  + ggtitle("d") +xlab(element_blank())
ggplot(tmp, aes(x=tmp$fork_length..mm., y=tmp$freshwater_entry_days, fill= set_location.x)) + geom_smooth(method="lm") + theme_classic(base_size = 18) + geom_point() + xlab("Body size (mm)") + ylab("Shannon diversity")  + ggtitle("d") +xlab(element_blank())#+ylim (0,300)

#NOTE dette er ikke noe interessant



str(tmp)


ggplot() + 
  #geom_polygon(data=data.scores,aes(x=NMDS1,y=NMDS2,fill=common_name,group=common_name),alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=common_name,colour=common_name),size=4) + # add the point markers
  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

##pull points from MDS
NMDS1 <- nmms$points[,1] ##also found using scores(birdMDS)
NMDS2 <- nmms$points[,2]
str(nmms)
nmms$species

species <- as.data.frame(nmms$species)
names <- rownames(species)
rownames(species) <- NULL
species <- cbind(names,species)

nmms_plot<-cbind(positive_fish, NMDS1, NMDS2)

str(nmms)

str(nmms_plot)

p<-ggplot(nmms_plot, aes(NMDS1, NMDS2, color=common_name))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points
  geom_point(data=species,mapping = aes(x=V1, y=V2))+
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()
p

plot(nmms, display = c("sites", "species"), choices = c(1,2), type = "p", shrink = FALSE)


p<-ggplot(nmms_plot, aes(NMDS1, NMDS2, color=common_name))+
  stat_ellipse(type='t',size =1)+
  theme_minimal()+geom_label(data=species,aes(V1, V2, label=names), position=position_jitter(.35))
p


hist(nmms$ndis)

#shan_all$pathogen_count <- shan_all$ascv+shan_all$c_b_cys+shan_all$fl_psy+shan_all$ic_mul+shan_all$IcD+shan_all$my_sp+shan_all$pa_pse+shan_all$pa_ther+shan_all$pch_sal+shan_all$pisck_sal+shan_all$prv1+shan_all$prv3+shan_all$sch
#colnames(shan_all)

############################################################################
############################################################################













#Salmon
p2 <- ggplot(salmon, aes(y = agent_name, x = prop_positive*100, fill=set_location))+ theme_classic(base_size = 18) + geom_col(position = position_dodge(width = 0.9)) + theme(axis.text.x=element_text(angle=90, hjust=1))+ ggtitle("Pathogen prevalence") +xlab("Proportion prevalence (%)")+ ylab(element_blank())
p2
ggsave("./data/modified_data/pathogen_prevalence_salmon_NOV-21.tiff", p2, units="cm", width=30, height=30, dpi=300, compression = 'lzw')


ggplot2.barplot(data=df1, xName='time', yName="total_bill",
                groupName='sex', position=position_dodge())

grid_plot <- arrangeGrob(p1, p2, nrow = 2)
ggsave("./data/modified_data/pathogen_prevalence_trout_salmon_NOV-21.tiff", grid_plot, units="cm", width=30, height=30, dpi=300, compression = 'lzw')

#COMPARE SPECIES
tmp2 <- pathogens_NOV21 %>%
  group_by(common_name, pathogen) %>%
  dplyr::summarise(pathogen_precence = n())
tmp2 <- merge(tmp2, pathogen_list, by.x = "pathogen", by.y= "assay_name", all.x = TRUE, all.y = FALSE)

tmp2$prop_positive_species[tmp2$common_name=='Atlantic Salmon'] <- tmp2$pathogen_precence[tmp2$common_name=='Atlantic Salmon']/40
tmp2$prop_positive_species[tmp2$common_name=='Sea trout'] <- tmp2$pathogen_precence[tmp2$common_name=='Sea trout']/122

#Species
p2 <- ggplot(tmp2, aes(y = agent_name, x = prop_positive_species*100, fill=common_name))+ theme_classic(base_size = 18) + geom_col(position = position_dodge(width = 0.9)) + theme(axis.text.x=element_text(angle=90, hjust=1))+ ggtitle("Pathogen prevalence") +xlab("Proportion prevalence (%)")+ ylab(element_blank())
p2
ggsave("./data/modified_data/pathogen_prevalence_salmon_NOV-21.tiff", p2, units="cm", width=30, height=30, dpi=300, compression = 'lzw')


######################################################################################################
#Plotting prevalence levels
######################################################################################################
str(pathogens_NOV21)
pathogens_NOV21 <- merge(pathogens_NOV21, pathogen_list, by.x = "pathogen", by.y= "assay_name", all.x = TRUE, all.y = FALSE)

p3 <- ggplot(pathogens_NOV21[pathogens_NOV21$common_name=='Sea trout', ], aes(y = agent_name, x = measurement, col=set_location))+ theme_classic(base_size = 18) + geom_point(position = position_dodge(width = 0.9)) + theme(axis.text.x=element_text(angle=90, hjust=1))+ ggtitle("Pathogen prevalence") +xlab("Proportion prevalence (%)")+ ylab(element_blank())
p3
ggsave("./data/modified_data/pathogen_prevalence_level_trout_NOV-21.tiff", p3, units="cm", width=30, height=30, dpi=300, compression = 'lzw')

p4 <- ggplot(pathogens_NOV21[pathogens_NOV21$common_name=='Sea trout' & pathogens_NOV21$measurement<5000, ], aes(y = agent_name, x = measurement, col=set_location))+ theme_classic(base_size = 18) + geom_point(position = position_dodge(width = 0.9)) + theme(axis.text.x=element_text(angle=90, hjust=1))+ ggtitle("Pathogen prevalence") +xlab("Proportion prevalence (%)")+ ylab(element_blank())
p4
ggsave("./data/modified_data/pathogen_prevalence_level_5000_trout_NOV-21.tiff", p4, units="cm", width=30, height=30, dpi=300, compression = 'lzw')



p3 <- 
  ggplot(pathogens_NOV21[pathogens_NOV21$common_name=='Sea trout', ], aes(x=agent_name, y=measurement, color=set_location)) +
  geom_boxplot()+
  geom_jitter(width=0.15, alpha=0.5)+
  labs(x="Pathogen name", y="CT value (reversed)") +
  theme(legend.position="none")+
  ylim(0, 45)+
  coord_flip()+
  theme_classic(base_size = 18)
ggsave("./data/modified_data/pathogen_prevalence_level_trout_FEB-22.tiff", p3, units="cm", width=30, height=30, dpi=300, compression = 'lzw')

######################################################################################################




######################################################################################################
#Make network analysis of prevalence
######################################################################################################
str(pathogens_NOV21)
pathogens_NOV21$transmitterID <- pathogens_NOV21$Transmitter.ID
network_data <- pathogens_NOV21[pathogens_NOV21$common_name=='Sea trout',][c("transmitterID", "pathogen", "measurement")]
head(network_data)


BPEdges.ot <- network_data
str(BPEdges.ot)

range(BPEdges.ot$weight)
summary(BPEdges.ot)
str(BPEdges.ot)
Make a bipartite graph out of the edge list
# Use an igraph function to convert the edgelist into a graph object
require(igraph)

BPGraph.ot = graph.data.frame(BPEdges.ot, directed=T)
plot(BPGraph.ot) # Check plot

###########################################################################
#Define bipartite graph
# Define a bipartite mapping
V(BPGraph.ot)$type = bipartite.mapping(BPGraph.ot)$type
class(V(BPGraph.ot)$type)
## [1] "logical"
Type.ot<- as.logical(V(BPGraph.ot)$type)
VClass = ifelse(Type.ot, "Receiver", "Fish")
V(BPGraph.ot)$VClass = VClass
V(BPGraph.ot)$type = Type.ot
list.vertex.attributes(BPGraph.ot)
## [1] "name"   "degree" "type"   "VClass"
V(BPGraph.ot)$VClass
# Save long names
V(BPGraph.ot)$longName = V(BPGraph.ot)$name
# Abbreviate names to make graph more readable
# Fish List
V(BPGraph.ot)$name[V(BPGraph.ot)$VClass=="Fish"]
# Abbreviate fish names
#AbbFish = c("C2", "C5", "P1", paste("B", 1:244, sep=""))
#length(AbbFish)
#V(BPGraph.ot)$name[V(BPGraph.ot)$VClass=="Fish"] = AbbFish

# Receiver list
V(BPGraph.ot)$name[V(BPGraph.ot)$VClass=="Receiver"]
# Abbreviate receiver names
#AbbRecs = c("CDP1", paste("D", 1:2, sep=""), paste("D", 5:10, sep=""), paste("LP", 1:2, sep=""),
"PDM1", paste("PV", 1:2, sep=""), paste("V", 1:25, sep=""))
#length(AbbRecs)
#V(BPGraph.ot)$name[V(BPGraph.ot)$VClass=="Receiver"]= AbbRecs

# Make a simple bipartite plot
plot(BPGraph.ot, layout=layout.bipartite,
     vertex.color=c("green","cyan")[as.logical(V(BPGraph.ot)$type)+1])

# Figure out a scaling of degree that we can use for the size of a node (in points?)
range (5+degree(BPGraph.ot)/3) 
## [1]  6.00 15.33
V(BPGraph.ot)$size = 5+degree(BPGraph.ot)/100
V(BPGraph.ot)$size

# Figure out edge width
range ( 1+(BPEdges.ot$weight)/10000)
## [1]  1.00 11.27
E(BPGraph.ot)$size = 1+(BPEdges.ot$weight)/10000
range(E(BPGraph.ot)$size)
## [1]  1.00 11.27
# Try a log scale
range(log(E(BPGraph.ot)$weight+1)+1)
## [1]  1.693 11.846
E(BPGraph.ot)$logweight = log(E(BPGraph.ot)$weight+1)+1

# Plot BPGraph as an assymetric graph (use logweight for edges)
plot.igraph(BPGraph.ot,
            layout=layout.fruchterman.reingold, 
            vertex.label.color="black",
            vertex.label.cex = 1.25,
            edge.color="dark gray",
            edge.width=E(BPGraph.ot)$logweight,      
            vertex.label=V(BPGraph.ot)$name, 
            edge.arrow.size=.5,
            edge.curved=FALSE)

# Try colors by type
Type.ot = bipartite.mapping(BPGraph.ot)$type
V(BPGraph.ot)$color = ifelse(Type.ot, "green", "light blue")

# Figure 2A
plot.igraph(BPGraph.ot,
            layout=layout.fruchterman.reingold, 
            vertex.label.color="black",
            vertex.label.cex = 1.1,
            edge.color="dark gray",
            edge.width=E(BPGraph.ot)$logweight,      
            vertex.label=V(BPGraph.ot)$name, 
            edge.arrow.size=.4,
            edge.curved=FALSE,
            main="")



# Community detection returns an object of class "communities" 
#cop <- cluster_optimal(BPGraph.ot)
#clp<-cluster_label_prop(BPGraph.ot)
#cfg<-cluster_fast_greedy(BPGraph.ot)

cwg<-cluster_walktrap(BPGraph.ot)

cx<-cluster_walktrap(BPGraph.ot,steps=6)

summary(cwg)

cwg$membership
cx$modularity
cx$names
cx$vcount

cx$algorithm

View(cx)


plot(cx, BPGraph.ot,edge.arrow.size=0)
V(BPGraph.ot)$community <- cx$membership
plot(BPGraph.ot,edge.arrow.size=0,vertex.color=V(BPGraph.ot)$community)

#Make table of community structure
communiti_1 <- cx$names
communiti_1 <- as.data.frame(communiti_1)
communiti_1$rowID <- seq.int(nrow(communiti_1))
communiti_2 <- cx$membership
communiti_2 <- as.data.frame(communiti_2)
communiti_2$rowID <- seq.int(nrow(communiti_2))
community_structure <- merge(communiti_1, communiti_2, by = "rowID")
View(community_structure)
#print table
#write.csv(community_structure, "C:\\Rdata\\behavioural_repeatability\\export\\community_structure_hemnfjord_27.07.2019_6_steps.csv")
##########################################################################################
##########################################################################################




########################################################
#PCA Analysis
########################################################
str(pathogens_NOV21_0)
scaled_pathogens <- pathogens_NOV21_0[pathogens_NOV21_0$common_name=='Sea trout',] %>% group_by(set_location) %>% mutate(ascv = scale(ascv), c_b_cys=scale(c_b_cys), fl_psy=scale(fl_psy), ic_mul=scale(ic_mul),
                                                                                                                         IcD = scale(IcD), my_sp=scale(my_sp), pa_pse=scale(pa_pse), pa_ther=scale(pa_ther),
                                                                                                                         pch_sal = scale(pch_sal), pisck_sal=scale(pisck_sal), prv1=scale(prv1), prv3=scale(prv3),
                                                                                                                         sch = scale(sch), te_dic=scale(te_dic), te_fin=scale(te_fin), te_mar=scale(te_mar))

head(scaled_pathogens)
str(scaled_pathogens)

scaled_pathogens$set_location <- as.character(scaled_pathogens$set_location)
scaled_pathogens$set_location <- as.factor(scaled_pathogens$set_location)


scaled_pathogens <- scaled_pathogens[c(6, 9:23)]
scaled_pathogens <- scaled_pathogens[which (complete.cases(scaled_pathogens)), ]
str(scaled_pathogens)
head(scaled_pathogens)
scaled_pathogens$rowid <- seq.int(nrow(scaled_pathogens))

str(scaled_pathogens)
pca_scaled_pathogens <- princomp(scaled_pathogens[3:16])
summary(pca_scaled_pathogens)


pca_scores_all <- pca_scaled_pathogens$scores
pca_scores_all <- as.data.frame(pca_scores_all)
pca_scores_all$rowid <- seq.int(nrow(pca_scores_all))

scaled_pathogens <- merge(scaled_pathogens, pca_scores_all[c("Comp.1","Comp.2", "rowid")], by = "rowid")
str(scaled_pathogens)
colnames(scaled_pathogens)[18] <- "PCA_all_C1"
colnames(scaled_pathogens)[19] <- "PCA_all_C2"

#main_table <- read.csv("./data/modified/main_table.csv")
#main_table <- merge(main_table, scaled_physiology_all[c("fishID", "PCA_all_C1", "PCA_all_C2")], by = "fishID", all.x = TRUE)

pca_scaled_pathogens$contrib
library(factoextra)
var <- get_pca_var(pca_scaled_pathogens)
head(var$coord)
var$contrib

pca_scaled_pathogens
fviz_eig(pca_scaled_pathogens, geom = "bar", bar_width = 0.4) + ggtitle("")
fviz_pca_biplot(pca_scaled_pathogens, label = "var", habillage = scaled_pathogens$set_location, addEllipses=TRUE, ellipse.level=0.95)
fviz_pca_biplot(pca_scaled_physiology_all, label = "var", habillage = scaled_physiology_all$sex, addEllipses=TRUE, ellipse.level=0.95)
#ggsave("PCA_physiology_07.01.2020.tiff", units="cm", width=20, height=24, dpi=600, compression = 'lzw')





###########################################################################
#Look at fitchip data
###########################################################################
Sasa_log2_SasaAQ_1 <- read.csv("./data/raw_data/4089_4090_Sasa_log2_SasaAQ_1.csv", sep = ";")
str(Sasa_log2_SasaAQ_1)

Satr_log2_SasaAQ_1 <- read.csv("./data/raw_data/4089_4090_Satr_log2_SasaAQ_1.csv", sep = ";")
str(Sasa_log2_SasaAQ_1)

########################################################
#PCA Analysis
########################################################
str(pathogens_NOV21_0)
str(Sasa_log2_SasaAQ_1)
fitchip_1 <- Sasa_log2_SasaAQ_1[c(3,4,20,47:53,114:122)]
str(fitchip_1)
fitchip_2 <- Satr_log2_SasaAQ_1[c(3,4,20,47:53,114:122)]
str(fitchip_2)
fitchip <- rbind(fitchip_1, fitchip_2)


fitchip$common_name <- as.factor(fitchip$common_name)
fitchip$set_location <- as.factor(fitchip$set_location)
summary(fitchip$common_name)
summary(fitchip$set_location)
fitchip$common_name[which (fitchip$common_name=='Brown trout')] <- 'Sea trout'
fitchip$common_name <- as.character(fitchip$common_name)
fitchip$set_location <- as.character(fitchip$set_location)
fitchip$common_name <- as.factor(fitchip$common_name)
fitchip$set_location <- as.factor(fitchip$set_location)



fitchip <- fitchip[which (fitchip$set_location=='Beiarfjorden'|fitchip$set_location=='Bolstadfjorden'|fitchip$set_location=='Stjordal'), ]


fitchip %>% as_tibble %>% 
  mutate(i=c(1:nrow(.))) %>% 
  gather(key, value, -common_name, -set_location, -i) %>% 
  ggplot(aes(i, key, fill=value))+
  geom_tile()+
  scale_fill_gradientn(colours=c("royalblue", "white", "red4"))+
  facet_wrap(~paste(set_location, common_name), scales="free")+
  coord_flip()

fitchip %>% 
  dplyr::select(39:ncol(.)) %>% 
  dplyr::select(-VDD_RSAD, -IM_H1F0) %>% 
  as_tibble %>% 
  kmeans(., 7) %>% 
  purrr::pluck(1) %>% 
  as_tibble %>% 
  bind_cols(fitchip %>%  dplyr::select(-VDD_RSAD)) %>% 
  dplyr::select(40:ncol(.), value, common_name, set_location, alternate_num) %>% 
  gather(key, val, -value, -common_name, -set_location, -alternate_num) %>% 
  as_tibble %>% 
  ggplot(aes(paste(alternate_num, set_location, common_name), key, fill=val))+
  geom_tile()+
  scale_fill_viridis_c(option="plasma")+
  facet_wrap(~value, scales="free")+
  coord_flip()

fitchip <- fitchip[which (fitchip$common_name!=''), ]
#GENE VDD_RSAD IS MISSING IN ONE OF THE DATASETS
fitchip <- fitchip[c(1:8, 10:11)]

fitchip <- fitchip[which (complete.cases(fitchip)), ]
fitchip$rowid <- seq.int(nrow(fitchip))

str(fitchip)

str(fitchip)
pca_fitchip <- princomp(fitchip[c(3:10)])
str(pca_fitchip)
summary(pca_fitchip)



pca_scores_all <- pca_fitchip$scores
pca_scores_all <- as.data.frame(pca_scores_all)
pca_scores_all$rowid <- seq.int(nrow(pca_scores_all))
str(pca_scores_all)
pca_scores_all$group <- interaction(pca_scores_all$common_name, pca_scores_all$set_location)

pca_scores_all <- merge(fitchip, pca_scores_all[c("Comp.1","Comp.2", "rowid")], by = "rowid")
#colnames(scaled_pathogens)[18] <- "PCA_all_C1"
#colnames(scaled_pathogens)[19] <- "PCA_all_C2"

#main_table <- read.csv("./data/modified/main_table.csv")
#main_table <- merge(main_table, scaled_physiology_all[c("fishID", "PCA_all_C1", "PCA_all_C2")], by = "fishID", all.x = TRUE)

library(factoextra)
var <- get_pca_var(pca_fitchip)
head(var$coord)
contrib <- var$contrib

fitchip
fviz_eig(c, geom = "col", bar_width = 0.4) + ggtitle("")
fviz_pca_biplot(pca_fitchip, label = "var", habillage = pca_scores_all$group, addEllipses=TRUE, ellipse.level=0.95)
fviz_pca_biplot(pca_fitchip, label = "var", habillage = pca_scores_all$common_name, addEllipses=TRUE, ellipse.level=0.95)
#ggsave("PCA_physiology_07.01.2020.tiff", units="cm", width=20, height=24, dpi=600, compression = 'lzw')

