###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################
options(scipen=999)
#test


#########################################################################
#download Pathogen data
#########################################################################

#Data files located on NTNU BOX - Data for R - PACE - WP2
#fitchip_data <- "https://ntnu.box.com/shared/static/pyjppqevterjfhsd4be4zapqssmbh97o.csv" #Uploaded new version 18.12.20
#download.file(url=fitchip_data,destfile="./data/raw_data/fitchip_data_kristi_prelim_analyses_0122.csv")  

###########################################################

#NOTE
#Satr_S05  and BF1003 were outliers and removed from all analysis
#IF44a was a pretty strong outlier and removed for general PCA analysis

##I have analyzed this dataset several times, and in this analysis I removed all assays with measured efficiencies not between 0.8-1.2.  This brought us down to 43 assays and impacted our ability to recognize osmotic variation--i.e. FW vs SW fish. 

#I generally take three approaches to the data
#General PCAs over all markers to see if any obvious known signatures come out in the data
#With Pacific salmon, genearally the imminent mortality signature will pull out right away if it is there.  It does not appear to be there in your data
#What I have seen obviously with this approach is the thermal stress signature. See the tab general PCA 

#I also analyze the biomarker panels of interest themselves to see what pulls out. I look to see if they are correlated, which they should be if present.  
#Ideally this analysis would also utilize the control samples, but they were too out of range to work in this case
#This analysis also pulled up the thermal stress signature (See Thermal Signature development 1
#But really little else.  Note, I only showed the tabs for analyses that I thought were compelling
#I also used the annotation data to try to pull out signatures associated with measured variables. 



###########################################################################
#Look at fitchip data
###########################################################################
#Data files located on NTNU BOX - Data for R - PACE - WP2
fitchip_data <- read.csv("./data/raw_data/fitchip_data_kristi_prelim_analyses_0122.csv", sep = ";")
str(fitchip_data)

fitchip_data$gill_id <- fitchip_data$fit_chip_id

library(stringr)
fitchip_data$gill_id <- str_replace_all(fitchip_data$gill_id, 'Satr_', '')

metadata <-  read.csv("./data/modified_data/summary_table_metadata_PACE_WP2_270122.csv", sep = ",")

str(fitchip_data)

#Merge fitchip_data and metadata
str(fitchip_data)
str(metadata)
fitchip_data <- merge(fitchip_data, metadata, by.x = "gill_id", by.y = "dna_id", all.x = TRUE)
str(fitchip_data)
tmp <- fitchip_data[c("gill_id", "common_name", "set_location", "Temperature", "tagging_temp", "avg_temp","max_temp")]
summary(tmp)
#Consider to revisit the temperature in order to decrease the numbers of NA values

str(fitchip_data)
summary(fitchip_data)


#Make a summary table providing the highest CT values for each fish
pathogen_prevalence <- read.csv("./data/modified_data/pathogen_prevalence_NOV21.csv")
str(pathogen_prevalence)
library(dplyr)
pathogen_prevalence$fluidigm_num <- as.factor(pathogen_prevalence$fluidigm_num)
pathogen_prevalence$alternate_num <- as.factor(pathogen_prevalence$alternate_num)
pathogen_prevalence$pathogen
#Number with atleast one pathogen - SEA TROUT
ct_values <- pathogen_prevalence[pathogen_prevalence$common_name=='Sea trout',] %>%
  group_by(alternate_num)%>%
  summarise(max_ct_value = max(measurement))

hist(ct_values$max_ct_value)
fitchip_data <- merge(fitchip_data, ct_values, by = "alternate_num", all.x = TRUE)


#The max_ct_value is categorized into three: Low: (< 11C), Moderate (11C-13C) and High (<13C)
fitchip_data$ct_level <-''

fitchip_data$ct_level[fitchip_data$max_ct_value<20] <- 'Low' 
fitchip_data$ct_level[fitchip_data$max_ct_value>20&fitchip_data$max_ct_value<25] <- 'Moderate' 
fitchip_data$ct_level[fitchip_data$max_ct_value>25] <- 'High' 
fitchip_data <- fitchip_data[!is.na(fitchip_data$max_ct_value),]
summary(as.factor(fitchip_data$ct_level))

#Remove outliers from the dataset
fitchip_data$gill_id <- str_replace_all(fitchip_data$gill_id, 'Satr_', '')
fitchip_data <- fitchip_data[fitchip_data$gill_id!='S05', ]
fitchip_data <- fitchip_data[fitchip_data$gill_id!='BF1003', ]
#Remove BF42 and BF42_2
fitchip_data <- fitchip_data[fitchip_data$gill_id!='BF42', ]
fitchip_data <- fitchip_data[fitchip_data$gill_id!='BF42_2', ]


library(tidyr)
names(fitchip_data)
fitchip_long <- gather(fitchip_data, gene_marker, measurement, CL_H2EB1_672,CL_ICLP2_674,CL_PSMB7_686,IF_ES1_668,IF_txn_683,      
                       IM_ARRDC2_663,IM_EPD_667,IM_GLUL_670,IM_NUPR1_677,IM_ODC1_678,IM_TAGLN3_681,   
                       IM_napepld_676,IM_tgfb_682,IS_B2M_182,IS_C5aR_577,IS_CD83_579,IS_IL1B_295,     
                       IS_RIG1_361,MRS_ATP5G3_181,MRS_C7_189,MRS_FYB_241,MRS_HTATIP_272,MRS_NKAB2_328,   
                       OS_CCL4_195,OS_CFTR_I_206,OS_HBA_254,OS_NDUFB2_322,OS_UBA1_605,TM_FKBP10_4_583, 
                       TM_Hsp90a_15_269,TM_HSP90a_6_271,TM_SERPIN20_379,TM_SERPIN_9_380,TM_HSP70_267,VDD_HERC6_77,   
                       VDD_IFI44A_81,VDD_IFIT5_2_83,VDD_MX_86,VDD_NFX_87)

str(fitchip_long)

fitchip_long$fitchip_panel <- substr(fitchip_long$gene_marker, start = 1, stop = 2)

fitchip_long$measurement <- str_replace_all(fitchip_long$measurement, ',', '.')
fitchip_long$measurement <- as.numeric(fitchip_long$measurement)

summary(as.factor(fitchip_long$fitchip_panel))
fitchip_long$fitchip_panel[fitchip_long$fitchip_panel=='MR'] <- 'MRS'
fitchip_long$fitchip_panel[fitchip_long$fitchip_panel=='VD'] <- 'VDD'


viral_disease <- fitchip_long#[fitchip_long$fitchip_panel=='TM' & fitchip_long$Temperature!='', ]


#Make inital plots of thermal genes
str(viral_disease)
viral_disease$gene_marker <- as.factor(viral_disease$gene_marker)
library(ggplot2)
library(tidyverse)
str(viral_disease)
viral_disease <- group_by(viral_disease, gene_marker) %>% mutate(scaled_measurement = as.numeric(scale(measurement)))
str(viral_disease)

b <- ggplot(viral_disease[viral_disease$ct_level!='',], aes(x=gene_marker, y=scaled_measurement, fill=ct_level)) + geom_boxplot()+ theme_classic(base_size = 18)+ theme(legend.position = "top") + ggtitle("Gen expression thermal stress indicators") + labs(fill ="Temperature")+ xlab("Gene")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
b

# Load required R packages
library(tidyverse)
library(rstatix)
library(ggpubr)

#Do subset using genes suggested by Kristi in pane "Thermal stress 2"

summary(as.factor(fitchip_long$gene_marker), maxsum = 999)

viral_disease <- fitchip_long[(fitchip_long$gene_marker=='VDD_HERC6_77'
                               |fitchip_long$gene_marker=='VDD_IFI44A_81'
                               |fitchip_long$gene_marker=='VDD_IFIT5_2_83'
                               |fitchip_long$gene_marker=='VDD_MX_86'
                               |fitchip_long$gene_marker=='VDD_NFX_87'), ]


ge_data <- viral_disease

########################################################
#PCA Analysis
########################################################

str(ge_data)
viral_disease_wide <- viral_disease[c("fit_chip_id", "ct_level", "gill_id", "gene_marker", "measurement")]
viral_disease_wide <- viral_disease_wide[which (complete.cases(viral_disease_wide)), ]
viral_disease_wide <- spread(viral_disease_wide, gene_marker, measurement)
str(viral_disease_wide)

#GENE VDD_RSAD IS MISSING IN ONE OF THE DATASETS
#str(thermal_stress)
#str(fitchip_data)
viral_pca <- viral_disease_wide[c("gill_id", "ct_level", "VDD_HERC6_77","VDD_IFI44A_81","VDD_IFIT5_2_83","VDD_MX_86","VDD_NFX_87")]
#thermal_pca <- str_replace_all(thermal_pca[c(3:9)], ',', '.')


viral_pca <- viral_pca[which (complete.cases(viral_pca)), ]
viral_pca$rowid <- seq.int(nrow(viral_pca))
str(viral_pca)

str(viral_pca)
pca_fitchip <- princomp(viral_pca[c(3:7)])



str(pca_fitchip)
summary(pca_fitchip)



pca_scores_all <- pca_fitchip$scores
pca_scores_all <- as.data.frame(pca_scores_all)
pca_scores_all$rowid <- seq.int(nrow(pca_scores_all))
str(pca_scores_all)

pca_scores_all <- merge(viral_pca, pca_scores_all[c("Comp.1","Comp.2", "rowid")], by = "rowid")
#colnames(scaled_pathogens)[18] <- "PCA_all_C1"
#colnames(scaled_pathogens)[19] <- "PCA_all_C2"

#main_table <- read.csv("./data/modified/main_table.csv")
#main_table <- merge(main_table, scaled_physiology_all[c("fishID", "PCA_all_C1", "PCA_all_C2")], by = "fishID", all.x = TRUE)

library(factoextra)
var <- get_pca_var(pca_fitchip)
head(var$coord)
contrib <- var$contrib

pca_fitchip
str(pca_fitchip)
str(pca_scores_all)
fviz_pca_biplot(pca_fitchip, label = "var", habillage = pca_scores_all$ct_level, addEllipses=FALSE, ellipse.level=0.95)
#ggsave("PCA_physiology_07.01.2020.tiff", units="cm", width=20, height=24, dpi=600, compression = 'lzw')


c <- ggplot(pca_scores_all, aes(x=Comp.1, y=Comp.2, col=ct_level)) + geom_point()+ theme_classic(base_size = 18)+ theme(legend.position = "top") + ggtitle("Gen expression thermal stress indicators") + labs(fill ="Temperature")+ xlab("Gene") 
c

d <- ggplot(pca_scores_all, aes(x=ct_level, y=Comp.1, col=ct_level)) + geom_boxplot()+ theme_classic(base_size = 18)+ theme(legend.position = "top") + ggtitle("Gen expression thermal stress indicators") + labs(fill ="Temperature")+ xlab("Gene") 
d

e <- ggplot(pca_scores_all, aes(x=ct_level, y=Comp.2, col=ct_level)) + geom_boxplot()+ theme_classic(base_size = 18)+ theme(legend.position = "top") + ggtitle("Gen expression thermal stress indicators") + labs(fill ="Temperature")+ xlab("Gene") 
e


str(pca_scores_all)

#Lets bring in the real ct values to look at the point where fish get a genetic response
str(viral_disease)
str(pca_scores_all)
#pca_scores_all <- merge(pca_scores_all, viral_disease[c("max_temp", "fork_length..mm.","max_ct_value", "gill_id", "set_location")], by.x ="gill_id", by.y="gill_id", all.x =  TRUE, all.y = FALSE)
pca_scores_all <- merge(pca_scores_all, viral_disease, by.x ="gill_id", by.y="gill_id", all.x =  TRUE, all.y = FALSE)
str(pca_scores_all)
pca_scores_all$fork_length <- pca_scores_all$fork_length..mm.

mean_comp1 <- mean(pca_scores_all$Comp.1)
sd_comp1 <- sd(pca_scores_all$Comp.1)
mean_comp1+sd_comp1
mean_comp1-sd_comp1
str(pca_scores_all)

f <- ggplot(pca_scores_all, aes(x=max_ct_value, y=Comp.1)) + geom_point()+ geom_smooth(method = "lm")+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("Gene expression for pathogen detection") + 
  xlab("CT-valye (inverse)")+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted") 
f



f <- ggplot(pca_scores_all, aes(x=max_ct_value, y=Comp.1, col = set_location)) + geom_point()+ geom_smooth()+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("Gene expression for thermal stress") + 
  xlab("Temperature (C)")+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted") 
f

str(pca_scores_all)

f <- ggplot(pca_scores_all, aes(x=fork_length, y=Comp.1, col = set_location)) + geom_point()+ geom_smooth(method="lm")+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("Gene expression for thermal stress") + 
  xlab("fork_length (mm)")+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted") 
f





f <- ggplot(pca_scores_all, aes(x=max_ct_value, y=Comp.2)) + geom_point()+ geom_smooth(method = "lm")+ theme_classic(base_size = 18)+ theme(legend.position = "top") + ggtitle("Gen expression thermal stress indicators") + labs(fill ="Temperature")+ xlab("Gene") 
f

str(pca_scores_all)
tmp <- gather(pca_scores_all, Comp., PCA_value, VDD_comp1, VDD_comp2)

f <- ggplot(tmp, aes(x=max_ct_value, y=PCA_value, col=Comp.)) + geom_point()+ geom_smooth(method = "lm")+ theme_classic(base_size = 18)+ theme(legend.position = "top") + ggtitle("Viral disease development") 
f

str(pca_scores_all)
pca_scores_all$VDD_comp1 <- pca_scores_all$Comp.1
pca_scores_all$VDD_comp2 <- pca_scores_all$Comp.2

fitchip_wide <- fitchip_long[c("fit_chip_id", "max_ct_value", "gill_id", "gene_marker", "measurement")]
#fitchip_wide <- fitchip_wide[which (complete.cases(fitchip_long)), ]
fitchip_wide <- spread(fitchip_wide, gene_marker, measurement)

str(fitchip_wide)

str(pca_scores_all)
pca_scores_all <- pca_scores_all[!duplicated(pca_scores_all$gill_id),]

#
saveRDS(pca_scores_all, "./data/modified_data/fever_summary_table_kristi.RDS")

export_file <- pca_scores_all[c("gill_id", "VDD_comp1", "VDD_comp2")]
export_file <- export_file[!duplicated(export_file),]

#export_file <- merge(fitchip_wide, pca_scores_all[c("gill_id", "VDD_comp1", "VDD_comp2")], by = "gill_id")
#str(export_file)

write.csv(export_file, "./data/modified_data/VDD_expression.csv", row.names = FALSE)
