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
fitchip_data <- "https://ntnu.box.com/shared/static/pyjppqevterjfhsd4be4zapqssmbh97o.csv" #Uploaded new version 18.12.20
download.file(url=fitchip_data,destfile="./data/raw_data/fitchip_data_kristi_prelim_analyses_0122.csv")  

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



#Remove outliers from the dataset
fitchip_data$gill_id <- str_replace_all(fitchip_data$gill_id, 'Satr_', '')
fitchip_data <- fitchip_data[fitchip_data$gill_id!='S05', ]
fitchip_data <- fitchip_data[fitchip_data$gill_id!='BF1003', ]

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


thermal_stress <- fitchip_long[fitchip_long$fitchip_panel=='TM', ]


#Make inital plots of thermal genes
str(thermal_stress)
thermal_stress$gene_marker <- as.factor(termal_stress$gene_marker)
library(ggplot2)
a <- ggplot(thermal_stress[thermal_stress$Temperature!='',], aes(x=gene_marker, y=measurement, fill=Temperature)) + geom_boxplot()+ theme_classic(base_size = 18)+ theme(legend.position = "top") + ggtitle("Gen expression thermal stress indicators") + labs(fill ="Temperature")+ xlab("Gene") 
a



boxplot(thermal_stress$measurement~thermal_stress$gene_marker)


###########################################################################
#Look at fitchip data
###########################################################################
#Data files located on NTNU BOX - Data for R - PACE - WP2
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

