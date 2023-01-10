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

metadata <-  read.csv("./data/modified_data/summary_table_metadata_PACE_WP2_270122.csv", sep = ",")

#Merge fitchip_data and metadata
str(fitchip_data)
str(metadata)
fitchip_data <- merge(fitchip_data, metadata, by.x = "gill_id", by.y = "dna_id", all.x = TRUE)
str(fitchip_data)
tmp <- fitchip_data[c("gill_id", "common_name", "set_location", "Temperature", "tagging_temp", "avg_temp","max_temp")]
summary(tmp)
#Consider to revisit the temperature in order to decrease the numbers of NA values

#The temperature is categorized into three: Low: (< 11C), Moderate (11C-13C) and High (<13C)
fitchip_data$temperature[fitchip_data$max_temp>5&fitchip_data$max_temp<11.5] <- 'Low' 
fitchip_data$temperature[fitchip_data$max_temp>11.5&fitchip_data$max_temp<13] <- 'Moderate' 
fitchip_data$temperature[fitchip_data$max_temp>13] <- 'High' 
summary(as.factor(fitchip_data$temperature))


#Remove outliers from the dataset
fitchip_data$gill_id <- str_replace_all(fitchip_data$gill_id, 'Satr_', '')
fitchip_data <- fitchip_data[fitchip_data$gill_id!='S05', ]
fitchip_data <- fitchip_data[fitchip_data$gill_id!='BF1003', ]
#Remove BF42 and BF42_2
fitchip_data <- fitchip_data[fitchip_data$gill_id!='BF42', ]
fitchip_data <- fitchip_data[fitchip_data$gill_id!='BF42_2', ]

#Correct the temperature categories and rerun analyses
fitchip_data$Temperature <- fitchip_data$temperature



#######################################################################
#function to export PCA plots and data for each panel
fitchip_long <- readRDS("./data/modified_data/fitchip_long.rds")
str(fitchip_long)
summary(as.factor((fitchip_long$gene_marker)))
ID_list <- sort(unique(fitchip_long$fitchip_panel))
panel <- list()
temp_panel <-list()
pcapanel <- list()
pca_fitchip <- list()
pca_scores_all <- list()
var <- list()
contrib <- list()
fviz_pca_biplot <-list()
pa <- list()
pb <- list()
pc <- list()
for (i in seq_along(ID_list)) {
#  panel[[i]] <- fitchip_long[fitchip_long$fitchip_panel==ID_list[i],]
########################################################
#PCA Analysis
########################################################
 # thermal_stress <- fitchip_long[fitchip_long$fitchip_panel=="TM",]
  thermal_stress <- fitchip_long[fitchip_long$fitchip_panel==ID_list[i],]
  ########################################################
  #PCA Analysis
  ########################################################
  
  str(thermal_stress)
  str(thermal_stress)
  
  str(thermal_stress)
  thermal_wide <- thermal_stress[c("fit_chip_id", "Temperature", "gill_id", "gene_marker", "measurement")]
  thermal_wide <- thermal_wide[which (complete.cases(thermal_wide)), ]
  thermal_wide <- spread(thermal_wide, gene_marker, measurement)
  
  
  #GENE VDD_RSAD IS MISSING IN ONE OF THE DATASETS
  #str(thermal_stress)
  str(thermal_wide)
  
  #thermal_pca <- thermal_wide[c("gill_id", "Temperature", "TM_Hsp90a_15_269","TM_SERPIN20_379","TM_HSP70_267","VDD_HERC6_77","IM_ARRDC2_663")]
  thermal_pca <- thermal_wide[-c(1)]
  #thermal_pca <- str_replace_all(thermal_pca[c(3:9)], ',', '.')
  str(thermal_pca)
  
  thermal_pca <- thermal_pca[which (complete.cases(thermal_pca)), ]
  
  str(thermal_pca)
  
  str(thermal_pca)
  pca_fitchip <- princomp(thermal_pca[-c(1:2)])
  str(pca_fitchip)
  str(pca_fitchip)
  summary(pca_fitchip)
  thermal_pca$rowid <- seq.int(nrow(thermal_pca))
  
  
  pca_scores_all <- pca_fitchip$scores
  pca_scores_all <- as.data.frame(pca_scores_all)
  pca_scores_all$rowid <- seq.int(nrow(pca_scores_all))
  str(pca_scores_all)
  
  pca_scores_all <- merge(thermal_pca, pca_scores_all[c("Comp.1","Comp.2", "rowid")], by = "rowid")

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
  fviz_pca_biplot <- fviz_pca_biplot(pca_fitchip, label = "var", habillage = pca_scores_all$Temperature, addEllipses=FALSE, ellipse.level=0.95)

  pca_scores_all <- merge(pca_scores_all, metadata[c("transmitterID","max_temp", "fork_length..mm.", "dna_id", "set_location")], by.x ="gill_id", by.y="dna_id")
  str(pca_scores_all)
  pca_output <- pca_scores_all[c("transmitterID", "Comp.1", "Comp.2")]

  names(pca_output) <- c("transmitterID", paste0("comp1_", as.character(ID_list[i])), paste0("comp_", as.character(ID_list[i])))
  pa[[i]] <- pca_output[c(1:2)]
  pb[[i]] <- fviz_pca_biplot
  pc[[i]] <- pca_output
  ggsave(pb[[i]], file=paste0("./data/modified_data/PCA_gene_expression_",  as.character(ID_list[i]),".tiff"), units="cm", width=20, height=20, dpi=200, compression = 'lzw', limitsize = FALSE)
  
  }


pca_export <- pa %>% reduce(full_join, by = "transmitterID")
str(pca_export)
saveRDS(pca_export, "./data/modified_data/gene_expression_pca_export.rds")


panel_pca <- pca_export[c(2:9)]

#panel_pca <- scale(panel_pca)

# check that we get mean of 0 and sd of 1
colMeans(panel_pca)  # faster version of apply(scaled.dat, 2, mean)
apply(panel_pca, 2, sd)

str(panel_pca)

panel_pca <- princomp(panel_pca)
library(factoextra)
var <- get_pca_var(panel_pca)
head(var$coord)
contrib <- var$contrib

panel_pca
str(pca_fitchip)
fviz_pca_biplot <- fviz_pca_biplot(panel_pca, label = "var", addEllipses=FALSE, ellipse.level=0.95)
fviz_pca_biplot


str(pca_export)
f <- ggplot(pca_export, aes(x=comp1_TM, y=comp1_VDD)) + geom_point()+ geom_smooth(method="lm")+ theme_classic(base_size = 18)+ 
   ggtitle("Gene expression for thermal stress") + 
  xlab("Temperature (C)") 
f
ggsave("./data/modified_data/PCA_thermal_stress.tiff", fviz_pca_biplot, units="cm", width=20, height=15, dpi=600, compression = 'lzw')
