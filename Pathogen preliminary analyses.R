#Peliminary analyses Pathogen data received 04.12.2021

###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################

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


URL_4089_4090_Sasa_log2_SasaAQ_1 <- "https://ntnu.box.com/shared/static/wycb8ibkk58k2nb2mvyey9jr7rir6m3o.csv" #Uploaded new version 18.12.20
download.file(url=URL_4089_4090_Sasa_log2_SasaAQ_1,destfile="./data/raw_data/4089_4090_Sasa_log2_SasaAQ_1.csv")  

URL_4089_4090_Satr_log2_SasaAQ_1 <- "https://ntnu.box.com/shared/static/5eax7kkduvt606h9bzmgpwr30oldk6d3.csv" #Uploaded new version 18.12.20
download.file(url=URL_4089_4090_Satr_log2_SasaAQ_1,destfile="./data/raw_data/4089_4090_Satr_log2_SasaAQ_1.csv")  
###########################################################


library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

#Read pathogen list
pathogen_list <- read.csv("./data/raw_data/pathogen_list.csv", sep = ";")
str(pathogen_list)
summary(as.factor(pathogen_list$agent_name))

pathogens_NOV21 <- read.csv("./data/raw_data/pathogens_NOV21.csv", sep = ";")
str(pathogens_NOV21)
summary(pathogens_NOV21)

pathogens_NOV21$common_name[pathogens_NOV21$common_name=='Brown trout'] <- 'Sea trout'
pathogens_NOV21$common_name[pathogens_NOV21$common_name=='Atlantic'] <- 'Atlantic Salmon'

pathogens_NOV21$hkg_alert <- as.factor(pathogens_NOV21$hkg_alert)
pathogens_NOV21$common_name <- as.factor(pathogens_NOV21$common_name)
pathogens_NOV21$dna_id <- as.factor(pathogens_NOV21$dna_id)
pathogens_NOV21$hatchery_wild <- as.factor(pathogens_NOV21$hatchery_wild)
pathogens_NOV21$set_location <- as.factor(pathogens_NOV21$set_location)
pathogens_NOV21$station <- as.factor(pathogens_NOV21$station)
pathogens_NOV21$Transmitter.ID <- as.factor(pathogens_NOV21$Transmitter.ID)
pathogens_NOV21$pathogen <- as.factor(pathogens_NOV21$pathogen)
pathogens_NOV21$measurement <- as.numeric(pathogens_NOV21$measurement)

pathogens_NOV21 <- pathogens_NOV21[c("hkg_alert", "common_name", "dna_id", "hatchery_wild", "fork_length..mm.", "set_location", "station", "Transmitter.ID", 
                                     "ascv", "c_b_cys", "fl_psy", "ic_mul", "IcD", "my_sp", "pa_pse", "pa_ther", "pch_sal", "pisck_sal", "prv1", "prv3", "sch", "te_dic", "te_fin",
                                     "te_mar")]
pathogens_NOV21_0 <- pathogens_NOV21

count_groups <- pathogens_NOV21 %>%
  group_by(set_location, common_name) %>%
  dplyr::summarise(pathogen_precence = n())


pathogens_NOV21 <- gather(pathogens_NOV21, pathogen, measurement, ascv:te_mar, factor_key=TRUE)

summary(pathogens_NOV21$common_name)

str(pathogens_NOV21)

pathogens_NOV21 <- pathogens_NOV21[which (!is.na(pathogens_NOV21$measurement)), ]
pathogens_NOV21 <- pathogens_NOV21[which (pathogens_NOV21$measurement!=0), ]




###########################################################################
#Plotting proportional detected prevalence
###########################################################################
tmp <- dplyr::select(pathogens_NOV21, common_name, set_location, hatchery_wild, pathogen, measurement)

tmp <- pathogens_NOV21 %>%
  group_by(set_location, common_name, pathogen) %>%
  dplyr::summarise(pathogen_precence = n())

str(tmp)
str(pathogen_list)
tmp <- merge(tmp, pathogen_list, by.x = "pathogen", by.y= "assay_name", all.x = TRUE, all.y = FALSE)
tmp$prop_positive[tmp$set_location=='Beiarfjorden'& tmp$common_name=='Sea trout'] <- tmp$pathogen_precence[tmp$set_location=='Beiarfjorden'& tmp$common_name=='Sea trout']/50
tmp$prop_positive[tmp$set_location=='Bolstadfjorden'& tmp$common_name=='Sea trout'] <- tmp$pathogen_precence[tmp$set_location=='Bolstadfjorden'& tmp$common_name=='Sea trout']/32
tmp$prop_positive[tmp$set_location=='Bolstadfjorden'& tmp$common_name=='Atlantic Salmon'] <- tmp$pathogen_precence[tmp$set_location=='Bolstadfjorden'& tmp$common_name=='Atlantic Salmon']/37
tmp$prop_positive[tmp$set_location=='Stjordal'& tmp$common_name=='Atlantic Salmon'] <- tmp$pathogen_precence[tmp$set_location=='Stjordal'& tmp$common_name=='Atlantic Salmon']/3
tmp$prop_positive[tmp$set_location=='Stjordal'& tmp$common_name=='Sea trout'] <- tmp$pathogen_precence[tmp$set_location=='Stjordal'& tmp$common_name=='Sea trout']/35
count_groups


summary(tmp$set_location)

trout <- tmp[tmp$common_name=='Sea trout', ]
salmon <- tmp[tmp$common_name=='Atlantic Salmon', ]

#Sea trout
p1 <- ggplot(trout, aes(y = agent_name, x = prop_positive*100, fill=set_location))+ theme_classic(base_size = 18) + geom_col(position = position_dodge(width = 0.9)) + theme(axis.text.x=element_text(angle=90, hjust=1))+ ggtitle("Pathogen prevalence") +xlab("Proportion prevalence (%)")+ ylab(element_blank())
p1
ggsave("./data/modified_data/pathogen_prevalence_trout_NOV-21.tiff", p1, units="cm", width=30, height=30, dpi=300, compression = 'lzw')

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

