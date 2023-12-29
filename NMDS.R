#NMDS PLOT

###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################

library(ggplot2)
library(lubridate)
library(dplyr)
library(vegan)
library(MASS)
dat <- readRDS("./data/modified_data/summary_table_191223.RDS")

summary(dat)
dat <- dat[dat$tot_rib>0, ]
str(dat)

com = dat[c(5:13, 15, 17)]
com <- as.data.frame(com)
com %>% mutate(sumrow = rowSums(.))
str(com)
com <- com %>%
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(across(where(is.numeric))))
?rowSums
env = dat[c(1:4, 18:22)]

names(com)
names(env)

#Import gene markers for VDD and thermal stress
thermal_stress <- readRDS("./data/modified_data/thermal_stress_211223.RDS")
env <- merge(env, thermal_stress, by = "vial", all.x = TRUE)

VDD <- readRDS("./data/modified_data/VDD_211223.RDS")
env <- merge(env, VDD[c("vial", "Viral disease development")], by = "vial", all.x = TRUE)

names(env)

#convert com to a matrix
m_com = as.matrix(com)

#nmds code
set.seed(123)
nmds = metaMDS(com)
nmds

?metaMDS


nmds <- metaMDS(com, maxit = 100, permutations = 999, k = 2, autotransform = FALSE, engine = "isoMDS")
summary(m_com)

#data.scores <- as.data.frame(scores(nmms, "sites"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
#data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
#data.scores <- cbind(data.scores, positive_fish)  #  add the grp variable created earlier
#head(data.scores)  #look at the data

scores_sites <-scores(nmds, "sites")
scores_sites <- as.data.frame(scores_sites)
scores_sites <- cbind(env, scores_sites)
head(scores_sites)
scores_sites$Fjord <- scores_sites$fjord

scores_species <- scores(nmds, "species")
names(com)
#scores_species$species <- list(names(com)) 
#scores_species <- as.data.frame(scores_species)
#head(scores_species)

species.scores <- as.data.frame(scores(nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

#plot ordination species
 p1 <- ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(
    data=scores_sites, 
    aes(
      x=NMDS1, 
      y=NMDS2,
      colour=Fjord), 
    size=3,
    alpha = 0.6)+
  theme_classic(base_size = 18)+
  ylab("NMDS2")+
  xlab("NMDS1")+
  stat_ellipse(data=scores_sites, aes(x=NMDS1, y=NMDS2,color=Fjord, group=Fjord),type = "norm", level = 0.5)#+
  #coord_fixed() + ## need aspect ratio of 1!
  #geom_segment(data = en,
  #             aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #            arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
  #geom_text(data = spp.scrs, aes(x = NMDS1, y = NMDS2, label = Species),
  #          size = 3)

?stat_ellipse

library(ggrepel)
names(env)
env <- env[c("fjord", "tot_rib", "pathogen_count", "warm_rib", "cold_rib", "temperature", "scale_temp", "diff_avg", "Thermal stress", "Viral disease development")]
names(env) <- c("Fjord", "Total RIB", "Pathogen count", "Warm RIB", "Cold RIB", "Temperature", "Scaled relative temperature", "Temperature deviation", "Thermal stress indicator", "Viral disease development")

envfit_1 = envfit(nmds, env, permutations = 999, na.rm = TRUE)
envfit_1

en_coord_cont = as.data.frame(scores(envfit_1, "vectors")) * ordiArrowMul(envfit_1)
en_coord_cat = as.data.frame(scores(envfit_1, "factors")) * ordiArrowMul(envfit_1)

p1
p2 <- p1 + geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                  data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30")+
  geom_text_repel(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont), direction = "both", force_pull = 30)

p2

ggsave("./data/modified_data/NMDS_thermal_paper.tiff", p2, units="cm", width=30, height=15, dpi=600, compression = 'lzw', limitsize = FALSE)
ggsave("./data/modified_data/NMDS_thermal_paper.png", p2, units="cm", width=30, height=15, dpi=600,  limitsize = FALSE)


adonis2(nmds ~ Fjord, data = env, permutations = 999)
env$`Scaled relative temperature`
env$`Thermal stress indicator`
str(env)

p0 <- ggplot(env, aes(x=`Temperature`, y = `Temperature deviation`)) +
  geom_point(aes(colour = Fjord))+  theme_classic(base_size = 12) + #xlab("PRV-3") +# ylab("Temperature deviation (celcius)")+
  geom_smooth(aes(group = 1), method = "lm")

p0

#Viral disease development
p0 <- ggplot(env, aes(y=`Temperature`, x = `Viral disease development`)) +
  geom_point(aes(colour = Fjord))+  theme_classic(base_size = 12) + #xlab("PRV-3") +# ylab("Temperature deviation (celcius)")+
  geom_smooth(aes(group = 1), method = "lm")

p0

str(env)
str(scores_sites)

p0 <- ggplot(scores_sites, aes(y=`scale_temp`, x = `NMDS2`)) +
  geom_point(aes(colour = Fjord))+  theme_classic(base_size = 12) + #xlab("PRV-3") +# ylab("Temperature deviation (celcius)")+
  geom_smooth(aes(group = 1), method = "lm")

p0

p0 <- ggplot(scores_sites, aes(y=`temperature`, x = `NMDS1`)) +
  geom_point(aes(colour = Fjord))+  theme_classic(base_size = 12) + #xlab("PRV-3") +# ylab("Temperature deviation (celcius)")+
  geom_smooth(aes(group = 1), method = "lm")

p0

p0 <- ggplot(env, aes(x=`Temperature`, y = `Viral disease development`)) +
  geom_point(aes(colour = Fjord))+  theme_classic(base_size = 12) + #xlab("PRV-3") +# ylab("Temperature deviation (celcius)")+
  geom_smooth(aes(group = 1), method = "lm")

p0


p0 <- ggplot(env, aes(y=`Temperature deviation`, x = `Viral disease development`)) +
  geom_point(aes(colour = Fjord))+  theme_classic(base_size = 12) + #xlab("PRV-3") +# ylab("Temperature deviation (celcius)")+
  geom_smooth(aes(group = 1), method = "lm")

p0




names(env)

p3 <- ggplot(env, aes(x=`Temperature`, y = `Thermal stress indicator`)) +
  geom_point(aes(colour = Fjord))+  theme_classic(base_size = 18) + #xlab("PRV-3") +# ylab("Temperature deviation (celcius)")+
  geom_smooth(aes(group = 1), method = "lm")

p3

ggsave("./data/modified_data/thermal_stress_thermal_paper.tiff", p3, units="cm", width=30, height=15, dpi=600, compression = 'lzw', limitsize = FALSE)
ggsave("./data/modified_data/thermal_stress_thermal_paper.png", p3, units="cm", width=15, height=15, dpi=600,  limitsize = FALSE)



p0 <- ggplot(env, aes(x=`Temperature`, y = `Cold RIB`)) +
  geom_point()+  theme_classic(base_size = 12) + #xlab("PRV-3") +# ylab("Temperature deviation (celcius)")+
  geom_smooth()

p0

summary(lm(env$`Cold RIB`~env$`Temperature`))

p0 <- ggplot(env, aes(x=`Temperature`, y = `Cold RIB`)) +
  geom_point()+  theme_classic(base_size = 12) + #xlab("PRV-3") +# ylab("Temperature deviation (celcius)")+
  geom_smooth()

p0

p0 <- ggplot(env, aes(x=`Temperature`, y = `Total RIB`)) +
  geom_point()+  theme_classic(base_size = 12) + #xlab("PRV-3") +# ylab("Temperature deviation (celcius)")+
  geom_smooth()

p0

#ggsave("./data/modified_data/temperature_lines_14days_081223_new.tiff", p, units="cm", width=30, height=15, dpi=600, compression = 'lzw', limitsize = FALSE)





?geom_text_repel
?envfit
# e
# 
# 
# 
# 
# envfit_0 <- (envfit(nmms, trout_metadata[c("CL_H2EB1_672","CL_ICLP2_674","CL_PSMB7_686",
#                                            "IF_ES1_668","IF_txn_683","IM_ARRDC2_663","IM_EPD_667","IM_GLUL_670","IM_napepld_676",
#                                            "IM_NUPR1_677","IM_ODC1_678","IM_TAGLN3_681","IM_tgfb_682","IS_B2M_182","IS_C5aR_577",
#                                            "IS_CD83_579", "IS_IL1B_295" ,"IS_RIG1_361" ,"MRS_ATP5G3_181","MRS_C7_189","MRS_FYB_241", 
#                                            "MRS_HTATIP_272","MRS_NKAB2_328","OS_CCL4_195","OS_CFTR_I_206","OS_HBA_254","OS_NDUFB2_322",
#                                            "OS_UBA1_605","TM_FKBP10_4_583","TM_HSP70_267","TM_Hsp90a_15_269","TM_HSP90a_6_271","TM_SERPIN_9_380",
#                                            "TM_SERPIN20_379","VDD_HERC6_77","VDD_IFI44A_81","VDD_IFIT5_2_83","VDD_MX_86","VDD_NFX_87")], permutations = 999, na.rm = TRUE))
# envfit_0
# 
# #NOTE: No significant effect of single genes
# 
# str(trout_metadata)
# envfit_1 <- (envfit(nmms, trout_metadata[c("fork_length..mm.","skin_colour", "daysurv", "shan_div", "thermal_stress_comp1")], permutations = 999, na.rm = TRUE))
# envfit_1
# en_coord_cont = as.data.frame(scores(envfit_1, "vectors")) * ordiArrowMul(envfit_1)
# en_coord_cat = as.data.frame(scores(envfit_1, "factors")) * ordiArrowMul(envfit_1)
# envfit_2 <- (envfit(nmms, trout_metadata[c("freshwater_entry_days")], permutations = 999, na.rm = TRUE))
# envfit_2
# en_coord_cont2 = as.data.frame(scores(envfit_2, "vectors")) * ordiArrowMul(envfit_2)
# trout_metadata[c("fork_length..mm.")]
# str(data.scores)
# 
# gg = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
#   geom_point(data = data.scores, aes(colour = set_location), size = 3, alpha = 0.5) + 
#   geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
#   scale_colour_manual(values = c("orange", "steelblue","purple")) + 
#   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
#         axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
#         legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         legend.text = element_text(size = 9, colour = "grey30")) +
#   labs(colour = "Season")+
#   stat_ellipse(data=data.scores, aes(x=NMDS1, y=NMDS2,color=set_location, group=set_location),type = "norm")
# 
# gg
# 
# 
# gg = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
#   geom_point(data = data.scores, aes(colour = set_location), size = 3, alpha = 0.5) + 
#   
#   geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
#   scale_colour_manual(values = c("orange", "steelblue", "pink"))  + 
#   geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
#                data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
#   geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
#                data = en_coord_cont2, size =1, alpha = 0.5, colour = "grey30") +
#   geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), 
#              shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
#   geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2+0.04), 
#             label = row.names(en_coord_cat), colour = "navy", fontface = "bold") + 
#   geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
#             fontface = "bold", label = row.names(en_coord_cont)) + 
#   geom_text(data = en_coord_cont2, aes(x = NMDS1, y = NMDS2+0.05), colour = "grey30", 
#             fontface = "bold", label = row.names(en_coord_cont2)) +
#   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
#         axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
#         legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
#         legend.text = element_text(size = 9, colour = "grey30")) + 
#   labs(colour = "Season")+
#   stat_ellipse(data=data.scores, aes(x=NMDS1, y=NMDS2,color=set_location, group=set_location),type = "norm")
# 
# 
# gg
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# scores_sites %>%
#   ggplot(aes(scores_sites, x = NMDS1, y = NMDS2)) +
#   geom_point() +
#   stat_ellipse(geom = "polygon", aes(group = fjord, color = fjord, fill = fjord), alpha = 0.3) +
#   annotate("text", x = -2, y = 0.95, label = paste0("stress: ", format(nmds$stress, digits = 4)), hjust = 0) +
#   theme_bw()
# 
# summary(com)
# str(com)
# 
# nmds
# en = envfit(nmds, env, permutations = 999, na.rm = TRUE)
# en
# 
# plot(nmds)
# plot(en)
# 
# 
# #Non-metric multidimensional scaling
# #Non-metric multidimensional scaling
# 
# positive_fish <- dat[dat$pathogen_count>0, ]
# positive_fish <- dat[dat$tot_rib>0, ]
# 
# tmp_pos <- positive_fish[c(5:13, 15, 17)]
# tmp_pos[tmp_pos == 0] <- NA
# str(positive_fish)
# nmms <- metaMDS(positive_fish[c(5:13, 15, 17)], maxit=100, permutations = 999, k = 2, autotransform = FALSE, engine = "isoMDS")
# plot(nmms, type = "t")
# 
# 
# positive_fish <- com
# tmp_pos <- positive_fish
# tmp_pos[tmp_pos == 0] <- NA
# str(positive_fish)
# nmms <- metaMDS(tmp_pos, maxit = 100, permutations = 999, k = 2, autotransform = FALSE, engine = "isoMDS", na.rm=TRUE)
# plot(nmms, type = "t")
# 
# data.scores <- as.data.frame(scores(nmms, "species"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
# data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
# data.scores <- cbind(data.scores, positive_fish)  #  add the grp variable created earlier
# head(data.scores)  #look at the data
# 
# species.scores <- as.data.frame(scores(nmms, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
# species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
# head(species.scores)  #look at the data
# 
# #plot ordination species
# ggplot() + 
#   geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
#   geom_point(
#     data=data.scores, 
#     aes(
#       x=NMDS1, 
#       y=NMDS2,
#       colour=common_name), 
#     size=3,
#     alpha = 0.6)+
#   geom_point(
#     data=data.scores,
#     aes(
#       x=NMDS1,
#       y=NMDS2,
#       colour=common_name,
#       shape = common_name),
#     size=3.5,
#     alpha = 0.6) +
#   theme_light()+
#   ylab("NMDS2")+
#   xlab("NMDS1")+
#   stat_ellipse(data=data.scores, aes(x=NMDS1, y=NMDS2,color=common_name, group=common_name),type = "norm")