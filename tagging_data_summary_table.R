#Sorting pathogen data that was received September 2023

###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################
options(scipen=999)

fishdata <-readRDS("./data/modified_data/fish_metadata_PACE_temp_paper.RDS")
####################################################################################################
#Make summary table
####################################################################################################
str(fishdata)
fishdata <- fishdata[!duplicated(fishdata$vial), ]
names(fishdata)
fishdata <- fishdata[c("vial", "dmy", "System", "TL", "weight.g", "Transmitter")]
names(fishdata) <- c("vial", "tagging_date", "set_location", "tot_length", "mass_g", "tag_type")
str(fishdata)

fishdata$tagging_date
library(lubridate)
?lubridate
fishdata$tagging_date <- dmy(fishdata$tagging_date)
library("dplyr")
str(fishdata)

dplyr::select(fishdata, tagging_date, set_location, tot_length, mass_g, tag_type)
summary_table <- fishdata %>%
  group_by(set_location, tag_type) %>%
  summarize(n = n(), min_date = min(tagging_date, na.rm = TRUE), max_date = max(tagging_date, na.rm = TRUE), mean_size = mean(tot_length, na.rm = TRUE), sd_size = sd(tot_length, na.rm = TRUE), min_size = min(tot_length, na.rm = TRUE), max_size = max(tot_length, na.rm = TRUE), 
            mean_mass = mean(mass_g, na.rm = TRUE), sd_mass = sd(mass_g, na.rm = TRUE), min_mass = min(mass_g, na.rm = TRUE), max_mass = max(mass_g, na.rm = TRUE)) 



summary_table$Tag_date <- paste(summary_table$min_date, "-", summary_table$max_date, sep = '')
summary_table$size <- paste(round(summary_table$mean_size, digits = 0), "±", round(summary_table$sd_size, digits = 0), " (", summary_table$min_size, "-", summary_table$max_size, ")", sep = '')
summary_table$mass <- paste(round(summary_table$mean_mass, digits = 0), "±", round(summary_table$sd_mass, digits = 0), " (", summary_table$min_mass, "-", summary_table$max_mass, ")", sep = '')
str(summary_table)
#summary_table <- summary_table[c(1:3, 19:22)]
write.csv(summary_table,"./data/modified_data/PACE_Temperature_summary_table.csv", row.names = FALSE)
