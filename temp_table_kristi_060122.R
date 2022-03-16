#Table to kristie 06012022


###########################################################
#Clear memory
rm(list = ls(all = TRUE))
###########################################################


table_kristi <- read.csv("./data/modified_data/table_kristi.csv", sep = ",")

str(table_kristi)
table_kristi$Transmitter.ID

table_kristi$tagging_habitat <- ''


head(table_kristi)
table_kristi$set_location
table_kristi$tagging_habitat[table_kristi$set_location=='Beiarfjorden'] <- 'estuary'
table_kristi$tagging_habitat[table_kristi$set_location=='Stjordal'] <- 'estuary'
table_kristi$tagging_habitat[table_kristi$set_location=='Stjordal'] <- 'estuary'
str(table_kristi)

beiarfjorden_fish <- read.csv("C:/Rdata/Github/Beiarfjorden_2/data/modified_data/beiarfjorden_tagging_kristie.csv", sep = ",")
str(beiarfjorden_fish)
beiarfjorden_fish$transmitterID <- beiarfjorden_fish$TransmitterID
#beiarfjorden_fish$size <- beiarfjorden_fish$Length..mm.
#beiarfjorden_fish$mass <- beiarfjorden_fish$Mass..g.
#beiarfjorden_fish$tagging_temp <- beiarfjorden_fish$water_temperature
#names(beiarfjorden_fish) <- c("transmitterID", "gill_sample",  "skin_colour", "tagging_temp","Date", "max_tracking_date", "tagging_date", "daysurv", "avg_temp", "min_temp", "max_temp", "n_station", "n_all", "freshwater_entry", "freshwater_entry_days")
beiarfjorden_fish <- beiarfjorden_fish[c("transmitterID", "gill_sample", "tagging_temp", "skin_colour", "tagging_date", "daysurv", "avg_temp", "min_temp", "max_temp", "freshwater_entry", "freshwater_entry_days")]

hellstranda_fish <- read.csv("C:/Rdata/Github/hellstranda/data/modified_data/stjordal_tagging_kristie.csv", sep = ",")
str(hellstranda_fish)
hellstranda_fish <- hellstranda_fish[c("transmitterID", "gill_sample", "tagging_temp", "skin_colour", "tagging_date", "daysurv", "avg_temp", "min_temp", "max_temp", "freshwater_entry", "freshwater_entry_days")]

bolstad_fish <-  read.csv("C:/Rdata/Github/PACE_WP2_Bolstadfjord/data/modified_data/bolstadfjorden_tagging_kristie.csv", sep = ",")
str(bolstad_fish)
str(table_kristi)
summary(as.factor(table_kristi$common_name))
summary(as.factor(bolstad_fish$Spp))

bolstad_fish$Spp[bolstad_fish$Spp=='h_salmon'] <- 'Atlantic Salmon'
bolstad_fish$Spp[bolstad_fish$Spp=='w_salmon'] <- 'Atlantic Salmon'
bolstad_fish$Spp[bolstad_fish$Spp=='s_trout'] <- 'Sea trout'
bolstad_fish$common_name <- bolstad_fish$Spp
table_kristi$interaction <- interaction(table_kristi$common_name, table_kristi$dna_id)
summary(as.factor(table_kristi$interaction))

bolstad_fish$interaction <- interaction(bolstad_fish$common_name, bolstad_fish$gill_sample)
bolstad_fish <- merge(bolstad_fish, table_kristi[c("interaction")], by = "interaction")

str(bolstad_fish)
names(bolstad_fish) <- c("interaction", "transmitterID", "gill_sample",   "tagging_temp","skin_colour","Date","Spp", "max_tracking_date", "tagging_date", "daysurv", "avg_temp", "min_temp", "max_temp", "n_station", "n_all" , "freshwater_entry", "freshwater_entry_days", "common_name")
bolstad_fish <- bolstad_fish[c("transmitterID", "gill_sample", "tagging_temp", "skin_colour", "tagging_date", "daysurv", "avg_temp", "min_temp", "max_temp", "freshwater_entry", "freshwater_entry_days")]
tmp <- bolstad_fish[duplicated(bolstad_fish$gill_sample), ]

str(beiarfjorden_fish)
str(hellstranda_fish)
str(bolstad_fish)
summary_data <- rbind(beiarfjorden_fish, hellstranda_fish, bolstad_fish)


str(table_kristi)

combined_table <- merge(table_kristi, summary_data, by.x = "dna_id", by.y = "gill_sample", all.x = TRUE)

str(combined_table)

removeRows <- function(rowNum, data) {
  newData <- data[-rowNum, , drop = FALSE]
  rownames(newData) <- NULL
  newData
}

combined_table <- removeRows(c(63, 96), combined_table)

str(combined_table)

headings <- colnames(combined_table)

combined_table <- combined_table[c("dna_id","unique_id","fluidigm_num","alternate_num","common_name","hatchery_wild","fork_length..mm.","set_location", "station",
                                   "Transmitter.ID","capture_date", "tagging_habitat","skin_colour","transmitterID","daysurv","tagging_temp","avg_temp",
                                  "min_temp","max_temp", "freshwater_entry", "freshwater_entry_days")]
head(combined_table)
summary(as.factor(combined_table$skin_colour))

combined_table$skin_colour[combined_table$skin_colour=='(Sølv)-blank'] <-'silver'
combined_table$skin_colour[combined_table$skin_colour=='blank'] <- 'silver'
combined_table$skin_colour[combined_table$skin_colour=='Blank'] <- 'silver'
combined_table$skin_colour[combined_table$skin_colour=='S'] <- 'silver'
combined_table$skin_colour[combined_table$skin_colour=='Silver'] <- 'silver'
combined_table$skin_colour[combined_table$skin_colour=='SB'] <- 'silver'

combined_table$skin_colour[combined_table$skin_colour=='BB'] <- 'silver_brown'
combined_table$skin_colour[combined_table$skin_colour=='blank, noe farge'] <- 'silver_brown'
combined_table$skin_colour[combined_table$skin_colour=='brun-blank'] <- 'silver_brown'
combined_table$skin_colour[combined_table$skin_colour=='Brun-blank'] <- 'silver_brown'
combined_table$skin_colour[combined_table$skin_colour=='Brun-blank m/ røde prikker'] <- 'silver_brown'
combined_table$skin_colour[combined_table$skin_colour=='brunblank'] <- 'silver_brown'
combined_table$skin_colour[combined_table$skin_colour=='brunblank m prikker'] <- 'silver_brown'
combined_table$skin_colour[combined_table$skin_colour=='Grey_brown'] <- 'silver_brown'
combined_table$skin_colour[combined_table$skin_colour=='litt farge'] <- 'silver_brown'
combined_table$skin_colour[combined_table$skin_colour=='Silver_grey'] <- 'silver_brown'

combined_table$skin_colour[combined_table$skin_colour=='B'] <- 'brown'
combined_table$skin_colour[combined_table$skin_colour=='brun'] <- 'brown'
combined_table$skin_colour[combined_table$skin_colour=='brun m prikker'] <- 'brown'
combined_table$skin_colour[combined_table$skin_colour=='Brun m/ prikker'] <- 'brown'
combined_table$skin_colour[combined_table$skin_colour=='?'] <- NA
combined_table$tagging_temp[combined_table$tagging_temp=='?'] <- NA


str(combined_table)
summary(as.factor(combined_table$set_location))
summary(as.factor(combined_table$station))

combined_table$tagging_habitat[combined_table$set_location=='Bolstadfjorden'] <- 'estuary'
combined_table$tagging_habitat[combined_table$set_location=='Bolstadfjorden'& combined_table$station=='Bolstad_høl'] <- 'river'
combined_table$tagging_habitat[combined_table$set_location=='Bolstadfjorden'& combined_table$station=='Langhølen'] <- 'river'
combined_table$tagging_habitat[combined_table$set_location=='Bolstadfjorden'& combined_table$station=='Rognahølen'] <- 'river'
combined_table$tagging_habitat[combined_table$set_location=='Bolstadfjorden'& combined_table$station=='Merrhølen'] <- 'river'
combined_table$tagging_habitat[combined_table$set_location=='Bolstadfjorden'& combined_table$station=='Osen'] <- 'river'

write.csv(combined_table, "./data/modified_data/summary_table_metadata_PACE_WP2_270122.csv", row.names = FALSE)

