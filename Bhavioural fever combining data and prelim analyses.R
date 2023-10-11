###########################################################
#Clear memory
rm(list = ls(all = TRUE))
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
###########################################################
library(lme4)
library(ggplot2)
library(nlme)
library(MuMIn)
library(performance)

tracking_bolstad <-readRDS("./data/modified_data/fever_bolstad.RDS")
tracking_beiarn <- readRDS("./data/modified_data/fever_beiarfjorden.RDS")
tracking_stjordal <- readRDS("./data/modified_data/fever_stjordal.RDS")
str(tracking_bolstad)
tracking_bolstad$temperature <- tracking_bolstad$Data/10
tracking_bolstad$receiverID <- tracking_bolstad$Receiver
str(tracking_bolstad)
tracking_bolstad <- tracking_bolstad[c("transmitterID", "receiverID", "datetime", "temperature", "gill_sample", "fjord", "tagging_date")]
tracking_bolstad$fishID <- tracking_bolstad$transmitterID
tracking_bolstad$datetime <- as.POSIXct(tracking_bolstad$datetime)
tracking_bolstad$day <- as.numeric(tracking_bolstad$datetime-tracking_bolstad$tagging_date)
summary(tracking_bolstad$day)

str(tracking_beiarn)
fishdata_beiarn <- readRDS("./data/modified_data/fishdata.RDS")
tracking_beiarn <- merge(tracking_beiarn, fishdata_beiarn[c("transmitterID", "gill_ID")], by = "transmitterID")
str(tracking_beiarn)
tracking_beiarn$gill_sample <- tracking_beiarn$gill_ID
tracking_beiarn <- tracking_beiarn[c("fishID","transmitterID", "receiverID", "datetime", "temperature", "gill_sample", "fjord", "tagging_date")]
tracking_beiarn$day <- as.numeric(tracking_beiarn$datetime-tracking_beiarn$tagging_date)
head(tracking_beiarn$day)
summary(tracking_beiarn$day)

str(tracking_stjordal)
fishdata_stjordal <- readRDS("./data/modified_data/fishdata_stjordal.RDS")
str(fishdata_stjordal)
export <- fishdata_stjordal
export <- export[export$gill_sample!='',]
write.csv(export, "./data/modified_data/samples_stjordal_140623.csv")
tracking_stjordal <- merge(tracking_stjordal, fishdata_stjordal[c("transmitterID", "gill_sample")], by = "transmitterID")
str(tracking_stjordal)
tracking_stjordal <- tracking_stjordal[c("fishID","transmitterID", "receiverID", "datetime", "temperature", "gill_sample", "fjord", "tagging_date")]
tracking_stjordal$day <- as.numeric(tracking_stjordal$datetime-tracking_stjordal$tagging_date)
head(tracking_stjordal$day)
summary(tracking_stjordal$day)

tracking_data <- rbind(tracking_bolstad, tracking_beiarn, tracking_stjordal)

tmp <- tracking_data



library(dplyr)
str(tracking_data)
tracking_data$temperature <- as.numeric(tracking_data$temperature)
tracking_data$fjord <- as.factor(tracking_data$fjord)
tracking_data$fishID <- as.factor(tracking_data$fishID)
tracking_data$receiverID <- as.factor(tracking_data$receiver)
tracking_data$transmitterID <- as.factor(tracking_data$transmitterID)
tracking_data <- tracking_data[!is.na(tracking_data$temperature), ]
#tracking_data$day <- floor(tracking_data$day)

str(tracking_beiarn)
str(tracking_data)

tracking_data <- tracking_data[tracking_data$fishID!=128,]
tracking_data <- tracking_data[tracking_data$fishID!=145,]


fishdata <- readRDS("./data/modified_data/fever_summary_table_kristi.RDS")
str(fishdata)
fishdata$common_name <- as.factor(fishdata$common_name)
summary(fishdata$common_name)
summary(fishdata$common_name)
fishdata <- fishdata[!duplicated(fishdata$transmitterID), ]
str(fishdata)

str(fishdata_beiarn)


tracking_data <- merge(tracking_data, fishdata[c("transmitterID", "ct_level.y", "Comp.1", "Comp.2")], by="transmitterID")
tracking_data$ct_level <- tracking_data$ct_level.y
tmp <- tracking_data[!duplicated(tracking_data$transmitterID), ]
summary(tmp$fjord)
p <- ggplot(tracking_data[tracking_data$fjord=="Beiarfjorden",], aes(x=datetime, y=temperature, col= fishID)) + geom_point()+ geom_smooth(method = "lm")+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("River Beiarelva, n=43") + 
  xlab("Day of tracking") + ylab("Celius")+ ylim(6,16)
#+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted")
p

p <- ggplot(tracking_data[tracking_data$fjord=="Beiarfjorden",], aes(x=datetime, y=temperature, col= fishID)) + geom_point()+ geom_smooth(method = "lm")+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("River Beiarelva, n=43") + 
  xlab("Day of tracking") + ylab("Celcius")+ ylim(6,16)
#+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted")
p

ggsave("./data/modified_data/nytemp_14_beiarfjorden.tiff", p, units="cm", width=18, height=15, dpi=400, compression = 'lzw')


summary(tracking_data$fjord)


p <- ggplot(tracking_data[tracking_data$fjord=="Stjordal", ], aes(x=datetime, y=temperature, col= fishID)) + geom_point()+ geom_smooth(method="lm")+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("River Stjordal, n=29") + 
  xlab("Day of tracking") + ylab("Celcius")+ ylim(6,16)
#+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted")
p

p <- ggplot(tracking_data[tracking_data$fjord=="Stjordal", ], aes(x=day, y=temperature, col= fishID)) + geom_point()+ geom_smooth()+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("River Stjordal, n=29") + 
  xlab("Day of tracking") + ylab("Celcius")+ ylim(6,16)
#+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted")
p
ggsave("./data/modified_data/temp_14_Stjordal.tiff", p, units="cm", width=16, height=15, dpi=400, compression = 'lzw')


tracking_data <- tracking_data[tracking_data$fishID!=2687,]


p <- ggplot(tracking_data[tracking_data$fjord=="bolstad", ], aes(x=day, y=temperature, col= fishID)) + geom_point()+ geom_smooth()+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("River Vosso, n=22") + 
  xlab("Day of tracking") + ylab("Celius") + ylim(6,16)
#+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted")
p

# p <- ggplot(tracking_data[tracking_data$fjord=="bolstad" & tracking_data$fishID=='2687', ], aes(x=day, y=temperature, col= fishID)) + geom_point()+ geom_smooth()+ theme_classic(base_size = 18)+ 
#   theme(legend.position = "right") + ggtitle("Vosso") + 
#   xlab("Day of tracking") + ylab("Celius")
# #+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted")
# p

ggsave("./data/modified_data/temp_14_Vosso.tiff", p, units="cm", width=16, height=15, dpi=400, compression = 'lzw')

str(tracking_data)
str(fishdata)
tracking_data <- merge(tracking_data, fishdata[c("transmitterID", "VDD_comp1", "VDD_comp2", "max_ct_value", "Days.survived", "fork_length..mm.")], by = "transmitterID")
str(tracking_data)


p <- ggplot(temp_use, aes(x=day, y=mean_temp, col= fjord)) + geom_point()+ geom_smooth()+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("Body temperature") + 
  xlab("Day of tracking (+1)") + ylab("Celius")
#+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted")
p


#bring in the gill samples that have been analyzed

# fishdata <- readRDS("./data/modified_data/fever_summary_table_kristi.RDS")
# str(fishdata)
# fishdata$common_name <- as.factor(fishdata$common_name)
# summary(fishdata$common_name)
# fishdata <- fishdata[!duplicated(fishdata$transmitterID), ]
# str(fishdata)
# str(temp_use)
temp_use <- tracking_data[tracking_data$day>7, ]
str(temp_use)
temp_use <- temp_use%>%
  select(fishID, transmitterID,VDD_comp1, VDD_comp2, max_ct_value, temperature, fjord, day, tagging_date)%>%
  group_by(fjord, transmitterID, fishID, tagging_date, VDD_comp1, VDD_comp2, max_ct_value)%>%
  summarize(mean_temp = mean(temperature))
head(temp_use)
#Note: there is two few fish in here now!
?scale
scaled <- ''
scaled$temperature <- temp_use$mean_temp
scaled$fjord <- temp_use$fjord
scaled$transmitterID <- temp_use$transmitterID
scaled$mean_temp <- scale(temp_use$mean_temp)
scaled$VDD_comp1 <- scale(temp_use$VDD_comp1)
scaled$VDD_comp2 <- scale(temp_use$VDD_comp2)
scaled$max_ct_value <- scale(temp_use$max_ct_value)
scaled$tagging_date <- scale(temp_use$tagging_date)
f1 <- formula(temperature~ VDD_comp1 + VDD_comp2 + max_ct_value + tagging_date)
m1 <- lme(f1, data=scaled, random = ~1|fjord/transmitterID, method = "ML", control=(msMaxIter=100))
dredge_mod <- dredge(m1, rank="AIC")
dredge_mod
warnings(dredge_mod)
summary(model.avg(dredge_mod, subset = delta < 2)) 


str(fever)

p <- ggplot(fever, aes(x=VDD_comp1, y=mean_temp, col= fjord)) + geom_point()+ geom_smooth(method = "lm")+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right") + ggtitle("Body temperature") + 
  xlab("Day of tracking (+1)") + ylab("Celius")
#+ geom_hline(yintercept=c(mean_comp1+sd_comp1, mean_comp1-sd_comp1), linetype="dotted")
p

str(fever)
#exclude <- fever[fever$mean_temp<7, ]
#fever <- anti_join(fever, exclude)

main_table$marine_residence <- scale(main_table$marine_residence)
main_table$condition_factor <- scale(main_table$condition_factor)
main_table$size <- scale(main_table$size)

p <- ggplot(fever, aes(x=mean_temp, y=VDD_comp1, col= fjord)) + geom_point()+ geom_smooth(method = "lm")+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right")
p

p <- ggplot(fever, aes(x=max_ct_value, y=mean_temp, col= fjord)) + geom_point()+ geom_smooth(method = "lm")+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right")
p

p <- ggplot(fever, aes(x=max_ct_value, y=VDD_comp1, col= fjord)) + geom_point()+ geom_smooth(method = "lm")+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right")
p

str(tracking_data$fjord)
fever <- tracking_data[tracking_data$day>6,]
summary(tracking_data$day)
head(tracking_data)
str(fever)
str(tracking_data)
str(tracking_data)
str(fever)
tracking_data$day_of_year <- tracking_data$tagging_date- as.POSIXct('2020-01-01')
fever$day_of_year <- fever$tagging_date- as.POSIXct('2020-01-01')
tracking_data_0 <- tracking_data
tracking_data <- tracking_data[tracking_data$day>7,]
tracking_data <- tracking_data_0

scaled <- ''
scaled$temperature <- tracking_data$temperature
scaled$fjord <- tracking_data$fjord
scaled$transmitterID <- tracking_data$transmitterID
scaled$temp <- scale(tracking_data$temperature)
scaled$VDD_comp1 <- scale(tracking_data$VDD_comp1)
scaled$VDD_comp2 <- scale(tracking_data$VDD_comp2)
scaled$max_ct_value <- scale(tracking_data$max_ct_value)
scaled$tagging_date <- scale(tracking_data$tagging_date)
scaled$day <- scale(tracking_data$day)
scaled$dayofyear <- scale(tracking_data$day_of_year)
str(scaled)
scaled <- as.data.frame(scaled)
f2 <- formula(temp~ VDD_comp1 + VDD_comp2 + max_ct_value + day + tagging_date)
m2 <- lme(f2, data=scaled, random = ~1|fjord/transmitterID, method = "ML", control = lmeControl(opt = "optim"))
dredge_mod2 <- dredge(m2, rank="AIC")
dredge_mod2
warnings(dredge_mod2)
summary(model.avg(dredge_mod2, subset = delta < 2)) 

p <- ggplot(tracking_data, aes(x=datetime, y=temperature, col= fjord)) + geom_point()+ geom_smooth(method = "lm")+ theme_classic(base_size = 18)+ 
  theme(legend.position = "right")
p

check_collinearity(m1)

library(gridExtra)

df <- data.frame(1:5)
df$parameter <- c("PCA_VDD_D1", "PCA_VDD_D2","Max_CT", "Day",  "Tag_date")
df$coeff <- c(-0.0083107, -0.1168401, 0.0005140, -0.2151778, 0.0100348)
df$se <- c(0.0306819, 0.0519406,0.0166380,0.0008543,0.0315024)
df$p <- c(0.4758, 0.0218,"<0.001", 0.9118)


a <- ggplot((df), aes(x=parameter, y=coeff)) + theme_classic(base_size = 18)+ theme(legend.position = "top") + ggtitle("Conditional average estimates") + labs(fill =element_blank())+ xlab("Model parameter") + 
  ylab("Estimate")+ 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=coeff-se, ymax=coeff+se), width=.2,
                position=position_dodge(.9)) 

a

f1 <- formula(mean_temp~ VDD_comp1 + max_ct_value + fjord + fjord*max_ct_value)
m1 <- glm(f1, data=fever)
dredge_mod <- dredge(m1, rank="AIC")
summary(m1)


str(fever)
f1 <- formula(temperature~ VDD_comp1 + VDD_comp2 + ct_level.y)
m1 <- lme(f1, data=tracking_data, random = list(~1|fjord), method = "ML")
summary(m1)
dredge_mod <- dredge(m1, rank="AIC")
dredge_mod
dredge_1 <- dredge_mod

check_collinearity(m)
summary(model.avg(dredge_mod, subset = delta < 2)) 


f1 <- formula(marine_residence~condition_factor + sex + size)
f2 <- formula(marine_residence~condition_factor + size)
f3 <- formula(marine_residence~sex + size)
f4 <- formula(marine_residence~condition_factor + sex)

t1 <- lme(f1, data=main_table, random = list(~1|location, ~1|tracking_year), method = "ML", na.action = na.fail)
t2 <- lme(f2, data=main_table, random = list(~1|location, ~1|tracking_year), method = "ML", na.action = na.fail)
t3 <- lme(f3, data=main_table, random = list(~1|location, ~1|tracking_year), method = "ML", na.action = na.fail)
t4 <- lme(f4, data=main_table, random = list(~1|location, ~1|tracking_year), method = "ML", na.action = na.fail)

check_collinearity(t1)
check_collinearity(t2)
check_collinearity(t3)
check_collinearity(t4)