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

env <-readRDS("./data/modified_data/env_291223.RDS")
names(env)
env$scaled_temp <-env$`Scaled relative temperature`
env$fjord <- env$Fjord
env$tot_rib <- env$`Total RIB`
env$tsi <- env$`Thermal stress indicator`
env$vdd <- env$`Viral disease development`
env <- env[complete.cases(env),]
env$fjord <- as.factor(env$fjord)
str(env)
  
#fishdata <- readRDS("./data/modified_data/fever_summary_table_kristi.RDS")
f1 <- formula(scaled_temp ~ Fjord + Temperature + tot_rib + vdd)
m1 <- glm(f1, data = env, na.action = "na.fail")
summary(m1)
dredge_mod <- dredge(m1, rank="AIC")
dredge_mod
summary(model.avg(dredge_mod, subset = delta < 2)) 



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