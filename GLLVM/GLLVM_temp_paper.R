#Example for using GLLVM
#follows https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13303

library(gllvm)
library(gratia)
library(mgcv)
library(ggplot2)
library(dplyr)
library(patchwork)

data(antTraits)

y <- as.matrix(antTraits$abund);
y <- readRDS("./data/modified_data/GLLVM_y.RDS")
y <- y %>%
  dplyr::select(-c(sum))

X <- scale(as.matrix(antTraits$env))
X <- readRDS("./data/modified_data/GLLVM_X.RDS")
X$thermal_stress <- X$`Thermal stress`
X$VDD <- X$`Viral disease development`
X <- X %>%
  dplyr::select(c(fjord, temperature, scale_temp, thermal_stress, VDD))
X$temperature <- scale(X$temperature) 

TR <- antTraits$traits

fitp <- gllvm(y, family = poisson())
fitp
par(mfrow = c(3, 3))
plot(fitp)


fit_ord <- gllvm(y, family = "negative.binomial")
fit_ord
par(mfrow = c(3, 3))
plot(fit_ord)

fit_tweed <- gllvm(y, family = "tweedie")
fit_tweed
par(mfrow = c(3, 3))
plot(fit_ord)


par(mfrow = c(1, 1))
ordiplot(fit_ord)

#include environmental parameters in the model
str(X)
#X$fjord <- as.factor(X$fjord)
X[is.na(X)] <- 0

str(X)
X$fjord <- as.factor(X$fjord)
X <- as.matrix(X)
str(X)
fit_env <- gllvm(y, X, family= "negative.binomial", formula= ~ temperature+ scale_temp+ thermal_stress+ VDD)
par(mfrow = c(3, 3))
plot(fit_env)
ordiplot(fit_env)
par(mfrow = c(1, 1))
summary(fit_env)
coefplot(fit_env)

coefplot(fit_env, cex.ylab = 1, mar = c(4, 5, 2, 1), xlim.list = list(c(-2, 2),c(-2, 2),c(-4, 4)))
?coefplot

cr <- getResidualCor(fit_env)
options(scipen=999)
library(corrplot)
library(gclus)
par(mfrow = c(1, 1))
corrplot(cr[order.single(cr),order.single(cr)], diag=TRUE, type="lower", method="square", tl.cex=0.8, tv.srt=45, tl.col="red")
?corrplot

fit_4th <- gllvm(y, X, TR, family= "negative.binomial", num.lv = 3, formula= ~(Bare.ground + Shrub.cover + Volume.lying.CWD) + (Bare.ground + Shrub.cover + Volume.lying.CWD):(Pilosity + Polymorphism + Webers.length))

coefplot(fit_4th, cex.ylab = 0.7, mar = c(4, 9, 2, 1), xlim.list = list(NULL,NULL,c(-4, 4)))

library(lattice)
coefplot(fit_4th, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourth <- fit_4th$fourth.corner
a <- max( abs(fourth))
colort <- colorRampPalette(c("blue", "white", "red"))
plot4th <- levelplot((as.matrix(fourth)), xlab = "Environmental Variables",
                     ylab= "Species traits", col.regions = colort(100), cex.lab=1.3,
                     at = seq(-a, a, length =100), scales = list(x=list(rot=45)))


plot4th

#Final note: probably good to consider random slopes: https://link.springer.com/article/10.1007/s11222-022-10189-w

fit_4th2 <- gllvm(y, X, TR, family = "negative.binomial", num.lv = 3,
                  formula = y ~ (Bare.ground + Shrub.cover + Volume.lying.CWD), seed = 1234)

anova(fit_4th, fit_4th2)
