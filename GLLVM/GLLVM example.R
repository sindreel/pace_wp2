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
X <- scale(as.matrix(antTraits$env))
TR <- antTraits$traits

fitp <- gllvm(y, family = poisson())
fitp
par(mfrow = c(3, 3))
plot(fitp)

p <- draw(fitp)

fit_ord <- gllvm(y, family = "negative.binomial")
fit_ord
par(mfrow = c(3, 3))
plot(fit_ord)

par(mfrow = c(1, 1))
ordiplot(fit_ord, biplot = TRUE, ind.spp = 15, xlim = c(-3, 3), ylim=c(-2, 1.6))

#include environmental parameters in the model

fit_env <- gllvm(y, X, family= "negative.binomial", num.lv = 3, formula= ~Bare.ground + Shrub.cover + Volume.lying.CWD)
par(mfrow = c(3, 3))
plot(fit_env)

par(mfrow = c(1, 1))
coefplot(fit_env, cex.ylab = 0.7, mar = c(4, 9, 2, 1), xlim.list = list(NULL,NULL,c(-4, 4)))


cr <- getResidualCor(fit_env)
library(corrplot)
library(gclus)
par(mfrow = c(1, 1))
corrplot(cr[order.single(cr),order.single(cr)], diag=FALSE, type="lower", method="square", tl.cex=0.8, tv.srt=45, tl.col="red")


fit_4th <- gllvm(y, X, TR, family= "negative.binomial", num.lv = 3, formula= ~(Bare.ground + Shrub.cover + Volume.lying.CWD) + (Bare.ground + Shrub.cover + Volume.lying.CWD):(Pilosity + Polymorphism + Webers.length))

coefplot(fit_4th, cex.ylab = 0.7, mar = c(4, 9, 2, 1), xlim.list = list(NULL,NULL,c(-4, 4)))


#Final note: probably good to consider random slopes: https://link.springer.com/article/10.1007/s11222-022-10189-w

