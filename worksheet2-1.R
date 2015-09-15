rm(list=ls())

#getting data
rm(list=ls())
ecoli <- read.table("ecoli_cows.txt", header = T)
ecoli <- as.matrix(ecoli) #do this so we can do matrix algebra with it
ecoli

#matricies by hand
y <- ecoli[,3]
x <- cbind(rep(1,12), ecoli[,1], ecoli[,2], ecoli[,1]*ecoli[,2])

xtx <- t(x) %*% x
xty <- t(x) %*% y

xtx

betaHat <- solve(xtx) %*% xty

betaHat

#model with lm

lmEcoli=lm(EColiCount~carcassWash*sprayChill, data=ecoli)
summary(lmEcoli)

#model with aov

aovEcoli=aov(EColiCount~carcassWash*sprayChill, data=ecoli)
summary(aovEcoli)
