install.packages("car")
library(car)
install.packages("nortest")
library(nortest)
d2 <- read.table("http://math.unm.edu/~luyan/ADA118/acid.txt", header= TRUE)
a1<- d2[which(d2[,2]=="Acid1"),]
a2<- d2[which(d2[,2]=="Acid2"),]
hist(a1$conc, freq= FALSE, main="Histogram of Acid1", xlab = "concentration")
points(density(a1$conc), type="l")

hist(a2$conc, freq = FALSE, main = "Histogram of Acid2", xlab="concentration")
points(density(a2$conc), type = "l")

boxplot(a1$conc, main= "Boxplot of Acid1")
boxplot(a2$conc, main= "Boxplot of Acid2")

qqnorm(a1$conc)
qqline(a1$conc)
qqPlot(a1$conc)

qqnorm(a2$conc)
qqline(a2$conc)
qqPlot(a2$conc)

sh1<- shapiro.test(a1$conc)
sh2<- shapiro.test(a2$conc)
ad1<- ad.test(a1$conc)
ad2<- ad.test(a2$conc)
cvm1<- cvm.test(a1$conc)
cvm2<- cvm.test(a2$conc)

