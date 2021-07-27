dat1 <- read.table(text="
0 g/kg 88.6 73.2 91.4 68.0 75.2
1 g/kg 63.0 53.9 69.2 50.1 71.5
2 g/kg 44.9 59.5 40.2 56.3 38.7
4 g/kg 31.0 39.6 45.3 25.2 22.7
                   ", header=FALSE)
dat2 <-dat1[, 3:7]
dat3 <- t(dat2)
rem <- as.data.frame(dat3)
colnames(rem)<- c("0","1","2","4")
library(reshape2)
rem.long <- melt(rem, variable.name = "dose", value.name = "minutes")

boxplot(rem$'0', main="Boxplot for 0g/kg")
boxplot(rem$`1`, main="Boxplot for 1g/kg")
boxplot(rem$`2`, main="Boxplot for 2g/kg")
boxplot(rem$`4`, main="Boxplot for 4g/kg")

mean_0 <- mean(rem$`0`)
mean_1 <- mean(rem$'1')
mean_2 <- mean(rem$'2')
mean_4 <- mean(rem$'4')

med_0 <- median(rem$`0`)
med_1 <- median(rem$`1`)
med_2 <- median(rem$`2`)
med_4 <- median(rem$`4`)

sd_0 <- sd(rem$`0`)
sd_1 <- sd(rem$`1`)
sd_2 <- sd(rem$`2`)
sd_4 <- sd(rem$`4`)

mean_data <- c(mean_0,mean_1,mean_2, mean_4)

sd(mean_data)

aov(minutes~dose,data = rem.long)

summary(aov(minutes~dose,data = rem.long))
F_value <- 21.09
F_Crit <- qf(0.95,3,16)

t_crit <- abs(qt(.025,16))
s_pooled <- 9.641706
lsd <- t_crit*s_pooled*sqrt(2/5)

pairwise.t.test(rem.long$minutes, rem.long$dose, pool.sd= TRUE, p.adjust.method= "none")
pairwise.t.test(rem.long$minutes, rem.long$dose, pool.sd= TRUE, p.adjust.method= "bonf")

TukeyHSD(aov(minutes~dose,data = rem.long))
