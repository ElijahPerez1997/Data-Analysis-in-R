d1<-read.table(file="C:/Users/Elijah/Desktop/acid.txt",header=T)
acid_1<-d1[which(d1[,2]=="Acid1"),]
acid_2<-d1[which(d1[,2]=="Acid2"),]
boxplot(acid_1$conc, main="Acid1 Boxplot")
boxplot(acid_2$conc, main="Acid2 Boxplot")
#mean of Acid1
mean1<-mean(acid_1$conc)
#median of Acid1
med1<-median(acid_1$conc)
#mean of Acid2
mean2<-mean(acid_2$conc)
#median of Acid2
med2<-median(acid_2$conc)
bs.one.samp.dist <- function(dat, N = 1e4) {
  n <- length(dat);
  # resample from data
  sam <- matrix(sample(dat, size = N * n, replace = TRUE), ncol=N);
  # draw a histogram of the means
  sam.mean <- colMeans(sam);
  # save par() settings
  old.par <- par(no.readonly = TRUE)
  # make smaller margins
  par(mfrow=c(2,1), mar=c(3,2,2,1), oma=c(1,1,1,1))
  # Histogram overlaid with kernel density curve
  hist(dat, freq = FALSE, breaks = 6
       , main = "Plot of data with smoothed density curve")
  points(density(dat), type = "l")
  rug(dat)
  
  hist(sam.mean, freq = FALSE, breaks = 25
       , main = "Bootstrap sampling distribution of the mean"
       , xlab = paste("Data: n =", n
                      , ", mean =", signif(mean(dat), digits = 5)
                      , ", se =", signif(sd(dat)/sqrt(n)), digits = 5))
  # overlay a density curve for the sample means
  points(density(sam.mean), type = "l")
  # overlay a normal distribution, bold and red
  x <- seq(min(sam.mean), max(sam.mean), length = 1000)
  points(x, dnorm(x, mean = mean(dat), sd = sd(dat)/sqrt(n))
         , type = "l", lwd = 2, col = "red")
  # place a rug of points under the plot
  rug(sam.mean)
  # restore par() settings
  par(old.par)
}
bs.two.samp.diff.dist <- function(dat1, dat2, N = 1e4) {
  n1 <- length(dat1);
  n2 <- length(dat2);
  # resample from data
  sam1 <- matrix(sample(dat1, size = N * n1, replace = TRUE), ncol=N);
  sam2 <- matrix(sample(dat2, size = N * n2, replace = TRUE), ncol=N);
  # calculate the means and take difference between populations
  sam1.mean <- colMeans(sam1);
  sam2.mean <- colMeans(sam2);
  diff.mean <- sam1.mean - sam2.mean;
  # save par() settings
  old.par <- par(no.readonly = TRUE)
  # make smaller margins
  par(mfrow=c(3,1), mar=c(3,2,2,1), oma=c(1,1,1,1))
  # Histogram overlaid with kernel density curve
  hist(dat1, freq = FALSE, breaks = 6
       , main = paste("Sample 1", "\n"
                      , "n =", n1
                      , ", mean =", signif(mean(dat1), digits = 5)
                      , ", sd =", signif(sd(dat1), digits = 5))
       , xlim = range(c(dat1, dat2)))
  points(density(dat1), type = "l")
  rug(dat1)
  
  hist(dat2, freq = FALSE, breaks = 6
       , main = paste("Sample 2", "\n"
                      , "n =", n2
                      , ", mean =", signif(mean(dat2), digits = 5)
                      , ", sd =", signif(sd(dat2), digits = 5))
       , xlim = range(c(dat1, dat2)))
  points(density(dat2), type = "l")
  rug(dat2)
  
  hist(diff.mean, freq = FALSE, breaks = 25
       , main = paste("Bootstrap sampling distribution of the difference in means", "\n"
                      , "mean =", signif(mean(diff.mean), digits = 5)
                      , ", se =", signif(sd(diff.mean), digits = 5)))
  # overlay a density curve for the sample means
  points(density(diff.mean), type = "l")
  # overlay a normal distribution, bold and red
  x <- seq(min(diff.mean), max(diff.mean), length = 1000)
  points(x, dnorm(x, mean = mean(diff.mean), sd = sd(diff.mean))
         , type = "l", lwd = 2, col = "red")
  # place a rug of points under the plot
  rug(diff.mean)
  # restore par() settings
  par(old.par)
}
#Bootstrap for Acid1
bs.one.samp.dist(acid_1$conc)
#Bootstrap for Acid2
bs.one.samp.dist(acid_2$conc)
#Bootstrap of the difference between the means of Acid1 and Acid2
bs.two.samp.diff.dist(acid_1$conc, acid_2$conc)
#Standard deviation for Acid1
sd1<-sd(acid_1$conc)
#Standard deviation for Acid2
sd2<-sd(acid_2$conc)
n1<-length(acid_1$conc)
n2<-length(acid_2$conc)
x<-(((sd1^2)/n1)+((sd2^2)/n2))^2
y<-(((sd1^4)/(n1*n1*(n1-1)))+((sd2^4)/(n2*n2*(n2-1))))
#Degrees of freedom for the two data sets
df<-x/y
#Standard error for the two data sets
se<- sqrt(((sd1^2)/n1)+((sd2^2)/n2))
#t-crit value
tcrit<- abs(qt(.025,df))
#t-statistic value
tstat<-(mean1-mean2)/se
