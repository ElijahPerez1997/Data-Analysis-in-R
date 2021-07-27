suicide <- read.table(text="
Month Suicides
                      01Jan 1867
                      02Feb 1789
                      03Mar 1944
                      04Apr 2094
                      05May 2097
                      06Jun 1981
                      07Jul 1887
                      08Aug 2024
                      09Sep 1928
                      10Oct 2032
                      11Nov 1978
                      12Dec 1859
                      ", header=TRUE)
prop<- rep(1/12,12)
x.summary<- chisq.test(suicide$Suicides, correct = FALSE, p=prop)
x.summary
x.table<-data.frame(month = suicide$Month
                    , obs = x.summary$observed
                    , exp = x.summary$expected
                    , res = x.summary$residuals
                    , chisq = x.summary$residuals^2
                    , stdres = x.summary$stdres)
x.table
b.sum1<- prop.test(suicide$Suicides[1],sum(suicide$Suicides),p=1/12, conf.level = 0.95)
b.sum2<- prop.test(suicide$Suicides[2],sum(suicide$Suicides),p=1/12, conf.level = 0.95)
b.sum3<- prop.test(suicide$Suicides[3],sum(suicide$Suicides),p=1/12, conf.level = 0.95)
b.sum4<- prop.test(suicide$Suicides[4],sum(suicide$Suicides),p=1/12, conf.level = 0.95)  
b.sum5<- prop.test(suicide$Suicides[5],sum(suicide$Suicides),p=1/12, conf.level = 0.95)  
b.sum6<- prop.test(suicide$Suicides[6],sum(suicide$Suicides),p=1/12, conf.level = 0.95)
b.sum7<- prop.test(suicide$Suicides[7],sum(suicide$Suicides),p=1/12, conf.level = 0.95)
b.sum8<- prop.test(suicide$Suicides[8],sum(suicide$Suicides),p=1/12, conf.level = 0.95)
b.sum9<- prop.test(suicide$Suicides[9],sum(suicide$Suicides),p=1/12, conf.level = 0.95)
b.sum10<- prop.test(suicide$Suicides[10],sum(suicide$Suicides),p=1/12, conf.level = 0.95)
b.sum11<- prop.test(suicide$Suicides[11],sum(suicide$Suicides),p=1/12, conf.level = 0.95)
b.sum12<- prop.test(suicide$Suicides[12],sum(suicide$Suicides),p=1/12, conf.level = 0.95)

b.sum<- data.frame(
  rbind(c(b.sum1$p.value,b.sum1$conf.int)
        ,c(b.sum2$p.value,b.sum2$conf.int)
        ,c(b.sum3$p.value,b.sum3$conf.int)
        ,c(b.sum4$p.value,b.sum4$conf.int)
        ,c(b.sum5$p.value,b.sum5$conf.int)
        ,c(b.sum6$p.value,b.sum6$conf.int)
        ,c(b.sum7$p.value,b.sum7$conf.int)
        ,c(b.sum8$p.value,b.sum8$conf.int)
        ,c(b.sum9$p.value,b.sum9$conf.int)
        ,c(b.sum10$p.value,b.sum10$conf.int)
        ,c(b.sum11$p.value,b.sum11$conf.int)
        ,c(b.sum12$p.value,b.sum12$conf.int)
        )
)
names(b.sum)<-c("p.value", "CI.lower","CI.upper")
b.sum$Month<- suicide$Month
b.sum$p<-prop

b.sum

plot(suicide$Suicides,col="blue")
plot(suicide$Month,rep(sum(suicide$Suicides)/12,12),add=T)
