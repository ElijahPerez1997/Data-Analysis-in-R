p_1<-0.16
z_crit<- abs(qnorm(0.025, mean=0, sd=1))
n_1<-1014
se_1<-sqrt(p_1*(1-p_1)/n_1)
lb<-p_1-z_crit*se_1
ub<-p_1+z_crit*se_1
ci_1<-c(lb,ub)

p_2<- 0.2
se_2<- sqrt((p_1*(1-p_1)/n_1) +(p_2*(1-p_2)/n_1))
lb_2<- (p_1-p_2)-z_crit*se_2
ub_2<- (p_1-p_2)+z_crit*se_2
ci_2<-c(lb_2,ub_2)        
