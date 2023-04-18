# calculation of lan demets o brien fleming boundary

```
#################
# check by hand #
#################

library(mvtnorm)

rate1<-64/100 # assuming that sample size ratio is approximately the information ratio
rate2<-1

alpha2<-0.025
alpha1<-2*(1-pnorm(-qnorm(alpha2/2)/sqrt(64/100)))
# check: 2*(1-pnorm(-qnorm(alpha2/2)/sqrt(1)))

prob_diff1<-alpha1
prob_diff2<-alpha2-alpha1

z_boundary1<-qnorm(1-alpha1)
chisq_boundary1<-z_boundary1^2

mu<-c(0,0)
sigma<-rbind(
  c(1,sqrt(64/100)),
  c(sqrt(64/100),1))

search_z_boundary2<-function(z){
  pmvnorm(
    lower=c(-Inf,z),
    upper=c(z_boundary1,Inf),
    mean=mu,
    sigma=sigma)-prob_diff2
}

z_boundary2<-uniroot(search_z_boundary2,interval=c(-5,5))$root
chisq_boundary2<-z_boundary2^2
(1-pnorm(z_boundary2))

P(test1<sqrt(5.76))

(p1<-1-pnorm(sqrt(5.76)))
(p2<-pmvnorm(
  c(-Inf,sqrt(4.016)),c(sqrt(5.76),Inf),
  mean=c(0,0),sigma=rbind(c(1,sqrt(64/100)),c(sqrt(64/100),1))))
p1+p2

(p1<-1-pnorm(sqrt(5.76)))
(p2<-pmvnorm(
  c(-Inf,sqrt(4.08)),c(sqrt(5.76),Inf),
  mean=c(0,0),sigma=rbind(c(1,sqrt(64/100)),c(sqrt(64/100),1))))
p1+p2


#####################
# check by gsDesign #
#####################

library(gsDesign)
sfLDOF(0.025,64/100)$spend

##################
# check by rpact #
##################

library(rpact)
design1 <- getDesignGroupSequential(
  sided=1, alpha=0.025, beta=0.2,
  informationRates=c(64/100,1),
  typeOfDesign = "asOF")
summary(design1)
```

Notes:

- One-sided, alpha=0.025
- Two stages, test statistics (T1, T2)
- E(T1,T2)=(0,0)
- Var(T1,T2)=sigma
- Boundaries: B1, B2
- P(T1>=B1)=alpha1
- P(T1>=B1 or T2>=B2)=alpha2
- So, P(T1<B1 and T2>=B2)=alpha2-alpha1
- By formula, ```alpha1=2*(1-pnorm(-qnorm(alpha2/2)/sqrt(information ratio)))```

