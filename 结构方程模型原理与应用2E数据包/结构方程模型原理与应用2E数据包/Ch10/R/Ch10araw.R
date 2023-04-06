#Chapter 10: Mean structure SEM using raw data
library(lavaan)
Ch10.male  <-read.csv("c:\\SEM\\Ch10\\R\\Ch10m.csv", header=TRUE)
Ch10.female<-read.csv("c:\\SEM\\Ch10\\R\\Ch10f.csv", header=TRUE)
Ch10f.mean<-read.csv("c:\\SEM\\Ch10\\R\\Ch10fmean.csv", header=TRUE)
Ch10m.mean<-read.csv("c:\\SEM\\Ch10\\R\\Ch10mmean.csv", header=TRUE)
Ch10m.cov  <-cov(Ch10.male)
Ch10f.cov  <-cov(Ch10.female)
#Combine the means, covariances and sample sizes
Ch10all.mean<- list(MM=Ch10m.mean, FM=Ch10f.mean)
Ch10all.cov <- list(MCOV=Ch10m.cov, FCOV=Ch10f.cov)
Ch10all.n   <- list(M=170, F=142)
Ch10.model<-'FA =~ A1 + A2 + A3
             FB =~ B1 + B2 + B3
             FC =~ C1 + C2 + C3
             FD =~ D1 + D2 + D3
             FE =~ E1 + E2 + E3
             FF =~ F1 + F2 + F3 
# intercepts
  A1 ~ 1
  A2 ~ 1
  A3 ~ 1
  B1 ~ 1
  B2 ~ 1
  B3 ~ 1
  C1 ~ 1
  C2 ~ 1
  C3 ~ 1
  #D1 ~ 1
  #D2 ~ 1
  #D3 ~ 1
  #E1 ~ 1
  #E2 ~ 1
  #E3 ~ 1
  #F1 ~ 1
  #F2 ~ 1
  #F3 ~ 1
#FA ~ *1
#FB ~ 1
#FC ~ 1
#FD ~ 1
#FE ~ 1
#FF ~ 1
'
library(semTools)
Ch10inv.fit <- measurementInvariance(model=Ch10.model, 
                   sample.mean=Ch10all.mean, 
                   sample.cov=Ch10all.cov, 
                   sample.nobs=Ch10all.n, 
                   meanstructure=TRUE
)
inspect(Ch10inv.fit)
summary(Ch10inv.fit, fit.measures=TRUE, standard=TRUE)

#configural invariance
Ch10MG1.fit <- cfa(model=Ch10.model, 
                   sample.mean=Ch10all.mean, 
                   sample.cov=Ch10all.cov, 
                   sample.nobs=Ch10all.n, 
                   meanstructure=TRUE
                   )
inspect(Ch10MG1.fit)
summary(Ch10MG1.fit, fit.measures=TRUE, standard=TRUE)

#factor loading + residuals + lv.covariances invariance
Ch10MG4.fit <- cfa(model=Ch10.model, sample.cov=Ch10all.cov, sample.nobs=Ch10all.n,
                   group.equal=c("loadings", "residuals", "lv.covariances", "means", "intercepts"))
inspect(Ch10MG4.fit)
summary(Ch10MG4.fit, fit.measures=TRUE, standard=TRUE)

