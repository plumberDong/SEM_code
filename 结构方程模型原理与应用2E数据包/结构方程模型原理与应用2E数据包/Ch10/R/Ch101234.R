#Chapter 10: Mean structure SEM using raw data
library(lavaan)

Ch10.female<-read.csv("c:\\SEM\\Ch10\\R\\Ch10f.csv", header=TRUE)
Ch10.mean<-read.csv("c:\\SEM\\Ch10\\R\\Ch10fmean.csv", header=TRUE)

Ch10.cov  <-cov(Ch10.female)
Ch10all.mean<- list(MM=Ch10.mean)
Ch10all.cov <- list(MCOV=Ch10.cov)

Ch10.model<-'FA =~ A1 + A2 + A3
             FB =~ B1 + B2 + B3
             FC =~ C1 + C2 + C3
             FD =~ D1 + D2 + D3
             FE =~ E1 + E2 + E3
             FF =~ F1 + F2 + F3 
'
Ch10MG1.fit <- cfa(model=Ch10.model, 
                   sample.mean=Ch10all.mean, 
                   sample.cov=Ch10all.cov, 
                   sample.nobs=142, 
                   meanstructure=TRUE
                   )
inspect(Ch10MG1.fit)
summary(Ch10MG1.fit, fit.measures=TRUE, standard=TRUE)

