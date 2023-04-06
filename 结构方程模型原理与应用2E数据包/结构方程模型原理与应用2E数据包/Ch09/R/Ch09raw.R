#Chapter 9: Mutliple group CFA using raw data
library(lavaan)
ch09.male  <-read.csv("c:\\SEM\\Ch09\\R\\Ch09m.csv", header=TRUE)
ch09.female<-read.csv("c:\\SEM\\Ch09\\R\\Ch09f.csv", header=TRUE)
ch09m.cov<-cov(ch09.male)
ch09f.cov<-cov(ch09.female)
#Combine the covariances and sample sizes
ch09all.cov <- list(MCOV=ch09m.cov, FCOV=ch09f.cov)
ch09all.n   <- list(M=170, F=142)
ch09.model<-'FA =~ NA*A1 + A2 + A3
             FB =~ NA*B1 + B2 + B3
             FC =~ NA*C1 + C2 + C3
             FD =~ NA*D1 + D2 + D3
             FE =~ NA*E1 + E2 + E3
             FF =~ NA*F1 + F2 + F3
             FA~~1*FA 
             FB~~1*FB 
             FC~~1*FC 
             FD~~1*FD 
             FE~~1*FE 
             FF~~1*FF'

#configural invariance
ch09MG1.fit <- cfa(model=ch09.model, sample.cov=ch09all.cov, sample.nobs=ch09all.n)
inspect(ch09MG1.fit)
summary(ch09MG1.fit, fit.measures=TRUE, standard=TRUE)

#factor loading invariance
ch09MG2.fit <- cfa(model=ch09.model, sample.cov=ch09all.cov, sample.nobs=ch09all.n,
                   group.equal=c("loadings"))
inspect(ch09MG2.fit)
summary(ch09MG2.fit, fit.measures=TRUE, standard=TRUE)

#factor loading + residuals invariance
ch09MG3.fit <- cfa(model=ch09.model, sample.cov=ch09all.cov, sample.nobs=ch09all.n,
                   group.equal=c("loadings", "residuals"))
inspect(ch09MG3.fit)
summary(ch09MG3.fit, fit.measures=TRUE, standard=TRUE)

#factor loading + residuals + lv.covariances invariance
ch09MG4.fit <- cfa(model=ch09.model, sample.cov=ch09all.cov, sample.nobs=ch09all.n,
                   group.equal=c("loadings", "residuals", "lv.covariances"))
inspect(ch09MG4.fit)
summary(ch09MG4.fit, fit.measures=TRUE, standard=TRUE)

library(semPlot)
semPaths(ch09MG1.fit, mar = c(30, 1, 20, 1))
semPaths(ch09MG1.fit, 'est', mar = c(30, 1, 20, 1))
