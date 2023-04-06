#Chapter 6: Higher-order CFA using raw data
library(lavaan)
Ch06.rawdata<-read.csv("c:\\SEM\\Ch06\\R\\ch06.csv", header=TRUE)
Ch06.cov<-cov(Ch06.rawdata)
Ch06.model<-' 
#set the first order factor structure
  FA  =~ tf1+tf2+tf3
  FB  =~ tl1+tl2+tl3
  FC  =~ w1+w2+w3+w4
#set the higher order factor structure
  Crea =~NA*FA+FB+FC
  Crea ~~1*Crea'
Ch06.fit <- cfa(model=Ch06.model, sample.cov=Ch06.cov, sample.nobs=802)
inspect(Ch06.fit)
summary(Ch06.fit, fit.measures=TRUE, standard=TRUE)
modificationIndices(Ch06.fit)

library(semPlot)
semPaths(Ch06.fit, 'std', mar = c(30, 5, 10, 5))
