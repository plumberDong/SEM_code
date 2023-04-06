#Chapter 5: Confirmatory Factor Analysis using raw data
library(lavaan)
ch05.rawdata<-read.csv("c:\\SEM\\Ch05\\R\\Ch05.csv", header=TRUE)
ch05.cov<-cov(ch05.rawdata)
ch05.model<-'
#define the measurement model  
  FA =~ L11*A1 + L21*A2 + L31*A3
  FB =~ L12*B1 + L22*B2 + L32*B3
  FC =~ L13*C1 + L23*C2 + L33*C3
  FD =~ L14*D1 + L24*D2 + L34*D3
  FE =~ L15*E1 + L25*E2 + L35*E3
  FF =~ L16*F1 + L26*F2 + L36*F3'
#define CFA
ch05a.fit<-cfa(model=ch05.model,sample.cov=ch05.cov,sample.nobs=313,
               orthogonal = FALSE)
ch05b.fit<-cfa(model=ch05.model,sample.cov=ch05.cov,sample.nobs=313,
               orthogonal = TRUE)

inspect(ch05a.fit)
inspect(ch05b.fit)
summary(ch05a.fit, fit.measures=TRUE, standard=TRUE)
summary(ch05b.fit, fit.measures=TRUE, standard=TRUE)
modificationIndices(ch05a.fit)

library(semPlot)
semPaths(ch05a.fit, mar = c(30, 1, 30, 1))
semPaths(ch05a.fit, 'std', mar = c(30, 1, 30, 1))
semPaths(ch05b.fit, mar = c(30, 1, 30, 1))
semPaths(ch05b.fit, 'std', mar = c(30, 1, 30, 1))