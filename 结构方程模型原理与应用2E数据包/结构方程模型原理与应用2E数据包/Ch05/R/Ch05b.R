#Chapter 5: Confirmatory Factor Analysis using raw data
library(lavaan)
ch05.rawdata<-read.csv("c:\\SEM\\Ch05\\R\\Ch05.csv", header=TRUE)
ch05.cov<-cov(ch05.rawdata)
ch05.model<-' 
FA=~NA*A1+A2+A3
FB=~NA*B1+B2+B3
FC=~NA*C1+C2+C3
FD=~NA*D1+D2+D3
FE=~NA*E1+E2+E3
FF=~NA*F1+F2+F3
FA~~1*FA
FB~~1*FB 
FC~~1*FC 
FD~~1*FD
FE~~1*FE
FF~~1*FF'
ch05.fit <- cfa(model=ch05.model, sample.cov=ch05.cov, sample.nobs=313)
inspect(ch05.fit)
summary(ch05.fit, fit.measures=TRUE, standard=TRUE)
parameterEstimates(ch05.fit, standardized = TRUE)
fitted(ch05.fit)
modificationIndices(ch05.fit)
