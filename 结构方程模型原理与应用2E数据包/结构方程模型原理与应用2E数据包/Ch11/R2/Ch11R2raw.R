#Chapter 9: Mutliple group CFA using raw data
library(lavaan)
Ch11R2  <-read.csv("c:\\SEM\\Ch11\\R2\\Ch11R2.csv", header=TRUE)
Ch11R2.cov<-cov(Ch11R2)
Ch11R2.model<-'FA =~ NA*A1 + A2 + A3
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
             FF~~1*FF
A1+A2+A3 ~1
'
Ch11R2.fit <- cfa(model=Ch11R2.model, 
                   data=Ch11R2, 
                   #sample.nobs=ch11R2.n
                   meanstructure=TRUE
                   )
inspect(Ch11R2.fit)
summary(Ch11R2.fit, fit.measures=TRUE, standard=TRUE)

