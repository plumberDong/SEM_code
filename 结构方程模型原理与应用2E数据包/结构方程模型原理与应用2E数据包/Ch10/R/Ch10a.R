#Chapter 10: Mean structure SEM using raw data
library(lavaan)
Ch10a.data  <-read.csv("c:\\SEM\\Ch10\\R\\Ch10a.csv", header=TRUE)
Ch10a.model<-'FA =~ NA*A1 + A2 + A3
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
              FA ~ c(0,NA)*1
              FB ~ c(0,NA)*1
              FC ~ c(0,NA)*1
              FD ~ c(0,NA)*1
              FE ~ c(0,NA)*1
              FF ~ c(0,NA)*1'
Ch10a.fit<-cfa(model=Ch10a.model, 
               data=Ch10a.data,
               group= "gender",  
               meanstructure=TRUE,
               group.equal=c("loadings","residuals","intercepts"))
inspect(Ch10a.fit)
summary(Ch10a.fit, fit.measures=TRUE, standard=TRUE)

library(semPlot)
semPaths(Ch10a.fit)
semPaths(Ch10a.fit, 'est', mar = c(10, .5, 10, 0.5))
