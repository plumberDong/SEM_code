#Chapter 10b: Mean structure SEM on Commit-Outcome study
library(lavaan)
Ch10b.data  <-read.csv("c:\\SEM\\Ch10\\R\\Ch10b.csv", header=TRUE)
Ch10b.model<- '
  # measurement model
      FY =~ y1 + y2 + y3
      FX =~ x1 + x2 + x3
  # structural model
      FY ~ FX
  # constain latent mean    
      FX ~ c(0,NA)*1
      FY ~ c(0,NA)*1'
Ch10b.fit<-sem(model=Ch10b.model, 
               data=Ch10b.data,
               group= "grp",
               meanstructure=TRUE,
               group.equal=c("loadings","intercepts"))
inspect(Ch10b.fit)
summary(Ch10b.fit, fit.measures=TRUE, standard=TRUE)

library(semPlot)
semPaths(Ch10b.fit, mar = c(5, 30, 5, 30))
semPaths(Ch10b.fit, 'est', mar = c(5, 30, 5, 30))
