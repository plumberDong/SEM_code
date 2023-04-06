#Chapter 12: Mediation and Moderation analysis
library(lavaan)
Ch12.data <- read.csv("c:\\SEM\\Ch12\\R\\Ch12.csv", header=TRUE)
#Mediation analysis
  model1<-'Y  ~ c*X + b*ME
           ME ~ a*X
  # indirect effect (a*b)
    ab := a*b
  # total effect
    total := c + (a*b)'
  
#Moderation analysis
  model2<-'Y  ~ a*X + b*MO + c*XMO'

#Moderated Mediation analysis
  model3<-'Y  ~ a*X + b*MO + c*XMO + d*ME + f*MOME
           ME ~ g*X + h*MO + i*XMO
  # indirect effect (a*d)
             ad := a*d'

fit1 <- sem(model1,data=Ch12.data)
fit2 <- sem(model2,data=Ch12.data)
fit3 <- sem(model3,data=Ch12.data)

inspect(fit1)
summary(fit1, fit.measures=TRUE)
inspect(fit2)
summary(fit2, fit.measures=TRUE)
inspect(fit3)
summary(fit3, fit.measures=TRUE)

library(semPlot)
semPaths(fit1,        mar = c(40, 20, 40, 20))
semPaths(fit1, 'est', mar = c(40, 20, 40, 20))
semPaths(fit2,        mar = c(40, 20, 40, 20))
semPaths(fit2, 'est', mar = c(40, 20, 40, 20))
semPaths(fit3,        mar = c(40, 20, 40, 20))
semPaths(fit3, 'est', mar = c(40, 20, 40, 20))