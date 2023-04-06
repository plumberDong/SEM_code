#Chapter 11: Salary data LGM
library(lavaan)
library(semPlot)
salary.data <- read.csv("c:\\SEM\\Ch11\\R\\salary.csv", header=TRUE)
model1<-'I =~ 1*Y1 + 1*Y2 + 1*Y3 + 1*Y4 + 1*Y5 + 1*Y6'
model2<-'I =~ 1*Y1 + 1*Y2 + 1*Y3 + 1*Y4 + 1*Y5 + 1*Y6
         S =~ 0*Y1 + 1*Y2 + 2*Y3 + 3*Y4 + 4*Y5 + 5*Y6'
model3<-'I =~ 1*Y1 + 1*Y2 + 1*Y3 + 1*Y4 + 1*Y5 + 1*Y6
         S =~ 0*Y1 + 1*Y2 +   Y3 +   Y4 +   Y5 +  Y6
         Y1 ~~ r*Y1
         Y2 ~~ r*Y2
         Y3 ~~ r*Y3
         Y4 ~~ r*Y4
         Y5 ~~ r*Y5
         Y6 ~~ r*Y6
'
model4<-'I =~ 1*Y1 + 1*Y2 + 1*Y3 + 1*Y4 + 1*Y5 + 1*Y6
         S =~ 0*Y1 + 1*Y2 + 2*Y3 + 3*Y4 + 4*Y5 + 5*Y6
         Q =~ 0*Y1 + 1*Y2 + 4*Y3 + 9*Y4 +16*Y5 +25*Y6'
model5<-'I =~ 1*Y1 + 1*Y2 + 1*Y3 + 1*Y4 + 1*Y5 + 1*Y6
         S =~ 0*Y1 + 1*Y2 + 2*Y3 + 3*Y4 + 4*Y5 + 5*Y6
         Q =~ 0*Y1 + 1*Y2 + 4*Y3 + 9*Y4 +16*Y5 +25*Y6
         C =~ 0*Y1 + 1*Y2 + 8*Y3 +27*Y4 +196*Y5 +225*Y6'
fit1 <- growth(model1,data=salary.data)
fit2 <- growth(model2,data=salary.data)
fit3 <- growth(model3,data=salary.data)
fit4 <- growth(model4,data=salary.data)
fit5 <- growth(model5,data=salary.data)
inspect(fit5)
summary(fit1, fit.measures=TRUE)
summary(fit2, fit.measures=TRUE)
summary(fit3, fit.measures=TRUE)
summary(fit4, fit.measures=TRUE)
summary(fit5, fit.measures=TRUE)
semPaths(fit1)
semPaths(fit2)
semPaths(fit3)
semPaths(fit4)
semPaths(fit5)

