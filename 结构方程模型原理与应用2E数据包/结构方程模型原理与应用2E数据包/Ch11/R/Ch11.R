#Chapter 11: Three Factor LGM
library(lavaan)
Ch11.lower <- 
'307.46
 296.52 377.21
 295.02 365.10 392.47
 291.02 355.88 358.25 376.84'
Ch11.cov <- getCov(Ch11.lower, names=c("RT1","RT2","RT3","RT4"))
Ch11.means <- c(37.48, 53.30, 54.82, 52.63)
names(Ch11.means)=c("RT1","RT2","RT3","RT4")
m1 <-'I =~ 1*RT1 + 1*RT2 + 1*RT3 + 1*RT4'
m2 <-'I =~ 1*RT1 + 1*RT2 + 1*RT3 + 1*RT4
      S =~ 0*RT1 + 1*RT2 + 2*RT3 + 3*RT4'
m3a<-'I =~ 1*RT1 + 1*RT2 + 1*RT3 + 1*RT4
      S =~ 0*RT1 +  RT2 +    RT3 + 1*RT4'
m3b<-'I =~ 1*RT1 + 1*RT2 + 1*RT3 + 1*RT4
      S =~ 0*RT1 +  RT2 +    RT3 + 1*RT4
      #constrain the error variance to equal
      RT1 ~~ r*RT1
      RT2 ~~ r*RT2
      RT3 ~~ r*RT3
      RT4 ~~ r*RT4'
m4 <-'I =~ 1*RT1 + 1*RT2 + 1*RT3 + 1*RT4
      S =~ 0*RT1 + 1*RT2 + 2*RT3 + 3*RT4
      Q =~ 0*RT1 + 1*RT2 + 4*RT3 + 9*RT4
      #constrain the error variance to equal
      RT1 ~~ r*RT1
      RT2 ~~ r*RT2
      RT3 ~~ r*RT3
      RT4 ~~ r*RT4'
m5 <-'I =~ 1*RT1 + 1*RT2 + 1*RT3 + 1*RT4
      S =~ 0*RT1 + 1*RT2 + 2*RT3 + 3*RT4
      Q =~ 0*RT1 + 1*RT2 + 4*RT3 + 9*RT4
      C =~ 0*RT1 + 1*RT2 + 8*RT3 +27*RT4
      #constrain the error variance to equal
      RT1 ~~ r*RT1
      RT2 ~~ r*RT2
      RT3 ~~ r*RT3
      RT4 ~~ r*RT4'
fit1 <-growth(m1, sample.cov=Ch11.cov,sample.means=Ch11.means,sample.nobs=200)
fit2 <-growth(m2, sample.cov=Ch11.cov,sample.means=Ch11.means,sample.nobs=200)
fit3a<-growth(m3a,sample.cov=Ch11.cov,sample.means=Ch11.means,sample.nobs=200,orthogonal=FALSE)
fit3b<-growth(m3b,sample.cov=Ch11.cov,sample.means=Ch11.means,sample.nobs=200,orthogonal = TRUE)
fit4 <-growth(m4, sample.cov=Ch11.cov,sample.means=Ch11.means,sample.nobs=200,orthogonal = TRUE)
fit5 <-growth(m5, sample.cov=Ch11.cov,sample.means=Ch11.means,sample.nobs=200,orthogonal = TRUE)
inspect(fit1)
summary(fit1, fit.measures=TRUE)
summary(fit2, fit.measures=TRUE)
summary(fit3a,fit.measures=TRUE)
summary(fit3b,fit.measures=TRUE)
summary(fit4, fit.measures=TRUE)
summary(fit5, fit.measures=TRUE)
library(semPlot)
semPaths(fit1,  mar = c(10, 5, 10, 5))
semPaths(fit2,  mar = c(10, 5, 10, 5))
semPaths(fit3a, mar = c(10, 5, 10, 5))
semPaths(fit3b, mar = c(10, 5, 10, 5))
semPaths(fit4,  mar = c(10, 5, 10, 5))
semPaths(fit5,  mar = c(10, 5, 10, 5))
semPaths(fit5,  color=list(lat = rgb(245, 253, 118, maxColorValue = 255),
                man = rgb(155, 253, 175, maxColorValue = 255)), mar = c(10, 1, 10, 1))