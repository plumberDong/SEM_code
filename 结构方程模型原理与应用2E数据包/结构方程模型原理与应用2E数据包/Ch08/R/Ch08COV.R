#Chapter 8: Hybrid SEM Analysis using covariance matrix
library(lavaan)
Ch08.lower <- 
'17.711
1.843 0.404
0.801 0.146 0.374
1.243 0.227 0.110 0.589
1.692 0.226 0.115 0.208 0.393
7.518 1.074 0.301 0.531 0.741 7.565
7.585 1.062 0.538 0.609 0.806 5.202 7.046
6.499 0.906 0.388 0.434 0.664 4.530 4.626 6.335
3.020 0.369 0.237 0.322 0.196 1.947 1.524 1.649 4.482
2.663 0.318 0.307 0.335 0.326 2.092 1.824 1.662 2.335 2.982'
Ch08.cov <- getCov(Ch08.lower, names = 
c("CREAT","SEFF1","SEFF2","SEFF3","SEFF4","PER1","PER2","PER3","SOC1","SOC2"))
Ch08.model<-'
# measurement model
    CREA =~ 1*CREAT
    SEFF =~ SEFF1 + SEFF2 + SEFF3 + SEFF4
    PER  =~ PER1  + PER2  + PER3
    SOC  =~ SOC1  + SOC2
# structural model
    SEFF ~ a1*PER + c1*SOC
    CREA ~ a2*PER + b1*SEFF + c2*SOC
#set the correlation between variables to free
    SOC  ~~ PER
#define indirect effect (a*b)
    a1b1 := a1*b1
    c1b1 := c1*b1
#define total effect
    total := a1+a2+b1+c1+c2+a1b1+c1b1'
Ch08.fit <- sem(Ch08.model, sample.cov=Ch08.cov, sample.nobs=250)
inspect(Ch08.fit)
summary(Ch08.fit, fit.measures=TRUE, standard=TRUE)
library(semPlot)
semPaths(Ch08.fit, mar = c(5, 10, 5, 10))
semPaths(Ch08.fit, 'std', mar = c(5, 10, 5, 10))