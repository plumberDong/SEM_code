#Chapter 1: R example
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
c("Y1","Y3","Y4","Y5","Y2","X1","X2","X3","X4","X5"))
Ch08.model<-'
# measurement model
    KSI1 =~ Y1 +Y2
    KSI2 =~ Y3 + Y4 + Y5
    ETA1  =~ X1  + X2  + X3
    ETA2  =~ X4  + X5
    X1+X2+X3+X4+X5+Y1+Y2+Y3+Y4+Y5 ~~ 1
# structural model
    KSI2 ~ a1*ETA1 + c1*ETA2
    KSI1 ~ a2*ETA1 + b1*KSI2 + c2*ETA2
    KSI1 +KSI2+ETA1+ETA2 ~ 1 
    
#set the correlation between variables to free
    ETA2  ~~ ETA1
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