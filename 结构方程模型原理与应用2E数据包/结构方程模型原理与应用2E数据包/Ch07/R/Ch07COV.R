#Chapter 7: Path Analysis using covariance matrix
library(lavaan)
Ch07.lower <- 
'.402 
 .466 2.537
 .185 .667 .649
 .174 .704 .368 .910 
 .167 .578 .272 .421 .693 
 .157 .483 .207 .330 .320 .600 
 .221 .730 .346 .463 .392 .387 .779,
 .172 .647 .370 .270 .369 .250 .333 .768,
1.406 2.883 .949 .154 .347 .274 .527 1.116 85.137'
Ch07.cov <- getCov(Ch07.lower, names = 
c("OUTCOME","COMMIT","VALUE","JOBSTYLE","TEAMWORK","LEADERSH","LEARNING","ENVIRONM","TENURE"))
Ch07.model<-'
COMMIT ~ a1*VALUE+a2*JOBSTYLE+a3*TEAMWORK+a4*LEADERSH+a5*LEARNING+a6*ENVIRONM+a7*TENURE
OUTCOME ~ c7*TENURE+b*COMMIT

#set the correlation between variables to free
VALUE ~~ JOBSTYLE 
VALUE ~~ TEAMWORK 
VALUE ~~ LEADERSH 
VALUE ~~ LEARNING
VALUE ~~ ENVIRONM 
JOBSTYLE ~~ TEAMWORK 
JOBSTYLE ~~ LEADERSH 
JOBSTYLE ~~ LEARNING 
JOBSTYLE ~~ ENVIRONM 
TEAMWORK ~~ LEADERSH 
TEAMWORK ~~ LEARNING 
TEAMWORK ~~ ENVIRONM
LEADERSH ~~ LEARNING 
LEADERSH ~~ ENVIRONM 
LEARNING ~~ ENVIRONM 

#set the correlation between variables to zero
TENURE ~~ 0*VALUE 
TENURE ~~ 0*JOBSTYLE 
TENURE ~~ 0*TEAMWORK 
TENURE ~~ 0*LEADERSH 
TENURE ~~ 0*LEARNING
TENURE ~~ 0*ENVIRONM

#define indirect effect (a*b)
a1b := a1*b
a2b := a2*b
a3b := a3*b
a4b := a4*b
a5b := a5*b
a6b := a6*b
a7b := a7*b'

Ch07.fit <- sem(Ch07.model, sample.cov=Ch07.cov, sample.nobs=281)
inspect(Ch07.fit)
summary(Ch07.fit, fit.measures=TRUE, standard=TRUE)

library(semPlot)
semPaths(Ch07.fit, 'std', mar = c(40, 20, 40, 20))
