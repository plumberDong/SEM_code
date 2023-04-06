#Chapter 7: Path Analysis using raw data
library(lavaan)
Ch07.rawdata<-read.csv("c:\\SEM\\Ch07\\R\\ch07.csv", header=TRUE)
Ch07.model<-'
#set the structure model
  COMMIT~ a1*VALUE+a2*JOBSTYLE+a3*TEAMWORK+a4*LEADERSH
          +a5*LEARNING+a6*ENVIRONM+a7*TENURE
  OUTCOME~c*TENURE+b*COMMIT
#set the correlation between variables to free
  VALUE    ~~ JOBSTYLE 
  VALUE    ~~ TEAMWORK 
  VALUE    ~~ LEADERSH 
  VALUE    ~~ LEARNING
  VALUE    ~~ ENVIRONM 
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
  TENURE   ~~ 0*VALUE 
  TENURE   ~~ 0*JOBSTYLE 
  TENURE   ~~ 0*TEAMWORK 
  TENURE   ~~ 0*LEADERSH 
  TENURE   ~~ 0*LEARNING
  TENURE   ~~ 0*ENVIRONM
#define indirect effect (a*b)
  a1b := a1*b
  a2b := a2*b
  a3b := a3*b
  a4b := a4*b
  a5b := a5*b
  a6b := a6*b
  a7b := a7*b
#define total effect
  total := c+a1b+a2b+a3b+a4b+a5b+a6b'

Ch07.fit <- sem(Ch07.model, data=Ch07.rawdata)
inspect(Ch07.fit)
summary(Ch07.fit, fit.measures=TRUE, standard=TRUE)

library(semPlot)
semPaths(Ch07.fit, 'std', mar = c(40, 10, 30, 20))
