rm(list=ls(all=TRUE)) # Cleaning earlier environment/datasets
setwd("D:/Srimatha_BABI/Decision Trees & C4.5/handson/handson")
univ = read.table('UnivBank.csv', header=T,sep=',',
                  col.names=c('ID','age','exp','inc',
                              'zip','family','ccavg',
                              'edu','mortgage','loan',
                              'securities','cd',
                              'online','cc'))

# removing the id, experience and Zip, 
# experience is correlated to age
univ=univ[,-c(1,3,5)]

univ$family=as.factor(univ$family)
univ$edu=as.factor(univ$edu)
univ$mortgage=as.factor(univ$mortgage)
univ$loan=as.factor(univ$loan)
univ$securities=as.factor(univ$securities)
univ$cd=as.factor(univ$cd)
univ$online=as.factor(univ$online)
univ$cc=as.factor(univ$cc)

#convert mortgage as numeric
univ$mortgage=as.numeric(univ$mortgage)

set.seed(123)
rows = seq(1, nrow(univ), 1)
trainRows = sample(rows, nrow(univ) * .6)
testRows = rows[-(trainRows)]


train = univ[trainRows,] 
test=univ[testRows,] 

rm(univ,rows,testRows,trainRows)

install.packages("C50")
library(C50)
dtC50 = C5.0(loan ~ ., data = train, rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)

dtC50$size
dtC50 = C5.0(loan ~ ., data = train, rules=FALSE)
summary(dtC50)
plot(dtC50)

a=table(train$loan, predict(dtC50, newdata=train, type="class"))
a
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
rcTrain
a=table(test$loan, predict(dtC50, newdata=test, type="class"))
a
rcTest=(a[2,2])/(a[2,1]+a[2,2])*100
rcTest

