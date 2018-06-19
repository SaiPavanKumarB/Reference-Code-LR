rm(list = ls(all=T))

#installation of maxnet library
#http://mxnet.readthedocs.io/en/latest/how_to/build.html?&toperStarEhJUS=1
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("mxnet")


setwd("D:/Srimatha_BABI/Decision Trees, C4.5, Deep Learning/handson day2/handson")
library(mxnet)
# require(mxnet)

# Data preparation
train<-read.csv('train_sample.csv')
test<-read.csv('test_sample.csv')
train<-data.matrix(train)
test<-data.matrix(test)
train.x<-train[,-1]
train.y<-train[,1]
train.x<-t(train.x/255) # Binarizing the data. Converting data to binary
test_org<-test
test<-test[,-1]
test<-t(test/255)
table(train.y)

# Model Architechture
data <- mx.symbol.Variable("data")


fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=10)
softmax <- mx.symbol.SoftmaxOutput(fc2, name="sm")


devices <- mx.cpu()
devices
mx.set.seed(0)
model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y, 
                                     ctx=devices, 
                                     num.round=250, 
                                     array.batch.size=100,
                                     learning.rate=0.07)
preds <- predict(model, test)
dim(preds)

pred.label <- max.col(t(preds)) - 1
table(pred.label)

table(test_org[,1],pred.label)
sum(diag(table(test_org[,1],pred.label)))/1000