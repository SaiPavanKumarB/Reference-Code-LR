rm(list = ls(all=T))

#installation of mxnet library
#http://mxnet.readthedocs.io/en/latest/how_to/build.html?&toperStarEhJUS=1
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("mxnet")


setwd("C:/Users/gmanish/Desktop/current/Teaching/isb/Session2")
library(mxnet)
# require(mxnet)

# Data preparation
train<-read.csv('train_sample.csv')
test<-read.csv('test_sample.csv')
train<-data.matrix(train)
test<-data.matrix(test)
train.x<-train[,-1]
train.y<-train[,1]
train.x<-t(train.x/255)
test_org<-test
test<-test[,-1]
test<-t(test/255)
table(train.y)

# Model Architechture
data <- mx.symbol.Variable("data")

conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=20)
tanh1 <- mx.symbol.Activation(data=conv1, act_type="tanh")									 
pool1 <- mx.symbol.Pooling(data=tanh1, pool_type="max",									 
                          kernel=c(2,2), stride=c(2,2))									 
# second conv									 
conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=50)									 
tanh2 <- mx.symbol.Activation(data=conv2, act_type="tanh")									 
pool2 <- mx.symbol.Pooling(data=tanh2, pool_type="max",									 
                          kernel=c(2,2), stride=c(2,2))									 
# first fullc									 
flatten <- mx.symbol.Flatten(data=pool2)									 
fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)									 
tanh3 <- mx.symbol.Activation(data=fc1, act_type="tanh")									 
# second fullc									 
fc2 <- mx.symbol.FullyConnected(data=tanh3, num_hidden=10)									 
# loss									 
lenet <- mx.symbol.SoftmaxOutput(data=fc2)									 
									 							 
train.array <- train.x
dim(train.array) <- c(28, 28, 1, ncol(train.x))
test.array <- test
dim(test.array) <- c(28, 28, 1, ncol(test))
									 
device.cpu <- mx.cpu()
mx.set.seed(0)
tic <- proc.time()
model <- mx.model.FeedForward.create(lenet, X=train.array, y=train.y,
                                     ctx=device.cpu, num.round=10, array.batch.size=100,
                                     learning.rate=0.05, momentum=0.9, wd=0.00001,
                                     eval.metric=mx.metric.accuracy,
                                     epoch.end.callback=mx.callback.log.train.metric(100))
print(proc.time() - tic)

preds <- predict(model, test.array)
dim(preds)

pred.label <- max.col(t(preds)) - 1
table(pred.label)

table(test_org[,1],pred.label)
sum(diag(table(test_org[,1],pred.label)))/1000

