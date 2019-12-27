movie <- read.csv("C:/ML/data/movie.csv", header = T)
library(e1071)
nm <- naiveBayes(movie[1:5], movie$장르, laplace = 0)
head(movie)

result <- predict(nm, movie[1:5])
sum(movie$장르!=result)
result

install.packages("caret")
library(caret)
idx <- createDataPartition(iris$Species,p=0.7,list=F)
iris_train <- iris[idx,]
iris_test <- iris[-idx,]
table(iris_train$Species)
library(nnet)
iris_train_scale <- as.data.frame(sapply(iris_train[,-5],scale))
iris_test_scale <- as.data.frame(sapply(iris_test[,-5],scale))
iris_train_scale$Species <- iris_train$Species
iris_test_scale$Species <- iris_test$Species
nnet.result <- nnet(Species~.,iris_train_scale,size=3)
nnet.pred <- predict(nnet.result,iris_test_scale,type="class")
table(nnet.pred,iris_test$Species)

prob <- read.csv("C:/ML/data/problem.csv", heade = T, stringsAsFactors = F)
for (i in 1:30){
  prob[i] <- prob[i]*1/5
}
head(prob)
prob$accident2 <- with(prob, ifelse(accident=="suicide"|accident=="violence",1,0))
head(prob)

library(nnet)
prob<-prob[-31]
m1 <- nnet(accident2~.,data=prob,size=10)
r1 <- predict(m1,prob)
head(r1)

cbind(prob$accident2, r1>0.5)
sum(as.numeric(r1>0.5)!=prob$accident2)


install.packages("neuralnet")
library(neuralnet)
xnam <- paste0("ans",1:30)
fmla <- as.formula(paste("accident2~",paste(xnam,collapse = "+")))
m2 <- neuralnet(fmla, data=prob, hidden=10)
plot(m2)
