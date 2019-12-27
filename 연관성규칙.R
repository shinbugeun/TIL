install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
data(Groceries)
summary(Groceries)


sort(itemFrequency(Groceries,type="absolute"),decreasing = T)
itemFrequencyPlot(Groceries,topN=10,type="absolute")
itemFrequencyPlot(Groceries,topN=10,type="relative")


rm(list = ls())
build<-read.csv("data/building.csv", header =T)
build[is.na(build)]<-0
build<-build[-1]
build
trans<-as.matrix(build, "Transaction")
rules1<-apriori(trans, parameter = list(supp=0.2, conf=0.6, target="rules"))
rules1

inspect(sort(rules1))

rules2<-subset(rules1, subset=lhs%pin%'보습학원'&confidence>0.7)
inspect(sort(rules2))

rules3<-subset(rules1, subset=rhs%pin%'편의점'&confidence>0.7)
rules3
inspect(sort(rules3))

