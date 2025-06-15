data<-read.csv("data2794.csv",header=T)##Substitute data
head(data)

CV=function(n,Z=10,seed=888){
  z=rep(1:Z,ceiling(n/Z))[1:n];set.seed(seed);z=sample(z,n)
  mm=list();for (i in 1:Z) mm[[i]]=(1:n)[z==i];return(mm)}
n=nrow(data)
Z=10
mm=CV(n,Z)
D=1       ##10-crossing test
summary(data)

##regressing tree 10-crossing test
library(rpart.plot)
library(maptree)
colnames(data)<-c("单价","城区","卧室数","卫生间数","总楼层","所处楼层","建筑面积","房屋朝向","装修","是否邻近公交","一公里内中学数","是否邻近医院")
fit=rpart(单价~.,method='anova',data=data)
draw.tree(fit)
a=rpart(y~.,data)
rpart.plot(fit,type=2)

MSE=rep(0,Z);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
a=rpart(y~.,data=data[-m,])
MSE[i]=mean((data[m,D]-predict(a,data[m,]))^2)/M}
mean(MSE)###calculate test_set_NMSE

MSE=rep(0,Z);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
a=rpart(y~.,data[-m,])
MSE[i]=mean((data[-m,D]-predict(a,data[-m,]))^2)/M}
mean(MSE)###calculate train_set_NMSE


###bagging 10-crossing test
install.packages("ipred")
library(ipred)
MSE=rep(0,Z)
set.seed(1010);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
a=bagging(y~.,data=data[-m,])
MSE[i]=mean((data[m,D]-predict(a,data[m,]))^2)/M}
mean(MSE)###calculate test_set_NMSE

MSE=rep(0,Z)
set.seed(1010);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
a=bagging(y~.,data=data[-m,])
MSE[i]=mean((data[-m,D]-predict(a,data[-m,]))^2)/M}
mean(MSE)###calculate train_set_NMSE

##boosting 10-crossing test
install.packages("mboost")
library(mboost)
MSE=rep(0,Z)
set.seed(1010);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
a=mboost(y~btree(x1)+btree(x2)+btree(x3)+btree(x4)+btree(x5)+
           btree(x6)+btree(x7)+btree(x8)+btree(x9)+btree(x10)+btree(x11),data=data[-m,])
MSE[i]=mean((data[m,D]-predict(a,data[m,]))^2)/M}
mean(MSE)###calculate test_set_NMSE

library(mboost)
MSE=rep(0,Z)
set.seed(1010);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
a=mboost(y~btree(x1)+btree(x2)+btree(x3)+btree(x4)+btree(x5)+
           btree(x6)+btree(x7)+btree(x8)+btree(x9)+btree(x10)+btree(x11),data=data[-m,])
MSE[i]=mean((data[-m,D]-predict(a,data[-m,]))^2)/M}
mean(MSE)###calculate train_set_NMSE

##SVR 10-crossing test
install.packages("rminer")
library(rminer);set.seed(1010)
MSE=rep(0,Z);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
a=fit(y~.,data[-m,],model="svm")
MSE[i]=mean((data[m,D]-predict(a,data[m,]))^2)/M}
mean(MSE)###calculate test_set_NMSE

library(rminer);set.seed(1010)
MSE=rep(0,Z);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
a=fit(y~.,data[-m,],model="svm")
MSE[i]=mean((data[-m,D]-predict(a,data[-m,]))^2)/M}
mean(MSE)###calculate train_set_NMSE

##RF 10-crossing test
data<-read.csv("data.csv",header=T)##Substitute data
head(data)
summary(data)

data=na.omit(data)
library(randomForest);MSE=rep(0,Z)
set.seed(1010);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
a=randomForest(y~.,data=data[-m,])
MSE[i]=mean((data[m,D]-predict(a,data[m,]))^2)/M}
mean(MSE)###calculate test_set_NMSE

library(randomForest);MSE=rep(0,Z)
set.seed(1010);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
a=randomForest(y~.,data=data[-m,])
MSE[i]=mean((data[-m,D]-predict(a,data[-m,]))^2)/M}
mean(MSE)###calculate train_set_NMSE



#### RF Parameter tuning

install.packages("ggplot2")
install.packages("caret")
install.packages("lattice")
library(randomForest)
library(ggplot2)
library("caret")
#Enter the preset mtry, try to find ntree
ntree_fit<-randomForest(y~.,data=data,mtry=4,ntree=1000)
plot(ntree_fit)

###Importance ranking
data<-read.csv("data.csv",header=T)##Substitute data
head(data)
forest<-randomForest(y~.,data =data,importance=TRUE,ntree=200,mtry=5)
forest
importance<-importance(x=forest)
importance
colnames(data)<-c("单价","所属城区","卧室数","卫生间数","总楼层","所处楼层","建筑面积","房屋朝向","装修","是否邻近公交","附近中学个数","是否邻近医院")
rforest<-randomForest(单价~.,data =data,ntree=200,importance=TRUE,mtry=5)
#rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance


## Heterogeneity analysis of urban areas
data<-read.csv("lucheng.csv",header=T)##Substitute data
head(data)
colnames(data)<-c("单价","卧室数","卫生间数","总楼层","所处楼层","建筑面积","房屋朝向","装修","是否邻近公交","一公里内中学数","是否邻近医院")
rforest<-randomForest(单价~.,data =data,ntree=200,importance=TRUE,mtry=5)
rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance

par(mfrow=c(2,3))
barplot(height = c(2.812509,5.140836,6.696686,7.090581,19.763696),# Graph data (numerical vector)
        names.arg = c('trans',"area",'bed','dec','zlc'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '永嘉县二手房样本',  # main 
        horiz = TRUE,  # horizontally placing
)
barplot(height = c(15.353216,17.173198,18.171312 ,18.882450,42.436628),# Graph data (numerical vector)
        names.arg = c('dec',"area",'trans','toi','zlc'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '瓯海区二手房样本',  # main 
        horiz = TRUE,  # horizontally placing
)
barplot(height = c(10.1113690,11.4307463,12.7994444,14.1043914,25.4070567),# Graph data (numerical vector)
        names.arg = c('sch',"dec",'bed','area','zlc'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '龙湾区二手房样本',  # main
        horiz = TRUE,  # horizontally placing
)
barplot(height = c(8.8190284,9.6710978,14.0030294,18.9490885,22.3742101),# Graph data (numerical vector)
        names.arg = c('toi',"trans",'area','zlc','dec'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '瑞安市二手房样本',  # main
        horiz = TRUE,  # horizontally placing
)
barplot(height = c(9.477987,11.491677,13.959563,28.167931,30.362942),# Graph data (numerical vector)
        names.arg = c('toi',"trans",'sch','zlc','area'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '鹿城区二手房样本',  # main
        horiz = TRUE,  # horizontally placing
)
barplot(height = c(7.8405405,9.0719140,10.1101774 ,10.8601852,29.0964758),# Graph data (numerical vector)
        names.arg = c('bed',"dec",'sch','area','zlc'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '乐清市二手房样本',  # main
        horiz = TRUE,  # horizontally placing
)

## Heterogeneity analysis of total_layers
data<-read.csv("diceng.csv",header=T)##Substitute data
head(data)
colnames(data)<-c("单价","城区","卧室数","卫生间数","所处楼层","建筑面积","房屋朝向","装修","是否邻近公交","一公里内中学数","是否邻近医院")
rforest<-randomForest(单价~.,data =data,ntree=200,importance=TRUE,mtry=5)
rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance

par(mfrow=c(2,2))
barplot(height = c(2.15,2.18,4.17 ,7.56,7.64),# Graph data (numerical vector)
        names.arg = c('sch',"bed",'city','area','toi'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '低层住宅',  # main
        horiz = TRUE,  # horizontally placing
)
barplot(height = c(6.15,8.59,10.51 ,22.69,31.33),# Graph data (numerical vector)
        names.arg = c('bed',"dec",'toi','area','city'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '多层住宅',  # main
        horiz = TRUE,  # horizontally placing
)
barplot(height = c(7.21,9.23,9.89,20.45,48.62),# Graph data (numerical vector)
        names.arg = c('trans',"toi",'bed','area','city'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '中高层住宅',  # main
        horiz = TRUE,  # horizontally placing
)
barplot(height = c(26.82,26.99,36.48 ,38.79,73.81),# Graph data (numerical vector)
        names.arg = c('trans',"dec",'sch','area','city'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '高层住宅',  # main
        horiz = TRUE,  # horizontally placing
)


### Heterogeneity Analysis of House Types
data<-read.csv("xiaohu.csv",header=T)##Substitute data
head(data)
colnames(data)<-c("单价","城区","卧室数","卫生间数","总楼层","所处楼层","房屋朝向","装修","是否邻近公交","一公里内中学数","是否邻近医院")
rforest<-randomForest(单价~.,data =data,ntree=200,importance=TRUE,mtry=5)
rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance

par(mfrow=c(1,3))
barplot(height = c(12.29,16.26,16.56 ,32.32,54.9),# Graph data (numerical vector)
        names.arg = c('toi',"sch",'bed','zlc','city'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '小户型',  # main
        horiz = TRUE,  # horizontally placing
)

barplot(height = c(21.21,21.36,23.63 ,63.95,67.27),# Graph data (numerical vector)
        names.arg = c('trans',"sch",'dec','zlc','city'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '中户型',  # main
        horiz = TRUE,  # horizontally placing
)

barplot(height = c(11.38,15.24,15.38 ,27.35,40.38),# Graph data (numerical vector)
        names.arg = c('sch',"toi",'dec','zlc','city'),  # Column name
        family = 'Kai',  # Chinese font
        xlab = '重要程度',  # X_name
        ylab = '影响变量',  # Y_name
        main = '大户型',  # main
        horiz = TRUE, # horizontally placing
)




#### Partial effect analysis
data<-read.csv("data2794.csv",header=T)##Substitute data
head(data)
colnames(data)<-c("单价","所属城区","卧室数","卫生间数","总楼层","所处楼层","建筑面积","房屋朝向","装修","是否邻近公交","一公里内中学数","是否邻近医院")
rforest<-randomForest(单价~.,data =data,ntree=200,importance=TRUE,mtry=5)
rforest
varImpPlot(rforest)
importance<-importance(x=rforest)
importance
par(mfrow=c(2,2))
partialPlot(rforest,data,x.var = 总楼层)###Partial effect graph
partialPlot(rforest,data,x.var = 建筑面积)
partialPlot(rforest,data,x.var = 附近中学个数)
partialPlot(rforest,data,x.var = 卫生间数)




