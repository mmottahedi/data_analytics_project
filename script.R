# set cwd
setwd("/home/mfc/cwd/Data_project/Data_project")
set.seed(1234)

#load packages
require(ggplot2)
library(tree)
library(readr)
library(randomForest)
library(gbm)

#load data
train <- read.csv("train.csv",header = T )#, stringsAsFactors=FALSE)
test <- read.csv("test.csv")

submission.tree <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)

submission.rf <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)

submission.bagging <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)

submission.boost <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)


#hist(train$feat_1)


# make train and test data from the the given train model

train.n <- sample(1:nrow(train),nrow(train)*.8)
data.test<- train[-train.n,]

#tree based method

tree.otto = tree(target ~ . -id, data=train,subset=train.n)

# plot tree
#par(mfrow = c(1 ,1))
#plot(tree.otto)
#text(tree.otto,pretty=0)
#print(summary(tree.otto))

#prediction of the test data set
tree.pred <- predict( tree.otto,data.test,type="class")
pred.table <- table(tree.pred,data.test$target)
# error rate
sum(pred.table* diag(9))/sum(pred.table)

# perfoming crossvalidation and pruning the tree to get better prediction

cv.trees = cv.tree(tree.otto,FUN=prune.misclass)
pruned.tree <- prune.misclass(tree.otto,best=cv.trees$size[which.min(cv.trees$dev)])

# make plots for pruned tree
# par(mfrow = c(1 ,1))
# plot(pruned.tree)
# text(pruned.tree,pretty=0)
# par(mfrow = c(1 ,2))
# plot(cv.trees$size , cv.trees$dev , type ="b")
# plot(cv.trees$k , cv.trees$dev , type ="b")

#  calculating prediction rate for pruned tree
tree.pruned.pred <- predict( pruned.tree,data.test,type="class")
pred.pruned.table <- table(tree.pruned.pred,data.test$target)
sum(pred.pruned.table* diag(9))/sum(pred.pruned.table)

#make submission file for competition

submission.tree[,2:10] <- predict( pruned.tree,test,type="vector")
write.csv(submission.tree,"tree.submission.csv")

# Bagging

#bag.otto <- randomForest(target ~.- id, data=train, subset=train.n ,mtry=93,importance=T)

pred.bagging <- predict(bag.otto,data.test,type="class")
pred.bagging.table <- table(pred.bagging,data.test$target)
sum(pred.bagging.table* diag(9))/sum(pred.bagging.table)

#make submission with bagging
submission.bagging[,2:10] <- predict( bag.otto,test,type="prob")
write.csv(submission.bagging,"bag.submission.csv")

#random FOrest with m = 10

#rf <- randomForest(train[train.n,c(-1,-95)], as.factor(train$target[train.n]), mtry=10 ,importance=TRUE)

pred.rf <- predict(rf,data.test)

pred.rf.table <- table(pred.rf,data.test$target)
sum(pred.rf.table* diag(9))/sum(pred.rf.table)


#make submission with random forest
submission.rf[,2:10] <- (predict(rf, test[,-1], type="prob"))
write.csv(submission.rf,"rf.submission.csv")

# make 2 importance plot
varImpPlot(rf)


imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
        geom_bar(stat="identity", fill="#53cfff") +
        coord_flip() +
        theme_light(base_size=20) +
        xlab("Importance") +
        ylab("") +
        ggtitle("Random Forest Feature Importance\n") +
        theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p, height=20, width=8, units="in")


# boosting
num.tree = 500
boost <- gbm(target ~ .-id ,data=train[train.n,], n.trees =num.tree, interaction.depth = 1  )

pred.boost <- predict(boost ,data.test,n.trees = num.tree)
pred.boost <- max.col(abs(pred.boost[,,1]))

# target = data.frame(id=data.test$id,t=NA)
# for (i in 1:length(data.test$target)){
#        target$t[i] = strsplit(as.character(data.test$target),"_")[[i]][2]
# }
#
target <- read.csv("test_class_index.csv")

# pred.boost.table <- table(pred.boost,target$t)
# sum(pred.boost.table* diag(9))/sum(pred.boost.table)
sum(ifelse(pred.boost == target$t,1,0))/length(target$t)

#make submission with random forest
submission.boost[,2:10] <- abs(predict(boost, test[,-1],n.trees = num.tree )[,,1])
write.csv(submission.boost,"boost.submission.csv")
