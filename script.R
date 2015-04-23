# set cwd
setwd("/home/mfc/cwd/Data_project/Data_project")
set.seed(1234)

#load packages
require(ggplot2)
library(tree)
library(readr)
library(randomForest)

#load data
train <- read.csv("train.csv",header = T )#, stringsAsFactors=FALSE)
test <- read.csv("test.csv")

submission.tree <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)

submission.rf <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)

#hist(train$feat_1)

#tree based method

tree.otto = tree(target ~ . -id, data=train)
submission.tree[,2:10] <- predict( tree.otto,test,type="vector")
write.csv(submission.tree,"tree.submission.csv")
plot(tree.otto)
text(tree.otto,pretty=0)
print(summary(tree.otto))
tree.pred <- predict( tree.otto,test,type="class")
cv.trees = cv.tree(tree.otto,FUN=prune.misclass)
pruned.tree <- prune.misclass(tree.otto,best=0)
plot(pruned.tree)
text(pruned.tree,pretty=0)


par(mfrow = c(1 ,2))
plot(cv.trees$size , cv.trees$dev , type ="b")
plot(cv.trees$k , cv.trees$dev , type ="b")



#random FOrest

rf <- randomForest(train[,c(-1,-95)], as.factor(train$target), ntree=25,importance=TRUE)
submission.rf[,2:10] <- (predict(rf, test[,-1], type="prob"))
write.csv(submission.rf,"rf.submission.csv")
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

