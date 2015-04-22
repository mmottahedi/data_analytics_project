# set cwd
setwd("/home/mfc/cwd/Data_project/Data_project")
set.seed(1234)

#load packages
require(ggplot2)
library(tree)


#load data
train <- read.csv("train.csv",header = T )#, stringsAsFactors=FALSE)
test <- read.csv("test.csv")
hist(train$feat_1)

#tree based method

tree.otto = tree(target ~ . -id, data=train)
plot(tree.otto)
text(tree.otto,pretty=0)
print(summary(tree.otto))
tree.pred <- predict( tree.otto,test,type="class")
cv.trees = cv.tree(tree.otto,FUN=prune.misclass)
pruned.tree <- prune.misclass(tree.otto,best=0)
plot(pruned.tree)
text(pruned.tree,pretty=0)


par( mfrow = c (1 ,2) )
plot( cv.trees$size , cv.tree$dev , type ="b")
plot( cv.trees$k , cv.tree$dev , type ="b")




