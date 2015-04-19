# set cwd
setwd("/home/mfc/cwd/Data_project/Data_project")

#load packages
require(ggplot2)
library(tree)


#load data
train <- read.csv("train.csv",header = T)

hist(train$feat_1)

#tree based method

tree.otto = tree(target ~ . -id, data=train)
plot(tree.otto)
text(tree.otto,pretty=0)
summary(tree.otto)


