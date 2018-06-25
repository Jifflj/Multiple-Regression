library(readr)
library(caret)
library(doMC)
library(ggplot2)
library(arules)
library(corrplot)
registerDoMC(cores=4)

newproducts <- read.csv("/home/johannes/Documents/Ubiqum/Course2/task3/data/newproductattributes2017.2.csv")

new <- dummyVars("~ .", data = newproducts)

newproducts_dummy <- data.frame(predict(new, newdata = newproducts))

newproducts_dummy$BestSellersRank <- NULL
newproducts_dummy$ProductNum <- NULL

newproducts_dummy$x1StarReviews <- NULL
newproducts_dummy$x2StarReviews <- NULL
newproducts_dummy$x4StarReviews <- NULL
newproducts_dummy$x5StarReviews <- NULL

predset <- newproducts_dummy
nrow(predset)

set.seed(456)

predSVM <- predict(SVMfit, newdata = predset)

predSVM

predset$Volume <- predSVM

predset

write.csv(predset, file="SVMoutput.csv", row.names = TRUE)
