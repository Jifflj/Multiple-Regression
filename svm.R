library(readr)
library(caret)
library(doMC)
library(ggplot2)
library(arules)
library(corrplot)

registerDoMC(cores=4)

existing <- read.csv("/home/johannes/Documents/Ubiqum/Course2/task3/data/existingproductattributes2017.2.csv")

str(existing)
summary(existing)

#removing unnecessary Product Types
#remove rows which are not PC,Laptop, Netbook, Smartphone
existing 
readyData <- existing
#indices_1 <- c("PC","Laptop","Netbook","Smartphone","Software","Accessories","Display","Tablet","GameConsole","Printer","PrinterSupplies","ExtendedWarranty")
indices_1 <- c("PC","Laptop","Netbook","Smartphone","ExtendedWarranty")
indices_2 <- which(readyData$ProductType %in% indices_1) #call rows WHICH contain INDICES_1 in MY_SOURCE$TYPE
#which(!my_source$type %in% indices_1) #call rows WHICH are NOT (!) in INDICES_1 in MY_SOURCE$TYPE
readyData_reduced <- readyData[indices_2,]
readyData_reduced$ProductType <- factor(readyData_reduced$ProductType)

readyData_reduced
str(readyData_reduced)

new <- dummyVars("~ .", data = readyData_reduced)

readyData_clean <- data.frame(predict(new, newdata = readyData_reduced))

#deletes columns completeley
readyData_clean$BestSellersRank <- NULL
readyData_clean$ProductNum <- NULL


#REPLACE OUTLIERS BY MEDIAN #
outlierKD <- function(dt, var) {
  plotting_out <- FALSE
  
  #define variables
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  m1 <- median(var_name, na.rm = T)
  outlier <- boxplot.stats(var_name)$out
  mo <- median(outlier)
  
  #create 2x2 canvas
  if(plotting_out){
    par(mfrow=c(2, 2), oma=c(0,0,3,0))
    boxplot(var_name, main="With outliers")
    hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  }
  # If value is an outlier introduce median
  # If not, do nothing
  var_name <- ifelse(var_name %in% outlier, m1, var_name)
  m2 <- median(var_name, na.rm = T)
  na <- length(outlier)
  
  if(plotting_out){
    boxplot(var_name, main="Without outliers")
    hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
    title("Outlier Check for var", outer=TRUE)
  }  
  #print messages
  message("Outliers identified: ", na, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", na / tot*100)
  message("Median of the outliers: ", mo)
  message("Median without removing outliers: ", m1)
  message("Median if we remove outliers: ", m2)
  
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  message("Outliers successfully removed", "\n")
  
  if(plotting_out){
    par(mfrow= c(1,1),oma=c(0,0,0,0))
  }
  
  return(invisible(dt))
}


####outlier print####
outlier_detect <- TRUE
if(outlier_detect){
  plotting <- FALSE
  if(plotting){
    setwd("/home/johannes/Documents/Ubiqum/Course2/task3/data/")
    pdf("outliers.pdf")
  }
  outlierKD(readyData_clean,Volume) # insert data frame and outlier search variable here
  if(plotting){
    dev.off()
  }
}


#removing overfitting attributes
readyData_clean$x1StarReviews <- NULL
readyData_clean$x2StarReviews <- NULL
readyData_clean$x4StarReviews <- NULL
readyData_clean$x5StarReviews <- NULL

str(readyData_clean)

set.seed(456)

# creates partition of the data 75% / 25%
inTrain <- createDataPartition(y=readyData_clean$Volume,
                               p=.75,
                               list=FALSE)


trainSet<-readyData_clean[inTrain,]

testSet<-readyData_clean[-inTrain,]


ctrl <- trainControl(method = "repeatedcv", # specifies repeated K-fold cross-validation 
                     number = 10,
                     repeats = 3)
                     #summaryFunction = twoClassSummary,
                     #classProbs = TRUE) # repititions 

set.seed(456)
SVMfit <- train(Volume~.
                -Price
                -ShippingWeight
                -ProductDepth
                -ProductWidth
                -ProductHeight
                -ProfitMargin
                +Recommendproduct
                -NegativeServiceReview
                +PositiveServiceReview
                +x3StarReviews,
                data = trainSet,
                method = "svmLinear",
                tuneLength = 30,
                trControl = ctrl,
                preProc = c("center","scale"))


SVMfit
#plot(SVMfit)

# PREDICTION
predictors(SVMfit)

testPredSVM <- predict(SVMfit, testSet) #type = "prob")

postResample(testPredSVM, testSet$Volume)

#confusionMatrix(data = testPredknn1, testingset$brand)

plot(testPredSVM,testSet$Volume)