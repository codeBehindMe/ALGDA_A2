### Question 1 part 3


# import libraries
require(ggplot2)
require(reshape2)


# import datasets
testData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2A_test.csv")
testLabel <- testData[, 'y']
testData <- testData[, 1:4]
trainData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2A_train.csv ")
trainLabel <- trainData[, 'y']
trainData <- trainData[, 1:4]


# load algorithms
source("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/RidgeRegressionR01.R")

# run sgd
sgd_ <- stochasticDescent(trainData, trainLabel, iterMax = 20 * nrow(trainData));

sgdErr <- sgd_$errors

# run bgd
bgd_ <- batchDescent(trainData, trainLabel, iterMax = 20);

bgdErr <- bgd_$errors


# record errors
errsTot <- data.frame('iter' = 1:nrow(sgdErr))
# merge sgd
errsTot <- merge(errsTot, sgdErr, by.x = 'iter', by.y = 'iteration')
# rename sgd training error
names(errsTot)[names(errsTot) == 'train'] <- "SGD Training Error";

# adjust key for bgd error for merge
bgdErr[, 'iterKey'] <- seq(1, 18600, 930)

errsTot <- merge(errsTot, bgdErr, by.x = 'iter', by.y = 'iterKey', all.x = TRUE)

# rename bgd training error
names(errsTot)[names(errsTot) == 'train'] <- "BGD Training Error";

# clean up dataframe
errsTot <- subset(errsTot, select = c("iter", "SGD Training Error", "BGD Training Error"))

errMelt <- melt(errsTot, id = 'iter')

ggplot(errMelt, aes(x = iter, y = value, colour = variable)) + geom_point() + ggtitle("BGD vs SGD Errors") + ylab("Error") + xlab("Visited Data Points");
