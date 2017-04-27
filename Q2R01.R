require(ggplot2)
require(reshape2)

# load datasets
testData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2B_test.csv")
testLabel <- testData[, 'y']
testData <- testData[, 1:4]
trainData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2B_train.csv ")
trainLabel <- trainData[, 'y']
trainData <- trainData[, 1:4]

source("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/RidgeRegressionR01.R")

# number of datasets 
L <- 50;
# data points in each dataset
N <- 100;
M <- length(seq(0, 5, 0.2));

# modelSpaceMatrix
# modelSpace_ <- matrix(, nrow = L, ncol = ncol(trainData) + 1);

# create matrix to store predictions
y_ <- data.frame(matrix(, nrow = L * length(seq(0, 5, 0.2)), ncol = nrow(testData) + 2));
# +2 for dataset number and model number (lambda)

# rename the first 2 columns appropriately.
names(y_) <- c('L', 'M', paste0('y', 1:nrow(testData)))

# initalise the keys for L and M.
y_$M <- rep(1:M, 1, each = L);
y_$L <- rep(1:L, M, each = 1);

iter_ <- 1;

# build 50 regression models
for (l in 1:L) {

    # bootstrapping
    trainIndex <- sample(1:nrow(trainData), N, replace = TRUE);

    # generate m models for each lambda
    for (m in 1:M) {

        # generate coefficients
        obj_ <- stochasticDescent(trainData[trainIndex,], trainLabel[trainIndex], iterMax = 5 * length(trainIndex), lambda = m*0.2-0.2);
        # get coefficients from resultant object
        obj_ <- obj_$coefficients

        # reference using m and l to matrix and put in predicted values off coefficients in object.
        y_[y_$M == m & y_$L == l, - c(1, 2)] <- as.matrix(cbind(1, testData)) %*% obj_;
        print(paste("built model", iter_, "of", nrow(y_), sep = " "));
        iter_ = iter_ + 1;
    }

}

# mean aggregate ybar
y_bar <- aggregate(y_, list(y_$M), mean);
# remove unnecessary columns
y_bar <- as.matrix(y_bar[, - c(1:2)]);

# create containers for error, bias2 and variance
error_ <- matrix(0, nrow = M);
bias2_ <- matrix(0, nrow = M);
variance_ <- matrix(0, nrow = M);

# for each model, calculate error, bias2 and variance

for (m in 1:M) {

    bias2_[m] <- mean((y_bar[m,] - testLabel) ^ 2);
    variance_[m] <- mean((y_[y_$M == m, - c(1, 2)] - y_bar[m,]) ^ 2);
    error_[m] <- mean((y_[y_$M == m, - c(1, 2)] - testLabel) ^ 2);

}


# aggregate results
final_ <- as.data.frame(cbind(seq(0,5,0.2), bias2_, variance_, error_, bias2_ + variance_));

names(final_) <- c("Lambda", "bias2", "variance", "error", "bias2+variance");

final_m <- melt(final_, id = 'Lambda');

# plot
ggplot(data = final_m, aes(x = log(Lambda), y = value, color = variable)) + geom_line() + ggtitle("Bias,Variance and Errors") + xlab("log(Lambda)");