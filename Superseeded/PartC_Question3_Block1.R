## Multiclass Perceptron


# basic algorithm
# 1. Initialse weight vectors randomly w_1,...,w_k
# 2. While not converged
#   For datapoint n=1 to N do:
#       y = argmax_k w_k * x_n
#       if y != y_n do:
#           w_y = w_y - eta * x_n
#           w_yn = w_yn + eta * x_n

### Load datasets
trainData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2C_train.csv")
trainLabel <- trainData[, 'y']
trainData <- trainData[, 1:4]

testData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2C_test.csv")
testLabel <- testData[, 'y']
testData <- testData[, 1:4]


# basis function
phi_ <- as.matrix(cbind(1, trainData));
eta_ <- 0.01;
epsilon_ <- 0.001;
iterMax_ <- 100;

# convert class labels to numerics
trLab_ <- as.numeric(as.factor(trainLabel));
# instantiate empty weight vector
w_ <- matrix(, nrow = iterMax_, ncol = ncol(phi_));

# initialise the first weights
w_[1,] <- runif(ncol(phi_));

# container for trackiner errors
err_ <- matrix(0, nrow = iterMax_, ncol = length(levels(trainLabel)));

# find error for each level




perceptron <- function(trainData,eta=0.01,epsilon=0.001,maxIters=100) {

    # perceptron ninja function. 

    # step 1 basis function development
    phi_ <- as.matrix(cbind(1, trainData)) # column with phi_0

    # initialisation
    eta_ <- eta;
    epsilon_ <- epsilon;
    iterMax_ <- maxIters;

    # convert class labels to numerics
    trainLabel <- as.numeric(trainLabel)
    testLabel <- as.numeric(testLabel)

    # container for weight vector
    w_ <- matrix(, nrow = iterMax_, ncol = ncol(phi_))

    #instantialise initial vectors
    w_[1,] <- runif(ncol(phi_))

    # container for errors
    err_ <- matrix(0, nrow = iterMax_, ncol = ncol(phi_ - 1));

    # record errors from initial random guess
    


}