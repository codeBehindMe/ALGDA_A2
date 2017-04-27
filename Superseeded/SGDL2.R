# Stochastic gradient descent with Ridge regression

testData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2A_test.csv")
testLabel <- testData[, 'y']
testData <- testData[, 1:4]
trainData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2A_train.csv ")
trainLabel <- trainData[, 'y']
trainData <- trainData[, 1:4]

trainLen <- nrow(trainData)


set.seed(1234)

f_prediction <- function(phi, w) {

    return(phi %*% w)
}

f_error <- function(phi, w, trainLabel) {

    # RMSE
    return(sqrt(sum((((trainLabel - f_prediction(phi = phi, w = w)) ^ 2) / 2))));
}

# cbind basis function (use 1 for now) ## container ##
phi_ <- as.matrix(cbind('x0' = 1, trainData));

# max iteration handler ## function parameter ##
iterMax_ <- 20 * nrow(trainData);

# learning rate ## function parameter ##
eta_ <- 0.00000000001;

# rmse termination critereon ## function parameter ##
epsilon_ <- 0.1

# ridge regression lambda critereon ## function parameter ##
lambda_ <- 5

# container for coefficients ## container ##
w_ <- matrix(, nrow = iterMax_, ncol = ncol(phi_));

# instantiate guess coefficients ## initialisation ##
w_[1,] <- runif(ncol(phi_));

# error tracking ## container ##
err_ <- data.frame('iter' = 1:iterMax_);

# instantiate error counter ## initialisation ##
iter_ <- 1;

# instantiate termination flag
term_ <- FALSE;

while (!term_) {

    # check termination criteron
    term_ <- iter_ >= iterMax_ | epsilon_ > f_error(phi = phi_, w = w_[iter_,], trainLabel = trainLabel);

    # randomise data
    trainIndex_ <- sample(1:trainLen, trainLen, replace = FALSE);

    # rearrange the basis function and training labels (do this locally).
    phi_ <- phi_[trainIndex_,];
    trLabels_ <- trainLabel[trainIndex_];

    # visit each data point
    for (n in 1:trainLen) {
        
        
        # calculate error in iter, it should terminiate correctly.
        err_[iter_, 'train'] <- f_error(phi = as.matrix(cbind(1, trainData)), w = w_[iter_,], trainLabel);
        err_[iter_, 'test'] <- f_error(phi = as.matrix(cbind(1, testData)), w = w_[iter_,], testLabel);

        # check for termination criterea.
        if (iter_ >= iterMax_) {
            term_ <- TRUE;
            break;
        }

        # make a prediction with the current coefficients for the datapoint.
        wPhi_ <- f_prediction(phi = phi_[n,], w = w_[iter_,]);

        # for each coefficient, use the loss function to update the coefficients.
        for (c in 1:ncol(w_)) {
            
            w_[iter_ + 1, c] <- w_[iter_, c] + eta_ * sum((trLabels_[n] - wPhi_) * phi_[n, c] + (lambda_ * w_[iter_, c])); # L2
        }

        # increment iterator
        iter_ = iter_ + 1;
    }

    
}


