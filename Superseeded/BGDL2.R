# Batch descent with L2


testData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2A_test.csv")
testLabel <- testData[, 'y']
testData <- testData[, 1:4]
trainData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2A_train.csv ")
trainLabel <- trainData[, 'y']
trainData <- trainData[, 1:4]

trainLen <- nrow(trainData)



# utility functions
f_prediction <- function(phi, w) {
    # basis function * params.
    return(phi %*% w);
}

f_error <- function(phi, w, label) {
    # rmse
    return(sqrt(sum((((label - f_prediction(phi = phi, w = w)) ^ 2) / 2))));
}

# basis function 
phi_ <- as.matrix(cbind('X0' = 1, trainData)); # container #

# max iteration parameter #
iterMax_ <- 1000;

# learning rate parameter #
eta_ <- 0.00000001

# min error paramter #
epsilon_ <- 0.000000000001

# regularaisation parameter #
lambda_ <- 0.2

# coefficient container # 
w_ <- matrix(, nrow = iterMax_, ncol = ncol(phi_));

# error container
err_ <- data.frame('iteration' = 1:iterMax_);

# iteration counter initialisation #
iter_ <- 1;

# initial random guess of coefficients initialisation #
w_[iter_,] <- rnorm(ncol(phi_));

# termination flag initialisation #
term_ <- FALSE;

# begin descent
while (!term_) {

    # check termination criterion
    term_ <- iter_ >= iterMax_ | epsilon_ > f_error(phi = phi_, w = w_[iter_,], trainLabel);

    # make a prodiction from the current coefficients
    wPhi_ <- f_prediction(phi = phi_, w = w_[iter_,]);

    # initialise line search eta decay parameter
    etaPrime_ <- eta_;

    # begine line search
    while (etaPrime_ > epsilon_) {

        # check termination criteria
        if (iter_ >= iterMax_ | f_error(phi = phi_, w = w_[iter_, ], trainLabel) < epsilon_) {
            term_ <- TRUE;
            break;
        }

        # update params for each columns
        for (c in 1:ncol(w_)) {

            w_[iter_ + 1, c] <- w_[iter_, c] + etaPrime_ * sum((trainLabel - wPhi_) * phi_[, c] + (lambda_ * w_[iter_, c]));

        }

        # get the error.
        eIter <- f_error(phi = phi_, w = w_[iter_,], trainLabel);
        eIterPlus1 <- f_error(phi = phi_, w = w_[iter_ + 1,], trainLabel);

        if (eIter > eIterPlus1) {
            break;
        }
        # decay eta 
        etaPrime_ = etaPrime_ / 2;
    }

    # record error
    err_[iter_, 'train'] <- f_error(phi = phi_, w = w_[iter_,], trainLabel);
    err_[iter_, 'test'] <- f_error(phi = as.matrix(cbind(1,testData)), w = w_[iter_,], testLabel);

    iter_ = iter_ + 1;

}


