## Assessment 2 Part A

# Question 1 Block 1.

# Batch Gradient Descent with L2 Regularisation

# Set reproducibility 
# set.seed(1234)


testData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2A_test.csv")
testLabel <- testData[, 'y']
testData <- testData[, 1:4]
trainData <- read.csv("C:/Users/aaron/OneDrive/Documents/Monash Data Science/Algorithms For Data Analysis/Assignment 2/Dataset/Task2A_train.csv ")
trainLabel <- trainData[, 'y']
trainData <- trainData[, 1:4]

trainLen <- nrow(trainData)

stochasticDescentL2 <- function(trainData, trainLabel, testData, testLabel, iterMax = 10000, lRate = 0.001, lambda = 0.001, epsilon = 0.01) {


    set.seed(1234)
    # length of training data
    trainLen <- nrow(trainData)

    # prediction function
    f_predict <- function(phi, coeffs) {

        return(phi %*% coeffs)
    }

    # error function
    f_error <- function(phi, coeffs, label) {

        return(sqrt(sum((f_predict(phi, coeffs) - label) ^ 2) / 2))


    }

    # make matrix with training data and phi. Initialise as 1
    phi_ <- as.matrix(cbind('X0' = 1, trainData))

    # max number of iterations
    iterMax_ <- iterMax

    # learning rate
    eta_ <- lRate

    # error termination criterion
    epsilon_ <- epsilon

    # lambda criterion
    lambda_ <- lambda

    # container for storing the coefficients at each iteration
    w_ <- matrix(, iterMax_, ncol(phi_))

    # instantiate a guess for coefficients to begin with.
    w_[1,] <- runif(ncol(phi_))

    # generate container for tracking error of training and test at each iteration
    err_ <- data.frame("iter" = 1:iterMax_)

    # instantiate itaration counter
    iter_ <- 1

    # instantiate terminiation variable for main search loop.
    term_ <- FALSE

    # iteratively search for parameters
    while (!term_) {

        # check terminiation criteria
        term_ <- iter_ >= iterMax_ | f_error(phi_, w_[iter_,], trainLabel) <= epsilon_

        # shuffle data for random visits
        trainIndex <- sample(1:trainLen, trainLen, replace = FALSE)

        # rearrange basis function data
        phi_ <- phi_[trainIndex,]
        # rearrange training labels locally
        trainLabel <- trainLabel[trainIndex]
        trainData <- trainData[trainIndex,]

        # visit each datapoint
        for (dp in 1:trainLen) {

            # check terminiation criteria
            if ((iter_ >= iterMax_ | f_error(phi_, w_[iter_, ], trainLabel) <= epsilon_)) {
                term_ <- TRUE
                break;
            }

            # make a prediction for that data point using the variables from phi and coefficents in the currently active iteration.
            wPhi_ <- f_predict(phi_[dp,], w_[iter_,])

            # for each coefficient, use the loss function to update the new params
            for (c in 1:ncol(phi_)) {
                # use the lossfunction with L2
                w_[iter_ + 1, c] <- w_[iter_, c] + eta_ * (((trainLabel[dp] - wPhi_) * phi_[dp, c]) + (lambda_ * w_[iter_, c]))

            }

            # record the errors
            # training error
            err_[iter_, 'train'] <- f_error(as.matrix(cbind(1, trainData)), w_[iter_,], trainLabel)
            # test error
            err_[iter_, 'test'] <- f_error(as.matrix(cbind(1, testData)), w_[iter_,], testLabel)

            # update iterator.
            iter_ = iter_ + 1

        }

        # record final params
        res_ <- w_[iter_,]
    }

    return(list("finalCoefficients" = res_, "errors" = err_))

}

# +(lambda_ * w_[iter_, c])


batchDescentL2 <- function(trainData, trainLabel, testData, testLabel, iterMax = 100, lRate = 0.01, lambda = 0.1, epsilon = 0.001 * mean(trainLabel)) {

    # Aux functions

    f_predict <- function(phi, coeffs) {

        return(phi %*% coeffs)

    }

    f_error <- function(phi, coeffs, label) {

        return((sum((f_predict(phi, coeffs) - label) ^ 2) / 2) + (sum(coeffs ^ 2)) * (lambda / 2))

    }

    # make matrix with training data and phi. Initialise as 1
    phi_ <- as.matrix(cbind('X0' = 1, trainData))

    # max number of iterations
    iterMax_ <- iterMax

    # learning rate
    eta_ <- lRate

    # error termination criterion
    epsilon_ <- epsilon

    # lambda criterion
    lambda_ <- lambda

    # container for storing the coefficients at each iteration
    w_ <- matrix(, iterMax_, ncol(phi_))

    # instantiate a guess for coefficients to begin with.
    w_[1,] <- runif(ncol(phi_))

    # generate container for tracking error of training and test at each iteration
    err_ <- data.frame("iter" = 1:iterMax_)

    # instantiate itaration counter
    iter_ <- 1

    # instantiate terminiation variable for main search loop.
    term_ <- FALSE


    ## Main Loop ##
    while (!term_) {

        ## check termination
        if (iter_ >= iterMax_ | f_error(phi_, w_[iter_, ], trainLabel) <= epsilon_) {
            term_ <- TRUE
            break;
        }

        # make prediction
        phi_omega <- f_predict(phi_, w_[iter_,])

        # set up decay container for decay eta
        etaHash_ <- eta_

        # line search
        while (etaHash_ > epsilon_) {

            # for each coefficient
            for (c in 1:ncol(w_)) {

                # loss function
                w_[iter_ + 1, c] <- w_[iter_, c] + etaHash_ * sum((trainLabel - phi_omega) * phi_[, c])

            }

            etaHash_ = etaHash_ / 2;
        }

        # use the correct etaHash after linesearch.
        # for each coefficient
        for (c in 1:ncol(w_)) {

            w_[iter_ + 1, c] <- w_[iter_, c] + etaHash_ * (sum((trainLabel - phi_omega) * phi_[, c])+(lambda_*w_[iter_,c]))

        }

        # error recording
        err_[iter_, 'train'] <- f_error(as.matrix(cbind(1, trainData)), w_[iter_,], trainLabel)
        err_[iter_, 'test'] <- f_error(as.matrix(cbind(1, testData)), w_[iter_,], testLabel)
        err_[iter_, 'etaHash'] <- etaHash_

        # iteration tracking
        iter_ = iter_ + 1;


    }

    # record final params
    res_ <- w_[iter_,]

    return(list("finalCoefficients" = res_, "errors" = err_))

}